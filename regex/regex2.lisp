;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGEX2; Base: 10 -*-


(defpackage REGEX2
  (:use COMMON-LISP CLOS)
#+:Genera  (:import-from SCL ONCE-ONLY)
  (:export
    ;; compiler
    COMPILE-STR
    ;; match/scan
    MATCH-STR SCAN-STR SCAN-STR-FAST
    ;; Accessors into matcher.  Overloaded on string, symbol, and matcher.
    MATCHED-P MATCH-START MATCH-LENGTH
    ;; Accessors into match registers.  Overloaded on string, symbol,
    ;; matcher, and register array.
    REGISTER-MATCHED-P REGISTER-START REGISTER-END
    ;; convert any accepted representation of a pattern into a compiled pattern
    GET-MATCHER-FOR-PATTERN
   ))





(in-package "REGEX2")

(proclaim '(optimize (speed 3) (safety 0) (space 0)))




(defmacro defregex (name patstr &key (compile nil) (optimize nil))
"Define a regular expression matching function.  The :compile keyword will
 force the function to be compiled.  The :optimize keyword will trigger
 additional optimizations that are fairly expensive, and so are only really
 worth it for frequently used patterns."
  `(progn
     (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
     (setf (symbol-function ',name)
		,(compile-sexpr (regex::parse-str patstr)
				:optimize optimize))
	  ,@(if compile `((unless (compiled-function-p (function ,name))
                            (compile ',name))) )))



(defun add-state (name statebody vars &key (inlineable t))
  (declare (special *states* *inlineable-states* *vars-used*))
  (push `(,name (pos)
	  (declare (fixnum pos))
	  ,@statebody) *states*)
  (dolist (var vars)
    (pushnew var *vars-used*))
  (if inlineable
      (push name *inlineable-states*))
  name)


(defun compile-sexpr (tree &key (optimize nil))
  (let* ((success-fn (gensym "SUCCEEDSTATE-"))
	 (*states*)
	 (*inlineable-states*)
	 (*vars-used*))
    (declare (special *states* *inlineable-states* *vars-used*))
    (add-state success-fn
	       '((return-from matcher (values t start (- pos start) regs)))
	       '(start pos regs))
    (let ((start-fn (compile-state-machine tree success-fn)))
      (let ((fxn `(function (lambda (str regs
				     start end
				     start-is-startanchor-p
				     end-is-endanchor-p)
			      (declare (string str) (fixnum start end))
			      ,@(unless (member 'str *vars-used*)
				 '((declare (ignore str))))
			      ,@(unless (member 'regs *vars-used*)
				 '((declare (ignore regs))))
			      ,@(unless (member 'end *vars-used*)
				 '((declare (ignore end))))
			      ,@(unless (member 'start-is-startanchor-p *vars-used*)
				 '((declare (ignore start-is-startanchor-p))))
			      ,@(unless (member 'end-is-endanchor-p *vars-used*)
				 '((declare (ignore end-is-endanchor-p))))
			      (block matcher
				(labels ,*states*
				  ,@(if optimize
				       `((declare (inline ,@*inlineable-states*))))
				  (,start-fn start)))))))
	fxn))))

(defun compile-state-machine (tree success-fn)
  (cond ((eq tree nil))
        ((characterp tree)
         (compile-char tree success-fn))
        ((eq 'REGEX::seq (first tree))
         (compile-seq (rest tree) success-fn))
        ((eq '* (first tree))
         (compile-star (second tree) success-fn))
        ((eq '+ (first tree))
         (compile-plus (second tree) success-fn))
        ((eq 'REGEX::alt (first tree))
         (compile-alt (rest tree) success-fn))
        ((eq '? (first tree))
         (compile-question (second tree) success-fn))
        ((eq 'REGEX::start (first tree))
         (compile-startanchor (second tree) success-fn))
        ((eq 'REGEX::end (first tree))
         (compile-endanchor (second tree) success-fn))
        ((eq 'REGEX::reg (first tree))
         (compile-register (rest tree) success-fn))
        ((eq 'REGEX::class (first tree))
         (compile-class (second tree) success-fn))
        ((eq 'REGEX::nclass (first tree))
         (compile-nclass (second tree) success-fn))
        ((eq 'REGEX::any (first tree))
         (compile-any tree success-fn))
        (t ;; once we're done, this should throw the :invalid-parse-tree tag
	 ; (throw 'REGEX::regex-parse-error (list "Unknown regex parse tree node ~A" tree))
	 (error "Unknown regex parse tree node ~A" tree)
	 )))

(defun compile-char (tree success-fn)
  (add-state (gensym "CHAR-STATE-")
	     `((if (and (< pos end) (char= ,tree (char str pos)))
		   (,success-fn (1+ pos))))
	     '(end str)))
  
(defun compile-seq (tree success-fn)
  (if (null tree)
      success-fn
      (compile-state-machine (first tree) (compile-seq (rest tree) success-fn))))

(defun compile-star (tree success-fn)
  (let* ((this-fn (gensym "STAR-STATE-"))
	 (body-fn (compile-state-machine tree this-fn)))
    (add-state this-fn
	       `((,body-fn pos)
		 (,success-fn pos)
		 nil)
	       '()
	       :inlineable nil)))

(defun compile-plus (tree success-fn)
  (let ((rest-fn (compile-star tree success-fn)))
    (compile-state-machine tree rest-fn)))

(defun compile-alt (tree success-fn)
  (let ((second-fn (compile-state-machine (second tree) success-fn))
	(first-fn (compile-state-machine (first tree) success-fn)))
    (add-state (gensym "ALT-STATE-")
	       `((,first-fn pos)
		 (,second-fn pos))
	       '())))

(defun compile-question (tree success-fn)
  (let ((body-fn (compile-state-machine tree success-fn)))
    (add-state (gensym "QUESTION-STATE-")
	       `((,body-fn pos)
		 (,success-fn pos))
	       '())))

(defun compile-startanchor (tree success-fn)
  (declare (ignore tree))
  (add-state (gensym "STARTANCHOR-STATE-")
	     `((if (and start-is-startanchor-p (= pos start ))
		   (,success-fn pos)))
	     '(start-is-startanchor-p start)))

(defun compile-endanchor (tree success-fn)
  (declare (ignore tree))
  (add-state (gensym "ENDANCHOR-STATE-")
	     `((if (and end-is-endanchor-p (= pos end))
		   (,success-fn pos)))
	     '(end-is-endanchor-p)))

(defun compile-register (tree success-fn)
  (let* ((regnum (first tree))
	 (pat (second tree))
	 (rend-fn (add-state (gensym "REND-STATE-")
			     `((setf (reg-end regs ,regnum) pos)
			       (,success-fn pos)
			       (setf (reg-end regs ,regnum) nil)
			       nil)
			     '(regs)))
	 (body-fn (compile-state-machine pat rend-fn))
	 (start-fn (add-state (gensym "RSTART-STATE-")
			      `((setf (reg-start regs ,regnum) pos)
				(,body-fn pos)
				(setf (reg-start regs ,regnum) nil)
				nil)
			      '(regs))))
    start-fn))

(defun compile-class (tree success-fn)
  (let ((specclass (REGEX::special-class tree)))
    (if specclass
	(add-state (gensym "SPEC-STATE-")
		   `((if (and (< pos end)
			      (,(REGEX::get-spec-pat-fxn specclass) (char str pos)))
			 (,success-fn (1+ pos))))
		   '(end str))
	(let* ((chars (REGEX::expand-char-class tree))
	       (cclen (length chars)))
	  (if (<= cclen 4)
	      (add-state
		(gensym "CLASS-STATE-")
		`((if (< pos end)
		      (let ((chr (char str pos)))
			(if (or ,@(loop for i from 0 below cclen
					collect `(char= chr ,(schar chars i))))
			    (,success-fn (1+ pos))))))
		'(end str))
	      (add-state (gensym "CLASS-STATE-")
			 `((if (and (< pos end)
				    (find (char str pos) ,chars))
			       (,success-fn (1+ pos))))
			 '(end str)))))))

(defun compile-nclass (tree success-fn)
  (let ((specclass (REGEX::special-class tree)))
    (if specclass
	(add-state (gensym "NSPEC-STATE-")
		   `((if (and (< pos end)
			      (not (,(REGEX::get-spec-pat-fxn specclass) (char str pos))))
			 (,success-fn (1+ pos))))
		   '(end str))
	(let* ((chars (REGEX::expand-char-class tree))
	       (cclen (length chars)))
	  (if (<= cclen 4)
	      (add-state
		(gensym "NCLASS-STATE-")
		`((if (< pos end)
		      (let ((chr (char str pos)))
			(if (not (or ,@(loop for i from 0 below cclen
					     collect `(char= chr
							     ,(schar chars i)))))
			    (,success-fn (1+ pos))))))
		'(end str))
	      (add-state (gensym "NCLASS-STATE-")
			 `((if (and (< pos end)
				    (not (find (char str pos) ,chars)))
			       (,success-fn (1+ pos))))
			 '(end str)))))))

(defun compile-any (tree success-fn)
  (declare (ignore tree))
  (add-state (gensym "ANY-STATE-")
	     `((if (< pos end)
		   (,success-fn (1+ pos))))
	     '(end)))




(defun make-regs (n)
  (let ((regs (make-array n)))
    (dotimes (i n regs)
      (setf (aref regs i) (cons nil nil)))))

(defun reg-start (regs n)
  (car (aref regs n)))

(defun reg-end (regs n)
  (cdr (aref regs n)))

(defsetf reg-start (regs n) (val)
  `(setf (car (svref ,regs ,n)) ,val))
(defsetf reg-end (regs n) (val)
  `(setf (cdr (svref ,regs ,n)) ,val))



(defregex pat1-fast "A*BD" :compile t :optimize t)
(defregex pat2-fast "((A*B)|(AC))D" :compile t :optimize t)
(defregex pat3-fast "((A*B)|(A*C))D" :compile t :optimize t)
(defregex pat4-fast "[Aa]*[Bb][Dd]" :compile t :optimize t)

(defregex pat1-slow "A*BD" :compile t :optimize nil)
(defregex pat2-slow "((A*B)|(AC))D" :compile t :optimize nil)
(defregex pat3-slow "((A*B)|(A*C))D" :compile t :optimize nil)
(defregex pat4-slow "[Aa]*[Bb][Dd]" :compile t :optimize nil)

(defun respeedtest (numreps name matcher candstr)
  (format t "~%~%Timing ~S" name)
  (let ((regs (make-regs 10))
	(len (length candstr))
	(matchedp nil))
    (time
      (dotimes (rep numreps)
	(setq matchedp (funcall matcher candstr regs 0 len t t))))
    (when (not matchedp)
      (format t "~%Didn't match"))))

(defun speedtest ()
  (let ((numreps 1000000)
        (candstr "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABD"))
;    (respeedtest numreps "slow A*BD" #'pat1-slow candstr)
    (respeedtest numreps "fast A*BD" #'pat1-fast candstr)
;    (respeedtest numreps "slow ((A*B)|(AC))D" #'pat2-slow candstr)
    (respeedtest numreps "fast ((A*B)|(AC))D" #'pat2-fast candstr)
;    (respeedtest numreps "slow ((A*B)|(A*C))D" #'pat3-slow candstr)
    (respeedtest numreps "fast ((A*B)|(A*C))D" #'pat3-fast candstr)
;    (respeedtest numreps "slow [Aa]*[Bb][Dd]" #'pat4-slow candstr)
    (respeedtest numreps "fast [Aa]*[Bb][Dd]" #'pat4-fast candstr)
  ))
