It consists of the base regex package, the CL-AWK macro package which
provides an AWK-like language for Lisp (but with compiled speed), and
a lexer package which makes it easy to generate compact and fast
tokenizers similar to Lex.
    
This code was originally written in Zetalisp on a Symbolics XL1200,
then ported to a variety of Common Lisp systems on a variety of
machines and operating systems.  It has been successfully used on
32-bit x86 running Windows with Xanalys Lispworks, on 32-bit x86
running Linux with GCL and CLISP, on MC68040 CPU running NeXTStep with
GCL, CLisp, and Allegro Common Lisp, and XL1200 running Genera with
Symbolics Common Lisp.

It was originally released under a 4-clause BSD license.  It is being
re-released here under the 3-clause BSD License.
