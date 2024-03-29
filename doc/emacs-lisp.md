# Emacs Lisp

## Summary

```
`foo  -> (quote foo)
#`foo -> (function foo) see quote
```

## Emacs Help Tools

`M-x shortdoc-display-group`

## References

- https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
- https://en.wikipedia.org/wiki/Lisp_(programming_language)

- [Common Lisp](https://lisp-lang.org)
- [Common Lisp Extensions](https://www.gnu.org/software/emacs/manual/html_mono/cl.html)

- [Emacs's Builtin Elisp Cheat Sheet - Mastering Emacs](https://www.masteringemacs.org/article/emacs-builtin-elisp-cheat-sheet)

## eval-when-compile

`eval-when-compile body`

This form marks body to be evaluated at compile time but not when the compiled program is
loaded. The result of evaluation by the compiler becomes a constant which appears in the compiled
program. If you load the source file, rather than compiling it, body is evaluated normally.

## nil and t

- *nil** is
- a symbol with the name ‘nil’
- the logical truth value false
- the empty list `nil` === `()`

- *t** is true

## List / Dotted Pair Notation

A Lisp list is implemented as a singly linked list.
Each cell of this list is called a **cons** (in Scheme, a pair) and is composed of two pointers, called the **car** and **cdr**.

Dotted pair notation is a general syntax for cons cells that represents the CAR and CDR explicitly.

https://www.gnu.org/software/emacs/manual/html_node/elisp/Dotted-Pair-Notation.html

```
(cons 1 2)
;; same as
(1 . 2)

(cons 1 (cons 2 (cons 3 nil)))
;; same as
(list 1 2 3)

(list 1 2 3)
;; same as
(1 . (2 . (3 . nil)))
;;  *--*--*--nil
;;  |  |  |
;;  1  2  3

(cons (cons 1 2) (cons 3 4))
;; same as
((1 . 2) . (3 . 4))
;;    *
;;   / \
;;  *   *
;; / \ / \
;; 1 2 3 4

(rose . violet) ; is a cons
(rose . (violet . (buttercup))) ; === (rose violet buttercup)
(rose violet . buttercup) ; === (rose . (violet . buttercup))
(rose violet) ; === (rose . (violet))
```

## String

```
(string ?a ?b ?c)
;; -> "abc"

(substring "abcdefg" 0 3)
;; -> "abc"
```

## Quote

Lisp evaluates expressions which are entered by the user, e.g. `(+ 2 3)` evaluates to `5`.
Any expression can also be marked to prevent it from being evaluated.

```
(quote foo)
;; or
'foo
```

[elisp - When should sharp quotes be used? - Emacs Stack Exchange](https://emacs.stackexchange.com/questions/35988/when-should-sharp-quotes-be-used)


## Backquote

[Backquote](https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html)

Backquote constructs allow you to quote a list, but selectively evaluate elements of that list. In
the simplest case, it is identical to the special form quote.

```
`(a list of (+ 2 3) elements)
     ⇒ (a list of (+ 2 3) elements)

'(a list of (+ 2 3) elements)
     ⇒ (a list of (+ 2 3) elements)
```

The special marker `,` inside of the argument to backquote indicates a value that isn’t
constant. The Emacs Lisp evaluator evaluates the argument of `,`, and puts the value in the list
structure:

```
`(a list of ,(+ 2 3) elements)
     ⇒ (a list of 5 elements)
```

```
(setq some-list '(2 3))
     ⇒ (2 3)

(cons 1 (append some-list '(4) some-list))
     ⇒ (1 2 3 4 2 3)

`(1 ,@some-list 4 ,@some-list)
     ⇒ (1 2 3 4 2 3)
```

## Function

See https://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html

```
(lambda (x) (* x x))
;; or
(function (lambda (x) (* x x)))
;; or
#'(lambda (x) (* x x))
```

## Function / Lambda

```
(lambda (arg) (+ arg 1))

(defun foo (a b c d) (+ a b c d))
```

## Dash — A modern list API for Emacs

[magnars/dash.el: A modern list library for Emacs](https://github.com/magnars/dash.el)

See also
- [Common Lisp Extensions](https://www.gnu.org/software/emacs/manual/html_node/cl/)
- [EmacsWiki : Common Lisp For Emacs](https://www.emacswiki.org/emacs/CommonLispForEmacs)

## Keyword Arguments

[EmacsWiki : Keyword Arguments](https://www.emacswiki.org/emacs/KeywordArguments)

Keyword parameters may be emulated (assuming you have no `&rest` args) by treating the `&rest` arguments
as a property list:

```
(defun foobar-plist (&rest args)
  "`foobar-plist' takes keyword arguments of :foo and :bar like this:
\(foobar-plist :foo 23)
\(foobar-plist :bar 42)"
  (format "foo is %S and bar is %S" (plist-get args :foo) (plist-get args :bar)))
```

`(plist-get PLIST PROP &optional PREDICATE)`
Extract a value from a property list.
PLIST is a property list, which is a list of the form (PROP1 VALUE1 PROP2 VALUE2...)

# Symbol Properties

- [Symbol Properties](https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Properties.html)
- [Standard Properties](https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Properties.html)

```
(put symbol property value)
```

# Autoload

[Autoload (GNU Emacs Lisp Reference Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html)

The autoload facility lets you register the existence of a function or macro, but put off loading
the file that defines it. The first call to the function automatically loads the proper library, in
order to install the real definition and other associated code, then runs the real definition as if
it had been loaded all along. Autoloading can also be triggered by looking up the documentation of
the function or macro), and completion of variable and function names.

There are two ways to set up an autoloaded function: by calling autoload, and by writing a “magic”
comment in the source before the real definition

```
;;;###autoload
(defun foo () ...)
```
