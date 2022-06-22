# Emacs Lisp

* https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
* https://en.wikipedia.org/wiki/Lisp_(programming_language)

## eval-when-compile

`eval-when-compile body`

This form marks body to be evaluated at compile time but not when the compiled program is
loaded. The result of evaluation by the compiler becomes a constant which appears in the compiled
program. If you load the source file, rather than compiling it, body is evaluated normally.

## nil and t

**nil** is 
* a symbol with the name ‘nil’
* the logical truth value false
* the empty list `nil` === `()`

**t** is true

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
