;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; [GNU Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html)
;; [Elisp Reference Sheet - CheatSheet.pdf](http://alhassy.com/ElispCheatSheet/CheatSheet.pdf)

;; [File Names](https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Names.html)

;; Lisp... 
(+ 1 (+ 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; nil <=> ()
;; t means true
;; [nil explained](https://www.gnu.org/software/emacs/manual/html_node/eintr/nil-explained.html)
;; [nil and t](https://www.gnu.org/software/emacs/manual/html_node/elisp/nil-and-t.html)

(booleanp object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; symbol
foo
Foo
foo/bar
foo+
:foo ; keyword symbol
\(*\ 1\ 2\) ; means '(* 1 2)'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; set value

(setq foo 1)
(setq foo 1.234) ; float as usual
(setq foo ?a) ; character 'a'
(setq foo 1
      bar 2)

;; foo ⇒ (a b c)
;; ` or (quote ...)  means don't evaluate
(setq foo '(a b c))

(setq counter 0)
(setq counter (+ counter 1))

;; Constant
(defconst foo 1) ; informative and not enforced in Emacs !

;; setf macro
;; [Setting Generalized Variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Generalized-Variables.html)
(setq a (list "hello" "world"))
(setf (substring (cadr a) 2 4) "o")
(cadr a) ; ⇒ "wood"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Number

(1+ 1) ; ⇒ 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; String
;; [Strings and Characters](https://www.gnu.org/software/emacs/manual/html_node/elisp/Strings-and-Characters.html)

(setq foo "...")
(concat foo "...")
(substring "The quick brown fox jumped." 16 19)
(string ?a) ; ⇒ "a"
(make-string 3 ?x) ; ⇒ "xxx"

;; Message
(message "This message appears in the echo area!")
(message "The name of this buffer is: %s." (buffer-name))
(message "The value of fill-column is %d." fill-column)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; list car crd cons...
;;

(car '(a b c)) ; ⇒ a
(cdr '(a b c)) ; ⇒ (b c)
(cons 'a '(b c)) ; ⇒ (a b c)
(nthcdr 2 '(a b c d)) ; ⇒ (c d)
(nth 2 '(a b c d)) ; ⇒ c

(setq alist '(a b c))
(setcar alist 'aa) ; ⇒ aa and now alist = (aa b c)
(setcdr alist 'bb) ; ⇒ bb and now alist = (aa . bb)

(list 'a 'b 'c) ; ⇒ (a b c)

;; returns t if object1 and object2 are the same object, and nil otherwise
(eq object1 object2)
(eq 'foo 'foo) ; ⇒ t
(eq ?A ?A) ; ⇒ t
(eq '(1 (2 (3))) '(1 (2 (3)))) ; ⇒ nil
;; don't use for float or string !

;; returns t if object1 and object2 have equal components, and nil otherwise
(equal object1 object2)
(equal "asdf" "asdf") ; ⇒ t
(equal '(1 (2 (3))) '(1 (2 (3)))) ; ⇒ t

;; Return non-nil if ELT is an element of LIST.  Comparison done with eq.
(memq ELT LIST)

;; Add NEWELT to the list stored in the generalized variable PLACE.
;;   This is morally equivalent to (setf PLACE (cons NEWELT PLACE)),
;;   except that PLACE is evaluated only once (after NEWELT).
(push NEWELT PLACE)

;; Return the reversed copy of list, vector, or string SEQ.
(reverse SES)
;; Reverse order of items in a list, vector or string SEQ.
(nreverse SEQ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Association Lists or alist
;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html
;;  An association list, or alist for short, records a mapping from keys to values. It is a list of
;;  cons cells called associations: the CAR of each cons cell is the key, and the CDR is the
;;  associated value.

(setq alist ((key_a . 1) ("key_b" 2 3)))
(assoc 'key_a alist) -> 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Vector type
;; [Vectors](https://www.gnu.org/software/emacs/manual/html_node/elisp/Vectors.html)

[1 "two" (three)]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hash
;; [Hash Tables](https://www.gnu.org/software/emacs/manual/html_node/elisp/Hash-Tables.html)

(setq ahash (make-hash-table))
(puthash 'key1 1 ahash)
(gethash 'key1 ahash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Variable

;; Sets up local bindings for a certain set of variables, as specified by bindings,
;; and then evaluates all of the forms in textual order.
(setq y 2)
(let ((y 1)
      (z y))
  (list y z)) ; ⇒ (1 2)

;; like let, but it binds each variable right after computing its local value,
;; before computing the local value for the next variable.
(let* ((y 1)
       (z y)) ; Use the just-established value of y
  (list y z)) ; ⇒ (1 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Control Structures

;; [Sequencing](https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequencing.html)
;; block of codes
(progn
  ()
  ()
  () ; return this
  )
(prog1 ; or prog2
  () ; prog1 return this
  () ; prog2 return this
  ()
  )

;; [Conditionals](https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html)
;; if
(if (> 5 4)                             ; if-part
    (message "5 is greater than 4!"))   ; then-part

(if (> 4 5)                               ; if-part
    (message "4 falsely greater than 5!") ; then-part
  (message "4 is not greater than 5!"))   ; else-part

;; macro when / unless
(when condition a b c)
;; is
(if condition (progn a b c) nil)
(unless condition a b c)

;; macro when-let if-let while-let
(when-let ((result1 (do-computation))
           (result2 (do-more result1)))
  (do-something result2))
;;
(let ((result1 (do-computation)))
  (when result1
    (let ((result2 (do-more result1)))
      (when result2
        (do-something result2)))))

;; Macro: if-let spec then-form else-forms...
;; Evaluate each binding in spec in turn, like in let* (see Local Variables, stopping if a binding
;; value is nil. If all are non-nil, return the value of then-form, otherwise the last form in
;; else-forms.

;; Macro: when-let spec then-forms...
;; Like if-let, but without else-forms.

;; Macro: while-let spec then-forms...
;; Like when-let, but repeat until a binding in spec is nil. The return value is always nil.

;; case
;; [Conditionals](https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html)
(cond ((numberp x) x)
      ((stringp x) x)
      ((bufferp x)
       (setq temporary-hack x) ; multiple body-forms
       (buffer-name x))        ; in one clause
      ((symbolp x) (symbol-value x)))

;; pattern matching
;; [Pattern-Matching Conditional](https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern_002dMatching-Conditional.html)
;; pcase macro
(pcase (get-return-code x)
  ;; string
  ((and (pred stringp) msg)
   (message "%s" msg))

  ;; symbol
  ('success       (message "Done!"))
  ('would-block   (message "Sorry, can't do it now"))
  ('read-only     (message "The schmilblick is read-only"))
  ('access-denied (message "You do not have the needed rights"))

  ;; default
  (code           (message "Unknown return code %S" code)))

;; while
(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(defun triangle (number)
  "Return sum of numbers 1 through NUMBER inclusive."
  (let ((total 0))
    (while (> number 0)
      (setq total (+ total number))
      (setq number (1- number)))
    total))

;; dolist dotimes
;; dolist works like a while loop that CDRs down a list
;; (dolist (VAR LIST [RESULT]) BODY...)

;; reverse a list
(let (value) ; make sure list starts empty
  (while list
    (setq value (cons (car list) value)) ; ⇒ (a . nil) ⇒ (b . nil) ⇒ (c . (b a))
    (setq list (cdr list)))
  value))

(let (value)  ; make sure list starts empty
(dolist (element list value)
  (setq value (cons element value))))

;; (dotimes (VAR COUNT [RESULT]) BODY...)
(let (value)
  (dotimes (number 3)
    (setq value (cons number value)))
  value)
; ⇒ (2 1 0)

;; generators
;; [Generators](https://www.gnu.org/software/emacs/manual/html_node/elisp/Generators.html)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; let
;; [Lexical & Dynamic Binding Differences](https://www.gnu.org/software/emacs/manual/html_node/eintr/Lexical-_0026-Dynamic-Binding-Differences.html)

(let ((zebra "stripes")
      (tiger "fierce")
      (unitialised) ; ⇒ nil
      )
  (message "One kind of animal has %s and another is %s."
           zebra tiger))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Function
;; [Functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Functions.html)

(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (* 7 number))
(multiply-by-seven 3)

;; Interactive version.
;; [Using Interactive](https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html)
(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (interactive "p")
  (message "The result is %d" (* 7 number)))

;; &optional &rest
;; [Argument List](https://www.gnu.org/software/emacs/manual/html_node/elisp/Argument-List.html)
(defun foo (a b &optional c d &rest e)
  (message "%s %s [%s %s] * %s"  a b c d e))
(foo 1 2)
;; ⇒ "1 2 [nil nil] * nil"
(foo 1 2 3)
;; ⇒ "1 2 [3 nil] * nil"
(foo 1 2 3 4)
;; ⇒ "1 2 [3 4] * nil"
;(foo 1 2 2 3 4 5 6)
;; ⇒ "1 2 [2 3] * (4 5 6)"

;; Keyword Arguments
;; [EmacsWiki : Keyword Arguments](https://www.emacswiki.org/emacs/KeywordArguments)

(cl-defun foobar (&key foo bar)
  "`foobar' takes keyword arguments of :foo and :bar like this:
\(foobar :foo 23)
\(foobar :bar 42)"
  (format "foo is %S and bar is %S" foo bar))

;; or emulated using plist-get
;;  (plist-get PLIST PROP &optional PREDICATE)
;;    Extract a value from a property list.
;;    PLIST is a property list, which is a list of the form (PROP1 VALUE1 PROP2 VALUE2...)
(defun foobar-plist (&rest args)
  "`foobar-plist' takes keyword arguments of :foo and :bar like this:
\(foobar-plist :foo 23)
\(foobar-plist :bar 42)"
  (format "foo is %S and bar is %S" (plist-get args :foo) (plist-get args :bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lambda

(lambda (a b c) (+ a b c))
(funcall (lambda (a b c) (+ a b c))
         1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macro
;;
;; [Macros](https://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html)
;;
;; Macros enable you to define new control constructs and other language features. A macro is
;; defined much like a function, but instead of telling how to compute a value, it tells how to
;; compute another Lisp expression which will in turn compute the value. We call this expression the
;; expansion of the macro.

(defmacro inc (var)
   (list 'setq var (list '1+ var)))
(macroexpand '(inc r)) ; ⇒ (setq r (1+ r))
