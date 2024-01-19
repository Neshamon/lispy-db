;;; Needed Macros

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
          syms)
     ,@body))

;;; Continuation Passing Functions / Macros

(setq *cont* #'identity)

(defmacro =lambda (params &body body)
  `#'(lambda (*cont* ,@params) ,@body))

(defmacro =defun (name params &body body)
  (let ((f (intern (concatenate 'string
                                "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,params
         `(,',f *cont* ,,@params))
       (defun ,f (*cont* ,@params) ,@body))))

(defmacro =bind (params expr &body body)
  "=bind is used like multiple-value-bind in that it takes
   a list of parameters, an expression, and a body of code.
   The parameters are bound to the values returned by the
   expression, and the body is evaluated with those bindings.

   =bind creates a new *cont* variable by binding it to a
   continuation. The continuation consists of the lambda list
   of parameters & the body of the function =bind was called on

   The logic of the given expression is then called on the newly
   created continuation.

   It is advised that whenever passing an expression to =bind,
   the expression should include be a continuation function,
   otherwise the chain of continuations will be broken.

   Acceptable functions are functions that either terminate by
   returning values with =values, or by calling another function
   that obeys this restriction. "
  `(let ((*cont* #'(lambda ,params ,@body)))
     ,expr))

(defmacro =values (&rest retvals)
  "=values is similar to values;
   It can return multiple values if
   there is a =bind with the same
   number of arguments waiting for them
   but can't return mutliple values to the
   toplevel.

   When =values is expanded it will capture
   *cont*, and use it to simulate returning
   from the function."
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))

;;; Nondeterministic Algorithms

#|

These algorithms focus on making nondeterministic algorithms
deterministic. This is done through the choose & fail operators
With these operators we can guide a nondeterministic program
down specific paths, thus enabling it to become deterministic.

|#

(defparameter *paths* nil
  "The variable paths acts as a
   store for contiuations.")

(defconstant failsym '@
  "The variable failsym acts as
   the symbol returned when there
   are no more available
   continuations in the paths list")

(defun fail ()
  "The fail function checks if
   there are elements in the
   variable *paths*. If so,
   fail will pop the first
   continuation off of the
   *paths* list and call the it.

   If there are no more
   continuations stored in *paths*,
   fail will return the failure
   symbol, failsym."
  (if *paths*
      (funcall (pop *paths*))
      failsym))

(defmacro choose (&rest choices)
  "The choose macro works by simulating correct guesses
   on given choices. "
  (if choices
      `(progn
         ,@(mapcar #'(lambda (c)
                       `(push #'(lambda () ,c) *paths*))
                   (reverse (cdr choices)))
         ,(car choices))
      '(fail)))

(defun cb (fn choices)
  (if choices
      (progn
        (if (cdr choices)
            (push #'(lambda () (cb fn (cdr choices)))
                  *paths*))
        (funcall fn (car choices)))
      (fail)))

(defmacro choose-bind (var choices &body body)
  "The macros choose-bind & choose essentially
   do the same thing.

   choose-bind operates by binding the chosen
   symbol and evaluating the given body of code"
  `(cb #'(lambda (,var) ,@body) ,choices))

(=defun two-numbers ()
  (choose-bind n1 '(0 1 2 3 4 5)
    (choose-bind n2 '(0 1 2 3 4 5)
      (=values n1 n2))))

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
    (if (= (+ n1 n2) sum)
        `(the sum of ,n1 ,n2)
        (fail))))

(parlor-trick 8) ; => (THE SUM OF 3 5)

;;; Augmented Transition Networks

#| Augmented Transition Networks (ATNs) are a form of parser discovered by
Bill Woods in the 1970s. They are an early & much more limited form of
Natural Language Processing algorithms for English.

A transition network is a set of nodes joined together by directed arcs,
which is essentially a flow chart. Conditions are connected to each arc
which must be met before the arc can be followed.

Terminal nodes of a transition network will utilized the accumulated
information to build list structures.
|#

(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))

;; pos represents the input of the current pointer (an integer)
;; regs represents the current set of registers (a list of assoc-lists)

(defun compile-cmds (cmds)
  (if (null cmds)
      'regs
      `(,@(car cmds) ,(compile-cmds (cdr cmds)))))

(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))

(defun types (word)
  "A primitive dictionary that associates
   each word with a list of one or more
   simple grammatical roles."
  (case word
    ((do does did) '(aux v))
    ((time times) '(n v))
    ((fly flies)  '(n v))
    ((like) '(v prep))
    ((liked likes) '(v))
    ((a an the) '(det))
    ((arrow arrows) '(n))
    ((i you he she him her it) '(pron))))

(defmacro down (sub next &rest cmds)
  `(if (= (length *sent*) pos)
       (fail)
       (let ((* (nth pos *sent*)))
         (if (member ',cat (types *))
             (,next (1+ pos) ,(compile-cmds cmds))
             (fail)))))

(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
     (=values ,expr pos (cdr regs))))

(defmacro getr (key &optional (regs 'regs))
  "Reads registers"
  `(let ((result (cdr (assoc ',key (car ,regs)))))
     (if (cdr result)
         result
         (car result))))

(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs))
         (cdr ,regs)))

(defmacro setr (key val regs)
  "Sets registers"
  `(set-register ',key (list ,val) ,regs))

(defmacro pushr (key val regs)
  "Pushes to registers"
  `(set-register ',key
                 (cons ,val (cdr (assoc ',key (car ,regs))))
                 ,regs))

(defmacro with-parses (node sent &body body)
  "Enables us to invoke an ATN"
  (with-gensyms (pos regs)
    `(progn
       (setq *sent* ,sent)
       (setq *paths* nil)
       (=bind (parse ,pos ,regs) (,node 0 '(nil))
         (if (= ,pos (length *sent*))
             (progn ,@body
                    (fail))
             (fail))))))

(defnode mods
    (cat n mods/n
         (setr mods *)))

(defnode mods/n
    (cat n mods/n
         (pushr mods *))
  (up `(n-group ,(getr mods))))

(defnode np
    (cat det np/det
         (setr det *))
  (jump np/det
        (setr det nil))
  (cat pron pron
       (setr n *)))

(defnode pron
    (up `(np (pronoun ,(getr n)))))

(defnode np/det
    (down mods np/mods
          (setr mods *))
  (jump np/mods
        (setr mods nil)))

(defnode np/mods
    (cat n np/n
         (setr n *)))

(defnode np/n
    (up `(np (det ,(getr det))
             (modifiers ,(getr mods))
             (noun ,(getr n))))
  (down pp np/pp
        (setr pp *)))

(defnode np/pp
    (up `(np (det ,(getr det))
             (modifiers ,(getr mods))
             (noun ,(getr n))
             ,(getr pp))))

#| The difference between Paul Graham's Common Lisp implementation
& SBCL seem to be too great for me to figure out how this code
should be run. As of now, the ATN does not work. Will debug more
as I become more knowledgable about Common Lisp. |#
