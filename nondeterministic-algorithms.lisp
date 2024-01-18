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

(defparameter *paths* nil)
(defconstant failsym '@)

(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym))

(defmacro choose (&rest choices)
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

(parlor-trick 8)
