;;; Continuations
;; Continuations are a programming concept originating from
;; Scheme, and they are essentially programs frozen in action.
;; When a frozen object is evaluated the stored
;; computation is restarted where it left off.
;; Continuations manage this by using its own copy of the frozen
;; stack and by ignoring the current stack.
;; How continuations achieve this is by utilizing a function and
;; a pointer to the whole stack pending at the moment of creation.

;; Continuations can be useful in solving specific types of problems whether it may be
;; representing suspended processes in multiprocessing or representing nodes in a search
;; tree.

;; To depict functions that are continuations we will prepend a function name with the
;; "=" symbol.


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
  `(let ((*cont* #'(lambda ,params ,@body)))
     ,expr))

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))
