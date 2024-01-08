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

(defun dft (tree)
  "Depth first traversal"
  (cond ((null tree) nil)
        ((atom tree) (princ tree))
        (t (dft (car tree))
           (dft (cdr tree)))))

(setq *saved* nil)

(=defun restart-cont ()
  (if *saved*
      (funcall (pop *saved*))
      (=values 'done)))
;; The function, restart-cont simply pops the most recently saved continuation
;; and calls it.

(=defun dft-node (tree)
  (cond ((null tree) (restart-cont))
        ((atom tree) (=values tree))
        (t (push #'(lambda () (dft-node (cdr tree)))
                 *saved*)
           (dft-node (car tree)))))
;; The function, dft-node traverses an entire tree but pushes each node to
;; the variable *saved*, which acts as a continuation.

(=defun dft2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
    (cond ((eq node 'done) (=values nil))
          (t (princ node)
             (restart-cont)))))
;; dft2 combines dft-node and restart-cont together.

;; With continuations there is no need for explicit iteration or recursion
;; because the continuations invoked by restart-cont always return
;; through the same cond clause in dft-node. So long as the value, 'done',
;; isn't returned, restart-cont will continue to go down the subsequent stack calls.

;;; Small Demo

(setq t1 '(a (b (d h)) (c e (f i) g))
      t2 '(1 (2 (3 6 7) 4 5)))

(dft2 t1) ; => ABDHCEFIG
(dft2 t2) ; => 1236745

;; The below function binds the previously created lists, t1 & t2, to the variables node1 & node2.
;; Then node1 & node2 are paired in a list

(=bind (node1) (dft-node t1)
  (if (eq node1 'done) ; Error checking?
      'done
      (=bind (node2) (dft-node t2)
        (list node1 node2)))) ; => (A 1)

;; As restart-cont is called, the trees are traversed
(atom t1)
(restart-cont) ; => (A 2)
(restart-cont) ; => (A 3)
(restart-cont) ; => (A 6)
;; ...
(restart-cont) ; => (B 1)
;; And so on...

;;; Continuation Passing Style (CPS)
;; In the same way as the previous continuation functions needed *cont* or *saved*
;; as variables to facilitate continuations, so do these subsequent functions.
;; One main difference is that the functions will have these continuations built in
;; as an extra parameter.

;; Below is an example of writing in Continuation Passing Style

;; Normal List reversal
(defun rev (x)
  "Reverses lists. This function is written in a way that is normal in lisp."
  (if (null x)
      nil
      (append (rev (cdr x)) (list (car x)))))

;; Continuation Passing Style list reversal
(defun revc (x k)
  "This function is rev, but rewritten in a way that passes continuations.
 The parameter 'k' is what would be the continuation passed into this function."
  (if (null x)
      (funcall k nil)
      (revc (cdr x)
            #'(lambda (w)
                (funcall k (append w (list car x)))))))

;; The continuation is a closure representing what should be
;; done with the current value of the function.

(defun rev2 (x)
  (revc x #'identity))

;; During the first recursion, the continuation will be identity.
;; The subsequent recursions will have #'(lambda (w) identity (append w (list (car x))))
;; as the continuation
