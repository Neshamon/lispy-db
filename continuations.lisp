;;; Anaphora

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defmacro defanaph (name &optional &key calls (rule :all))
  "Defines anaphoric functions with three rules. Anaphora are good for utilizing variable capture.
The rule :all evaluates all args in a macro, and assigns always 'it' to the previous argument.
The rule :first only evaluates the first arg, and 'it' will be bound to its value.
The rule :place treats the first arg as a generalized variable, and 'it' will be bount its value."
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun anaphex1(args call)
  "All args in the macro call will be evaluated, with 'it' always
bound to the value of the previous argument"
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
      call))

(defun anaphex2 (op args)
  "Only the first arg will be evaluated, and 'it' will be
bound to its value"
  `(let ((it ,(car args)))
     (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  "The first argument will be treated as a generalized variable,
 and 'it' will be bound to its value"
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

(defmacro _f (op place &rest args)
  "Applies a function !destructively! to a generalized variable."
  (multiple-value-bind (vars forms var set access) (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defanaph asetf :rule :place)

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

;;;; Multiple Processes

(defstruct (proc pri state wait)
  "pri is the priority of each process

   state is a continuation representing the state of a suspended process.
   A suspended process can be restarted by funcalling the state.

   wait is a function which must return true for the process to be restarted.
   Initially the wait of a new process is nil, but a process with a null wait
   can still be restarted.")

(proclaim '(special *procs* *proc*)) ; Does special define procs and proc for me?

(defvar *procs*)
(defvar *proc*) ; Did these need to be defined?

(defvar *halt* (gensym))

(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
        (proc-wait *proc*) test)
  (push *proc* *procs*)
  (pick-process))

(defun most-urgent-process ()
  (let ((proc1 *default-proc*)
        (max -1)
        (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
        (if (> pri max)
            (let ((val (or (not (proc-wait p))
                           (funcall (proc-wait p)))))
              (when val
                (setq proc1 p
                      max pri
                      val1 val))))))
    (values proc1 val1)))

(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
          *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

(defmacro wait (param test &body body)
  "Wait is similar to =bind"
  `(arbitrator #'(lambda () ,test)
               #'(lambda (,param) ,@body)))

(defmacro yield (&body body)
  "Similar to wait. This macro gives processes with higher priority
   a chance to run."
  `(arbitrator nil #'(lambda (,(gensym)) ,@body)))

(defun setpri (n)
  (setf (proc-pri *proc*) n))

(defun halt (&optional val)
  (throw *halt* val))

(defun kill (&optional obj &rest args)
  (if obj
      (setq *procs* (apply #'delete obj *procs* args))
      (pick-process)))

(defvar *open-doors* nil)

(=defun pedestrian ()
  (wait d (car *open-doors*)
        (format t "Entering ~A~%" d)))

(defmacro fork (expr pri)
  "Instantiates a process from a function call"
  `(prog1 ',expr
     (push (make-proc
            :state #'(lambda (,(gensym))
                       ,expr
                       (pick-process))
            :pri ,pri)
           *procs*)))

(defmacro program (name args &body body)
  `(=defun ,name ,args
     (setq *procs* nil)
     ,@body
     (catch *halt* (loop (pick-process)))))

(defvar *default-proc*
  (make-proc :state #'(lambda (x)
                        (format t "~%>> ")
                        (princ (eval (read)))
                        (pick-process)))
  "This process runs only when no other processes can. Similar to the
   top level of Lisp.")

(program ped ()
  (fork (pedestrian) 1))

(=defun foo (x)
  (format t "Foo was called with ~A.~%" x)
  (=values (1+ x)))

(defmacro pull (obj place &rest args)
  `(asetf ,place (delete ,obj it ,@args)))

(defvar *bboard* nil)

(defun claim (&rest f) (push f *bboard*))

(defun unclaim (&rest f) (pull f *bboard* :test #'equal))

(defun check (&rest f) (find f *bboard* :test #'equal))

(=defun visitor (door)
  (format t "Approach ~A. " door)
  (claim 'knock door)
  (wait d (check 'open door)
    (format t "Enter ~A. " door)
    (unclaim 'knock door)
    (claim 'inside door)))

(=defun host (door)
  (wait k (check 'knock door)
    (format t "Open ~A. " door)
    (claim 'open door)
    (wait g (check 'inside door)
      (format t "Close ~A.~%" door)
      (unclaim 'open door))))

(program ballet ()
  (fork (visitor 'door1) 1)
  (fork (host 'door1) 1)
  (fork (visitor 'door2) 1)
  (fork (host 'door2) 1))
