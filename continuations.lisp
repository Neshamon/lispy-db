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
;; Continuations are functional objects that contain the frozen state
;; of a computation & a programming concept originating from Scheme;
;; They are essentially programs frozen in action.
;; When a frozen object is evaluated, the stored computation is
;; restarted where it left off. Continuations manage this by
;; using its own copy of the frozen stack and by ignoring the current stack.
;; How continuations achieve this is by utilizing a function and
;; a pointer to the whole stack pending at the moment of creation.

;; Continuations can be useful in solving specific types of problems whether it may be
;; representing suspended processes in multiprocessing or representing nodes in a search
;; tree.

;; To depict functions that have continuations we will prepend a function name with the
;; "=" symbol.


(setq *cont* #'identity)

;; At the toplevel, the value of *cont* is identity, as you can see.
;; Identity returns whatever is passed to it in the fashion of a self
;; evaluating atom.

;; By manipulating *cont* we can get the effect of continuations.
;; Though *cont* has a global value, it is rarely the one to be used;
;; *cont* will 99% of the time be a parameter passed through continuation
;; macros.

;; This is important to understand because if *cont* were not a local variable,
;; none of these continuation macros would work. This is why *cont*
;; is defined with a setq rather than a defvar, which would give *cont* a
;; special status as well.

;; Knowing these things, it is important to remember that anywhere a continuation
;; function is defined, as we will see further down this file, *cont* is
;; going to be utilized under the hood.

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
;; The functions below must be run in a Lisp repl

(defstruct proc pri state wait)

#|
pri is the priority of each process

state is a continuation representing the state of a suspended process.
A suspended process can be restarted by funcalling the state.

wait is a function which must return true for the process to be restarted.
Initially the wait of a new process is nil, but a process with a null wait
can still be restarted.
|#

(proclaim '(special *procs* *proc*)) ; Does special define procs and proc for me?

(defvar *procs*)
(defvar *proc*) ; Did these need to be defined?

#| As shown in the previous section of code with CPS programming,
the variables *procs* & *procs* act as continuations and are
interacted with in nearly all subsequent functions.

|#

(defvar *halt* (gensym))

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
  "As noted earlier, a process can be brought out
   of suspension when either the proc-state
   continuation is called or the proc-wait symbol
   returns t.

   Pick-process calls the continuation proc-state
   to activate the process on the variable p
   & removes p from *procs* to show that this new
   process has been activated.
   Sorting through processes with the highest
   proc-pri is relegated to most-urgent-process."
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
          *procs* (delete p *procs*))
    (funcall (proc-state p) val)))

(defun arbitrator (test cont)
  "Arbitrator is a function created in CPS style.
   As you can see, the first parameter is test but the other
   is cont, which stands for continuation.

   The arbitrator function uses generalized variables by setting
   the funcall of proc-state on *proc* to the parameter, cont,
   and the funcall of proc-wait to the given predicate, test.

   When the expression (proc-state *proc*) or (proc-wait *proc*)
   is evaluated we are effectively popping off the previous
   continuation in a similar fashion as (restart-cont) which
   is defined in the first section. We then assign the
   evaluation of proc-state and proc-wait to the given
   parameters, cont & test. When proc-state & proc-wait are
   assigned to the given parameters, they are stored within the
   proc structure.

   The proc structure is where the current continuation of a
   process is stored. But before the continuation be stored,
   the current proc is pushed into the *procs* object, which
   is where all the suspended processes are stored.

   As noted within the proc struct docstring, in order
   for a process to be restarted from a suspended
   state either the proc-state function must be called or
   the proc-wait function must evaluate to t.

   This process takes place by way of the pick-process function
   in the last form."
  (setf (proc-state *proc*) cont
        (proc-wait *proc*) test)        ; Is test itself a continuation?
  (push *proc* *procs*)
  (pick-process))

(defmacro wait (param test &body body)
  "The wait macro takes the arbitrator function and
   abstracts it by allowing it to be given any arbitrary test
   and list of params to act on the variable body"
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
    (format t "Entering ~A~%. " d)))

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
  "Programs defined by this macro must be called
   only from the toplevel, aka within a Lisp env / repl"
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

;;; Demo

(program ped ()
  (fork (pedestrian) 1))

(=defun foo (x)
  (format t "Foo was called with ~A.~%" x)
  (=values (1+ x)))

(defmacro pull (obj place &rest args)
  `(asetf ,place (delete ,obj it ,@args)))

(defvar *bboard* nil
  "Bboard is the mechanism through which the function arbitrator
   facilitates which continuation gets the priority of going first
   without assessing the assigned priority, proc-pri.")

(defun claim (&rest f)
  "Appends the params,
   f to *bboard*"
  (push f *bboard*))

(defun unclaim (&rest f)
  "Removes the params f from
   *bboard*"
  (pull f *bboard* :test #'equal))

(defun check (&rest f)
  "Checks to see if the params f is
   inside *bboard*"
  (find f *bboard* :test #'equal))

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

#| When running the program ballet you will see that each portion of
the functions =host & =visitor are called harmoniously.

The macro wait enables the two functions =host & =visitor to work
with each other. If you run the ballet function in a Lisp repl you should
see that it returns:

Approach DOOR2. Open DOOR2. Enter DOOR2. Close DOOR2.
Approach DOOR1. Open DOOR1. Enter DOOR1. Close DOOR1.

Which is very interesting, because if you look at the ballet program,
DOOR2 should not be the first symbol to be returned if we follow the
normal way of evaluating the program, but rather DOOR1 should be.

The reason why DOOR2 comes first is precisely because of the wait macro,
and the power of continuations at work.

When visitor is run it will always print out, "Approach (var here)".
Oddly enough this should be all the more justification for
DOOR1 to be printed first. But due to the implicit nature of
Lisp returning the last form, we can begin to understand
why DOOR1 is not printed first.

The last form of the =visitor function has the wait macro
at the car (head / zeroth index) of it. And if you know how
function calls work within Lisp, the car of a list is 99% of the
time a function name, thus Lisp will evaluate it as a function.
Lisp saw that the wait macro had a test that needed to be passed
before the body of wait (the list where wait's logic lives) could
be run. And that test refers back to a previously defined function,
check.

Now we must remember the difference between macros and functions in
Lisp. When you look at a function definition in any language, you
can get a good gauge of what the function will do when evaluated.
But one main difference between a macro and a function is, macros
do not return values like functions do, or rather macros return
more functions. We call this returning of functions a macroexpansion
in Lisp.

If we look at the definition of the wait macro, we can see that it uses
the arbitrator function at its core.

The function, check, searches the variable *bboard* for the given param.
Inside the context of =visitor, the wait macro was looking to see if
the symbol 'open was inside of *bboard*. This simple test is both
the constraint of the continuation in this program yet also its very
power. Without this simple check the mechanism of continuation would
fall apart.

Due to the lack of 'open being inside of *bboard*, none of the code
within the body of =visitor's wait macro will be run until this test
is successfully passed.

Given that the constraint of =visitor's wait not being met it is
easy to think that =host's may not be met either. But this is
the very moment at which we can finally understand why DOOR2
was printed first instead of DOOR1.

Right before the wait form was evaluated, we can see that the =visitor
function adds the symbol 'knock to *bboard*. Though Lisp only returns
the last form of a given function, it still evaluates all previous forms.
Thus, though =visitor was frozen by an unmet constraint we can atleast
know that 'knock was very likely to have been added to *bboard*
by way of the function claim.

Why is this important? This matters because if we look at =host's
constraint on its wait macro, we can see that it's checking to see if
the symbol 'knock is within *bboard*. And if we look into the body of
=host's wait macro we can see

|#
