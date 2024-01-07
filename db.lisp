;;; Abbreviations

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defun group (source n)
  "Groups lists into sublists by taking a list, source,
and grouping it into lengths of, n"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro abbrevs (&rest names)
  "Macro that creates a macro with the functionality of the second name (eg. list) with the
 name of the first name"
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

(abbrevs dbind destructuring-bind
         mvbind multiple-value-bind
         mvsetq multiple-value-setq)

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

(defanaph aif :rule :first)

(defmacro aif2 (test &optional then else)
  "Anaphoric if. This version creates a context using a built in let and assigns
 the variable to 'it'."
  (let ((win (gensym)))
    `(mvbind (it ,win) ,test
             (if (or it ,win) ,then ,else))))

;;; Basic DB functions

(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db))

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))

(defmacro fact (pred &rest args)
  "Adds a record to the db. The first arg is the category
or data type of the record, the second is all relevant information
for that record"
  `(progn (db-push ',pred ',args)
          ',args))

;;; Matching & Lookup

(defun lispy-lookup (pred args &optional binds)
  (mapcan #'(lambda (x)
              (aif2 (match x args binds) (list it)))
          (db-query pred)))

(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query #'atom))
         ,@body))))

(defun interpret-query (expr &optional binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t (lispy-lookup (car expr) (cdr expr) binds))))

(defun interpret-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
                  (interpret-query (car clauses) b))
              (interpret-and (cdr clauses) binds))))

(defun interpret-or (clauses binds)
  (mapcan #'(lambda (c)
              (interpret-query c binds))
          clauses))

(defun interpret-not (clause binds)
  (if (interpret-query clause binds)
      nil
      (list binds)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defmacro acond2 (&rest clauses)
  "Anaphoric cond."
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(defun match (x y &optional binds)
  "Compares args element by element and accumulates values in binds.
 If a match is successful it returns generated bindings, otherwise nil."
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

;;; DB demo

(fact painter reynolds joshua english) ;; Add the painter Joshua Reynolds
(fact painter canale antonio venetian) ;; Add the painter Antonio Canale

(db-query 'painter) ;; Query for all painters
                                        ; => ((CANALE ANTONIO VENETIAN) (REYNOLDS JOSHUA ENGLISH))

(lispy-lookup 'painter '(?x ?y english)) ; => (((?Y . JOSHUA) (?X . REYNOLDS)))

(interpret-query '(and
                   (painter ?x ?y ?z)
                   (dates ?x 1697 ?w))) ;; A combinded query that looks for any painters from the year 1697

(clear-db) ;; Clears the db

(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)
