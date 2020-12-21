(in-package :aoc2020.19)

;;; ============================================================
;;; Grammar implementation
;;; ============================================================

(defclass grammar-impl (grammar)
  ((fresh-symbols :initarg symbols
                  :accessor %fresh-symbols
                  :initform nil)))

(defmethod updated-grammar-from progn ((o grammar-impl) (n grammar-impl))
  (setf (%fresh-symbols n) (%fresh-symbols o)))

(defun rules-add (production rules)
  (cons production rules))

(defmacro with-rule ((lhs rhs) rule &body body)
  `(adt:with-data (rule.production ,lhs ,rhs) ,rule
     ,@body))

(defun fresh-symbols (grammar qty)
  (let ((*grammar-class* 'grammar-impl))
    (let ((grammar (update-grammar grammar)))
      (values (loop repeat qty
                    for sym = (gensym "GRAM-SYM")
                    do (push sym (%fresh-symbols grammar))
                    collect sym)
              grammar))))

(defun fresh-symbol (grammar)
  (multiple-value-bind (symbols grm) (fresh-symbols grammar 1)
    (values (first symbols) grm)))

(defun add-symbols (grammar symbols)
  (let ((*grammar-class* 'grammar-impl))
    (let ((grammar (update-grammar grammar)))
      (prog1 grammar
        (setf (%fresh-symbols grammar)
              (append symbols (%fresh-symbols grammar)))))))

;;; ============================================================
;;; Parsed grammar to Chomsky Normal Form
;;; ============================================================

;;; Ref. https://www.cs.bgu.ac.il/~auto202/wiki.files/9a.pdf
;;; and Wikipedia

;;;;  START
;;;;#########

(defun chomsky/start (grammar)
  (multiple-value-bind (start-symbol grammar) (fresh-symbol grammar)
    (let ((rule (production start-symbol (lhs (start grammar)))))
      (update-grammar grammar
                      :start rule
                      :rules (rules-add rule (rules grammar))))))

;;;;  TERM
;;;;#########

;; input data already have terminals in dedicated rules
(defun chomsky/term (grammar)
  grammar)

;;;;  BIN
;;;;#########
;;
;; BIN: Eliminate right-hand sides with more than 2 nonterminals
;; Replace each rule A → X1 X2 ... Xn
;; with more than 2 nonterminals X1,...,Xn by rules
;;     A → X1 A1,
;;     A1 → X2 A2,
;;     ... ,
;;     An-2 → Xn-1 Xn,
;;
;; where Ai are new nonterminal symbols.

(defun bin-rule (rule)
  (adt:with-data (rule.production lhs body) rule
    (adt:match rule-body body
      ((rule.term _) rule)
      ((rule.expr terms)
       (ematch terms
         ((list _) rule)
         ((list _ _) rule)
         ((list* var rest)
          (let ((new (gensym)))
            (values (rule.production lhs (rule.expr (list var new)))
                    (list (rule.production new (rule.expr rest)))
                    (list new)))))))))

;; AOC2020.19> (bin-rules (list (RULE.PRODUCTION 1 (RULE.EXPR '(119 65 32 82 29)))))
;; ((RULE.PRODUCTION #:G954 (RULE.EXPR (82 29)))
;;  (RULE.PRODUCTION #:G953 (RULE.EXPR (32 #:G954)))
;;  (RULE.PRODUCTION #:G952 (RULE.EXPR (65 #:G953)))
;;  (RULE.PRODUCTION 1 (RULE.EXPR (119 #:G952))))
;; (#:G954 #:G953 #:G952)

(defun bin-rules (rules &optional result new-vars)
  (if rules
      (destructuring-bind (rule . rules) rules
        (multiple-value-bind (done todo vars) (bin-rule rule)
          (bin-rules (nconc todo rules)
                     (cons done result)
                     (nconc vars new-vars))))
      (values result new-vars)))

(defun chomsky/bin (grammar)
  (multiple-value-bind (new-rules new-vars) (bin-rules (rules grammar))
    (update-grammar (add-symbols grammar new-vars)
                    :start (find (lhs (start grammar)) new-rules :key #'lhs)
                    :rules new-rules)))

;;;;  UNIT
;;;;#########
;;
;; A unit production is where RHS has only one symbol.  Consider
;; production A → B. Then for every production B → α, add the
;; production A → α. Repeat until done (but don’t re-create a unit
;; production already deleted).

(defun unit-rule (rules rule deleted)
  (adt:with-data (rule.production lhs b) rule
    (adt:match rule-body b
      ((rule.term _) (values (list rule) deleted))
      ((rule.expr terms)
       (case (length terms)
         (1 (destructuring-bind (rhs) terms
              (let ((deleted (pushnew rule deleted :test #'equalp)))
                (values
                 (mapcan
                  (lambda (rule)
                    (let ((new (rule.production lhs (rhs rule))))
                      (unless (member new deleted :test #'equalp)
                        (list new))))
                  (remove rhs rules :test-not #'eql :key #'lhs))
                 deleted))))
         (t (values (list rule) deleted)))))))

(defun chomsky/unit (grammar)
  (let ((g-rules (rules grammar)))
    (labels ((recurse (rules res del)
               (if rules
                   (destructuring-bind (rule . rules) rules
                     (multiple-value-bind (done del) (unit-rule g-rules rule del)
                       (recurse rules (nconc done res) del)))
                   (values res del))))
      (multiple-value-bind (new-rules deleted) (recurse g-rules nil nil)
        (let ((new-start (find (lhs (start grammar)) new-rules :key #'lhs)))
          (values (update-grammar grammar
                                  :start new-start
                                  :rules new-rules)
                  deleted))))))

;;; ============================================================
;;; Compress CNF grammar
;;; ============================================================
;;
;; grammar in normal forms are either A -> BC, with BC non-terminals,
;; or A -> t with t a terminal.
;;
;; - replace temporary symbols by integer indices, so that they are
;;   unique in the resulting grammar.
;; - sort by production index
;; - store rules in an array, the index is the position in the array
;; - only store a character, or a cons pair of integers
;; - START is an index

(defun symbol-number-mapping (grammar)
  "create mapping from fresh symbols to never-used integer indices"
  (flet ((id (r) (let ((id (lhs r))) (and (typep id 'fixnum) (list id)))))
    (let ((max (reduce #'max (mapcan #'id (rules grammar)))))
      (alist-hash-table (loop
                          for x from (1+ max)
                          for s in (%fresh-symbols grammar)
                          collect (cons s x))))))

(defclass cnf-grammar (grammar)
  ((rmap :initform (make-hash-table :test #'equal) :reader reverse-map)))

(defmethod initialize-instance :after ((g cnf-grammar) &key)
  (let ((m (reverse-map g)))
    (loop for (id . rule) across (rules g) do
      (pushnew id (gethash rule m)))))

(defun compress-normal-form (grammar)
  (check-type grammar grammar-impl)
  (let ((mapping (symbol-number-mapping grammar)))
    (labels ((resolve% (s) (if (numberp s) s (gethash s mapping)))
             (resolve (s) (or (resolve% s) (error "not found")))
             (coerce-body (body)
               (adt:match rule-body body
                 ((rule.term c) c)
                 ((rule.expr list)
                  (destructuring-bind (a b) list
                    (list (resolve a) (resolve b))))))
             (coerce-rule (rule)
               (with-rule (lhs rhs) rule
                 (cons (resolve lhs) (coerce-body rhs)))))
      (let* (;; transform and collect as a vector
             (rules (map 'simple-vector #'coerce-rule (rules grammar)))
             ;; insertion sort would be better, but that's ok here
             (rules (sort rules #'< :key #'first))
             ;; let start be the index
             (start (resolve (lhs (start grammar)))))
        (let ((*grammar-class* 'cnf-grammar))
          (values (make-grammar :rules rules
                                :start start)
                  mapping))))))

(defun chomsky-normal-form (grammar)
  (compress-normal-form
   (reduce (lambda (g f) (funcall f g))
           ;; following Wikipedia et al. advice about how to order transforms
           '(chomsky/start
             chomsky/term
             chomsky/bin
             chomsky/unit)
           :initial-value grammar)))
