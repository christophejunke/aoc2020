(defpackage :aoc2020.16
  (:use :aoc2020)
  (:export #:test))

(in-package :aoc2020.16)

(defstruct (input (:conc-name)) rules my-ticket nearby-tickets)

(defun input (in)
  (let ((chunk 0) rules my-ticket nearby-tickets)
    (labels ((section-1 (lines)
               (mapcar (lambda (s)
                         (destructuring-bind (tag exp) (split ": " s)
                           (scanner-bind ("%d-%d or %d-%d" x1 y1 x2 y2) exp
                             (list tag (cons x1 y1) (cons x2 y2)))))
                       lines))
             (uncsv (lines)
               (mapcar (lambda (s)
                         (mapcar #'parse-integer
                                 (split #\, s)))
                       lines))
             (section-2/3 (lines)
               (uncsv (rest lines)))
             (process (lines)
               (ecase (incf chunk)
                 (1 (setf rules (section-1 lines)))
                 (2 (setf my-ticket (first (section-2/3 lines))))
                 (3 (setf nearby-tickets (section-2/3 lines))))))
      (map-line-chunks in #'process)
      (make-input :rules rules
                  :my-ticket my-ticket
                  :nearby-tickets nearby-tickets))))

(defun interval-union (a b)
  (cond
    ((and a b)
     (destructuring-bind (al . ah) a
       (destructuring-bind (bl . bh) b
         (assert (<= al ah))
         (assert (<= bl bh))
         (cond
           ((or (< ah (1- bl)) (< bh (1- al)))
            (sort (list a b) #'< :key #'car))
           (t (list (cons (min al bl)
                          (max ah bh))))))))
    (a (list a))
    (b (list b))))

;; SDI: sorted disjoint intervals (sorted by lower bound)
;; AL, AH: bounds such that AL <= AH
;; LEFT: recursion accumulator, left part of the split in reverse order
;;
(defun split-at (sdi al ah left)
  "Split SDI at an interval (IL . IH) such that AL <= IL.

   Return two values:

     - the reverse left part of SDI excluding (IL . IH)

     - the right part of the split from the first interval (XL . XH), starting
       from (IL . IH), where XH > AH.
"
  (if sdi
      (destructuring-bind (head . tail) sdi
        (if (<= al (car head))
            (values left (member-if (lambda (i) (> (cdr i) ah)) sdi))
            (split-at tail al ah (cons head left))))
      (values left nil)))

(defun intervals-unions (disjoint-intervals interval)
  (if interval
      (destructuring-bind (il . ih) interval
        ;; rev-left is the reversed left part of split
        ;; right is the right part of the split
        (multiple-value-bind (rev-left right) (split-at disjoint-intervals il ih ())
          ;; I is an interval, IS is a list of disjoint intervals sorted by their
          ;; lower bound. I is either strictly below the first element of IS, or
          ;; can be fused with it. Function U computes the new IS by joining I
          ;; with the first element of IS.
          (flet ((U (i is) (nconc (interval-union i (first is)) (rest is))))
            (revappend (rest rev-left) (U (first rev-left) (U interval right))))))
      disjoint-intervals))

(defmacro check-union (a &key with is)
  `(assert (equalp ',is (interval-union ',a ',with))))

(define-test basic-unions
  (progn
    (check-union (5 . 8)  :with (0 . 10) :is ((0 . 10)))
    (check-union (0 . 6)  :with (4 . 10) :is ((0 . 10)))
    (check-union (7 . 10) :with (0 . 8) :is ((0 . 10)))
    (check-union (0 . 6)  :with (7 . 10) :is ((0 . 10)))
    (check-union (7 . 10) :with (0 . 3) :is ((0 . 3) (7 . 10)))))

(defmacro check-unions (in &key is with)
  `(assert (equalp ',is (intervals-unions ,with ',in))))

(define-test multi-unions
  (let ((sdi '((0 . 3) (5 . 8) (10 . 13) (15 . 16))))
    (check-unions ( 1  . 1) :with sdi :is ((0 . 3) (5 . 8) (10 . 13) (15 . 16)))
    (check-unions ( 1  . 4) :with sdi :is ((0 . 8) (10 . 13) (15 . 16)))
    (check-unions ( 4  . 4) :with sdi :is ((0 . 8) (10 . 13) (15 . 16)))
    (check-unions (20 . 24) :with sdi :is ((0 . 3) (5 . 8) (10 . 13) (15 . 16) (20 . 24)))
    (check-unions (14 . 14) :with sdi :is ((0 . 3) (5 . 8) (10 . 16)))
    (check-unions ( 5 . 25) :with sdi :is ((0 . 3) (5 . 25)))
    (check-unions ( 5 . 13) :with sdi :is ((0 . 3) (5 . 13) (15 . 16)))
    (check-unions ( 4 . 13) :with sdi :is ((0 . 13) (15 . 16)))
    (check-unions (20 . 24) :with sdi :is ((0 . 3) (5 . 8) (10 . 13) (15 . 16) (20 . 24)))
    (check-unions (20 . 24) :with ()  :is ((20 . 24)))
    (check-unions ()        :with ()  :is ())))

(defun belongs-to (x sdi)
  (some (lambda (i) (<= (car i) x (cdr i))) sdi))

(defun fuse-intervals (intervals)
  (reduce #'intervals-unions intervals :initial-value nil))

(defun input-rule-intervals (input)
  (mappend 'rest (rules input)))

(defun filtered-input (input)
  (let ((sdi (fuse-intervals (input-rule-intervals input))))
    (flet ((validp (u) (belongs-to u sdi)))
      (make-input :my-ticket (coerce (my-ticket input) 'vector)
                  :rules (rules input)
                  :nearby-tickets (loop
                                    for ticket in (nearby-tickets input)
                                    when (every #'validp ticket)
                                    collect (coerce ticket 'vector))))))

(define-test fusion-filter
  (assert
   (equalp (fuse-intervals (input-rule-intervals (input "16-t1")))
           '((1 . 3) (5 . 11) (13 . 50))))
  (assert (equalp (filtered-input (input "16-t1"))
                  (load-time-value
                   (make-input
                    :rules '(("class" (1 . 3) (5 . 7)) ("row" (6 . 11) (33 . 44))
                             ("seat" (13 . 40) (45 . 50)))
                    :my-ticket #(7 1 14)
                    :nearby-tickets '(#(7 3 47))))))
  (assert (equalp (filtered-input (input "16-t2"))
                  (load-time-value
                   (make-input
                    :rules '(("class" (0 . 1) (4 . 19)) ("row" (0 . 5) (8 . 19))
                             ("seat" (0 . 13) (16 . 19)))
                    :my-ticket #(11 12 13)
                    :nearby-tickets '(#(3 9 18) #(15 1 5) #(5 14 9)))))))

(defun filtered/cols (i)
  (let ((input (filtered-input (input i))))
    (loop
      with cols = (make-array (length (rules input)) :initial-element nil)
      for entry in (nearby-tickets input)
      do (loop for i below (length cols) do (push (aref entry i) (aref cols i)))
      finally (return (values input (coerce cols 'list))))))

(defun candidate-columns (cols rules)
  ;; collect (col #(n0 n1 ... nm)) for each column when n0 to nm are the names
  ;; of rules that are satisfied by all the values in that column in nearby
  ;; tickets. In other words, each vector is the domain of satisfiable rule
  ;; names for each column.
  (sort (loop
          for i from 0
          for column in cols
          collect
             (list i (coerce (loop
                               for (name . sdi) in rules
                               when (every (lambda (v) (belongs-to v sdi))
                                           column)
                               collect name)
                             'vector)))
        #'<
        :key (compose #'length #'second)))

;; Solve the constraint system by filtering domains. For example, if the list
;; of candidates is '((0 #("a")) (1 #("a" "b"))), then column 1 is necessarily
;; associated with rule "b", even though it could also be "a", because "a" is
;; the only possible value for column 0.
;;
;; Sort according the length of domain and recursively prune domains. The
;; function only keeps the columns for which there is only one value possible,
;; which is sufficient for this puzzle (and in fact, there is a unique
;; solution).

(defun solve-candidates% (candidates except)
  (when candidates
    (destructuring-bind ((col domain) . candidates) candidates
      (let ((new-domain (remove-if (lambda (v) (member v except)) domain)))
        (case (length new-domain)
          (0 (error "unsat"))
          (1 (let ((n (aref new-domain 0)))
               (acons n col (solve-candidates% candidates (cons n except)))))
          (t (solve-candidates% candidates except)))))))

(defun solve-candidates (candidates)
  (solve-candidates% candidates nil))

;;; PUZZLE SOLUTIONS

(defun part-1 (&optional (in 16))
  (let* ((input (input in))
         (sdi (fuse-intervals (input-rule-intervals input))))
    (z:collect-sum
     (z:choose-if (lambda (u) (not (belongs-to u sdi)))
                  (z:scan-lists-of-lists-fringe
                   (nearby-tickets input))))))

(defun part-2 (&optional in)
  (multiple-value-bind (in cols) (filtered/cols in)
    (let ((departure-cols
            (mapcar #'cdr
                    (remove-if-not (lambda (s) (search "departure" s))
                                   (solve-candidates
                                    (candidate-columns cols (rules in)))
                                   :key #'car))))
      (reduce #'*
              (loop
                for c in departure-cols
                collect (aref (my-ticket in) c))))))

(define-test test
  (assert (= (part-1 "16-t1") 71))
  (assert (= (part-1) 23009))
  (assert (= 10458887314153 (part-2 16))))

