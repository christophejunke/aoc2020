(defpackage :aoc2020.19
  (:use :aoc2020))

(in-package :aoc2020.19)

(defun chomsky-input (&optional (in (input 19)))
  (chomsky-normal-form (grammar in)))

;; ============================================================
;; Cocke–Younger–Kasami algorithm
;; https://en.wikipedia.org/wiki/CYK_algorithm
;; ============================================================

(defun cyk-match-p (grammar string)
  (let* ((length (length string))
         (revmap (reverse-map grammar))
         (matrix (make-array (list length length)
                             :element-type 'list
                             :initial-element nil)))
    (flet ((lookup (x) (gethash x revmap))
           ;; -1 to translate size as index in zero-based arrays.
           (store (id lev pos) (push id (aref matrix (1- lev) pos)))
           (at       (lev pos)          (aref matrix (1- lev) pos)))

      (declare (inline at store lookup))

      ;; MATRIX[L,S] = { V1 .. Vn }
      ;;
      ;; Matrix is a two-dimensional array of sets that is used to
      ;; remember the non-terminal V1 ... Vn that generate the
      ;; substring of length L starting at position S in the input
      ;; string.
      ;;
      ;; The first dimension is the length of the substring.
      ;; The seconds dimension is the starting point of a substring.

      ;; Initialize length 1 with the set of non-terminals leading to each
      ;; character in input string.
      (dotimes (S length)
        (let ((c (char string S)))
          (dolist (R (lookup c))
            (store R 1 S))))

      ;; Propagate rules from smaller to larger substrings.
      ;; L is the size of each substring.
      (loop :for L :from 2 :to length :do
        ;; For each substring size, consider all starting points.
        ;; S is the position of the substring in the input string.
        (loop :for S :from 0 :to (- length L) :do
          ;; The substring is split in two parts, at varying points.
          ;; For example, "abc" is split as "a,bc" and "ab,c".
          ;; P is the relative index in the substring where the split occurs.
          (loop :for P :from 1 :below L :do
            ;; Cartesian product, for all non-terminals B generating the left
            ;; part of the substring (S) and for all non-terminals C generating
            ;; the right part of the substring (S+P). For each couple (B,C),
            ;; find rules that generates BC, ie. A -> BC; the non-terminals A
            ;; generate the whole substring, starting at S.
            ;;
            ;;     [~~~~~~~~~~~~ L ~~~~~~~~~~~~[
            ;;     :                           :
            ;;     [~~ P ~~[-~~~~~~ L-P ~~~~~~~[
            ;;     :       :                   :
            ;; ====////////%%%%%%%%%%%%%%%%%%%%======== STRING
            ;;     :   B   :         C
            ;;     :       :
            ;;     S      S+P
            ;;     :
            ;;     ############################
            ;;                  A
            ;;
            (loop :for B :in (at P S) :do
              (loop :for C :in (at (- L P) (+ S P)) :do
                ;; find rule such that A -> BC
                (dolist (A (lookup (list B C)))
                  ;; mark reachability
                  (store A L S)))))))

      (member (start grammar)
              (at length 0)))))

(defun inputs (&optional (name 19))
  (let* ((i0 (input name))
         (g0 (grammar i0))
         (g1 (update-grammar g0
                             :rules (list* (production 8 42 8)
                                           (production 11 42 11 31)
                                           (rules g0)))))
    (values (entries i0)
            (chomsky-normal-form g0)
            (chomsky-normal-form g1))))

(defun solve ()
  (let ((channel (make-channel))
        (submitted 0)
        (parts (vector 0 0)))
    (multiple-value-bind (entries p1 p2) (inputs 19)
      (flet ((s1 (s) (cons 0 (cyk-match-p p1 s)))
             (s2 (s) (cons 1 (cyk-match-p p2 s))))
        (dolist (s entries)
          (submit-task channel #'s1 s)
          (submit-task channel #'s2 s)
          (incf submitted 2)))
      (do-fast-receives (r channel submitted)
        (destructuring-bind (id . matchp) r
          (when matchp
            (incf (aref parts id)))))
      parts)))

;; AOC2020.19> (time (solve))
;; Evaluation took:
;;   2.023 seconds of real time
;;   12.761700 seconds of total run time (12.157804 user, 0.603896 system)
;;   [ Run times consist of 0.458 seconds GC time, and 12.304 seconds non-GC time. ]
;;   630.85% CPU
;;   4,036,682,343 processor cycles
;;   4,050,548,592 bytes consed
;;
;; #(134 377)
