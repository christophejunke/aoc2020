(defpackage :aoc2020
  (:use . #1=(:cl :alexandria :ppcre :trivia
              :named-readtables :aoc2020.fetch))
  (:export #:fold-input-lines
           #:do-input-lines
           #:with-input
           #:slurp-line
           #:int
           #:word
           #:letter
           #:decode-format
           #:scanner-bind
           .
           #.(aoc2020.utils:external-symbols . #1#)))

(in-package :aoc2020)

(when-let ((package (find-package :series)))
  (rename-package package "Z"))

;;;;
;;;; PUZZLE INPUTS
;;;;

(defvar *input-base*
  (merge-pathnames "inputs/*.txt" (asdf:system-source-directory "aoc2020")))

(defun input-file (name)
  (typecase name
    (number (input-file (format nil "~2,'0d" name)))
    (string (input-file (make-pathname :name name)))
    (pathname (merge-pathnames name *input-base*))))

(defmacro with-input ((stream name) &body body)
  `(with-open-file (,stream (input-file ,name))
     ,@body))

(defmacro do-input-lines ((line name &optional result) &body body)
  (with-gensyms (stream)
    `(with-input (,stream ,name)
       (loop :for ,line := (read-line ,stream nil nil)
             :while ,line
             :do (progn ,@body)
             :finally (return ,result)))))

(defun fold-input-lines (input function &optional accumulator)
  (do-input-lines (line input accumulator)
    (setf accumulator (funcall function line accumulator))))

(defun slurp-line (input)
  (with-input (s input)
    (read-line s)))

;;;;
;;;; PARSING
;;;;

(define-parse-tree-synonym int
    (:register
     (:sequence
      (:greedy-repetition 0 1 (:char-class #\- #\+))
      (:greedy-repetition 1 nil :digit-class))))

(define-parse-tree-synonym letter
    (:register :word-char-class))

(define-parse-tree-synonym word
    (:register
     (:sequence
      :word-boundary
      (:greedy-repetition 1 nil :word-char-class)
      :word-boundary)))

(defun map-tokens (callback stream &key (sharedp nil) (debugp nil))
  (declare (type (function (t)) callback)
           (type stream stream))
  (let ((buffer (make-array 256
                            :fill-pointer 0
                            :element-type 'character
                            :adjustable t))
        (state-fn #'values)
        (buffer-copy (if sharedp #'identity #'copy-seq)))
    (declare (type function state-fn)
             (type string buffer)
             (type function buffer-copy))
    ;; helper functions
    (labels ((feed-fsm (c)
               (when debugp
                 (print `(:feed ,state-fn ,c) *trace-output*))
               (funcall state-fn c))
             (switch-to (s)
               (setf state-fn s))
             (clear ()
               (setf (fill-pointer buffer) 0))
             (buffer (c)
               (vector-push-extend c buffer (array-total-size buffer)))
             (emit (token-type &rest components)
               (apply callback token-type components))
             (finish-literal-token ()
               (when (> (length buffer) 0)
                 (emit :literal (funcall buffer-copy buffer))
                 (clear))))
      ;; fsm state functions
      (labels ((dispatch (c)
                 (case c
                   (#\%
                    (switch-to #'await-type))
                   (t (buffer c))))
               (await-type (c)
                 (flet ((emit (tok) (finish-literal-token) (emit tok)))
                   (case c
                     ((#\i #\d) (emit :integer))
                     (#\c (emit :character))
                     (#\s (emit :word))
                     ;; escape %% as %
                     (#\% (buffer c))
                     (t (error "unexpected %~a sequence" c))))
                 (switch-to #'dispatch)))
        ;; lexer
        (loop
           :initially (switch-to #'dispatch)
           :for c := (read-char stream () ())
           :while c
           :do (feed-fsm c)
           :finally (finish-literal-token))))))

(defun as-regex-node (token)
  (ematch token
    ((list :integer) (values 'int 'parse-integer))
    ((list :character) (values 'letter 'first-elt))
    ((list :word) (values 'word 'identity))
    ((list :literal string) (copy-seq string))))

(defun decode-format (format)
  (let (regex-tree decoders)
    (with-input-from-string (stream format)
      (map-tokens (lambda (&rest token)
                    (multiple-value-bind (node decoder) (as-regex-node token)
                      ;; decoder iff a variable is needed
                      (push node regex-tree)
                      (when decoder
                        (push decoder decoders))))
                  stream
                  :sharedp t))
    (values `(:sequence ,@(nreverse regex-tree))
            (nreverse decoders))))

(defmacro scanner-bind ((format &rest variables) input &body body)
  (check-type format string)
  (multiple-value-bind (tree decoders) (decode-format format)
    (assert (= (length variables) (length decoders))
            (variables)
            "Invalid number of variables ~a for given format ~s"
            variables format)
    `(register-groups-bind ,(mapcar (lambda (d v) `((function ,d) ,v))
                                    decoders
                                    variables)
         ((let ((*use-bmh-matchers* t))
            (load-time-value (create-scanner ',tree)))
          ,input
          :sharedp t)
       ,@body)))
