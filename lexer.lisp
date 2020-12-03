(in-package :aoc2020)

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
