(in-package :aoc2020)

;; for simplicity this relies on a regex but this kind of format could
;; be implemented differently.

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
