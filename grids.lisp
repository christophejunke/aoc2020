(in-package :aoc2020)

(defun read-grid (stream &key (element-type t) (transform #'identity))
  (loop
    :with grid = (make-array 256
                             :adjustable t
                             :element-type element-type
                             :fill-pointer 0)
    :for line = (read-line stream nil nil)
    :while line
    :for same-width = t :then (= width (length line))
    :for width = (length line)
    :count line :into height
    :do (assert same-width
                ()
                "rows with different width as expected (~d): ~a"
                width
                line)
        (map ()
             (lambda (c)
               (vector-push-extend (funcall transform c)
                                   grid
                                   (array-total-size grid)))
             line)
    :finally (return (make-array (list height width)
                                 :element-type element-type
                                 :displaced-to grid))))
