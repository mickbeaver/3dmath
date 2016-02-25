(in-package #:3dmath-tests)

(defun mat2-approx-equal (a b)
  (and (float-approx-equal (mat2-11 a) (mat2-11 b))
       (float-approx-equal (mat2-21 a) (mat2-21 b))
       (float-approx-equal (mat2-12 a) (mat2-12 b))
       (float-approx-equal (mat2-22 a) (mat2-22 b))))

(define-test test-mat2-multiply-01
  (assert-true (mat2-approx-equal (mat2-multiply (make-mat2 1.0 5.0 -2.0 0.0)
                                                 (make-mat2 -3.0 4.0 7.0 (/ 1.0 3.0)))
                                  (make-mat2 -11.0 -15.0 (/ 19.0 3.0) 35.0))))

