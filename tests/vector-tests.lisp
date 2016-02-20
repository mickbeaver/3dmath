(in-package #:3dmath-tests)

(defconstant +float-epsilon+ 0.000001)

(defun float-approx-equal (float1 float2)
  (<= +float-epsilon+ (abs (- float1 float2))))

(defun vec3-approx-equal (a b)
  (and (float-approx-equal (vec-x a)
                           (vec-x b))
       (float-approx-equal (vec-y a)
                           (vec-y b))
       (float-approx-equal (vec-z a)
                           (vec-z b))))

(define-test test-cross-product
    (assert-true (vec3-approx-equal (vec3-cross (make-vec3 1.0 0.0 0.0)
                                                (make-vec3 0.0 1.0 0.0))
                                    (make-vec3 0.0 0.0 1.0))))
                             
