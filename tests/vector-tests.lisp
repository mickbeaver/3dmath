(in-package #:3dmath-tests)

(defconstant +float-epsilon+ 0.000001)

(defun float-approx-equal (float1 float2)
  (<= (abs (- float1 float2))
      +float-epsilon+))

(defun vec2-approx-equal (a b)
  (and (float-approx-equal (vec-x a)
                           (vec-x b))
       (float-approx-equal (vec-y a)
                           (vec-y b))))

(defun vec3-approx-equal (a b)
  (and (float-approx-equal (vec-x a)
                           (vec-x b))
       (float-approx-equal (vec-y a)
                           (vec-y b))
       (float-approx-equal (vec-z a)
                           (vec-z b))))

(define-test test-vec2-add
    (assert-true (vec2-approx-equal (vec2-add (make-vec2 1.0 1.0)
                                              (make-vec2 1.0 1.0))
                                    (make-vec2 2.0 2.0))))

(define-test test-cross-product
    (assert-true (vec3-approx-equal (vec3-cross (make-vec3 1.0 0.0 0.0)
                                                (make-vec3 0.0 1.0 0.0))
                                    (make-vec3 0.0 0.0 1.0)))
    (assert-true (vec3-approx-equal (vec3-cross (make-vec3 2.0  2.0 1.0)
                                                (make-vec3 1.0 -2.0 0.0))
                                    (make-vec3 2.0 1.0 -6.0))))
