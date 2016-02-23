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

(define-test test-vec2-add-01
    (assert-true (vec2-approx-equal (vec2-add (make-vec2 1.0 1.0)
                                              (make-vec2 1.0 1.0))
                                    (make-vec2 2.0 2.0))))

(define-test test-vec2-distance-01
  (assert-true (float-approx-equal (vec2-distance (make-vec2 10.0 6.0)
                                                  (make-vec2 14.0 30.0))
                                   24.331051)))

(define-test test-vec2-distance-02
  (assert-true (float-approx-equal (vec2-distance (make-vec2 0.0 0.0)
                                                  (make-vec2 -12.0 5.0))
                                   13.0)))

(define-test test-vec2-dot-01
  (assert-true (float-approx-equal (vec2-dot (make-vec2 2.0 1.25)
                                             (make-vec2 1.0 -3.0))
                                   -1.75)))

(define-test test-vec2-length-01
  (assert-true (float-approx-equal (vec2-length (make-vec2 -12.0 5.0))
                                   13.0)))
  
(define-test test-vec2-normalize-01
    (assert-true (vec2-approx-equal (vec2-normalize (make-vec2 12.0 5.0))
                                    (make-vec2 0.923077 0.384615))))

(define-test test-vec2-normalize-02
    (assert-true (vec2-approx-equal (vec2-normalize (make-vec2 0.0 743.632))
                                    (make-vec2 0.0 1.0))))

;; (define-test test-cross-product
;;     (assert-true (vec3-approx-equal (vec3-cross (make-vec3 1.0 0.0 0.0)
;;                                                 (make-vec3 0.0 1.0 0.0))
;;                                     (make-vec3 0.0 0.0 1.0)))
;;     (assert-true (vec3-approx-equal (vec3-cross (make-vec3 2.0  2.0 1.0)
;;                                                 (make-vec3 1.0 -2.0 0.0))
;;                                     (make-vec3 2.0 1.0 -6.0))))
