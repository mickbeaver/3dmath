(in-package #:3dmath-tests)

(defconstant +float-epsilon+ 0.000001)

(defun float-approx-equal (float1 float2)
  (<= (abs (- float1 float2))
      +float-epsilon+))

(defun vec2-approx-equal (p q)
  (and (float-approx-equal (vec-x p)
                           (vec-x q))
       (float-approx-equal (vec-y p)
                           (vec-y q))))

(defun vec3-approx-equal (p q)
  (and (float-approx-equal (vec-x p)
                           (vec-x q))
       (float-approx-equal (vec-y p)
                           (vec-y q))
       (float-approx-equal (vec-z p)
                           (vec-z q))))

(defun vec4-approx-equal (p q)
  (and (float-approx-equal (vec-x p)
                           (vec-x q))
       (float-approx-equal (vec-y p)
                           (vec-y q))
       (float-approx-equal (vec-z p)
                           (vec-z q))
       (float-approx-equal (vec-w p)
                           (vec-w q))))

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
  (let ((p (vec2-normalize (make-vec2 12.0 5.0))))
    (assert-true (float-approx-equal (vec2-length p) 1.0))
    (assert-true (vec2-approx-equal p
                                    (make-vec2 0.923077 0.384615)))))

(define-test test-vec2-normalize-02
  (let ((p (vec2-normalize (make-vec2 0.0 743.632))))
    (assert-true (float-approx-equal (vec2-length p) 1.0))
    (assert-true (vec2-approx-equal p
                                    (make-vec2 0.0 1.0)))))

(define-test test-vec2-projection-01
  (assert-true (vec2-approx-equal (vec2-projection (make-vec2 4.0 3.0)
                                                   (make-vec2 (/ (sqrt 2.0) 2.0)
                                                              (/ (sqrt 2.0) 2.0)))
                                  (make-vec2 3.5 3.5))))

(define-test test-vec2-scale-01
  (assert-true (vec2-approx-equal (vec2-scale 3.0 (make-vec2 4.0 -7.0))
                                  (make-vec2 12.0 -21.0))))

(define-test test-vec2-sub-01
  (assert-true (vec2-approx-equal (vec2-sub (make-vec2 3.0 10.0)
                                            (make-vec2 8.0 -7.0))
                                  (make-vec2 -5.0 17.0))))

(define-test test-vec2-sub-02
  (assert-true (vec2-approx-equal (vec2-sub (make-vec2 4.0 5.0)
                                            (make-vec2 -4.0 -5.0))
                                  (make-vec2 8.0 10.0))))

(define-test test-vec3-add-01
  (assert-true (vec3-approx-equal (vec3-add (make-vec3 7.0 -2.0 -3.0)
                                            (make-vec3 6.0 6.0 -4.0))
                                  (make-vec3 13.0 4.0 -7.0))))

(define-test test-vec3-add-02
  (assert-true (vec3-approx-equal (vec3-add (make-vec3 2.0 9.0 -1.0)
                                            (make-vec3 -2.0 -9.0 1.0))
                                  (make-vec3 0.0 0.0 0.0))))

(define-test test-vec3-cross-product-01
  (assert-true (vec3-approx-equal (vec3-cross (make-vec3 1.0 0.0 0.0)
                                              (make-vec3 0.0 1.0 0.0))
                                  (make-vec3 0.0 0.0 1.0))))

(define-test test-vec3-cross-product-02
  (assert-true (vec3-approx-equal (vec3-cross (make-vec3 2.0 2.0 1.0)
                                              (make-vec3 1.0 -2.0 0.0))
                                  (make-vec3 2.0 1.0 -6.0))))

(define-test test-vec3-distance-01
  (assert-true (float-approx-equal (vec3-distance (make-vec3 3.0 10.0 7.0)
                                                  (make-vec3 8.0 -7.0 4.0))
                                   17.972200)))

(define-test test-vec3-distance-02
  (assert-true (float-approx-equal (vec3-distance (make-vec3 -2.0 -4.0 9.0)
                                                  (make-vec3 6.0 -7.0 9.5))
                                   8.558621)))

(define-test test-vec3-dot-product-01
  (assert-true (float-approx-equal (vec3-dot (make-vec3 2.0 2.0 1.0)
                                             (make-vec3 1.0 -2.0 0.0))
                                   -2.0)))

(define-test test-vec3-length-01
  (assert-true (float-approx-equal (vec3-length (make-vec3 8.0 -3.0 0.5))
                                   8.558621384311845)))

(define-test test-vec3-normalize-01
  (let ((p (vec3-normalize (make-vec3 8.0 -3.0 0.5))))
    (assert-true (float-approx-equal (vec3-length p) 1.0))
    (assert-true (vec3-approx-equal p
                                    (make-vec3 0.934730 -0.350524 0.058421)))))

(define-test test-vec3-normalize-02
  (let ((p (vec3-normalize (make-vec3 -12.0 3.0 -4.0))))
    (assert-true (float-approx-equal (vec3-length p) 1.0))
    (assert-true (vec3-approx-equal p
                                  (make-vec3 -0.923077 0.23077 -0.307692))))

(define-test test-vec3-projection-01
  (let ((p (make-vec3 4.0 3.0 -1.0))
        (q (make-vec3 (/ (sqrt 2.0) 2.0)
                      (/ (sqrt 2.0) 2.0))))
    (assert-true (vec3-approx-equal (vec3-projection p q)
                                    (make-vec3 3.5 3.5 0.0)))))

(define-test test-vec3-scale-01
  (assert-true (vec3-approx-equal (vec3-scale 3.0 (make-vec3 4.0 -7.0 0.0))
                                  (make-vec3 12.0 -21.0 0.0))))

(define-test test-vec3-sub-01
  (assert-true (vec3-approx-equal (vec3-sub (make-vec3 3.0 10.0 7.0)
                                            (make-vec3 8.0 -7.0 4.0))
                                  (make-vec3 -5.0 17.0 3.0))))

(define-test test-vec3-sub-02
  (assert-true (vec3-approx-equal (vec3-sub (make-vec3 4.0 5.0 -11.0)
                                            (make-vec3 -4.0 -5.0 11.0))
                                  (make-vec3 8.0 10.0 -22.0))))

(define-test test-vec4-add-01
  (assert-true (vec4-approx-equal (vec4-add (make-vec4 7.0 -2.0 -3.0 -8.2)
                                            (make-vec4 6.0 6.0 -4.0 -7.11))
                                  (make-vec4 13.0 4.0 -7.0 -15.309999))))

(define-test test-vec4-add-02
  (assert-true (vec4-approx-equal (vec4-add (make-vec4 2.0 9.0 -1.0 -2.5)
                                            (make-vec4 -2.0 -9.0 1.0 23.0))
                                  (make-vec4 0.0 0.0 0.0 20.5))))

(define-test test-vec4-distance-01
  (assert-true (float-approx-equal (vec4-distance (make-vec4 4.0 -4.0 -4.0 4.0)
                                                  (make-vec4 -6.0 6.0 6.0 -6.0))
                                   20.0)))

(define-test test-vec4-dot-product-01
  (assert-true (float-approx-equal (vec4-dot (make-vec4 2.0 2.0 1.0 3.5)
                                             (make-vec4 1.0 -2.0 0.0 12.0))
                                   40.0)))

(define-test test-vec4-length-01
  (assert-true (float-approx-equal (vec4-length (make-vec4 8.0 -3.0 0.5 2.5))
                                   8.91627725)))

(define-test test-vec4-normalize body...)-01
  (let ((p (vec4-normalize (make-vec4 1.0 1.0 1.0 1.0))))
    (assert-true (float-approx-equal (vec4-length p) 1.0))
    (assert-true (vec4-approx-equal p
                                    (make-vec4 0.5 0.5 0.5 0.5))))

(define-test test-vec4-scale-01
  (assert-true (vec4-approx-equal (vec4-scale 3.0 (make-vec4 4.0 -7.0 1.0 -2.5))
                                  (make-vec4 12.0 -21.0 3.0 -7.5))))

(define-test test-vec4-sub-01
  (assert-true (vec4-approx-equal (vec4-sub (make-vec4 3.0 10.0 7.0 23.0)
                                            (make-vec4 8.0 -7.0 4.0 12.0))
                                  (make-vec4 -5.0 17.0 3.0 11.0))))

(define-test test-vec4-sub-02
  (assert-true (vec4-approx-equal (vec4-sub (make-vec4 4.0 5.0 -11.0 -17.0)
                                            (make-vec4 -4.0 -5.0 11.0 9.5))
                                  (make-vec4 8.0 10.0 -22.0 -26.5))))

