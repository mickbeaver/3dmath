(in-package #:3dmath)

(defun make-vec2 (&optional (x 0.0) (y 0.0))
  (make-array 2 :element-type 'single-float :initial-contents (list x y)))

(defun make-vec3 (&optional (x 0.0) (y 0.0) (z 0.0))
  (make-array 3 :element-type 'single-float :initial-contents (list x y z)))

(defun make-vec4 (&optional (x 0.0) (y 0.0) (z 0.0) (w 0.0))
  (make-array 4 :element-type 'single-float :initial-contents (list x y z w)))

(defun vec-x (v)
  (aref v 0))

(defun vec-y (v)
  (aref v 1))

(defun vec-z (v)
  (aref v 2))

(defun vec-w (v)
  (aref v 3))

(defun vec2-equal (a b)
  (and (= (vec-x a)
          (vec-x b))
       (= (vec-y a)
          (vec-y b))))

(defun vec3-equal (a b)
  (and (= (vec-x a)
          (vec-x b))
       (= (vec-y a)
          (vec-y b))
       (= (vec-z a)
          (vec-z b))))

(defun vec4-equal (a b)
  (and (= (vec-x a)
          (vec-x b))
       (= (vec-y a)
          (vec-y b))
       (= (vec-z a)
          (vec-z b))
       (= (vec-w a)
          (vec-w b))))

(defun vec2-add (a b)
  (make-vec2 (+ (vec-x a)
                (vec-x b))
             (+ (vec-y a)
                (vec-y b))))

(defun vec3-add (a b)
  (make-vec3 (+ (vec-x a)
                (vec-x b))
             (+ (vec-y a)
                (vec-y b))
             (+ (vec-z a)
                (vec-z b))))

(defun vec4-add (a b)
  (make-vec4 (+ (vec-x a)
                (vec-x b))
             (+ (vec-y a)
                (vec-y b))
             (+ (vec-z a)
                (vec-z b))
             (+ (vec-w a)
                (vec-w b))))

(defun vec2-dot (a b)
  (+ (* (vec-x a)
        (vec-x b))
     (* (vec-y a)
        (vec-y b))))

(defun vec3-dot (a b)
  (+ (* (vec-x a)
        (vec-x b))
     (* (vec-y a)
        (vec-y b))
     (* (vec-z a)
        (vec-z b))))

(defun vec4-dot (a b)
  (+ (* (vec-x a)
        (vec-x b))
     (* (vec-y a)
        (vec-y b))
     (* (vec-z a)
        (vec-z b))
     (* (vec-w a)
        (vec-w b))))

(defun vec3-cross (a b)
  (make-vec3 (- (* (vec-y a)
                   (vec-z b))
                (* (vec-y b)
                   (vec-z a)))
             (- (* (vec-x b)
                   (vec-z a))
                (* (vec-x a)
                   (vec-z b)))
             (- (* (vec-x a)
                   (vec-y b))
                (* (vec-x b)
                   (vec-y a)))))
