(in-package #:3dmath)

;;(defun make-vec2 (&optional (x 0.0) (y 0.0))
;;  (make-array 2 :element-type 'single-float :initial-contents (list x y)))

;; Column-major order matrices

(defun make-mat2 (&optional
                    (m11 0.0) (m21 0.0)
                    (m12 0.0) (m22 0.0))
  (make-array 4 :element-type 'single-float :initial-contents (list
                                                               m11 m21
                                                               m12 m22)))

(defun make-mat3 (&optional
                    (m11 0.0) (m21 0.0) (m31 0.0)
                    (m12 0.0) (m22 0.0) (m32 0.0)
                    (m13 0.0) (m23 0.0) (m33 0.0))
  (make-array 9 :element-type 'single-float :initial-contents (list
                                                               m11 m21 m31
                                                               m12 m22 m32
                                                               m13 m23 m33)))

(defun make-mat4 (&optional
                    (m11 0.0) (m21 0.0) (m31 0.0) (m41 0.0)
                    (m12 0.0) (m22 0.0) (m32 0.0) (m42 0.0)
                    (m13 0.0) (m23 0.0) (m33 0.0) (m43 0.0)
                    (m14 0.0) (m24 0.0) (m34 0.0) (m44 0.0))
  (make-array 16 :element-type 'single-float :initial-contents (list
                                                               m11 m21 m31 m41
                                                               m12 m22 m32 m42
                                                               m13 m23 m33 m43
                                                               m14 m24 m34 m44)))
(defun mat2-11 (a)
  (aref a 0))

(defun mat2-21 (a)
  (aref a 1))

(defun mat2-12 (a)
  (aref a 2))

(defun mat2-22 (a)
  (aref a 3))

(defun mat3-11 (a)
  (aref a 0))

(defun mat3-21 (a)
  (aref a 1))

(defun mat3-31 (a)
  (aref a 2))

(defun mat3-12 (a)
  (aref a 3))

(defun mat3-22 (a)
  (aref a 4))

(defun mat3-32 (a)
  (aref a 5))

(defun mat3-13 (a)
  (aref a 6))

(defun mat3-23 (a)
  (aref a 7))

(defun mat3-33 (a)
  (aref a 8))

(defun mat4-11 (a)
  (aref a 0))

(defun mat4-21 (a)
  (aref a 1))

(defun mat4-31 (a)
  (aref a 2))

(defun mat4-41 (a)
  (aref a 3))

(defun mat4-12 (a)
  (aref a 4))

(defun mat4-22 (a)
  (aref a 5))

(defun mat4-32 (a)
  (aref a 6))

(defun mat4-42 (a)
  (aref a 7))

(defun mat4-13 (a)
  (aref a 8))

(defun mat4-23 (a)
  (aref a 9))

(defun mat4-33 (a)
  (aref a 10))

(defun mat4-43 (a)
  (aref a 11))

(defun mat4-14 (a)
  (aref a 12))

(defun mat4-24 (a)
  (aref a 13))

(defun mat4-34 (a)
  (aref a 14))

(defun mat4-44 (a)
  (aref a 15))

(defun mat2-add (a b)
  (make-mat2 (+ (mat2-11 a) (mat2-11 b)) (+ (mat2-21 a) (mat2-21 b))
             (+ (mat2-12 a) (mat2-12 b)) (+ (mat2-22 a) (mat2-22 b))))

(defun mat2-multiply (a b)
  (make-mat2 (+ (* (mat2-11 a) (mat2-11 b)) (* (mat2-12 a) (mat2-21 b)))
             (+ (* (mat2-21 a) (mat2-11 b)) (* (mat2-22 a) (mat2-21 b)))
             (+ (* (mat2-11 a) (mat2-12 b)) (* (mat2-12 a) (mat2-22 b)))
             (+ (* (mat2-21 a) (mat2-12 b)) (* (mat2-22 a) (mat2-22 b)))))

