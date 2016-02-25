(in-package #:3dmath-tests)

(defconstant +float-epsilon+ 0.000001)

(defun float-approx-equal (float1 float2)
  (<= (abs (- float1 float2))
      +float-epsilon+))

