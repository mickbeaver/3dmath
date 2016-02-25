(asdf:defsystem #:3dmath-tests
  :serial t
  :description "3dmath unit tests"
  :author "Mick Beaver <m.charles.beaver@gmail.com>"
  :license "MIT License"
  :depends-on (#:3dmath #:lisp-unit)
  :components
  ((:module "tests"
	    :components ((:file "package")
                         (:file "float")
                         (:file "vector-tests")
                         (:file "matrix-tests")))))


