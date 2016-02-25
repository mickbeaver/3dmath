(defpackage #:3dmath
  (:use #:cl)
  (:export
   #:make-vec2
   #:make-vec3
   #:make-vec4
   #:vec-x
   #:vec-y
   #:vec-z
   #:vec-w
   #:vec2-sqlength
   #:vec3-sqlength
   #:vec4-sqlength
   #:vec2-sub
   #:vec3-sub
   #:vec4-sub
   #:vec2-length
   #:vec3-length
   #:vec4-length
   #:vec2-distance
   #:vec3-distance
   #:vec4-distance
   #:vec2-scale
   #:vec3-scale
   #:vec4-scale
   #:vec2-normalize
   #:vec3-normalize
   #:vec4-normalize
   #:vec2-projection
   #:vec3-projection
   #:vec4-projection
   #:vec4-scale
   #:vec2-equal
   #:vec3-equal
   #:vec4-equal
   #:vec2-add
   #:vec3-add
   #:vec4-add
   #:vec2-dot
   #:vec3-dot
   #:vec4-dot
   #:vec3-cross
   #:make-mat2
   #:make-mat3
   #:make-mat4
   #:mat2-11
   #:mat2-21
   #:mat2-12
   #:mat2-22
   #:mat3-11
   #:mat3-21
   #:mat3-31
   #:mat3-12
   #:mat3-22
   #:mat3-32
   #:mat3-13
   #:mat3-23
   #:mat3-33
   #:mat4-11
   #:mat4-21
   #:mat4-31
   #:mat4-41
   #:mat4-12
   #:mat4-22
   #:mat4-32
   #:mat4-42
   #:mat4-13
   #:mat4-23
   #:mat4-33
   #:mat4-43
   #:mat4-14
   #:mat4-24
   #:mat4-34
   #:mat4-44
   #:mat2-add
   #:mat2-multiply))
