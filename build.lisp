#+sbcl
(require 'sb-posix)

(ql:quickload '(:alexandria
                :iterate
                :cl-glfw3
                :cl-opengl
                :cl-soil
                :fset
                :glkit
                :trivial-garbage
                :defenum
                :swank
                :ironclad
                :cl-freetype2
                :err))

(asdf:oos 'asdf:load-op 'added-text)

#+sbcl
(sb-ext:save-lisp-and-die "abc.bin"
                          :toplevel (lambda ()
                                      (sb-posix:putenv
                                       (format nil "SBCL_HOME=~A"
                                               #.(sb-ext:posix-getenv "SBCL_HOME")))
                                      (added-text:game)
                                      0)
                          :executable t)

#+ccl
(ccl:save-application "ccl-abc.bin"
                      :toplevel-function (lambda ()
                                           (added-text:game)
                                           0)
                      :prepend-kernel t)
