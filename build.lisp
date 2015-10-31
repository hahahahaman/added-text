(require 'sb-posix)

(ql:quickload '(:alexandria
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
(sb-ext:save-lisp-and-die "abc.bin"
                          :toplevel (lambda ()
                                      (sb-posix:putenv
                                       (format nil "SBCL_HOME=~A"
                                               #.(sb-ext:posix-getenv "SBCL_HOME")))
                                      (added-text:game)
                                      0)
                          :executable t)

