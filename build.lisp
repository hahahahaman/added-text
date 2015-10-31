(require 'sb-posix)

(load #P"~/quicklisp/setup.lisp")

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
                                      ;; (freetype2:with-open-face
                                      ;;     (face "./data/fonts/DejaVuSans.ttf" 0
                                      ;;      (ft2:make-freetype))
                                      ;;   )
                                      (added-text:game)
                                      0)
                          :executable t)

