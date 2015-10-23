;;;; added-text.lisp

(in-package #:added-text)

;;; "added-text" goes here. Hacks and glory await!
(defglobal *title* "added text")

(defglobal *debug* nil)

(defenum:defenum *enum-game-state* ((+game-menu+ 0)
                                    +game-play+
                                    +game-help+))

(defenum:defenum *enum-level-state* ((+level-play+ 0)
                                     +level-completed+))

(defglobal *game-state* +game-menu+)
(defglobal *level-state* +level-play+)

(defglobal *rect-depth* 0.0)

(defun update-window-title (window title)
  (cl-glfw3:set-window-title (format nil "~A" title)
                             window))

(defun init ()
  (setf *random-state* (make-random-state t))
  (let ((sprite-program (make-program #p"./data/shaders/sprite.v.glsl"
                                      #p"./data/shaders/sprite.f.glsl"))
        (rect-program (make-program #p"./data/shaders/rect.v.glsl"
                                    #p"./data/shaders/rect.f.glsl")))
    (setf *program-manager* (make-instance 'program-manager)
          *texture-manager* (make-instance 'texture-manager)
          *sprite-drawer* (make-instance 'sprite-drawer :program sprite-program)
          *rect-drawer* (make-instance 'rect-drawer :program rect-program))
    (load-program "sprite" sprite-program)
    (load-program "rect" rect-program)

    ;; textures
    ;; (load-texture "menu"
    ;;               (make-texture2d "./data/images/menu.png" t))
    ;; (load-texture "help"
    ;;               (make-texture2d "./data/images/help.png" t))
    ;; (load-texture "complete"
    ;;               (make-texture2d "./data/images/complete.png" t))

    ;; use current program
    (gl:use-program (id sprite-program))

    ;;set image uniform to texture0
    (gl:uniformi (get-uniform sprite-program "image") 0)

    ;; set projection matrix
    (gl:uniform-matrix-4fv
     (get-uniform sprite-program "projection")
     (vector
      ;; left, right, bottom, top, near, far
      (kit.glm:ortho-matrix 0.0
                            (cfloat *width*)
                            (cfloat *height*)
                            0.0
                            -100.0 100.0))
     ;; (vector (kit.math:perspective-matrix (kit.glm:deg-to-rad 45.0)
     ;;                               (cfloat (/ *width* *height*))
     ;;                               -2.1
     ;;                               100.0))
     nil)

    (gl:use-program (id rect-program))
    (gl:uniform-matrix-4fv (get-uniform rect-program "projection")
                           (vector (kit.glm:ortho-matrix
                                    0.0
                                    (cfloat *width*)
                                    (cfloat *height*)
                                    0.0
                                    -1.0 1.0))
                           nil))
  ;; (track-vars *game-state*
  ;;             *level-state*
  ;;             *difficulty*
  ;;             *grid*
  ;;             *final-grid*
  ;;             *selected-matrix*
  ;;             *matrices*)
  )

(defun handle-input ()
  (when (key-action-p :escape :press)
    (glfw:set-window-should-close))
  (when (mouse-button-action-p :left :press)
    (add-entity (map (:pos (vec3 (cfloat *cursor-x*)
                                 (cfloat *cursor-y*)
                                 (cfloat *rect-depth*)))
                     (:size (vec2 50.0 50.0))
                     (:color (vec4 1.0 1.0 1.0 0.5))))))

(defun update ()
  (when *scroll-callback-p*
    (incf *rect-depth* (cfloat *scroll-y*))
    (print *rect-depth*)))

(defun render-entities ()
  (do-map (id entity *entities*)
    (declare (ignore id))
    (rect-draw (@ entity :pos)
               (@ entity :size)
               (@ entity :color)
               0.0)))

(defun render ()
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (render-entities))

(defun cleanup ()
  (clear-resources *program-manager*)
  (clear-resources *texture-manager*)
  t)

(defun run () 
  (glfw:with-init-window (:title *title*
                          :width *width*
                          :height *height*
                          :opengl-forward-compat t
                          :opengl-profile :opengl-core-profile
                          :context-version-major 3
                          :context-version-minor 3
                          :decorated t
                          :resizable nil
                          ;;full screen mode
                          ;; :monitor (glfw:get-primary-monitor)
                          ;; :refresh-rate 60
                          )
    ;; (glfw:swap-interval 1)
    (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

    (unless (gl::features-present-p (>= :glsl-version 3.3))
      (return-from run nil))

    ;; initialize
    (initialize-globals)
    (init)

    (gl:enable :blend)
    (gl:enable :depth-test)
    (gl:blend-func :src-alpha :one-minus-src-alpha)

    (glfw:set-key-callback 'key-callback)
    (glfw:set-mouse-button-callback 'mouse-callback)
    (glfw:set-cursor-position-callback 'cursor-callback)
    (glfw:set-scroll-callback 'scroll-callback)
    ;; (glfw:set-input-mode :cursor :disabled)

    (iter (until (glfw:window-should-close-p))
      (update-swank)
      ;; give some fps data in title
      (update-window-title cl-glfw3:*window* *title*)

      (glfw:poll-events)

      (handle-input)
      (render)
      (update)

      (glfw:swap-buffers)
      (update-globals))

    (cleanup)))
