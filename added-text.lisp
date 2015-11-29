;;;; added-text.lisp

(in-package #:added-text)

;;; "added-text" goes here. Hacks and glory await!

#|
(defstruct screen
  (hooks (empty-map)))

(defmethod initialize-instance :after ((screen screen) &key)
  t)

(defmethod screen-add-hook ((screen screen) name hook)
  (with! (@ (screen-hooks name) name)
         (alexandria:appendf (@ (screen-hooks screen) name) hook)))

(defmethod screen-run ((screen screen) name)
  (mapcar #'funcall (@ (screen-hooks screen) name)))
|#

(defglobal *rect-depth* 0.0)

(defun load-rect-drawer ()
  (let ((program (make-program #p"./data/shaders/rect.v.glsl"
                               #p"./data/shaders/rect.f.glsl")))
    (setf *rect-drawer* (make-instance 'rect-drawer :program program))
    (load-program "rect" program)
    (gl:use-program (id program))
    (gl:uniform-matrix-4fv (get-uniform program "projection")
                           (kit.glm:ortho-matrix 0.0
                                                 (cfloat *width*)
                                                 (cfloat *height*)
                                                 0.0
                                                 -100.0 100.0)
                           nil)))

(defun init ()
  (track-file #p"./data/shaders/rect.v.glsl" #'load-rect-drawer)
  (track-file #p"./data/shaders/rect.f.glsl" #'load-rect-drawer)

  (let ((sprite-program (make-program #p"./data/shaders/sprite.v.glsl"
                                      #p"./data/shaders/sprite.f.glsl"))
        (rect-program (make-program #p"./data/shaders/rect.v.glsl"
                                    #p"./data/shaders/rect.f.glsl"))
        (text-program (make-program #p"./data/shaders/text.v.glsl"
                                    #p"./data/shaders/text.f.glsl")))
    (setf *program-manager* (make-instance 'program-manager)
          *texture-manager* (make-instance 'texture-manager)
          *font-manager* (make-instance 'font-manager)
          *sprite-drawer* (make-instance 'sprite-drawer :program sprite-program)
          *rect-drawer* (make-instance 'rect-drawer :program rect-program)
          *text-drawer* (make-instance 'text-drawer :program text-program))
    (load-program "sprite" sprite-program)
    (load-program "rect" rect-program)
    (load-program "text" text-program)

    (let ((font-path "./data/fonts/DejaVuSans.ttf"))
      (load-font "sans14" font-path 14)
      (load-font "sans24" font-path 24))

    ;; use current program
    (let ((proj
            ;; left, right, bottom, top, near, far
            (kit.glm:ortho-matrix 0.0
                                  (cfloat *width*)
                                  (cfloat *height*)
                                  0.0
                                  -100.0 100.0)))

      (gl:use-program (id sprite-program))

      ;;set image uniform to texture0
      (gl:uniformi (get-uniform sprite-program "image") 0)

      ;; set projection matrix
      (gl:uniform-matrix-4fv
       (get-uniform sprite-program "projection")
       proj
       nil)

      (gl:use-program (id rect-program))
      (gl:uniform-matrix-4fv (get-uniform rect-program "projection")
                             proj
                             nil)

      (gl:use-program (id text-program))
      (gl:uniformi (get-uniform text-program "text") 0)
      (gl:uniform-matrix-4fv (get-uniform text-program "projection")
                             proj
                             nil)))
  (gl:enable :blend)
  (gl:enable :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defun handle-input ()
  (when (key-action-p :escape :press)
    (glfw:set-window-should-close))
  (when (mouse-button-action-p :left :press)
    (add-event :code (add-entity (map (:pos (vec3 (cfloat *cursor-x*)
                                                  (cfloat *cursor-y*)
                                                  (cfloat *rect-depth*)))
                                      (:size (vec2 50.0 50.0))
                                      (:color (vec4 1.0 0.0 1.0 0.5)))))))

(defun update ()
  (when *scroll-callback-p*
    (incf *rect-depth* (cfloat *scroll-y*)))
  (update-window-title cl-glfw3:*window* "abc"))

(defun render-entities ()
  (do-map (id entity *entities*)
    (declare (ignore id))
    (rect-draw :position (@ entity :pos)
               :size (@ entity :size)
               :color (@ entity :color)
               :rotate 0.0
               :draw-mode :line-strip)))

(defun draw-rect-spiral (&key
                           (n 10)
                           (current 0)
                           (position (vec3 0.0 0.0 0.0))
                           (size (vec2 100.0 100.0))
                           (rotate 0.0))
  (when (> n current)
    (rect-draw
     :position position
     :size (vec2 (* (x-val size) (/ current n))
                 (* (y-val size) (/ current n)))
     :color (vec4 (cfloat (/ (mod (+ rotate (* 2 pi (/ current n))) (* 2 pi)) (* 2 pi)))
                  (cfloat (/ (mod (+ rotate (* 2 pi (/ current n))) pi) pi))
                  (cfloat (/ (mod (+ rotate (* 2 pi (/ current n))) (/ pi 4)) (/ pi 4)))
                  (cfloat (mod (+ rotate (* 2 pi (/ current n))) 1.0)))
     :rotate (+ rotate (* 2 pi (/ current n)))
     :draw-center (vec3 0.5 0.5 0.0)
     :draw-mode :triangle-strip)
    (draw-rect-spiral
     :n n
     :current (1+ current)
     :position position
     :size size
     :rotate rotate)))

(defun render ()
  (gl:clear-color 0.0 0.0 0.0 0.25)
  (gl:clear :color-buffer-bit
            :depth-buffer)
  (render-entities)

  (draw-rect-spiral :n 100
                    :current 0
                    :position (vec3 400.0 300.0 0.0)
                    :size (vec2 200.0 200.0)
                    :rotate (glfw:get-time))
  (let ((text "\"What is love?\"")
        (font (get-font "sans24"))
        (scale (vec2 1.0 1.00)))
    (multiple-value-bind (x y) (text-dimensions text font
                                                :scale scale)
      (text-draw text
                 font
                 :position (vec2 (- *width* (cfloat x) 10) (- *height* (cfloat y) 10))
                 :scale scale
                 :color (vec4 0.0 0.61 1.0 0.5))))
  (text-draw (format nil "~4f" (average-fps))
             (get-font "sans14")
             :position (vec2 0.0 0.0)
             :scale (vec2 0.8 0.8)))

(defun cleanup ()
  (clear-resources *program-manager*)
  (clear-resources *texture-manager*)
  t)

(defun update-window-title (window title)
  (cl-glfw3:set-window-title (format nil "~A" title)
                             window))

(defun game ()
  (run "added text"
       :init-code (init)
       :input-code (handle-input)
       :render-code (render)
       :update-code (update)
       :cleanup-code (cleanup)))
