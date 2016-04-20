;;;; added-text.lisp

(in-package #:added-text)

;;; "added-text" goes here. Hacks and glory await!

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
  (initialize-globals)
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
                                  0.0
                                  (cfloat *height*)
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
    (add-event :code (add-entity (map (:pos (vec3f (cfloat *cursor-x*)
                                                   (- *height* (cfloat *cursor-y*))
                                                   (cfloat *rect-depth*)))
                                      (:size (vec2f 50.0 50.0))
                                      (:color (vec4f 1.0 0.0 1.0 0.5))))))
  (when (and (key-pressed-p :left-control)
             (key-action-p :r :press))
    (add-event :code (setf *entities* (empty-map)))))

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
               :rotation 0.0
               :draw-mode :line-strip)))

(defun draw-rect-spiral (&key
                           (n 10)
                           (current 0)
                           (position (vec3f 0.0 0.0 0.0))
                           (size (vec2f 100.0 100.0))
                           (rotation 0.0))
  (when (> n current)
    (let ((ratio (cfloat (/ current n)))
          (rotation (cfloat rotation)))
      (rect-draw
       :position (vec3f+ position (vec3f 0.0 0.0 0.0))
       :size (vec2f (* (x-val size) (cfloat ratio))
                    (* (y-val size) (cfloat ratio)))
       ;; :color (vec4f (cfloat (/ (mod (+ rotation (* 2 pi (/ current n))) (* 2 pi)) (* 2 pi)))
       ;;               (cfloat (/ (mod (+ rotation (* 2 pi (/ current n))) pi) pi))
       ;;               (cfloat (/ (mod (+ rotation (* 2 pi (/ current n))) (/ pi 4)) (/ pi 4)))
       ;;               (cfloat (mod (+ rotation (* 2 pi (/ current n))) 1.0)))

       ;; :color (vec4f (cfloat (sin (* rotation ratio (/ pi 4))))
       ;;               (cfloat (sin (* rotation ratio (/ pi 0.5))))
       ;;               (cfloat (sin (* rotation ratio (/ pi 6))))
       ;;               (cfloat (* (sin ratio) )))
       :color (vec4f (cfloat (sin (* rotation (/ pi 4))))
                     (cfloat (sin (* rotation (/ pi 2))))
                     (cfloat (sin (* rotation (/ pi 1))))
                     1.0)
       :rotation (/ rotation 30)
       ;; :draw-center (vec3f -0.5 -0.5 0.0)
       :draw-mode :triangle-strip)
      (draw-rect-spiral
       :n n
       :current (1+ current)
       :position position
       :size size
       :rotation (+ (1+ rotation) (/ (glfw:get-time) 5))))))

(defun render ()
  (gl:clear-color 0.0 0.0 0.0 0.25)
  (gl:clear :color-buffer-bit
            :depth-buffer)
  (render-entities)

  (draw-rect-spiral :n 50
                    :current 0
                    :position (vec3f 400.0 300.0 0.0)
                    :size (vec2f 200.0 400.0)
                    :rotation (glfw:get-time))
  (let ((text "meh")
        (font (get-font "sans24"))
        (scale (vec2f 1.0 1.00)))
    (multiple-value-bind (x y) (text-dimensions text font
                                                :scale scale)
      (text-draw text
                 font
                 :position (vec2f (- *width* (cfloat x) 10) (- *height* (cfloat y) 10))
                 :scale scale
                 :color (vec4f 0.0 0.61 1.0 0.5))))
  (text-draw (format nil "~4f" (average-fps))
             (get-font "sans14")
             :position (vec2f 0.0 0.0)
             :draw-center (vec3f -0.5 -0.5 0.0)
             :scale (vec2f 0.8 0.8)))

(defun cleanup ()
  (clear-resources *program-manager*)
  (clear-resources *texture-manager*)
  t)

(defun update-window-title (window title)
  (cl-glfw3:set-window-title (format nil "~A" title)
                             window))

(defun game ()
  (err-run "added text"
           :init-code (init)
           :input-code (handle-input)
           :render-code (render)
           :update-code (update)
           :cleanup-code (cleanup)))
