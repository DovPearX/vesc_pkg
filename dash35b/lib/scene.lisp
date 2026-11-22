(def scene-strip nil)
(def scene-lock nil)

@const-start

(defun scene-init () {
        (setq scene-strip (bufcreate (+ 5 8192)))
        (setq scene-lock (mutex-create))
})

(defun scene-buffer (format width height)
    (list nil
        (cond
            ((eq format 'indexed2) 1)
            ((eq format 'indexed4) 2)
            ((eq format 'indexed16) 4)
            ((eq format 'rgb332) 8)
            ((eq format 'rgb565) 16)
            ((eq format 'rgb888) 24)
            (t (exit-error 'type_error))
        )
    width height 0
    )
)

(defun scene-dims (image)
    (if (eq (type-of image) 'type-list)
        (list (ix image 2) (ix image 3))
        (img-dims image)
    )
)

(defun scene-clear (image) {
        (setcar image nil)
        (var color (ix (rest-args) 0))
        (setix image 4 (if color color 0))
})

(defun scene-command (draw args source-first) {
        (var coords (if source-first (cdr args) args))
        (var x (car coords))
        (var y (ix coords 1))
        (var position (cons x (cons y (cdr (cdr coords)))))
        (var arguments (if source-first (cons (car args) position) position))
        (var call (cons scene-strip arguments))
        (list draw x y position call)
})

(defun scene-record (image draw args source-first)
    (setcar image (cons (scene-command draw args source-first) (car image)))
)

(defun scene-rectangle (image)
    (scene-record image img-rectangle (rest-args) nil)
)

(defun scene-circle (image)
    (scene-record image img-circle (rest-args) nil)
)

(defun scene-arc (image)
    (scene-record image img-arc (rest-args) nil)
)

(defun scene-blit (image)
    (scene-record image img-blit (rest-args) t)
)

(defun scene-text (image)
    (scene-record image ttf-text (rest-args) nil)
)

(defun scene-min (a b) (if (< a b) a b))
(defun scene-max (a b) (if (> a b) a b))

(defun scene-clip (x y width height) {
        (var area (ix (rest-args) 0))
        (var (ax ay aw ah) (if area area '(0 0 480 320)))
        (var left (scene-max (scene-max 0 x) ax))
        (var top (scene-max (scene-max 0 y) ay))
        (var right (scene-min (scene-min 480 (+ x width)) (+ ax aw)))
        (var bottom (scene-min (scene-min 320 (+ y height)) (+ ay ah)))
        (if (and (> right left) (> bottom top))
            (list left top (- right left) (- bottom top))
        )
})

(defun scene-shift-x (commands origin) {
        (loopforeach command commands
            (setcar (ix command 3) (- (ix command 1) origin))
        )
})

(defun scene-replay (commands origin) {
        (loopforeach command commands {
                (var (draw x y position call) command)
                (setcar (cdr position) (- y origin))
                (apply draw call)
        })
})

(defun scene-flush (damage x y bits background commands colors) {
        (var (left top width height) damage)
        (var pixels (/ (* (- (buflen scene-strip) 5) 8) bits))
        (var tile-width (if (< width pixels) width pixels))
        (bufset-u8 scene-strip 4 bits)
        (looprange tile 0 (/ (+ width tile-width -1) tile-width) {
                (var dx (* tile tile-width))
                (var remaining-x (- width dx))
                (var w (if (< remaining-x tile-width) remaining-x tile-width))
                (var rows (/ pixels w))
                (bufset-u16 scene-strip 0 w)
                (scene-shift-x commands (- (+ left dx) x))
                (looprange strip 0 (/ (+ height rows -1) rows) {
                        (var dy (* strip rows))
                        (var strip-height (scene-min (- height dy) rows))
                        (bufset-u16 scene-strip 2 strip-height)
                        (img-clear scene-strip background)
                        (scene-replay commands (- (+ top dy) y))
                        (disp-render scene-strip (+ left dx) (+ top dy) colors)
                })
        })
        true
})

(defun scene-draw-region (image x y colors area) {
        (var recorded (eq (type-of image) 'type-list))
        (var (width height)
        (if recorded (list (ix image 2) (ix image 3)) (img-dims image)))
        (var damage (scene-clip x y width height area))
        (if damage
            (cond
                (recorded {
                        (var commands (reverse (car image)))
                        (scene-flush damage x y (ix image 1) (ix image 4) commands colors)
                })
                ((and (= (ix damage 2) width) (= (ix damage 3) height))
                    (disp-render image x y colors))
                (t {
                        (var command (scene-command img-blit (list image 0 0 -1) t))
                        (scene-flush damage x y (bufget-u8 image 4) 0 (list command) colors)
                })
            )
            true)
})

(defun scene-present (image x y) {
        (var args (rest-args))
        (var colors (ix args 0))
        (var area (ix args 1))
        (mutex-lock scene-lock)
        (var result (trap (scene-draw-region image (to-i x) (to-i y) colors area)))
        (mutex-unlock scene-lock)
        (if (eq (car result) 'exit-ok)
            (ix result 1)
            (exit-error (ix result 1)))
})

(defun scene-fill (x y width height color)
    (scene-present (scene-buffer 'indexed2 width height) x y (list color color)))

(defun scene-clear-screen (color) {
        (mutex-lock scene-lock)
        (var result (trap (disp-clear color)))
        (mutex-unlock scene-lock)
        (if (eq (car result) 'exit-ok)
            (ix result 1)
            (exit-error (ix result 1)))
})

(defun scene-animate (duration draw) {
        (var started (systime))
        (var frame -1)
        (var elapsed (secs-since started))
        (loopwhile (< elapsed duration) {
                (var slot (to-i (* elapsed 30.0)))
                (if (= frame slot)
                    (sleep 0.004)
                    {
                        (setq frame slot)
                        (draw (/ elapsed duration))
                    }
                )
                (setq elapsed (secs-since started))
        })
        (draw 1.0)
})
