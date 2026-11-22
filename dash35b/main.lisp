@const-start

(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)

(import "version.lisp" 'code-version)
(read-eval-program code-version)

(import "config.lisp" 'code-config)
(read-eval-program code-config)

; Lib
(import "lib/vehicle-state.lisp" 'code-vehicle-state)
(read-eval-program code-vehicle-state)

(import "lib/colors.lisp" 'code-colors)
(read-eval-program code-colors)

(import "lib/user-settings.lisp" 'code-user-settings)
(read-eval-program code-user-settings)

(import "lib/persistent-settings.lisp" 'code-persistent-settings)
(read-eval-program code-persistent-settings)

(import "lib/statistics.lisp" 'code-statistics)
(read-eval-program code-statistics)

(import "lib/scene.lisp" 'code-scene)
(read-eval-program code-scene)

(import "lib/draw-utils.lisp" 'code-draw-utils)
(read-eval-program code-draw-utils)

(import "lib/input.lisp" 'code-input)
(read-eval-program code-input)

(import "lib/communication.lisp" 'code-communication)
(read-eval-program code-communication)

; Views
(import "lib/standalone.lisp" 'code-standalone)
(read-eval-program code-standalone)

(import "views/view_static.lbm" 'code-view-static)
(read-eval-program code-view-static)

(import "views/view_pages.lbm" 'code-view-pages)
(read-eval-program code-view-pages)

; Assets
(import "assets/sym_vesc_37x34.bin" 'img-vesc)
(import "assets/vesclabs_260x80.bin" 'img-vesclabs)
(import "assets/motor.bin" 'img-wheelie-bike)
(import "assets/batt_level_50x210.bin" 'img-batt-level)
(import "assets/highbeam_32x24.bin" 'img-highbeam)
(import "assets/lowbeam_32x24.bin" 'img-lowbeam)
(import "assets/indicator_l_34x30.bin" 'img-indicator-l)
(import "assets/indicator_r_34x30.bin" 'img-indicator-r)
(import "assets/kickstand_26x29.bin" 'img-kickstand)
(import "assets/temp_b_37x38.bin" 'img-temp-b)
(import "assets/temp_e_37x38.bin" 'img-temp-e)
(import "assets/temp_m_37x38.bin" 'img-temp-m)
(import "assets/warning_35x38.bin" 'img-warning)
(import "assets/cruise_40x36.bin" 'img-cruise)
(import "assets/charging_160x61.bin" 'img-charging)

; Fonts
(import "font/roboto-bold-16-4c.bin" 'font-16)
(import "font/roboto-bold-32-4c.bin" 'font-32)
(import "font/roboto-bold-48-4c.bin" 'font-48)
(import "font/roboto-bold-16-2c.bin" 'font-16-2c)
(import "font/roboto-bold-90-2c.bin" 'font-90)

(defun write-adjusted-setting (setting op step limits) {
        (var value (eval (list op (read-setting setting) step)))
        (write-setting setting (clamp value (first limits) (second limits)))
})

(defun adjust-setting (setting op step limits) {
        (write-adjusted-setting setting op step limits)
        (settings-load)
        (settings-apply-units)
        (disp-set-bl (if backlight-dim settings-bl-dim settings-bl-bright))
})

(defun setting-update (op) {
        (if (> setting-num 0)
            (adjust-setting (ix setting-list-page1 setting-now) op
                (ix setting-step-page1 setting-now) (ix setting-lim-page1 setting-now)))
})

; Stored in eeprom, so these keep their meaning.
; 0 none  1 page+  2 page-  3 settings  4 mode+  5 mode-
; 6 lights  7 backlight dim  8 cruise
(defun btn-do-action (a)
    (cond
        ((= a 1) (setq page-now (mod (+ page-now 1) page-num)))
        ((= a 2) (setq page-now (mod (+ page-now (- page-num 1)) page-num)))
        ((= a 3) (setq page-now (if (= page-now page-num) 0 page-num)))
        ((= a 4) (if (< drive-mode (- drive-mode-num 1)) (mode-set (+ drive-mode 1))))
        ((= a 5) (if (> drive-mode 0) (mode-set (- drive-mode 1))))
        ((= a 6) (setq light-on (not light-on)))
        ((= a 7) {
                (setq backlight-dim (not backlight-dim))
                (disp-set-bl (if backlight-dim settings-bl-dim settings-bl-bright))
        })
        ((= a 8) (comm-send-event 0))
        ((= a 9) (comm-send-event 2)) ; start or stop logging on the controller
        ((= a 10) (wheelie-toggle))
        (t nil)
))

; On the settings page short presses always navigate it
(defun btn-short (idx)
    (cond
        (wheelie-open (wheelie-button idx))
        ((= page-now page-num)
            (cond
                ((= idx 0)
                    (if (> setting-num 0)
                        (setq setting-now (mod (+ setting-now 1) setting-num))))
                ((= idx 1) (setting-update -))
                ((= idx 2) (setting-update +))
                (t (btn-do-action (ix btn-actions-short idx)))))
        (t (btn-do-action (ix btn-actions-short idx)))))

; Shown once at boot, so a dead panel looks different from a blank one.
; A short-lived banner over the normal views, used for things the controller
; reports back such as logging starting and stopping. Cleared by forcing the
; views to redraw rather than by repainting what was underneath.
(def notify-txt nil)
(def notify-ts 0)

(defun notify (txt) {
        (setq notify-txt txt)
        (setq notify-ts (systime))
})

(defun notify-draw (txt) {
        (var imgbuf (scene-buffer 'indexed4 260 34))
        (scene-clear imgbuf)
        (scene-rectangle imgbuf 0 0 260 34 1 '(rounded 6))
        (ttf-txt-center txt font-16 imgbuf)
        (scene-present imgbuf 110 143 colors-text-aa)
})

(defun notify-thread () {
        (var service-last false)

        (loopwhile t {
                ; Leaving service mode uncovers whatever the banner sat on
                (if (and service-last (not (or service-mode motor-bad))) {
                        (setq view-force-static true)
                        (setq view-force-pages true)
                })
                (setq service-last (or service-mode motor-bad))

                (cond
                    ; The motor will not run at all in this state, so this
                    ; outranks everything else on screen.
                    (motor-bad (notify-draw "MOTOR CONFIG BAD"))

                    ; With the drive profile suspended the mode on screen means
                    ; nothing and neutral no longer holds the throttle shut, so
                    ; say so, and keep saying it until it is switched back.
                    (service-mode (notify-draw "SERVICE"))

                    (notify-txt
                        (if (> (secs-since notify-ts) 2.5) {
                                (setq notify-txt nil)
                                (setq view-force-static true)
                                (setq view-force-pages true)
                        }
                        (notify-draw notify-txt)))
                )

                (sleep 0.2)
        })
})

(defun splash-progress (progress start duration) {
        (var t0 (clamp01 (/ (- progress start) duration)))
        (* t0 t0 (- 3.0 (* 2.0 t0)))
})

(defun show-splash () {
        (var label (scene-buffer 'indexed4 192 20))
        (ttf-txt-center splash-version font-16 label)
        (disp-set-bl settings-bl-bright)

        (var draw-frame (fn (progress) {
                (var fade (splash-progress progress 0.0 0.82))
                (var orange (color-mix color-bg 0xF05A22 fade))
                (var white (color-mix color-bg 0xFBFCFC fade))
                (scene-present img-vesclabs 110 100 (list color-bg orange orange white))

                (if (> progress 0.52) {
                        (var caption (splash-progress progress 0.52 0.48))
                        (var palette
                            (map (fn (color) (color-mix color-bg color caption)) colors-text-aa))
                        (scene-present label 144 196 palette)
                })
        }))

        (scene-animate 0.72 draw-frame)
        (sleep 0.42)
        (scene-fill 110 100 260 116 color-bg)
})

(defun main () {
        (disp-set-bl 0)
        (scene-init)

        (set-print-prefix "DISP-")

        (def init-complete nil)
        (def rx-cnt-can 0)

        (if (eq (car (trap {
                            (settings-load)
                            (settings-build)
                            (colors-build)
                })) 'exit-error) {
                (print "Settings failed to load, using defaults")

                (trap {
                        (restore-settings)
                        (settings-load)
                        (settings-build)
                        (colors-build)
                })
        })

        (settings-apply-units)

        (if config-code-server (start-code-server)) ; Enable remote code execution

        (disp-reset)
        (disp-orientation 3)
        (scene-clear-screen color-bg)
        (sleep 0.04)

        (if settings-splash {
                (var result (trap (show-splash)))
                (if (eq (car result) 'exit-error) {
                        (disp-set-bl 0)
                        (scene-clear-screen color-bg)
                        (sleep 0.04)
                })
        })
        (disp-set-bl settings-bl-bright)

        (event-register-handler (spawn event-handler))
        (event-enable 'event-can-sid)
        (event-enable 'event-data-rx)

        (loopwhile-thd ("Stats" 200) t {
                (print "Starting stats-thread")

                (match (trap (stats-thread))
                    ((exit-ok (? a)) (print "Stats-thread exit"))
                    (_ (print "Stats-thread crashed"))
                )

                (sleep 5.0)
        })

        (input-cleanup-on-pressed)
        (loopwhile-thd ("Input" 200) t {
                (print "Starting Input-thread")

                (match (trap (input-thread))
                    ((exit-ok (? a)) (print "Input-thread exit"))
                    (_ (print "Input-thread crashed"))
                )

                (sleep 5.0)
        })

        (loopwhile-thd ("CommTX" 200) t {
                (print "Starting CommTX-thread")

                (match (trap (comm-tx-thread))
                    ((exit-ok (? a)) (print "CommTX-thread exit"))
                    (_ (print "CommTX-thread crashed"))
                )

                (sleep 5.0)
        })

        (loopwhile-thd ("Notify" 120) t {
                (print "Starting Notify-thread")

                (match (trap (notify-thread))
                    ((exit-ok (? a)) (print "Notify-thread exit"))
                    (_ (print "Notify-thread crashed"))
                )

                (sleep 5.0)
        })

        (loopwhile-thd ("ViewStatic" 200) t {
                (print "Starting ViewStatic-thread")

                (match (trap (view-static-thread))
                    ((exit-ok (? a)) (print "ViewStatic-thread exit"))
                    (_ (print "ViewStatic-thread crashed"))
                )

                (sleep 5.0)
        })

        (loopwhile-thd ("ViewPages" 200) t {
                (print "Starting ViewPages-thread")

                (match (trap (view-pages-thread))
                    ((exit-ok (? a)) (print "ViewPages-thread exit"))
                    (_ (print "ViewPages-thread crashed"))
                )

                (sleep 5.0)
        })

        (loopwhile-thd ("Standalone" 200) t {
                (print "Starting Standalone-thread")

                (match (trap (standalone-thread))
                    ((exit-ok (? a)) (print "Standalone-thread exit"))
                    (_ (print "Standalone-thread crashed"))
                )

                (sleep 5.0)
        })

        (loopwhile-thd ("Worker" 150) t {
                (if settings-redraw (trap (settings-apply-visual)))
                (if battery-a-charging (mode-set 1)) ; Put in neutral when charging
                (if kickstand-down (mode-set 1)) ; Put in neutral when kickstand is down
                (sleep 0.1)
        })

        (def init-complete true)

        (def on-btn-0-pressed (fn () (btn-short 0)))
        (def on-btn-1-pressed (fn () (btn-short 1)))
        (def on-btn-2-pressed (fn () (btn-short 2)))
        (def on-btn-3-pressed (fn () (btn-short 3)))

        (def on-btn-0-long-pressed (fn () (btn-do-action (ix btn-actions-long 0))))
        (def on-btn-1-long-pressed (fn () (btn-do-action (ix btn-actions-long 1))))
        (def on-btn-2-long-pressed (fn () (btn-do-action (ix btn-actions-long 2))))
        (def on-btn-3-long-pressed (fn () (btn-do-action (ix btn-actions-long 3))))

        (def on-btn-0-repeat-press nil)
        (def on-btn-1-repeat-press nil)
        (def on-btn-2-repeat-press nil)
        (def on-btn-3-repeat-press nil)
})

@const-end

(image-save)
(main)

