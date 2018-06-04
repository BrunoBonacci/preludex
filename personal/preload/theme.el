;;; Selecting solarized theme

(disable-theme 'zenburn)
(setq prelude-theme nil)


;;(if (require 'solarized-theme nil 'noerror)
;;  (setq prelude-theme 'solarized-dark)
;;  (setq prelude-theme nil))
(setq solarized-use-variable-pitch nil)


;; set favourite font
(defvar default-preferred-font "Roboto Mono Thin for Powerline"
  "Select your default preferred font.  This will be activated only if present.")
(defvar default-preferred-font-size 150
  "Select your preferred font size.")

;; enable default font if present
(if (member default-preferred-font (font-family-list))
    (progn
      (add-to-list 'default-frame-alist
                   `(font . ,(concat default-preferred-font  "-" (number-to-string (/ default-preferred-font-size 10)))))
      (set-frame-font default-preferred-font))
  (message (format "'%s' font not available" default-preferred-font)))

;; set default font size
(set-face-attribute 'default nil :height default-preferred-font-size)

;; hightlight modeline of active buffer
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "green4")
(set-face-background 'mode-line-inactive "grey20")
(set-face-foreground 'mode-line-buffer-id "white")

;; highligh tags
(custom-set-faces
 '(org-tag
   ((t (:foreground "goldenrod"
        ;;:box (:line-width 1 :color "DarkGoldenrod4")
        :weight regular)))))


;; disable scroll bars
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
