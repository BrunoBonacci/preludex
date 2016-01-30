;;; Selecting solarized theme

(disable-theme 'zenburn)
(setq prelude-theme nil)


;;(if (require 'solarized-theme nil 'noerror)
;;  (setq prelude-theme 'solarized-dark)
;;  (setq prelude-theme nil))
(setq solarized-use-variable-pitch nil)

(set-face-attribute 'default nil :height 140)
;; hightlight modeline of active buffer
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "green4")
(set-face-background 'modeline-inactive "grey20")
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
