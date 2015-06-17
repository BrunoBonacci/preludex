;;; Selecting solarized theme

;;(if (require 'color-theme-sanityinc-solarized nil 'noerror)
;;  (setq prelude-theme 'sanityinc-solarized-dark)
;;  (setq prelude-theme nil))


(set-face-attribute 'default nil :height 140)
;; hightlight modeline of active buffer
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "green3")
(set-face-background 'modeline-inactive "grey20")
