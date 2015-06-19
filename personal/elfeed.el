;;; prodigy installation

(prelude-require-packages '(elfeed))
(require 'elfeed)

;;
;; if a .prodigy exists load it
;;
(if (file-exists-p "~/.feeds.el")
    (load "~/.feeds.el"))

;; set key binding
(global-set-key (kbd "C-x w") 'elfeed)
