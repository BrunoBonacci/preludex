;;; prodigy installation

(prelude-require-packages '(prodigy))
(require 'prodigy)

;;
;; if a .prodigy exists load it
;;
(if (file-exists-p "~/.prodigy.el")
    (load "~/.prodigy.el"))

;; set key binding
(global-set-key (kbd "C-x p") 'prodigy)
