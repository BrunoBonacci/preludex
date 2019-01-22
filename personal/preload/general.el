;; Pinning cider to stable version
(require 'package)
(setq package-check-signature nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;
;; Pin CIDER and clj-refactor to stable versions
;; *comment this for the latest development versions*
;;
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
