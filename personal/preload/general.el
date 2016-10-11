;; Pinning cider to stable version
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
