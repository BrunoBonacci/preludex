;;;; Customization for shell modes


;; Add more natural <up> and <down> key bindings for shell mode
(defun my-shell-mode-keys ()
  "Modify keymaps used by repl."
  (local-set-key (kbd "<up>")   'comint-previous-input)
  (local-set-key (kbd "<down>") 'comint-next-input)
  )

(add-hook 'shell-mode-hook 'my-shell-mode-keys)
