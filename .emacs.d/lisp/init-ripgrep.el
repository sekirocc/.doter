;; -*- lexical-binding: t -*-
;;; init-ripgrep.el --- Ripgrep Configuration

;;; Commentary:
;;
;;  Fast grep tool configuration
;;

;;; Code:

(defun my-ripgrep-kill-buffer()
  (interactive)
  (ripgrep/kill-buffer)
  (delete-window))


;;; rg.el
(use-package rg
  :ensure nil
  :bind
  (
    ("C-c g" . rg)
    )
  :config
  (setq rg-command-line-flags '("--context" "2"))
  (setq rg-show-header nil)


  (defun my-disable-my-special-buffer-keys (&rest r)
    (my-change-buffer-name-from-special-to-legendary (buffer-name))
    (my-special-buffer-keys-minor-mode 0))

  (defun my-enable-my-special-buffer-keys (&rest r)
    (my-change-buffer-name-from-legendary-to-special (buffer-name))
    (my-special-buffer-keys-minor-mode 1))


  (advice-add 'wgrep-change-to-wgrep-mode :after #'my-disable-my-special-buffer-keys)
  (advice-add 'wgrep-finish-edit :after #'my-enable-my-special-buffer-keys)

  (advice-remove 'wgrep-change-to-wgrep-mode #'my-disable-my-special-buffer-keys)
  (advice-remove 'wgrep-finish-edit #'my-enable-my-special-buffer-keys)

  )



; (use-package ripgrep
;   :ensure t
;   :config
;   ;; Face configuration moved to custom-set-faces
;   :bind
;   (:map ripgrep-search-mode-map
;         (";" . scroll-up-command)
;         ("'" . scroll-down-command)
;         ("k" . previous-line)
;         ("j" . next-line)
;         ("x" . my-ripgrep-kill-buffer)))

(provide 'init-ripgrep)

;;; init-ripgrep.el ends here
