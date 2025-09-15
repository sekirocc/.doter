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
