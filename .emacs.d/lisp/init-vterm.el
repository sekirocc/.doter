;; -*- lexical-binding: t -*-
;;; init-vterm.el --- VTerm Configuration

;;; Commentary:
;;
;;  Terminal emulator configuration
;;

;;; Code:

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-timer-delay 0.001
        vterm-max-scrollback 10000)

  (define-prefix-command 'my-vterm-ctrl-s-key)
  (define-key vterm-mode-map (kbd "C-s") 'my-vterm-ctrl-s-key)

  (define-key vterm-mode-map (kbd "C-s [")  #'vterm-copy-mode)
  (define-key vterm-copy-mode-map (kbd "y")  #'vterm-copy-mode-done)
  (define-key vterm-copy-mode-map (kbd "M-w")  #'vterm-copy-mode-done)

  (define-key vterm-mode-map (kbd "C-s-c")  #'(lambda() (interactive) (vterm-send-key "c" nil nil t)))
  (define-key vterm-mode-map (kbd "M-i")  #'er/expand-region)
  (add-hook 'vterm-copy-mode-hook #'(lambda() (if (bound-and-true-p vterm-copy-mode)
                                                   (my-special-buffer-keys-minor-mode 1)
                                                 (my-special-buffer-keys-minor-mode 0))))
  (keymap-unset vterm-mode-map "M-`")
  (keymap-unset vterm-mode-map "M-:")
  )

(use-package multi-vterm
  :ensure t
  (setq multi-vterm-dedicated-window-height-percent 90)
  )



(provide 'init-vterm)

;;; init-vterm.el ends here
