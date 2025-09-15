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

  ;; fix claude code line flickering
  ;; https://github.com/anthropics/claude-code/issues/247#issuecomment-3058405139
  (defun diego--vterm-font-setup ()
    "Configure font settings specifically for vterm buffers, workaround claude-code."

    ;; Apply ASCII replacements for vterm specifically
    (let ((tbl (or buffer-display-table (setq buffer-display-table (make-display-table)))))
      (dolist (pair
                '((#x273B . ?*) ; ✻ TEARDROP-SPOKED ASTERISK
                   (#x273D . ?*) ; ✽ HEAVY TEARDROP-SPOKED ASTERISK
                   (#x2722 . ?+) ; ✢ FOUR TEARDROP-SPOKED ASTERISK
                   (#x2736 . ?+) ; ✶ SIX-POINTED BLACK STAR
                   (#x2733 . ?*) ; ✳ EIGHT SPOKED ASTERISK
                   ))
        (aset tbl (car pair) (vector (cdr pair))))))

  (add-hook 'vterm-mode-hook #'diego--vterm-font-setup)
  )

(use-package multi-vterm
  :ensure t
  :config
  (setq multi-vterm-dedicated-window-height-percent 90)
  )



;; 当用户主动滚动到上方时，自动进入 copy mode，从而阻止自动刷新终端
(with-eval-after-load 'pixel-scroll
  (advice-add 'pixel-scroll-precision :after
    (defun me/vterm-toggle-scroll (&rest _)
      (when (eq major-mode 'vterm-mode)
        (if (> (window-end) (buffer-size))
          (when vterm-copy-mode (vterm-copy-mode-done nil))
          (vterm-copy-mode 1)))))
  )


(provide 'init-vterm)

;;; init-vterm.el ends here
