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

  (defun my-vterm-copy-region(arg)
    (interactive "p")
    (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
      (kill-whole-line)))

  (setq vterm-timer-delay 0.001
    vterm-max-scrollback 10000)

  (define-prefix-command 'my-vterm-ctrl-s-key)
  (define-key vterm-mode-map (kbd "C-s") 'my-vterm-ctrl-s-key)

  (define-key vterm-mode-map (kbd "s-c")  #'vterm-copy-mode-done)
  (define-key vterm-copy-mode-map (kbd "s-c")  #'my-vterm-copy-region)

  (define-key vterm-copy-mode-map [return] #'vterm-copy-mode)

  (define-key vterm-mode-map (kbd "C-s [")  #'vterm-copy-mode)
  (define-key vterm-copy-mode-map (kbd "y")  #'vterm-copy-mode-done)
  (define-key vterm-copy-mode-map (kbd "M-w")  #'vterm-copy-mode-done)

  (define-key vterm-mode-map (kbd "C-s-c")  #'(lambda() (interactive) (vterm-send-key "c" nil nil t)))
  (define-key vterm-mode-map (kbd "M-i")  #'er/expand-region)

  (defun my-toggle-legendary-buffer-for-vterm()
    (if (bound-and-true-p vterm-copy-mode)
      (my-remove-from-legendary-buffers "*vterm")
      (my-add-to-legendary-buffers "*vterm"))
    (refresh-current-mode))

  (add-hook 'vterm-copy-mode-hook #'my-toggle-legendary-buffer-for-vterm)
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


(defun me/vterm-toggle-scroll (&rest event)
  (when (eq major-mode 'vterm-mode)
    (let ((event-type (car-safe (car-safe event))))
      (cond
        ((eq event-type 'triple-wheel-up)
          (unless vterm-copy-mode (vterm-copy-mode 1)))
        ((eq event-type 'triple-wheel-down)
          (when (and vterm-copy-mode (> (window-end) (buffer-size)))
            (vterm-copy-mode-done nil)))))))



;; 当用户主动滚动到上方时，自动进入 copy mode，从而阻止自动刷新终端
(with-eval-after-load 'pixel-scroll
  (advice-add 'pixel-scroll-precision :after #'me/vterm-toggle-scroll))


(provide 'init-vterm)

;;; init-vterm.el ends here
