(require 'god-mode)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(setq god-mode-alist '((nil . "C-") ("r" . "M-") ("R" . "C-M-")))




;; special-buffers are not affected by god-mode bindings, but affected by my-special-buffer-keys-minor-mode-map
(setq special-buffer-modes (list "dired-mode"))
(setq special-buffers (list
                        "*Pos-Frame-Read*"
                        "*Treemacs"
                        "*Messages*"
                        "HELLO"
                        "*Ibuffer*"
                        "*deadgrep"
                        "*xref"
                        "*Buffer"
                        "*Annotate"
                        "*vc-diff*"
                        "*Packages"
                        "*lsp-log*"
                        "*Help*"
                        "*Ivy"
                        "*Occur*"
                        "*info*"
                        "*Warnings*"
                        "helm-*"
                        ;; "*helm-mode-switch-to-buffer*"
                        "*Helm Help*"
                        "*Flymake diagnostics"
                        "*ansi-term*"
                        "*fzf*"
                        "*Ilist*"
                        "*NeoTree*"))

;; legendary-buffers are not affected by god-mode AND my-special-buffer-keys-minor-mode-map
(setq legendary-buffers (list
                          "*this-buffer-is-left-alone-without-god-mode-at-all"
                          "*Minibuf"
                          "*terminal*"
                          "*eshell*"
                          "*blink-search"
                          "*blink search"
                          "*shell*"
                          "*slime"
                          "*sldb"
                          "magit"
                          "git-rebase-todo"
                          "*Backtrace*"
                          "menu"
                          "*ielm*"
                          "*slime-repl"
                          "*Customize"))

(setq legendary-modes (list
                        "*this-buffer-is-left-alone-without-god-mode-at-all"
                        "cfrs-input-mode"
                        "lisp-mode"
                        "slime-mode"
                        "minibuffer-mode"
                        "deadgrep-edit-mode"))





(defun my-god-this-is-special-buffer (bufname)
  (interactive)
  (let ((this-buffer-name (string-trim bufname))
         (this-buffer-mode (symbol-name (buffer-mode bufname))))
    (or
      (seq-filter
        (lambda (n) (string-prefix-p n this-buffer-name))
        special-buffers)
      (seq-filter
        (lambda (n) (string-prefix-p n this-buffer-mode))
        special-buffer-modes))))

(defun* my-god-this-is-legendary-buffer (bufname)
  (interactive)
  ;; (message "buffer-mode type is %s" (type-of (buffer-mode bufname))) ==> symbol
  ;;;; use symbol-name convert symbol to string; And for the reverse, (intern "some-string") to get symbol
  (let ((this-buffer-name (string-trim bufname))
         (this-buffer-mode (symbol-name (buffer-mode bufname))))
    ;; (message "this-buffer-name %s" this-buffer-name)
    ;; (message "this-buffer-mode %s" this-buffer-mode)
    (or
      (seq-filter
        (lambda (n) (string-prefix-p n this-buffer-name))
        legendary-buffers)
      (seq-filter
        (lambda (n) (string-prefix-p n this-buffer-mode))
        legendary-modes))))


(defun my-god-this-is-normal-editor-buffer (bufname)
  (interactive)
  (not
    (or (my-god-this-is-special-buffer bufname)
      (my-god-this-is-legendary-buffer bufname))))


(setq my-god-mode-is-active-flag nil)

(defun* refresh-current-mode ()
  (interactive)
  (cond
    ((my-god-this-is-legendary-buffer (buffer-name))
      ;; (message "%s is legendary buffer" (buffer-name))
      (my-ctrl-w-window-keys-minor-mode 0)
      (god-local-mode 0)
      (hl-line-mode 0)
      (my-keys-minor-mode 0)
      (my-ctrl-w-window-keys-minor-mode 0)
      (my-special-buffer-keys-minor-mode 0))
    ((my-god-this-is-special-buffer (buffer-name))
      ;; (message "%s is special buffer" (buffer-name))
      (my-ctrl-w-window-keys-minor-mode 1)
      (god-local-mode 0)
      (hl-line-mode 1)
      (my-keys-minor-mode 0)
      (my-special-buffer-keys-minor-mode 1))
    (t
      ;; (message "%s not a special buffer" (buffer-name))
      (my-ctrl-w-window-keys-minor-mode 1)
      (god-local-mode 1) ;; start local mode
      (hl-line-mode 1)
      (my-keys-minor-mode 1)
      ;; (visual-line-mode 1)
      ;; (global-visual-line-mode 1) ;;
      (setq my-god-mode-is-active-flag t)
      (my-special-buffer-keys-minor-mode 0))
    ))

(add-hook 'find-file-hook 'refresh-current-mode)

(defun my-quit-god-mode ()
  (interactive)
  (my-ctrl-w-window-keys-minor-mode 0)
  (god-local-mode 0)
  (setq my-god-mode-is-active-flag nil))

(defun my-toggle-god-mode ()
  (interactive)
  (if (bound-and-true-p god-local-mode)
    (my-quit-god-mode)
    (refresh-current-mode)))




(defun my-god-mode-with-switch-any-buffer (prev curr)
  (cl-assert (eq curr (current-buffer))) ;; Always t
  ;; (message "%S -> %S -> %S" prev curr (string-trim (buffer-name curr)))
  (refresh-current-mode))

(add-hook 'switch-buffer-functions #'my-god-mode-with-switch-any-buffer)


(defun my-god-below-newline-and-insert-mode ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (my-quit-god-mode))

(defun my-god-above-newline-and-insert-mode ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command)
  (my-quit-god-mode))

(defun my-god-mwin-end-and-insert-mode ()
  (interactive)
  (mwim-end-of-code-or-line)
  (my-quit-god-mode))

(defun my-god-mwin-beginning-and-insert-mode ()
  (interactive)
  (mwim-beginning-of-code-or-line)
  (my-quit-god-mode))

(defun my-god-char-forward-and-insert-mode ()
  (interactive)
  (forward-char)
  (my-quit-god-mode))


;; donot warnning, just wrap search
(defun isearch-repeat-forward+ ()
  (interactive)
  (unless isearch-forward
    (goto-char isearch-other-end))
  (isearch-repeat-forward)
  (unless isearch-success
    (isearch-repeat-forward))
  ;; (recenter)
  )

(defun isearch-repeat-backward+ ()
  (interactive)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward)
  (unless isearch-success
    (isearch-repeat-backward))
  ;; (recenter)
  )

(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "TAB") #'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "TAB") #'god-mode-isearch-disable)
;; RET to god, then RET to exit
;; (define-key isearch-mode-map (kbd "RET") #'god-mode-isearch-activate)
;; this variable make sure RET exit isearch-mode immediately

(define-key isearch-mode-map (kbd "RET") #'isearch-exit)
(define-key isearch-mode-map (kbd "C-l") #'recenter-top-bottom)

(define-key isearch-mode-map (kbd "C-s") 'isearch-repeat-forward+)
(define-key isearch-mode-map (kbd "C-r") 'isearch-repeat-backward+)
;; like vim
;; reset all other keys. then add our specific keys.
(set-char-table-range (nth 1 god-mode-isearch-map) t #'ignore)
(define-key god-mode-isearch-map (kbd "n") 'isearch-repeat-forward+)
(define-key god-mode-isearch-map (kbd "N") 'isearch-repeat-backward+)
(define-key god-mode-isearch-map (kbd "p") 'yank)
(define-key god-mode-isearch-map (kbd "u") 'undo)
(define-key god-mode-isearch-map (kbd "d") 'kill-whole-line)
(define-key god-mode-isearch-map (kbd "j") #'(lambda ()
                                               (interactive)
                                               (isearch-exit)
                                               (next-line)))
(define-key god-mode-isearch-map (kbd "k") #'(lambda ()
                                               (interactive)
                                               (isearch-exit)
                                               (previous-line)))
(define-key god-mode-isearch-map (kbd "h") #'(lambda ()
                                               (interactive)
                                               (isearch-exit)
                                               (my-forward-char-no-cross-line)))
(define-key god-mode-isearch-map (kbd "l") 'recenter-top-bottom)
(define-key god-mode-isearch-map (kbd "C-l") 'recenter-top-bottom)
(define-key god-mode-isearch-map (kbd ";") 'scroll-up-command)
(define-key god-mode-isearch-map (kbd "'") 'scroll-down-command)
(define-key god-mode-isearch-map (kbd "s") 'isearch-repeat-forward+)
(define-key god-mode-isearch-map (kbd "r") 'isearch-repeat-backward+)
(define-key god-mode-isearch-map (kbd "RET") #'isearch-exit)


(defun my-search-selection ()
  "search for selected text"
  (interactive)
  (if (region-active-p)
    (let ((selection (buffer-substring-no-properties (mark) (point)))
           (case-fold-search 'default))
      (message "search the marked region")
      (deactivate-mark)
      (isearch-mode t nil nil nil)
      ;; activate god-mode-isearch
      (god-mode-isearch-activate)
      (isearch-yank-string selection))
    (progn
      (isearch-forward-symbol-at-point)
      (god-mode-isearch-activate))))


(defun my-isearch-forward ()
  (interactive)
  (isearch-mode t nil nil nil)
  (god-mode-isearch-activate)
  (isearch-repeat-forward+))


(defun my-isearch-backward ()
  (interactive)
  (isearch-mode nil nil nil nil)
  (god-mode-isearch-activate)
  (isearch-repeat-backward+))


(setq window-divider-color "#06C668")
(setq window-divider-right-color "#26282F")



(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (bound-and-true-p god-local-mode) 'box 'box))
  (set-cursor-color (if (bound-and-true-p god-local-mode) "red" "red"))
  (blink-cursor-mode (if (bound-and-true-p god-local-mode) -1 -1))
  (if (bound-and-true-p god-local-mode)
    (progn
      (set-face-attribute 'hl-line nil :foreground 'unspecified :background "#33485e")
      (set-face-attribute 'line-number-current-line nil :foreground "white" :background "#33485e")
      (when (display-graphic-p)
        (set-face-attribute 'window-divider nil :foreground window-divider-right-color)
        (set-face-attribute 'window-divider-first-pixel nil :foreground window-divider-right-color)
        (set-face-attribute 'window-divider-last-pixel nil :foreground window-divider-right-color)
        ;; (set-face-attribute 'mode-line nil          :background "#7AA2F7" :foreground "#262831" :overline "#374250"   :box nil) ;; draw a line above mode-line
        ;; (set-face-attribute 'mode-line-inactive nil :background "#262831" :foreground "#7AA2F7" :overline "#374250"  :box nil)
        ;; (set-face-attribute 'mode-line-buffer-id nil :distant-foreground "#262831" :foreground "#7AA2F7")
        )
      ;; (unless (display-graphic-p)
      ;;   (set-face-attribute 'mode-line          nil :foreground "black" :background "#00AFFF")
      ;;   (set-face-attribute 'mode-line-inactive nil :foreground "#00AFFF" :background "black")
      ;;   )
      ;; (setq cursor-type 'bar)
      ;; (set-cursor-color "red")
      ;; (set-face-attribute 'mode-line nil :box '(:line-width 1 :color "gray" ))
      ;; (set-face-attribute 'mode-line nil :background "#38424B")
      (set-face-foreground 'vertical-border window-divider-color)
      ;; (set-face-foreground 'vertical-border "#374250")
      )
    (progn
      (set-face-attribute 'line-number-current-line nil :foreground "black" :background "#7fdc59")
      (when (my-god-this-is-normal-editor-buffer (buffer-name))
        (set-face-attribute 'hl-line nil :background (face-background 'default)))
      ;; (set-face-attribute 'line-number-current-line nil :foreground "black" :background "#7fdc59")
      (when (display-graphic-p)
        (set-face-attribute 'window-divider nil :foreground window-divider-right-color)
        (set-face-attribute 'window-divider-first-pixel nil :foreground window-divider-right-color)
        (set-face-attribute 'window-divider-last-pixel nil :foreground window-divider-right-color)
        ;; (set-face-attribute 'mode-line nil          :background "#7fdc59" :foreground "black" :overline "green"   :box nil) ;; draw a line above mode-line
        ;; (set-face-attribute 'mode-line-inactive nil :background "#262831" :foreground "#7AA2F7" :overline "#374250"  :box nil)
        ;; (set-face-attribute 'mode-line-buffer-id nil :distant-foreground "#7AA2F7" :foreground "black")
        )
      ;; (unless (display-graphic-p)
      ;;   (set-face-attribute 'mode-line          nil :foreground "black" :background "cyan")
      ;;   (set-face-attribute 'mode-line-inactive nil :foreground "#00AFFF" :background "black")
      ;;   )
      ;; (setq cursor-type 'bar)
      ;; (set-cursor-color "red")
      ;; (set-face-attribute 'mode-line nil :box '(:line-width 1 :color "green" ))
      ;;(set-face-attribute 'mode-line nil :background "#38424B")
      ;; (set-face-foreground 'vertical-border "#7fdc59")
      (set-face-foreground 'vertical-border window-divider-color))))


(provide 'init-god)