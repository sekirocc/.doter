(require 'diminish)
(defun purge-minor-modes ()
  (diminish 'global-whitespace-mode)
  (diminish 'yank-indent-mode)
  (diminish 'format-all-mode)
  (diminish 'flymake-mode)
  (diminish 'ivy-mode)
  (diminish 'ivy-posframe-mode)
  (diminish 'which-key-mode)
  (diminish 'selected-minor-mode)
  (diminish 'selected-global-mode)
  (diminish 'my-keys-minor-mode)
  (diminish 'projectile-mode)
  (diminish 'global-hl-line-mode)
  (diminish 'highlight-parentheses-mode)
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'company-posframe-mode)
  (diminish 'global-company-mode)
  (diminish 'line-number-mode)
  (diminish 'global-eldoc-mode)
  (diminish 'eldoc-mode)
  (diminish 'yas-minor-mode)
  (diminish 'smartparens-mode)
  (diminish 'smartparens-global-mode)
  (diminish 'show-paren-mode)
  (diminish 'abbrev-mode)
  (diminish 'electric-indent-mode))
(add-hook 'after-change-major-mode-hook 'purge-minor-modes)


(provide 'init-diminish)