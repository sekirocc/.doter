

(setq ignored-projectile-projects (list "/opt/homebrew/" "Xcode.app"))
(defun ignored-projectile-project (project-root)
  (seq-filter (lambda (candidate) (string-search candidate project-root))
    ignored-projectile-projects))

(setq-default
  projectile-cache-file (expand-file-name "~/.emacs.d/.local/projectile.cache")
  projectile-known-projects-file (expand-file-name "~/.emacs.d/.local/projectile-bookmarks.eld")
  projectile-enable-caching t
  projectile-indexing-method 'native
  projectile-track-known-projects-automatically t
  projectile-ignored-project-function 'ignored-projectile-project)

(require 'projectile)
(projectile-mode 1)



(setq projectile-completion-system 'ivy)
(defun my-projectile-find-file ()
  (interactive)
  (projectile-find-file)
)




(defun my/return-t (orig-fun &rest args) t)

(defun my/disable-yornp (orig-fun &rest args)
  (advice-add 'yes-or-no-p :around #'my/return-t)
  (advice-add 'y-or-n-p :around #'my/return-t)
  (let ((res (apply orig-fun args)))
    (advice-remove 'yes-or-no-p #'my/return-t)
    (advice-remove 'y-or-n-p #'my/return-t)
    res))


(advice-add 'projectile-compile-project :around #'my/disable-yornp)
(advice-add 'projectile-run-project :around #'my/disable-yornp)


(provide 'init-projectile)