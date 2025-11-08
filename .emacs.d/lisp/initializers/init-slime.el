
;;
;; brew install roswell
;;

(use-package slime
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "ros -L sbcl-bin run")
  ;; 在 Lisp 启动时就加载 Quicklisp 的 Swank
  (setq slime-lisp-implementations
        '((sbcl ("ros" "-L" "sbcl-bin"
                 "-e" "(ql:quickload :swank)"
                 "-e" "(swank:create-server :dont-close t)"
                 "run"))))
  :config
  (slime-setup '(slime-fancy)))

(use-package slime-company
  :after slime
  :bind ((:map slime-repl-mode-map
           ("C-n" . company-select-next)
           ("C-p" . company-select-previous)
           ("M-." . company-show-location)
           ))
  :config
  (setq
    slime-company-completion 'fuzzy
    slime-company-after-completion 'slime-company-just-one-space))


(provide 'init-slime)
