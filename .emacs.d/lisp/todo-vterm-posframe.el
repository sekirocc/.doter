(require 'posframe)
(require 'vterm)

(defvar my/vterm-posframe-buffer nil
  "存储 vterm buffer。")

(defun my/vterm-posframe--get-buffer ()
  "获取或创建 vterm buffer。"
  (unless (and my/vterm-posframe-buffer
            (buffer-live-p my/vterm-posframe-buffer))
    (let ((buf (generate-new-buffer "*vterm-posframe*")))
      (with-current-buffer buf
        (vterm-mode))
      (setq my/vterm-posframe-buffer buf)))
  my/vterm-posframe-buffer)




(defun my/vterm-posframe ()
  "Toggle vterm posframe with cursor restoration."
  (interactive)
  (let ((buffer (my/vterm-posframe--get-buffer))
         (main-frame (selected-frame)))
    (posframe-show buffer
      :buffer buffer
      :position (point)
      :width 150
      :height 50
      :window-point (with-current-buffer buffer (point-max))
      :border-width ivy-posframe-border-width
      :border-color "green"
      ;; :hidehandler #'posframe-hidehandler-when-buffer-switch
      :poshandler #'posframe-poshandler-frame-center
      :accept-focus t)
    (my/vterm-posframe--ensure-scroll)
    ))

(defun my/vterm-posframe--ensure-scroll ()
  "确保 vterm posframe 滚动到底部。"
  (let ((buffer my/vterm-posframe-buffer))
    (when (and (buffer-live-p buffer)
            (posframe-visible-p))
      (with-current-buffer buffer
        (goto-char (point-max)))
      (dolist (win (get-buffer-window-list buffer nil t))
        (with-selected-window win
          (goto-char (point-max))
          (recenter -1))))))


(defun my/vterm-posframe--scroll-to-bottom (buffer)
  "仅滚动 posframe 中的 window 到底部。"
  (when-let* ((frame (posframe--frame buffer))
               (win (frame-root-window frame)))
    (with-selected-window win
      (with-current-buffer buffer
        (goto-char (point-max)))
      (recenter -1)))) ; 滚动到底部



(defun my/vterm-posframe-hide ()
  (interactive)
  (when my/vterm-posframe-buffer
    (posframe-hide my/vterm-posframe-buffer)
    (ns-next-frame)
    ))


(defun get-frame-by-buffer-name (buffer-name)
  "Returns the frame displaying the buffer named BUFFER-NAME, or nil if not found."
  (let ((window (get-buffer-window buffer-name t))) ; 't' means search all frames
    (when window
      (window-frame window))))



(defun my/vterm-posframe-visible-p ()
  "使用 frame 的 visible-p 属性判断。"
  (and my/vterm-posframe-buffer
    (frame-live-p (get-frame-by-buffer-name (buffer-name my/vterm-posframe-buffer)))
    (frame-visible-p (get-frame-by-buffer-name (buffer-name my/vterm-posframe-buffer)))))

(defun my/vterm-posframe-toggle ()
  "切换 vterm posframe 的显示状态。"
  (interactive)
  (if (my/vterm-posframe-visible-p)
    (my/vterm-posframe-hide)
    (my/vterm-posframe)))

;; 绑定快捷键
(global-set-key (kbd "C-,") 'my/vterm-posframe-toggle)
(global-set-key (kbd "C-t") 'my/vterm-posframe-toggle)
