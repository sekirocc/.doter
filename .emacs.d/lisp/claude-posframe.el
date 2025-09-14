;;; claude-posframe.el --- Claude terminal interface using posframe -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1") (posframe "1.0.0") (vterm "0.0.1"))
;; Keywords: convenience, terminal, claude
;; URL:

;;; Commentary:

;; This package provides a posframe-based terminal interface for Claude.
;; It creates a floating terminal window that can be toggled on/off.
;; The package includes customizable dimensions, colors, and behavior.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(declare-function vterm-mode "vterm")

;; Soft dependency handling
(defvar claude-posframe--dependencies-available nil
  "Whether required dependencies are available.")

(defun claude-posframe--check-dependencies ()
  "Check if required dependencies are available."
  (unless claude-posframe--dependencies-available
    (setq claude-posframe--dependencies-available
      (and (require 'posframe nil t)
        (require 'vterm nil t))))
  claude-posframe--dependencies-available)

(defgroup claude-posframe nil
  "Claude posframe configuration."
  :group 'convenience
  :prefix "claude-posframe-")

(defcustom claude-posframe-width-ratio 0.75
  "Width ratio of the posframe relative to the frame width."
  :type 'float
  :group 'claude-posframe)

(defcustom claude-posframe-height-ratio 0.75
  "Height ratio of the posframe relative to the frame height."
  :type 'float
  :group 'claude-posframe)

(defcustom claude-posframe-border-width 2
  "Border width of the posframe."
  :type 'integer
  :group 'claude-posframe)

(defcustom claude-posframe-border-color "green"
  "Border color of the posframe."
  :type 'string
  :group 'claude-posframe)

(defcustom claude-posframe-shell "claude"
  "Shell command to run in the vterm buffer."
  :type 'string
  :group 'claude-posframe)

(defcustom claude-posframe-position 'center
  "Position of the posframe."
  :type '(choice (const :tag "Center" center)
           (const :tag "Top" top)
           (const :tag "Bottom" bottom)
           (const :tag "Left" left)
           (const :tag "Right" right))
  :group 'claude-posframe)

(defcustom claude-posframe-min-width 80
  "Minimum width of the posframe."
  :type 'integer
  :group 'claude-posframe)

(defcustom claude-posframe-min-height 20
  "Minimum height of the posframe."
  :type 'integer
  :group 'claude-posframe)

(defcustom claude-posframe-auto-scroll t
  "Whether to automatically scroll to bottom when showing posframe."
  :type 'boolean
  :group 'claude-posframe)

(defcustom claude-posframe-working-directory nil
  "Working directory for the claude vterm process.
If nil, use the home directory to ensure consistent behavior."
  :type '(choice (const :tag "Home directory" nil)
                 (directory :tag "Custom directory"))
  :group 'claude-posframe)

(defconst claude-posframe-buffer-name "*claude-posframe*"
  "Name of the claude posframe buffer.")

;; Hooks
(defvar claude-posframe-show-hook nil
  "Hook run after showing the claude posframe.")

(defvar claude-posframe-hide-hook nil
  "Hook run after hiding the claude posframe.")

(defvar claude-posframe-kill-hook nil
  "Hook run before killing the claude posframe buffer.")


(defun claude-posframe--get-position-handler ()
  "Get the position handler based on customization."
  (pcase claude-posframe-position
    ('center #'posframe-poshandler-frame-center)
    ('top #'posframe-poshandler-frame-top-center)
    ('bottom #'posframe-poshandler-frame-bottom-center)
    ('left #'posframe-poshandler-frame-left-center)
    ('right #'posframe-poshandler-frame-right-center)
    (_ #'posframe-poshandler-frame-center)))

(defun claude-posframe--calculate-dimensions ()
  "Calculate posframe dimensions with minimum constraints."
  (let ((width (max claude-posframe-min-width
                 (round (* (frame-width) claude-posframe-width-ratio))))
         (height (max claude-posframe-min-height
                   (round (* (frame-height) claude-posframe-height-ratio)))))
    (list width height)))

;;;###autoload
(defun claude-posframe-show ()
  "Show the claude posframe."
  (interactive)
  (unless (claude-posframe--check-dependencies)
    (user-error "Required dependencies (posframe, vterm) are not available"))
  (let* ((buffer (claude-posframe--get-buffer))
          (dimensions (claude-posframe--calculate-dimensions))
          (width (car dimensions))
          (height (cadr dimensions)))
    (posframe-show buffer
      :buffer buffer
      :position (point)
      :width width
      :height height
      :window-point (when claude-posframe-auto-scroll
                      (with-current-buffer buffer (point-max)))
      :border-width claude-posframe-border-width
      :border-color claude-posframe-border-color
      :poshandler (claude-posframe--get-position-handler)
      :accept-focus t)
    (when claude-posframe-auto-scroll
      (claude-posframe--ensure-scroll))
    (run-hooks 'claude-posframe-show-hook)))

(defun claude-posframe--ensure-scroll ()
  "Ensure the claude posframe scrolls to bottom."
  (let ((buffer (get-buffer claude-posframe-buffer-name)))
    (when (and buffer
               (buffer-live-p buffer)
               (claude-posframe-visible-p))
      (let ((windows (get-buffer-window-list buffer nil t)))
        (when windows
          (with-current-buffer buffer
            (goto-char (point-max)))
          (dolist (win windows)
            (when (window-live-p win)
              (with-selected-window win
                (goto-char (point-max))
                (recenter -1)))))))))



;;;###autoload
(defun claude-posframe-hide ()
  "Hide the claude posframe."
  (interactive)
  (let ((buffer (get-buffer claude-posframe-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (posframe-hide buffer)
      (run-hooks 'claude-posframe-hide-hook))))


(defun claude-posframe-visible-p ()
  "Check if the claude posframe is visible."
  (let ((buffer (get-buffer claude-posframe-buffer-name)))
    (and buffer
         (buffer-live-p buffer)
         (let ((window (get-buffer-window buffer t)))
           (and window
                (window-live-p window)
                (let ((frame (window-frame window)))
                  (and frame
                       (frame-live-p frame)
                       (frame-visible-p frame))))))))

;;;###autoload
(defun claude-posframe-toggle ()
  "Toggle the claude posframe visibility."
  (interactive)
  (if (claude-posframe-visible-p)
    (claude-posframe-hide)
    (claude-posframe-show)))

;;;###autoload
(defun claude-posframe-kill-buffer ()
  "Kill the claude posframe buffer."
  (interactive)
  (let ((buffer (get-buffer claude-posframe-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (run-hooks 'claude-posframe-kill-hook)
      (claude-posframe-hide)
      (kill-buffer buffer)
      (message "Claude posframe buffer killed"))))

;;;###autoload
(defun claude-posframe-restart ()
  "Restart the claude posframe by killing and recreating the buffer."
  (interactive)
  (claude-posframe-kill-buffer)
  (claude-posframe-show)

;; Default key bindings
;;;###autoload
(defun claude-posframe-setup-keybindings ()
  "Set up default keybindings for claude-posframe."
  (interactive)
  (global-set-key (kbd "C-c t") #'claude-posframe-toggle)
  (global-set-key (kbd "C-c T") #'claude-posframe-kill-buffer)
  (global-set-key (kbd "C-c r") #'claude-posframe-restart)
  (message "Claude posframe keybindings set up"))

(defun claude-posframe--get-buffer ()
  "Get or create the claude vterm buffer using standard Elisp patterns."
  (unless (claude-posframe--check-dependencies)
    (user-error "Required dependencies (posframe, vterm) are not available"))
  
  (let ((buffer (get-buffer claude-posframe-buffer-name))
        (current-dir (or claude-posframe-working-directory
                        (expand-file-name "~")))
        (calling-dir default-directory))
    ;; Debug: Log buffer and directory state
    (message "Claude posframe check - calling from: %s, target dir: %s, buffer: %s" 
             calling-dir current-dir
             (cond 
              ((not buffer) "not found")
              ((not (buffer-live-p buffer)) "dead")
              ((not (with-current-buffer buffer (boundp 'vterm--process))) "no process var")
              ((not (with-current-buffer buffer vterm--process)) "process nil")
              ((not (with-current-buffer buffer (process-live-p vterm--process))) "process dead")
              (t "alive")))
    
    ;; Check if existing buffer has live process
    (when (and buffer
               (buffer-live-p buffer)
               (with-current-buffer buffer
                 (and (boundp 'vterm--process)
                      vterm--process
                      (not (process-live-p vterm--process)))))
      ;; Process is dead, kill the buffer to start fresh
      (message "Killing dead claude posframe buffer")
      (kill-buffer buffer)
      (setq buffer nil))
    
    ;; Create buffer if it doesn't exist or was killed
    (unless buffer
      (message "Creating new claude posframe buffer in directory: %s" current-dir)
      (setq buffer (generate-new-buffer claude-posframe-buffer-name))
      (with-current-buffer buffer
        ;; Set the working directory before initializing vterm
        (setq default-directory current-dir)
        (let ((vterm-shell claude-posframe-shell))
          (condition-case err
              (progn
                (vterm-mode)
                ;; Set up process sentinel for cleanup
                (when (and (boundp 'vterm--process) vterm--process)
                  (set-process-sentinel vterm--process #'claude-posframe--process-sentinel)))
            (error
             (kill-buffer buffer)
             (signal (car err) (cdr err)))))))
    buffer))

(defun claude-posframe--process-sentinel (process event)
  "Handle vterm process termination."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        ;; Hide posframe if visible
        (when (claude-posframe-visible-p)
          (posframe-hide buffer))
        ;; Optionally kill buffer on process exit
        (when (y-or-n-p "Claude process exited. Kill buffer? ")
          (kill-buffer buffer))))))

(defun claude-posframe-do-send-command (text)
  "Send TEXT to the claude vterm buffer."
  (let ((buffer (claude-posframe--get-buffer)))
    (with-current-buffer buffer
      (vterm-send-string text)
      ;; Send newline to execute the command
      (vterm-send-return))
    (claude-posframe-show)))

(defun claude-posframe-send-region (beg end)
  "Send the selected region to claude posframe."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (claude-posframe-do-send-command text)))


;; Cleanup function for process termination
(defun claude-posframe--cleanup ()
  "Clean up claude posframe resources."
  (let ((buffer (get-buffer claude-posframe-buffer-name)))
    (when (and buffer (buffer-live-p buffer))
      (claude-posframe-hide)
      (kill-buffer buffer))))

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-posframe--cleanup)

(provide 'claude-posframe)
;;; claude-posframe.el ends here
