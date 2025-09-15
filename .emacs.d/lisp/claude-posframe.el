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

(defcustom claude-posframe-border-color "gray"
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
If nil, automatically detect project directory using projectile, project.el, or vc.
Falls back to current directory if no project is detected."
  :type '(choice (const :tag "Auto-detect project directory" nil)
           (directory :tag "Custom directory"))
  :group 'claude-posframe)

(defcustom claude-posframe-fix-unicode t
  "Whether to apply Unicode character fixes to prevent line jitter.
Claude Code uses many special Unicode characters that can cause display issues
if your font doesn't support them. This setting helps mitigate the problem."
  :type 'boolean
  :group 'claude-posframe)

(defconst claude-posframe-buffer-base-name "*claude-posframe*"
  "Base name of the claude posframe buffer.")

(defun claude-posframe--get-buffer-name ()
  "Get project-specific buffer name."
  (let ((project-dir (claude-posframe--get-project-directory)))
    (if project-dir
      (format "*claude-posframe:%s*" (file-name-nondirectory (directory-file-name project-dir)))
      claude-posframe-buffer-base-name)))

(defvar claude-posframe--parent-frame nil
  "Store the parent frame to restore focus after hiding posframe.")

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

(defun claude-posframe--get-project-directory ()
  "Get the project root directory, trying multiple methods."
  (or
    ;; Try projectile if available
    (when (and (bound-and-true-p projectile-mode)
            (fboundp 'projectile-project-root))
      (ignore-errors (projectile-project-root)))
    ;; Try project.el if available (Emacs 25+)
    (when (fboundp 'project-current)
      (when-let ((project (project-current)))
        (if (fboundp 'project-root)
          (project-root project)
          (car (project-roots project)))))
    ;; Try vc as fallback
    (when (fboundp 'vc-root-dir)
      (ignore-errors (vc-root-dir)))
    ;; Fallback to current directory
    default-directory))

(defun claude-posframe--check-claude-running (directory)
  "Check if there's already a claude process running in DIRECTORY.
This checks if the current buffer already has a running claude session."
  ;; Since we can't reliably determine if a claude process belongs to our specific
  ;; directory/session, we'll let the buffer always start fresh with claude.
  ;; The user can manage claude sessions manually if needed.
  nil)

(defun claude-posframe--get-shell-command (directory)
  "Get appropriate shell command to start claude in DIRECTORY."
  ;; Validate inputs for security
  (unless (file-directory-p directory)
    (user-error "Invalid directory: %s" directory))
  (unless (stringp claude-posframe-shell)
    (user-error "Invalid shell command: %s" claude-posframe-shell))
  ;; Create a safe shell command that changes to the directory and runs claude
  claude-posframe-shell)

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
    ;; Store current frame for focus restoration
    (setq claude-posframe--parent-frame (selected-frame))
    (posframe-show buffer
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
  (let ((buffer (get-buffer (claude-posframe--get-buffer-name))))
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
  (let ((buffer (get-buffer (claude-posframe--get-buffer-name))))
    (when (and buffer (buffer-live-p buffer))
      (posframe-hide buffer)
      ;; Restore focus to parent frame
      (when (and claude-posframe--parent-frame
              (frame-live-p claude-posframe--parent-frame))
        (select-frame-set-input-focus claude-posframe--parent-frame))
      (run-hooks 'claude-posframe-hide-hook))))


(defun claude-posframe-visible-p ()
  "Check if the claude posframe is visible."
  (let ((buffer (get-buffer (claude-posframe--get-buffer-name))))
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
  (let ((buffer (get-buffer (claude-posframe--get-buffer-name))))
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
  (claude-posframe-show))

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

  (let* ((buffer-name (claude-posframe--get-buffer-name))
          (buffer (get-buffer buffer-name))
          (current-dir (or claude-posframe-working-directory
                         (claude-posframe--get-project-directory)
                         (expand-file-name "~")))
          (calling-dir default-directory))

    ;; Check if existing buffer has live process
    (when (and buffer
            (buffer-live-p buffer)
            (with-current-buffer buffer
              (and (boundp 'vterm--process)
                vterm--process
                (not (process-live-p vterm--process)))))
      ;; Process is dead, kill the buffer to start fresh
      (kill-buffer buffer)
      (setq buffer nil))

    ;; Create buffer if it doesn't exist or was killed
    (unless buffer
      (setq buffer (generate-new-buffer buffer-name))
      (with-current-buffer buffer
        ;; Set the working directory before initializing vterm
        (let ((vterm-shell (claude-posframe--get-shell-command current-dir))
               (default-directory current-dir))
          (message "vterm-shell: %s" vterm-shell)
          (condition-case err
            (progn
              (vterm-mode)
              ;; Configure vterm for better Unicode support
              (when claude-posframe-fix-unicode
                (setq-local vterm-max-scrollback 5000)
                (setq-local vterm-buffer-name-string nil)
                ;; Replace problematic Unicode characters with ASCII alternatives
                (claude-posframe--setup-unicode-fixes))
              ;; Set up process sentinel for cleanup
              (when (and (boundp 'vterm--process) vterm--process)
                (set-process-sentinel vterm--process #'claude-posframe--process-sentinel)))
            (error
              (kill-buffer buffer)
              (signal (car err) (cdr err)))))))
    buffer))

(defun claude-posframe--setup-unicode-fixes ()
  "Configure Unicode character replacements for Claude Code compatibility.
Replace problematic Unicode characters that cause line jitter with ASCII alternatives.
This fix come from: https://github.com/anthropics/claude-code/issues/247#issuecomment-3058405139"
  (let ((tbl (or buffer-display-table (setq buffer-display-table (make-display-table)))))
    (dolist (pair
              '((#x273B . ?*) ; ✻ TEARDROP-SPOKED ASTERISK
                 (#x273D . ?*) ; ✽ HEAVY TEARDROP-SPOKED ASTERISK
                 (#x2722 . ?+) ; ✢ FOUR TEARDROP-SPOKED ASTERISK
                 (#x2736 . ?+) ; ✶ SIX-POINTED BLACK STAR
                 (#x2733 . ?*) ; ✳ EIGHT SPOKED ASTERISK
                 (#x2699 . ?*) ; ⚙ GEAR (sometimes used by Claude)
                 (#x1F4DD . ?*) ; 📝 MEMO (sometimes used by Claude)
                 (#x1F916 . ?*) ; 🤖 ROBOT FACE (sometimes used by Claude)
                 ))
      (aset tbl (car pair) (vector (cdr pair))))))

(defun claude-posframe--process-sentinel (process event)
  "Handle vterm process termination."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        ;; Hide posframe if this buffer is being displayed
        (posframe-hide buffer)
        ;; Notify user but don't block with interactive prompt in sentinel
        (message "Claude process exited in buffer %s" (buffer-name buffer))
        ;; Auto-kill buffer after a delay to avoid blocking
        (run-with-timer 0.1 nil
          (lambda (buf)
            (when (buffer-live-p buf)
              (kill-buffer buf)))
          buffer)))))

(defun claude-posframe-do-send-command (text)
  "Send TEXT to the claude vterm buffer."
  (let ((buffer (claude-posframe--get-buffer)))
    (with-current-buffer buffer
      (vterm-send-string text)
      ;; Send newline to execute the command
      ;; (vterm-send-return)
      )
    (claude-posframe-show)))

(defun claude-posframe-send-region (beg end)
  "Send the selected region to claude posframe."
  (interactive "r")
  (let ((selection (buffer-substring-no-properties beg end)))
    (claude-posframe-do-send-command (format "%s\n" selection))))


(defun claude-posframe--get-buffer-file-name()
  (when buffer-file-name
    (file-local-name (file-truename buffer-file-name))))

(defun claude-posframe-send-buffer-file()
  (interactive)
  (let ((text (format "@%s " (claude-posframe--get-buffer-file-name))))
    (claude-posframe-do-send-command text)))


;; Cleanup function for process termination
(defun claude-posframe--cleanup ()
  "Clean up claude posframe resources."
  ;; Clean up all project-specific buffers
  (dolist (buffer (buffer-list))
    (when (string-match-p "\*claude-posframe:" (buffer-name buffer))
      (when (buffer-live-p buffer)
        (posframe-hide buffer)
        (kill-buffer buffer))))
  ;; Also clean up the default buffer if it exists
  (let ((buffer (get-buffer claude-posframe-buffer-base-name)))
    (when (and buffer (buffer-live-p buffer))
      (posframe-hide buffer)
      (kill-buffer buffer))))

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-posframe--cleanup)

(provide 'claude-posframe)
;;; claude-posframe.el ends here
