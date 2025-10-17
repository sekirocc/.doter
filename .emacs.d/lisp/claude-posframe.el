;;; claude-posframe.el --- Claude terminal interface using posframe and direct process -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude & User
;; Version: 4.0.0
;; Package-Requires: ((emacs "29.1") (posframe "1.0.0") (mistty "20250716.1914"))
;; Keywords: convenience, terminal, claude
;; URL:

;;; Commentary:

;; This package provides a posframe-based terminal interface for Claude using mistty.
;; It creates a floating terminal window that can be toggled on/off.
;; The package includes customizable dimensions, colors, and behavior.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Dependencies

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-frame-center "posframe")
(declare-function posframe-poshandler-frame-top-center "posframe")
(declare-function posframe-poshandler-frame-bottom-center "posframe")
(declare-function posframe-poshandler-frame-left-center "posframe")
(declare-function posframe-poshandler-frame-right-center "posframe")
(declare-function mistty-create "mistty")
(declare-function mistty-exec "mistty")
(declare-function mistty-buffer-p "mistty")
(declare-function mistty-live-buffer-p "mistty")
(declare-function mistty-send-string "mistty")
(declare-function mistty-send-command "mistty")

;;; Customization

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

(defcustom claude-posframe-border-width 1
  "Border width of the posframe."
  :type 'integer
  :group 'claude-posframe)

(defcustom claude-posframe-border-color "green"
  "Border color of the posframe."
  :type 'string
  :group 'claude-posframe)

(defcustom claude-posframe-shell "claude"
  "Shell command to run in the terminal buffer."
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
  "Working directory for the claude process."
  :type '(choice (const :tag "Auto-detect project directory" nil)
           (directory :tag "Custom directory"))
  :group 'claude-posframe)

;;; Variables

(defvar claude-posframe--parent-frame nil
  "Store the parent frame to restore focus after hiding posframe.")

(defvar claude-posframe--posframe-visible nil
  "Track whether the posframe is currently visible.")

(defvar claude-posframe--project-buffer-map (make-hash-table :test 'equal)
  "Hash table mapping project names to their mistty buffer names.")

(defun claude-posframe--ensure-project-map ()
  "Ensure the project buffer map is initialized."
  (unless (and (boundp 'claude-posframe--project-buffer-map)
               claude-posframe--project-buffer-map)
    (setq claude-posframe--project-buffer-map (make-hash-table :test 'equal))))


;;; Utility Functions

(defun claude-posframe--get-project-name ()
  "Get a unique project name for the current directory."
  (let ((project-dir (or claude-posframe-working-directory
                        (claude-posframe--get-project-directory)
                        (expand-file-name "~"))))
    ;; Use the directory name as project identifier
    (file-name-nondirectory (directory-file-name project-dir))))

(defun claude-posframe--get-buffer-name-for-project (project-name)
  "Get the buffer name for a specific project from the map."
  (claude-posframe--ensure-project-map)
  (gethash project-name claude-posframe--project-buffer-map))

(defun claude-posframe--set-buffer-name-for-project (project-name buffer-name)
  "Set the buffer name for a specific project in the map."
  (claude-posframe--ensure-project-map)
  (puthash project-name buffer-name claude-posframe--project-buffer-map))

(defun claude-posframe-list-project-buffers ()
  "List all project buffers and their associated projects."
  (interactive)
  (claude-posframe--ensure-project-map)
  (if (hash-table-empty-p claude-posframe--project-buffer-map)
      (message "No Claude posframe buffers found")
    (let ((projects '()))
      (maphash (lambda (project-name buffer-name)
                 (push (list project-name buffer-name (get-buffer buffer-name)) projects))
               claude-posframe--project-buffer-map)
      (message "Claude posframe buffers:\n%s"
               (mapconcat (lambda (info)
                            (format "  %s -> %s %s"
                                    (car info)
                                    (cadr info)
                                    (if (caddr info) "(live)" "(dead)")))
                          projects "\n")))))

(defun claude-posframe-kill-all-buffers ()
  "Kill all Claude posframe buffers for all projects."
  (interactive)
  (claude-posframe--ensure-project-map)
  (maphash (lambda (project-name buffer-name)
             (let ((buffer (get-buffer buffer-name)))
               (when (and buffer (buffer-live-p buffer))
                 (when (and (mistty-buffer-p buffer) (mistty-live-buffer-p buffer))
                   (let ((proc (get-buffer-process buffer)))
                     (when (and proc (process-live-p proc))
                       (kill-process proc))))
                 (posframe-hide buffer)
                 (kill-buffer buffer))))
           claude-posframe--project-buffer-map)
  (clrhash claude-posframe--project-buffer-map)
  (message "All Claude posframe buffers killed"))

(defun claude-posframe-reset-project-map ()
  "Reset the project buffer map.
This is useful for debugging or when the map gets corrupted."
  (interactive)
  (when claude-posframe--project-buffer-map
    (claude-posframe-kill-all-buffers))
  (setq claude-posframe--project-buffer-map nil)
  (message "Claude posframe project map reset"))

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
   ;; Try project.el if available
   (when (fboundp 'project-current)
     (when-let* ((project (project-current)))
       (if (fboundp 'project-root)
           (project-root project)
         (car (project-roots project)))))
   ;; Try vc as fallback
   (when (fboundp 'vc-root-dir)
     (ignore-errors (vc-root-dir)))
   ;; Fallback to current directory
   default-directory))

(defun claude-posframe--calculate-dimensions ()
  "Calculate posframe dimensions with minimum constraints."
  (let ((width (max claude-posframe-min-width
                   (round (* (frame-width) claude-posframe-width-ratio))))
        (height (max claude-posframe-min-height
                    (round (* (frame-height) claude-posframe-height-ratio)))))
    (list width height)))

(defun claude-posframe--check-dependencies ()
  "Check if required dependencies are available."
  (require 'posframe nil t)
  (require 'mistty nil t)
  ;; Verify that key functions are available
  (or (fboundp 'mistty-buffer-p)
      (user-error "mistty package not properly loaded. Please install mistty package")))

;;; Buffer Management

(defun claude-posframe--get-buffer (&optional switches)
  "Get or create the claude buffer using mistty."
  (unless (claude-posframe--check-dependencies)
    (user-error "Required dependencies (posframe, mistty) are not available"))

  (let* ((project-name (claude-posframe--get-project-name))
         (buffer-name (claude-posframe--get-buffer-name-for-project project-name))
         (buffer (when buffer-name (get-buffer buffer-name)))
         (current-dir (or claude-posframe-working-directory
                        (claude-posframe--get-project-directory)
                        (expand-file-name "~"))))

    ;; Create buffer if it doesn't exist
    (unless buffer
      (setq buffer (condition-case err
                       (let ((default-directory current-dir))
                         (save-excursion
                           (save-window-excursion
                             (if switches
                                 (mistty-create (append (list claude-posframe-shell) switches))
                               (mistty-create claude-posframe-shell)))))
                     (error
                      (message "Failed to start Claude mistty: %s" (error-message-string err))
                      (user-error "Could not start Claude terminal"))))
      ;; Store the actual buffer name created by mistty
      (when buffer
        (claude-posframe--set-buffer-name-for-project project-name (buffer-name buffer))
        ;; Ensure the buffer is not displayed in any window
        (let ((windows (get-buffer-window-list buffer)))
          (dolist (win windows)
            (when (window-live-p win)
              (delete-window win))))))
    buffer))


;;; Core Functions

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
                 (#x00A0 . ? ) ; NO-BREAK SPACE -> regular space
                 ))
      (aset tbl (car pair) (vector (cdr pair))))))

(defun claude-posframe--set-buffer-padding (buffer)
  "Set padding for the buffer in posframe."
  (with-current-buffer buffer
    ;; Apply Unicode fixes for Claude Code compatibility
    (claude-posframe--setup-unicode-fixes)

    (setq-local header-line-format " ")
    (setq-local mode-line-format " ")
    ;; Use face-remap-add-relative for buffer-local face changes
    (face-remap-add-relative 'header-line
      :height 0.8
      :background (face-attribute 'default :background)
      :foreground (face-attribute 'default :foreground)
      :underline nil
      :box nil
      :inherit 'default)
    (face-remap-add-relative 'mode-line-active
      :height 0.8
      :background (face-attribute 'default :background)
      :foreground (face-attribute 'default :foreground)
      :underline nil
      :box nil
      :inherit 'default)
    (face-remap-add-relative 'mode-line-inactive
      :height 0.8
      :background (face-attribute 'default :background)
      :foreground (face-attribute 'default :foreground)
      :underline nil
      :box nil
      :inherit 'default)))

(defun claude-posframe--ensure-scroll ()
  "Ensure the claude posframe scrolls to bottom."
  (let* ((project-name (claude-posframe--get-project-name))
         (buffer-name (claude-posframe--get-buffer-name-for-project project-name))
         (buffer (when buffer-name (get-buffer buffer-name))))
    (when (and buffer
               (buffer-live-p buffer)
               (boundp 'claude-posframe--posframe-visible)
               claude-posframe--posframe-visible)
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
(defun claude-posframe-show (&optional switches)
  "Show the claude posframe."
  (interactive)
  (unless (claude-posframe--check-dependencies)
    (user-error "Required dependencies (posframe) are not available"))
  (let* ((buffer (claude-posframe--get-buffer switches))
         (dimensions (claude-posframe--calculate-dimensions))
         (width (car dimensions))
         (height (cadr dimensions)))

    ;; Store current frame for focus restoration
    (setq claude-posframe--parent-frame (selected-frame))

    ;; Show posframe
    (posframe-show buffer
      :position (point)
      :width width
      :height height
      :window-point (when claude-posframe-auto-scroll
                      (with-current-buffer buffer (point-max)))
      :border-width claude-posframe-border-width
      :border-color claude-posframe-border-color
      :poshandler (claude-posframe--get-position-handler)
      :respect-header-line t
      :respect-mode-line t
      :accept-focus t)

    ;; Apply padding after posframe is fully displayed
    (run-with-timer 0.01 nil
      (lambda ()
        (claude-posframe--set-buffer-padding buffer)))

    (when claude-posframe-auto-scroll
      (claude-posframe--ensure-scroll))

    ;; Mark posframe as visible
    (setq claude-posframe--posframe-visible t)
    (run-hooks 'claude-posframe-show-hook)))

;;;###autoload
(defun claude-posframe-hide ()
  "Hide the claude posframe."
  (interactive)
  (let* ((project-name (claude-posframe--get-project-name))
         (buffer-name (claude-posframe--get-buffer-name-for-project project-name))
         (buffer (when buffer-name (get-buffer buffer-name))))
    (when (and buffer (buffer-live-p buffer))
      (posframe-hide buffer)
      ;; Mark posframe as hidden
      (setq claude-posframe--posframe-visible nil)
      ;; Restore focus to parent frame
      (when (and claude-posframe--parent-frame
                  (frame-live-p claude-posframe--parent-frame))
        (select-frame-set-input-focus claude-posframe--parent-frame))
      (run-hooks 'claude-posframe-hide-hook))))

(defun claude-posframe-visible-p ()
  "Check if the claude posframe is visible."
  (if (boundp 'claude-posframe--posframe-visible)
      claude-posframe--posframe-visible
    nil))

;;;###autoload
(defun claude-posframe-toggle (&optional arg)
  "Toggle the claude posframe visibility.
With prefix argument ARG (C-u), start Claude with bypassed permissions."
  (interactive "P")
  (let ((switches (when (equal arg '(4))
                    '("--permission-mode" "bypassPermissions"))))
    (if (claude-posframe-visible-p)
        (claude-posframe-hide)
      (claude-posframe-show switches))))

;;;###autoload
(defun claude-posframe-kill-buffer ()
  "Kill the claude posframe buffer."
  (interactive)
  (let* ((project-name (claude-posframe--get-project-name))
         (buffer-name (claude-posframe--get-buffer-name-for-project project-name))
         (buffer (when buffer-name (get-buffer buffer-name))))
    (when (and buffer (buffer-live-p buffer))
      (claude-posframe-hide)
      ;; Kill the mistty process if it exists
      (when (and (mistty-buffer-p buffer) (mistty-live-buffer-p buffer))
        (let ((proc (get-buffer-process buffer)))
          (when (and proc (process-live-p proc))
            (kill-process proc))))
      (kill-buffer buffer)
      ;; Remove from project map
      (when claude-posframe--project-buffer-map
        (remhash project-name claude-posframe--project-buffer-map))
      (message "Claude posframe buffer killed"))))

;;;###autoload
(defun claude-posframe-restart ()
  "Restart the claude posframe by killing and recreating the buffer."
  (interactive)
  (claude-posframe-kill-buffer)
  (claude-posframe-show))

;;; Send Commands to Claude

(defun claude-posframe-do-send-command (text)
  "Send TEXT to the claude mistty process."
  (let ((buffer (claude-posframe--get-buffer)))
    (when (and buffer (buffer-live-p buffer) (mistty-buffer-p buffer) (mistty-live-buffer-p buffer))
      (with-current-buffer buffer
        (goto-char (point-max))
        (mistty-send-string text)
        (mistty-send-string "\n")
        (mistty-send-command)))
    (when (claude-posframe-visible-p)
      (claude-posframe-show))))

(defun claude-posframe-send-region (beg end)
  "Send the selected region to claude posframe."
  (interactive "r")
  ;; Validate region bounds
  (when (or (< beg (point-min)) (> end (point-max)) (< end beg))
    (user-error "Invalid region bounds"))
  (let ((file-name (claude-posframe--get-buffer-file-name))
        (selection (buffer-substring-no-properties beg end)))
    (if file-name
        (claude-posframe-do-send-command (format "@%s:%d-%d" file-name (line-number-at-pos beg) (line-number-at-pos end)))
      (claude-posframe-do-send-command selection))))

(defun claude-posframe--get-buffer-file-name ()
  "Get the current buffer's file name."
  (when buffer-file-name
    (file-local-name (file-truename buffer-file-name))))

(defun claude-posframe-send-buffer-file ()
  "Send the current buffer's file to Claude."
  (interactive)
  (let ((filename (claude-posframe--get-buffer-file-name)))
    (if filename
        (claude-posframe-do-send-command (format "@%s " filename))
      (message "Current buffer is not visiting a file"))))

;;; Minor Mode

(defvar claude-posframe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a t") #'claude-posframe-toggle)
    (define-key map (kbd "C-,") #'claude-posframe-toggle)
    (define-key map (kbd "C-c a k") #'claude-posframe-kill-buffer)
    (define-key map (kbd "C-c a r") #'claude-posframe-restart)
    (define-key map (kbd "C-c a b") #'claude-posframe-send-buffer-file)
    (define-key map (kbd "C-c a s") #'claude-posframe-send-region)
    (define-key map (kbd "C-c a l") #'claude-posframe-list-project-buffers)
    (define-key map (kbd "C-c a K") #'claude-posframe-kill-all-buffers)
    (define-key map (kbd "C-c a R") #'claude-posframe-reset-project-map)
    map)
  "Keymap for `claude-posframe-mode'.")

;;;###autoload
(define-minor-mode claude-posframe-mode
  "Minor mode for Claude posframe integration."
  :init-value nil
  :lighter " Claude"
  :keymap claude-posframe-mode-map
  :group 'claude-posframe
  (if claude-posframe-mode
      (message "Claude posframe mode enabled")
    (message "Claude posframe mode disabled")))

;;;###autoload
(define-globalized-minor-mode global-claude-posframe-mode
  claude-posframe-mode
  (lambda () (claude-posframe-mode 1))
  :group 'claude-posframe)

;;;###autoload
(defun claude-posframe-setup-keybindings ()
  "Set up default keybindings for claude-posframe.
This function is deprecated. Use `claude-posframe-mode' instead."
  (interactive)
  (claude-posframe-mode 1)
  (message "Claude posframe keybindings set up (consider using claude-posframe-mode instead)"))

;;; Cleanup

(defun claude-posframe--cleanup ()
  "Clean up claude posframe resources."
  ;; Clean up all buffers tracked in the project map
  (when claude-posframe--project-buffer-map
    (maphash (lambda (project-name buffer-name)
               (let ((buffer (get-buffer buffer-name)))
                 (when (and buffer (buffer-live-p buffer) (mistty-buffer-p buffer))
                   ;; Kill mistty process if it exists
                   (when (mistty-live-buffer-p buffer)
                     (let ((proc (get-buffer-process buffer)))
                       (when (and proc (process-live-p proc))
                         (kill-process proc))))
                   (posframe-hide buffer)
                   (kill-buffer buffer))))
             claude-posframe--project-buffer-map)
    ;; Clear the project map
    (clrhash claude-posframe--project-buffer-map)))

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-posframe--cleanup)

(provide 'claude-posframe)
;;; claude-posframe.el ends here