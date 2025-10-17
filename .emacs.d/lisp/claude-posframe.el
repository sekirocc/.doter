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

;;; Dependencies
(require 'cl-lib)
(require 'mistty)
(require 'posframe)


;;; Hack
(defun mistty-send-key-sequence ()
  "Send all keys to terminal until interrupted.

This function continuously read keys and sends them to the
terminal, just like `mistty-send-key', until it is interrupted
with \\[keyboard-quit] or until it is passed a key or event it
doesn't support, such as a mouse event.."
  (interactive)
  (mistty--require-proc)
  (let ((proc mistty-proc)
         key)
    (while
      (and
        (setq key
          (read-key "Sending all KEYS to terminal... Exit with C-g."
            'inherit-input-method))
        (not (eq key ?\C-t))   ;;;;;;;  add by bellt
        (not (eq key ?\C-g)))
      (pcase key
        (`(xterm-paste ,str)
          (mistty--send-string proc (mistty--maybe-bracketed-str str)))
        (_ (mistty-send-key 1 (make-vector 1 key)))))))


;;; Customization

(defgroup claude-posframe nil
  "Claude posframe configuration."
  :group 'convenience
  :prefix "claude-posframe-")

(defcustom claude-posframe-size-ratio '(0.75 . 0.75)
  "Size ratio of the posframe relative to frame (width . height)."
  :type '(cons (float :tag "Width ratio")
               (float :tag "Height ratio"))
  :group 'claude-posframe)

(defcustom claude-posframe-min-size '(80 . 20)
  "Minimum size of the posframe (width . height)."
  :type '(cons (integer :tag "Minimum width")
               (integer :tag "Minimum height"))
  :group 'claude-posframe)

(defcustom claude-posframe-border '(1 . "green")
  "Border configuration (width . color)."
  :type '(cons (integer :tag "Border width")
               (string :tag "Border color"))
  :group 'claude-posframe)

(defcustom claude-posframe-shell "claude"
  "Shell command to run in the terminal buffer."
  :type 'string
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

(defvar claude-posframe--cached-dimensions nil
  "Cached posframe dimensions to avoid recalculation.")

(defvar claude-posframe--cached-project-dir nil
  "Cached project directory to avoid repeated detection.")


;;; Utility Functions

(defun claude-posframe--get-project-name ()
  "Get a unique project name for the current directory."
  (let ((project-dir (or claude-posframe-working-directory
                       (claude-posframe--get-project-directory)
                       (expand-file-name "~"))))
    ;; Use the directory name as project identifier
    (file-name-nondirectory (directory-file-name project-dir))))


(defun claude-posframe-list-project-buffers ()
  "List all project buffers and their associated projects."
  (interactive)
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
  (maphash (lambda (project-name buffer-name)
             (let* ((buffer (get-buffer buffer-name))
                     (proc (get-buffer-process buffer)))
               (when (and (mistty-live-buffer-p buffer)
                       (process-live-p proc))
                 (kill-process proc)
                 (posframe-hide buffer)
                 (kill-buffer buffer))))
    claude-posframe--project-buffer-map)
  (clrhash claude-posframe--project-buffer-map)
  (message "All Claude posframe buffers killed"))

(defun claude-posframe-reset-project-map ()
  "Reset the project buffer map.
This is useful for debugging or when the map gets corrupted."
  (interactive)
  (claude-posframe-kill-all-buffers)
  (setq claude-posframe--project-buffer-map nil)
  (message "Claude posframe project map reset"))


(defun claude-posframe--get-project-directory (&optional force-recalculate)
  "Get the project root directory, trying multiple methods.
If FORCE-RECALCULATE is non-nil, ignore cached value."
  (or (and (not force-recalculate) claude-posframe--cached-project-dir)
      (setq claude-posframe--cached-project-dir
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
             default-directory))))

(defun claude-posframe--calculate-dimensions (&optional force-recalculate)
  "Calculate posframe dimensions with minimum constraints.
If FORCE-RECALCULATE is non-nil, ignore cached values."
  (or (and (not force-recalculate) claude-posframe--cached-dimensions)
      (setq claude-posframe--cached-dimensions
            (let ((width (max (car claude-posframe-min-size)
                           (round (* (frame-width) (car claude-posframe-size-ratio)))))
                  (height (max (cdr claude-posframe-min-size)
                            (round (* (frame-height) (cdr claude-posframe-size-ratio))))))
              (list width height)))))

;;; Buffer Management

(defun claude-posframe--create-mistty-buffer (switches)
  "Create a new mistty buffer with SWITCHES."
  (let* ((current-dir (or claude-posframe-working-directory
                         (claude-posframe--get-project-directory)
                         (expand-file-name "~")))
         (default-directory current-dir))
    (condition-case err
        (save-excursion
          (save-window-excursion
            (if switches
                (mistty-create (append (list claude-posframe-shell) switches))
              (mistty-create claude-posframe-shell))))
      (error
       (let ((msg (format "Failed to start Claude mistty: %s" (error-message-string err))))
         (display-warning 'claude-posframe msg :error)
         (user-error msg))))))

(defun claude-posframe--setup-new-buffer (project-name buffer)
  "Setup new BUFFER for PROJECT_NAME and return it."
  (puthash project-name (buffer-name buffer) claude-posframe--project-buffer-map)
  ;; Ensure buffer is not displayed in any window
  (dolist (win (get-buffer-window-list buffer))
    (when (window-live-p win)
      (delete-window win)))
  buffer)

(defun claude-posframe--get-buffer (&optional switches)
  "Get or create the claude buffer using mistty."
  (let* ((project-name (claude-posframe--get-project-name))
         (buffer (gethash project-name claude-posframe--project-buffer-map)))
  
    (or (and buffer (buffer-live-p buffer) buffer)
        (when-let* ((new-buffer (claude-posframe--create-mistty-buffer switches)))
          (claude-posframe--setup-new-buffer project-name new-buffer)))))


(defun claude-posframe--get-buffer-file-name ()
  "Get the current buffer's file name."
  (when buffer-file-name
    (file-local-name (file-truename buffer-file-name))))

;;; Core Functions

(defconst claude-posframe--unicode-replacements
  '((#x273B . ?*) ; ✻ TEARDROP-SPOKED ASTERISK
    (#x273D . ?*) ; ✽ HEAVY TEARDROP-SPOKED ASTERISK
    (#x2722 . ?+) ; ✢ FOUR TEARDROP-SPOKED ASTERISK
    (#x2736 . ?+) ; ✶ SIX-POINTED BLACK STAR
    (#x2733 . ?*) ; ✳ EIGHT SPOKED ASTERISK
    (#x2699 . ?*) ; ⚙ GEAR (sometimes used by Claude)
    (#x1F4DD . ?*) ; 📝 MEMO (sometimes used by Claude)
    (#x1F916 . ?*) ; 🤖 ROBOT FACE (sometimes used by Claude)
    (#x00A0 . ? )) ; NO-BREAK SPACE -> regular space
  "Unicode character replacements for Claude Code compatibility.
See: https://github.com/anthropics/claude-code/issues/247#issuecomment-3058405139")

(defun claude-posframe--setup-unicode-fixes ()
  "Configure Unicode character replacements for Claude Code compatibility."
  (let ((tbl (or buffer-display-table (setq buffer-display-table (make-display-table)))))
    (dolist (pair claude-posframe--unicode-replacements)
      (aset tbl (car pair) (vector (cdr pair))))))

(defun claude-posframe--configure-buffer (buffer)
  "Configure buffer for posframe display."
  (with-current-buffer buffer
    ;; Apply Unicode fixes for Claude Code compatibility
    (claude-posframe--setup-unicode-fixes)

    ;; Hide header and mode lines
    (setq-local header-line-format " ")
    (setq-local mode-line-format " ")

    ;; Apply face remapping for clean display
    (dolist (face '(header-line mode-line-active mode-line-inactive))
      (face-remap-add-relative face
        :height 0.8
        :background (face-attribute 'default :background)
        :foreground (face-attribute 'default :foreground)
        :underline nil
        :box nil
        :inherit 'default)))

  ;; Configure terminal input and scrolling
  (let ((windows (get-buffer-window-list buffer nil t)))
    (when windows
      (dolist (win windows)
        (when (window-live-p win)
          (with-selected-window win
            (with-current-buffer buffer
              ;; Enable terminal input
              (mistty-send-key-sequence)
              ;; Scroll to bottom
              (goto-char (point-max))
              (recenter -1))))))))


(defun claude-posframe-show (&optional switches)
  "Show the claude posframe."
  (interactive)
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
      :window-point (with-current-buffer buffer (point-max))
      :border-width (car claude-posframe-border)
      :border-color (cdr claude-posframe-border)
      :poshandler #'posframe-poshandler-frame-center
      :respect-header-line t
      :respect-mode-line t
      :accept-focus t)

    ;; Configure buffer after posframe is fully displayed
    (run-with-timer 0.01 nil
      (lambda ()
        (claude-posframe--configure-buffer buffer)))

    ;; Mark posframe as visible
    (setq claude-posframe--posframe-visible t)
    (run-hooks 'claude-posframe-show-hook)))

;;;###autoload
(defun claude-posframe-hide ()
  "Hide the claude posframe."
  (interactive)
  (let* ((project-name (claude-posframe--get-project-name))
         (buffer-name (gethash project-name claude-posframe--project-buffer-map))
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
  claude-posframe--posframe-visible)

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
         (buffer-name (gethash project-name claude-posframe--project-buffer-map))
         (buffer (when buffer-name (get-buffer buffer-name)))
         (proc (when buffer (get-buffer-process buffer))))
    (when (mistty-live-buffer-p buffer)
      (claude-posframe-hide)
      ;; Kill the mistty process if it exists
      (when (process-live-p proc)
        (kill-process proc))
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
    (when (mistty-live-buffer-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (mistty-send-string text)
        (mistty-send-string "\n")))
    (unless (claude-posframe-visible-p)
      (claude-posframe-show))))

(defun claude-posframe-send-region (beg end)
  "Send the selected region to claude posframe."
  (interactive "r")
  (let ((file-name (claude-posframe--get-buffer-file-name))
         (selection (buffer-substring-no-properties beg end)))
    (if file-name
      (claude-posframe-do-send-command (format "@%s:%d-%d" file-name (line-number-at-pos beg) (line-number-at-pos end)))
      (claude-posframe-do-send-command selection))))

(defun claude-posframe-send-buffer-file ()
  "Send the current buffer's file to Claude."
  (interactive)
  (let ((filename (claude-posframe--get-buffer-file-name)))
    (if filename
        (claude-posframe-do-send-command (format "@%s " filename))
      (message "Current buffer is not visiting a file"))))

(defun claude-posframe-send-current-line ()
  "Send the current line to Claude."
  (interactive)
  (let ((line-content (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
    (claude-posframe-do-send-command line-content)))

(defun claude-posframe-send-symbol-at-point ()
  "Send the symbol at point to Claude."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        (claude-posframe-do-send-command (format "%s" symbol))
      (message "No symbol at point"))))

(defun claude-posframe-send-project-context ()
  "Send project context (current file + git status) to Claude."
  (interactive)
  (let ((filename (claude-posframe--get-buffer-file-name))
        (commands '()))
    (when filename
      (push (format "@%s" filename) commands))
      ;; Try to get git status if available
      (when (and (fboundp 'magit-status)
                 (fboundp 'magit-git-repo-p))
        (with-temp-buffer
          (when (magit-git-repo-p default-directory)
            (call-process "git" nil t nil "status" "--porcelain")
            (let ((git-status (buffer-string)))
              (unless (string-empty-p git-status)
                (push (format "\n\nGit status:\n%s" git-status) commands))))))
      (when commands
        (claude-posframe-do-send-command (string-join "\n" (reverse commands))))))

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
    ;; New convenience functions
    (define-key map (kbd "C-c a L") #'claude-posframe-send-current-line)
    (define-key map (kbd "C-c a p") #'claude-posframe-send-symbol-at-point)
    (define-key map (kbd "C-c a c") #'claude-posframe-send-project-context)
    map)
  "Keymap for `claude-posframe-mode'.")

(defun claude-posframe--clear-cache ()
  "Clear cached values."
  (setq claude-posframe--cached-dimensions nil
        claude-posframe--cached-project-dir nil))

;;;###autoload
(define-minor-mode claude-posframe-mode
  "Minor mode for Claude posframe integration."
  :init-value nil
  :lighter " Claude"
  :keymap claude-posframe-mode-map
  :group 'claude-posframe
  (if claude-posframe-mode
      (progn
        (add-hook 'window-size-change-functions #'claude-posframe--clear-cache)
        (message "Claude posframe mode enabled"))
    (remove-hook 'window-size-change-functions #'claude-posframe--clear-cache)
    (message "Claude posframe mode disabled")))

;;;###autoload
(define-globalized-minor-mode global-claude-posframe-mode
  claude-posframe-mode
  (lambda () (claude-posframe-mode 1))
  :group 'claude-posframe)

;;; Cleanup

(defun claude-posframe--cleanup ()
  "Clean up claude posframe resources."
  ;; Clean up all buffers tracked in the project map
  (maphash (lambda (project-name buffer-name)
             (let* ((buffer (get-buffer buffer-name))
                    (proc (get-buffer-process buffer)))
               (when (mistty-live-buffer-p buffer)
                 ;; Kill mistty process if it exists
                 (when (process-live-p proc)
                   (kill-process proc))
                 (posframe-hide buffer)
                 (kill-buffer buffer))))
    claude-posframe--project-buffer-map)
  ;; Clear the project map
  (when claude-posframe--project-buffer-map
    (clrhash claude-posframe--project-buffer-map)))

;; Register cleanup on Emacs exit
(add-hook 'kill-emacs-hook #'claude-posframe--cleanup)

(provide 'claude-posframe)
;;; claude-posframe.el ends here
