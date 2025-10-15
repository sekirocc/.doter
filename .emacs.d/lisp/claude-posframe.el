;;; claude-posframe.el --- Claude terminal interface using posframe -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude & User
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1") (posframe "1.0.0") (vterm "0.0.1"))
;; Keywords: convenience, terminal, claude
;; URL:

;;; Commentary:

;; This package provides a posframe-based terminal interface for Claude.
;; It creates a floating terminal window that can be toggled on/off.
;; The package includes customizable dimensions, colors, and behavior.
;;
;; Usage:
;;   (require 'claude-posframe)
;;   (claude-posframe-mode 1)  ; Enable in current buffer
;;   ;; or
;;   (global-claude-posframe-mode 1)  ; Enable globally
;;
;; Key bindings (when claude-posframe-mode is active):
;;   C-c a t - Toggle Claude posframe
;;   C-,     - Toggle Claude posframe (quick alternative)
;;   C-c a k - Kill Claude posframe buffer
;;   C-c a r - Restart Claude posframe
;;   C-c a b - Send current buffer file to Claude
;;   C-c a s - Send selected region to Claude

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Dependencies and Declarations

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

(defcustom claude-posframe-buffer-multiline-output t
  "Whether to buffer vterm output to prevent flickering on multi-line input.

When non-nil, vterm output that appears to be redrawing multi-line
input boxes will be buffered briefly and processed in a single
batch. This prevents the flickering that can occur when Claude redraws
its input box as it expands to multiple lines."
  :type 'boolean
  :group 'claude-posframe)

(defcustom claude-posframe-multiline-delay 0.01
  "Delay in seconds before processing buffered vterm output.

This controls how long vterm waits to collect output before processing
it when `claude-posframe-buffer-multiline-output' is enabled.
The delay should be long enough to collect bursts of updates but short
enough to not be noticeable to the user.

The default value of 0.01 seconds (10ms) provides a good balance
between reducing flickering and maintaining responsiveness."
  :type 'number
  :group 'claude-posframe)

(defconst claude-posframe-buffer-base-name "*claude-posframe*"
  "Base name of the claude posframe buffer.")

;;; Variables and State

(defvar claude-posframe--parent-frame nil
  "Store the parent frame to restore focus after hiding posframe.")

(defvar-local claude-posframe--multiline-buffer nil
  "Buffer for accumulating multi-line vterm output.")

(defvar-local claude-posframe--multiline-buffer-timer nil
  "Timer for processing buffered multi-line vterm output.")

;;; Utility Functions

(defun claude-posframe--get-buffer-name ()
  "Get project-specific buffer name."
  (let ((project-dir (claude-posframe--get-project-directory)))
    (if project-dir
      (format "*claude-posframe:%s*" (file-name-nondirectory (directory-file-name project-dir)))
      claude-posframe-buffer-base-name)))

;;; Hooks
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

;;; Core Functions

(defun claude-posframe--calculate-dimensions ()
  "Calculate posframe dimensions with minimum constraints."
  (let ((width (max claude-posframe-min-width
                 (round (* (frame-width) claude-posframe-width-ratio))))
         (height (max claude-posframe-min-height
                   (round (* (frame-height) claude-posframe-height-ratio)))))
    (list width height)))

(defun claude-posframe--set-buffer-padding(buffer)
  (with-current-buffer buffer
    (setq-local header-line-format " ")
    (setq-local mode-line-format " ")
    ;; Set buffer-local fringe for current window
    (when (get-buffer-window (current-buffer))
      (let* ((fringe-width 6))  ; 固定6像素的 fringe
        (set-window-fringes (get-buffer-window (current-buffer))
          fringe-width fringe-width)
        (message "Claude posframe padding applied: fringe=%d" fringe-width)))
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

(defun claude-posframe-debug-padding ()
  "Debug function to check padding settings."
  (interactive)
  (let ((buffer (get-buffer (claude-posframe--get-buffer-name))))
    (if buffer
      (with-current-buffer buffer
        (message "Header line: %s | Mode line: %s | Fringes: %s | Face remaps: %d"
          header-line-format
          mode-line-format
          (window-fringes (get-buffer-window buffer))
          (length face-remapping-alist)))
      (message "Claude posframe buffer not found"))))

;;;###autoload
(defun claude-posframe-show (&optional switches)
  "Show the claude posframe."
  (interactive)
  (unless (claude-posframe--check-dependencies)
    (user-error "Required dependencies (posframe, vterm) are not available"))
  (let* ((buffer (claude-posframe--get-buffer switches))
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
      :respect-header-line t
      :respect-mode-line t
      :accept-focus t)
    ;; Apply padding after posframe is fully displayed
    (run-with-timer 0.01 nil
      (lambda ()
        (claude-posframe--set-buffer-padding buffer)))
    (when claude-posframe-auto-scroll
      (claude-posframe--ensure-scroll))
    (run-hooks 'claude-posframe-show-hook)))

;;; Display and Interaction Functions

(defun claude-posframe--ensure-scroll ()
  "Ensure the claude posframe scrolls to bottom.
If vterm is in copy mode, exit to insert mode."
  (let ((buffer (get-buffer (claude-posframe--get-buffer-name))))
    (when (and buffer
            (buffer-live-p buffer)
            (claude-posframe-visible-p))
      (let ((windows (get-buffer-window-list buffer nil t)))
        (when windows
          (with-current-buffer buffer
            ;; Exit vterm copy mode if active
            (when (and (bound-and-true-p vterm-copy-mode)
                    vterm-copy-mode)
              (vterm-copy-mode -1))
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
(defun claude-posframe-toggle (&optional arg)
  "Toggle the claude posframe visibility.
With prefix argument ARG (C-u), start Claude with bypassed permissions."
  (interactive "P")
  (let ((switches (when (equal arg '(4))
                    '("--permission-mode bypassPermissions"))))
    (if (claude-posframe-visible-p)
      (claude-posframe-hide)
      (claude-posframe-show switches))))

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

;;; Minor Mode Definition
(defvar claude-posframe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a t") #'claude-posframe-toggle)
    (define-key map (kbd "C-,") #'claude-posframe-toggle)
    (define-key map (kbd "C-c a k") #'claude-posframe-kill-buffer)
    (define-key map (kbd "C-c a r") #'claude-posframe-restart)
    (define-key map (kbd "C-c a b") #'claude-posframe-send-buffer-file)
    (define-key map (kbd "C-c a s") #'claude-posframe-send-region)
    map)
  "Keymap for `claude-posframe-mode'.")

;;;###autoload
(define-minor-mode claude-posframe-mode
  "Minor mode for Claude posframe integration.
Provides convenient keybindings for interacting with Claude in a posframe."
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

;; Legacy function for backward compatibility
;;;###autoload
(defun claude-posframe-setup-keybindings ()
  "Set up default keybindings for claude-posframe.
This function is deprecated. Use `claude-posframe-mode' instead."
  (interactive)
  (claude-posframe-mode 1)
  (message "Claude posframe keybindings set up (consider using claude-posframe-mode instead)"))

;;; Buffer Management

(defun claude-posframe--get-buffer (&optional switches)
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
        (let ((vterm-shell (if switches
                             (concat claude-posframe-shell " " (mapconcat #'identity switches " "))
                             claude-posframe-shell))
               (default-directory current-dir))
          (message "vterm-shell: %s" vterm-shell)
          (condition-case err
            (progn
              (vterm-mode)
              ;; Top padding will be set up via vterm-mode-hook
              ;; Configure vterm for better Unicode support
              (when claude-posframe-fix-unicode
                (setq-local vterm-max-scrollback 5000)
                (setq-local vterm-buffer-name-string nil)
                ;; Replace problematic Unicode characters with ASCII alternatives
                (claude-posframe--setup-unicode-fixes))
              ;; Set up Claude Code specific key bindings
              (claude-posframe--setup-vterm-keybindings)
              ;; Set up multi-line buffering to prevent flickering
              (advice-add 'vterm--filter :around #'claude-posframe--multiline-buffer-filter)
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
                 (#x00A0 . ? ) ; NO-BREAK SPACE -> regular space
                 ))
      (aset tbl (car pair) (vector (cdr pair))))))

;;; vterm Integration

(defun claude-posframe--vterm-send-return ()
  "Send return key to vterm."
  (interactive)
  (vterm-send-key ""))

(defun claude-posframe--vterm-send-alt-return ()
  "Send Alt+Return to vterm."
  (interactive)
  (vterm-send-key "" nil t))

(defun claude-posframe--setup-vterm-keybindings ()
  "Set up Claude Code specific key bindings in vterm buffer."
  (let ((map (current-local-map)))
    (when map
      (define-key map (kbd "<return>") #'claude-posframe--vterm-send-return)
      (define-key map (kbd "<M-return>") #'claude-posframe--vterm-send-alt-return))))

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

;; copied from https://github.com/stevemolitor/claude-code.el/blob/main/claude-code.el#L1534  func claude-code--vterm-multiline-buffer-filter
(defun claude-posframe--multiline-buffer-filter (orig-fun process input)
  "Buffer vterm output when it appears to be redrawing multi-line input.
This prevents flickering when Claude redraws its input box as it expands
to multiple lines. We detect this by looking for escape sequences that
indicate cursor positioning and line clearing operations.

ORIG-FUN is the original vterm--filter function.
PROCESS is the vterm process.
INPUT is the terminal output string."
  (if (or (not claude-posframe-buffer-multiline-output)
        (not (string-match-p "\\*claude-posframe" (buffer-name (process-buffer process)))))
    ;; Feature disabled or not a Claude posframe buffer, pass through normally
    (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      ;; Check if this looks like multi-line input box redraw
      ;; Common patterns when redrawing multi-line input:
      ;; - ESC[K (clear to end of line)
      ;; - ESC[<n>;<m>H (cursor positioning)
      ;; - ESC[<n>A/B/C/D (cursor movement)
      ;; - Multiple of these in sequence
      (let ((has-clear-line (string-match-p "\\033\\[K" input))
             (has-cursor-pos (string-match-p "\\033\\[[0-9]+;[0-9]+H" input))
             (has-cursor-move (string-match-p "\\033\\[[0-9]*[ABCD]" input))
             (escape-count (cl-count ?\033 input)))

        ;; If we see multiple escape sequences that look like redrawing,
        ;; or we're already buffering, add to buffer
        (if (or (and (>= escape-count 3)
                  (or has-clear-line has-cursor-pos has-cursor-move))
              claude-posframe--multiline-buffer)
          (progn
            ;; Add to buffer
            (setq claude-posframe--multiline-buffer
              (concat claude-posframe--multiline-buffer input))
            ;; Cancel existing timer
            (when claude-posframe--multiline-buffer-timer
              (cancel-timer claude-posframe--multiline-buffer-timer))
            ;; Set timer with configurable delay
            (setq claude-posframe--multiline-buffer-timer
              (run-at-time claude-posframe-multiline-delay nil
                (lambda (buf)
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (when claude-posframe--multiline-buffer
                        (let ((inhibit-redisplay t)
                               (data claude-posframe--multiline-buffer))
                          ;; Clear buffer first to prevent recursion
                          (setq claude-posframe--multiline-buffer nil
                            claude-posframe--multiline-buffer-timer nil)
                          ;; Process all buffered data at once
                          (funcall orig-fun
                            (get-buffer-process buf)
                            data))))))
                (current-buffer))))
          ;; Not multi-line redraw, process normally
          (funcall orig-fun process input))))))

;;; Send Commands to Claude

(defun claude-posframe-do-send-command (text)
  "Send TEXT to the claude vterm buffer."
  (let ((buffer (claude-posframe--get-buffer)))
    (with-current-buffer buffer
      (vterm-send-string text))
    (claude-posframe-show)))

(defun claude-posframe-send-region (beg end)
  "Send the selected region to claude posframe."
  (interactive "r")
  (let ((file-name (claude-posframe--get-buffer-file-name))
         (selection (buffer-substring-no-properties beg end)))
    (if file-name
      (claude-posframe-do-send-command (format "@%s:%d-%d\n" file-name (line-number-at-pos beg) (line-number-at-pos end)))
      (claude-posframe-do-send-command (format "%s\n" selection)))
    ))


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

;;; Cleanup and Initialization
(defun claude-posframe--cleanup ()
  "Clean up claude posframe resources."
  ;; Remove vterm filter advice
  (advice-remove 'vterm--filter #'claude-posframe--multiline-buffer-filter)
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

;; Auto-enable mode for programming modes (optional)
;;;###autoload
(defun claude-posframe-auto-enable ()
  "Automatically enable claude-posframe-mode for programming modes."
  (when (derived-mode-p 'prog-mode 'text-mode)
    (claude-posframe-mode 1)))

;; Uncomment the following line to auto-enable for programming modes:
;; (add-hook 'after-change-major-mode-hook #'claude-posframe-auto-enable)

(provide 'claude-posframe)
;;; claude-posframe.el ends here
