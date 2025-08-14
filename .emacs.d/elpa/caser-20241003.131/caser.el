;;; caser.el --- Change text casing from camelCase to UpperCamelCase to dash-case to snake_case -*- lexical-binding: t; -*-

;; Package-Version: 20241003.131
;; Package-Revision: 6ed8fe13ff6a
;; Homepage: https://hg.sr.ht/~zck/caser.el

;; Package-Requires: ((emacs "29.1"))

;; Copyright 2023 Zachary Kanfer <zkanfer@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package changes text from camelCase to dash-case to snake_case.
;; We recommend binding the following keys:

;; (bind-key "M-C" #'caser-camelcase-dwim)
;; (bind-key "C-M-C" #'caser-upper-camelcase-dwim)
;; (bind-key "M-S" #'caser-snakecase-dwim)
;; (bind-key "M-D" #'caser-dashcase-dwim)

;; The code is at https://hg.sr.ht/~zck/caser.el
;; CI is at https://builds.sr.ht/~zck/caser.el

;;; Code:

(require 'keymap)

;;;###autoload
(defun caser-camelcase-dwim (arg)
  "Camelcase words in the region, if active; if not, camelcase word at point.

This converts it from dash-case or snake_case to camelCase.

If the region is active, this function calls `caser-camelcase-region'.
Otherwise, it calls `caser-camelcase-word', with prefix argument passed to it
to camelcase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (caser-camelcase-region (region-beginning) (region-end))
    (caser-camelcase-word arg)))

(defvar caser-camelcase-repeat-map (define-keymap "c" #'caser-camelcase-dwim))
(put #'caser-camelcase-dwim 'repeat-map 'caser-camelcase-repeat-map)

(defun caser-camelcase-region (region-beginning region-end)
  "Camelcase the region between REGION-BEGINNING and REGION-END.

  This converts it from snake_case or dash-case to camelCase.

  After returning, point is at the end of the region."
  (interactive "*r")

  (let ((end-marker (make-marker)))
    (move-marker end-marker region-end)

    (goto-char region-beginning)

    (while (re-search-forward (rx (or "-" "_"))
                              (marker-position end-marker)
                              t)
      (replace-match "")
      (unless (eolp)
        (caser-upcase-char)))

    (goto-char (marker-position end-marker))))

(defun caser-upper-camelcase-dwim (arg)
  "UpperCamelcase words in the region, if active; if not, word at point.

This converts it from dash-case, snake_case, or camelCase to UpperCamelCase.

If the region is active, this function calls `caser-upper-camelcase-region'.
Otherwise, it calls `caser-upper-camelcase-word', with prefix argument
passed to it to UpperCamelcase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (caser-upper-camelcase-region (region-beginning) (region-end))
    (caser-upper-camelcase-word arg)))

(defvar caser-upper-camelcase-repeat-map (define-keymap "c" #'caser-upper-camelcase-dwim))
(put #'caser-upper-camelcase-dwim 'repeat-map 'caser-upper-camelcase-repeat-map)

(defun caser-upper-camelcase-region (region-beginning region-end)
  "Upper camelcase the region between REGION-BEGINNING and REGION-END.

  This converts it from snake_case, dash-case, or camelCase to UpperCamelCase.

  After returning, point is at the end of the region."
  (let ((end-marker (make-marker)))
    (move-marker end-marker region-end)
    (goto-char region-beginning)
    (when (re-search-forward (rx word)
                             region-end
                             t)
      (upcase-char -1))
    (while (re-search-forward (rx (or "-" "_" space))
                              (marker-position end-marker)
                              t)
      (backward-char 1)
      (if (looking-at (rx space))
          (forward-char 1)
        (replace-match ""))
      (unless (or (eolp)
                  (>= (point)
                      (marker-position end-marker)))
        (caser-upcase-char)))
    (goto-char (marker-position end-marker))))

(defun caser-upcase-char ()
  "Upcase the char at point."
  (upcase-region (point)
                 (1+ (point))))

(defun caser--move-to-beginning-of-word ()
  "Move to the beginning of the word point is in."
  (while (looking-back (rx (one-or-more (or word
                                            "-"
                                            "_")))
                       (point-min)
                       t)
    (goto-char (match-beginning 0))))

(defun caser--move-to-end-of-word ()
  "Move to the beginning of the word point is in."
  (when (looking-at (rx (one-or-more (or word
                                         "-"
                                         "_"))))
    (goto-char (match-end 0))))

(defun caser--forward-word (number-of-words)
  "Move forward NUMBER-OF-WORDS words, defaulting to 1.

This differs from `forward-word' in that the only separators it
cares about are whitespace."
  (if (> number-of-words 0)
      (progn (looking-at (rx-to-string
                          `(repeat 1 ,number-of-words
                                   (seq (zero-or-more space)
                                        (one-or-more (not space))))))
             (goto-char (match-end 0)))
    (when (looking-back (rx-to-string
                         `(repeat ,(- number-of-words)
                                  (seq (or space string-start)
                                       (one-or-more (not space))
                                       (zero-or-more space))))
                        (point-min)
                        t)
      (goto-char (match-beginning 0))
      (when (looking-at (rx (one-or-more space)))
        (goto-char (match-end 0))))))

(defun caser--word-helper (words case-function)
  "Call CASE-FUNCTION on WORDS words."
  (cond ((and (> words 0)
              (looking-at (rx (or word
                                  "-"
                                  "_"))))
         (caser--move-to-beginning-of-word))
        ((looking-back (rx (or word
                               "-"
                               "_"))
                       (1- (point)))
         (caser--move-to-end-of-word)))
  (let* ((initial-bound (point))
         (other-bound (progn (caser--forward-word words) (point)))
         (starting-point (min initial-bound other-bound))
         (ending-point (max initial-bound other-bound))
         (marker (make-marker)))
    (move-marker marker other-bound)
    (funcall case-function starting-point ending-point)
    (goto-char (marker-position marker))))

(defun caser--region-helper-for-separator-case (region-beginning region-end separator-to-add separators-to-remove upcase-downcase-region-function)
  "Act on the region between REGION-BEGINNING and REGION-END.

  This removes SEPARATORS-TO-REMOVE, and replaces them with SEPARATOR-TO-ADD.

At the end, this calls UPCASE-DOWNCASE-REGION-FUNCTION on the region."
  (goto-char region-beginning)
  (let ((end-marker (make-marker))
        (case-fold-search nil))
    (move-marker end-marker region-end)

    ;;We want insertions before the marker, not after.
    ;;This prevents the marker being not at the end of a word we've already snakecased,
    ;;if the word ends with capital letters. (e.g., myIP)
    (set-marker-insertion-type end-marker t)

    (goto-char region-beginning)

    (while (re-search-forward (rx-to-string `(or (seq (group lower) ;;camelCase is groups 1 & 2
                                                      (group upper))
                                                 (seq (group (one-or-more (or ,@separators-to-remove))) ;;dash-case is groups 3 & 4
                                                      (group word))))
                              (marker-position end-marker)
                              t)
      (let ((matched-camelcase (match-string 1)))
        (if matched-camelcase
            (progn (replace-match (concat separator-to-add
                                          (downcase (match-string 2)))
                                  t nil nil 2)
                   (when (looking-at (rx upper))
                     ;;there is more than one uppercase letter in a row, so we're looking at an acronym.
                     (while (looking-at (rx upper))
                       (downcase-region (point)
                                        (1+ (point)))
                       (forward-char 1))
                     (unless (= (point)
                                (marker-position end-marker))
                       (backward-char 1)
                       (insert separator-to-add))))
          ;;dashcase
          (replace-match separator-to-add nil nil nil 3))))
    (funcall upcase-downcase-region-function region-beginning (marker-position end-marker))
    (goto-char (marker-position end-marker))))

(defun caser-camelcase-word (words)
  "Camelcase WORDS words forward from point."
  (caser--word-helper words #'caser-camelcase-region))

(defun caser-upper-camelcase-word (words)
  "UpperCamelcase WORDS words forward from point."
  (caser--word-helper words #'caser-upper-camelcase-region))

;;;###autoload
(defun caser-snakecase-dwim (arg)
  "Snakecase words in the region, if active; if not, snakecase word at point.

This converts it from camelCase or dash-case to snake_case.

If the region is active, this function calls `caser-snakecase-region'.
Otherwise, it calls `caser-snakecase-word', with prefix argument passed to it
to snakecase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (caser-snakecase-region (region-beginning) (region-end))
    (caser-snakecase-word arg)))

(defvar caser-snakecase-repeat-map (define-keymap "s" #'caser-snakecase-dwim))
(put #'caser-snakecase-dwim 'repeat-map 'caser-snakecase-repeat-map)

(defun caser-snakecase-region (region-beginning region-end)
  "Snakecase the region between REGION-BEGINNING and REGION-END.

  This converts it from camelCase or dash-case to snake_case."
  (interactive "*r")
  (caser--region-helper-for-separator-case region-beginning
                                           region-end
                                           "_"
                                           '("-")
                                           #'downcase-region))

(defun caser-snakecase-word (&optional words)
  "Snakecase WORDS words forward from point."
  (caser--word-helper words #'caser-snakecase-region))

(defun caser-dashcase-word (&optional words)
  "Dashcase WORDS words forward from point."
  (caser--word-helper words #'caser-dashcase-region))

(defun caser-dashcase-region (region-beginning region-end)
  "Dashcase the region between REGION-BEGINNING and REGION-END.

  This converts it from camelCase or snake_case to dash-case."
  (interactive "*r")
  (caser--region-helper-for-separator-case region-beginning
                                           region-end
                                           "-"
                                           '("_")
                                           #'downcase-region))

;;;###autoload
(defun caser-dashcase-dwim (arg)
  "Dashcase words in the region, if active; if not, dashcase word at point.

This converts it from camelCase or snake_case to dash-case.

If the region is active, this function calls `caser-dashcase-region'.
Otherwise, it calls `caser-dashcase-word', with prefix argument passed to it
to dashcase ARG words."
  (interactive "*p")
  (if (use-region-p)
      (caser-dashcase-region (region-beginning) (region-end))
    (caser-dashcase-word arg)))

(defvar caser-dashcase-repeat-map (define-keymap "d" #'caser-dashcase-dwim))
(put #'caser-dashcase-dwim 'repeat-map 'caser-dashcase-repeat-map)

(defun caser--convert-whitespace (beginning-point end-point string-to-convert-to)
  "Convert every space to STRING-TO-CONVERT-TO.

This only converts between BEGINNING-POINT and END-POINT."
  (save-excursion
    (goto-char beginning-point)
    (let ((end-marker (make-marker)))
      (move-marker end-marker end-point)
      (while (re-search-forward (rx (one-or-more ?\s)) (marker-position end-marker) t)
        (replace-match string-to-convert-to)))))

(defun caser--from-space-dwim-helper (words region-function)
  "Helper function for *-from-space-dwim functions.

This cases WORDS words, unless the region is active.

It uses REGION-FUNCTION to case the region."
  (if (use-region-p)
      (let ((beginning (region-beginning))
            (end (region-end))
            (starting-point-marker (point-marker))
            (end-marker (make-marker)))
        (move-marker end-marker end)

        (set-marker-insertion-type starting-point-marker nil)
        (caser--convert-whitespace beginning
                                   end
                                   "-")
        (funcall region-function beginning (marker-position end-marker))
        (goto-char starting-point-marker))
    (let* ((starting-end (point))
           (other-end (progn (caser--forward-word words)
                             (point)))
           (beginning (min starting-end other-end))
           (end (max starting-end other-end))
           (end-marker (make-marker))
           (point-at-end-marker (make-marker)))
      (set-marker end-marker end)
      (set-marker point-at-end-marker other-end)
      (set-marker-insertion-type point-at-end-marker (equal starting-end beginning))

      (caser--convert-whitespace beginning end "-")
      (funcall region-function beginning (marker-position end-marker))

      (goto-char point-at-end-marker))))

;;;###autoload
(defun caser-dashcase-from-space-dwim (&optional words)
  "Dashcase what you mean, including converting spaces to dashes.

If the region is active, dashcase it; otherwise dashcase the word at point.

This converts it from camelCase or snake_case to dash-case.

If the region is active, this function calls `caser-dashcase-region'.
Otherwise, it calls `caser-dashcase-word', with prefix argument passed to it
to dashcase WORDS words."
  (interactive "*p")
  (caser--from-space-dwim-helper words #'caser-dashcase-region))

;;;###autoload
(defun caser-snakecase-from-space-dwim (&optional words)
  "Snakecase what you mean, including converting spaces to underscores.

If the region is active, snakecase it; otherwise snakecase WORDS
words at point, defaulting to 1.

This converts it from camelCase or dash-case to snake_case."
  (interactive "*p")
  (caser--from-space-dwim-helper words #'caser-snakecase-region))

;;;###autoload
(defun caser-camelcase-from-space-dwim (&optional words)
  "Camelcase what you mean, including converting spaces to underscores.

If the region is active, camelcase it; otherwise camelcase WORDS
words at point, defaulting to 1.

This converts it from snake_case or dash-case to camelCase."
  (interactive "*p")
  (caser--from-space-dwim-helper words #'caser-camelcase-region))

;;suggested.
;; (bind-key "M-C" #'caser-camelcase-dwim)
;; (bind-key "M-S" #'caser-snakecase-dwim)
;; (bind-key "M-D" #'caser-dashcase-dwim)

(provide 'caser)
;;; caser.el ends here
