;;; diffsync.el --- Use diff to allow syncing of directories -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Bernhard Rotter

;; Author: Bernhard Rotter <bernhard@b-rotter.de>
;; Created: 28 Mar 2022
;; Keywords: tools
;; URL: https://github.com/ber-ro/diffsync
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

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

;; Diffsync uses recursive diff output as a base for directory synchronization.
;; There are shortcuts for the following operations: copy, move, delete, ediff,
;; find-file.

;;; Code:

(require 'diff-mode)

(defvar diffsync-bindings
  `(("c" . diffsync-copy-file)
    ("d" . diffsync-delete-file)
    ("e" . diffsync-ediff)
    ("f" . diffsync-find-file)
    ("m" . diffsync-move-file)
    ("t" . diffsync-toggle-diff)))
(easy-mmode-defmap diffsync-mode-map diffsync-bindings "Keymap for `diffsync'.")

(define-derived-mode
  diffsync-mode diff-mode "DiffSync"
  "Major mode which uses diff output as a base for synchronization.
Only the list of files is visible on startup. Detailed file differences are
hidden, but can be toggled.

Example: (diffsync \"/dir-a/\" \"/dir-b/\")

Some operations need to select a file to operate on. These are selected by
numeric prefix keys. '1' selects the file below /dir-a (1st parameter), '2'
selects the file below /dir-b (2nd parameter) and '0' selects both.

After copy, move and rename a text tag is inserted and further operations are
not allowed on the respective file."
  :group 'tools
  (setq-local revert-buffer-function #'diffsync-revert-buffer))

(defvar-local diffsync-dir1 nil "First directory to compare.")
(defvar-local diffsync-dir2 nil "Second directory to compare.")
(defvar-local diffsync-re-dir1 nil "Regex of first directory to compare.")
(defvar-local diffsync-re-dir2 nil "Regex of second directory to compare.")
(defvar-local diffsync-diff-options "-rsu" "Options for diff invocation.")
(defvar diffsync-buffer nil "Switch back to Diffsync.")

;;;###autoload
(defun diffsync (dir1 dir2)
  "Use recursive diff to enable syncing of directories.
DIR1 and DIR2 are the directories to sync."
  (interactive "DCompare directory: \nD with directory: ")
  (let ((diffsync-buffer (concat "diffsync " dir1 "|" dir2)))
    (get-buffer-create diffsync-buffer)
    (switch-to-buffer-other-window diffsync-buffer)
    (read-only-mode 1)
    (diffsync-mode)
    (setq diffsync-dir1 (directory-file-name dir1))
    (setq diffsync-dir2 (directory-file-name dir2))
    (setq diffsync-re-dir1 (regexp-quote diffsync-dir1))
    (setq diffsync-re-dir2 (regexp-quote diffsync-dir2))
    (diffsync-revert-buffer)
    (diffsync-keywords)))

(defun diffsync-keywords ()
  "Add font-lock keywords."
  (let ((kwl `((,diffsync-dir1 . 'diff-refine-removed)
               (,diffsync-dir2 . 'diff-refine-added))))
    ;; if one directory is a sub-directory of the other, then order makes a
    ;; difference
    (when (< (length diffsync-dir1) (length diffsync-dir2))
      (setq kwl (reverse kwl)))
    (font-lock-add-keywords nil kwl)))

(defun diffsync-revert-buffer (&optional _arg _noconfirm)
  "Rerun diff."
  (interactive)
  (let ((inhibit-read-only t)
        start)
    (erase-buffer)
    (call-process "diff" nil t nil diffsync-diff-options diffsync-dir1 diffsync-dir2)
    (goto-char (point-min))
    (setq buffer-invisibility-spec nil)
    (while (setq start (diffsync-find-diff-start))
      (let ((inhibit-read-only t)
            (filename (intern (car (diffsync-get-filenames))))
            (end (diffsync-find-diff-end)))
        (put-text-property start end 'invisible `(,filename . t))
        (add-to-invisibility-spec `(,filename . t))))
    (goto-char (point-min))))

(defun diffsync-find-diff-start ()
  "Find position of diff command of next file."
  (if (re-search-forward "^diff .*" nil 1)
      (match-end 0)
    nil))

(defun diffsync-find-diff-end ()
  "Find position of diff end of current file."
  (goto-char (if (re-search-forward "^[[:alpha:]]" nil 1)
                 (1- (match-beginning 0))
               (point-max))))

(defun diffsync-get-filenames ()
  "Return filenames of current line as list (1 or 2 items)."
  (save-excursion
    (beginning-of-line)
    (re-search-forward
     (concat
      "^diff " diffsync-diff-options
      " \"?\\(" diffsync-re-dir1 ".+?\\)\"?"
      " \"?\\(" diffsync-re-dir2 ".+?\\)\"?$"
      "\\|^Files \\(.*\\) and \\(.*\\) are identical"
      "\\|^Only in \\(.*\\): \\(.*\\)")
     (line-end-position) 1)
    (cond
     ((match-string 1) (mapcar #'match-string [1 2]))
     ((match-string 3) (mapcar #'match-string [3 4]))
     ((match-string 5) (list (expand-file-name (match-string 6) (match-string 5)))))))

(defun diffsync-get-filename (arg)
  "Select first or second or both files.
Query user, if ARG is required, but not supplied."
  (let ((files (diffsync-get-filenames)))
    (cond ((= 1 (length files)) files)
          (t (mapcar (lambda (arg) (nth arg files)) (diffsync-choice arg))))))

(defun diffsync-choice (arg)
  "Return indices of selected files (0, 1, 0/1).
Prompt user if ARG is not supplied."
  (pcase (or arg
             (read-char-choice "First/second/both files (1/2/0)? " '(?1 ?2 ?0)))
      ((or '1 '?1) '(0))
      ((or '2 '?2) '(1))
      ((or '0 '?0) '(0 1))))

(defun diffsync-toggle-diff (&optional arg)
  "Turn display of diff output on or off.
If ARG is nil, then toggle. If ARG is zero, then turn off. Else turn on."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^diff " (line-end-position) t)
      (let* ((filename (intern (car (diffsync-get-filenames))))
             (elem `(,filename . t)))
        (if (or (and (not arg)
                     (member elem buffer-invisibility-spec))
                (and arg (not (= arg 0))))
            (remove-from-invisibility-spec elem)
          (add-to-invisibility-spec elem))
        (force-window-update (current-buffer))))))

(defun diffsync-delete-file (arg)
  "Delete one file (specified by ARG)."
  (interactive "P")
  (let ((inhibit-read-only t)
        (filenames (diffsync-get-filename arg)))
    (dolist (f filenames)
      (when (yes-or-no-p (concat "Delete " f "? "))
        (delete-file f)
        (diffsync-toggle-diff 0)
        (beginning-of-line)
        (insert "DELETED ")))))

(defun diffsync-find-file (arg)
  "Open current file in buffer (first or second specifed by ARG)."
  (interactive "P")
  (let ((f (diffsync-get-filename arg)))
    (find-file (car f))
    (when (> (length f) 1) (find-file-other-window (nth 1 f)))))

(defun diffsync-copy-file ()
  "Copy current file to other directory."
  (interactive)
  (diffsync-file-op #'copy-file "copied"))

(defun diffsync-move-file ()
  "Move current file to other directory."
  (interactive)
  (diffsync-file-op #'rename-file "moved"))

(defun diffsync-file-op (op text)
  "Get context for copy/move operation.
OP is copy or move
TEXT is description for the message"
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^Only in " (line-end-position) t)
        (let* ((filename (car (diffsync-get-filename 1)))
               (other-filename (diffsync-get-other-filename filename))
               (inhibit-read-only t))
          (funcall op filename other-filename)
          (message (concat text " %s -> %s") filename other-filename)
          (beginning-of-line)
          (insert (upcase text) " "))
      (message "File operation allowed only for files with only 1 instance."))))

(defun diffsync-get-other-filename (filename)
  "Get other file.
FILENAME is below first (return second) or second (return first) diff directory
parameter."
  (let* ((relative1 (file-relative-name filename diffsync-dir1))
         (relative2 (file-relative-name filename diffsync-dir2))
         (firstp (< (length relative1) (length relative2))))
    (expand-file-name (if firstp relative1 relative2)
                      (if firstp diffsync-dir2 diffsync-dir1))))

(defun diffsync-ediff ()
  "Invoke ediff for current file."
  (interactive)
  (save-excursion
    (setq diffsync-buffer (current-buffer))
    (add-hook 'ediff-quit-hook #'diffsync-ediff-hook)
    (apply #'ediff (diffsync-get-filenames))))

(defun diffsync-ediff-hook ()
  "Switch back to Diffsync."
  (switch-to-buffer diffsync-buffer)
  (delete-other-windows)
  (remove-hook 'ediff-quit-hook #'diffsync-ediff-hook))

(provide 'diffsync)
;;; diffsync.el ends here
