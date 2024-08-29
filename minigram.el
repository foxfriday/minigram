;;; minigram.el --- Correct grammatical errors. -*- lexical-binding: t; -*-

;; Version: 0.0.01
;; URL: https://github.com/foxfriday/minigram
;; Package-Requires: ((emacs "29"))

;;; Commentary:
;; This package uses Language Tool to check a file or directory and displays
;; the results in a dedicated compilation buffer. This requires Language
;; Tool in the local machine and Java. This is not intended for use with the
;; server provided by Language Tool.

;;; Code:

(require 'compile)
(require 'grep)
(require 'rx)
(require 'simple)

;;;###autoload
(defvar minigram-tool-jar nil
  "Location of language tool jar file.")

(defvar minigram-java "/usr/bin/java"
  "Path to the Java executable, not just java, even if in PATH.")

(defvar minigram-language "en-US"
  "Default language.")

(defvar minigram-disabled-rules "WHITESPACE_RULE,MORFOLOGIK_RULE_EN_US"
  "Disabled rules.")

(defvar minigram-buffer "*minigram*"
  "Default error and log buffer.")

(defvar minigram-raw-output nil
  "Set to true to leave the output from grammar tool unchanged.")

(defvar minigram--regexp-alist (list 'gnu)
  "See `compilation-error-regexp-alist'.")

(defvar minigram--last-check nil
  "Store the path of the last grammar check.")

;;;###autoload
(defvar minigram-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (keymap-set map "g" 'minigram-recompile)
    map)
  "The keymap associated with `minigram-mode'.")

;;;###autoload
(define-compilation-mode minigram-mode "minigram-mode"
  "Uses `compilation-mode' to show the results of Grammar Tool.

The mode only runs the hooks of its parent mode, `compilation-mode'. Navigation
is done using the functions provided by the parent mode.

Key bindings:
\\{minigram-mode-map}"
  :init-value nil
  (setq-local compilation-error-regexp-alist minigram--regexp-alist))

(defun minigram--format-error (file line column type msg)
  "Format line using the FILE, LINE, COLUMN, TYPE, and MSG."
  (format "%s:%s:%s:%s: [%s]\n" file line column type msg))

(defun minigram--clean-buffer ()
  "Clean the error buffer."
  (goto-char (point-min))
  (let* ((lnrg (rx line-start
                   (+ digit)
                   ".)"
                   (+ " ")
                   "Line"
                   (+ " ")
                   (group-n 1 (+ digit))
                   ","
                   (+ space)
                   "column"
                   (+ space)
                   (group-n 2 (+ digit))
                   ","
                   (+ space)
                   "Rule ID:"
                   (+ space)
                   (group-n 3 (+ (or upper "_"))
                          "["
                          (+ digit)
                          "]")
                   (+ space)
                   "premium:"
                   (+ not-newline)
                   (or "\r\n" "\r" "\n")))
         (flrg (rx line-start
                   "Working on "
                   (group "/"
                          (+ (or alphanumeric "/" "-" "_"))
                          "."
                          (or "org" "md" "tex"))
                   "..."
                   (or "\r\n" "\r" "\n")))
         (tmrg (rx line-start
                   "Time:"
                   (+ space)
                   (+ digit)
                   "ms"
                   (+ not-newline)
                   (or "\r\n" "\r" "\n")))
         (empt (rx line-start
                   "Working on "
                   (group "/"
                          (+ (or alphanumeric "/" "-" "_"))
                          "."
                          (or "org" "md" "tex"))
                   "..."
                   (or "\r\n" "\r" "\n")
                   "Time: "
                   (+ digit)
                   "ms"
                   (+ not-newline)
                   (or "\r\n" "\r" "\n")))
         (name)
         (spnt))
    (while (re-search-forward empt nil t)
      (setq name (file-relative-name (match-string 1)))
      (replace-match (minigram--format-error name
                                             "1"
                                             "1"
                                             "Info"
                                             "OK")))
    (goto-char (point-min))
    (while (re-search-forward flrg nil t)
      (setq name (file-relative-name (match-string 1)))
      (replace-match "")
      (setq spnt (point))
      (if (re-search-forward flrg nil t)
          (while (re-search-backward lnrg spnt t)
            (replace-match (minigram--format-error name
                                                   (match-string 1)
                                                   (match-string 2)
                                                   "Error"
                                                   (match-string 3))))
        ;; The last line
        (while (re-search-forward lnrg nil t)
          (replace-match (minigram--format-error name
                                                 (match-string 1)
                                                 (match-string 2)
                                                 "Error"
                                                 (match-string 3)))))
      (re-search-forward tmrg nil t)
      (replace-match ""))))

(defun minigram-sentinel (process _event)
  "Wait for minigram PROCESS to close but has no use for EVENT."
  (when (memq (process-status process) '(exit signal))
    (unless minigram-raw-output
      (let* ((inhibit-read-only t))
        (with-current-buffer minigram-buffer
          (minigram--clean-buffer)
          (minigram-mode))))
    (message "grammar tool is done")))

;;;###autoload
(defun minigram-check (&optional file-or-dir)
  "Check FILE-OR-DIR for grammatical errors."
  (interactive)
  (let* ((inhibit-read-only t)
         (check (if file-or-dir file-or-dir (read-file-name "File or directory:")))
         (java (list (expand-file-name minigram-java) "-jar" minigram-tool-jar))
         (isdr (cond ((file-directory-p check) (list "-r"))
                     ((file-exists-p check) nil)
                     (t (error "File or directory not found"))))
         (lang (if minigram-language (list "-l" minigram-language) nil))
         (disr (if minigram-disabled-rules (list "-d" minigram-disabled-rules) nil))
         (cmnd (append java isdr lang disr (list (expand-file-name check))))
         (ebuf (get-buffer-create minigram-buffer)))
    ;; check jar file
    (unless (file-exists-p minigram-tool-jar)
      (error "Invalid `minigram-tool-jar', file doesn't exist"))
    (with-current-buffer ebuf
      (erase-buffer))
    (setq minigram--last-check check)
    (make-process :name "minigram"
                  :buffer ebuf
                  :command cmnd
                  :sentinel 'minigram-sentinel)))

(defun minigram-recompile ()
  "Rerun the last check."
  (if minigram--last-check
      (minigram-check minigram--last-check)
    (warn "Nothing to recompile.")))

(provide 'minigram)
;;; minigram.el ends here
