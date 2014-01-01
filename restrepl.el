;;; restrepl.el -- An HTTP REPL

;; Author: Greg Sexton <gregsexton@gmail.com>

;;; Commentary:

;; Provides an interactive interface for making HTTP
;; requests. Inspiration was drawn from looking at the ielm
;; source. comint.el and request.el do much of the heavy lifting and
;; their respective variables will control much of the behaviour of
;; restrepl.

;;; Code:

(require 'comint)
(require 'request)

(defcustom restrepl-buffer-name "*restrepl*"
  "TODO"
  :type 'string
  :group 'restrepl)

(defcustom restrepl-prompt "> "
  "TODO"
  :type 'string
  :group 'restrepl)

(defvar restrepl-header "*** Welcome to REST REPL -- an HTTP REPL ***")

(defun restrepl-input-sender (proc string)
  string)

(define-derived-mode restrepl-mode comint-mode "RestRepl"
  "TODO"
  (setq comint-prompt-regexp (concat "^" (regexp-quote restrepl-prompt)))
  (setq comint-input-sender 'restrepl-input-sender)
  (setq comint-process-echoes nil)

  (let ((process (start-process "restrepl" (current-buffer) "hexl")))
    (set-process-query-on-exit-flag process nil)
    (goto-char (point-max))
    (insert restrepl-header)
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))))

(defun restrepl ()
  "TODO"
  (interactive)
  (let* ((buffer (comint-check-proc "*restrepl*")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'restrepl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer restrepl-buffer-name))
       (current-buffer)))
    (unless buffer (restrepl-mode))))

(provide 'restrepl)
