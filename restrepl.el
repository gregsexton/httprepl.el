;;; restrepl.el -- An HTTP REPL

;; Author: Greg Sexton <gregsexton@gmail.com>

;;; Commentary:

;; Provides an interactive interface for making HTTP
;; requests. Inspiration was drawn from looking at the ielm
;; source. comint.el and request.el do much of the heavy lifting and
;; their respective variables will control some of the behaviour of
;; restrepl.

;;; Code:

(require 'comint)
(require 'dash)
(require 'request)
(require 's)

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

(defun restrepl-read (input) input)

(defun restrepl-eval (expr) expr)

(defun restrepl-insert (&rest args)
  (dolist (string args)
    (when string
      (comint-output-filter (get-buffer-process (current-buffer)) string))))

(defun restrepl-print (result)
  (restrepl-insert result
                   (when (not (s-ends-with-p "\n" result)) "\n")
                   restrepl-prompt))

(defun restrepl-rep (input)
  (-> input restrepl-read restrepl-eval restrepl-print))

(define-derived-mode restrepl-mode comint-mode "RestRepl"
  "TODO"
  :group 'restrepl

  (setq comint-prompt-regexp (concat "^" (regexp-quote restrepl-prompt)))
  (setq comint-input-sender 'restrepl-input-sender)
  (setq comint-process-echoes nil)

  (unless (comint-check-proc (current-buffer))
    (let ((process (start-process "restrepl" (current-buffer) "hexl")))
      (set-process-query-on-exit-flag process nil)
      ;; TODO (insert restrepl-header)
      (unless comint-use-prompt-regexp
        (let ((inhibit-read-only t))
          (add-text-properties
           (point-min) (point-max)
           '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
      (restrepl-rep ""))))

(defun restrepl ()
  "TODO"
  (interactive)
  (let ((buffer (get-buffer restrepl-buffer-name)))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'restrepl-mode)))
         (get-buffer-create (or buffer restrepl-buffer-name))
       (current-buffer)))
    (restrepl-mode)))

(provide 'restrepl)
