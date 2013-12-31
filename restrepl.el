;;; restrepl.el -- An HTTP REPL

;;; Author: Greg Sexton <gregsexton@gmail.com>

(require 'comint)

(defcustom restrepl-buffer-name "*restrepl*"
  "TODO")

(define-derived-mode restrepl-mode comint-mode "RestRepl"
  "TODO"
  (start-process "restrepl" (current-buffer) "hexl")
  (goto-char (point-max)))

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
