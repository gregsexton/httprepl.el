;;; -*- lexical-binding: t -*-

;;; restrepl.el -- An HTTP REPL

;; Author: Greg Sexton <gregsexton@gmail.com>

;;; Commentary:

;; Provides an interactive interface for making HTTP
;; requests. Inspiration was drawn from looking at the ielm
;; source. comint.el and url/curl do much of the heavy lifting and
;; their respective variables will control some of the behaviour of
;; restrepl.

;;; Code:

(require 'comint)
(require 'dash)
(require 's)

(defcustom restrepl-buffer-name "*restrepl*"
  "TODO"
  :type 'string
  :group 'restrepl)

(defcustom restrepl-prompt "> "
  "TODO"
  :type 'string
  :group 'restrepl)

(defcustom restrepl-curl-exec "curl"
  "TODO"
  :type 'string
  :group 'restrepl)

(defcustom restrepl-curl-args "-i"
  "TODO"
  :type 'string
  :group 'restrepl)

;;; TODO: define a variable to allow switching the evaluation
;;; engine. Make it a list of choices. curl, url, others?, etc.

(defvar restrepl-header "*** Welcome to REST REPL -- an HTTP REPL ***")

;;; eager lexer

(defun restrepl-get-token (input)
  ;; TODO: these regexs are not complete
  (let ((rexps '((http-get . "GET")
                 (newline . "\n")
                 (ws . "[ \t]+")
                 (token . "[^ \t\n]+")
                 (err . ".+"))))
    (car
     (-drop-while 'null
                  (-map (lambda (rx)
                          (-when-let (m (s-match (s-concat "^" (cdr rx)) input))
                            (cons (car rx) (car m))))
                        rexps)))))

(defun restrepl-tokenize (input)
  (let (token tokens)
    (while (setq token (restrepl-get-token input))
      (push token tokens)
      (setq input (substring input (length (cdr token)) nil)))
    (reverse tokens)))

;;; parser combinator support -- I wish Emacs had namespaces

(defun restrepl-p-prim-parser (err f p)
  "Primitive used to create parsers. ERR should be a function
taking a token and returning an error string. F is a function
that manipulates state, taking a state and token and returning a
state. P is a predicate, takes a token and returns a truthy value
if it is to be consumed falsey otherwise. If the predicate
returns the symbol 'ignore', the parser will be deemed successful
but will not advance the token stream."
  (lambda (tokens state)
    (let* ((token (car tokens))
           (new-state (funcall f state token))
           (success (funcall p token)))
      (if success
          (list (if (equal success 'ignore) tokens (cdr tokens))
                new-state)
        (funcall err token)))))

(defun restrepl-p-error-p (result)
  "Check if the result of a parser was an error."
  (stringp result))

(defun restrepl-p-get-state (result)
  (cadr result))

(defun restrepl-p-get-remaining-tokens (result)
  (car result))

(defun restrepl-p-token (test-token &optional f)
  (restrepl-p-prim-parser
   (lambda (token)
     (format "Parse error - not expecting token: %s" token))
   (lambda (old-state token)
     (if f (funcall f old-state token)
       old-state))
   (lambda (token)
     (equal test-token (car token)))))

(defun restrepl-p-return (f)
  (restrepl-p-prim-parser
   (lambda (token) "")
   (lambda (old-state token) (f old-state))
   (lambda (token) 'ignore)))

(defun restrepl-p-seq (&rest parsers)
  (-reduce-r (lambda (parser acc)
               (lambda (tokens state)
                 (let ((result (funcall parser tokens state)))
                   (if (restrepl-p-error-p result)
                       result
                     (apply #'funcall acc result)))))
             parsers))

(defun restrepl-p-choice (&rest parsers)
  (-reduce (lambda (acc parser)
             (lambda (tokens state)
               (let ((result (funcall acc tokens state)))
                 (if (restrepl-p-error-p result)
                     (funcall parser tokens state)
                   result))))
           parsers))

(defun restrepl-p-true ()
  (restrepl-p-prim-parser (lambda (token) "")
                          (lambda (old-state token) old-state)
                          (lambda (token) 'ignore)))

(defun restrepl-p-many (parser)
  (restrepl-p-choice (restrepl-p-seq parser
                                     (lambda (tokens state)  ;emulate lazy semantics
                                       (funcall (restrepl-p-many parser) tokens state)))
                     (restrepl-p-true)))

(defun restrepl-p-many1 (parser)
  (restrepl-p-seq parser (restrepl-p-many parser)))

;;; parser

(defun restrepl-parse (tokens)
  (let* ((anything (restrepl-p-many1
                    (restrepl-p-choice
                     (restrepl-p-token 'http-get
                                       (lambda (old-state token)
                                         (s-concat old-state (cdr token))))
                     (restrepl-p-token 'ws
                                       (lambda (old-state token)
                                         (s-concat old-state (cdr token))))
                     (restrepl-p-token 'token
                                       (lambda (old-state token)
                                         (s-concat old-state (cdr token)))))))
         (op (restrepl-p-seq
              (restrepl-p-token 'http-get
                                (lambda (old-state token)
                                  (cons (cons 'method (cdr token)) old-state)))
              (restrepl-p-token 'ws)
              ;; TODO: extract a combining function - do NOT use destructuring bind
              (lambda (tokens old-state)
                (destructuring-bind (tokens state) (funcall anything tokens "")
                  (list tokens (cons (cons 'url state) old-state))))))
         (request op))
    (funcall request tokens '())))

;;; reader

(defun restrepl-read (input)
  (let ((result (restrepl-parse (restrepl-tokenize input))))
    (if (restrepl-p-error-p result) result
      (let ((tokens (restrepl-p-get-remaining-tokens result)))
        (if tokens "Parse error - could not consume all tokens"
          (restrepl-p-get-state result))))))

;;; evaluator

;;; TODO: abstract the 'evaluation engine'
;;; TODO: once abstracted, write an url-based engine

(defun restrepl-eval (expr)
  (if (restrepl-p-error-p expr) expr
    (let* ((url (cdr (assoc 'url expr)))
           (method (s-upcase (cdr (assoc 'method expr)))))
      ;; TODO: make robust
      (shell-command-to-string
       (format "%s %s -X %s %s"
               restrepl-curl-exec restrepl-curl-args method url)))))

;;; interface

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

(defun restrepl-input-sender (proc string) nil)

(defun restrepl-send-input ()
  (interactive)
  (let ((input (buffer-substring
                (process-mark (get-buffer-process (current-buffer)))
                (point))))
    (comint-send-input)        ;ends up invoking restrepl-input-sender
    (restrepl-rep input)))

(defvar restrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'restrepl-send-input)
    map)
  "Keymap for `restrepl-mode'.")

(define-derived-mode restrepl-mode comint-mode "RestRepl"
  "TODO"
  :group 'restrepl

  (setq comint-prompt-regexp (concat "^" (regexp-quote restrepl-prompt)))
  (setq comint-input-sender 'restrepl-input-sender)
  (setq comint-process-echoes nil)

  (unless (comint-check-proc (current-buffer))
    (let ((process (condition-case nil
                       (start-process "restrepl" (current-buffer) "hexl")
                     (file-error (start-process "restrepl" (current-buffer) "cat")))))
      (set-process-query-on-exit-flag process nil)
      (unless comint-use-prompt-regexp
        (let ((inhibit-read-only t))
          (add-text-properties
           (point-min) (point-max)
           '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
      (restrepl-print restrepl-header))))

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
