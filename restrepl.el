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

(defcustom restrepl-curl-args "-isS"
  "TODO"
  :type 'string
  :group 'restrepl)

;;; TODO: define a variable to allow switching the evaluation
;;; engine. Make it a list of choices. curl, url, others?, etc.

(defvar restrepl-header "*** Welcome to REST REPL -- an HTTP REPL ***")

;;; eager lexer

(defun restrepl-get-token (input)
  ;; TODO: these regexs are not complete
  (let ((rexps '((http-method . "\\(GET\\|POST\\|PUT\\|DELETE\\|OPTIONS\\|HEAD\\|TRACE\\|CONNECT\\)")
                 (header-sep . ":")
                 (newline . "\n")
                 (ws . "[ \t]+")
                 (token . "[^ \t\n:]+")
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
     (format "Parse error - not expecting token: %s" (cdr token)))
   (lambda (old-state token)
     (if f (funcall f old-state token)
       old-state))
   (lambda (token)
     (equal test-token (car token)))))

(defun restrepl-p-state (x)
  (restrepl-p-prim-parser
   (lambda (token) "")
   (lambda (old-state token) x)
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

(defun restrepl-p-optional (parser)
  (restrepl-p-choice parser (restrepl-p-true)))

(defun restrepl-p-0+ (parser)
  (restrepl-p-optional
   (restrepl-p-seq parser
                   (lambda (tokens state) ;emulate lazy semantics
                     (funcall (restrepl-p-0+ parser) tokens state)))))

(defun restrepl-p-1+ (parser)
  (restrepl-p-seq parser (restrepl-p-0+ parser)))

(defun restrepl-p-comp (parser f &optional initial-state)
  "Compose a parser into a composite. INITIAL-STATE is the state
passed to PARSER. F is a function that takes the currently
accumulated state and the output state of PARSER and produces a
new state."
  (lambda (tokens state)
    (let ((result (funcall parser tokens initial-state)))
      (if (restrepl-p-error-p result) result
        (list (restrepl-p-get-remaining-tokens result)
              (funcall f state (restrepl-p-get-state result)))))))

;;; parser

(defun restrepl-parse (tokens)
  (let* ((concat-token-val (lambda (old-state token)
                             (s-concat old-state (cdr token))))
         (anything (restrepl-p-seq
                    (restrepl-p-state "")
                    (restrepl-p-1+
                     (restrepl-p-choice
                      (restrepl-p-token 'http-method concat-token-val)
                      (restrepl-p-token 'header-sep  concat-token-val)
                      (restrepl-p-token 'ws          concat-token-val)
                      (restrepl-p-token 'token       concat-token-val)))))
         (header (restrepl-p-seq
                  (restrepl-p-token 'token (lambda (old-state token) (cdr token)))
                  (restrepl-p-optional (restrepl-p-token 'ws))
                  (restrepl-p-token 'header-sep)
                  (restrepl-p-optional (restrepl-p-token 'ws))
                  (restrepl-p-comp anything (lambda (acc-state state)
                                              (cons acc-state state)))))
         (headers (restrepl-p-seq
                   (restrepl-p-state '())
                   (restrepl-p-0+ (restrepl-p-seq
                                   (restrepl-p-token 'newline)
                                   (restrepl-p-comp header (lambda (acc-state state)
                                                             (cons state acc-state)))))))
         (request (restrepl-p-seq
                   (restrepl-p-token 'http-method
                                     (lambda (old-state token)
                                       (cons (cons 'method (cdr token)) old-state)))
                   (restrepl-p-token 'ws)
                   (restrepl-p-comp anything (lambda (acc-state state)
                                               (cons (cons 'url state) acc-state)))
                   (restrepl-p-comp headers (lambda (acc-state state)
                                              (cons (cons 'headers state) acc-state))))))
    (funcall request tokens '())))

;;; reader

(defun restrepl-read (input)
  (let ((result (restrepl-parse (restrepl-tokenize input))))
    (if (restrepl-p-error-p result) result
      (let ((tokens (restrepl-p-get-remaining-tokens result)))
        (if tokens "Parse error - could not consume all tokens"
          (restrepl-p-get-state result))))))

;;; evaluator
;;; TODO: write an url-based evaluator

(defun restrepl-eval-curl-header-args (headers)
  (-mapcat (lambda (header)
             (list "-H" (s-concat (car header) ":" (cdr header))))
           headers))

(defun restrepl-eval-curl-args (method url headers entity)
  ;; TODO: check for prefix and allow manipulating the args
  (let ((arg-str (format "%s -X %s %s"
                         restrepl-curl-args method url)))
    ;; TODO: not good enough, need to properly parse args or switch to
    ;; having to quote and sending to a shell - in the case where
    ;; restrepl-curl-args contains e.g. -H 'head: foo bar'
    (-concat (s-split "[[:space:]]+" restrepl-curl-args)
             (list "-X" method)
             (restrepl-eval-curl-header-args headers)
             (list url))))

(defun restrepl-eval-curl (method url headers entity)
  (let ((args (restrepl-eval-curl-args method url headers entity)))
    (with-output-to-string
      (with-current-buffer
          standard-output
        (apply 'process-file restrepl-curl-exec nil t nil args)))))

(defun restrepl-eval (expr)
  (if (restrepl-p-error-p expr) expr
    (let* ((url (cdr (assoc 'url expr)))
           (method (s-upcase (cdr (assoc 'method expr))))
           (headers (cdr (assoc 'headers expr))))
      (restrepl-eval-curl method url headers nil))))

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
  ;; TODO: move point to max first -- hitting ret in the middle of a
  ;; line partially parses
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
