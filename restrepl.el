;;; restrepl.el -- An HTTP REPL  -*- lexical-binding: t -*-

;; Author: Greg Sexton <gregsexton@gmail.com>

;;; Commentary:

;; Provides an interactive interface for making HTTP
;; requests. Inspiration was drawn from looking at the ielm
;; source. comint.el and url or curl do much of the heavy lifting and
;; their respective variables will control some of the behaviour of
;; restrepl.

;;; Code:

(require 'comint)
(require 'dash)
(require 's)
(require 'url)

;;; customisation and variables

(defcustom restrepl-buffer-name "*restrepl*"
  "TODO"
  :type 'string
  :group 'restrepl)

(defcustom restrepl-response-buffer-name "*rr-response*"
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

(defcustom restrepl-curl-args '("-isS")
  "TODO"
  :type 'sexp
  :group 'restrepl)

(defcustom restrepl-url-middleware '(url-encode-url)
  "Functions applied to a request url in sequence. Each function
should take the url and return the transformed url. You could
make use of this to add custom signing logic for example or
whatever really."
  :type 'sexp
  :group 'restrepl)

(defcustom restrepl-response-middleware
  '(restrepl-apply-content-type-middleware restrepl-comment-headers)
  "Functions applied to a response buffer in sequence. Each
function should take the buffer and return the buffer, after
manipulating it as desired. For example, you may wish to add
restrepl-delete-headers to this list if you do not wish to see
the headers."
  :type 'sexp
  :group 'restrepl)

(defcustom restrepl-content-type-alist
  '(("text/html" . html)
    ("application/json" . js)
    ("application/javascript" . js)
    ("text/xml" . xml)
    ("text/plain" . text)
    ("application/xml" . xml)
    ("html" . html)
    ("json" . js)
    ("javascript" . js)
    ("xml" . xml)
    ("text" . text))
  "TODO: regexp -> key in precedence order. not definitive obviously!"
  :type 'sexp
  :group 'restrepl)

(defcustom restrepl-content-type-middleware-alist
  '((html . ((lambda (b) (html-mode) b)))
    (js . ((lambda (b) (js-mode) b)))
    (xml . ((lambda (b) (xml-mode) b)))
    (text . ((lambda (b) (text-mode) b))))
  "TODO"
  :type 'sexp
  :group 'restrepl)

;;; TODO: define a variable to allow switching the evaluation
;;; engine. Make it a list of choices. curl, url, others?, etc.

(defvar restrepl-header "*** Welcome to REST REPL -- an HTTP REPL ***")

;;; utils

(defun restrepl-apply-middleware (middleware input)
  (-reduce-from (lambda (acc ware) (funcall ware acc))
                input
                middleware))

(defun restrepl-find-headers-end (buffer)
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (+ 1 (re-search-forward "^$")))))

(defun restrepl-get-content-type (buffer)
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (-when-let (header-val (re-search-forward
                              "^[[:space:]]*Content-Type[[:space:]]*:[[:space:]]*"
                              (restrepl-find-headers-end buffer) t))
        (buffer-substring-no-properties header-val (point-at-eol))))))

;;; lexer

(defun restrepl-get-token (input)
  (let ((rexps '((http-method . "\\(GET\\|POST\\|PUT\\|DELETE\\|OPTIONS\\|HEAD\\|TRACE\\|CONNECT\\)")
                 (header-sep . ":")
                 (newline . "\n")
                 (ws . "[ \t]+")
                 (token . "[^ \t\n:]+")
                 (err . ".+"))))
    (car
     (-drop-while 'null
                  (-map (lambda (rx)
                          (-when-let (m (s-match (s-concat "\\`" (cdr rx)) input))
                            (cons (car rx) (car m))))
                        rexps)))))

(defun restrepl-tokenize (input)
  (let (token tokens)
    (while (setq token (restrepl-get-token input))
      (push token tokens)
      (setq input (substring input (length (cdr token)) nil)))
    (reverse tokens)))

;;; parser combinator support

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
  ;; this was originally recursive and built out of optional and seq
  ;; but hits the stack size limit quickly. Not sure if elisp has
  ;; tail-recursion optimization and/or lazy evaluation?
  (lambda (tokens state)
    (let ((prev-result (list tokens state))
          (result (funcall parser tokens state)))
     (while (not (restrepl-p-error-p result))
       (setq prev-result result)
       (setq result (apply 'funcall parser result)))
     prev-result)))

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
         (build-assoc (lambda (key)
                        (lambda (acc-state state) (cons (cons key state) acc-state))))
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
         ;; TODO: support entities in buffers or files
         (entity (restrepl-p-optional
                  (restrepl-p-seq
                   (restrepl-p-state "")
                   (restrepl-p-token 'newline)
                   (restrepl-p-token 'newline)
                   (restrepl-p-0+ (restrepl-p-choice
                                   (restrepl-p-comp anything (lambda (acc-state state)
                                                               (s-concat acc-state state)))
                                   (restrepl-p-token 'newline concat-token-val))))))
         (request (restrepl-p-seq
                   (restrepl-p-token 'http-method
                                     (lambda (old-state token)
                                       (cons (cons 'method (cdr token)) old-state)))
                   (restrepl-p-token 'ws)
                   (restrepl-p-comp anything (funcall build-assoc 'url))
                   (restrepl-p-comp headers (funcall build-assoc 'headers))
                   (restrepl-p-comp entity (funcall build-assoc 'entity)))))
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
  (-concat restrepl-curl-args
           (list "-X" method)
           (restrepl-eval-curl-header-args headers)
           (when entity
             (list "-d" entity))
           (list url)))

(defun restrepl-insertion-filter (buffer)
  (lambda (proc string)
    (with-current-buffer buffer
      (restrepl-print string))))

(defun restrepl-eval-curl (method url headers entity)
  (let* ((args (restrepl-eval-curl-args method url headers entity))
         (process (apply 'start-process "restrepl-curl" nil restrepl-curl-exec args)))
    (set-process-filter process (restrepl-insertion-filter (current-buffer)))
    nil))

(defun restrepl-eval (expr)
  (if (restrepl-p-error-p expr) expr
    (let* ((url (restrepl-apply-middleware
                 restrepl-url-middleware (cdr (assoc 'url expr))))
           (method (s-upcase (cdr (assoc 'method expr))))
           (headers (cdr (assoc 'headers expr)))
           (entity (cdr (assoc 'entity expr))))
      (restrepl-eval-curl method url headers entity))))

;;; open response

(defmacro restrepl-response-middleware (&rest body)
  "Assumes expands in an environment with a bound var 'buffer'."
  `(save-excursion
     (with-current-buffer buffer
       ,@body)
     buffer))

(defun restrepl-delete-headers (buffer)
  (restrepl-response-middleware
   (kill-region (point-min) (restrepl-find-headers-end buffer))))

(defun restrepl-comment-headers (buffer)
  (restrepl-response-middleware
   (comment-region (point-min) (restrepl-find-headers-end buffer))))

(defun restrepl-apply-content-type-middleware (buffer)
  (-when-let (content-type (restrepl-get-content-type buffer))
    (-> restrepl-content-type-alist
      (->> (-first (lambda (alist) (s-matches-p (car alist) content-type))))
      cdr
      (assoc restrepl-content-type-middleware-alist)
      cdr
      (restrepl-apply-middleware buffer))))

(defun restrepl-get-response ()
  (let ((proc (get-buffer-process (current-buffer))))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line -1)
                          (end-of-line)
                          (point-marker))))
        (buffer-substring-no-properties comint-last-input-end pmark)))))

(defun restrepl-display-response (response)
  (let ((buffer (get-buffer-create restrepl-response-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (fundamental-mode)
      (insert response))
    (pop-to-buffer buffer)))

(defun restrepl-open-response ()
  (interactive)
  (->> (restrepl-get-response)
    restrepl-display-response
    (restrepl-apply-middleware restrepl-response-middleware)))

;;; interface

(defun restrepl-insert (&rest args)
  (dolist (string args)
    (when string
      (comint-output-filter (get-buffer-process (current-buffer)) string))))

(defun restrepl-print (result)
  (when result
    (restrepl-insert result
                     (when (not (s-ends-with-p "\n" result)) "\n")
                     restrepl-prompt)))

(defun restrepl-rep (input)
  (-> input restrepl-read restrepl-eval restrepl-print))

(defun restrepl-input-sender (proc string) nil)

(defun restrepl-send-input ()
  (interactive)
  (let ((input (buffer-substring
                (process-mark (get-buffer-process (current-buffer)))
                (point-max))))
    (comint-send-input)        ;ends up invoking restrepl-input-sender
    (restrepl-rep input)))

(defvar restrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'restrepl-send-input)
    (define-key map (kbd "C-c C-c") 'restrepl-open-response)
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
