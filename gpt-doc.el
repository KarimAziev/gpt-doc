;;; gpt-doc.el --- Document Elisp code with chat GPT -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gpt-doc
;; Version: 0.1.0
;; Keywords: lisp docs
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Document Elisp code with chat GPT

;;; Code:


(defvar json-object-type)
(defvar json-array-type)
(defvar json-false)
(defvar json-null)
(defvar url-request-method)
(defvar url-request-data)
(defvar url-request-extra-headers)
(defvar url-http-end-of-headers)

(declare-function url-host "url-parse")
(declare-function json-encode "json")
(declare-function auth-source-search "auth-source")
(declare-function json-read-from-string "json")

(defcustom gpt-doc-api-key ""
  "An OpenAI API key (string).

Can also be a function of no arguments that returns an API
key (more secure)."
  :group 'gpt-doc
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that returns the API key")))

(defcustom gpt-doc-gpt-temperature 0.1
  "\"Temperature\" of ChatGPT response.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random."
  :group 'gpt-doc
  :type 'number)

(defcustom gpt-doc-gpt-model "gpt-3.5-turbo"
  "API Model for OpenAI."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-gpt-url "https://api.openai.com/v1/chat/completions"
  "Url queried by GPT."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-with-args-directive "Please follow these step-by-step instructions to respond to user inputs:

Step 1: The user will provide you with emacs-lisp code that includes a function definition enclosed in triple quotes.

Step 2: Write a short documentation (maximum 80 characters) using imperative verbs and avoiding third-party phrasing. For example, instead of saying \"This function returns ...\", simply write \"Return ...\"

Step 3: Describe each argument in one sentence, but only if there are arguments. Make sure to mention and capitalize every argument from the user code. Format the documentation string to fit within an 80-column screen in Emacs. Use imperative verbs and avoid third-party phrasing. Write a short documentation (maximum 80 characters) for the code using imperative verbs and avoiding third-party phrasing. Do not include the function name. If there are arguments, mention them without quotes and capitalize their names. Do not include \"No arguments\" if there are none. If there are arguments, for example, \"a\" and \"b\", instead of writing:
\"ARGUMENTS:
- A: ...some description,
- B: ...some description\"
write more literally, for example:
\"Argument A is ...some description.
\"Argument B is ...some description.\"

Step 4: Concatenate the sentences with an empty line. Do not use headings or markdown syntax. Capitalize only the arguments mentioned in Step 2. Do not include the function name."
  "System prompt (directive) for ChatGPT to document Elisp code with arguments.

These are system instructions sent at the beginning of each
request to ChatGPT."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-no-args-directive
  "Please follow these step-by-step instructions to respond to user inputs:

Step 1: The user will provide you with emacs-lisp code that includes a function definition enclosed in triple quotes.

Step 2: Write a short documentation (maximum 80 characters) using imperative verbs and avoiding third-party phrasing. For example, instead of saying \"This function returns ...\", simply write \"Return ...\"

Step 3: Concatenate the sentences with an empty line. Do not use headings or markdown syntax. Do not include the function name."
  "System prompt (directive) for ChatGPT to document Elisp code without arguments.

These are system instructions sent at the beginning of each
request to ChatGPT."
  :group 'gpt-doc
  :type 'string)


(defvar gpt-doc-docstring-positions
  (mapcar (lambda (it)
            (setcar it (intern (car it)))
            it)
          '(("defun" . 3)
            ("defmacro" . 3)
            ("defsubst" . 4)
            ("defcustom" . 3)
            ("define-skeleton" . 2)
            ("define-compilation-mode" . 3)
            ("define-minor-mode" . 2)
            ("define-derived-mode" . 4)
            ("define-generic-mode" . 8)
            ("cl-defun" . 3)
            ("cl-defsubst" . 3)
            ("cl-defmacro" . 3)
            ("cl-defgeneric" . 3)
            ("cl-defmethod" . 6)
            ("defalias" . 4)
            ("defhydra" . 4)
            ("define-widget" . 3)
            ("transient-define-suffix" . 3)
            ("transient-define-argument" . 3)
            ("transient-define-prefix" . 3)))
  "A list of positions of docstrings for various Elisp functions.")


(defvar gpt-doc-symbols-to-narrow
  (mapcar #'car
          gpt-doc-docstring-positions))

(defun gpt-doc-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count n))
    (while (and (not (= count 0))
                (when-let ((end (ignore-errors
                                  (funcall fn)
                                  (point))))
                  (unless (= end (or pos init-pos))
                    (setq pos end))))
      (setq count (1- count)))
    (if (= count 0)
        pos
      (goto-char init-pos)
      nil)))

(defun gpt-doc-backward-up-list (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (gpt-doc-move-with 'backward-up-list arg))

(defmacro gpt-doc--up-list-until-nil (&rest body)
  "Move backward up across and execute BODY until it's return value is nil."
  `(save-excursion
    (let ((result))
      (while (and (null result)
                  (gpt-doc-backward-up-list))
        (setq result (progn ,@body)))
      result)))

(defun gpt-doc-elisp-bounds-of-def-sexp (&optional symbols)
  "Return bounds of first parent sexp which head is a member of SYMBOLS.
If SYMBOLS is nil use `km-elisp-function-symbols'"
  (gpt-doc--up-list-until-nil
   (when-let ((sexp (sexp-at-point)))
     (when-let ((start (and (listp sexp)
                            (nth 1 sexp)
                            (memq (car sexp)
                                  (or (if (listp symbols)
                                          symbols
                                        (and symbols (list symbols)))
                                      gpt-doc-symbols-to-narrow))
                            (point))))
       (forward-sexp 1)
       (cons start (point))))))



(defvar gpt-doc-docstring-positions
  (mapcar (lambda (it)
            (setcar it (intern (car it)))
            it)
          '(("defun" . 3)
            ("defmacro" . 3)
            ("defsubst" . 4)
            ("defcustom" . 3)
            ("define-skeleton" . 2)
            ("define-compilation-mode" . 3)
            ("define-minor-mode" . 2)
            ("define-derived-mode" . 4)
            ("define-generic-mode" . 8)
            ("ert-deftest" . 3)
            ("cl-defun" . 3)
            ("cl-defsubst" . 3)
            ("cl-defmacro" . 3)
            ("cl-defgeneric" . 3)
            ("cl-defmethod" . 6)
            ("defalias" . 4)
            ("defhydra" . 4)
            ("defgroup" . 3)
            ("deftheme" . 3)
            ("define-widget" . 3)
            ("transient-define-suffix" . 3)
            ("transient-define-argument" . 3)
            ("transient-define-prefix" . 3)
            ("defvar" . 4)
            ("defvar-local" . 4)
            ("cl-defstruct" . 3)
            ("easy-mmode-define-minor-mode" . 2)
            ("transient-define-infix" . 3)
            ("defface" . 3)))
  "A list of positions of docstrings for various Elisp functions.")



(defun gpt-doc--json-parse-string (str &optional object-type array-type
                                           null-object false-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false))
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read-from-string str))))


(defun gpt-doc-api-key-from-auth-source (&optional url user)
  "Return the API key from the auth source for URL and USER.
By default, `gpt-doc-gpt-url' is used as URL and \"apikey\" as USER."
  (require 'auth-source)
  (require 'url-parse)
  (if-let* ((url-obj (url-generic-parse-url (or url gpt-doc-gpt-url)))
            (host (url-host url-obj))
            (secret (plist-get (car (auth-source-search
                                     :host host
                                     :user
                                     (or user
                                         "apikey")
                                     :require '(:secret)))
                               :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `gpt-doc-api-key' found in the auth source")))


(defun gpt-doc-get-api-key ()
  "Get api key from `gpt-doc-api-key'."
  (pcase gpt-doc-api-key
    ((pred functionp)
     (funcall gpt-doc-api-key))
    ((pred stringp)
     (while (string-empty-p gpt-doc-api-key)
       (let ((key (read-string "GPT Api Key: ")))
         (customize-set-value 'gpt-doc-api-key key)
         (when (yes-or-no-p "Save this key?")
           (customize-save-variable 'gpt-doc-api-key key))))
     gpt-doc-api-key)
    (_ (error "`gpt-doc-api-key' is not set"))))


(defun gpt-doc-gpt-request (gpt-prompt system-prompt)
  "Return the response from a request to the OpenAI API.
Argument GPT-PROMPT is the prompt for the GPT model.
Argument SYSTEM-PROMPT is the prompt for the system role."
  (require 'url)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s"
                                        (gpt-doc-get-api-key)))))
         (data `((model . "gpt-3.5-turbo")
                 (messages .
                           ,(apply #'vector
                                   `(((role . "system")
                                      (content . ,system-prompt))
                                     ((role . "user")
                                      (content .
                                               ,gpt-prompt)))))
                 (temperature . ,gpt-doc-gpt-temperature)))
         (url-request-data
          (json-encode data))
         (buffer (url-retrieve-synchronously
                  gpt-doc-gpt-url nil 'silent))
         (response (gpt-doc--json-parse-string
                    (with-current-buffer buffer
                      (buffer-substring-no-properties
                       url-http-end-of-headers (point-max))))))
    (if (not buffer)
        (error "Failed to send request to OpenAI API")
      (condition-case gpt-err
          (if (assoc 'error response)
              (error
               (cdr (assoc 'message (cdr (assoc 'error response)))))
            response)
        (error "Error while parsing API response: %s"
               (error-message-string gpt-err))))))


(defun gpt-doc-upcased-p (string)
  "Return non-nil if STRING has no lowercase."
  (string= (upcase string) string))


(defun gpt-doc-symbol-p (elem)
  "Return whether the given element ELEM is a non-nil symbol.
Argument ELEM is the element to be checked."
  (and elem
       (not (eq elem t))
       (symbolp elem)))


(defun gpt-doc-get-args (sexp)
  "Return a list of arguments extracted from a function/macro definition.
Argument SEXP is the function/macro definition SEXP."
  (and (memq (car-safe sexp)
             '(defun defmacro defun*
                defsubst
                cl-defun define-inline
                cl-defgeneric
                cl-defmethod define-advice))
       (let ((args (car-safe
                    (cdr-safe (cdr-safe sexp)))))
         (let (elems)
           (if (catch 'not-arg
                 (while (consp args)
                   (let ((elem (pop args)))
                     (if
                         (and
                          (pcase (car sexp)
                            ('cl-defun (gpt-doc-symbol-p
                                        (if (listp elem)
                                            (car elem)
                                          elem)))
                            (_ (gpt-doc-symbol-p elem)))
                          (or (not args)
                              (consp args)))
                         (push elem elems)
                       (throw 'not-arg t)))))
               nil
             (seq-remove
              (lambda (it)
                (string-prefix-p "&"
                                 (symbol-name it)))
              elems))))))

(defun gpt-doc--upcase-args (sexp response)
  "Return the RESPONSE with all arguments upcased.
Argument SEXP is the SEXP to extract argument names from.
Argument RESPONSE is the RESPONSE string to upcase arguments in."
  (if-let* ((args-names  (mapcar #'symbol-name
                                 (gpt-doc-get-args sexp)))
            (regex
             (and args-names
                  (regexp-opt args-names
                              'symbols))))
      (with-temp-buffer
        (insert response)
        (goto-char (point-min))
        (while (re-search-forward regex nil t 1)
          (let ((full (match-string-no-properties 0))
                (beg (match-beginning 0))
                (end (match-end 0)))
            (unless (gpt-doc-upcased-p full)
              (upcase-region beg end))))
        (buffer-string))
    response))

(defun gpt-doc--unqote-response-args (response)
  "Return a modified version of the input string with backquoted symbols unquoted.
Argument RESPONSE is the input string to be modified."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (while (re-search-forward
            "\\(\\(`\\)\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)\\(`\\)\\)" nil t 1)
      (let ((full (match-string-no-properties 1))
            (symb (match-string-no-properties 3)))
        (if (gpt-doc-upcased-p full)
            (replace-match symb)
          (replace-match (concat "`" symb "'")))))
    (buffer-string)))

(defun gpt-doc--normalize-gpt-response (sexp response)
  "Return a normalized RESPONSE by removing code blocks and trimming whitespace.
Argument SEXP is the SEXP representation of the code.
Argument RESPONSE is the GPT RESPONSE string."
  (when-let ((code (string-match "```emacs-lisp[\s\t\n]"
                                 response)))
    (setq response (substring-no-properties response 0 code)))
  (string-trim
   (replace-regexp-in-string
    "[\n][\n]+" "\n"
    (mapconcat
     (lambda (l)
       (concat (if (> (length l) 80)
                   (with-temp-buffer
                     (insert l)
                     (fill-region-as-paragraph (point-min)
                                               (point))
                     (buffer-string))
                 l)
               "."))
     (split-string
      (gpt-doc--unqote-response-args
       (gpt-doc--upcase-args sexp response))
      "\\.\\([\s]?+\\)" t)
     "\n"))))

;;;###autoload
(defun gpt-doc-document-current-function ()
  "Document current Elisp definition using the GPT-3 API.
It prompts the user for code input, sends the code to an external service for
documentation generation, and inserts the generated documentation into the
current buffer.
The generated documentation is formatted to fit within an 80-column screen."
  (interactive)
  (pcase-let*
      ((buff (current-buffer))
       (types
        (mapcar #'car
                gpt-doc-docstring-positions))
       (`(,beg . ,_end)
        (gpt-doc-elisp-bounds-of-def-sexp
         types))
       (sexp (if beg
                 (save-excursion
                   (goto-char beg)
                   (sexp-at-point))
               (when-let ((sexp (sexp-at-point)))
                 (memq (car-safe sexp)
                       types))))
       (code (if sexp
                 (let ((formatted (pp-to-string (if (stringp (nth 3 sexp))
                                                    (remove (nth 3 sexp) sexp)
                                                  sexp))))
                   (if (and (memq (car-safe sexp)
                                  '(defun cl-defun)))
                       (with-temp-buffer
                         (insert formatted)
                         (goto-char (point-min))
                         (when (re-search-forward
                                (regexp-opt
                                 '("nil")
                                 'symbols)
                                nil t 1)
                           (replace-match "()"))
                         (buffer-string))
                     formatted))
               (read-string "Code: ")))
       (prompt
        (format
         "```emacs-lisp\n%s\n```\n"
         code))
       (response (gpt-doc-gpt-request
                  prompt
                  (if (and (proper-list-p (nth 2 sexp))
                           (> (length (nth 2 sexp)) 0))
                      gpt-doc-with-args-directive
                    gpt-doc-no-args-directive)))
       (text
        (cdr
         (assoc 'content
                (cdr
                 (assoc 'message (elt (cdr (assoc 'choices
                                                  response))
                                      0)))))))
    (setq text (gpt-doc--normalize-gpt-response sexp text))
    (with-current-buffer buff
      (if buffer-read-only
          (message "%s"
                   text)
        (save-excursion
          (goto-char beg)
          (down-list 1)
          (let ((count (cdr (assq (car sexp)
                                  gpt-doc-docstring-positions))))
            (while (> count 0)
              (setq count (1- count))
              (ignore-errors (forward-sexp 1))))
          (newline-and-indent)
          (insert (prin1-to-string text))
          (forward-sexp -1)
          (forward-char 1))))))

(provide 'gpt-doc)
;;; gpt-doc.el ends here