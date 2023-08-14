;;; gpt-doc.el --- Document Elisp code with chat GPT -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/gpt-doc
;; Version: 0.1.0-git
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

;; Document Elisp code using the GPT-3 API.

;;  Setup

;; To use this library, you need to have an OpenAI API key. You can either set
;; `gpt-doc-api-key' as a string directly or define a function that returns the API key.

;;  Usage

;; `gpt-doc-document-current-function'

;; This command documents the current Elisp definition using the GPT-3 API. It
;; prompts the user for code input, sends the code to the API for documentation
;; generation, and inserts the generated documentation into the current buffer.


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
  :type '(radio
          (string :tag "API key")
          (function-item gpt-doc-api-key-from-auth-source)
          (function :tag "Function that returns the API key")))

(defcustom gpt-doc-gpt-temperature 0.1
  "\"Temperature\" of ChatGPT response.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random."
  :group 'gpt-doc
  :type 'number)

(defcustom gpt-doc-gpt-model "gpt-3.5-turbo"
  "A string variable representing the API model for OpenAI."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-gpt-url "https://api.openai.com/v1/chat/completions"
  "Url queried by GPT."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-args-directive
  "Please follow these step-by-step instructions to document the Elisp function arguments:

Step 1: The user will provide a function definition enclosed in triple quotes.

Step 3: Describe each argument in separate sentences. For example: \"Argument A is some description.\" \"Argument B ise some description.\" Write a short documentation (maximum 78 characters) for the code using imperative verbs and avoiding third-party phrasing.

Step 4: Concatenate the sentences with an empty line. Do not use headings or markdown syntax."
  "System prompt (directive) for ChatGPT to document Elisp arguments.

These are system instructions sent at the beginning of each
request to ChatGPT."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-with-args-directive "Please follow these step-by-step instructions to document the elisp function:

Step 1: The user will provide a function definition enclosed in triple quotes.

Step 2: Write a short documentation (maximum 78 characters) using imperative verbs and avoiding third-party phrasing to describe what this function does. Use sentence format.

Step 3: If there are arguments, mention them without quotes and capitalize their names in separate sentences. For example: \"Argument A is some description.\" \"Argument B is some description.\" Write a short documentation (maximum 78 characters) for the code using imperative verbs and avoiding third-party phrasing.

Step 4: Concatenate the sentences with an empty line. Do not use headings or markdown syntax. Capitalize only the arguments mentioned in Step 3. Do not include the function name."
  "System prompt (directive) for ChatGPT to document Elisp code with arguments.

These are system instructions sent at the beginning of each
request to ChatGPT."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-variable-prompt
  "Please follow these step-by-step instructions to respond to user inputs:

Step 1: The user will provide you with emacs-lisp code that includes a variable enclosed in triple quotes.

Step 2: Write a short documentation (maximum 70 characters) using imperative verbs and avoiding third-party phrasing. For example, instead of saying \"This variable is a string...\", simply write \"A string ...\" and so on.

Step 3: If it is a custom variable, also decribe it's :type.

Step 4: Concatenate the sentences with an empty line. Do not use headings or markdown syntax. Do not include the variable name."
  "System prompt (directive) for ChatGPT to document Elisp variables.

These are system instructions sent at the beginning of each
request to ChatGPT."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-no-args-directive "Please follow these step-by-step instructions to respond to user inputs:

Step 1: The user will provide you with emacs-lisp code that includes a function definition enclosed in triple quotes.

Step 2: Write a short documentation (maximum 70 characters) using imperative verbs and avoiding third-party phrasing. For example, instead of saying \"This function returns ...\", simply write \"Return ...\"

Step 3: Concatenate the sentences with an empty line. Do not use headings or markdown syntax. Do not include the function name."
  "System prompt (directive) for ChatGPT to document Elisp code without arguments.

These are system instructions sent at the beginning of each
request to ChatGPT."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-docstring-positions
  (mapcar (lambda (it)
            (setcar it (intern (car it)))
            it)
          '(("defun" . 3)
            ("defmacro" . 3)
            ("defsubst" . 3)
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
            ("cl-defmethod" . gpt-doc-forward-to-cl-defmethods-args)
            ("defalias" . 4)
            ("defhydra" . 4)
            ("define-widget" . 3)
            ("transient-define-suffix" . 3)
            ("transient-define-argument" . 3)
            ("transient-define-prefix" . 3)))
  "An alist that maps definition types to their respective documentation positions.
If the value of cell is a number, move forward across N balanced expressions.
If the value is a function, it will be called with definition sexp and should
return number to move forward across."
  :group 'gpt-doc
  :type '(alist
          :key-type symbol
          :value-type (choice
                       (number :tag "Documentation position")
                       (function :tag "Custom function"))))




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
  "Return the bounds of the nearest parent defintion.
Argument SYMBOLS is an optional list of symbols to match against.
Return a cons cell containing the start and end positions of the defun sexp."
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


(defun gpt-doc-api-key-from-auth-source (&optional url)
  "Return the fist API key from the auth source for URL.
By default, the value of `gpt-doc-gpt-url' is used as URL."
  (require 'auth-source)
  (require 'url-parse)
  (if-let* ((url-obj (url-generic-parse-url (or url gpt-doc-gpt-url)))
            (host (url-host url-obj))
            (secret (plist-get (car (auth-source-search
                                     :host host
                                     :require '(:secret)))
                               :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `gpt-doc-api-key' found in the auth source")))


(defun gpt-doc-get-api-key ()
  "Return the value of `gpt-doc-api-key' if it is a function.
If it is a string, prompt the user for a key, save it, and renturn the key.
If `gpt-doc-api-key' is not set, raise an error."
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
         (data `((model . ,gpt-doc-gpt-model)
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

(defun gpt-doc-response-text (response)
  "Return the RESPONSE text from a GPT-3 API RESPONSE.
Argument RESPONSE is the alist."
  (cdr
   (assq 'content
         (cdr
          (assq 'message
                (elt
                 (cdr
                  (assq 'choices
                        response))
                 0))))))

(defun gpt-doc-upcased-p (string)
  "Return non-nil if STRING has no lowercase."
  (string= (upcase string) string))


(defun gpt-doc-symbol-p (elem)
  "Return whether the given element ELEM is a non-nil symbol.
Argument ELEM is the element to be checked."
  (and elem
       (not (eq elem t))
       (symbolp elem)))

(defun gpt-doc-extract-sym (sexp arg)
  "Return the symbol extracted from the given SEXP and argument.
Argument SEXP is the SEXP from which the symbol is extracted.
Argument ARG is the argument used to extract the symbol from the SEXP."
  (let ((sym
         (pcase (car sexp)
           ((or 'cl-defun 'cl-defmethod)
            (if (consp arg)
                (car arg) arg))
           (_ arg))))
    (when (gpt-doc-symbol-p sym)
      sym)))

(defun gpt-doc-get-args (sexp)
  "Return a list of arguments extracted from a function/macro definition.
Argument SEXP is the function/macro definition SEXP."
  (when (and (proper-list-p sexp)
             (assq (car-safe sexp)
                   gpt-doc-docstring-positions))
    (let ((args (seq-find #'proper-list-p sexp)))
      (let (elems)
        (if (catch 'not-arg
              (while (consp args)
                (let* ((elem (pop args))
                       (sym (gpt-doc-extract-sym sexp elem)))
                  (if
                      (and sym
                           (or (not args)
                               (consp args)))
                      (push sym elems)
                    (throw 'not-arg t)))))
            nil
          (seq-remove
           (lambda (it)
             (let ((name (symbol-name it)))
               (or (string-prefix-p "&"
                                    name)
                   (string-prefix-p "_"
                                    name))))
           (nreverse elems)))))))

(defun gpt-doc--downcase-repeat-args (response)
  "Downcase repeat args.
Argument RESPONSE is the string to be processed."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (when (re-search-forward
           "Argument \\(\\(['`]?\\)\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)\\([`']?\\)\\)"
           nil t 1)
      (let ((arg (match-string-no-properties 3)))
        (while (re-search-forward (regexp-quote arg) nil t 1)
          (downcase-region (match-beginning 0)
                           (match-end 0)))))
    (buffer-string)))

(defun gpt-doc-quote-args (text)
  "Extracts and formats quoted arguments in a given TEXT.

Argument TEXT is a string that represents the text to be processed."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (re-search-forward
           "\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+[-]\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
           nil t 1)
      (let ((arg (match-string-no-properties 0))
            (beg (match-beginning 0))
            (end (match-end 0)))
        (when (member (char-to-string (char-before beg)) '("'" "`"))
          (setq beg (1- beg)))
        (when (member (char-to-string (char-before end)) '("'" "`"))
          (setq end (1+ end)))
        (unless (gpt-doc-upcased-p arg)
          (delete-region beg end)
          (insert (format "`%s'" arg)))))
    (buffer-string)))

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
      (gpt-doc--downcase-repeat-args
       (with-temp-buffer
         (insert response)
         (goto-char (point-min))
         (while (re-search-forward regex nil t 1)
           (let ((full (match-string-no-properties 0))
                 (beg (match-beginning 0))
                 (end (match-end 0)))
             (if (save-excursion
                   (goto-char beg)
                   (skip-chars-backward "\s\t")
                   (looking-back
                    "Argument \\(\\(['`]?\\)\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)\\([`']?\\)\\)[\s\t]+is the[\s]\\{1\\}"
                    0))
                 (downcase-region beg end)
               (unless (gpt-doc-upcased-p full)
                 (upcase-region beg end)))))
         (buffer-string)))
    response))

(defun gpt-doc--unqote-response-args (response)
  "Return a modified version of the input string with backquoted symbols unquoted.
Argument RESPONSE is the input string to be modified."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (while (re-search-forward
            "\\(\\(['`]\\)\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)\\([`']\\)\\)" nil t
            1)
      (let ((full (match-string-no-properties 1))
            (symb (match-string-no-properties 3)))
        (if (gpt-doc-upcased-p full)
            (replace-match symb)
          (replace-match (concat "`" symb "'")))))
    (buffer-string)))


(defun gpt-doc-uniq-sentences (text)
  "Return a list of unique sentences from the given TEXT.
Argument TEXT is the input TEXT from which sentences are extracted."
  (let ((sentences (split-string
                    text
                    "\\.\\([^a-z-0-9]\\|$\\)" t)))
    (seq-uniq (mapcar
               (lambda (it)
                 (format "%s."
                         (string-trim
                          (replace-regexp-in-string "[\n]+" "\s" it))))
               sentences))))



(defun gpt-doc-shorten-text (text)
  "Shorten TEXT by removing the leading \"a macro/function/variable that\" phrase.
Argument TEXT is the input TEXT."
  (replace-regexp-in-string
   (rx (seq "\"^"
            (one-or-more
             (any "a-z"))
            (any "\\s")
            "a"
            (any "\\s")
            "\\(macro\\|function\\|variable\\)"
            (one-or-more
             (any "\\st"))
            "\\(\\(\\("
            (opt (any "'`"))
            "\\)\\(\\"
            (opt "(")
            ":\\sw\\|\\s_\\|\\\\" nonl "\\"
            (one-or-more ")")
            "\\)\\("
            (opt (any "'`"))
            "\\)\\)"
            (any "\\st")
            "\\"
            (opt ")")
            "that"
            (one-or-more
             (any "\\s"))
            "\""))
   "" text))

(defun gpt-doc-ensure-first-sentence (text)
  "Ensure the first sentence of the given TEXT is capitalized."
  (let ((new-text (gpt-doc-shorten-text text)))
    (if (not (= (length text)
                (length new-text)))
        (with-temp-buffer
          (insert new-text)
          (goto-char (point-min))
          (capitalize-word 1)
          (when-let ((c (char-before (point))))
            (when (= c 115)
              (delete-char -1)))
          (buffer-string))
      text)))

(defun gpt-doc-trim-steps (text)
  "Remove \"Step [0-9]+: Argument\" from the given TEXT.

Argument TEXT is the string input that the function will process to remove
specific patterns."
  (replace-regexp-in-string "\\(Step [0-9]+: \\)Argument " "" text nil nil 1))

(defun gpt-doc-fill-docs (text)
  "Fill the TEXT with formatted sentences, each ending with a period.
Argument TEXT is a string containing the text to be formatted."
  (let ((sentences (split-string text
                                 "\\.\\([\s\t\n\r\f]\\|$\\)" t)))
    (mapconcat
     (lambda (l)
       (setq l (format "%s." l))
       (if (> (length l) 80)
           (with-temp-buffer
             (insert l)
             (fill-region-as-paragraph (point-min)
                                       (point))
             (buffer-string))
         l))
     sentences
     "\n")))

(defun gpt-doc--normalize-gpt-response (sexp response)
  "Return a normalized RESPONSE by removing code blocks and trimming whitespace.
Argument SEXP is the SEXP representation of the code.
Argument RESPONSE is the GPT RESPONSE string."
  (when-let ((code (string-match "```emacs-lisp[\s\t\n]"
                                 response)))
    (setq response (substring-no-properties response 0 code)))
  (let ((sentences (gpt-doc-uniq-sentences response)))
    (when-let ((first-sentence (gpt-doc-ensure-first-sentence
                                (car sentences))))
      (setcar sentences first-sentence))
    (setq sentences (mapcar (apply-partially #'gpt-doc--upcase-args sexp)
                            sentences))
    (setq sentences (mapcar #'gpt-doc--unqote-response-args sentences))
    (mapconcat
     (lambda (l)
       (if (> (length l) 80)
           (with-temp-buffer
             (insert l)
             (fill-region-as-paragraph (point-min)
                                       (point))
             (buffer-string))
         l))
     sentences
     "\n")))


(defun gpt-doc-forward-sexp (count)
  "Return the position after moving forward by COUNT balanced sexps.
Argument COUNT is the number of balanced sexps to move forward."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let ((pos))
      (while (and (> count 0)
                  (or (not pos)
                      (= pos (point))))
        (setq count (1- count))
        (ignore-errors (forward-sexp 1))
        (setq pos (point)))
      pos)))

(defun gpt-doc-first-list-pos (sexp)
  "Return position of the first proper list in the given SEXP.
Argument SEXP is the SEXP to search for proper lists."
  (and (proper-list-p sexp)
       (seq-position sexp nil (lambda (a &rest _)
                                (proper-list-p a)))))

(defun gpt-doc-first-doc-pos (sexp)
  "Return the position of documentation string after args lists in the given SEXP.
Argument SEXP is the SEXP to search for the documentation string."
  (when-let* ((arg-pos (gpt-doc-first-list-pos sexp))
              (doc (nth (1+ arg-pos) sexp)))
    (and (stringp doc)
         (1+ arg-pos))))

(defun gpt-doc-remove-doc-string (sexp)
  "Return a modified version of the input SEXP with the doc string removed.
Argument SEXP is the s-expression to remove the doc string from."
  (when-let* ((doc-pos (gpt-doc-first-doc-pos sexp))
              (left (seq-subseq sexp 0 doc-pos)))
    (if (> (length sexp)
           (1+ doc-pos))
        (append left
                (seq-subseq sexp (1+ doc-pos)
                            (length sexp)))
      left)))

(defun gpt-doc-forward-to-cl-defmethods-args (sexp)
  "Return the position of the first proper list in SEXP plus 2.
Argument SEXP is a list."
  (when-let ((pos
              (and (proper-list-p sexp)
                   (gpt-doc-first-list-pos
                    (seq-find #'proper-list-p
                              (seq-drop
                               sexp 2))))))
    (+ pos 2)))

(defun gpt-doc-pp-sexp (sexp)
  "Return a formatted string representation of a given s-expression.
Argument SEXP is the s-expression to be formatted."
  (let ((formatted (pp-to-string
                    (or (gpt-doc-remove-doc-string sexp)
                        sexp))))
    (pcase (car sexp)
      ((or 'defvar 'defvar-local 'defcustom)
       formatted)
      ((guard (gpt-doc-get-args sexp))
       formatted)
      (_ (with-temp-buffer
           (insert formatted)
           (goto-char (point-min))
           (when (re-search-forward
                  (regexp-opt
                   '("nil")
                   'symbols)
                  nil t 1)
             (replace-match "()"))
           (buffer-string))))))



(defun gpt-doc-document-arguments (sexp)
  "Describe arguments in a function or macro SEXP."
  (when-let ((args
              (pcase (car sexp)
                ((or 'defvar 'defvar-local 'defcustom)
                 nil)
                (_
                 (mapcar #'symbol-name
                         (gpt-doc-get-args sexp))))))
    (let* ((args-directives
            (string-join
             (append
              '("Please follow these step-by-step instructions."
                "Step 1: The user will provide you with an Emacs Lisp function/macro enclosed in triple quotes.")
              (seq-map-indexed
               (lambda (arg i)
                 (format
                  "Step %d: Describe argument %s in one sentence that starts with \"Argument %s is \"."
                  (+ i 2)
                  arg
                  (upcase
                   arg)))
               args))
             "\n"))
           (args-response
            (when args-directives
              (gpt-doc-response-text
               (gpt-doc-gpt-request
                (format
                 "```emacs-lisp\n%s\n```\n"
                 (gpt-doc-pp-sexp sexp))
                args-directives)))))
      (gpt-doc-fill-docs
       (gpt-doc-trim-steps
        (gpt-doc--unqote-response-args
         (gpt-doc--upcase-args sexp args-response)))))))

(defun gpt-doc-get-short-documentation (sexp)
  "Get the short documentation for an Emacs Lisp function or variable.
Argument SEXP is the function or variable definition."
  (let* ((code (gpt-doc-pp-sexp sexp))
         (prompt
          (pcase (car sexp)
            ((or 'defvar 'defvar-local 'defcustom)
             "Please follow these step-by-step instructions:

Step 1: The user will provide you with an Emacs Lisp variable definition enclosed in triple quotes.

Step 2: Describe this variable in one sentence. Use imperative verbs only and avoid third-party phrasing, with a maximum of 78 characters.")
            (_
             "Write a very short sentence that starts with imperative verb about what the function below does in maximum *70* characters. Don't use phrases like \"in Emacs\", \"in Emacs Lisp\" and so on.")))
         (text (gpt-doc-response-text
                (gpt-doc-gpt-request
                 (format
                  "```emacs-lisp\n%s\n```\n"
                  code)
                 prompt)))
         (normalized (gpt-doc--unqote-response-args
                      (gpt-doc--upcase-args sexp
                                            (if
                                                (and (string-prefix-p "\"" text)
                                                     (string-suffix-p "\"" text))
                                                (read text)
                                              text))))
         (parts (split-string normalized nil t))
         (first-word (pop parts)))
    (when-let
        ((imp (and first-word
                   (boundp 'checkdoc-common-verbs-wrong-voice)
                   (cdr (assoc-string (downcase first-word)
                                      checkdoc-common-verbs-wrong-voice)))))
      (setq first-word (capitalize (seq-copy imp))))
    (concat first-word " " (string-join parts " "))))

(defun gpt-doc-document-sexp (sexp)
  "Join the first sentence and argument documentation of a SEXP.
Argument SEXP is the sexp (S-expression) that will be documented."
  (let* ((first-sentence (gpt-doc-get-short-documentation sexp))
         (args-docs (gpt-doc-document-arguments sexp)))
    (gpt-doc-quote-args
     (string-join (delq nil (list first-sentence args-docs))
                  "\n\n"))))

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
       (text
        (gpt-doc-document-sexp sexp)))
    (with-current-buffer buff
      (if buffer-read-only
          (message "%s"
                   text)
        (save-excursion
          (goto-char beg)
          (down-list 1)
          (let ((count (cdr (assq (car sexp)
                                  gpt-doc-docstring-positions))))
            (when (functionp count)
              (setq count (funcall count sexp)))
            (when count
              (gpt-doc-forward-sexp count)
              (if (looking-back "\n" 0)
                  (indent-according-to-mode)
                (newline-and-indent)
                (insert (prin1-to-string text))
                (forward-sexp -1)
                (forward-char 1)))))))))

(provide 'gpt-doc)
;;; gpt-doc.el ends here