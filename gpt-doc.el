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

(defcustom gpt-doc-gebug nil
  "Whether to enable debugging in the GPT documentation group."
  :group 'gpt-doc
  :type 'boolean)

(defcustom gpt-doc-gpt-url "https://api.openai.com/v1/chat/completions"
  "The URL to the OpenAI GPT API endpoint for chat completions."
  :group 'gpt-doc
  :type 'string)


(defcustom gpt-doc-variable-prompt "The user will provide you an Emacs Lisp Code. Your task is to write documentation for %s in one sentence. Use imperative verbs only and avoid third-party phrasing, with a maximum of 78 characters. Don't use phrases like \"in Emacs\", \"in Emacs Lisp\", in `%s' and so on. Do NOT wrap the start and end of your text in quotes."
  "System prompt (directive) for ChatGPT to document Elisp variables."
  :group 'gpt-doc
  :type 'string)


(defcustom gpt-doc-first-sentence-doc-prompt "Write a very short sentence that starts with imperative verb about what the %s below does in maximum *70* characters. Don't use phrases like \"in Emacs\", \"in Emacs Lisp\", in `%s' and so on. Do NOT wrap the start and end of your text in quotes."
  "System prompt to generate first sentence of function documentation."
  :group 'gpt-doc
  :type 'string)

(defcustom gpt-doc-args-prompt "The user will provide you an Emacs Lisp code.
Provide a sentence for every argument of %s that starts either with \"Argument \",  \"Optional argument \", \"Remaining arguments\" or \"Arguments \" description such as type, default value without any additional text, prompt or note.

Sentences should be separated by two newline characters.
1. Do NOT wrap the start and end of your text in quotes.
2. Make sure that your text doesn't exceed 70 columns in width.
3. Don't use phrases like \"in Emacs\", \"in Emacs Lisp\", \"this function\",
\"you\" and so on.

Failure to comply with these rules will result in an error.

Example 1:
```elisp
(defun km-call-process (program &rest args)
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil
                         (flatten-list args))))
      (let ((result (string-trim (buffer-string))))
        (if (zerop status)
            result
          (message result) nil)))))
```
Argument PROGRAM is the shell command to run.

Remaining arguments ARGS are strings passed as command arguments to PROGRAM.

Example 2:
```elisp
(defun gpt-doc--json-parse-string (str &optional object-type array-type null-object false-object) (if (and (fboundp 'json-parse-string) (fboundp 'json-available-p) (json-available-p)) (json-parse-string str :object-type (or object-type 'alist) :array-type (pcase array-type ('list 'list) ('vector 'array) (_ 'array)) :null-object (or null-object :null) :false-object (or false-object :false)) (require 'json) (let ((json-object-type (or object-type 'alist)) (json-array-type (pcase array-type ('list 'list) ('array 'vector) (_ 'vector))) (json-null (or null-object :null)) (json-false (or false-object :false))) (json-read-from-string str))))
```
The argument OBJECT-TYPE is the type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  "System prompt for ChatGPT to document Elisp arguments."
  :group 'gpt-doc
  :type 'string)


(defcustom gpt-doc-docstring-positions (mapcar (lambda (it)
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
                                                 ("transient-define-prefix" . 3)
                                                 ("ert-deftest" . 3)))
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


(defcustom gpt-doc-prompt-types (mapcar (lambda (it)
                                          (setcar it (intern (car it)))
                                          it)
                                        '(("defun" . "function")
                                          ("defmacro" . "macro")
                                          ("defsubst" . "inline function")
                                          ("defcustom" . "custom variable")
                                          ("define-skeleton" . "skeleton")
                                          ("define-compilation-mode" . "compilation mode")
                                          ("define-minor-mode" . "minor mode")
                                          ("define-derived-mode" . "major mode")
                                          ("define-generic-mode" . "generic mode")
                                          ("cl-defun" . "function")
                                          ("cl-defsubst" . "fuction")
                                          ("cl-defmacro" . "macro")
                                          ("cl-defgeneric" . "generic")
                                          ("cl-defmethod" . "method")
                                          ("defalias" . "alias")
                                          ("defhydra" . "hydra")
                                          ("define-widget" . "widget")
                                          ("transient-define-suffix" . "transient suffix command")
                                          ("transient-define-argument" . "transient argument command")
                                          ("transient-define-prefix" . "transient prefix")
                                          ("ert-deftest" . "test")))
  "An alist that maps definition types to their respective documentation labels."
  :group 'gpt-doc
  :type '(alist
          :key-type symbol
          :value-type string))


(defun gpt-doc-debug-log (&rest args)
  "Log debug messages when `gpt-doc-debug' is true.

Argument ARGS is a list of arguments for `message'."
  (when gpt-doc-gebug
    (apply #'message args)))




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

(declare-function json-read "json")

(defun gpt-doc-json-read-buffer (&optional object-type array-type null-object
                                    false-object)
  "Parse json from the current buffer using specified object and array types.

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
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (or null-object :null)
       :false-object (or false-object :false))
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read))))
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
                                      (content . ,(string-to-unibyte
                                                   (encode-coding-string
                                                    system-prompt
                                                    'utf-8))))
                                     ((role . "user")
                                      (content .
                                               ,(string-to-unibyte
                                                 (encode-coding-string
                                                  gpt-prompt
                                                  'utf-8)))))))
                 (temperature . ,gpt-doc-gpt-temperature)))
         (url-request-data
          (json-encode data))
         (buffer (url-retrieve-synchronously
                  gpt-doc-gpt-url nil 'silent))
         (response (gpt-doc--json-parse-string
                    (with-current-buffer buffer
                      (buffer-substring-no-properties
                       url-http-end-of-headers (point-max))))))
    (gpt-doc-debug-log "gpt-doc: response %s"
                       response)
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
  "Return the RESPONSE text from a GPT-3 API response.
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
Argument SEXP is the s-expression from which the symbol is extracted.
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
Argument SEXP is the function/macro definition sexp."
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
        (when (member (char-to-string (char-after end)) '("'" "`"))
          (setq end (1+ end)))
        (unless (gpt-doc-upcased-p arg)
          (delete-region beg end)
          (insert (format "`%s'" arg)))))
    (buffer-string)))

(defun gpt-doc-get-args-names (sexp)
  "Extract and return argument names from a given s-expression SEXP.

Argument SEXP is a symbolic expression in Emacs Lisp."
  (mapcar #'symbol-name
          (gpt-doc-get-args sexp)))

(defun gpt-doc-upcase-regex (regex response)
  "Convert matched REGEX patterns in a RESPONSE string to uppercase.

Argument REGEX is a string that represents the regular expression to be used for
searching within the RESPONSE.
Argument RESPONSE is a string that contains the text in which the regular
expression search will be performed."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (while (re-search-forward regex nil t 1)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (upcase-region beg end)
        (while (looking-at "[`']")
          (delete-char 1))
        (save-excursion
          (goto-char beg)
          (while (looking-back "[`']" 0)
            (delete-char -1)))))
    (buffer-string)))


(defun gpt-doc--upcase-args (sexp response)
  "Return the RESPONSE with all arguments upcased.
Argument SEXP is the SEXP to extract argument names from.
Argument RESPONSE is the RESPONSE string to upcase arguments in."
  (if-let* ((args-names (mapcar #'symbol-name
                                (gpt-doc-get-args sexp)))
            (regex
             (and args-names
                  (regexp-opt args-names
                              'symbols))))
      (gpt-doc-upcase-regex regex response)
    response))

(defun gpt-doc--unqote-response-args (response)
  "Return a modified version of the input string with backquoted symbols unquoted.
Argument RESPONSE is the input string to be modified."
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (while (re-search-forward
            "\\(\\([\"'`]\\)\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)\\([`'\"]\\)\\)"
            nil t
            1)
      (let ((full (match-string-no-properties 1))
            (symb (match-string-no-properties 3)))
        (if (or (member symb '("t" "nil"))
                (gpt-doc-upcased-p full))
            (replace-match symb)
          (replace-match (concat "`" symb "'")))))
    (buffer-string)))

(defun gpt-doc-trim-steps (text)
  "Remove \"Step [0-9]+: Argument\" from the given TEXT.

Argument TEXT is the string input that the function will process to remove
specific patterns."
  (replace-regexp-in-string
   "\\(\\(Step\\[\s]+\\)?[0-9]+[.:][\s\t\r\f\n]*\\)\\(Argument\\|optional argument\\)"
   "" text
   nil nil 1))

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
Argument SEXP is the s-expression to search for proper lists."
  (and (proper-list-p sexp)
       (seq-position sexp nil (lambda (a &rest _)
                                (proper-list-p a)))))

(defun gpt-doc-first-doc-pos (sexp)
  "Return the position of documentation string after args lists in the given SEXP.
Argument SEXP is the s-expression to search for the documentation string."
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


(defun gpt-doc-get-prompt-for-args (sexp &optional related-sexps)
  "Generate user and system prompt for arguments of a given SEXP.

Argument SEXP is a list representing a Lisp expression.

Optional argument RELATED-SEXPS is a list of Lisp expressions related to SEXP."
  (when-let ((args
              (pcase (car sexp)
                ((or 'defvar 'defvar-local 'defcustom 'define-minor-mode
                     'define-derived-mode)
                 nil)
                (_
                 (gpt-doc-get-args-names sexp)))))
    (let* ((sexp-str (gpt-doc-join-sexps related-sexps))
           (label (format "%s `%s'"
                          (or (cdr (assq (car sexp) gpt-doc-prompt-types))
                              "definition")
                          (nth 1 sexp)))
           (system-prompt
            (apply #'format gpt-doc-args-prompt
                   (seq-take (list label (car sexp))
                             (length
                              (gpt-doc-get-matches "%s"
                                                   gpt-doc-args-prompt)))))
           (user-prompt (format
                         "```elisp\n%s\n\n%s\n```\n"
                         (gpt-doc-pp-sexp sexp)
                         sexp-str)))
      (gpt-doc-debug-log "user-prompt for arguments:\n```\n%s\n```\n"
                         user-prompt)
      (gpt-doc-debug-log "system-prompt for arguments:\n```\n%s\n```\n"
                         system-prompt)
      (cons user-prompt system-prompt))))

(defun gpt-doc-document-arguments (sexp &optional related-sexps)
  "Generate documentation for each argument of a given Emacs Lisp function.

Argument SEXP is a list that represents a symbolic expression.

Optional argument RELATED-SEXPS is a list of symbolic expressions that are
related to SEXP.
It is not required and has no default value."
  (pcase-let*
      ((`(,user-prompt . ,system-prompt)
        (gpt-doc-get-prompt-for-args sexp
                                     related-sexps))
       (text
        (when system-prompt
          (gpt-doc-response-text
           (gpt-doc-gpt-request
            user-prompt
            system-prompt)))))
    (gpt-doc-debug-log
     "gpt-doc-document-arguments raw response text:\n```\n%s\n```\n" text)
    (when text
      (gpt-doc-pipe-ignore-errors
       `(gpt-doc-maybe-read
         ,(apply-partially #'gpt-doc--upcase-args sexp)
         gpt-doc--unqote-response-args
         gpt-doc-trim-steps
         gpt-doc-fill-docs)
       text))))

(defun gpt-doc-pipe-ignore-errors (fns text)
  "Apply a list of functions to a TEXT, ignoring any errors that occur.

Argument FNS is a list of functions.
Argument TEXT is a string."
  (let ((fn (pop fns)))
    (seq-reduce (lambda (acc fn)
                  (setq acc (or (ignore-errors
                                  (funcall fn acc))
                                acc)))
                fns
                (or (ignore-errors (funcall fn text))
                    text))))



(defun gpt-doc-maybe-read (text)
  "Convert a string TEXT to a Lisp object if it's enclosed in quotes.

Argument TEXT is a string that is checked if it starts and ends with a quotation
mark."
  (if
      (and (string-prefix-p "\"" text)
           (string-suffix-p "\"" text))
      (or (ignore-errors (read text))
          (substring-no-properties text 1 (1- (length text)))
          text)
    text))

(defun gpt-doc-normalize-response-content (text &optional sexp)
  "Normalize and unquote the response content from a GPT request.

Argument TEXT is a string that represents the text to be normalized.

Optional argument SEXP is a boolean value that, if provided, indicates whether
the TEXT should be treated as a symbolic expression (sexp).
The default value is nil."
  (gpt-doc-pipe-ignore-errors
   `(gpt-doc-maybe-read
     ,(apply-partially #'gpt-doc--upcase-args sexp)
     gpt-doc--unqote-response-args)
   text))

(defun gpt-doc-get-matches (re str)
  "Search backwards for matches of a regular expression in a string.

Argument RE is a regular expression string that the function will search for in
the backward direction.

Argument STR is a string where the function will perform the search operation."
  (with-temp-buffer
    (let ((res))
      (insert str)
      (while (re-search-backward re nil t 1)
        (push (match-string-no-properties 0) res))
      res)))

(defun gpt-doc-get-prompt-for-summary (sexp &optional related-sexps)
  "Generate user and system prompt messages for summarizing a given SEXP.

Argument SEXP is a symbolic expression (sexp) in Emacs Lisp.

Optional argument RELATED-SEXPS is a list of related symbolic expressions
\(sexps) in Emacs Lisp."
  (let* ((code (gpt-doc-pp-sexp sexp))
         (name (nth 1 sexp))
         (label (format "the %s `%s'"
                        (or (cdr (assq (car sexp) gpt-doc-prompt-types))
                            "definition")
                        name))
         (sexp-str (gpt-doc-join-sexps related-sexps))
         (user-prompt (format
                       "```elisp\n%s\n\n%s\n```\n"
                       code sexp-str))
         (system-prompt
          (pcase (car sexp)
            ((or 'defvar 'defvar-local 'defcustom)
             (apply #'format gpt-doc-variable-prompt
                    (seq-take (list label name)
                              (length
                               (gpt-doc-get-matches "%s" gpt-doc-variable-prompt)))))
            (_
             (apply #'format gpt-doc-first-sentence-doc-prompt
                    (seq-take (list label name)
                              (length
                               (gpt-doc-get-matches
                                "%s"
                                gpt-doc-first-sentence-doc-prompt))))))))
    (gpt-doc-debug-log "user-prompt for summary\n```\n%s\n```" user-prompt)
    (gpt-doc-debug-log "system-prompt for summary:\n\n%s\n" system-prompt)
    (cons user-prompt system-prompt)))



(defun gpt-doc-get-short-documentation (sexp &optional related-sexps)
  "Generate a short documentation for a given Emacs Lisp function or macro.

Argument SEXP is a symbolic expression (sexp) in Emacs Lisp that the function
will generate documentation for.

Argument RELATED-SEXPS is an optional argument that contains related definitions
that will be joined with the main SEXP for documentation generation."
  (pcase-let* ((`(,user-prompt . ,system-prompt)
                (gpt-doc-get-prompt-for-summary sexp related-sexps))
               (text
                (gpt-doc-response-text (gpt-doc-gpt-request
                                        user-prompt
                                        system-prompt)))
               (normalized (gpt-doc-normalize-response-content
                            text
                            sexp))
               (parts (split-string normalized nil t))
               (first-word (pop parts)))
    (when-let
        ((imp (and first-word
                   (boundp 'checkdoc-common-verbs-wrong-voice)
                   (cdr (assoc-string (downcase first-word)
                                      checkdoc-common-verbs-wrong-voice)))))
      (setq first-word (capitalize (seq-copy imp))))
    (concat first-word " " (string-join parts " "))))

(defun gpt-doc-document-sexp (sexp &optional related-sexps)
  "Join the first sentence and argument documentation of a SEXP.
Argument SEXP is the sexp (S-expression) that will be documented.
RELATED-SEXPS is additional definitions for context."
  (let* ((first-sentence (gpt-doc-get-short-documentation sexp
                                                          related-sexps))
         (args-docs (gpt-doc-document-arguments sexp
                                                related-sexps)))
    (gpt-doc-quote-args
     (string-join (delq nil (list first-sentence args-docs))
                  "\n\n"))))

(defun gpt-doc-get-all-buffer-definitions ()
  "Extract all buffer definitions from a given package."
  (let ((sexps)
        (package-name (progn
                        (require 'lisp-mnt)
                        (when (fboundp 'lm-get-package-name)
                          (when-let ((name (lm-get-package-name)))
                            (file-name-sans-extension name))))))
    (save-excursion
      (goto-char (point-max))
      (while (gpt-doc-move-with 'backward-sexp)
        (when-let ((sexp (sexp-at-point)))
          (when (and (proper-list-p sexp)
                     (symbolp (nth 1 sexp))
                     (string-prefix-p package-name (symbol-name (nth 1 sexp))))
            (push (cons (nth 1 sexp)
                        sexp)
                  sexps)))))
    sexps))

(defun gpt-doc-unquote-sym (exp)
  "Return the unquoted expression EXP.
Argument EXP is the expression to be unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun gpt-doc-flatten-vectors (items)
  "Flatten nested vectors and lists in the given input.

Argument ITEMS is a list or vector that needs to be flattened."
  (setq items (gpt-doc-unquote-sym items))
  (cond ((and items
              (proper-list-p items))
         (mapcar #'gpt-doc-flatten-vectors items))
        ((and items
              (vectorp items))
         (mapcar #'gpt-doc-flatten-vectors (append items nil)))
        (t items)))

(defun gpt-doc-inner-symbols (prefix-name sexp)
  "Return a list of inner symbols in SEXP that match PREFIX-NAME.
PREFIX-NAME is a string used to filter symbols by their name.
SEXP is an Emacs Lisp expression."
  (pcase-let* ((`(,_type ,_name . ,body) sexp)
               (inner (flatten-list (gpt-doc-flatten-vectors body))))
    (seq-filter
     (lambda (it)
       (and (symbolp it)
            (or (not prefix-name)
                (string-prefix-p prefix-name (symbol-name it)))))
     inner)))

(defun gpt-doc-join-sexps (sexps)
  "Join SEXPS into a string with newlines."
  (mapconcat
   (lambda (it)
     (or (ignore-errors (pp-to-string it))
         (prin1-to-string it)
         ""))
   sexps "\n\n"))

(defun gpt-doc-looks-like-keymapp (sexp)
  "Check if a given s-expression SEXP resembles a keymap definition.

Argument SEXP is a symbolic expression (sexp) that is being checked if it looks
like a keymap."
  (pcase sexp
    (`(defvar ,(and (pred symbolp) _name)
        (let ((,(and (pred symbolp) sym)
               (make-sparse-keymap)))
          . ,(and body (guard (eq sym (car (last body))))
                  (guard (seq-every-p (lambda (it)
                                        (memq (car-safe it)
                                              '(set-keymap-parent define-key
                                                                  lookup-key
                                                                  make-composed-keymap
                                                                  when if
                                                                  unless)))
                                      (butlast body)))))
        . ,_rest)
     t)
    (`(defvar-keymap ,(and (pred symbolp) _name)
        . ,rest)
     (let ((body (reverse rest))
           (kalist))
       (while
           (let* ((fn
                   (pop body))
                  (key (pop body)))
             (when (and
                    key
                    fn
                    (stringp key))
               (when (memq (car-safe fn) '(quote function))
                 (setq fn (gpt-doc-unquote-sym fn)))
               (push (cons key fn) kalist))))
       kalist))))

(defun gpt-doc-get-related-defs (sexp)
  "Extract related definitions from a given `S-expression'.

Argument SEXP is a symbolic expression (sexp) in Emacs Lisp."
  (pcase-let*
      ((package-name (progn
                       (require 'lisp-mnt)
                       (when (fboundp 'lm-get-package-name)
                         (when-let ((name (lm-get-package-name)))
                           (file-name-sans-extension name)))))
       (all-defs (gpt-doc-get-all-buffer-definitions))
       (filtered-defs (seq-remove (pcase-lambda (`(,_n . ,item))
                                    (or
                                     (not
                                      (memq (car-safe item)
                                            '(defvar defun cl-defun defcustom)))
                                     (gpt-doc-looks-like-keymapp item)))
                                  all-defs)))
    (let ((name)
          (items (delete-dups
                  (delq nil (gpt-doc-inner-symbols package-name sexp))))
          (processed-syms)
          (related-defs))
      (while (setq name (pop items))
        (unless (memq name processed-syms)
          (let* ((def (cdr (assq name filtered-defs)))
                 (new-syms (delete-dups (delq nil (gpt-doc-inner-symbols
                                                   package-name def)))))
            (when new-syms
              (setq items (delq nil (append items new-syms))))
            (unless (or (not def)
                        (memq def related-defs))
              (push def related-defs)))
          (push name processed-syms)))
      (seq-sort-by (lambda (it)
                     (let* ((full-def (assq (nth 1 it) filtered-defs))
                            (pos (seq-position filtered-defs full-def 'eq)))
                       (or pos 0)))
                   #'< related-defs))))

(defun gpt-doc-get-shallow-related-defs (sexp)
  "Extract related definitions from a given package and SEXP.

Parses the given SEXP and returns a list of related definitions.
The SEXP is parsed to extract symbols that are related to the package
name. The related definitions are then returned as a list, sorted by
their position in the original list of all definitions."
  (pcase-let*
      ((package-name (progn
                       (require 'lisp-mnt)
                       (when (fboundp 'lm-get-package-name)
                         (when-let ((name (lm-get-package-name)))
                           (file-name-sans-extension name)))))
       (all-defs (gpt-doc-get-all-buffer-definitions))
       (filtered-defs (seq-remove (pcase-lambda (`(,_n . ,item))
                                    (or
                                     (not
                                      (memq (car-safe item)
                                            '(defvar-local defvar defun cl-defun
                                                           defmacro defcustom)))
                                     (gpt-doc-looks-like-keymapp item)))
                                  all-defs)))
    (let ((name)
          (items (delete-dups
                  (delq nil (gpt-doc-inner-symbols package-name sexp))))
          (processed-syms)
          (related-defs))
      (while (setq name (pop items))
        (unless (memq name processed-syms)
          (let* ((def (cdr (assq name filtered-defs))))
            (unless (or (not def)
                        (memq def related-defs))
              (push def related-defs)))
          (push name processed-syms)))
      (seq-sort-by (lambda (it)
                     (let* ((full-def (assq (nth 1 it) filtered-defs))
                            (pos (seq-position filtered-defs full-def 'eq)))
                       (or pos 0)))
                   #'< related-defs))))

(defun gpt-doc-get-sexp-with-doc-pos ()
  "Extracts the sexp and its documentation position from the current function."
  (pcase-let*
      ((types
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
                 (and (memq (car-safe sexp)
                            types)
                      sexp))))
       (doc-pos))
    (save-excursion
      (when beg
        (goto-char beg))
      (down-list 1)
      (let ((count (cdr (assq (car sexp)
                              gpt-doc-docstring-positions))))
        (when (functionp count)
          (setq count (funcall count sexp)))
        (when count
          (gpt-doc-forward-sexp count)
          (setq doc-pos (point)))))
    (cons sexp doc-pos)))


;;;###autoload
(defun gpt-doc-document-current-function (&optional with-related-defs)
  "Generate documentation for the current defintion.

Optional argument WITH-RELATED-DEFS is a flag that determines whether related
definitions should be included in the documentation.

It can take three values: 1 (default), 4, or 16.

When it's 1, no related definitions are included.
When it's 4, shallow related definitions are included.
When it's 16, all related definitions are included."
  (interactive "p")
  (pcase-let*
      ((`(,sexp . ,doc-pos)
        (gpt-doc-get-sexp-with-doc-pos))
       (buff (current-buffer))
       (related-defs
        (and sexp
             doc-pos
             (pcase with-related-defs
               (1 nil)
               (4 (gpt-doc-get-shallow-related-defs sexp))
               (16 (gpt-doc-get-related-defs sexp)))))
       (text
        (gpt-doc-document-sexp sexp related-defs)))
    (when (and text doc-pos)
      (with-current-buffer buff
        (if buffer-read-only
            (message "%s" text)
          (save-excursion
            (goto-char doc-pos)
            (if (looking-back "\n" 0)
                (indent-according-to-mode)
              (newline-and-indent)
              (insert (prin1-to-string text))
              (forward-sexp -1)
              (forward-char 1))))))))

(provide 'gpt-doc)
;;; gpt-doc.el ends here