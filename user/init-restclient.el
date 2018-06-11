;;; init-restclient.el --- restclient-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This module re-implements restclient calls
;; using request.el package

;;; Code:

(eval-when-compile
  (require 'init-list)
  (require 'init-keybind))

(require 'rfc2231)
(require 'restclient)
(require 'request)


(defvar my:restclient-content-type-mode-alist
  '(("text/css" . css-mode)
    ("text/xml" . xml-mode)
    ("application/xml" . xml-mode)
    ("application/atom+xml" . xml-mode)
    ("application/atomcat+xml" . xml-mode)
    ("application/x-javascript" . js-mode)
    ("application/json" . js-mode)
    ("text/javascript" . js-mode)
    ("text/html" . html-mode)
    ("text/plain" . text-mode)
    ("image/gif" . image-mode)
    ("image/png" . image-mode)
    ("image/jpeg" . image-mode)
    ("image/x-icon" . image-mode)
    ("image/svg+xml" . image-mode))
  "Response Content-Type to Emacs major mode mapping.")


(defvar my:restclient-formatter-alist
  '((js-mode . my:restclient-json-formatter)
    (xml-mode . my:restclient-xml-formatter))
  "Response guessed mode to body formatting function mapping.")


(defun my:restclient-xml-formatter ()
  "Format current buffer as xml."
  (goto-char (point-min))
  (while (search-forward-regexp "\>[ \\t]*\<" nil t)
    (backward-char) (insert "\n"))
  (indent-region (point-min) (point-max)))


(defun my:restclient-json-formatter ()
  "Format current buffer as json."
  (let ((json-special-chars (remq (assoc ?/ json-special-chars)
                                  json-special-chars)))
    (ignore-errors (json-pretty-print-buffer)))
  (restclient-prettify-json-unicode))


(defun -my:restclient-http-do (method url headers entity &rest handle-args)
  "Perform METHOD http request to URL with HEADERS and ENTITY as request body.
Optional HANDLE-ARGS are passed to callback."
  (if restclient-log-request
      (message "HTTP %s %s Headers:[%s] Body:[%s]"
               method url headers entity))
  (setq restclient-within-call t)
  (setq restclient-request-time-start (current-time))
  (run-hooks 'restclient-http-do-hook)
  (request
   url
   :type method
   :headers headers
   :data entity
   :parser #'buffer-string
   :complete (lambda (&rest kwargs)
               (apply '-my:restclient-handle-response
                      kwargs
                      (if restclient-same-buffer-response
                          restclient-same-buffer-response-name
                        (format "*HTTP %s %s*" method url))
                      handle-args))))


(defun -my:restclient-handle-response (kwargs
                                       target-buffer
                                       &optional raw stay-in-window)
  "Callback for `request', which data is passed as KWARGS plist.
Results will be sent to TARGET-BUFFER. Do not run formatter
if RAW is non-nil. Do not switch windows focus if STAY-IN-WINDOW is non-nil."
  (setq restclient-within-call nil)
  (setq restclient-request-time-end (current-time))
  (run-hooks 'restclient-response-received-hook)
  (let* ((response (plist-get kwargs :response))
         (url (format "%s" (request-response-url response)))
         (settings (request-response-settings response))
         (data (or (plist-get kwargs :data) ""))
         (method (plist-get settings :type))
         (headers (request-response--raw-header response))
         (content-type
          (request-response-header response "content-type"))
         (duration (my:restclient-duration)))
    (with-current-buffer (get-buffer-create target-buffer)
      (erase-buffer)
      (let ((guessed-mode
             (-my:restclient-decode-response data content-type raw)))
        (when guessed-mode
          (-my:restclient-insert-footer
           method url headers duration guessed-mode)))

      (run-hooks 'restclient-response-loaded-hook)
      (if stay-in-window
          (display-buffer (current-buffer) t)
        (switch-to-buffer-other-window (current-buffer))))))


(defun my:image-type-from-mime (content-type)
  "Guess image type from CONTENT-TYPE."
  (let ((type (and (string-match "^[Ii]mage/\\(.*\\)$" content-type)
                   (match-string-no-properties 1 content-type))))
    (image-type-from-file-name (concat "." type))))



(defun my:conding-system-from-charset (charset)
  "Return coding-system symbol from CHARSET string."
  (or (and charset (intern (downcase charset))) 'utf-8))


(defun my:restclient-duration ()
  "Get last request time as float."
  (float-time (time-subtract restclient-request-time-end
                             restclient-request-time-start)))


(defun -my:restclient-insert-footer (method url headers duration mode)
  "Format and insert last request data."
  (unless (eq (point) (point-min))
    (insert "\n"))
  (let ((hstart (point)))
    (insert method " " url "\n" headers)
    (insert (format "Request duration: %fs\n" duration))
    (unless (member mode '(image-mode text-mode))
      (comment-region hstart (point)))))


(defun -my:restclient-decode-response (data content-type &optional noformat)
  "Decode DATA assuming CONTENT-TYPE and display it in `current-buffer'.
Skip formatter from `my:restclient-formatter-alist' if NOFORMAT is non-nil."
  (let* ((parsed-content-type
          (and content-type (rfc2231-parse-string content-type)))

         (type (car parsed-content-type))
         (coding-system (my:conding-system-from-charset
                         (cdr (assq 'charset (cdr parsed-content-type)))))
         (maybe-img-type (my:image-type-from-mime type))

         (guessed-mode (or
                        (assoc-default
                         type
                         my:restclient-content-type-mode-alist)))
         (formatter (and (not noformat) guessed-mode
                         (assoc-default guessed-mode
                                        my:restclient-formatter-alist))))

    (if (or (eq guessed-mode 'image-mode) maybe-img-type)
        (progn
          (fundamental-mode)
          (insert-image
           (create-image (string-make-unibyte data) maybe-img-type t))
          'image-mode)
      (progn
        (set-buffer-file-coding-system coding-system)
        (insert (decode-coding-string data coding-system))
        (when guessed-mode
          (apply guessed-mode '())
          (if (fboundp 'font-lock-flush)
              (font-lock-flush)
            (with-no-warnings
              (font-lock-fontify-buffer)))
          (when formatter
            (with-demoted-errors
                "Error while calling response formatter: %S"
              (funcall formatter)))
          (goto-char (point-max)))
        guessed-mode))))


(defun my:restclient-http-send-current (&optional raw stay-in-window)
  "Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t."
  (interactive)
  (restclient-http-parse-current-and-do #'-my:restclient-http-do
                                        raw stay-in-window))


(defun my:restclient-http-send-current-raw ()
  "Sends current request and get raw result.
No reformatting or syntax highlight of XML, JSON or images."
  (interactive)
  (my:restclient-http-send-current t))


(defun my:restclient-http-send-current-stay-in-window ()
  "Send current request and keep focus in request window."
  (interactive)
  (my:restclient-http-send-current nil t))


(my:kmap*
 restclient-mode-map
 ([remap restclient-http-send-current]
  #'my:restclient-http-send-current)
 ([remap restclient-http-send-current-raw]
  #'my:restclient-http-send-current-raw)
 ([remap restclient-http-send-current-stay-in-window]
  #'my:restclient-http-send-current-stay-in-window))


(provide 'init-restclient)

;;; init-restclient.el ends here
