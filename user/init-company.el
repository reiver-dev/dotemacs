;;; init-company.el --- company-mode customization  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module is to be loaded after both
;; ivy-mode and company-mode are loaded

;;; Code:
(require 'init-list)
(require 'company)
(require 'ivy)


(defun -my:company-candidate-get (candidate)
  (let ((annotation (company-call-backend 'annotation candidate))
        (meta (company-call-backend 'meta candidate)))
    (list candidate (or annotation "") (or meta ""))))


(defun -my:str-set-face (str start end face)
  (when (and str face (< start end))
    (put-text-property start end 'face face str)))


(defun -my:length-to-stops (numbers offset)
  (let ((acc  0))
    (mapcar (lambda (x)
              (setq acc (+ acc x (if (eql acc 0) 0 offset))))
            numbers)))


(defun -my:stops-to-ranges (stops offset)
  (let ((begin (cons 0 (mapcar (lambda (x) (+ x offset)) stops)))
        (end (mapcar #'identity stops)))
    (my:mapcar-zip #'cons begin end)))


(defun -my:entry-face (string dims faces)
  (my:mapc-zip
   (lambda (range face)
     (-my:str-set-face string (car range) (cdr range) face))
   dims faces))


(defun -my:counsel-company-candidates (candidates)
  (let* ((items (mapcar #'-my:company-candidate-get candidates))
         (separator " | ")
         (lengths (my:reduce-cols #'max #'length items 0))
         (dims (-my:length-to-stops lengths (length separator)))
         (face-ranges (-my:stops-to-ranges dims (length separator)))
         (faces '(nil compilation-info nil))
         (format-tpl (mapconcat
                      (lambda (x)
                        (format "%%-%ds" x)) lengths separator))
         (result (mapcar (lambda (candidate)
                           (cons (apply 'format format-tpl candidate)
                                 (car candidate)))
                         items)))
    (mapc (lambda (candidate)
            (-my:entry-face (car candidate) face-ranges faces))
          result)
    result))


(defun my:counsel-company-finish (candidate)
  (company-finish (cdr candidate)))


(defun my:counsel-company-maybe-abort ()
  (unless (eq ivy-exit 'done)
    (company-abort)))


(defun my:counsel-company (&optional backend)
  (interactive)
  (when (if backend
            (company-begin-backend backend)
          (company-manual-begin))
    (if (cdr company-candidates)
        (ivy-read (format "%s: " company-backend)
                  (-my:counsel-company-candidates company-candidates)
                  :initial-input company-common
                  :action #'my:counsel-company-finish
                  :unwind #'my:counsel-company-maybe-abort
                  :caller 'my:counsel-company)
      (company-finish (car company-candidates)))))


(defun -my:cleanup-company-backends ()
  (delq nil
        (let ((to-remove '(company-semantic company-files)))
          (mapcar
           (lambda (x) (unless (member x to-remove) x))
           company-backends))))


(setq-default company-tooltip-limit 20
              company-tooltip-align-annotations t
              company-require-match 'never
              company-idle-delay nil
              company-dabbrev-downcase nil
              company-dabbrev-ignore-case nil
              company-dabbrev-code-other-buffers t
              ;; company-frontends '(company-preview-if-just-one-frontend)
              company-backends (-my:cleanup-company-backends))


(my:kmap* company-active-map
          ("C-p" #'company-select-previous)
          ("C-n" #'company-select-next))


(provide 'init-company)

;;; init-company.el ends here
