;;; init-company.el --- company-mode customization  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module is to be loaded after both
;; ivy-mode and company-mode are loaded

;;; Code:
(require 'init-list)
(require 'company)
(require 'ivy)


(defun my:ivy-company-maybe-abort ()
  "Call `company-abort' if `ivy-exit' is not 'done."
  (unless (eq ivy-exit 'done)
    (company-abort)))


(defun -my:ivy-make-company-transformer (candidates)
  "Create function customize CANDIDATES formatting during ivy completion.
Result is CANDIDATES as `company-candidates' with their annotations on a side."
  (let* ((format-template
          (format "%%-%ds %%s"
                  (my:mapreduce #'max #'string-width candidates 0))))
    (lambda (candidate)
      (format format-template candidate
              (let ((annotation
                     (my:string-trim
                      (or (company-call-backend 'annotation candidate) ""))))
                (put-text-property 0 (length annotation)
                                   'face font-lock-keyword-face
                                   annotation)
                annotation)))))


(defun my:ivy-company (&optional backend)
  "Do `company-mode' completion using `ivy'.
Can specify company BACKEND to use."
  (interactive)
  (when (if backend
            (company-begin-backend backend)
          (company-manual-begin))
    (if (cdr company-candidates)
        (let ((ivy--display-transformers-list
               (list 'my:ivy-company
                     (-my:ivy-make-company-transformer company-candidates))))
          (ivy-read (format "%s: " company-backend)
                    company-candidates
                    :initial-input company-common
                    :action #'company-finish
                    :unwind #'my:ivy-company-maybe-abort
                    :caller 'my:ivy-company))
      (company-finish (car company-candidates)))))


(defun -my:cleanup-company-backends ()
  "Remove unnecessary `company-mode' backends."
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
              company-frontends '(company-preview-if-just-one-frontend)
              company-backends (-my:cleanup-company-backends))


(my:kmap* company-active-map
          ("C-p" #'company-select-previous)
          ("C-n" #'company-select-next))


(provide 'init-company)

;;; init-company.el ends here
