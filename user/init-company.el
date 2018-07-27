;;; init-company.el --- company-mode customization  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module is to be loaded after both
;; ivy-mode and company-mode are loaded

;;; Code:
(require 'init-list)
(require 'company)
(require 'ivy)

(eval-when-compile
  (require 'cl))


(defun -my:ivy-make-company-transformer (candidates)
  "Create function customize CANDIDATES formatting during ivy completion.
Result is CANDIDATES as `company-candidates' with their annotations on a side."
  (let* ((format-template
          (format "%%-%ds %%s"
                  (my:mapreduce #'max #'string-width candidates 0))))
    (lambda (candidate)
      (format format-template candidate
              (let ((annotation
                     (truncate-string-to-width
                      (my:string-trim
                       (or (company-call-backend 'annotation candidate) ""))
                      100)))
                (put-text-property 0 (length annotation)
                                   'face font-lock-keyword-face
                                   annotation)
                annotation)))))


(defun -my:cleanup-company-backends ()
  "Remove unnecessary `company-mode' backends."
  (delq nil
        (let ((to-remove '(company-semantic company-files)))
          (mapcar
           (lambda (x) (unless (member x to-remove) x))
           company-backends))))


(defun -my:company-ivy-backends-format (backends)
  "Abbreviate names for grouped BACKENDS list using common prefix."
  (let ((backend-segments
         (mapcar (lambda (b) (split-string (symbol-name b) "-")) backends))
        (common-length 0))

    (while (my:mapreduce
            (lambda (acc x) (and (equal acc x) acc))
            #'car (cdr backend-segments) (caar backend-segments))
      (setq common-length (+ common-length (length (caar backend-segments)) 1)
            backend-segments (mapcar #'cdr backend-segments)))

    (concat (substring (symbol-name (car backends)) 0 common-length)
            (mapconcat (lambda (x) (mapconcat #'identity x "-"))
                       backend-segments "|"))))


(defun -my:company-ivy-frontend--show ()
  "Prepare `ivy-read' to complete  `company-candidates'."
  (let ((ivy--display-transformers-list
         (list 'my:ivy-company
               (-my:ivy-make-company-transformer company-candidates))))
    (ivy-read (if (symbolp company-backend)
                  (format "%s: " company-backend)
                (format "%s: "
                        (truncate-string-to-width
                         (-my:company-ivy-backends-format company-backend)
                         35 nil nil "...")))
              company-candidates
              :initial-input company-common
              :action #'company-finish
              :caller 'my:ivy-company)))


(defun my:company-ivy-frontend (command)
  "Display `company-candidates' using `ivy-read'.
For possible COMMAND values see `company-frontends'."
  (cl-case command
    (pre-command nil)
    (show (-my:company-ivy-frontend--show))
    (hide nil)
    (update (-my:company-ivy-frontend--show))
    (post-command (-my:company-ivy-frontend--show))))


(defun my:company-ivy-unless-just-one-frontend (command)
  "Display `company-candidates' using `ivy-read'.
For possible COMMAND values see `company-frontends'. Complete
immediately if only single candidate."
  (unless (company--show-inline-p)
    (my:company-ivy-frontend command)))


(setq-default company-tooltip-limit 20
              company-tooltip-minimum-width 50
              company-tooltip-align-annotations t
              company-require-match 'never
              company-idle-delay nil
              company-dabbrev-downcase nil
              company-dabbrev-ignore-case nil
              company-dabbrev-code-other-buffers t
              company-backends (-my:cleanup-company-backends))


(my:kmap* company-active-map
          ("C-p" #'company-select-previous)
          ("C-n" #'company-select-next)
          ("C-r" #'company-search-candidates)
          ("M-s" #'company-filter-candidates))

(my:kmap* company-search-map
          ("C-p" #'company-select-previous)
          ("C-n" #'company-select-next))


(provide 'init-company)

;;; init-company.el ends here
