;;; init-flymake-mypy.el --- Flymake mypy checker  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'init-defs)
(require 'python)
(require 'flymake)

(eval-when-compile
  (require 'cl-lib))


(defvar my:python-mypy-command
  (list
   "python" "-c"
   (concat "import sys, runpy; "
           "sys.path.pop(0); "
           "runpy.run_module('mypy', run_name='__main__')"))
  "Arguments to execute mypy python type checker.")


(defvar-local my:python-mypy-extra-args nil
  "Additional arguments to execute mypy python type checker.")


(defvar-local -my:flymake-mypy-proc nil
  "Mypy process for flymake")


(defconst -my:flymake-python-mypy-line-pattern
  ":\\(?:\\(?1:[0-9]+\\):\\)?\\(?:\\(?2:[0-9]+\\):\\)? \\(?3:[a-z]+\\): \\(?4:.*\\)"
  "Regex for parsing.")


(defun -my:flymake-python-mypy-parse-line (source-file)
  "Parse mypy line from current `point' of the SOURCE-FILE."
  (let ((bound (line-end-position)))
    (and (search-forward source-file bound :no-error 1)
         (search-forward-regexp -my:flymake-python-mypy-line-pattern
                                bound :no-error 1))))


(defun -my:maybe-string-to-number (string)
  "Convert STRING to number, return nil if argument is not a string."
  (when (stringp string) (string-to-number string)))


(defun -my:flymake-python-mypy-parse (output-buffer source-buffer source-file)
  "Parse mypy output from OUTPUT-BUFFER.
SOURCE-FILE is filename for SOURCE-BUFFER that were used to execute
mypy against."
  (with-current-buffer output-buffer
    (when (< 0 (- (point-max) (point-min)))
      (goto-char (point-min))
      (cl-loop
       while (-my:flymake-python-mypy-parse-line source-file)
       for msg = (match-string 4)
       for (beg . end) = (flymake-diag-region
                          source-buffer
                          (-my:maybe-string-to-number (match-string 1))
                          (-my:maybe-string-to-number (match-string 2)))
       for kind = (let ((k (match-string 3)))
                    (cond
                     ((eq k "note") :note)
                     ((eq k "warning") :warning)
                     ((eq k "error") :error)
                     (t :error)))
       collect (flymake-make-diagnostic
                source-buffer beg end kind msg)))))


(my:when-windows
  (defun -my:flymake-python-mypy-buffer-relative-path (buffer)
    "Find `buffer-file-name' of BUFFER relative to `default-directory'."
    (subst-char-in-string
     ?/ ?\\
     (file-relative-name (buffer-file-name buffer) default-directory))))


(my:when-posix
  (defun -my:flymake-python-mypy-buffer-relative-path (buffer)
    "Find `buffer-file-name' of BUFFER relative to `default-directory'."
    (file-relative-name (buffer-file-name buffer) default-directory)))


(defun -my:flymake-python-mypy-exec (report-fn &rest _args)
  "Execute mypy as Flymake checker.
REPORT-FN is Flymake's callback function."

  ;; Disable checker if not python available
  (unless (executable-find (car my:python-mypy-command))
    (error "Cannot find a suitable checker"))

  (let* ((source-buffer (current-buffer))
         (source-file
          (-my:flymake-python-mypy-buffer-relative-path source-buffer)))

    (if (not (file-exists-p source-file))
        ;; Short circuit, mypy requires source file to exist
        (funcall report-fn nil)

      ;; Kill process if already running
      (when (and -my:flymake-mypy-proc
                 (process-live-p -my:flymake-mypy-proc))
        (kill-process -my:flymake-mypy-proc))

      ;; Make temp file
      (let ((temp-file (make-temp-file "flymake-mypy")))
        (save-restriction
          ;; Cancel buffer narrowing
          (widen)
          ;; Populate temp file
          (write-region (point-min) (point-max) temp-file nil 'nomessage))

        (let ((output-buffer (generate-new-buffer " *python-mypy*")))
          (setq -my:flymake-mypy-proc
                (make-process
                 :name "python-mypy"
                 :buffer output-buffer
                 :command
                 (append my:python-mypy-command
                         my:python-mypy-extra-args
                         (list "--show-column-numbers"
                               "--shadow-file" temp-file source-file
                               source-file))
                 :sentinel
                 (lambda (proc _event)
                   (when (eq (process-status proc) 'exit)
                     (unwind-protect
                         (if (not (and (buffer-live-p source-buffer)
                                       (eq proc
                                           (with-current-buffer source-buffer
                                             -my:flymake-mypy-proc))))
                             (flymake-log :warning
                                          "mypy process %s obsolete" proc)
                           (funcall report-fn
                                    (-my:flymake-python-mypy-parse
                                     output-buffer
                                     source-buffer
                                     source-file)))
                       (ignore-errors (delete-file temp-file))
                       (kill-buffer output-buffer))))
                 :noquery t
                 :connection-type 'pipe)))))))


(defun my:flymake-python-mypy (report-fn &rest args)
  "Execute mypy as `flymake' checker in python environment.
REPORT-FN is flymake report function. ARGS is flymake parameters."
  (python-shell-with-environment
    (apply #'-my:flymake-python-mypy-exec report-fn args)))


(defvar-local my:flymake-mypy-mode nil
  "Enables Mypy Flymake checker.")


;;;###autoload
(define-minor-mode my:flymake-mypy-mode
  "Enables flymake mypy checker."
  :variable my:flymake-mypy-mode
  (if (not my:flymake-mypy-mode)
      (remove-hook 'flymake-diagnostic-functions #'my:flymake-python-mypy t)
    (add-hook 'flymake-diagnostic-functions #'my:flymake-python-mypy nil t)))


(provide 'init-flymake-mypy)

;;; init-flymake-mypy.el ends here
