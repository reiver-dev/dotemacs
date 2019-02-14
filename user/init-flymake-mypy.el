;;; init-flymake-mypy.el --- Flymake mypy checker  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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


(defun -my:flymake-python-mypy-parse-line (source-file)
  "Parse mypy line from current `point' of the SOURCE-FILE."
  (and (search-forward source-file (line-end-position) t 1)
       (search-forward-regexp
        ":\\(?1:[0-9]+\\):\\(?2:[0-9]+\\): \\(?3:[a-z]+\\): \\(?4:.*\\)")))


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
                          (string-to-number (match-string 1))
                          (string-to-number (match-string 2)))
       for kind = (let ((k (match-string 3)))
                    (cond
                     ((eq k "note") :note)
                     ((eq k "warning") :warning)
                     ((eq k "error") :error)
                     (t :error)))
       collect (flymake-make-diagnostic
                source-buffer beg end kind msg)))))


(defun -my:flymake-python-mypy-exec (report-fn &rest _args)
  "Execute mypy as Flymake checker.
REPORT-FN is Flymake's callback function."

  ;; Disable checker if not python available
  (unless (executable-find (car my:python-mypy-command))
    (error "Cannot find a suitable checker"))

  ;; Kill process if already running
  (when (and -my:flymake-mypy-proc
             (process-live-p -my:flymake-mypy-proc))
    (kill-process -my:flymake-mypy-proc))

  ;; Make temp file
  (let* ((temp-file (make-temp-file "flymake-mypy"))
         (source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer)))

    ;; Cancel buffer narrowing
    ;; Populate temp file
    (save-restriction
      (widen)
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
                         (flymake-log :warning "mypy process %s obsolete" proc)
                       (funcall report-fn
                                (-my:flymake-python-mypy-parse
                                 output-buffer
                                 source-buffer
                                 (file-name-nondirectory source-file))))
                   (ignore-errors (delete-file temp-file))
                   (kill-buffer output-buffer))))
             :noquery t
             :connection-type 'pipe)))))


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
