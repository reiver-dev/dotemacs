;;; init-environ.el --- Environment variables  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'init-powershell)
(require 'init-list)


(defconst my:env-split-regex-simple "^\\(.*?\\)=\\(.*\\)$")
(defconst my:env-split-regex-export "^export \\(.*?\\)=\\(.*\\)$")


(defun my:env-valid-line-p (string)
  "Check if STRING is not empty and is not a comment line."
  (and (stringp string)
       (> (length string) 0)
       (not (eq (aref string 0) ?#))))


(defun my:env-split-entry (entry &optional expr)
  "Return cons pair (name . var) from ENTRY string.
Entry string should be 'NAME=VALUE'. Optional EXPR regex is used
instead of default `my:env-split-regex'."
  (when (and (my:env-valid-line-p entry)
             (string-match (or expr my:env-split-regex-simple) entry))
    (let ((key (match-string 1 entry))
          (value (match-string 2 entry)))
      (when key
        (cons key value)))))


(defun my:env-parse-entries (env &optional expr)
  "Collect list of cons pairs (name . var) from ENV list.
Each entry in ENV list should be 'NAME=VALUE'. Optional EXPR regular
expression string will be matched, it must return group 1 for env var
name and group 2 for value."
  (let ((expr (if (and expr (stringp expr))
                  expr
                my:env-split-regex-simple))
        result)
    (dolist (line (if (stringp env) (split-string env "\n" t) env))
      (let ((pair (my:env-split-entry line expr)))
        (when pair
          (setq result (cons pair result)))))
    (reverse result)))


(defun my:env-apply-entries (pairs)
  "Apply cons PAIRS (name . value t) to current `process-environment'.
See `setenv'."
  (dolist (pair pairs)
    (setenv (car pair) (cdr pair))))


(defun my:env-w32-vs-installation-path (version)
  "Find Visual Studio installation location for VERSION.
Try to use vswhere if available."
  (let* ((program-files (or (getenv "ProgramFiles(x86)")
                            (getenv "ProgramFiles")))
         (vswhere (expand-file-name
                   "Microsoft Visual Studio/Installer/vswhere.exe"
                   program-files)))
    (when (file-executable-p vswhere)
      (let ((version-range (format "[%d.0, %d.0)" version (+ version 1))))
        (my:string-trim
         (with-output-to-string
           (call-process vswhere nil standard-output nil
                         "-version" version-range
                         "-products" "*"
                         "-legacy"
                         "-property" "installationPath")))))))


(defun my:env-w32-vcvars-location (version)
  "Find 'vcvarsall.bat' path for specified VERSION string."
  (let ((vspath
         (or (my:env-w32-vs-installation-path version)
             (let ((comntools (getenv (format "VS%d0COMNTOOLS" version))))
               (when comntools
                 (expand-file-name "../../" comntools))))))
    (cond
     ((or (not vspath) (string= "" vspath)) nil)
     ((>= version 15) (expand-file-name
                       "VC/Auxiliary/Build/vcvarsall.bat"
                       vspath))
     (t (expand-file-name "VC/vcvarsall.bat" vspath)))))


(defun my:env-w32-vcvars-collect (version &rest args)
  "Try to get env variables from visual studio vcvarsall.bat.
One should specify VERSION (12 | 13 | 14 | 15) that will be substituted
as env var name like VS140COMNTOOLS.
ARGS specify additional batch file arguments such as architecture (i.e. x64)"
  (let* ((vcvarsall (my:env-w32-vcvars-location version))
         (command0 (combine-and-quote-strings (cons vcvarsall args)))
         (command1 (format "%s > nul 2>&1 && set" command0)))
    (split-string (shell-command-to-string command1) "\n" t)))


(defun my:env-w32-vcvars-apply (version &rest argv)
  "Apply vcvarsall.bat variables to currentb `process-environment'.
VERSION and ARGV definition are same as for `my:env-w32-collect-vcvars'"
  (my:env-apply-entries
   (my:env-parse-entries
    (apply #'my:env-w32-vcvars-collect version argv))))


(defun my:env-w32-get-registry-values ()
  "Gather current windows registry environment variables.
Execute `my:powershell-env-from-registry' using powershell."
  (my:poweshell-exec-command my:powershell-env-from-registry))


(defun my:env-w32-refresh ()
  "Apply current windows registry environment variables."
  (interactive)
  (my:env-apply-entries
   (my:env-parse-entries
    (my:env-w32-get-registry-values))))


(defun my:env-make-local ()
  "Create buffer-local copy of `process-environment'."
  (interactive)
  (let ((value (copy-sequence process-environment)))
    (set (make-local-variable 'process-environment) value)))

(defun my:env-make-exec-path-local ()
  "Create buffer-local copy of `process-environment'."
  (interactive)
  (let ((value (copy-sequence exec-path)))
    (set (make-local-variable 'exec-path) value)))


(defun my:env-restore-initial ()
  "Replace current `process-environment' with copy of `initial-environment'."
  (interactive)
  (setq process-environment (copy-sequence initial-environment)))


(defun my:env-path-split (value)
  "Split VALUE containing list of paths using `path-separator'."
  (split-string value path-separator t "\\s-"))

(defun my:env-path-get ()
  "Get $PATH entries as list."
  (my:env-path-split (getenv "PATH")))

(defun my:env-path-set (paths)
  "Join list of filesyste PATHS and set it to `process-environment'."
  (setenv "PATH" (mapconcat #'identity (delete-dups paths) path-separator)))

(defun my:env-path-prepend (&rest paths)
  "Add PATHS values to environment variable $PATH."
  (my:env-path-set (append paths (my:env-path-get))))

(defun my:env-path-append (&rest paths)
  "Add PATHS values to environment variable $PATH."
  (my:env-path-set (append (my:env-path-get) paths)))

(defun my:env-exec-path-prepend (&rest paths)
  "Add PATHS to `exec-path'."
  (setq exec-path (delete-dups (append paths exec-path))))

(defun my:env-exec-path-append (&rest paths)
  "Add PATHS to `exec-path'."
  (setq exec-path (delete-dups (append exec-path paths))))

(defun my:env-exec-path-update (&optional path force)
  "Reset `exec-path' from PATH value using one from `process-environment'.
If FORCE non-nil, current `exec-path' value will be discarded."
  (let ((path-entries (my:env-path-split (or path (getenv "PATH")))))
    (setq exec-path
          (delete-dups (if force
                           (append path-entries (list "." exec-directory))
                         (append path-entries exec-path))))))


(provide 'init-environ)


;;; init-environ.el ends here
