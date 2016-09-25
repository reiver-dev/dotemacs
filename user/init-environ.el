;;; init-cc.el --- Environment variables  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defconst my:msvc-vars
  '("PATH" "INCLUDE" "LIB" "LIBPATH"
    "CL" "_CL_" "LINK" "_LINK_"))


(defun my:env-split-entry (entry)
  "Return cons pair (name . var) from ENTRY string.
Entry string should be 'NAME=VALUE'"
  (when (string-match "^\\(.*?\\)=\\(.*\\)" entry)
    (let ((name (match-string 1 entry))
          (value (match-string 2 entry)))
      (when value (cons name value)))))


(defun my:env-parse-entries (env &optional varnames)
  "Collect list of cons pairs (name . var) from ENV list.
Each entry in ENV list should be 'NAME=VALUE'.
If VARNAMES list is specified, filter only those variables."
  (delete nil (if varnames
                  (mapcar
                   (lambda (name)
                     (let ((val (getenv-internal name env)))
                       (when val (cons name val))))
                   varnames)
                (mapcar #'my:env-split-entry env))))


(defun my:env-apply-entries (pairs)
  "Apply cons PAIRS (name . value t) to current `process-environment'.
See `setenv'."
  (mapc (lambda (pair)
          (setenv (car pair) (cdr pair)))
        pairs))


(defun my:env-w32-collect-vcvars (version &rest args)
  "Try to get env variables from visual studio vcvarsall.bat.
One should specify VERSION (90 | 120 | 130 | 140) that will be substituted
as env var name like VS140COMNTOOLS.
ARGS specify additional batch file arguments such as architecture (i.e. x64)"
  (let* ((vcvarsall
          (expand-file-name
           "vcvarsall.bat"
           (concat (getenv (format "VS%sCOMNTOOLS" version)) "/../../VC")))
         (command0 (combine-and-quote-strings (cons vcvarsall args)))
         (command1 (format "%s > nul 2>&1 && set" command0)))
    (split-string (shell-command-to-string command1) "\n" t)))



(defun my:env-w32-apply-vcvarsall (version &rest argv)
  "Apply vcvarsall.bat variables to currentb `process-environment'.
VERSION and ARGV definition are same as for `my:env-w32-collect-vcvars'"
  (my:env-apply-entries
   (my:env-parse-entries (apply #'my:env-w32-collect-vcvars version argv)
                         my:msvc-vars)))


(defconst my:env-w32-choco-refresh
  "refreshenv > $null; ls Env: | %{ echo $($_.Name,$_.Value -join \"=\") }")


(defun my:env-w32-get-registry-values ()
  "Gather current windows registry environment variables.
Uses 'Update-SessionEnvironment.ps1' script from Chocolatey.
See `my:env-w32-choco-refresh'"
  (with-output-to-string
    (call-process "powershell" my:env-w32-choco-refresh
                  standard-output nil "-")))


(defun my:env-w32-refresh ()
  "Apply current windows registry environment variables."
  (interactive)
  (my:env-apply-entries (my:env-parse-entries (my:env-w32-get-registry-values))))


(defun my:env-get-copy ()
  "Return unrelated copy of `process-environment'."
  (mapcar #'concat process-environment))


(defun my:env-make-local ()
  "Create buffer-local copy of `process-environment'."
  (interactive)
  (set (make-local-variable 'process-environment)
       (my:env-get-copy)))


(defun my:env-restore-initial ()
  "Replace current `process-environment' with copy of `initial-environment'."
  (interactive)
  (setq process-environment (mapcar #'concat initial-environment)))


(defun my:add-to-path (&rest paths)
  "Add PATHS values to `exec-path' and environment variable $PATH."
  (let ((env-path (getenv "PATH"))
        (value (mapconcat #'identity paths path-separator)))
    (setenv "PATH" (concat env-path path-separator value)))
  (setq exec-path (append exec-path paths)))


(defun my:add-to-path-front (&rest paths)
  "Add PATHS values to `exec-path' and environment variable $PATH."
  (let ((env-path (getenv "PATH"))
        (value (mapconcat #'identity paths path-separator)))
    (setenv "PATH" (concat value path-separator env-path)))
  (setq exec-path (nconc paths exec-path)))



(provide 'init-environ)


;;; init-environ.el ends here
