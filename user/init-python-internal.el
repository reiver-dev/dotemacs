;;; -*- lexical-binding: t -*-
;;; init-python-internal.el --- Python mode tweaks

;;; Code:

(require 'init-defs)
(require 'init-package)
(require 'python)


(defconst -my:python-venv-bin-list
  (if (eq system-type 'windows-nt)
      (list "Scripts" ".")
    (list "bin")))


(defun -my:python-venv-root-expand (path)
  "Expand PATH relative to `python-shell-virtualenv-root'."
  (expand-file-name path python-shell-virtualenv-root))


;; Consider visible frames
(defun -my:python-shell-switch-to-shell (&optional msg)
  "Switch to inferior Python process buffer.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive "p")
  (pop-to-buffer
   (process-buffer (python-shell-get-process-or-error msg))
   '(display-buffer-reuse-window (reusable-frames . visible)) t))


;; Consider Scripts folder on windows
(defun -my:python-shell-calculate-exec-path ()
    "Calculate `exec-path'.
Prepends `python-shell-exec-path' and adds the binary directory
for virtualenv if `python-shell-virtualenv-root' is set.  If
`default-directory' points to a remote host, the returned value
appends `python-shell-remote-exec-path' instead of `exec-path'."
    (let ((new-path (copy-sequence
                     (if (file-remote-p default-directory)
                         python-shell-remote-exec-path
                       exec-path))))
      (python-shell--add-to-path-with-priority
       new-path python-shell-exec-path)
      (if (not python-shell-virtualenv-root)
          new-path
        (python-shell--add-to-path-with-priority
         new-path
         (mapcar #'-my:python-venv-root-expand
                 -my:python-venv-bin-list))
        new-path)))


(my:after 'python
  (fset 'python-shell-switch-to-shell
        '-my:python-shell-switch-to-shell)
  (fset 'python-shell-calculate-exec-path
        '-my:python-shell-calculate-exec-path))


;;;###autoload
(defun my:python-executable ()
  "Find python executable considering virtual env.
Executable name is `python-shell-virtualenv-root' and virtual env path
is `python-shell-virtualenv-root'."
  (python-shell-with-environment
    (executable-find python-shell-interpreter)))


(defvar flycheck-python-flake8-executable)
(defvar flycheck-python-pylint-executable)
(defvar flycheck-python-pycompile-executable)
(defvar flycheck-python-mypy-executable)


;;;###autoload
(defun my:flycheck-python-setup ()
  "Setup python executable for flycheck python checkers."
  (interactive)
  (with-demoted-errors "Error in flycheck-python-setup: %S"
    (let* ((executables
            (python-shell-with-environment
              (list
               :pyexe (or (executable-find python-shell-interpreter) "python")
               :mypy (or (executable-find "mypy") "mypy"))))
           (pyexe (plist-get executables :pyexe))
           (mypy (plist-get executables :mypy)))
      (setq-local flycheck-python-flake8-executable pyexe)
      (setq-local flycheck-python-pylint-executable pyexe)
      (setq-local flycheck-python-pycompile-executable pyexe)
      (setq-local flycheck-python-mypy-executable mypy)
      python-shell-virtualenv-root)))


(autoload 'my:flycheck-python-setup "flycheck")


(my:when-windows
  (my:after 'anaconda-mode
    (defun -my:anaconda-mode-bootstrap (&optional callback)
      "Run `anaconda-mode' server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."
      (setq anaconda-mode-process
            (pythonic-start-process
             :process anaconda-mode-process-name
             :buffer (get-buffer-create anaconda-mode-process-buffer)
             :query-on-exit nil
             :filter (lambda (process output)
                       (anaconda-mode-bootstrap-filter process output callback))
             :sentinel (lambda (_process _event))
             :args `("-c"
                     ,anaconda-mode-server-command
                     ,(anaconda-mode-server-directory)
                     ,(if (pythonic-remote-p)
                          "0.0.0.0"
                        anaconda-mode-localhost-address)
                     ,(if python-shell-virtualenv-root
                          (my:python-executable)
                        ""))))

      (let ((params `((interpreter . ,python-shell-interpreter)
                      (virtualenv . ,python-shell-virtualenv-root)
                      (port . ,nil))))
        (when (pythonic-remote-p)
          (setq params (append params
                               `((remote-p . t)
                                 (remote-method . ,(pythonic-remote-method))
                                 (remote-user . ,(pythonic-remote-user))
                                 (remote-host . ,(pythonic-remote-host))
                                 (remote-port . ,(pythonic-remote-port))))))
        (dolist (kv params)
          (process-put anaconda-mode-process (car kv) (cdr kv))))))
  (my:after' anaconda-mode
             (fset 'anaconda-mode-bootstrap '-my:anaconda-mode-bootstrap)))


(provide 'init-python-internal)

;;; init-python-internal.el
