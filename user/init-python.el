;;; init-python.el --- Python configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-completion)

(eval-when-compile
  (require 'python))

;; Replace python.el version to support venv on windows

(defconst -my:python-venv-bin-list
  (if (eq system-type 'windows-nt)
      (list "Scripts" ".")
    (list "bin")))


(setq-default python-shell-completion-native-enable
              (not (eq system-type 'windows-nt)))


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
         (mapcar (lambda (x)
                   (expand-file-name x python-shell-virtualenv-root))
                 -my:python-venv-bin-list))
        new-path)))


(my:after 'python
  (fset 'python-shell-switch-to-shell
        '-my:python-shell-switch-to-shell)
  (fset 'python-shell-calculate-exec-path
        '-my:python-shell-calculate-exec-path))


(my:with-package anaconda-mode
  :ensure t
  :init (add-hook 'python-mode-hook #'anaconda-mode)
  :config (progn
            (my:kmap* anaconda-mode-map
                      ("M-*" "M-," "M-." "C-M-i" nil)
                      ("M-." #'anaconda-mode-find-definitions)
                      ("M-," #'anaconda-mode-go-back)
                      ("C-M-." #'anaconda-mode-find-assignments)
                      ("M-]" #'anaconda-mode-find-references)
                      ([remap completion-at-point] #'anaconda-mode-complete))))


(my:with-package company-anaconda
  :ensure t
  :init (my:after 'anaconda-mode
          (add-to-list 'company-backends #'company-anaconda)))

(my:with-package python-environment
  :disabled t
  :ensure t
  :config (progn
            (setq python-environment-virtualenv
                  (append (list "python" "-m") python-environment-virtualenv))))

(my:with-package jedi-core
  :disabled t
  :ensure t
  :init (add-hook 'python-mode-hook 'jedi:setup)
  :config (my:kmap* jedi-mode-map
                    ("M-." #'jedi:goto-definition)
                    ("M-," #'jedi:goto-definition-pop-marker)))

(my:with-package company-jedi
  :disabled t
  :ensure t
  :init (my:after 'jedi-core
          (add-to-list 'company-backends #'company-jedi)))


(provide 'init-python)

;;; init-python.el ends here
