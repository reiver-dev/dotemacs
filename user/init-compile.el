;;; init-compile.el --- Eshell config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-keybind)


(setq-default
 compilation-always-kill t             ;; kill process before starting new one
 compilation-ask-about-save nil        ;; save everything
 compilation-skip-threshold 2)         ;; skip warnings


(defun my:compile (&optional comint)
  "Compile without confirmation.
If used with a COMINT argument, use `comint-mode'."
  (interactive "P")
  ;; Do the command without a prompt.
  (save-window-excursion
    (compile (eval compile-command) (and comint t)))
  ;; Create a compile window of the desired width.
  (pop-to-buffer (get-buffer "*compilation*"))
  (enlarge-window (- (frame-width) 105 (window-width)) 'horizontal))


(autoload 'my:compile "compile")


(my:kmap* prog-mode-map
          ("<f8>" #'my:compile)
          ("C-<f8>" #'compile))


(defun -my:compile-mock-fringes (&optional _window)
  "Always return list of (-1 -1 nil).
This is to override `window-fringes.'"
  (list -1 -1 nil))


(defun -my:compile-set-window-nojump (proc &rest args)
  "Call PROC with ARGS while wrapping `window-fringes'."
  (let ((old (symbol-function 'window-fringes)))
    (unwind-protect
        (progn (fset 'window-fringes #'-my:compile-mock-fringes)
               (apply proc args))
      (fset 'window-fringes old))))


(defun -my:find-file-noselect-maybe-literally (proc &rest args)
  "Force file opened literally if already opened and requested otherwise.
Wraps PROC which is meant to be `find-file-noselect' called with ARGS
arguments."
  (condition-case err
      (apply proc args)
    (error (progn
             (let* ((sym (car err))
                    (msg (car (cdr err)))
                    (filename (car args))
                    (nowarn (car (setq args (cdr args))))
                    (_rawfile (car (setq args (cdr args))))
                    (wildcards (car (setq args (cdr args)))))
               (cond
                ((string-equal msg "File already visited literally")
                 (funcall proc filename nowarn t wildcards))
                ((string-equal msg "File already visited normally")
                 (funcall proc filename nowarn nil wildcards))
                (t (progn
                     (signal sym msg)))))))))

(advice-add 'find-file-noselect :around
            #'-my:find-file-noselect-maybe-literally)

(my:after compile
  (my:kmap* compilation-shell-minor-mode-map
            ("<f8>" "<C-<f8>" #'recompile))
  (advice-add 'compilation-goto-locus :around
              #'-my:compile-set-window-nojump))


(provide 'init-compile)

;;; init-compile.el ends here
