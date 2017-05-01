;;; init.el --- Startup file  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(prefer-coding-system 'utf-8-unix)


(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("marmelade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/")))


(defun package--compile---no-safe (proc &rest args)
  "Ignore unsaved files during package install."
  (let ((old (symbol-function 'save-some-buffers)))
    (unwind-protect
        (progn (fset 'save-some-buffers 'ignore)
               (apply proc args))
      (fset 'save-some-buffers old))))


(advice-add 'package--compile :around
            #'package--compile---no-safe)


(package-initialize)

(defun -in-dir (name &optional root)
  "Convert name to absolute path.
NAME - directory name
ROOT - relative path, user directory as default"
  (let ((-root (or root user-emacs-directory)))
    (file-name-as-directory
     (expand-file-name name -root))))

(defconst init:user-modules-dir (-in-dir "user"))
(defconst init:load-path-dir (-in-dir "load-path"))
(defconst init:themes-dir (-in-dir "themes"))
(defconst init:recovery-dir (-in-dir "recovery"))
(defconst init:backup-dir (-in-dir "backup" init:recovery-dir))
(defconst init:auto-save-list-dir (-in-dir "auto-save-list" init:recovery-dir))
(defconst init:auto-save-dir (-in-dir "auto-save" init:recovery-dir))

(defconst init:auto-dirs (list init:user-modules-dir
                               init:load-path-dir
                               init:themes-dir
                               init:recovery-dir
                               init:backup-dir
                               init:auto-save-list-dir
                               init:auto-save-dir))

;; Create directories
(dolist (dir init:auto-dirs)
  (unless (file-directory-p dir)
    (make-directory dir)))

;; Load path for additional modules
(dolist (default-directory
          (list init:user-modules-dir
                init:load-path-dir))
  (normal-top-level-add-to-load-path (list "."))
  (normal-top-level-add-subdirs-to-load-path))

;; Themes directory
(setq custom-theme-directory init:themes-dir)

;; Backup, autosave, lockfiles
(setq backup-directory-alist `((".*" . ,init:backup-dir))
      auto-save-list-file-prefix (concat init:auto-save-list-dir "saves-")
      auto-save-file-name-transforms `((".*" ,init:auto-save-dir t))
      create-lockfiles nil)

;; Custom and current config
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Persistent common configuration
(load custom-file t)
(require 'init-main)

(provide 'init-el)

;;; init.el ends here
