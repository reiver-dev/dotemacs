;; -*- lexical-binding: t; -*-


(package-initialize)

(defun -in-dir (name &optional root)
  (let ((-root (or root user-emacs-directory)))
    (file-name-as-directory
     (expand-file-name name -root))))

(defconst init:load-path-dir (-in-dir "load-path"))
(defconst init:themes-dir (-in-dir "themes"))
(defconst init:recovery-dir (-in-dir "recovery"))
(defconst init:backup-dir (-in-dir "backup" init:recovery-dir))
(defconst init:auto-save-list-dir (-in-dir "auto-save-list" init:recovery-dir))
(defconst init:auto-save-dir (-in-dir "auto-save" init:recovery-dir))

(defconst init:auto-dirs (list init:load-path-dir
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
(let ((default-directory init:load-path-dir))
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
(load custom-file t)

;; Persistent common configuration
(load (concat user-emacs-directory "user/init-impl"))
