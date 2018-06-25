;;; init-base.el --- fundamental defines  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default locale-coding-system 'utf-8
              buffer-file-coding-system 'utf-8)


(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'toolltip-mode)
  (tooltip-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/")))


(defun package--compile---no-save (proc &rest args)
  "Advice to ignore unsaved files during package install.
Expects PROC to be `package--compile' with all ARGS used in it's call."
  (let ((old (symbol-function 'save-some-buffers)))
    (unwind-protect
        (progn (fset 'save-some-buffers 'ignore)
               (apply proc args))
      (fset 'save-some-buffers old))))


(advice-add 'package--compile :around
            #'package--compile---no-save)


(defun init:in-dir (name &optional root asfile)
  "Convert file NAME to absolute path relative to ROOT directory.
If ROOT is nil uses `user-emacs-directory' instead. If ASFILE is not nil
returns result as filename, uses `file-name-as-directory' otherwise."
  (let ((dest (expand-file-name name (or root user-emacs-directory))))
    (if asfile
        dest
      (file-name-as-directory dest))))

(defconst init:emacs (expand-file-name invocation-name invocation-directory)
  "Current emacs executable")
(defconst init:user-modules-dir (init:in-dir "user")
  "Directory for the most of init code.")
(defconst init:load-path-dir (init:in-dir "load-path")
  "Directory for deployment-specific manually-installed packages.")
(defconst init:themes-dir (init:in-dir "themes")
  "Directory for themes, initializes `custom-theme-directory'.")
(defconst init:recovery-dir (init:in-dir "recovery")
  "Directory for backup and auto-save data.")
(defconst init:snippets-dir (init:in-dir "snippets")
  "Directory for custom snippets for yasnippet package.")

(defconst init:backup-dir
  (init:in-dir "backup" init:recovery-dir)
  "Directory for backup data. Initializes `backup-directory-alist'.")
(defconst init:auto-save-list-dir
  (init:in-dir "auto-save-list" init:recovery-dir)
  "Directory for auto-save lists. Initializes `auto-save-list-file-prefix'.")
(defconst init:auto-save-dir
  (init:in-dir "auto-save" init:recovery-dir)
  "Directory for auto-save data.
Initializes `auto-save-file-name-transforms'.")

(defconst init:base-file (init:in-dir "init-base.el" init:user-modules-dir t)
  "This file, to be loaded when emacs is called non-interactively")
(defconst init:custom-file (init:in-dir "custom.el" nil t)
  "File for stored 'customize' settings. Initializes `custom-file'.")
(defconst init:after-file (init:in-dir "after.el" nil t)
  "File to be executed if present after main init code.")

(defconst init:auto-create-dirs (list init:user-modules-dir
                                      init:load-path-dir
                                      init:themes-dir
                                      init:snippets-dir
                                      init:recovery-dir
                                      init:backup-dir
                                      init:auto-save-list-dir
                                      init:auto-save-dir)
  "Directories to create during init.")


(defvar init:first-frame-hook nil
  "Hook to handle init tasks that require frame.
This is executed at the end of init or, if Emacs
is executed as daemon, when the first frame is created.
Called by `init:at-first-frame-function'")


(defun init:at-frame-function (frame)
  "Function to be called after each FRAME creation.
Handled by `after-make-frame-functions' and called once at the end of init."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))


(defun init:at-first-frame-function (frame)
  "Function to be called at first FRAME creation.
Can be used to initialize theme. Executes `init:first-frame-hook'.
Handled by `after-make-frame-functions' and removes itself from
function list afterwards."
  (with-selected-frame frame
    (run-hooks 'init:first-frame-hook))
  (remove-hook 'after-make-frame-functions #'init:at-first-frame-function))


;; Create directories
(eval-when-compile
  (dolist (dir init:auto-create-dirs)
   (unless (file-directory-p dir)
     (make-directory dir))))


(defun init:run-emacs (script &optional buffer)
  "Execute SCRIPT using current Emacs binary.
Display result in BUFFER."
  (let ((buffer (get-buffer-create (or buffer "*Exec Emacs*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (display-buffer buffer)
      (call-process init:emacs nil buffer t
                    "--batch" "--quick"
                    "--load" init:base-file
                    "--eval" script))))


(defun init:recompile-elpa ()
  "Recompile packages in elpa directory"
  (interactive)
  (init:run-emacs
   "(progn
 (package-initialize)
 (byte-recompile-directory package-user-dir 0 t))"))


(defun init:recompile ()
  (interactive)
  (init:run-emacs
   (format
    "(progn
 (package-initialize)
 (byte-recompile-directory \"%s\" 0 t))"
    init:user-modules-dir)))


;; Load path for additional modules
(eval-when-compile
  (dolist (default-directory
            (list init:user-modules-dir
                  init:load-path-dir))
    (normal-top-level-add-to-load-path (list "."))
    (normal-top-level-add-subdirs-to-load-path)))

(eval-when-compile
  (unless (file-exists-p init:after-file)
    (with-temp-file init:after-file
      (insert ";;; after.el -*- lexical-binding: t; -*-\n\n"))))


;; Themes directory, Custom and current config
(setq custom-theme-directory init:themes-dir
      custom-file init:custom-file)


;; Backup, autosave, lockfiles
(setq backup-directory-alist `((".*" . ,init:backup-dir))
      auto-save-list-file-prefix (concat init:auto-save-list-dir "saves-")
      auto-save-file-name-transforms `((".*" ,init:auto-save-dir t))
      create-lockfiles nil)


(setq idle-update-delay 2)


(provide 'init-base)

;;; init-base.el ends here
