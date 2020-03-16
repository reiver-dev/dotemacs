;;; init-base.el --- fundamental defines  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defmacro init:when-windows (&rest body)
  "Evaluate BODY if current os is windows."
  (if (eq system-type 'windows-nt) (cons 'progn body)))

(defmacro init:when-posix (&rest body)
  "Evaluate BODY if current os is posix (not windows)."
  (if (eq system-type 'windows-nt) nil (cons 'progn body)))


(put 'init:when-windows 'lisp-indent-function 'defun)
(put 'init:when-posix 'lisp-indent-function 'defun)


(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

(when (< emacs-major-version 27)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
 (when (fboundp 'tool-bar-mode)
   (tool-bar-mode -1))
 (when (fboundp 'toolltip-mode)
   (tooltip-mode -1))
 (when (fboundp 'scroll-bar-mode)
   (scroll-bar-mode -1)))

(setq-default load-prefer-newer t)


(eval-when-compile
  (require 'package))

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/")))


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
  "Current Emacs executable.")
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
  "This file, to be loaded when Emacs is called non-interactively.")
(defconst init:custom-file (init:in-dir "custom.el" nil t)
  "File for stored 'customize' settings. Initializes variable `custom-file'.")
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


(defun init:try-run-hook (fn hook)
  "Run a FN associated with HOOK wrapped in a `condition-case-unless-debug'.
Its objective is to include more information in the error message, without
sacrificing your ability to invoke the debugger in debug mode."
  (condition-case-unless-debug ex (funcall fn)
    ('error
     (lwarn hook :error "%s in '%s' -> %s"
            (car ex) fn (error-message-string ex))))
  nil)


(defvar init:startup-hook nil
  "Hook to be executed during `emacs-startup-hook'.")


(defun init:finalize ()
  "Run routines after config is loaded."
  (unless (or (not after-init-time) noninteractive)
    (run-hook-wrapped 'init:startup-hook
                      #'init:try-run-hook 'init:startup-hook)))


(add-hook 'emacs-startup-hook #'init:finalize)


;; Create directories
(defun init:create-default-directories ()
  "Make directories defined in `init:auto-create-dirs'."
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
                    "--quick" "--batch"
                    "--load" init:base-file
                    "--eval" script))))


(defun init:recompile-elpa ()
  "Recompile packages in elpa directory."
  (interactive)
  (init:run-emacs
   (prin1-to-string
    `(progn
       (package-initialize)
       (byte-recompile-directory package-user-dir 0 t)))))


(defun init:user-modules-files ()
  "Find elisp files eligible to be byte compiled in `init:user-modules-dir'."
  (let ((directory init:user-modules-dir)
        (include emacs-lisp-file-regexp)
        (exclude
         "\\`\\(?:\\.#.*\\|#.*#\\|\\.dir-locals\\(?:-[0-9]+\\)?\\.el\\)\\'")
        result)
    (dolist (name (directory-files directory))
      (let ((filename (file-name-nondirectory name))
            (path (expand-file-name name directory)))
        (when (and
               (not (file-directory-p path))
               (string-match include filename)
               (not (string-match exclude filename))
               (file-readable-p path))
          (setq result (cons path result)))))
    result))


(defun init:recompile-file (file buffer)
  "Run Emacs to compile FILE.
Use BUFFER for process and compilation log."
  (call-process
   init:emacs nil buffer t
   "--quick" "--batch"
   "--load" init:base-file
   "--eval" (prin1-to-string
             `(progn
                (setq lexical-binding t
                      exit-code 0
                      target-file ,file)
                (package-initialize)
                (message (format "Compiling %s..." target-file))
                (if (null (batch-byte-compile-file target-file))
                    (setq exit-code 1))
                (message (format "Compiling %s... done" target-file))
                (kill-emacs exit-code)))))


(defun init:recompile (&optional force)
  "Recompile user config by running Emacs for each file.
If univesal argument FORCE is set recompile even if result exists."
  (interactive "P")
  (let ((files (init:user-modules-files))
        (buffer (get-buffer-create "*Exec Emacs*")))
    (with-current-buffer buffer
      (erase-buffer)
      (pop-to-buffer buffer nil t)
      (dolist (file files)
        (if (or force
                (file-newer-than-file-p file (byte-compile-dest-file file)))
            (init:recompile-file file buffer)
          (insert (format "Compiling %s... skip\n" file)))))))


;; Load path for additional modules

(defun init:setup-load-path ()
  "Initialize load path recursively to to '~/.emacs.d/load-path'."
  (dolist (default-directory
            (list init:user-modules-dir
                  init:load-path-dir))
    (normal-top-level-add-to-load-path (list "."))
    (normal-top-level-add-subdirs-to-load-path)))


(defun init:create-after-file ()
  "Create empty 'after.el' in '~/.emacs.d'."
  (unless (file-exists-p init:after-file)
    (with-temp-file init:after-file
      (insert ";;; after.el -*- lexical-binding: t; -*-\n\n"))))


(init:create-default-directories)
(init:create-after-file)
(init:setup-load-path)


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
