;; Load path for additional modules
(let ((lpdir (concat user-emacs-directory "load-path")))
  (unless (file-directory-p lpdir)
    (make-directory lpdir))
  (let ((default-directory lpdir))
    (normal-top-level-add-to-load-path (list "."))
    (normal-top-level-add-subdirs-to-load-path)))

;; Themes directory
(let ((themes-dir (concat user-emacs-directory "themes")))
  (unless (file-directory-p themes-dir)
        (make-directory themes-dir))
  (setq custom-theme-directory themes-dir))

;; Custom and current config
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; Persistent common configuration
(load (concat user-emacs-directory "user/init_impl"))
