;; theme dirs
(let ((lpdir (concat user-emacs-directory "load-path")))
  (unless (file-directory-p lpdir)
        (make-directory lpdir))
  (add-to-list 'load-path lpdir))

(let ((themes-dir (concat user-emacs-directory "themes")))
  (unless (file-directory-p themes-dir)
        (make-directory themes-dir))
  (setq custom-theme-directory themes-dir))

;; custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; load additional config
(load (concat user-emacs-directory "user/init_impl"))
