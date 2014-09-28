;; custom file ;;
(setq user-emacs-directory-full (expand-file-name user-emacs-directory))

(setq custom-file (concat user-emacs-directory-full "custom.el"))
(load custom-file t)

;; load additional config
(load (concat user-emacs-directory-full "user/init_impl"))
