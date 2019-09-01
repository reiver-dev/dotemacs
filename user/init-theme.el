;;; init-theme.el --- Theme overrides  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(deftheme my:theme "My face theme settings")


(defun my:change-theme (theme)
  "Load Custom theme named THEME from its file.
Like `load-theme', but disables all themes before loading the new one."
  ;; Select theme from available
  (interactive
   (list
    (intern
     (completing-read "Load custom theme: "
                      (mapcar 'symbol-name (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (dolist (theme custom-enabled-themes) (disable-theme theme))
  (load-theme theme :no-confirm :no-enable)
  (enable-theme theme)
  (enable-theme 'my:theme))


(custom-theme-set-faces
 'my:theme

 '(vertical-border
   ((((type tty))
     (:foreground nil :background nil :inherit (mode-line default)))
    (((type graphic))
     (:foreground nil :backround nil :inherit fringe))))

 ;; Modeline highligh box is ugly
 '(mode-line-highlight ((t (:box nil :inverse-video t))))

 ;; Disable underline and dir highlight
 ;; '(helm-selection      ((t (:underline nil))))
 ;; '(helm-selection-line ((t (:underline nil))))
 ;; '(helm-ff-directory   ((t (:background nil))))

 ;; Make function-args respect current theme
 '(fa-face-hint      ((t (:inherit highlight))))
 '(fa-face-hint-bold ((t (:bold t :inherit fa-face-hint))))
 '(fa-face-semi      ((t (:inherit (highlight font-lock-keyword-face)))))
 '(fa-face-type      ((t (:inherit (highlight font-lock-type-face)))))
 '(fa-face-type-bold ((t (:bold t :inherit fa-face-type)))))

(provide 'init-theme)

;;; init-theme.el ends here
