;;; init-theme.el --- Theme overrides  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(deftheme my:theme "My face theme settings")


(defun my:change-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough.
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (dolist (theme custom-enabled-themes)
    (unless (eq theme 'my:theme)
      (disable-theme theme)))
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))


(custom-theme-set-faces
 'my:theme

 '(vertical-border
   ((((type tty))
     (:foreground nil :background nil :inherit mode-line))
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
