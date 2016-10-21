;;; init-theme.el --- Theme overrides  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(deftheme my:theme "My face theme settings")

(custom-theme-set-faces
 'my:theme
 ;; Line numbers appearance
 '(linum ((t (:bold t :italic nil))))

 ;; '(vertical-border
 ;;   ((((type graphic))
 ;;     (:foreground nil :backround nil :inherit fringe :inverse-video t))))

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
