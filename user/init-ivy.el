;;; init-ivy.el --- ivy-mode customization  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module is to be loaded after ivy-mode

;;; Code:

(eval-when-compile
  (require 'init-keybind))

(require 'init-filesystem)
(require 'ivy)


;; https://github.com/abo-abo/swiper/issues/1953
(defvar ivy-recursive-restore-in-progress nil)


(defun ivy-recursive-restore-flag (func &rest args)
  "Bind `ivy-recursive-restore-in-progress' while calling FUNC with ARGS."
  (let ((ivy-recursive-restore-in-progress t))
    (apply func args)))


(defun ivy-recursive-restore-skip-when-in-progress (func &rest args)
  "Skip call to FUNC with ARGS if `ivy-recursive-restore-in-progress' is set."
  (if ivy-recursive-restore-in-progress
      nil
    (apply func args)))


(advice-add 'ivy-recursive-restore :around #'ivy-recursive-restore-flag)
(advice-add 'ivy-read :around #'ivy-recursive-restore-skip-when-in-progress)


(defvar counsel-find-file-map)
(autoload 'counsel--find-file-matcher "counsel")


(defun -my:ivy-complete-files-components (path)
  "Split PATH string into cons pair of components.
First component is existing prefix, second is not-existing suffix."
  (let* ((tail nil)
         (head path))
    (while (not (file-directory-p head))
      (let ((next (directory-file-name head)))
        (setq tail (cons (file-name-nondirectory next) tail)
              head (file-name-directory (directory-file-name next)))))
    (cons (file-name-as-directory head) (mapconcat #'identity tail "/"))))


(defun my:ivy-complete-files (&optional initial-input)
  "Complete filesystem paths using `ivy-read'.
Start from INITIAL-INPUT path if provided. Siminal to
`counsel-find-file' but inserts path into buffer instead of visiting
it."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'filename))
         (ivy-completion-beg (and bounds (car bounds)))
         (ivy-completion-end (and bounds (cdr bounds)))
         (existing (or (and bounds
                            (buffer-substring-no-properties
                             ivy-completion-beg ivy-completion-end))
                       default-directory))
         (components (-my:ivy-complete-files-components existing)))
    (ivy-read "Complete file: " #'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :initial-input (or initial-input (cdr components))
              :action
              (lambda (result)
                (with-ivy-window
                  (ivy-completion-in-region-action
                   (if bounds
                       (let* ((target-dir
                               (if (file-directory-p existing)
                                   (file-name-as-directory existing)
                                 (file-name-directory existing)))
                              (relative (file-relative-name
                                         result target-dir)))
                         (if (string-equal "./" relative) target-dir
                           (concat target-dir relative)))
                     (abbreviate-file-name result)))))
              :preselect (concat (car components) ".")
              :require-match 'confirm-after-completion
              :keymap counsel-find-file-map
              :caller 'read-file-name-internal)))


(setq-default
 ;; no overlays
 ivy-display-functions-alist nil
 ;; include recent files and bookmarks
 ivy-use-virtual-buffers t
 ;; no '^' for M-x and others
 ivy-initial-inputs-alist nil
 ;; do not complete selected dir if slash entered
 ivy-magic-slash-non-match-action nil
 ;; keep minibuffer large if candidate list is low
 ivy-height 12
 ivy-fixed-height-minibuffer t
 ;; highligh full line
 ivy-format-function #'ivy-format-function-line)


(my:kmap ("C-o f" #'my:ivy-complete-files))


(provide 'init-ivy)

;;; init-ivy.el ends here
