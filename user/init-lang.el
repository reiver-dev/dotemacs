;;; init-lang.el --- Input-method configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The code is taken from
;; https://www.alexkorablev.ru/2017/06/10/emacs-got-keys/
;;
;; Which originally based on
;; http://reangdblog.blogspot.com/2015/05/emacs.html
;;
;; Repotedly has problems with asterisk `*' on mac layouts

;;; Code:


(require 'quail)


(defun my:reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))


(provide 'init-lang)

;;; init-lang ends here
