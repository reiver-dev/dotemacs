;;; init-keybind.el --- Keybindings routines -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-list)


(defvar my:bindings-mode-map
  (make-sparse-keymap)
  "Keymap for `my:bindings-mode'.")


(define-minor-mode my:bindings-mode
  "My key bindings
\\{my:bindings-mode-map}"
  :global t
  :keymap my:bindings-mode-map)


(my:bindings-mode t)


(defmacro -my:kmap (keymap &rest bindings)
  "Binding keys to KEYMAP.
Should get (kbd1 kbd2 .. function) as BINDINGS args"
  (let ((result
         (my:mapcan
          (lambda (bind)
            (let ((keys (butlast bind))
                  (func (last bind)))
              (mapcar (lambda (key)
                        (when (stringp key)
                          (setq key `(kbd ,key)))
                        `(define-key ,keymap ,key ,@func))
                      keys)))
          bindings)))
    (if (< 1 (length result))
        `(progn ,@result)
      (car result))))


(defmacro my:kmap* (keymap &rest bindings)
  "Binding keys to KEYMAP.
Should get (kbd1 kbd2 .. function) as BINDINGS args"
  (macroexp-let2
      (lambda (form) (or (not (consp form)) (macroexp-const-p form)))
      keymap keymap
    `(-my:kmap ,keymap ,@bindings)))


(defmacro my:kmap (&rest bindings)
  "Set BINDINGS to `my:bindings-mode-map' keymap.
See `my:kmap*'."
  (if (stringp (car bindings))
      `(my:kmap* my:bindings-mode-map ,bindings)
    `(my:kmap* my:bindings-mode-map ,@bindings)))


(defmacro my:kmap-global (&rest bindings)
  "Set BINDINGS to `current-global-map' keymap.
See `my:kmap*'."
  `(my:kmap* (current-global-map) ,@bindings))


(defun my:minibuffer-set-key (key command)
  "Bind KEY with COMMAND to all common minibuffer states."
  (dolist (m (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map))
    (define-key m key command)))


(defun my:global-unset-command (command)
  "Unset all key binding for COMMAND symbol.
See `global-unset-key'."
  (let ((bindings (where-is-internal command)))
    (mapc (lambda (bnd) (global-unset-key bnd)) bindings)))


(provide 'init-keybind)

;;; init-keybind.el ends here
