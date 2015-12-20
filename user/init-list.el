;;; init-list.el --- List manipulation helpers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defmacro my:mapcan (function sequence)
  "Replacement for `mapcan' to not require `cl.el'"
  `(apply #'nconc (mapcar ,function ,sequence)))

(defun my:remove-if (func sequence)
  "Reimplacement for `remove-if' to not use `cl.el'"
  (delq nil (mapcar
             (lambda (x) (if (funcall func x) nil x))
             sequence)))

(defun my:remove-if-not (func sequence)
  "Reimplacement for `remove-if-not' to not use `cl.el'"
  (delq nil (mapcar
             (lambda (x) (if (funcall func x) x nil))
             sequence)))

(defmacro my:add-to (l el)
  `(setq ,l (cons ,el ,l)))

(defmacro my:append-to (to from)
  `(setq ,to (append ,from ,to)))


(provide 'init-list)

;;; init-list.el ends here
