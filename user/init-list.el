;;; init-list.el --- List manipulation helpers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defmacro my:mapcan (func sequence)
  "Replacement for `mapcan' to not require \"cl.el\".
Applies FUNC to SEQUENCE to joins results."
  `(apply #'nconc (mapcar ,func ,sequence)))

(defun my:remove-if (func sequence)
  "Replacement for `remove-if' to not use `cl.el'.
Applies FUNC to SEQUENCE elements and removes elements
from it if result is not true."
  (delq nil (mapcar
             (lambda (x) (if (funcall func x) nil x))
             sequence)))

(defun my:remove-if-not (func sequence)
  "Reiplacement for `remove-if-not' to not use \"cl.el\".
Applies FUNC to SEQUENCE elements and removes elements
from it if result is true."
  (delq nil (mapcar
             (lambda (x) (if (funcall func x) x nil))
             sequence)))

(defmacro my:add-to (list element)
  "Update LIST by prepending ELEMENT."
  `(setq ,list (cons ,element ,list)))

(defmacro my:append-to (list &rest sequences)
  "Update LIST by prepending SEQUENCES."
  `(setq ,list (append ,@sequences ,list)))


(provide 'init-list)

;;; init-list.el ends here
