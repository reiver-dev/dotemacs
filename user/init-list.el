;;; init-list.el --- List manipulation helpers -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my:mapcan (func sequence)
  "Replacement for `mapcan' to not require \"cl.el\".
Applies FUNC to SEQUENCE to join results."
  (apply #'nconc (mapcar func sequence)))


(defconst --filter-marker-- (make-symbol "--filter-marker--"))


(defun my:remove-if (func sequence)
  "Replacement for `remove-if' to not use `cl.el'.
Applies FUNC to SEQUENCE elements and removes elements
from it if result is not true."
  (delq --filter-marker--
        (mapcar
         (lambda (x)
           (if (funcall func x) --filter-marker-- x))
         sequence)))

(defun my:remove-if-not (func sequence)
  "Reiplacement for `remove-if-not' to not use \"cl.el\".
Applies FUNC to SEQUENCE elements and removes elements
from it if result is true."
  (delq --filter-marker--
        (mapcar
         (lambda (x)
           (if (funcall func x) x --filter-marker--))
         sequence)))




(provide 'init-list)

;;; init-list.el ends here
