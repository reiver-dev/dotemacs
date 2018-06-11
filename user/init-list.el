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


(defun my:reduce (func sequence &optional init)
  "Perform reduction with FUNC over ROWS sequence.
Use optional INIT value or first element of ROWS."
  (when sequence
    (let* ((acc (or init (car sequence)))
           (seq (if init sequence (cdr sequence))))
      (mapc (lambda (el)
              (setq acc (funcall func acc el)))
            seq)
      acc)))


(defun my:find-first (func sequence)
  "Apply FUNC to each element of SEQUENCE.
Return first non-nil result."
  (let (result)
    (while (and (not result) sequence)
      (setq result (funcall func (car sequence)))
      (setq sequence (cdr sequence)))
    result))


(defun my:reduce-cols (colfun cellfun rows &optional init)
  "Apply COLFUN to each cell in table ROWS.
Apply CELLFUN to each table element. Optional INIT is used
 to assign first result value, nil if not set."
  (let* ((num-cols (seq-max (mapcar #'length rows)))
         (coldata (make-vector num-cols init)))
    (mapc
     (lambda (row)
       (let ((i 0))
         (while (and row (< i num-cols))
           (aset coldata i
                 (funcall colfun
                          (aref coldata i)
                          (funcall cellfun (car row))))
           (setq row (cdr row)
                 i (1+ i)))))
     rows)
    coldata))


(defun my:mapcar-zip (func &rest sequences)
  "Apply FUNC to list of corresponding SEQUENCES element.
Return result of each function call. See `mapcar'."
  (let* ((el (cons nil nil))
         (result el))
    (while (not (memq nil sequences))
      (setq el (push (apply func (mapcar #'car sequences)) (cdr el))
            sequences (mapcar #'cdr sequences)))
    (cdr result)))


(defun my:mapc-zip (func &rest sequences)
  "Apply FUNC to list of corresponding SEQUENCES element.
Return nil. See `mapc'."
  (while (not (memq nil sequences))
    (apply func (mapcar #'car sequences))
    (setq sequences (mapcar #'cdr sequences))))



(provide 'init-list)

;;; init-list.el ends here
