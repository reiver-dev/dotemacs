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


(defun my:reduce (reducer sequence &optional init)
  "Perform reduction with REDUCER over SEQUENCE.
Use optional INIT value or first element of SEQUENCE."
  (when sequence
    (let* ((acc (or init (car sequence)))
           (seq (if init sequence (cdr sequence))))
      (mapc (lambda (el)
              (setq acc (funcall reducer acc el)))
            seq)
      acc)))


(defun my:mapreduce (reducer mapper sequence &optional init)
  "Perform reduction with REDUCER over SEQUENCE.
Apply MAPPER to each SEQUENCE element. Use optional INIT value
or first element of SEQUENCE."
  (when sequence
    (let* ((acc (or init (car sequence)))
           (seq (if init sequence (cdr sequence))))
      (mapc (lambda (el)
              (setq acc (funcall reducer acc (funcall mapper el))))
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


(defun my:aposf (array element &optional start end)
    "Find first position of the ELEMENT in ARRAY.
Comparison is performed using `eq' function. Optionally use START and
END positions to limit search to a region."
  (let ((pos (or start 0))
        (end (if end (min end (length array))
               (length array)))
        result)
    (while (and (not result) (< pos end))
      (when (eq element (aref array pos))
        (setq result pos))
      (setq pos (1+ pos)))
    result))


(defun my:aposl (array element &optional start end)
    "Find last position of ELEMENT in ARRAY.
Comparison is performed using `eq' function. Optionally use START and
END positions to limit search to a region."
  (let ((start (or start 0))
        (pos (1- (if end
                     (min end (length array))
                   (length array))))
        result)
    (while (and (not result) (>= pos start))
      (when (eq element (aref array pos))
        (setq result pos))
      (setq pos (1- pos)))
    result))


(defun my:afindf (predicate array &optional start end)
  "Find a position of first element that conforms to a PREDICATE in the ARRAY.
Optionally use START and END positions to limit search to a region."
  (let ((pos (or start 0))
        (end (if end (min end (length array))
               (length array)))
        result)
    (while (and (not result) (< pos end))
      (when (funcall predicate (aref array pos))
        (setq result pos))
      (setq pos (1+ pos)))
    result))


(defun my:afindl (predicate array &optional start end)
  "Find a position of last element that conforms to a PREDICATE in the ARRAY.
Optionally use START and END positions to limit search to a region."
  (let ((start (or start 0))
        (pos (1- (if end
                     (min end (length array))
                   (length array))))
        result)
    (while (and (not result) (>= pos start))
      (when (funcall predicate (aref array pos))
        (setq result pos))
      (setq pos (1- pos)))
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


(defun my:length-to-stops (values offset)
  "Transform column length VALUES to list of tab stop positions.
Use OFFSET as column separator width."
  (let ((acc  0))
    (mapcar (lambda (x)
              (setq acc (+ acc x (if (eql acc 0) 0 offset))))
            values)))


(defun my:stops-to-ranges (stops offset)
  "Transform column tab STOPS to list of (begin . end) pairs.
Use OFFSET as column separator width."
  (let ((begin (cons 0 (mapcar (lambda (x) (+ x offset)) stops)))
        (end (mapcar #'identity stops)))
    (my:mapcar-zip #'cons begin end)))


(defun my:string-trim (str)
  "Remove leading and trailing whitespace from STR string."
  (save-match-data
    (let ((begin (if (string-match "\\`[ \t\n\r]+" str)
                     (match-end 0)
                   0))
          (end (if (string-match "[ \t\n\r]+\\'" str)
                   (match-beginning 0)
                 nil)))
      (substring str begin end))))


(provide 'init-list)

;;; init-list.el ends here
