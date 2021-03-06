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


(defun my:amap (func array &optional start end)
  "Apply FUNC to each element of the ARRAY.
Optional START and END declare array slice."
  (let* ((start (or start 0))
         (end (if end (min end (length array))
                (length array)))
         (pos 0)
         (result (make-vector (- end start) nil)))
    (while (< start end)
      (aset result pos (funcall func (aref array start)))
      (setq pos (1+ pos)
            start (1+ start)))
    result))


(defun my:array-double-elements (array)
  "Double each element in input ARRAY."
  (let* ((len (length array))
         (result (make-vector (* 2 len) 0))
         (idx 0))
    (while (< idx len)
      (aset result (* idx 2) (aref array idx))
      (aset result (1+ (* idx 2)) (aref array idx))
      (setq idx (1+ idx)))
    result))


(defun my:reduce-cols (colfun cellfun rows &optional init)
  "Apply COLFUN to each cell in table ROWS.
Apply CELLFUN to each table element. Optional INIT is used
 to assign first result value, nil if not set."
  (let* ((num-cols (apply #'max (mapcar #'length rows)))
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


(defun my:string-partition-regex (str regex)
  "Split STR around first and last groups of REGEX.
Return (prefix . suffix) pair."
  (if (string-match regex str)
      (let ((md (match-data))
            pb pe sb se)
        (setq md (cddr md))
        (setq pb (pop md)
              pe (pop md))
        (while md
          (setq sb (pop md)
                se (pop md)))
        (cons (substring str pb pe)
              (substring str sb se)))
    (cons str nil)))


(defun my:string-partition (str delimiter)
  "Split STR around first occurance of DELIMITER.
Return (prefix . suffix) pair."
  (my:string-partition-regex
   str (concat "\\`\\(.*?\\)" delimiter "\\(.*\\)\\'")))


(defun my:string-rpartition (str delimiter)
  "Split STR around last occurance of DELIMITER.
Return (prefix . suffix) pair."
  (my:string-partition-regex
   str (concat "\\`\\(.*\\)" delimiter "\\(.*\\)\\'")))


(defun my:range-len (start end step)
  "Return number of elements in half-open range.
Range is defined as integers or floats [START, END) with STEP."
  (if (or (and (> step 0) (< start end))
          (and (< step 0) (< end start)))
      (ceiling (abs (- end start)) (abs step))
    0))


(defun my:range-last (start end step)
  "Return last element of half-open range.
Range is defined as integers or floats [START, END) with STEP.
If range is empty nil is returned."
  (let ((len (my:range-len start end step)))
    (if (= 0 len)
        nil
      (+ start (* (1- len) step)))))


(defun my:make-range (start end &optional step)
  "Construct list of members of half-open range.
Range is defined as integers or floats [START, END) with STEP."
  (let* ((step (or step 1))
         (idx (1- (my:range-len start end step)))
         items)
    (when (or (floatp start)
              (floatp end)
              (floatp step))
      (setq start (float start)
            end (float end)
            step (float step)))
    (while (>= idx 0)
      (setq items (cons (+ (* step idx) start) items)
            idx (1- idx)))
    items))


(defun my:make-arange (start end &optional step)
  "Construct array of members of half-open range.
Range is defined as integers or floats [START, END) with STEP."
  (let* ((step (or step 1))
         (count (my:range-len start end step))
         (items (make-vector count 0))
         (idx 0))
    (when (or (floatp start)
              (floatp end)
              (floatp step))
      (setq start (float start)
            end (float end)
            step (float step)))
    (while (< idx count)
      (aset items idx (+ (* step idx) start))
      (setq idx (1+ idx)))
    items))


(defmacro my:any-of (value &rest items)
  "Check if the VALUE is `eq' to any of ITEMS."
  (let (result)
    (dolist (item items)
      (setq result (append (list `(eq ,value ,item)) result)))
    `(or ,@(nreverse result))))


(provide 'init-list)

;;; init-list.el ends here
