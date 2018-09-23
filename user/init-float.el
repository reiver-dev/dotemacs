;;; init-float.el --- Floating point parsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Based on Floating-Point Language-Independent Type for YAML

;;; Code:


(defconst my:regexp-float-canonical
  "-?[1-9]\\.\\(?:\\.[0-9]*[1-9]\\)?\\(?:e[-+][1-9][0-9]*\\)?"
  "Canonical float regexp.")

(defconst my:regexp-float-canonical-special
  "\\(?:0|-?.inf|.nan\\)"
  "Canonical zero, positive and negative infinity, not a number regexp.")

(defconst my:regexp-float
  "[-+]?\\(?:\\.[0-9]+\\|[0-9]+\\(\\.[0-9]*\\)?\\)\\(?:[eE][-+]?[0-9]+\\)?"
  "Relaxed float number regexp.")

(defconst my:regexp-float-inf "[-+]?\\.\\(?:inf\\|Inf\\|INF\\)"
  "Float infinity regexp.")

(defconst my:regexp-float-pos-inf "+?\\.\\(?:inf\\|Inf\\|INF\\)"
  "Float positive infinity regexp.")

(defconst my:regexp-float-neg-inf "-\\.\\(?:inf\\|Inf\\|INF\\)"
  "Float negative infinity regexp.")

(defconst my:regexp-float-nan "\\.\\(?:nan\\|NaN\\|NAN\\)"
  "Float not a number regexp.")


(defun my:float-parse (text &optional offset)
  "Parse float value from TEXT string starting from OFFSET."
  (let ((offset (or offset 0)))
    (cond
     ((string-match my:regexp-float-inf text offset)
      (cons
       (if (eq (aref text offset) ?-)
           -1.0e+INF
         1.0e+INF)
       (match-end 0)))
     ((string-match my:regexp-float-nan text offset)
      (cons 0.0e+NaN (match-end 0)))
     ((string-match my:regexp-float text offset)
      (let ((e (match-end 0)))
        (cons (string-to-number (substring text offset e) 10)
              e))))))


(provide 'init-float)


;;; init-float.el ends here
