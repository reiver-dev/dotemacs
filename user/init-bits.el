;;; init-bits.el --- Bitmask and bitset -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-environ)
(require 'init-shlex)


(defun my:bits-string (value bits)
  "Convert an integer VALUE into it's binary representation in string format.
Expect value to be BITS length."
  (setq bits (max 1 bits))
  (let ((res ""))
    (while (not (= 0 bits))
      (setq res (concat (if (= 1 (logand value 1)) "1" "0") res)
            value (lsh value -1)
            bits (1- bits)))
    (if (string= res "")
        (setq res "0"))
    res))


(defun my:bits-reverse (value bits)
  "Reverse bitmask VALUE.
Expect value to be BITS length."
  (let ((result 0))
    (while (< 0 bits)
      (setq result (if (= 0 (logand value 1))
                       (lsh result 1)
                     (logior (lsh result 1) #b1))
            value (lsh value -1)
            bits (1- bits)))
    result))


(defun my:bits-double (value bits)
  "Double each bit in VALUE bitmask.
Expect value to be BITS length."
  (let ((result 0)
        (value (my:bits-reverse value bits)))
    (while (not (= 0 bits))
      (setq result (if (= 0 (logand value 1))
                       (lsh result 2)
                     (logior (lsh result 2) #b11))
            value (lsh value -1)
            bits (1- bits)))
    result))


(provide 'init-bits)

;;; init-bits.el ends here
