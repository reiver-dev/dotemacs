;;; init-chars.el --- Chars and unicode  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'init-list)


(defconst my:char-unicode-bom #xfeff
  "Unicode byte order mark.")


(defconst my:char-utf8-marker "\xef\xbb\xbf"
  "Sequence that indicates utf-8 file or stream.")


(defsubst my:char-unicode-special-p (char)
  "Check if CHAR code is part of unicode specials block."
  (<= #xfff9 char #xffff))


(defsubst my:char-unicode-noncharacter-p (char)
  "Check if CHAR code is noncharacter."
  (or (eq char #xfffe) (eq char #xffff)))


(defsubst my:char-utf16-high-surrogate-p (char)
  "Check if CHAR code is valid utf16 high component of surrogate pair."
  (<= #xd800 char #xdbff))


(defsubst my:char-utf16-low-surrogate-p (char)
  "Check if CHAR code is valid utf16 lower component of surrogate pair."
  (<= #xdc00 char #xdfff))


(defsubst my:char-utf16-surrogate-p (char)
  "Check if CHAR code is valid utf16 high or low surrogate."
  (<= #xd800 char #xdfff))


(defsubst my:char-utf16-surrogate-pair-p (high low)
  "Check if HIGH LOW pair is valid utf16 surrogate pair."
  (and (my:char-utf16-high-surrogate-p high)
       (my:char-utf16-low-surrogate-p low)))


(defsubst my:char-utf8-count-p (char)
  "Get number of utf8 code units required after CHAR code."
  (cond
   ((<= #x00 char #x7f) 1)
   ((<= #xc0 char #xdf) 2)
   ((<= #xe0 char #xef) 3)
   ((<= #xf0 char #xf7) 4)
   (t 0)))


(defsubst my:char-utf8-valid-rest-p (char)
  "Check if CHAR is valid subsequent utf8 code unit."
  (<= #x80 char #xbf))


(defsubst my:char-unicode-not-valid-p (char)
  "Check if CHAR code does not represent valid character."
  (or (<= #xd800 char #xdfff)
      (eq #xfffe char)
      (eq #xffff char)))


(defsubst my:char-hex-p (char)
  "Check if CHAR is valid hexadecimal ascii digit."
  (or (<= ?0 char ?9) (<= ?A char ?F) (<= ?a char ?f)))


(defsubst my:char-not-hex-p (char)
  "Check if CHAR is valid hexadecimal ascii digit."
  (not (my:char-hex-p char)))


(defsubst my:char-digit-p (char)
  "Check if CHAR is ascii digit."
  (<= ?0 char ?9))


(defsubst my:char-not-digit-p (char)
  "Check if CHAR is not ascii digit."
  (not (my:char-digit-p char)))


(defsubst my:char-oct-p (char)
  "Check if CHAR is valid ascii for octal digit."
  (<= ?0 char ?7))


(defun my:char-from-hex (text &optional start end)
  "Parse hex characters of TEXT string in START...END region."
  (let ((start (or start 0))
        (end (or end (length text)))
        (invalid (my:afindf #'my:char-not-hex-p text start end)))
    (if invalid
        (error "Invalid character ?%c at %d"
               (aref text invalid) (+ start invalid)))
    (decode-char 'ucs (string-to-number (substring text start end) 16))))


(defalias 'my:char-from-name #'char-from-name)


(defun my:char-unicode-parse (text &optional start end)
  "Parse [xuU] prefixed code from TEXT in START..END region."
  (let* ((kind (aref text (or start 0)))
         (s (1+ (or start 0)))
         (e (or end (length text)))
         (count (cond
                 ((eq kind ?x) 2)
                 ((eq kind ?u) 4)
                 ((eq kind ?U) 8))))
    (when count
      (when (> (+ s count) end)
        (error "Unexpected end of input, got '%d' required '%d'"
               (- e s) count))
      (cons (my:char-from-hex (substring text s (+ s count)))
            (+ s count)))))


(provide 'init-chars)


;;; init-chars.el ends here
