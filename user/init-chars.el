;;; init-chars.el --- Chars and unicode  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(require 'init-list)


(defconst my:char-unicode-bom #xfeff
  "Unicode byte order mark.")


(defconst my:char-utf8-marker "\xef\xbb\xbf"
  "Sequence that indicates utf-8 file or stream.")


(defconst my:char-escape-key ?\C-\[)

(defconst my:char-enter-key ?\C-m)


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


(defconst my:char-sp #x0020
  "Space.")

(defconst my:char-nbsp #x00A0
  "No-Break Space.")

(defconst my:char-fgsp #x2006
  "Firgure Space.")

(defconst my:char-nnbsp #x202F
  "Narrow No-Break Space.")

(defconst my:char-zwsp #x200B
  "Zero Width Space.")

(defconst my:char-wj #x2060
  "Word Joiner.")

(defconst my:char-horiz-tab #x0009
  "Horizontal tabulation \\t.")

(defconst my:char-line-feed #x000A
  "Line Feed (LN) \\n.")

(defconst my:char-vertical-tab #x000B
  "Vertical Tabulation.")

(defconst my:char-form-feed #x000C
  "Form Feed \\f.")

(defconst my:char-carriage-return #x000D
  "Carriage Return \\r.")

(defconst my:char-file-separator #x001C
  "File Separator.")

(defconst my:char-group-separator #x001C
  "Group Separator.")

(defconst my:char-record-separator #x001C
  "Record Separator.")

(defconst my:char-unit-separator #x001C
  "Unit Separator.")


(defconst my:char-category-space-separator
  [
   #x0020 ;; Space (SP)
   #x00A0 ;; No-Break Space (NBSP)
   #x1680 ;; Ogham Space Mark
   #x2000 ;; En Quad
   #x2001 ;; Em Quad
   #x2002 ;; En Space
   #x2003 ;; Em Space
   #x2004 ;; Three-Per-Em Space
   #x2005 ;; Four-Per-Em Space
   #x2006 ;; Six-Per-Em Space
   #x2007 ;; Figure Space
   #x2008 ;; Punctuation Space
   #x2009 ;; Thin Space
   #x200A ;; Hair Space
   #x202F ;; Narrow No-Break Space (NNBSP)
   #x205F ;; Medium Mathematical Space (MMSP)
   #x3000 ;; Ideographic Space
   ]
  "SPACE SEPARATOR (Zs) category.")


(defconst my:char-category-line-separator
  [#x2028]
  "LINE SEPARATOR (Zl) category.")


(defconst my:char-category-paragraph-separator
  [#x2029]
  "PARAGRAPH SEPARATOR (Zp) category.")


(defconst my:char-whitespace
  [
   ;; Space Separator (Zs)
   #x0020 ;; Space (SP)
   #x0009 ;; Line Feed
   #x000A ;; Horizontal Tab
   #x000B ;; Vertical Tabulation
   #x000C ;; Form Feed
   #x000D ;; Carriage Return
   #x001C ;; File Separator
   #x001D ;; Group Separator
   #x001E ;; Record Separator
   #x001F ;; Unit Separator

   ;; #x00A0 ;; No-Break Space (NBSP)
   #x1680 ;; Ogham Space Mark
   #x2000 ;; En Quad
   #x2001 ;; Em Quad
   #x2002 ;; En Space
   #x2003 ;; Em Space
   #x2004 ;; Three-Per-Em Space
   #x2005 ;; Four-Per-Em Space
   #x2006 ;; Six-Per-Em Space
   ;; #x2007 ;; Figure Space
   #x2008 ;; Punctuation Space
   #x2009 ;; Thin Space
   #x200A ;;
   ;; #x202F ;; Narrow No-Break Space (NNBSP)
   #x205F ;; Medium Mathematical Space (MMSP)
   #x3000 ;; Ideographic Space

   ;; Line Separator (Zl)
   #x2028

   ;; Paragraph Separator
   #x2029
   ]
  "Whitespace charters according to Java 11.")


(defun my:char-exit-p (char)
  "Check if CHAR corresponds to one of exit keystrokes."
  (or (eq char ?q)
      (eq char ?Q)
      (eq char ?\C-m) ;; Enter
      (eq char ?\C-\[) ;; Escape
      (eq char ?\C-q)
      (eq char ?\C-Q)
      (eq char ?\C-g)
      (eq char ?\C-G)))


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
