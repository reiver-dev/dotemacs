;;; init-shlex.el --- Shell options parsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Based on:
;; - [http://man.openbsd.org/OpenBSD-6.3/sh#Quoting]
;; - Python shlex module
;; - https://linux.die.net/man/1/dash

;;; Code:


(defconst my:sh-char-whitespace " \f\t\n\r\v"
  "Whitespace characters.")

(defconst my:sh-char-quotes "\"'"
  "Weak and strong quotes.")

(defconst my:sh-char-weak-special "\"\\"
  "Literally escaped inside weak quotes.")

(defconst my:char-escape-input "0abtnvfreN_LP"
  "Character backslash replacement.")

(defconst my:char-escape-result
  "\0\a\b\t\n\v\f\r\e\u0085\u00a0\u2080\u2029"
  "Result of `-my:char-escape-input' if preceded by backslash.")

(defconst my:sh-escape-unicode-prefix "xuU"
  "Characters that result in character unicode escape.")

(defconst my:sh-backslashed-literal " \"\\"
  "Literal if quoted.")


(defun my:sh-tokenize (input)
  "Split and unquote INPUT string as list of command line arguments."
  (let* ((len (length input))
         (whitespace (string-to-list my:sh-char-whitespace))
         (weak-special (string-to-list my:sh-backslashed-literal))
         (i 0)
         (state 'normal)
         slashed result acc)
    (while (< i len)
      (let ((char (aref input i)))
        (cond
         ((eq state 'normal)
          (cond

           (slashed
            (setq acc (cons char acc)
                  slashed nil))

           ((member char whitespace)
            (when acc
              (setq result (cons acc result)
                    acc nil)))

           ((equal char ?\") (setq state 'weak-quotes))
           ((equal char ?\') (setq state 'strong-quotes))
           ((equal char ?\\) (setq slashed t))
           (t (setq acc (cons char acc)))))

         ((eq state 'weak-quotes)
          (cond
           (slashed
            (setq acc (if (member char weak-special)
                          (cons char acc)
                        (cons char (cons ?\\ acc)))
                  slashed nil))
           ((equal char ?\\) (setq slashed t))
           ((equal char ?\") (setq state 'normal))
           (t (setq acc (cons char acc)))))

         ((eq state 'strong-quotes)
          (cond
           ((equal char ?\') (setq state 'normal))
           (t (setq acc (cons char acc)))))

         (t
          (error "Invalid state: %s" state))))
      (setq i (1+ i)))
    (setq result (cons acc result))
    (mapcar (lambda (x) (nreverse (apply 'string x)))
            (nreverse result))))


(provide 'init-shlex)


;;; init-shlex.el ends here
