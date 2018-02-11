;;; init-font.el --- Enable ligatures  -*- lexical-binding: t -*-

;;; Commentary:
;; Taken from:
;; https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md#appendix-b-update-1-firacode-integration

;;; Code:

(defconst my:space (decode-char 'ucs 32))
(defconst my:nbs (decode-char 'ucs 160))


(defun -my:ligatures-correct-symbol-bounds (len char)
  "Prepend up to LEN non-breaking spaces with reference points to CHAR.
This way `compose-region' called by function `prettify-symbols-mode'
will use the correct width of the symbols instead of the width
measured by `char-width'."
  (let ((acc (list (decode-char 'ucs char))))
    (while (> len 2)
      (setq acc (cons my:space (cons '(Br . Bl) acc)))
      (setq len (1- len)))
    (if (> len 1)
        (cons my:nbs (cons '(Br . Bl) acc))
      acc)))


(defun -my:ligatures-make-alist (ligatures starting-code)
  "Construct text to ligature character.
For each string in LIGATURES list add replacement from STARTING-CODE
sequentially."
  (mapcar (lambda (l)
            (let ((n starting-code))
              (setq starting-code (1+ starting-code))
              (when l
                (cons l (-my:ligatures-correct-symbol-bounds
                         (length l) n)))))
          ligatures))

(defconst my:ligatures-fira-code-list
  '("www" "**" "***" "*>" "*/" "\\\\" "\\\\\\" "]#" "::" ":::"
    ":=" "!!" "!=" "!==" "--" "---" "-->" "->" "->>" "-<"
    "-<<" "-~" "#{" "#[" "#!" "##" "###" "####" "#(" "#?"
    "#_" "#_(" ".-" ".=" ".." "..<" "..." ".?" "?:" "?="
    "?." "??" ";;" "/*" "/=" "/==" "/>" "//" "///" "__"
    "&&" "||" "|||>" "||=" "||>" "|=" "|>" "^=" "$>" "++"
    "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<=" "=<<"
    "=/=" ">-" ">->" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
    "<*>" "<|" "<||" "<|||" "<|>" "<$" "<$>" "<!--" "<-" "<--"
    "<->" "<-<" "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<"
    "<<-" "<<=" "<<<" "<~" "<~>" "<~~" "</" "</>" "~@" "~-"
    "~=" "~>" "~~" "~~>" "%%")
  "Ordered ligatures for Fira Code font")


(defconst my:ligatures-fira-code-start #Xe100)


(defconst my:ligatures-fira-code-pretty-alist
  (-my:ligatures-make-alist my:ligatures-fira-code-list
                            my:ligatures-fira-code-start))


(defun my:ligatures-fira-code-setup ()
  "Add Fira Code ligatures to `prettify-symbols-alist'."
  (setq prettify-symbols-alist
        (append my:ligatures-fira-code-pretty-alist
                prettify-symbols-alist)))


(provide 'init-font)

;;; init-font.el ends here
