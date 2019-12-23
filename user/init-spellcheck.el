;;; init-spellcheck.el --- Spell-check configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)


(defconst -my:hunspell-path (executable-find "hunspell"))


(defun my:spell-dictionary (name &rest parameters)
  "Construct `ispell' dictionary NAME and PARAMETERS.
The result is a list that can be a possible member of
`ispell-dictionary-alist'."
  (let* ((word (plist-get parameters :word))
         (non-word (plist-get parameters :non-word))
         (other (plist-get parameters :other))
         (many-other (plist-get parameters :many-other?))
         (args (plist-get parameters :args))
         (extended-chars (plist-get parameters :extended-chars))
         (encoding (or (plist-get parameters :encoding) 'utf-8)))
    (list name word non-word other many-other args extended-chars encoding)))

(put 'my:spell-dictionary 'lisp-indent-function 'defun)


(defun my:spell-dictionary-ru (name dictname)
  "Make dictionary configuration for Russian.
NAME is a name identifier and DICTNAME is filename."
  (my:spell-dictionary name
    :word "[Ё-ё]"
    :non-word "[^Ё-ё]"
    :other "[-]"
    :many-other? nil
    :args (list "-d" dictname)
    :encoding 'utf-8))


(defun my:spell-dictionary-en (name dictname)
  "Make dictionary configuration for English.
NAME is a name identifier and DICTNAME is filename."
  (my:spell-dictionary name
    :word "[A-z]"
    :non-word "[^A-z]"
    :other "[']"
    :args (list "-d" dictname)
    :encoding 'iso-8859-1))


(my:with-package ispell
  :if -my:hunspell-path
  :init (progn
          (setq-default ispell-program-name "hunspell"
                        flyspell-issue-message-flag nil)
          (setenv "DICPATH"
                  (expand-file-name "../share/hunspell"
                                    (file-name-directory -my:hunspell-path)))))

(provide 'init-spellcheck)

;;; init-spellcheck.el ends here
