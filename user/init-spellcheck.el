;;; init-spellcheck.el --- Spell-check configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)


(defconst -my:hunspell-path (executable-find "hunspell"))


(defun my:spell-dictionary (name &rest params)
  (let* ((word (plist-get params :word))
         (non-word (plist-get params :non-word))
         (other (plist-get params :other))
         (many-other (plist-get params :many-other?))
         (args (plist-get params :args))
         (extended-chars (plist-get params :extended-chars))
         (encoding (or (plist-get params :encoding) 'utf-8)))
    (list name word non-word other many-other args extended-chars encoding)))

(put 'my:spell-dictionary 'lisp-indent-function 'defun)


(defun my:spell-dictionary-ru (name dictname)
  (my:spell-dictionary name
    :word "[Ё-ё]"
    :non-word "[^Ё-ё]"
    :other "[-]"
    :many-other? nil
    :args (list "-d" dictname)
    :encoding 'utf-8))


(defun my:spell-dictionary-en (name dictname)
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
