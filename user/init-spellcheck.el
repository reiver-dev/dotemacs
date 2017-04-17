;;; init-spellcheck.el --- Spell-check configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)


(defconst -my:hunspell-path (executable-find "hunspell"))

(my:with-package ispell
  :if -my:hunspell-path
  :init (progn
          (setq-default ispell-program-name "hunspell"
                        flyspell-issue-message-flag nil)
          (setenv "DICPATH"
                  (expand-file-name "../share/hunspell"
                                    (file-name-directory -my:hunspell-path)))
          (setenv "DICTIONARY" "default"))
  :config
  (progn
    (add-to-list 'ispell-local-dictionary-alist
                 '("russian"
                   "[Ё-ё]"  ;; Word characters
                   "[^Ё-ё]" ;; Non-word characters
                   "[-]"    ;; Non-word characters in words
                   nil      ;; Many non-word chars?
                   ("-d" "ru_RU") ;; Args to use this dictionary
                   nil      ;; Ispell-related extenden char mode
                   utf-8))  ;; Charset checker uses
    (add-to-list 'ispell-local-dictionary-alist
                 '("english"
                   "[A-z]"
                   "[^A-z]"
                   "[']"
                   nil
                   ("-d" "en_US")
                   nil
                   iso-8859-1))
    (ispell-set-spellchecker-params)))

(provide 'init-spellcheck)

;;; init-spellcheck.el ends here
