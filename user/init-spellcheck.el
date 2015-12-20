;;; init-spellcheck.el --- Spell-check configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-macro)

(my:with-package ispell
  :if (executable-find "hunspell")
  :config
  (progn
    (setq-default flyspell-issue-message-flag nil)
    (add-to-list 'ispell-local-dictionary-alist
                 '("russian-hunspell"
                   "[Ё-ё]"  ;; Word characters
                   "[^Ё-ё]" ;; Non-word characters
                   "[-]"    ;; Non-word characters in words
                   nil      ;; Many non-word chars?
                   ("-d" "ru_RU") ;; Args to use this dictionary
                   nil      ;; Ispell-related extenden char mode
                   utf-8))  ;; Charset checker uses
    (add-to-list 'ispell-local-dictionary-alist
                 '("english-hunspell"
                   "[A-z]"
                   "[^A-z]"
                   "[']"
                   nil
                   ("-d" "en_US")
                   nil
                   iso-8859-1))
    (setq-default ispell-program-name "hunspell")))

(provide 'init-spellcheck)

;;; init-spellcheck.el ends here
