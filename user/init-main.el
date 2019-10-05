;;; init-main.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-defs)
(require 'init-package)
(require 'init-keybind)
(require 'init-edit)
(require 'init-export)
(require 'init-ui)
(require 'init-wm)
(require 'init-globalbind)
(require 'init-filesystem)
(require 'init-shlex)
(require 'init-largefile)
(require 'init-font)

(require 'init-environ)

(require 'init-dired)
(require 'init-compile)
(require 'init-comint)
(require 'init-eshell)
(require 'init-compare)
(require 'init-vcs)

(my:with-package flymake
  :ensure t
  :config (require 'init-flymake))

(require 'init-pkgs)
(require 'init-completion)
(require 'init-spellcheck)

(require 'init-wmpkg)
(require 'init-parens)
(require 'init-http)
(require 'init-org)

(require 'init-cc)
(require 'init-python)
(require 'init-clojure)
(require 'init-rust)
(require 'init-tex)
(require 'init-haskell)
(require 'init-lsp)
(require 'init-process)

(require 'init-misc)
(require 'init-lang)

(provide 'init-main)

;;; init-main.el ends here
