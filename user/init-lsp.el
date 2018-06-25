;;; init-lsp.el --- LSP configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module configures Language server protocol

;;; Code:


(my:with-package lsp-mode
  :ensure t
  :config (progn
            (require 'lsp-imenu)
            (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)))


(my:with-package company-lsp
  :ensure t
  :init (my:after (lsp-mode company)
          (add-to-list 'company-backends 'company-lsp)))


(defconst my:lsp-kinds-abbrev-alist
  '(("Array" . "arr")
    ("Boolean" . "bool")
    ("Class" . "cls")
    ("Color" . "color")
    ("Constant" . "const")
    ("Constructor" . "ctor")
    ("Enum" . "enum")
    ("EnumMember" . "embr")
    ("Event" . "ev")
    ("Field" . "field")
    ("File" . "file")
    ("Folder" . "dir")
    ("Function" . "fun")
    ("Interface" . "iface")
    ("Key" . "key")
    ("Keyword" . "kwd")
    ("Method" . "mtd")
    ("Module" . "mod")
    ("Namespace" . "ns")
    ("Null" . "null")
    ("Number" . "num")
    ("Object" . "obj")
    ("Operator" . "")
    ("Package" . "pkg")
    ("Property" . "prop")
    ("Reference" . "ref")
    ("Snippet" . "snip")
    ("String" . "str")
    ("Struct" . "struct")
    ("Text" . "text")
    ("TypeParameter" . "tpar")
    ("Unit" . "unit")
    ("Value" . "val")
    ("Variable" . "var"))
  "Abbreviations for lsp constants")


(defconst my:lsp-completion-item-kind
  `((1 . "Text")
    (2 . "Method")
    (3 . "Function")
    (4 . "Constructor")
    (5 . "Field")
    (6 . "Variable")
    (7 . "Class")
    (8 . "Interface")
    (9 . "Module")
    (10 . "Property")
    (11 . "Unit")
    (12 . "Value")
    (13 . "Enum")
    (14 . "Keyword")
    (15 . "Snippet")
    (16 . "Color")
    (17 . "File")
    (18 . "Reference")
    (19 . "Folder")
    (20 . "EnumMember")
    (21 . "Constant")
    (22 . "Struct")
    (23 . "Event")
    (24 . "Operator")
    (25 . "TypeParameter"))
  "LSP namespace CompletionItemKind")


(defconst my:lsp-symbol-kind
  '((1 . "File")
    (2 . "Module")
    (3 . "Namespace")
    (4 . "Package")
    (5 . "Class")
    (6 . "Method")
    (7 . "Property")
    (8 . "Field")
    (9 . "Constructor")
    (10 . "Enum")
    (11 . "Interface")
    (12 . "Function")
    (13 . "Variable")
    (14 . "Constant")
    (15 . "String")
    (16 . "Number")
    (17 . "Boolean")
    (18 . "Array")
    (19 . "Object")
    (20 . "Key")
    (21 . "Null")
    (22 . "EnumMember")
    (23 . "Struct")
    (24 . "Event")
    (25 . "Operator")
    (26 . "TypeParameter"))
  "LSP namespace SymbolKind")


(defconst my:lsp-completion-item-kind-abbrev
  (mapcar (lambda (item)
            (cons (car item)
                  (cdr (assoc (cdr item) my:lsp-kinds-abbrev-alist))))
          my:lsp-completion-item-kind)
  "Abbreviated LSP namespace CompletionItemKind")


(defconst my:lsp-symbol-kind-abbrev
  (mapcar (lambda (item)
            (cons (car item)
                  (cdr (assoc (cdr item) my:lsp-kinds-abbrev-alist))))
          my:lsp-symbol-kind)
  "Abbreviated LSP namespace SymbolKind")


(provide 'init-lsp)

;;; init-lsp.el ends here

