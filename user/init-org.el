;;; init-org.el --- Org-Mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This files provides customizations and
;; function used in org-mode

;;; Code:

(eval-when-compile
  (require 'init-package)
  (require 'init-list))


(setq-default org-src-fontify-natively t
              org-src-tab-acts-natively t
              org-src-window-setup 'current-window
              org-startup-folded nil
              org-use-sub-superscripts (quote {})
              org-export-with-sub-superscripts (quote {})
              ;; set maximum indentation for description lists
              org-list-description-max-indent 5
              ;; prevent demoting heading also shifting text inside sections
              org-adapt-indentation nil)


(defconst my:xhtml-style-begin
  "<style type=\"text/css\">\n<!--/*--><![CDATA[/*><!--*/\n"
  "Opening tag with CDATA for xhtml css.")

(defconst my:xhtml-style-end
  "/*]]>*/-->\n</style>\n"
  "Closing tag with CDATA for xhtml css.")

(defconst my:xhtml-script-begin
  "<script type=\"text/javascript\">\n<!--/*--><![CDATA[/*><!--*/\n"
  "Opening tag with CDATA for xhtml script")

(defconst my:xhtml-script-end
  "/*]]>*///-->\n</script>\n"
  "Closing tag with CDATA for xhtml script")

(defconst my:html-style
  ".outline-2 { margin-left: 2em; }
.outline-3 { margin-left: 2em; }
.outline-4 { margin-left: 2em; }
.outline-5 { margin-left: 2em; }
.outline-6 { margin-left: 2em; }
h1 { font-size: 1.5em; font-weight: bold; }
h2 { font-size: 1.5em; font-weight: normal; }
h3 { font-size: 1.25em; font-weight: normal; }
h4 { font-size: 1.12em; font-weight: normal; }
h5 { font-size: 1em; font-weight: normal; }
h6 { font-size: 1em; font-weight: normal; }
header h1 { font-size: 2em; font-weight: bold; }
table { border-top: 0.16rem solid; border-bottom: 0.16rem solid; }
table thead { border-bottom: 0.10rem solid; }
td, th { padding-left: 1em; padding-right: 1em; }
.org-svg { width: auto; max-width: 100%; }
#table-of-contents { font-variant: small-caps; }
"
  "Additional style to `org-html-style-default'")


(setq-default org-export-headline-levels 6
              org-html-toplevel-hlevel 1
              org-html-doctype "xhtml5"
              org-html-html5-fancy t
              org-html-postamble nil
              org-html-head (concat my:xhtml-style-begin
                                    my:html-style
                                    my:xhtml-style-end))


(my:after ox-html
  (setf (alist-get 'bold org-html-text-markup-alist) "<strong>%s</strong>"
        (alist-get 'italic org-html-text-markup-alist) "<em>%s</em>"))


(defun -my:org-add-language (&rest langs)
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append (my:remove-if (lambda (l) (memq (car l) langs))
                           org-babel-load-languages)
             (mapcar (lambda (l) (cons l t)) langs))))


(my:after org
  (-my:org-add-language 'python 'plantuml))


(my:after plantuml-mode
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))


(provide 'init-org)

;;; init-org.el ends here
