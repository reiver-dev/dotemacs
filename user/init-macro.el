;;; init-macro.el --- Helper macros for package init -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defmacro my:add-to (l el)
  `(setq ,l (cons ,el ,l)))

;; For autoload byte-compiling
;; http://www.lunaryorn.com/2013/06/25/introducing-with-eval-after-load.html
(defmacro my:with-eval-after-load (feature &rest forms)
  "Suppress warnings aroud `with-eval-after-load'"
  (declare (indent 1) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "Eval-After: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro my:with-package (name &rest args)
  (declare (indent 1))
  (unless (plist-get args :disabled)
    (let* ((ensure (plist-get args :ensure))
           (package (if (or (not ensure) (eq t ensure)) name ensure))
           (init (plist-get args :init))
           (defer (let ((d (plist-get args :defer)))
                    (when d
                      (if (numberp d) d 10))))
           (config (plist-get args :config))
           (condition (plist-get args :if))
           (lpath (plist-get args :load-path))
           (result '()))

      (when init
        (my:add-to
         result
         (if defer
             `(run-with-idle-timer ,defer nil (lambda () ,init))
           init)))

      (when config
        (my:add-to
         result
         `(my:with-eval-after-load ,name ,@(macroexp-unprogn config))))

      (when ensure
        (my:add-to result `(when (not (package-installed-p ',package))
                             (package-install ',package))))

      (when lpath
        (my:add-to result `(add-to-list 'load-path ,lpath)))

      (let ((r `(with-demoted-errors
                    ,(concat "Error loading " (symbol-name package) ": %s")
                  ,@result)))
        (if condition
            `(if ,condition ,r nil)
          r)))))

(put 'my:with-package 'lisp-indent-function 'defun)

(provide 'init-macro)

;;; init-macro.el ends here
