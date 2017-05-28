;;; init-package.el --- Helper macros for package init -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el
(defun my:require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (my:require-package package min-version t)))))


(defun my:macro-require (form)
  "Try to load FORM.
Form can be symbol, string or (quote form)."
  (if (with-no-warnings
        (cond ((symbolp form) (require form nil t))
              ((stringp form) (load form t t))
              ((and (consp form) (eq (car form) 'quote))
               (require (car (cdr form)) nil t))
              (t (error "Macro-require: cannot load form %s" form))))
      (progn (message "Macro-require: loaded %s" form) t)
    (progn (message "Macro-require: failed to load %s" form) nil)))


(defmacro my:with-eval-after-load (file &rest body)
  "Wait until FILE loaded to execute BODY.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See `eval-after-load'
for more details about the different forms of FILE and their semantics."
  (declare (indent defun) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (my:macro-require file))
         'progn 'with-no-warnings)
    (eval-after-load ,file (lambda () ,@body))))


(defmacro my:with-package (name &rest args)
  "Helper macro for package configuration.
Performs various tasks around package with NAME
depending on property list pairs in ARGS"
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
           (result
            `(with-demoted-errors
                 ,(concat "Error loading " (symbol-name package) ": %S")
               ,@(delq
                  nil
                  (list
                   (when lpath
                     `(add-to-list (quote load-path) ,lpath))
                   (when ensure
                     `(my:require-package (quote ,package)))
                   (when config
                     `(my:with-eval-after-load (quote ,name)
                        ,@(macroexp-unprogn config)))
                   (when init
                     (if defer
                         `(run-with-idle-timer ,defer nil (lambda () ,init))
                       init)))))))
      (if condition
          `(if ,condition ,result nil)
        result))))



(put 'my:with-package 'lisp-indent-function 'defun)

(provide 'init-package)

;;; init-package.el ends here
