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
        (package-install package)))))


(defun my:require-when-compile (feature)
  "Require or load FEATURE if file is currently compiled.
Meant to be used in macros."
  (or (not (boundp 'byte-compile-current-file))
      (not byte-compile-current-file)
      (cond
       ((symbolp feature)
        (require feature nil :no-error))
       ((stringp feature)
        (load feature :no-message :no-error)))))


(defun -my:is-function-form (form)
  (and (= (length form) 1)
       (= (length (car form)) 2)
       (let ((wrapper (caar form)))
         (or (eq wrapper 'function)
             (eq wrapper 'quote)))))


(defun -my:at-least-one (value)
  "Normalize VALUE to quoted or string."
  (while (or (eq 'quote (car-safe value))
             (eq 'backquote (car-safe value)))
    (setq value (eval value :lexical)))
  (if (consp value)
      value
    (cons value nil)))


(defmacro my:measure-time (name &rest body)
  "Measure the time it takes for NAME to evaluate BODY."
  `(let ((--time-- (current-time)))
     ,@body
     (message "Package: %s %.06f" ,name (float-time (time-since --time--)))))


(defmacro -my:run-after (count &rest body)
  "Create closure from BODY to be executed after COUNT funcalls."
  (let ((var '--times--))
    `(let ((,var ,count))
       (lambda ()
         (setq ,var (1- ,var))
         (if (= 0 ,var) ,@body)))))


(defmacro -my:maybe-no-warnings (flag &rest body)
  "Wrap BODY to lamba. Use `with-no-warnings' if FLAG is set."
  (declare (indent 1) (debug t))
  (if flag
      `(lambda () ,@body)
    `(lambda ()
       (with-no-warnings ,@body))))


(defmacro my:after (items &rest body)
  "Evaluate BODY after ITEMS are loaded.
ITEMS might be a symbol, string or list of these."
  (declare (indent 1) (debug t))
  (let* ((items (-my:at-least-one items))
         (loaded t))
    (dolist (item items)
      (setq loaded (and (my:require-when-compile item) loaded)))
    (if (= 1 (length items))
        `(eval-after-load ,(macroexp-quote (car items))
           (-my:maybe-no-warnings ,loaded ,@body))
      `(let* ((routine (-my:maybe-no-warnings ,loaded ,@body))
              (guarded (-my:run-after ,(length items)
                                      (funcall routine))))
         ,@(mapcar (lambda (item)
                     (list 'eval-after-load (macroexp-quote item) 'guarded))
                   items)))))


(defmacro my:add-hook (hook &rest body)
  "Execute BODY at HOOK.
Wrap BODY in `lambda' or use as is if function or symbol."
  (declare (indent 1) (debug t))
  (if (-my:is-function-form body)
      `(add-hook ,hook ,(car body))
    `(add-hook ,hook (lambda () ,@body))))


(defmacro my:add-hook-in (hook seconds &rest body)
  (declare (indent 2) (debug t))
  (if (-my:is-function-form body)
      `(add-hook ,hook
                 (lambda ()
                   (run-with-idle-timer ,seconds nil ,(car body))))
    `(add-hook ,hook
               (lambda ()
                 (run-with-idle-timer ,seconds nil (lambda () ,@body))))))


(defmacro my:after-init (&rest body)
  (declare (indent 0) (debug t))
  `(my:add-hook 'init:startup-hook ,@body))


(defmacro my:after-init-in (seconds &rest body)
  (declare (indent 1) (debug t))
  `(my:add-hook-in 'init:startup-hook ,seconds ,@body))


(defmacro my:with-package (name &rest args)
  "Helper macro for package configuration.
Performs various tasks around package with NAME
depending on property list pairs in ARGS"
  (unless (plist-get args :disabled)
    (let* ((ensure (plist-get args :ensure))
           (package (if (or (not ensure) (eq t ensure)) name ensure))
           (package-name (symbol-name package))
           (init (plist-get args :init))
           (defer (let ((d (plist-get args :defer)))
                    (when d
                      (if (numberp d) d 10))))
           (config (plist-get args :config))
           (condition (plist-get args :if))
           (lpath (plist-get args :load-path))
           (result
            `(with-demoted-errors
                 ,(concat "Error loading " package-name ": %S")
               ,@(delq
                  nil
                  (list
                   (when lpath
                     `(add-to-list (quote load-path) ,lpath))
                   (when ensure
                     `(my:require-package (quote ,package)))
                   (when init
                     (if defer
                         `(my:after-init-in ,defer ,init)
                       init))
                   (when config
                     `(my:after (quote ,name)
                        ,@(macroexp-unprogn config))))))))
      (if condition
          `(if ,condition ,result nil)
        result))))



(put 'my:with-package 'lisp-indent-function 'defun)

(provide 'init-package)

;;; init-package.el ends here
