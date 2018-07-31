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


(defun -my:macroexp-unwrap (body)
  "Unwrap BODY until it becomes list if single form."
  (let ((exp body)
        (counter 0))
    (while (and (consp exp) (not (cdr exp)))
      (setq exp (car exp)
            counter (1+ counter)))
    (when (> counter 2)
      (warn "Deep wrapped form: %S" body))
    (if (consp exp)
        exp
      (cons exp nil))))


(defun -my:macroexp-progn (body)
  "Wrap BODY form list in `progn' if needed."
  (let ((body (-my:macroexp-unwrap body)))
    (if (cdr body)
        (if (consp (car body))
            `(progn ,@body)
          body)
      (car body))))


(defun -my:macroexp-fun (body)
  "Wrap BODY with `lambda' without arguments.
Unwrap if BODY is single `function' or `quote' function expression."
  (let ((body (-my:macroexp-unwrap body)))
    (if (and (= (length body) 2)
             (memq (car body) '(function quote))
             (atom (cdr-safe (cdr body))))
        body)
    `(lambda () ,@(macroexp-unprogn
                   (-my:macroexp-progn body)))))



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
  "Wrap BODY to lambda. Use `with-no-warnings' if FLAG is set."
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
              (guarded (-my:run-after ,(length items) (funcall routine))))
         ,@(mapcar (lambda (item)
                     (list 'eval-after-load (macroexp-quote item) 'guarded))
                   items)))))


(defmacro my:add-hook (hook &rest body)
  "Execute BODY at HOOK.
Wrap BODY in `lambda' or use as is if function or symbol."
  (declare (indent 1) (debug t))
  `(add-hook ,hook ,(-my:macroexp-fun body)))


(defmacro my:add-hook-in (hook seconds &rest body)
  "Execute BODY at in SECONDS seconds after HOOK run."
  (declare (indent 2) (debug t))
  `(add-hook ,hook
             (lambda ()
               (run-with-idle-timer ,seconds nil ,(-my:macroexp-fun body)))))


(defmacro my:after-init (&rest body)
  "Execute BODY at `init:startup-hook' hook."
  (declare (indent 0) (debug t))
  `(my:add-hook 'init:startup-hook ,@body))


(defmacro my:after-init-in (seconds &rest body)
  "Execute BODY in SECONDS seconds after `init:startup-hook'."
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
