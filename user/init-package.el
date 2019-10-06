;;; init-package.el --- Helper macros for package init -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'package))


(defvar package-archive-contents)
(defvar package-selected-packages)
(declare-function package-installed-p "package")


(defvar my:selected-packages-list nil
  "List of packages that are requiested by `my:with-package'.")


(defun my:select-package (package)
  "Add PACKAGE symbol to `my:selected-packages-list'."
  (add-to-list 'my:selected-packages-list package))


(defun my:selected-packages-finalize nil
  "Add `my:selected-packages-list' to `package-selected-packages'."
  (interactive)
  (let* ((added 0)
         (already-selected package-selected-packages)
         (result already-selected))
    (dolist (package my:selected-packages-list)
      (unless (memq package already-selected)
        (setq result (cons package result)
              added (1+ added))))
    (when (> 0 added)
      (let ((save-silently inhibit-message))
        (customize-save-variable 'package-selected-packages result
                                 "From my:selected-packages-list")))))


(defun my:package-table ()
  "Collect list of currently installed package names."
  (let (result)
    (dolist (pkg-desc package-alist)
      (push (car pkg-desc) result))
    result))


(defun my:package-dir (package)
  "Discover installed PACKAGE directory."
  (interactive
   (list
    (intern-soft (completing-read
                  "Package: " (my:package-table)
                  nil t))))
  (let ((desc (cadr (assq package package-alist)))
        result)
    (when desc
      (setq result (package-desc-dir desc)))
    (if (called-interactively-p 'interactive)
        (message "%s" result)
      result)))


;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el
(defun my:require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (my:select-package package)
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


(defmacro my:require (feature)
  "Require or load FEATURE if file is currently compiled.
Macro version of `my:require-when-compile'."
  (my:require-when-compile feature))


(defun -my:macroexp-progn (body)
  "Wrap BODY form list in `progn' if needed."
  (cond
   ((atom body) body)
   ((cdr body) (if (consp (car body))
                   `(progn ,@body)
                 body))
   (t (car body))))


(defun -my:macroexp-fun-1 (body)
  "Wrap BODY with `lambda' without arguments.
Meant to be called by `-my:macroexp-fun'"
  (cond
   ((atom body) `(quote ,body))
   ((and (= (length body) 2)
         (memq (car body) '(function quote))
         (not (consp (cdr-safe (cdr body)))))
    body)
   ((and (symbolp (car body)) (not (cdr body)))
    `(function ,(car body)))
   (t `(lambda () ,@(macroexp-unprogn body)))))


(defun -my:macroexp-fun (body)
  "Wrap BODY with `lambda' without arguments.
Unwrap if BODY is single `function' or `quote' function expression."
  (-my:macroexp-fun-1 (-my:macroexp-progn body)))


(defun -my:macroexp-at-least-one (value)
  "Normalize VALUE to quoted or string."
  (while (memq (car-safe value) '(quote backquote))
    (setq value (eval value :lexical)))
  (if (consp value)
      value
    (cons value nil)))


(defmacro my:measure-time (name &rest body)
  "Measure the time it takes for NAME to evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ((--time-- (current-time)))
     (message "Package: %s start" ,name)
     (prog1 ,(-my:macroexp-progn body)
       (message "Package: %s done %.06f"
                ,name (float-time (time-since --time--))))))


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
  (let* ((items (-my:macroexp-at-least-one items))
         (loaded t))
    (dolist (item items)
      (setq loaded (and (my:require-when-compile item) loaded)))
    (if (= 1 (length items))
        (let ((item (macroexp-quote (car items))))
          `(eval-after-load ,item
             (-my:maybe-no-warnings ,loaded ,@body)))
      (let ((routine-sym (make-symbol "routine"))
            (guarded-sym (make-symbol "guarded")))
        `(let* ((,routine-sym (-my:maybe-no-warnings ,loaded ,@body))
                (,guarded-sym (-my:run-after ,(length items)
                                             (funcall ,routine-sym))))
           ,@(mapcar
              (lambda (item)
                (list 'eval-after-load (macroexp-quote item)
                      guarded-sym))
              items))))))


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
           (defer (plist-get args :defer))
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
                     (cond
                      ((numberp defer)
                       `(my:after-init-in ,defer
                          (my:measure-time ,(format "%s (init %d)" name defer)
                            ,init)))
                      (defer
                        `(my:after-init
                           (my:measure-time ,(format "%s (init t)" name)
                             ,init)))
                      (t
                       `(my:measure-time ,(format "%s (init)" name)
                          ,init))))
                   (when config
                     `(my:after (quote ,name)
                        (my:measure-time ,(format "%s (config)" name)
                          ,@(macroexp-unprogn config)))))))))
      (if condition
          `(if ,condition ,result nil)
        result))))



(put 'my:with-package 'lisp-indent-function 'defun)

(provide 'init-package)

;;; init-package.el ends here
