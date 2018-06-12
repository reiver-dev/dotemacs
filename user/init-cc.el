;;; init-cc.el --- C/C++ configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(eval-when-compile
  (require 'init-defs)
  (require 'init-package)
  (require 'init-completion)
  (require 'init-filesystem))


(defconst my:c-style
  '("linux"
    (c-basic-offset . 4)
    (c-offsets-alist
     (innamespace . 0)
     (inline-open . 0)
     (inextern-lang . 0))))


(my:after cc-mode
  (c-add-style "reiver" my:c-style)
  (setq-default c-default-style
                '((c-mode . "reiver")
                  (c++-mode . "reiver")
                  (java-mode . "java")
                  (awk-mode . "awk")
                  (other . "gnu"))))


(defconst my:clang-msvc-compat-args
  (list
   "-target" "x86_64-pc-windows-msvc"
   "-fms-extensions"
   "-fms-compatibility"
   "-fms-compatibility-version=19"
   "-fdelayed-template-parsing"
   "-ferror-limit=0"
   "--driver-mode=cl"))


(defun my:fix-compile-args (options file directory)
  (let* ((options (if (stringp options)
                      (my:tokenize-args options)
                    options))
         (absfile (my:expand-file-name file directory))
         (commands '(next))
         (result (cons nil nil))
         (it result))
    (while options
      (while commands
        (let ((state (pop commands)))
          (cond
           ;; Move option into result
           ((eq state 'take)
            (setq it (setcdr it (list (car options)))
                  options (cdr options)))

           ;; Skip current option
           ((eq state 'skip)
            (setq options (cdr options)))

           ;; Skip 2 options
           ((eq state 'skip2)
            (setq options (cddr options)))

           ;; Append option to last result
           ((eq state 'concat)
            (setcar it (concat (car it) (car options)))
            (setq options (cdr options)))

           ;; Expand response file inplace
           ((eq state 'response)
            (let* ((rsp-path (my:expand-file-name
                              (substring (car options) 1) directory))
                   (rsp-args (with-temp-buffer
                               (insert-file-contents rsp-path)
                               (my:tokenize-args (buffer-string)))))
              (setq options (nconc rsp-args (cdr options)))))

           ;; Expand current option path inplace
           ((eq state 'expand)
            (setq options (cons (my:expand-file-name
                                 (car options) directory)
                                (cdr options))))

           ;; Iteration ends
           ((eq state 'break)
            (setq options nil))

           ;; Evaluate next option
           ((eq state 'next)
            ;; Every condition returns command list
            (setq
             commands
             (let ((opt (car options)))
               (cond
                ;; Compile-only flag and empty response file
                ((member opt '("-c" "/c" "@"))
                 '(skip))

                ;; Output file
                ((member opt '("-o" "/F"))
                 '(skip2))

                ;; Include dirs
                ((member opt '("-I" "/I"))
                 '(take expand take))

                ;; Output file joined
                ((and (> (length opt) 2)
                      (member (substring opt 0 2) '("-o" "/F")))
                 '(skip2))

                ;; Include dirs joined
                ((and (> (length opt) 2)
                      (member (substring opt 0 2) '("-I" "/I")))
                 (setq options (nconc (list
                                       (substring opt 0 2)
                                       (substring opt 2))
                                      (cdr options)))
                 '(take expand concat))

                ;; Expand response file
                ((string-prefix-p "@" opt)
                 '(response))

                ;; Include dir
                ((string= absfile (my:expand-file-name opt directory))
                 '(skip))

                ;; Options break
                ((string= "--" opt)
                 '(break))

                (t '(take))))))

           (t (error "Invalid state: %s" state)))))

      (when (not commands)
        (setq commands '(next))))

    (cdr result)))



(defun my:extract-include-dirs (options)
  (let* ((result (cons nil nil))
         (head result)
         (it options))
    (while it
      (let ((opt (car it))
            (rest (cdr it)))
        (when (or (string-prefix-p "-I" opt)
                  (string-prefix-p "/I" opt))
          (if (= 2 (length opt))
              (progn
                (setcdr head (cons (car rest) nil))
                (setq head (cdr head)
                      rest (cdr rest)))
            (progn
              (setcdr head (cons (substring opt 2) nil))
              (setq head (cdr head)))))
        (setq it rest)))
    (cdr result)))


(when (string= system-type "windows-nt")
  (defun my:-w32-lowercase-path (proc &rest args)
    "Bind `w32-downcase-file-names' and call PROC with ARGS."
    (let ((w32-downcase-file-names t))
      (apply proc args))))


(when (executable-find "clang")
  (my:with-package irony
    :ensure t
    :init
    (progn
      (my:after irony-cdb-json
        (when (fboundp 'my:-w32-lowercase-path)
          (advice-add 'irony-cdb-json--adjust-compile-options :around
                      #'my:-w32-lowercase-path))
        (fset 'irony-cdb-json--adjust-compile-options 'my:fix-compile-args))))

  (my:with-package company-irony
    :ensure t
    :init (my:after company
            (my:after irony
              (add-to-list 'company-backends 'company-irony))))

  (my:with-package flycheck-irony
    :ensure t
    :init (my:after flycheck
            (flycheck-irony-setup)))

  (my:with-package irony-eldoc
    :disabled t
    :ensure t
    :init (add-hook 'irony-mode-hook 'irony-eldoc)))


(my:with-package company-c-headers
  :ensure t
  :init (my:after company
          (add-to-list 'company-backends #'company-c-headers)))


(my:with-package cquery
  :ensure t)


(provide 'init-cc)

;;; init-cc.el ends here
