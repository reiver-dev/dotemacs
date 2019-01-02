;;; init-cc.el --- C/C++ configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'init-defs)
(require 'init-shlex)
(require 'init-package)
(require 'init-completion)
(require 'init-filesystem)


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


(defun my:cdb-fix-args (options file directory)
  "Process compilation database OPTIONS.

Args:
  OPTIONS   :: command line string or list of arguments;
  FILE      :: processed file;
  DIRECTORY :: working directory for the compiler.

Return new list of options.

Remove options that are useless for completion:
  * compile-only flags (-c, /c);
  * output files (-o, /F);
  * input file (equal to FILE).

Make include paths absolute (-I, /I)."
  (let* ((options (if (stringp options)
                      (my:sh-tokenize options)
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
                               (my:sh-tokenize (buffer-string)))))
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
  "Gather include paths from list of OPTIONS.

Include paths are -I and /I arguments with
value eiter attached to them or as seperate option."
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


(defun my:compiler-include-dirs-run (binary &optional language)
  "Collect compiler's include directories.
Performed by running compiler BINARY with specified LANGUAGE argument."
  (with-output-to-string
    (call-process binary nil standard-output nil
                  (if language (concat "-x" language) "")
                  "-E" "-v" "-")))



(my:with-package cquery
  :ensure t)


(provide 'init-cc)

;;; init-cc.el ends here
