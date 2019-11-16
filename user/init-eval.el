;;; init-eval.el --- Evaluating lisp expressions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'cl-extra)


(defun my:pp (object buffer)
  "Pretty print OBJECT to BUFFER."
  (with-current-buffer buffer
    (terpri buffer :ensure)
    (let ((initial (point)))
      (prin1 object buffer)
      (goto-char initial))
    (cl--do-prettyprint)
    (terpri buffer :ensure)))


(defun my:eval-expression (exp)
  "Evaluate EXP and pretty print value in the echo area.
If INSERT-VALUE is non-null insert the result into the current
buffer. NO-TRUNCATE and CHAR-PRINT-LIMIT are for compatibility with
default `eval-expression' function."
  (interactive
   (list (read--expression "Eval: ")))
  (push (eval exp lexical-binding) values)
  (let* ((buffer (get-buffer-create "*Eval expression result*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (my:pp (car values) buffer)
      (emacs-lisp-mode)
      (setq buffer-read-only nil)
      (set (make-local-variable 'font-lock-verbose) nil)
      (display-buffer buffer))))


(defun my:eval-last-expr ()
  "Evaluate last sexp before point."
  (let ((value
         (let ((debug-on-error elisp--eval-last-sexp-fake-value))
           (cons (eval (eval-sexp-add-defvars (elisp--preceding-sexp))
                       lexical-binding)
                 debug-on-error))))
    (unless (eq (cdr value) elisp--eval-last-sexp-fake-value)
      (setq debug-on-error (cdr value)))
    (car value)))


(defun my:eval-print-last-sexp ()
  "Evaluate sexp before point and pretty print it."
  (interactive)
  (let ((result (my:eval-last-expr))
        (buffer (current-buffer)))
    (terpri buffer :ensure)
    (let ((pos (point)))
      (my:pp result buffer)
      (terpri buffer :ensure)
      (goto-char pos))))



(provide 'init-eval)

;;; init-eval.el ends here
