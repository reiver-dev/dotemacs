;;; init-docker.el --- Docker configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-environ)
(require 'init-shlex)


(defun my:docker-machine--env ()
  "Apply docker-machine environment.
Equivalent to evaluation result of docker-machine env."
  (interactive)
  (let ((entries (my:env-parse-entries
                  (with-temp-buffer
                    (call-process
                     (executable-find "docker-machine") nil
                     (cons (current-buffer) nil) nil
                     "env" "--shell" "sh")
                    (buffer-string))
                  my:env-split-regex-export)))
    (dolist (pair entries)
      (setcdr pair (car (my:sh-tokenize (cdr pair)))))
    (dolist (pair entries)
      (setenv (car pair) (cdr pair)))
    entries))


(provide 'init-docker)

;;; init-docker.el ends here
