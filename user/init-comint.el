;;; init-comint.el --- Comint config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'init-package)
(require 'init-keybind)


(setq-default comint-prompt-read-only t
              comint-process-echoes t
              comint-input-ignoredups t
              comint-scroll-show-maximum-output t
              comint-scroll-to-bottom-on-input t
              comint-scroll-to-bottom-on-output nil
              comint-buffer-maximum-size 8196)


(defvar-local -my:comint-output-chunks nil)
(defvar-local my:comint-max-line-length 255)

(my:after comint

  (defun -my:comint-filter-long-lines (string)
    "Accumulate input STRING chunks and truncate long lines."
    (push string -my:comint-output-chunks)
    (if (not (string-match comint-prompt-regexp string))
        ""
      (let* ((out (mapconcat
                   #'identity (nreverse -my:comint-output-chunks) ""))
             (split-str (split-string out "\n"))
             (max-len (* 2 my:comint-max-line-length))
             (disp-left (round (* (/ 1.0 3) my:comint-max-line-length)))
             (disp-right disp-left)
             (truncated (mapconcat
                         (lambda (x)
                           (if (> (length x) max-len)
                               (concat (substring x 0 disp-left)
                                       " ... (*TRUNCATED*) ... "
                                       (substring x (- disp-right)))
                             x))
                         split-str "\n")))
        (setq -my:comint-output-chunks nil)
        truncated)))

  (defun -my:comint-text-readonly (_text)
    (let ((inhibit-read-only t)
          (output-end (process-mark (get-buffer-process (current-buffer)))))
      (put-text-property comint-last-output-start output-end 'read-only t)))

  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (add-hook 'comint-output-filter-functions '-my:comint-text-readonly)
  (add-hook 'comint-preoutput-filter-functions '-my:comint-filter-long-lines)

  ;; We have `my:kill-region-or-word' already
  (my:kmap* comint-mode-map ("C-c C-w" nil)))


(my:after shell
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))


(provide 'init-comint)

;;; init-comint.el ends here
