;;; init-http.el --- http helpers  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module implements various http-related functionality
;;

;;; Code:

(require 'init-package)


(my:with-package request
  :ensure t)


(my:with-package restclient
  :ensure t
  :config
  (progn
    (my:after 'request
      (require 'init-restclient))
    (require 'request nil t)))


(provide 'init-http)

;;; init-http.el ends here
