;;; init-wmpkg.el --- Window management thirdparty  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-keybind)
(require 'init-wm)


(my:with-package shackle
  :ensure t
  :init (shackle-mode t)
  :config
  (progn
    (add-to-list 'shackle-rules
                 '(" *NeoTree*" :align left :size 25))))


(my:with-package ace-window
  :ensure t
  :init
  (progn
    (setq aw-scope 'visible
          aw-dispatch-always t
          aw-background nil)
    (my:kmap ("C-c w" "C-c C-w" #'ace-window)))
  :config
  (progn
    (defun my:aw-put-window (window)
      (my:apply-to-window
       #'my:query-move-to-window window (selected-window)))
    (fset 'aw-window-list 'my:visible-window-list)
    (setq aw-dispatch-alist
          '((?w aw-switch-to-window " Ace - Window")
            (?m my:aw-put-window " Ace - Move Window")
            (?n my:detach-window)
            (?r my:resize-window)
            (?h windmove-left) (?j windmove-down)
            (?k windmove-up) (?l windmove-right)
            (?s aw-swap-window " Ace - Swap Window")
            (?x aw-delete-window " Ace - Delete Window")
            (?c aw-split-window-fair " Ace - Split Fair Window")
            (?v aw-split-window-vert " Ace - Split Vert Window")
            (?b aw-split-window-horz " Ace - Split Horz Window")
            (?i delete-other-windows " Ace - Maximize Window")
            (?o delete-other-windows)))))


(provide 'init-wmpkg)

;;; init-wmpkg.el ends here
