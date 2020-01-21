;;; init-wmpkg.el --- Window management thirdparty  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)
(require 'init-keybind)
(require 'init-wm)
(require 'init-chars)


(my:with-package shackle
  :ensure t
  :init (shackle-mode t)
  :config
  (progn
    (add-to-list 'shackle-rules
                 '(" *NeoTree*" :align left :size 25))))


(my:with-package winum
  :ensure t
  :init
  (progn
    (setq-default winum-auto-assign-0-to-minibuffer t)
    (winum-set-keymap-prefix nil)
    (winum-mode +1))
  :config
  (progn
    (defun my:winum-action (num func &optional current)
      (let ((win (winum-get-window-by-number (abs num))))
        (if win
            (if current
                (my:apply-to-window func win (selected-window))
              (my:apply-to-window func win))
          (error "No window numbered %d" num))))
    (defun my:winum-update ()
      (winum--update)
      (force-mode-line-update t))
    (defun my:winum-dispatch ()
      (interactive)
      (let ((action #'select-window)
            (action-name "Select")
            current
            c exit)
        (while (not exit)
          (setq c (read-char (format "Window action: %s" action-name)))
          (cond
           ((eq c ?0) (winum-select-window-0) (setq exit t))
           ((my:char-digit-p c)
            (my:winum-action (- c ?0) action current)
            (setq exit t))
           ((my:any-of c ?h ?\C-h) (windmove-left) (setq exit t))
           ((my:any-of c ?j ?\C-j) (windmove-down) (setq exit t))
           ((my:any-of c ?k ?\C-k) (windmove-up) (setq exit t))
           ((my:any-of c ?l ?\C-l) (windmove-right) (setq exit t))
           ((eq c ?m) (setq action #'my:query-move-to-window
                            action-name "Move"
                            current t))
           ((eq c ?s) (setq action #'my:swap-windows
                            action-name "Swap"
                            current t))
           ((eq c ?x) (setq action #'my:delete-window
                            action-name "Delete"
                            current nil))
           ((eq c ?n) (my:detach-window) (setq exit t))
           ((eq c ?r) (my:resize-window) (setq exit t))
           ((eq c ?u) (my:winum-update) (setq exit 1))
           ((my:char-exit-p c)
            (setq exit t))))))
    (defun -my:winum--window-list ()
      "Return a list of interesting windows."
      (my:mapcan #'my:window-list
                 (sort (my:visible-frame-list) #'my:frame-sort-predicate)))
    (fset 'winum--window-list '-my:winum--window-list)
    (my:kmap
     ("C-c w" "C-c C-w" #'my:winum-dispatch))))


(my:with-package ace-window
  :disabled t
  :ensure t
  :init
  (progn
    (setq-default aw-scope 'visible
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
          '((?w aw-switch-to-window "Switch to Window")
            (?m my:aw-put-window "Move Window")
            (?n my:detach-window)
            (?r my:resize-window)
            (?h windmove-left) (?\C-h windmove-left)
            (?j windmove-down) (?\C-j windmove-down)
            (?k windmove-up) (?\C-k windmove-up)
            (?l windmove-right) (?\C-l windmove-right)
            (?s aw-swap-window "Swap Window")
            (?x aw-delete-window "Delete Window")
            (?c aw-split-window-fair "Split Fair Window")
            (?v aw-split-window-vert "Split Vert Window")
            (?b aw-split-window-horz "Split Horz Window")
            (?i delete-other-windows "Maximize Window")
            (?o delete-other-windows)
            (?& aw-show-dispatch-help)
            (?? aw-show-dispatch-help)))))


(provide 'init-wmpkg)

;;; init-wmpkg.el ends here
