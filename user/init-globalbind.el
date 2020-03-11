;;; init-globalbind.el --- Global keybindings -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'init-keybind)
(require 'init-edit)
(require 'init-wm)


;; Redefine esc
;; however esc will not break from hangs like C-g
(global-unset-key (kbd "ESC ESC ESC"))
(global-set-key (kbd "C-M-g")           #'keyboard-escape-quit)
(global-set-key (kbd "<escape>")        #'keyboard-quit)
(my:minibuffer-set-key (kbd "<escape>") #'my:minibuffer-keyboard-quit)

;; Never quit so fast
(global-unset-key (kbd "C-x C-c"))
;; We have suspend at `C-x C-z'
(global-unset-key (kbd "C-z"))
;; Use that for instant bindings
(global-unset-key (kbd "<menu>"))
;; Free face modification group as it is never used
(global-unset-key (kbd "M-o"))

(mapc #'my:global-unset-command
      '(upcase-region downcase-region capitalize-region))

(my:kmap*
 (current-global-map)

 ;; Mark
 ([remap exchange-point-and-mark] #'my:exchange-point-and-mark) ; "C-x C-x"
 ("C-x m" #'my:push-mark-no-activate)

 ;; Vim's word jumping
 ("M-a" #'my:backward-same-syntax)
 ("M-e" #'my:forward-same-syntax)

 ;; Editing
 ("C-w" #'my:kill-region-or-word)
 ("C-S-w" #'kill-region)

 ("C-h" #'delete-backward-char)
 ("M-h" #'my:backward-delete-word)

 ([remap capitalize-word] #'capitalize-dwim)
 ([remap upcase-word] #'upcase-dwim)
 ([remap downcase-word] #'downcase-dwim)

 ("M-<backspace>"
  [remap backward-kill-word] #'my:backward-delete-word)
 ("M-<delete>"
  [remap kill-word] #'my:delete-word)

 ("C-<backspace>" #'my:kill-line-to-indent)
 ("C-<delete>"    #'kill-line)

 ("M-k" #'kill-whole-line)
 ("M-j" #'my:join-line)
 ("M-o" #'my:open-line)

 ;; Buffers
 ("C-x k" #'my:kill-buffer)
 ("C-x K" #'my:kill-buffer-and-window)
 ("C-x C-c" #'switch-to-buffer)

 ([remap list-buffers] #'ibuffer)       ; "C-x C-b"

 ;; Window management
 ("C-c w h" "C-c w <left>" #'windmove-left)
 ("C-c w j" "C-c w <down>" #'windmove-down)
 ("C-c w k" "C-c w <up>" #'windmove-up)
 ("C-c w l" "C-c w <right>" #'windmove-right)

 ("C-c w r" #'my:resize-window)
 ("C-c w n" #'my:detach-window))

(my:kmap
 ;; To always keep M-x available
 ("M-X" #'execute-extended-command)

 ;; Jumping
 ("C-o f" #'ff-find-other-file)
 ("C-o i" #'imenu)

 ("<f9>" "C-o C-d" #'my:toggle-window-dedicated)
 ("<f5>" "C-o C-r" #'revert-buffer)
 ("C-o ?" help-map))


(provide 'init-globalbind)

;;; init-globalbind.el ends here
