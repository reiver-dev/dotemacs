;;;;;;;;;;
;; MAIN ;;
;;;;;;;;;;

;; my folders ;;
(ignore-errors
  (defconst my:user-dir (file-name-directory load-file-name)))

(defconst my:modules-dir (expand-file-name "modules" my:user-dir))
(defconst my:snippets-dir (expand-file-name "snippets" my:user-dir))

(dolist (dir (list my:modules-dir my:snippets-dir))
  (unless (file-directory-p dir)
    (make-directory dir)))

;; package dir ;;
(let ((default-directory my:modules-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; backup and autosave ;;
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;;;;;;;;;;;;;;;
;; Appearance ;;
;;;;;;;;;;;;;;;;

;; Toolbars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; Modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Make modeline flat
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line-highlight nil
                    :box nil
                    :inverse-video t)

;; basic
(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              visible-bell t)

;; Fringe
(setq-default indicate-buffer-boundaries t)

(setq-default
 ;; three lines at a time
 mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))
 ;; don't accelerate scrolling
 mouse-wheel-progressive-speed nil)

;; Text behavior
(setq-default shift-select-mode nil
              sentence-end-double-space nil)

;; Indentation
(setq-default indent-tabs-mode nil)

(defun my:prog-mode-setup ()
  "Basic settings for prog and other modes"
  (setq show-trailing-whitespace t
        truncate-lines t))

(add-hook 'prog-mode-hook 'my:prog-mode-setup)
(add-hook 'nxml-mode 'my:prog-mode-setup)


;;;;;;;;;;
;; Util ;;
;;;;;;;;;;

;; For convenient bindings
(defvar my:bindings-mode-map
  (make-sparse-keymap)
  "Keymap for `my:bindings-mode'")

(define-minor-mode my:bindings-mode
  "My key bindings
\\{my:bindings-mode-map}"
  :global t
  :keymap my:bindings-mode-map)

(my:bindings-mode t)

(defmacro my:kmap* (keymap &rest bindings)
  "Macro for binding keys to `keymap'
should get (kbd1 kbd2 .. function) as arguments"
  (let (result)
    (dolist (bind bindings)
      (let ((keys (butlast bind))
            (func (last bind)))
        (dolist (key keys)
          (setq result
                (cons
                 `(define-key ,keymap (kbd ,key) ,@func)
                 result)))))
    (if (< 1 (length result))
        `(progn ,@result)
      (car result))))

(defmacro my:kmap (&rest bindings)
  "Macro sets bindins to \\[my:bindings-mode-map] keymap"
  (if (stringp (car bindings))
      `(my:kmap* my:bindings-mode-map ,bindings)
    `(my:kmap* my:bindings-mode-map ,@bindings)))

(defun my:minibuffer-set-key (key command)
  "Binds key to all common minibuffer states"
  (dolist (m (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map))
    (define-key m key command)))

(defun my:move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

;; For project settings
(defun my:dir-locals-path (&optional relative)
  "Finds directory local (.dir-locals.el) settings location"
  (let* ((current (if (stringp buffer-file-name)
                      buffer-file-name
                    default-directory))
         (dl-dir (locate-dominating-file current ".dir-locals.el")))
    (unless (stringp dl-dir)
      (error ".dir-locals.el not found"))
    (file-name-as-directory
     (if (stringp relative)
         (expand-file-name relative dl-dir)
       (expand-file-name dl-dir)))))

(defmacro my:with-local-dir (relative &rest body)
  "Macro for running commands from location relative to \".dir-locals.el\""
  `(let ((default-directory
           (my:dir-locals-path ,relative)))
     ,@body))

;; For autoload byte-compiling
;; http://www.lunaryorn.com/2013/06/25/introducing-with-eval-after-load.html
(defmacro my:eval-after (feature &rest forms)
  "Suppress warnings aroud `with-eval-after-load'"
  (declare (indent 1) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "Eval-After: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

;; For window management
(defun my:one-window-p (&optional window)
  "Like `one-window-p', but correctly works with other frame selected"
  (let ((frame (window-frame window)))
    (eq window
        (next-window window 'no-minibuf frame))))

(defun my:move-window-to-other-window (target-window current-window side)
  "Moves buffer to other window's split"
  (when (eq target-window current-window)
    (error "Can't move to same window"))
  (let ((current-frame (window-frame current-window))
        (target-frame  (window-frame target-window))
        (buffer        (window-buffer current-window)))
    (unless (frame-parameter target-frame 'unsplittable)
      (if (my:one-window-p current-window)
          (delete-frame current-frame)
        (delete-window current-window))
      (let ((new-window (split-window target-window nil side)))
        (set-window-buffer new-window buffer)
        (select-window new-window)))))


;;;;;;;;;;;;;;;;;
;; Interactive ;;
;;;;;;;;;;;;;;;;;

(defun my:push-mark-no-activate ()
  "Calls `push-mark' like `push-mark-command' but withoug activation"
  (interactive)
  (push-mark))

;; Do not activate mark during jump
(defun my:exchange-point-and-mark (&optional ARG)
  "Inverse `exchange-point-and-mark' prefix argument"
  (interactive "P")
  (exchange-point-and-mark (unless ARG t)))

(defun my:kill-line-to-indent ()
  "Kills line backward (opposite to `kill-line')
and indents after that"
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun my:kill-region-or-word ()
  "Call `kill-region' or backward `kill-word'
depending on whether or not a region is selected."
   (interactive)
   (if (and transient-mark-mode mark-active)
       (kill-region (point) (mark))
     (kill-word -1)))

(defun my:join-line (&optional ARG)
  "Backward from `delete-indentation'.
Joins this line to following line.
With argument, join this line to previous line"
  (interactive "P")
  (delete-indentation (unless ARG t)))

(defun my:move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun my:move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun my:minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my:kill-buffer ()
  "Kills current active buffer without prompt"
  (interactive)
  (kill-buffer (current-buffer)))

(defun my:kill-buffer-and-window ()
  "Kills current active buffer without prompt, closes window too"
  (interactive)
  (let ((buf (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (kill-buffer buf)))

(defun my:toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (let ((window (selected-window)))
    (message (if (set-window-dedicated-p
                  window
                  (not (window-dedicated-p window)))
                 "Window %s dedicated"
               "Window %s normal")
             (buffer-name))))

(defun my:resize-window (&optional arg)
  "Resize window interactively"
  (interactive "P")
  (when (one-window-p)
    (error "Cannot resize sole window"))
  (when (window-fixed-size-p (selected-window))
    (error "Window has fixed size"))
  (let (c exit (n (or arg 5)))
    (while (not exit)
      (message "Resize window on hkjl")
      (setq c (read-char))
      (cond ((= c ?h) (shrink-window-horizontally n))
            ((= c ?j) (enlarge-window n))
            ((= c ?k) (shrink-window n))
            ((= c ?l) (enlarge-window-horizontally n))
            (t (setq exit t)))))
  (message "Done."))

(defun my:detach-window (&optional window)
  "Close current window and open it's buffer
in new frame"
  (interactive)
  (setq window (window-normalize-window window))
  (if (my:one-window-p window)
      (error "Can't detach single window"))
  (switch-to-buffer-other-frame (window-buffer window))
  (delete-window window))


;;;;;;;;;;;;;;;;;;;;;
;; Global bindings ;;
;;;;;;;;;;;;;;;;;;;;;

;; Redefine esc
;; however esc will not break from hangs like C-g
(global-unset-key (kbd "ESC ESC ESC"))
(global-set-key (kbd "C-S-g") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-g") 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-quit)
(my:minibuffer-set-key (kbd "<escape>") 'my:minibuffer-keyboard-quit)

(my:kmap
 ("M-/" 'hippie-expand)

 ;; Jumping
 ("C-x m" 'my:push-mark-no-activate)
 ("C-x p" 'pop-to-mark-command)
 ("C-x C-x" 'my:exchange-point-and-mark)
 ("C-c o" 'ff-find-other-file)

 ;; Buffers
 ("C-x B"   'ibuffer)
 ("C-x C-c" 'switch-to-buffer)
 ("C-x k"   'my:kill-buffer)
 ("C-x C-k" 'my:kill-buffer-and-window)
 ("C-x M-k" 'kill-buffer)

 ;; Editing
 ("C-w" 'my:kill-region-or-word)
 ("C-S-w" 'kill-region)
 ("C-x C-;" 'comment-or-uncomment-region)
 ("C-<backspace>" 'my:kill-line-to-indent)
 ("C-<delete>"    'kill-line)
 ("M-<delete>"    'kill-word)
 ("M-k"           'kill-whole-line)
 ("M-j"           'my:join-line)

 ;; Window management
 ("C-c w <left>"  'windmove-left)
 ("C-c w <down>"  'windmove-down)
 ("C-c w <up>"    'windmove-up)
 ("C-c w <right>" 'windmove-right)

 ("C-c w h" 'windmove-left)
 ("C-c w j" 'windmove-down)
 ("C-c w k" 'windmove-up)
 ("C-c w l" 'windmove-right)

 ("C-c w r" 'my:resize-window)
 ("C-c w n" 'my:detach-window)

 ("<f9>" 'my:toggle-window-dedicated)

 ("<f8>" 'compile))


;;;;;;;;;;;;;;;;;;;
;; Mode Settings ;;
;;;;;;;;;;;;;;;;;;;

;; hippie settings from Prelude
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Spell Check
(when (executable-find "hunspell")
  (my:eval-after ispell
    (add-to-list 'ispell-local-dictionary-alist
                 '("russian-hunspell"
                   "[Ё-ё]"  ;; Word characters
                   "[^Ё-ё]" ;; Non-word characters
                   "[-]"    ;; Non-word characters in words
                   nil      ;; Many non-word chars?
                   ("-d" "ru_RU") ;; Args to use this dictionary
                   nil      ;; Ispell-related extenden char mode
                   utf-8))  ;; Charset checker uses
    (add-to-list 'ispell-local-dictionary-alist
                 '("english-hunspell"
                   "[A-z]"
                   "[^A-z]"
                   "[']"
                   nil
                   ("-d" "en_US")
                   nil
                   iso-8859-1))
    (setq ispell-program-name "hunspell")))

(setq flyspell-issue-message-flag nil)

;; Sync unchanged buffers with filesystem
(global-auto-revert-mode t)

;; Show recent files
(recentf-mode t)

;; Ido mode for some operations
(ido-mode t)
(setq-default ido-create-new-buffer 'always
              ido-default-buffer-method 'selected-window
              ido-enable-last-directory-history nil
              ido-enable-flex-matching t
              ido-everywhere t)
(add-to-list 'ido-ignore-buffers "^\\*helm")

;; Window management
(setq-default windmove-wrap-around t)
(winner-mode t)

;; Comint
(setq-default comint-prompt-read-only t
              comint-process-echoes t
              comint-scroll-to-bottom-on-input t)

;; C/C++
(defconst my:c-style
  '("linux"
    (c-basic-offset . 4)
    (c-offsets-alist
     (innamespace . 0))))

(c-add-style "reiver" my:c-style)

;; CEDET
(my:eval-after semantic

  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-show-unmatched-syntax-mode)

  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  (defun my:system-include-path ()
    "Just returns `semantic-dependency-system-include-path'
to feed to other packages"
    semantic-dependency-system-include-path)

  (defun my:cedet-setup ()
    "Local settings for `semantic-mode'"
    (local-set-key (kbd "C-c i") 'semantic-decoration-include-visit)
    (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
    (local-set-key (kbd "C-c q") 'semantic-ia-show-doc)
    (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
    (local-set-key (kbd "C-c t") 'semantic-analyze-proto-impl-toggle))

  (add-hook 'c-mode-hook 'my:cedet-setup)
  (add-hook 'c++-mode-hook 'my:cedet-setup))


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(defun my:package-initialize ()
  ;; Navigation, editing, appearance
  (use-package relative-line-numbers
    :ensure t
    :config (progn
              (defun my:relative-ln-format (number)
                "Relative line numbers format function"
                (if (= number 0)
                    "  =>"
                  (format " %3d" (abs number))))
              (setq relative-line-numbers-format 'my:relative-ln-format)
              (my:eval-after relative-line-numbers
                (set-face-attribute 'relative-line-numbers nil
                                    :bold nil
                                    :italic nil))
              (add-hook 'prog-mode-hook 'relative-line-numbers-mode)))
  (use-package undo-tree
    :ensure t
    :config (progn
              (global-undo-tree-mode)
              (setq undo-tree-visualizer-timestamps t
                    undo-tree-visualizer-diff t)))
  (use-package multiple-cursors
    :ensure t
    :config (progn
              (my:kmap ("C->" 'mc/mark-next-like-this)
                       ("C-<" 'mc/mark-previous-like-this)
                       ("C-c C-<" 'mc/mark-all-like-this))))
  (use-package expand-region
    :ensure t
    :config (progn
              (my:kmap "C-=" 'er/expand-region)))
  (use-package smartparens
    :ensure t
    :config (progn
              (setq sp-autoskip-opening-pair t
                    sp-autoskip-closing-pair 'always
                    ;; disable overlay
                    sp-highlight-pair-overlay nil
                    sp-highlight-wrap-overlay nil
                    sp-highlight-wrap-tag-overlay nil
                    ;; show for evil-mode
                    sp-show-pair-from-inside t
                    ;; only html-mode by default
                    sp-navigate-consider-sgml-tags '(html-mode nxml-mode)
                    sp-hybrid-kill-entire-symbol nil
                    sp-base-key-bindings 'paredit)
              (sp-use-paredit-bindings)
              ;; Disable quote matching in lisp
              (sp-with-modes sp--lisp-modes
                (sp-local-pair "'" nil :actions nil)
                (sp-local-pair "`" "'" :when '(sp-in-string-p)))
              (smartparens-global-mode t)
              (show-smartparens-global-mode t)))
  (use-package ace-jump-mode
    :ensure t)
  (use-package ace-window
    :ensure t
    :config (progn
              (setq aw-keys '(?q ?w ?e ?r ?f ?1 ?2 ?3 ?4))
              (add-to-list 'aw-ignored-buffers " *NeoTree*")
              ;; Add killing buffer in window and moving windows operations
              (defun my:aw--window-base (position action)
                "Just like `aw-delete-window' but with custom action. Fix other-frame."
                (let ((current-window (selected-window)))
                  (if (windowp position)
                      (funcall action position current-window)
                    (let ((frame (aj-position-frame position))
                          (window (aj-position-window position)))
                      (if (and (frame-live-p frame)
                               (not (eq frame (selected-frame))))
                          (select-frame-set-input-focus (window-frame window)))
                      (if (window-live-p window)
                          (funcall action window current-window))))))
              (defun my:aw-delete-window (position)
                "Kill delete window of `aj-position' structure POSITION."
                (my:aw--window-base
                 position
                 (lambda (window _)
                   (let ((frame (window-frame window)))
                     (if (one-window-p t frame)
                         (delete-frame frame)
                       (delete-window window))))))
              (defun my:aw-kill-window (position)
                "Kill buffer and delete window of `aj-position' structure POSITION."
                (my:aw--window-base
                 position
                 (lambda (window _)
                   (when (kill-buffer (window-buffer window))
                     (let ((frame (window-frame window)))
                       (if (one-window-p t frame)
                           (delete-frame frame)
                         (delete-window window)))))))
              (defun my:aw-move-window (position)
                "Move window to split, choose split after window."
                (my:aw--window-base
                 position
                 (lambda (target-window current-window)
                   (let* ((choice (read-char-choice "Choose side with hjkl: "
                                                    '(?h ?j ?k ?l)))
			  (side (cond ((= choice ?h) 'left)
                                      ((= choice ?j) 'below)
                                      ((= choice ?k) 'above)
                                      ((= choice ?l) 'right))))
                     (my:move-window-to-other-window target-window
                                                     current-window side)))))
              (defun my:aw-swap-window (position)
                "Swap two window buffers"
                (my:aw--window-base
                 position
                 (lambda (next-window prev-window)
                   (let ((prev-buf (window-buffer prev-window))
                         (next-buf (window-buffer next-window)))
                     (set-window-buffer prev-window next-buf)
                     (set-window-buffer next-window prev-buf)
                     (select-window next-window)))))
              (defalias 'ace-swap-window
                (aw-generic " Ace - Swap" my:aw-swap-window))
              (defalias 'ace-delete-window
                (aw-generic " Ace - Del" my:aw-delete-window))
              (defalias 'ace-kill-window
                (aw-generic " Ace - Kill" my:aw-kill-window))
              (defalias 'ace-move-window
                (aw-generic " Ace - Move" my:aw-move-window))
              (my:kmap ("C-c w w" "C-c C-w" 'ace-window)
                       ("C-c w s" 'ace-swap-window)
                       ("C-c w d" 'ace-delete-window)
                       ("C-c w g" 'ace-kill-window)
                       ("C-c w m" 'ace-move-window))))
  ;; Fast access and searching
  (use-package ido-vertical-mode
    :ensure t
    :config (ido-vertical-mode 1))
  (use-package flx-ido
    :ensure t
    :config (progn
              (flx-ido-mode 1)
              (setq ido-use-faces nil)))
  (use-package ag
    :ensure t
    :config (progn (setq ag-highlight-search t)))
  (use-package helm-config
    :ensure helm
    :defer t
    :pre-load (setq
               helm-command-prefix-key (kbd "C-c h")
               helm-split-window-in-side-p t
               helm-candidate-number-limit 500)
    :config (progn
              ;; Prevent winner from restoring helm buffers
              (defun my:helm-display-buffer-winner-add (buffer)
                "Adds buffer name to `winner-boring-buffers' before openning"
                (add-to-list 'winner-boring-buffers buffer)
                (helm-default-display-buffer buffer))
              (setq helm-display-function 'my:helm-display-buffer-winner-add)
              ;; Disable underline and dir highlight
              (my:eval-after helm
                (set-face-attribute 'helm-selection nil
                                    :underline nil)
                (set-face-attribute 'helm-selection-line nil
                                    :underline nil))
              (my:eval-after helm-file
                (set-face-attribute 'helm-ff-directory nil
                                    :background nil))
              ;; Disable helm on some selections
              (my:eval-after helm-mode
                (defun my:helm-completion (engine actions)
                  "For convenient filling the `helm-completing-read-handlers-alist'"
                  (mapc
                   (lambda (action)
                     (add-to-list 'helm-completing-read-handlers-alist `(,action . ,engine)))
                   actions))
                (my:helm-completion nil '(switch-to-buffer
                                          kill-buffer
                                          multi-occur
                                          load-library))
                (my:helm-completion 'ido '(flycheck-set-checker-executable)))
              ;; Bindings, C-c ; to work in terminal
              (my:kmap ("C-; t" "C-c ; t" 'helm-etags-select)
                       ("C-; i" "C-c ; i" 'helm-semantic-or-imenu)
                       ("C-; m" "C-c ; m" 'helm-all-mark-rings)
                       ("C-; e" "C-c ; e" 'helm-list-emacs-process)
                       ("C-; r" "C-c ; r" 'helm-resume)
                       ("C-x b"   'helm-mini)
                       ("C-x C-b" 'helm-buffers-list)
                       ("C-x C-f" 'helm-find-files)
                       ("C-h f" 'helm-apropos)
                       ("M-x" 'helm-M-x)
                       ("M-y" 'helm-show-kill-ring))
              (my:kmap* helm-map
                        ("C-i" 'helm-execute-persistent-action)
                        ("<tab>" 'helm-execute-persistent-action)
                        ("C-z" 'helm-select-action))
              (helm-mode t)))
  (use-package helm-swoop
    :ensure t
    :pre-load (progn
                ;; Suppress compiler warning
                (defvar helm-swoop-last-prefix-number nil)))
  (use-package ggtags
    :ensure t
    :config (progn
              (defun my:ggtags-on ()
                "Set `ggtags-mode' on (for c/c++ switch)"
                (ggtags-mode t))
              (add-hook 'c-mode-hook 'my:ggtags-on)
              (add-hook 'c++-mode-hook 'my:ggtags-on)))
  ;; Completion
  (use-package company
    :ensure t
    :config (progn
              (define-key company-mode-map (kbd "C-<tab>") 'company-complete)
              (setq company-tooltip-limit 20)
              ;; Put semantic backend on separate key
              (setq-default company-backends
                            (remove 'company-semantic company-backends))
              (defun my:company-semantic-setup ()
                "Sets `company-semantic' keybind locally"
                (local-set-key (kbd "C-<return>") 'company-semantic))
              (add-hook 'c-mode-hook 'my:company-semantic-setup)
              (add-hook 'c++-mode-hook 'my:company-semantic-setup)
              (global-company-mode t)))
  (use-package yasnippet
    :ensure t
    :config (progn
              ;; No more toolkit popups
              (setq yas-prompt-functions
                    '(yas-ido-prompt yas-completing-prompt yas-no-prompt))
              ;; Just custom snippet dir
              (add-to-list 'yas-snippet-dirs my:snippets-dir)
              (advice-add 'yas-expand :before
                          #'(lambda ()
                              "Escape from `smartparens-mode' overlay"
                              (let ((times 5))
                                (while (and (> times 0) (sp--get-active-overlay))
                                  (sp-remove-active-pair-overlay)
                                  (setq times (- times 1))))))
              (yas-global-mode t)))
  (use-package function-args
    :ensure t
    :config (progn
              (my:eval-after function-args
                (dolist (face '(fa-face-hint
                                fa-face-hint-bold
                                fa-face-semi
                                fa-face-type
                                fa-face-type-bold))
                  (face-spec-reset-face face))
                (set-face-attribute 'fa-face-hint nil
                                    :inherit 'highlight)
                (set-face-attribute 'fa-face-hint-bold nil
                                    :bold t
                                    :inherit 'fa-face-hint)
                (set-face-attribute 'fa-face-semi nil
                                    :inherit '(highlight font-lock-keyword-face))
                (set-face-attribute 'fa-face-type nil
                                    :inherit '(highlight font-lock-type-face))
                (set-face-attribute 'fa-face-type-bold nil
                                    :bold t
                                    :inherit 'fa-face-type))
              (my:eval-after semantic/bovine
                (fa-config-default))))
  ;; External tools
  (use-package magit
    :defer t
    :ensure t)
  ;; Project management and project tree
  (use-package neotree
    :ensure t
    :init (progn
            (defun neo-global--create-window ()
              "Create global neotree window. Split root window."
              (let ((window nil)
                    (buffer (neo-global--get-buffer))
                    (root (frame-root-window (selected-frame))))
                (setq window
                      (split-window root (- neo-window-width) 'left))
                (select-window window)
                (neo-window--init window buffer)
                (setq neo-global--window window)
                window))
            ;; Allow delete window
            (setq neo-persist-show nil)
            (my:kmap ("<f5>" 'neotree-toggle)
                     ("<f6>" 'neotree-find))
            ;; Add jk movement
            (my:kmap* neotree-mode-map
                      ("r" 'neotree-refresh)
                      ("h" 'neotree-hidden-file-toggle)
                      ("a" 'neotree-stretch-toggle)
                      ("p" 'neotree-previous-node)
                      ("n" 'neotree-next-node)
                      ("k" 'neotree-previous-node)
                      ("j" 'neotree-next-node))))
  (use-package projectile
    :ensure t
    :config (progn
              (defun my:neotree-project-root ()
                "Jump neotree to current project root (if exists)"
                (interactive)
                (let ((root (projectile-project-root)))
                  (neotree-dir root)))
              (my:kmap "<f7>" 'my:neotree-project-root)
              ;; Try to emulate ede (from CEDET) project
              (setq semanticdb-project-root-functions
                    projectile-project-root-files-functions)
              (projectile-global-mode)))
  (use-package helm-projectile
    :ensure t
    :config (progn
              (helm-projectile-on)
              (my:kmap "C-; p" "C-c ; p" 'helm-projectile)))
  ;; Evil mode and Co
  (use-package evil
    :ensure t
    :pre-load (progn
                (setq evil-want-visual-char-semi-exclusive t))
    :config (progn
              ;; Start all insert-default modes in emacs state
              (setq evil-emacs-state-modes (append evil-emacs-state-modes
                                                   evil-insert-state-modes)
                    evil-insert-state-modes nil)
              (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)
              ;; NeoTree tweaks
              (evil-set-initial-state 'neotree-mode 'motion)
              (add-hook 'neotree-mode-hook
                        (lambda ()
                          (define-key evil-motion-state-local-map (kbd "TAB") 'neotree-enter)
                          (define-key evil-motion-state-local-map (kbd "SPC") 'neotree-enter)
                          (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)
                          (define-key evil-motion-state-local-map (kbd "q") 'neotree-hide)))
              (evil-mode t)))
  (use-package evil-leader
    :disabled t
    :ensure t
    :config (progn
              (global-evil-leader-mode)))
  ;; Language specific
  (use-package anaconda-mode
    :ensure t
    :init (add-hook 'python-mode-hook 'anaconda-mode))
  (use-package company-anaconda
    :ensure t
    :init (add-to-list 'company-backends 'company-anaconda))
  (use-package company-c-headers
    :ensure t
    :config (progn
              ;; Get include path from semantic
              (my:eval-after semantic/bovine
                (setq company-c-headers-path-system 'my:system-include-path))
              (add-to-list 'company-backends 'company-c-headers)))
  (use-package flycheck
    :ensure t)
  (use-package js2-mode
    :ensure t
    :init (progn (add-to-list 'auto-mode-alist '("\\.js" . js2-mode))))
  (use-package clojure-mode
    :ensure t)
  (use-package cider
    :ensure t))


(package-initialize)
(when (require 'use-package nil t)
  (my:package-initialize))
