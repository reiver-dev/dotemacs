;;; init-impl.el --- Main config file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;;;;;;;;
;; MAIN ;;
;;;;;;;;;;

;; my folders ;;
(defconst my:user-dir (file-name-directory
                       (or load-file-name buffer-file-name)))
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
(add-to-list 'package-archives
             '("marmelade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; backup, autosave, lockfiles ;;
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      create-lockfiles nil)


;;;;;;;;;;;;;;;;
;; Appearance ;;
;;;;;;;;;;;;;;;;

;; Toolbars
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'toolltip-mode)
  (tooltip-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Enable y/n answers
(fset #'yes-or-no-p #'y-or-n-p)
(setq use-dialog-box nil)

;; Modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Fringe
(setq-default indicate-empty-lines t)
(fringe-mode '(8 . 0))

;; Whitespace
(setq-default whitespace-line-column 79)

;; Misc settings
(defun my:bell-function ()
  (unless (memq this-command
                '(isearch-abort
                  abort-recursive-edit
                  exit-minibuffer
                  keyboard-quit
                  helm-keyboard-quit))
    (ding)))


(setq-default inhibit-startup-screen t
              initial-scratch-message nil
              visible-bell t ;; do not beep
              ring-bell-function #'my:bell-function ;; and blink less
              disabled-command-function nil) ;; enable all commands

;; Scroll
(setq-default
 ;; three lines at a time
 mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))
 ;; don't accelerate scrolling
 mouse-wheel-progressive-speed nil
 ;; don't recenter
 scroll-conservatively 100000
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; horizontal scroll jumps
 hscroll-step 1
 hscroll-margin 5)

;; Text behavior
(setq-default shift-select-mode nil
              sentence-end-double-space nil
              require-final-newline t)

(setq-default
 truncate-lines t ;; do not wrap lines
 bidi-display-reordering nil) ;; disable bidirectional text

;; Indentation
(setq-default indent-tabs-mode nil)

(defun my:prog-mode-setup ()
  "Basic settings for prog and other modes"
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook #'my:prog-mode-setup)
(add-hook 'nxml-mode #'my:prog-mode-setup)

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(deftheme my:theme "My face theme settings")

(custom-theme-set-faces
 'my:theme
 ;; Line numbers appearance
 '(linum ((t (:bold t :italic nil))))
 '(vertical-border
   ((((type graphic))
     (:foreground nil :backround nil :inherit fringe :inverse-video t))))

 ;; Modeline highligh box is ugly
 '(mode-line-highlight ((t (:box nil :inverse-video t))))

 ;; Disable underline and dir highlight
 ;; '(helm-selection      ((t (:underline nil))))
 ;; '(helm-selection-line ((t (:underline nil))))
 ;; '(helm-ff-directory   ((t (:background nil))))

 ;; Make function-args respect current theme
 '(fa-face-hint      ((t (:inherit highlight))))
 '(fa-face-hint-bold ((t (:bold t :inherit fa-face-hint))))
 '(fa-face-semi      ((t (:inherit (highlight font-lock-keyword-face)))))
 '(fa-face-type      ((t (:inherit (highlight font-lock-type-face)))))
 '(fa-face-type-bold ((t (:bold t :inherit fa-face-type)))))

(enable-theme 'my:theme)

;;;;;;;;;;
;; Util ;;
;;;;;;;;;;

(defun my:mapcan (function sequence)
  "Replacement for `mapcan' to not require `cl.el'"
  (apply #'nconc (mapcar function sequence)))

(defun my:remove-if (func sequence)
  "Reimplacement for `remove-if' to not use `cl.el'"
  (delq nil (mapcar
             (lambda (x) (if (funcall func x) nil x))
             sequence)))

(defun my:remove-if-not (func sequence)
  "Reimplacement for `remove-if-not' to not use `cl.el'"
  (delq nil (mapcar
             (lambda (x) (if (funcall func x) x nil))
             sequence)))

(defmacro my:add-to (l el)
  `(setq ,l (cons ,el ,l)))

(defmacro my:append-to (to from)
  `(setq ,to (append ,from ,to)))

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
  (let ((result
         (my:mapcan
          (lambda (bind)
            (let ((keys (butlast bind))
                  (func (last bind)))
              (mapcar (lambda (key)
                        (when (stringp key)
                          (setq key `(kbd ,key)))
                        `(define-key ,keymap ,key ,@func))
                      keys)))
          bindings)))
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

;; For project settings
(defun my:current-fs-point ()
  "Returns current `buffer-file-name' or `default-directory'"
  (if (stringp buffer-file-name) buffer-file-name default-directory))

(defun my:parent-dir (dir)
  "Just gets relative top dir for given DIR path."
  (file-name-directory (directory-file-name target)))

(defun my:locate-top-dominating-file (from name &optional default)
  "Find top file with given NAME starting from directory FROM.
Optional DEFAULT value returned if nothing found or nil.
See `locate-dominating-file' for reference"
  (let ((target (locate-dominating-file from name)))
    (if target
        (let ((parent (my:parent-dir target)))
          (if (equal target parent) target
            (my:locate-top-dominating-file parent name target)))
      default)))

(defun my:files-in-below-directory (directory pattern &optional ignore)
  "List the file names in DIRECTORY and in its sub-directories
equal to PATTERN. Optional IGNORE argument can be list of names to ignore
in recursive walk or function receiving directory name as single argument"
  (let (el-files-list
        (current-directory-list
         (with-demoted-errors
             (directory-files directory t nil nil)))
        (ignore-func (cond
                      ((functionp ignore) ignore)
                      ((listp ignore) (lambda (dir) (member dir ignore)))
                      (t (lambda (dir) nil)))))
    (while current-directory-list
      (let* ((file (car current-directory-list))
             (filename (file-relative-name file directory)))
        (when (string= pattern filename)
          (my:add-to el-files-list file))
        (when (and (file-directory-p (car current-directory-list))
                   (not (equal "." (substring file -1)))
                   (not (funcall ignore-func file)))
          (my:append-to
           el-files-list
           (my:files-in-below-directory file pattern))))
      (setq current-directory-list (cdr current-directory-list)))
    el-files-list))

(defun my:find-dir-locals ()
  "Find directory local (.dir-locals.el) settings location;
raises error if not found"
  (let ((dl-path
         (locate-dominating-file (my:current-fs-point) ".dir-locals.el"))o)
    (if (stringp dl-path) dl-path
      (error ".dir-locals.el not found"))))

(defun my:dir-locals-path (&optional relative)
  "Finds directory local (.dir-locals.el) settings location
with RELATIVE argument returns path relative to dir-locals location,
FORCE-DIR sets return value to be directory path"
  (if (stringp relative)
      (expand-file-name relative (my:find-dir-locals))
    (expand-file-name (my:find-dir-locals))))

(defmacro my:with-local-dir (relative &rest body)
  "Macro for running commands from location relative to \".dir-locals.el\""
  `(let ((default-directory
           (file-name-as-directory (my:dir-locals-path ,relative))))
     ,@body))

(defun my:add-to-path (&rest paths)
  "Add PATHS values to `exec-path' and environment variable $PATH"
  (let ((env-path (getenv "PATH"))
        (value (mapconcat #'identity paths path-separator)))
    (setenv "PATH" (concat env-path path-separator value)))
  (setq exec-path (append exec-path paths)))

(defun my:add-to-path-front (&rest paths)
  "Add PATHS values to `exec-path' and environment variable $PATH"
  (let ((env-path (getenv "PATH"))
        (value (mapconcat #'identity paths path-separator)))
    (setenv "PATH" (concat value path-separator env-path)))
  (setq exec-path (nconc paths exec-path)))


;; For autoload byte-compiling
;; http://www.lunaryorn.com/2013/06/25/introducing-with-eval-after-load.html
(defmacro my:with-eval-after-load (feature &rest forms)
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

(defun my:global-unset-command (command)
  "Unsets all key binding for COMMAND symbol. See `global-unset-key'."
  (let ((bindings (where-is-internal command)))
    (mapc (lambda (bnd) (global-unset-key bnd)) bindings)))

;; For window management
(defun my:one-window-p (&optional window)
  "Like `one-window-p', but correctly works with other frame selected"
  (let ((frame (window-frame window)))
    (eq window
        (next-window window 'no-minibuf frame))))

(defun my:delete-window (window)
  "Like `delete-window' but closes frame if WINDOW is only window left"
  (let ((frame (window-frame window)))
    (if (my:one-window-p frame)
        (delete-frame frame)
      (delete-window window))))

(defun my:visible-frame-list (&optional from-current-frame)
  "Returns list of \"visible\" frames starting from current frame
if FROM-CURRENT-FRAME is not nil.
Here \"visible\" frame is current frame or any graphical frame"
  (let* ((current-frame (selected-frame))
         (frames (my:remove-if-not
                  (lambda (frame)
                    (or (eq frame current-frame)
                        (window-system frame)))
                  (visible-frame-list))))
    (if from-current-frame
        (cons current-frame (remove current-frame frames))
      frames)))

(defun my:visible-window-list ()
  "Return windows from all visible frames"
  (my:mapcan #'window-list (my:visible-frame-list)))

(defun my:apply-to-window (action &optional window &rest args)
  "Calls ACTION with argument WINDOW"
  (when (window-live-p window)
    (let ((frame (window-frame window)))
      (when (and (frame-live-p frame)
                 (frame-visible-p frame)
                 (not (eq frame (selected-frame))))
        (select-frame-set-input-focus frame)))
    (if args (apply action window args)
      (funcall action window))))

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

(defun my:query-move-to-window (target-window current-window)
  "Function to interactevely choose side for `my:move-window-to-other-window'"
  (let* ((choice (read-char-choice "Choose side with hjkl: "
                                   '(?h ?j ?k ?l)))
         (side (cond ((= choice ?h) 'left)
                     ((= choice ?j) 'below)
                     ((= choice ?k) 'above)
                     ((= choice ?l) 'right))))
    (my:move-window-to-other-window target-window
                                    current-window side)))

(defun my:swap-windows (target-window current-window)
  "Swaps two windows' buffers"
  (let ((current-buf (window-buffer current-window))
        (target-buf (window-buffer target-window)))
    (set-window-buffer current-window target-buf)
    (set-window-buffer target-window current-buf)
    (select-window target-window)))

;; Misc
(defun my:region-active ()
  "Checks whether there is valid active region selection"
  (and transient-mark-mode mark-active
       (not (eq (region-beginning) (region-end)))))


;;;;;;;;;;;;;;;;;
;; Interactive ;;
;;;;;;;;;;;;;;;;;

(defun my:push-mark-no-activate ()
  "Calls `push-mark' like `push-mark-command' but withoug activation"
  (interactive)
  (push-mark))

;; Do not activate mark during jump
(defun my:exchange-point-and-mark (&optional ARG)
  "Inverse `exchange-point-and-mark' prefix argument
 when mark is not active (`mark-active')"
  (interactive "P")
  (exchange-point-and-mark
   (if mark-active ARG (not ARG))))

(defun my:kill-line-to-indent ()
  "Kills line backward (opposite to `kill-line')
and indents after that"
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

;; Region dependent choices
(defun my:kill-region-or-word ()
  "Call `kill-region' or `backward-kill-word'
depending on whether or not a region is selected."
  (interactive)
  (if (my:region-active)
      (call-interactively #'kill-region)
    (call-interactively #'backward-kill-word)))

(defun my:upcase-region-or-word ()
  "Call `upcase-region' or `upcase-word'
depending on whether or not a region is selected."
  (interactive)
  (if (my:region-active)
      (call-interactively #'upcase-region)
    (call-interactively #'upcase-word)))

(defun my:downcase-region-or-word ()
  "Call `downcase-region' or `downcase-word'
depending on whether or not a region is selected."
  (interactive)
  (if (my:region-active)
      (call-interactively #'downcase-region)
    (call-interactively #'downcase-word)))

(defun my:capitalize-region-or-word ()
  "Call `capitalize-region' or `capitalize-word'
depending on whether or not a region is selected."
  (interactive)
  (if (my:region-active)
      (call-interactively #'capitalize-region)
    (call-interactively #'capitalize-word)))

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
  "Abort recursive edit. In Delete Selection mode,
 if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (deactivate-mark)
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
  "Close WINDOW and open it's buffer in new frame.
 If WINDOW is nil, applies to selected window"
  (interactive)
  (let ((window (window-normalize-window window)))
    (if (my:one-window-p window)
        (error "Can't detach single window"))
    (switch-to-buffer-other-frame (window-buffer window))
    (delete-window window)))

(defun my:hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively #'hippie-expand)))

(defun my:hippie-expand-files ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name)))
    (call-interactively #'hippie-expand)))

;;;;;;;;;;;;;;;;;;;;;
;; Global bindings ;;
;;;;;;;;;;;;;;;;;;;;;

;; Redefine esc
;; however esc will not break from hangs like C-g
(global-unset-key (kbd "ESC ESC ESC"))
(global-set-key (kbd "C-S-g")           #'keyboard-escape-quit)
(global-set-key (kbd "C-x C-g")         #'keyboard-escape-quit)
(global-set-key (kbd "<escape>")        #'keyboard-quit)
(my:minibuffer-set-key (kbd "<escape>") #'my:minibuffer-keyboard-quit)

;; Never quit so fast
(global-unset-key (kbd "C-x C-c"))
;; We have suspend at `C-x C-z'
(global-unset-key (kbd "C-z"))
;; Use that for instant bindings
(global-unset-key (kbd "<menu>"))


(mapc #'my:global-unset-command
      '(upcase-region downcase-region capitalize-region))

(my:kmap
 ([remap dabbrev-expand] #'hippie-expand) ; "M-/"
 ([remap dabbrev-completion] #'my:hippie-expand-files) ; "C-M-/"

 ;; Jumping
 ([remap exchange-point-and-mark] #'my:exchange-point-and-mark) ; "C-x C-x"
 ("C-x m"   #'my:push-mark-no-activate)
 ("C-c o"   #'ff-find-other-file)

 ;; Vim's word jumping
 ("M-a" (lambda (ARG)
          (interactive "^p")
          (forward-same-syntax (- ARG))))
 ("M-e" #'forward-same-syntax)

 ;; Swap tag functions
 ("M-*" "C-M-," #'tags-loop-continue)
 ("M-," #'pop-tag-mark)

 ;; Buffers
 ([remap list-buffers] #'ibuffer) ; "C-x C-b"
 ("C-x k"   #'my:kill-buffer)
 ("C-x C-k" #'my:kill-buffer-and-window)

 ;; Editing
 ("C-w"           #'my:kill-region-or-word)
 ("C-S-w"         #'kill-region)
 ("C-x C-;"       #'comment-or-uncomment-region)
 ("C-<backspace>" #'my:kill-line-to-indent)
 ("C-<delete>"    #'kill-line)
 ("M-<delete>"    #'kill-word)
 ("M-k"           #'kill-whole-line)
 ("M-j"           #'my:join-line)

 ([remap capitalize-word] #'my:capitalize-region-or-word)
 ([remap upcase-word] #'my:upcase-region-or-word)
 ([remap downcase-word] #'my:downcase-region-or-word)

 ;; Window management
 ("C-c w <left>"  #'windmove-left)
 ("C-c w <down>"  #'windmove-down)
 ("C-c w <up>"    #'windmove-up)
 ("C-c w <right>" #'windmove-right)

 ("C-c w h" #'windmove-left)
 ("C-c w j" #'windmove-down)
 ("C-c w k" #'windmove-up)
 ("C-c w l" #'windmove-right)

 ("C-c w r" #'my:resize-window)
 ("C-c w n" #'my:detach-window)

 ("<f9>" #'my:toggle-window-dedicated)
 ("<f8>" #'compile))


;;;;;;;;;;;;;;;;;;;
;; Mode Settings ;;
;;;;;;;;;;;;;;;;;;;

;; hippie settings from Prelude
(setq-default hippie-expand-try-functions-list
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

;; Ediff
(my:with-eval-after-load ediff
  (setq-default
   ;; run control panel in same frame
   ediff-window-setup-function #'ediff-setup-windows-plain))

;; Spell Check
(when (executable-find "hunspell")
  (my:with-eval-after-load ispell
    (setq-default flyspell-issue-message-flag nil)
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
    (setq-default ispell-program-name "hunspell")))


;; Sync unchanged buffers with filesystem
(global-auto-revert-mode t)

;; Show recent files
(recentf-mode t)

;; Window management
(setq-default windmove-wrap-around t)
(winner-mode t)

;; Comint
(setq-default comint-prompt-read-only t
              comint-process-echoes t
              comint-scroll-to-bottom-on-input t)

(my:with-eval-after-load comint
  ;; We have `my:kill-region-or-word' already
  (my:kmap* comint-mode-map ("C-c C-w" nil)))

;; org-mode
(my:with-eval-after-load org
  (setq-default org-src-fontify-natively t))

;; C/C++
(with-eval-after-load 'cc-mode)
(defconst my:c-style
  '("linux"
    (c-basic-offset . 4)
    (c-offsets-alist
     (innamespace . 0)
     (inline-open . 0))))

(c-add-style "reiver" my:c-style)
(setq-default c-default-style
              '((c-mode . "reiver")
                (c++-mode . "reiver")
                (java-mode . "java")
                (awk-mode . "awk")
                (other . "gnu")))

;;  CEDET
(my:with-eval-after-load semantic

  (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-show-parser-state-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-show-unmatched-syntax-mode)

  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  (defun my:cedet-setup ()
    "Local settings for `semantic-mode'"
    (local-set-key (kbd "C-c i") #'semantic-decoration-include-visit)
    (local-set-key (kbd "C-c j") #'semantic-ia-fast-jump)
    (local-set-key (kbd "C-c q") #'semantic-ia-show-doc)
    (local-set-key (kbd "C-c s") #'semantic-ia-show-summary)
    (local-set-key (kbd "C-c t") #'semantic-analyze-proto-impl-toggle))

  (add-hook 'c-mode-hook #'my:cedet-setup)
  (add-hook 'c++-mode-hook #'my:cedet-setup))

(my:with-eval-after-load semantic/dep
  (defun my:system-include-path ()
    "Just returns `semantic-dependency-system-include-path'
to feed to other packages"
    semantic-dependency-system-include-path))


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(defvar my:packages nil)
(defmacro my:with-package (name &rest args)
  (declare (indent 1))
  (unless (plist-get args :disabled)
    (let* ((ensure (plist-get args :ensure))
           (package (if (or (not ensure) (eq t ensure)) name ensure))
           (init (plist-get args :init))
           (defer (let ((d (plist-get args :defer)))
                    (when d
                      (if (numberp d) d 10))))
           (config (plist-get args :config))
           (condition (plist-get args :if))
           (result '()))

      (when init
        (my:add-to
         result
         (if defer
             `(run-with-idle-timer ,defer nil (lambda () ,init))
           init)))

      (when config
        (my:add-to
         result
         `(my:with-eval-after-load ,name ,@(macroexp-unprogn config))))

      (when ensure
        (my:add-to result `(when (not (package-installed-p ',package))
                             (package-install ',package)))
        (my:add-to result `(add-to-list 'my:packages ',package)))

      (let ((r `(with-demoted-errors
                    ,(concat "Error loading " (symbol-name package) ": %s")
                  ,@result)))
        (if condition
            `(if ,condition ,r nil)
          r)))))

(put 'my:with-package 'lisp-indent-function 'defun)
(package-initialize)

(my:with-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :config (progn
            (setq-default undo-tree-visualizer-timestamps t
                          undo-tree-visualizer-diff t)))

(my:with-package multiple-cursors
  :ensure t
  :init (my:kmap ("C->" #'mc/mark-next-like-this)
                 ("C-<" #'mc/mark-previous-like-this)
                 ("C-c C-<" #'mc/mark-all-like-this)))

(my:with-package expand-region
  :ensure t
  :init (progn
          (autoload 'er/mark-symbol "expand-region")
          (my:kmap ("C-=" #'er/expand-region)
                   ("C-+" #'er/mark-symbol))))

(my:with-package visual-regexp
  :ensure t
  :init (my:kmap
         ([remap query-replace-regexp] #'vr/query-replace)
         ("M-s m" #'vr/mc-mark))
  :config (progn
            (setq-default vr/auto-show-help nil)
            (my:kmap* vr/minibuffer-replace-keymap
                      ("C-c p" nil) ;; Will be shadowed by projectile
                      ("C-c v" #'vr--shortcut-toggle-preview))))

(my:with-package iy-go-to-char
  :ensure t
  :init (my:kmap ("C-." #'iy-go-up-to-char)
                 ("C-," #'iy-go-up-to-char-backward))
  :config (setq-default
           ;; kill-region do not work with `multiple-cursors-mode'
           iy-go-to-char-override-local-map nil))

(my:with-package smartparens
  :ensure t
  :init (require 'smartparens-config)
  :config (progn
            (setq-default
             ;; disable overlay
             sp-highlight-pair-overlay nil
             sp-highlight-wrap-overlay nil
             sp-highlight-wrap-tag-overlay nil)
            (smartparens-global-mode t)
            (show-smartparens-global-mode t)
            (my:kmap* smartparens-mode-map
                      ;; Basic movements
                      ("C-M-f" #'sp-forward-sexp)
                      ("C-M-b" #'sp-backward-sexp)
                      ("C-M-d" #'sp-down-sexp)
                      ("C-M-u" #'sp-backward-up-sexp)
                      ("C-M-p" #'sp-backward-down-sexp) ; remap backward-list
                      ("C-M-n" #'sp-up-sexp) ; remap forward-list
                      ("C-M-k" #'sp-kill-sexp)
                      ;; List manipulation
                      ("C-x p c" #'sp-splice-sexp)
                      ("C-x p s" #'sp-split-sexp)
                      ("C-x p j" #'sp-join-sexp)
                      ("C-x p a" #'sp-splice-sexp-killing-around)
                      ("C-x p b" #'sp-splice-sexp-killing-backward)
                      ("C-x p f" #'sp-splice-sexp-killing-forward)
                      ("C-x p r" #'sp-rewrap-sexp)
                      ("C-x p u" #'sp-unwrap-sexp)
                      ("C-x p d" #'sp-backward-unwrap-sexp)
                      ("C-x p w" #'sp-swap-enclosing-sexp)
                      ("C-x p p" #'sp-select-next-thing-exchange))
            (defvar my:paredit-extended-mode-map (make-sparse-keymap)
              "Keymap for `my:paredit-exteded-mode'")
            (define-minor-mode my:paredit-extended-mode
              "Sets from `smartparens-mode': \\{my:paredit-extended-mode-map}"
              :keymap my:paredit-extended-mode-map)
            (my:kmap* my:paredit-extended-mode-map
                      ("C-M-t" #'sp-transpose-sexp) ; remap transpose-sex
                      ;; Direction manipulation
                      ("M-<up>"      #'sp-splice-sexp-killing-backward)
                      ("M-<down>"    #'sp-splice-sexp-killing-forward)
                      ("C-<right>"   #'sp-forward-slurp-sexp)
                      ("C-<left>"    #'sp-forward-barf-sexp)
                      ("C-M-<left>"  #'sp-backward-slurp-sexp)
                      ("C-M-<right>" #'sp-backward-barf-sexp))
            (defun my:lisp-setup-paredit ()
              (my:paredit-extended-mode t)
              (smartparens-strict-mode))
            (add-hook 'lisp-mode-hook #'my:lisp-setup-paredit)
            (add-hook 'emacs-lisp-mode-hook #'my:lisp-setup-paredit)))

(my:with-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(my:with-package ace-jump-mode
  :ensure t
  :init (my:kmap "M-o" #'ace-jump-word-mode))

(my:with-package switch-window
  :ensure t
  :init (progn
          (require 'switch-window nil t)
          (defun my:switch-ignored-p (window)
            "Returns if WINDOW should be ignored during `switch-window'"
            (and (member (buffer-name (window-buffer window))
                         (list " *NeoTree*"))
                 (not (eq (window-frame window) (selected-frame)))))
          (defun my:switch-window-list (&optional from-current-window)
            "Like `switch-window-list', but looks through all frames"
            (let ((wlist
                   (if (or from-current-window switch-window-relative)
                       (lambda (frame)
                         (window-list frame nil))
                     (lambda (frame)
                       (window-list frame nil (frame-first-window frame))))))
              (my:remove-if #'my:switch-ignored-p
                            (my:mapcan wlist (my:visible-frame-list t)))))
          (fset #'switch-window-list #'my:switch-window-list)
          (defun my:switch-window-list-enumerate ()
            (loop for _ in (my:switch-window-list)
                  for x in (switch-window-list-keys)
                  collect x))
          (fset #'switch-window-enumerate #'my:switch-window-list-enumerate)
          (defun my:switch-move-focus-with
              (action msg-before msg-after &optional args)
            "Choose window with overlay symbold and ACTION
 (with additional ARGS) to it"
            (let ((wlist (my:switch-window-list)))
              (if (<= (length wlist) switch-window-threshold)
                  (my:apply-to-window
                   action (car (remove (selected-window) wlist)) args)
                (let ((index (prompt-for-selected-window msg-before))
                      (eobps (switch-window-list-eobp)))
                  (apply-to-window-index
                   (lambda (window)
                     (my:apply-to-window action window args)) index msg-after)
                  (switch-window-restore-eobp
                   (my:remove-if-not #'window-valid-p eobps))))))
          (defun my:switch-window ()
            (interactive)
            (my:switch-move-focus-with #'select-window
                                       "Move to window: "
                                       "Moved to: %s"))
          (defun my:switch-move-window ()
            (interactive)
            (my:switch-move-focus-with #'my:query-move-to-window
                                       "Move window to other window: "
                                       "Moved to: %s"
                                       (selected-window)))
          (defun my:switch-swap-window ()
            (interactive)
            (my:switch-move-focus-with #'my:swap-windows
                                       "Swap window with: "
                                       "Swapped with: %s"
                                       (selected-window)))
          (my:kmap ("C-c C-w" "C-c w w" #'my:switch-window)
                   ("C-c w m" #'my:switch-move-window)
                   ("C-c w s" #'my:switch-swap-window))))

(my:with-package helm
  :ensure t
  :init (progn
          (setq-default helm-command-prefix-key (kbd "C-c h")
                        helm-buffers-fuzzy-matching t
                        helm-candidate-number-limit 500)
          (autoload 'helm--completion-in-region "helm-mode")
          (setq-default completion-in-region-function
                        #'helm--completion-in-region)
          (my:kmap ("M-x" #'helm-M-x)
                   ("M-X" #'execute-extended-command)
                   ("C-M-y" #'helm-show-kill-ring)
                   ([remap switch-to-buffer] #'helm-mini) ; C-x b
                   ("C-x C-c" #'helm-buffers-list)
                   ("C-x C-f" #'helm-find-files)
                   ("C-h f"   #'helm-apropos)
                   ("C-; i" "C-c ; i" #'helm-imenu)
                   ("C-; t" "C-c ; t" #'helm-etags-select)
                   ("C-; m" "C-c ; m" #'helm-all-mark-rings)
                   ("C-; e" "C-c ; e" #'helm-list-emacs-process)
                   ("C-; r" "C-c ; r" #'helm-resume))
          (require 'helm-config))
  :config (progn
            ;; Prevent winner from restoring helm buffers
            (defun my:helm-display-buffer (buffer)
              "Adds buffer name to `winner-boring-buffers' before openning"
              (add-to-list 'winner-boring-buffers buffer)
              (let* ((height (min (/ (frame-height) 2) 16))
                     (win (split-window (frame-root-window)
                                        (- height) 'below)))
                (set-window-buffer win buffer)))
            (setq-default helm-display-function
                          #'my:helm-display-buffer)
            ;; Bindings, C-c ; to work in terminal
            (my:with-eval-after-load semantic
              (my:kmap "C-; i" "C-c ; i" #'helm-semantic-or-imenu))
            (my:kmap* helm-map
                      ("C-i" #'helm-execute-persistent-action)
                      ("<tab>" #'helm-execute-persistent-action)
                      ("C-z" #'helm-select-action))))

(my:with-package helm-swoop
  :ensure t
  :init (progn
          ;; Suppress compiler warning
          (my:kmap ([remap occur] #'helm-swoop) ; "M-s o"
                   ("M-s /" #'helm-multi-swoop))))

;; Completion
(my:with-package company
  :ensure t
  :init (global-company-mode t)
  :config (progn
            (my:kmap* company-mode-map
                      ("C-<tab>" #'company-complete))
            (my:kmap* company-active-map
                      ("C-p" #'company-select-previous)
                      ("C-n" #'company-select-next))
            (my:kmap "C-M-/" #'company-files)
            (setq-default company-tooltip-limit 20
                          ;; Put semantic backend on separate key
                          company-backends
                          (remove 'company-semantic company-backends))
            (my:with-eval-after-load semantic
              (defun my:company-semantic-setup ()
                "Sets `company-semantic' keybind locally"
                (local-set-key (kbd "C-<return>") #'company-semantic))
              (add-hook 'c-mode-hook #'my:company-semantic-setup)
              (add-hook 'c++-mode-hook #'my:company-semantic-setup))))

(my:with-package yasnippet
  :ensure t
  :defer t
  :init (yas-global-mode t)
  :config (progn
            ;; No more toolkit popups
            (setq-default
             yas-prompt-functions
             '(yas-ido-prompt yas-completing-prompt yas-no-prompt))
            ;; Just custom snippet dir
            (add-to-list 'yas-snippet-dirs my:snippets-dir)
            (my:with-eval-after-load smartparens
              (advice-add #'yas-expand :before
                          #'(lambda ()
                              "Escape from `smartparens-mode' overlay"
                              (let ((times 5))
                                (while (and (> times 0)
                                            (sp--get-active-overlay))
                                  (sp-remove-active-pair-overlay)
                                  (setq times (- times 1)))))))
            (add-hook 'term-mode-hook
                      (lambda () (yas-minor-mode -1)))))

(my:with-package function-args
  :ensure t
  :init (my:with-eval-after-load semantic
          (fa-config-default)))

;; External tools
(my:with-package ag
  :ensure t
  :config (setq-default ag-highlight-search t))

(my:with-package magit
  :ensure t
  :config (setq-default
           ;; by-word diff
           magit-diff-refine-hunk t))

(my:with-package ggtags
  :if (executable-find "gtags")
  :ensure t
  :init (progn
          (defun my:turn-on-ggtags-mode ()
            "Set `ggtags-mode' on"
            (ggtags-mode t))
          (add-hook 'c-mode-hook #'my:turn-on-ggtags-mode)
          (add-hook 'c++-mode-hook #'my:turn-on-ggtags-mode))
  :config (my:kmap* ggtags-mode-map
                    ("M-." "C-M-." nil)
                    ([remap find-tag] #'ggtags-find-tag-dwim)
                    ([remap find-tag-regexp] #'ggtags-find-tag-regexp)))

;; Project management and project tree
(my:with-package neotree
  :ensure t
  :init (my:kmap ("<f5>" #'neotree-show)
                 ("<f6>" #'neotree-find))
  :config (progn
            (defun my:neotree-create-window ()
              "Create global NeoTree window. Split root window."
              (let ((window nil)
                    (buffer (neo-global--get-buffer t))
                    (root (frame-root-window (selected-frame))))
                (setq window
                      (split-window root (- neo-window-width) 'left))
                (select-window window)
                (neo-window--init window buffer)
                (setq neo-global--window window)
                window))
            (defun my:neotree-insert-symbol (name)
              (cond ((equal name 'open) (neo-buffer--insert-with-face
                                         "- " 'neo-expand-btn-face))
                    ((equal name 'close) (neo-buffer--insert-with-face
                                          "+ " 'neo-expand-btn-face))
                    ((equal name 'leaf) (insert-char ?\s 2))))
            (fset #'neo-global--create-window #'my:neotree-create-window)
            (fset #'neo-buffer--insert-fold-symbol #'my:neotree-insert-symbol)
            ;; Allow delete window
            (setq-default neo-persist-show nil
                          neo-hidden-files-regexp "\\(^\\.\\|.py[cd]\\)"
                          neo-theme 'ascii)
            ;; Add jk movement
            (my:kmap* neotree-mode-map
                      ("r" #'neotree-refresh)
                      ("h" #'neotree-hidden-file-toggle)
                      ("a" #'neotree-stretch-toggle)
                      ("p" #'neotree-previous-node)
                      ("n" #'neotree-next-node)
                      ("k" #'neotree-previous-node)
                      ("j" #'neotree-next-node))))

(my:with-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config (progn
            (my:kmap "<f7>" #'neotree-projectile-action)
            ;; Try to emulate ede (from CEDET) project
            (my:with-eval-after-load semanticdb
              (setq-default semanticdb-project-root-functions
                            projectile-project-root-files-functions))))

(my:with-package helm-projectile
  :ensure t
  :defer t
  :init (progn
          (my:kmap "C-; p" "C-c ; p" #'helm-projectile)
          (helm-projectile-on)
          (fset #'helm-projectile-ag #'projectile-ag)))

;; Language specific
(my:with-package anaconda-mode
  :ensure t
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(my:with-package company-anaconda
  :ensure t
  :config (add-to-list 'company-backends #'company-anaconda))

(my:with-package company-c-headers
  :ensure t
  :config (progn
            ;; Get include path from Semantic
            (my:with-eval-after-load semantic/dep
              (setq company-c-headers-path-system #'my:system-include-path))
            (add-to-list 'company-backends #'company-c-headers)))

(my:with-package flycheck
  :ensure t)

(my:with-package js2-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.json" . js-mode)))

(my:with-package yaml-mode
  :ensure t)

(my:with-package lua-mode
  :ensure t
  :config (setq-default lua-indent-level 4))

(my:with-package rust-mode
  :ensure t)

(my:with-package haskell-mode
  :ensure t
  :config (progn
            (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation)))

(my:with-package company-ghc
  :if (executable-find "ghc-mod")
  :ensure t
  :config (progn
            (add-to-list 'company-backends #'company-ghc)
            (add-hook 'haskell-mode-hook #'ghc-init)))

(my:with-package irony
  :if (executable-find "clang")
  :ensure t
  :init (progn
          (add-hook 'c++-mode-hook #'irony-mode)
          (add-hook 'c-mode-hook #'irony-mode))
  :config (progn
            (my:with-package company-irony
              :ensure t
              :init (add-to-list 'company-backends
                                 (list #'company-c-headers #'company-irony)))
            (my:with-package flycheck-irony
              :ensure t
              :init (my:with-eval-after-load flycheck
                      (flycheck-irony-setup)))
            (my:with-package irony-eldoc
              :ensure t
              :init (add-hook 'irony-mode-hook #'irony-eldoc))))

(my:with-package clojure-mode
  :ensure t
  :config (add-hook 'clojure-mode-hook #'my:lisp-setup-paredit))

(my:with-package cider
  :if (executable-find "lein")
  :ensure t
  :config (progn
            (add-hook 'cider-repl-mode-hook
                      #'my:lisp-setup-paredit)
            (my:kmap* cider-mode-map
                      ("M-." "M-," nil)
                      ([remap find-tag] #'cider-jump-to-var)
                      ([remap pop-tag-mark] #'cider-jump-back))))


(provide 'init-impl)

;;; init-impl.el ends here
