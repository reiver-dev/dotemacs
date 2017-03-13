;;; init-wm.el --- Window management  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-list)

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

(defun my:apply-to-window (action window &rest args)
  "Calls ACTION with argument WINDOW, switches frame focus if required"
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
    (display-buffer-pop-up-frame (window-buffer window) nil)
    (delete-window window)))

(provide 'init-wm)

;;; init-wm.el ends here
