;;; init-filesystem.el --- Filesystem functions -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This file provides functions to work with filesystem,
;; paths, files; helpers with .dir-locals.el

;;; Code:

(eval-when-compile
  (require 'init-defs)
  (require 'init-list))


(defun my:current-fs-point ()
  "Get current variable `buffer-file-name' or `default-directory'."
  (or (buffer-file-name) default-directory))

(defun my:parent-dir (target)
  "Just gets relative top directory for given TARGET path."
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


(my:when-windows
  (defun my:expand-file-name (name &optional directory)
    "Convert filename NAME to absolute, and canonicalize it.
Start from DEFAULT-DIRECTORY if set. Same as `expand-file-name'
but lowercase paths on windows. On other systems is just alias."
    (let ((w32-downcase-file-names t))
      (expand-file-name name directory))))

(my:when-posix
  (defalias 'my:expand-file-name 'expand-file-name))


(defun my:slurp (file)
  "Read FILE from filesystem."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))


(defun my:slurp-lines (file)
  "Read FILE from filesystem, split lines, trim whitespace."
  (with-temp-buffer
    (insert-file-contents file)
    (mapcar
     #'(lambda (line)
         (if (string-match "[ \t]+$" line)
             (replace-match "" t t line)
           line))
     (split-string (buffer-string)  "\n" t))))


(defun my:files-in-below-directory (directory pattern &optional ignore)
  "List the file names in DIRECTORY and in its sub-dirs equal to PATTERN.
Optional IGNORE argument can be list of names to ignore in recursive walk
or function receiving directory name as single argument.
See `directory-files-recursively' (since 25)."
  (let (el-files-list
        (current-directory-list
         (with-demoted-errors
             (directory-files directory t nil nil)))
        (ignore-func (cond
                      ((functionp ignore) ignore)
                      ((listp ignore) (lambda (dir) (member dir ignore)))
                      (t (lambda (_dir) nil)))))
    (while current-directory-list
      (let* ((file (car current-directory-list))
             (filename (file-relative-name file directory)))
        (when (string= pattern filename)
          (setq el-files-list (cons file el-files-list)))
        (when (and (file-directory-p (car current-directory-list))
                   (not (equal "." (substring file -1)))
                   (not (funcall ignore-func file)))
          (setq
           el-files-list
           (append (my:files-in-below-directory file pattern)
                   el-files-list))))
      (setq current-directory-list (cdr current-directory-list)))
    el-files-list))

(defun my:dir-locals ()
    "Find directory local (.dir-locals.el) settings location;
raises error if not found"
    (let* ((dl-path
            (dir-locals-find-file (my:current-fs-point))))
      (cond
       ((stringp dl-path) (file-name-directory dl-path))
       ((consp dl-path) (car dl-path))
       (t (error "Dir-locals location is undefined")))))


(defun my:dir-locals-path (relative)
  "Get filepath from directory local settings location.
Path is pecified by RELATIVE argument.  See `expand-file-name'."
  (expand-file-name relative (my:dir-locals)))


(defmacro my:with-local-dir (relative &rest body)
  "Evaluate BODY in RELATIVE location to \".dir-locals.el\"."
  `(let ((default-directory
           (file-name-as-directory (my:dir-locals-path ,relative))))
     ,@body))


(defconst my:path-regexp
  (let* ((root (if (eq system-type 'windows-nt)
                   "[a-zA-Z]:/"
                 "/"))
         (begin (concat "\\(?:\\.\\{1,2\\}/\\|~/\\|" root "\\)")))
    (list (concat "\"\\(" begin "[^\"\n]*\\)")
          (concat "\'\\(" begin "[^\'\n]*\\)")
          (concat "\\(?:[ \t=]\\|^\\)\\(" begin "[^ \t\n]*\\)"))))


(defun my:regexp-find-current-line (regexp &optional expression limit)
  "Perform REGEXP search on current line. Get EXPRESSION group up to LIMIT."
  (let ((inhibit-field-text-motion t)
        (group (or expression 0)))
    (when (looking-back regexp limit)
      (or (match-string-no-properties group) ""))))


(defun my:file-connected-p (file)
  (or (not (file-remote-p file))
      (file-remote-p file nil t)))


(defun my:find-path-at-point ()
  (let ((file (my:find-first
               (lambda (re)
                 (my:regexp-find-current-line re 1 (point-at-bol)))
               my:path-regexp)))
    (when (and file (my:file-connected-p file))
      (let ((dir (file-name-directory file)))
        (when (and dir
                   (not (string-match "//" dir))
                   (file-exists-p dir))
          file)))))


(provide 'init-filesystem)

;;; init-filesystem.el ends here
