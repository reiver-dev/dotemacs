;;; init-project.el --- Helpers for .dir-locals.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-list)

(defun my:current-fs-point ()
  "Returns current `buffer-file-name' or `default-directory'"
  (if (stringp buffer-file-name) buffer-file-name default-directory))

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
                      (t (lambda (_dir) nil)))))
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
         (locate-dominating-file (my:current-fs-point) ".dir-locals.el")))
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


(provide 'init-project)

;;; init-project.el ends here