;;; init-compare.el --- Diff and merge  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-package)


(my:after ediff
  (setq
   ;; run control panel in same frame
   ediff-window-setup-function #'ediff-setup-windows-plain))


(defun my:mergetool (local remote &optional base merged)
  "Use ediff for merge conflict resolution.

LOCAL  :: ours, current branch, pre-image
REMOTE :: theirs, to be merged, post-image
BASE   :: common ancestor if available
MERGED :: destination

This function is meant to be used as VCS mergetool.

Git example:

\[mergetool \"ediff\"\]
cmd = emacsclient -c --eval \\\"(my:mergetool \\
    \\\\\\\"$LOCAL\\\\\\\"   \\
    \\\\\\\"$REMOTE\\\\\\\"  \\
    \\\\\\\"$BASE\\\\\\\"    \\
    \\\\\\\"$MERGED\\\\\\\") \\\"

\[difftool \"ediff\"\]
cmd = emacsclient -c --eval \\\"(my:mergetool \\
    \\\\\\\"$LOCAL\\\\\\\"   \\
    \\\\\\\"$REMOTE\\\\\\\"  \\
    \\\\\\\"$MERGED\\\\\\\"  \\
    nil) \\\"

See:
- `ediff-files',
- `ediff-files3',
- `ediff-merge-files',
- `ediff-merge-files-with-ancestor'."
  (if (or (null merged) (string-equal "" merged) (string-equal base merged))
      (if (file-readable-p base)
          (ediff-files3 local remote base)
        (ediff-files local remote))
    (if (file-readable-p base)
        (ediff-merge-files-with-ancestor local remote base nil merged)
      (ediff-merge-files local remote nil merged))))



(provide 'init-compare)

;;; init-compare.el ends here
