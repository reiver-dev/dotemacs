;;; init-hashmap.el --- Hash tables and sets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defun my:plist-replace (func plist prop)
  "Apply FUNC to value of PROP in PLIST.
The function will result in replacing the value by FUNC result if the
PROP is already in the PLIST. Nothing happens if there is no such PROP
in PLIST."
  (let ((tail (plist-member plist prop)))
    (when tail
      (setcar (cdr tail) (funcall func (cadr tail)))))
  plist)


(defun my:make-hash-table-from-plist (plist &rest keyword-args)
  "Construct new hash-table from PLIST.
Optional KEYWORD-ARGS confugre the table as in `make-hash-table'."
  (let ((table (apply #'make-hash-table keyword-args)))
    (while plist
      (let ((key (car plist))
            (value (cadr plist))
            (tail (cddr plist)))
        (puthash key value table)
        (setq plist tail)))
    table))


(defun my:plist-default (plist &rest defaults)
  "Add values from DEFAULTS propety list to PLIST if missing."
  (let ((defaults (my:make-hash-table-from-plist defaults))
        (it plist))
    (while it
      (let ((key (car it))
            (tail (cddr it)))
        (remhash key defaults)
        (setq it tail)))
    (maphash
     (lambda (key value)
       (setq plist (cons key (cons value plist))))
     defaults))
  plist)


(defun my:make-hash-table-like (table &rest keyword-args)
  "Construct new empty hash table.
The result shares properties with TABLE. Optional KEYWORD-ARGS
allow to override the properties."
  (apply #'make-hash-table
         (my:plist-default
          keyword-args
          :size (hash-table-size table)
          :test (hash-table-test table)
          :weakness (hash-table-weakness table)
          :rehash-size (hash-table-rehash-size table)
          :rehash-threshold (hash-table-rehash-threshold table))))


(defun my:make-hash-set (list &rest keyword-args)
  "Create new hash table from LIST of elements.
KEYWORD-ARGS are same as in `make-hash-table'. If :size is not set,
then it is defaulted to length of the LIST."
  (let ((table (apply #'make-hash-table
                      (if (not (plist-member keyword-args :size))
                          (cons :size (cons (length list) keyword-args))
                        keyword-args))))
    (dolist (key list)
      (puthash key t table))
    table))


(defun my:hash-table-intersect (a b)
  "Find intersection of two hash-tables A and B.
Values of common keys and hash-table parameters for the new
hash-table are taken from table A."
  (let* ((count-a (hash-table-count a))
         (count-b (hash-table-count b))
         (result (make-hash-table
                  :test (hash-table-test a)
                  :size (min count-a count-b)
                  :rehash-size (hash-table-rehash-size a)
                  :rehash-threshold (hash-table-rehash-threshold a)
                  :weakness (hash-table-weakness a))))
    (if (<= count-a count-b)
        (maphash (lambda (key val)
                   (when (gethash key b nil)
                     (puthash key val result)))
                 a)
      (maphash (lambda (key val)
                 (let ((val-a (gethash key a)))
                   (when val-a
                     (puthash key val result))))
               b))
    result))


(provide 'init-hashmap)

;;; init-hashmap.el ends here
