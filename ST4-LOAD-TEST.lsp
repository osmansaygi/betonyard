;;; Minimal test - bu dosya yuklenirse ST4IMPORT komutu tanimlanir
(defun C:ST4IMPORT ()
  (alert "ST4IMPORT komutu calisiyor! LISP yuklendi.")
)
(defun C:ST4 () (C:ST4IMPORT))
(princ "\nST4-LOAD-TEST yuklendi. ST4IMPORT veya ST4 yazin.")
(princ)
