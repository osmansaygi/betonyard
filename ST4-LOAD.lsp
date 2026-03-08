;;; ST4-LOAD.lsp - APPLOAD ile yukleyin, sonra ST4 yazin
;;; Ana dosya yuklenemezse bu dosya komutu tanimlar

(defun C:ST4IMPORT ()
  (setq st4file (findfile "ST4-to-AutoCAD-3D.lsp"))
  (if (not st4file)
    (setq st4file (getfiled "ST4-to-AutoCAD-3D.lsp dosyasini secin" "" "lsp" 0))
  )
  (if st4file
    (if (load st4file)
      (C:ST4IMPORT)
      (alert "Dosya yuklenemedi! Syntax hatasi olabilir.")
    )
    (princ "\nIptal edildi.")
  )
)

(defun C:ST4 () (C:ST4IMPORT))

(princ "\nST4-LOAD yuklendi. ST4 veya ST4IMPORT yazin.")
(princ)
