;;; ============================================================
;;; ST4_Aks_Ciz.lsp - STA4CAD ST4 dosyasından aksları ve kolonları AutoCAD'e çizer
;;; Eksen koordinatları cm cinsinden çizilir
;;; Kullanım: (load "ST4_Aks_Ciz.lsp") ardından ST4AKS komutu
;;;
;;; STA4CAD YAPI AKS BİLGİLERİ:
;;;   - X yönü (10xx) -> Dikey çizgiler 1,2...
;;;   - Y yönü (20xx) -> Yatay çizgiler A,B,C...
;;;   - Y koordinatı: - değerler yukarı, + değerler aşağı (çizimde y=-değer)
;;;   - Kolonlar: Column axis data kesişim noktalarında
;;; ============================================================

(defun C:ST4AKS (/ st4file f line xlist ylist x-egim y-egim x y ycoord xmin xmax ymin ymax
                   margin pt1 pt2 i textpt layer-axes layer-text layer-cols
                   parsed all-values all-slopes in-axis-section in-colaxis result
                   max-x-idx max-y-idx n-x n-y header-n-x header-n-y
                   line-num header-parts pos eg col-list col-dim-list
                   ax-id ay-id off1 off2 col-ang parts col-xy cx cy cw ch p1 p2 p3 p4
                   ix iy pos-x pos-y eg-x eg-y denom hw hh ang-rad cosa sina
                   loc-x loc-y dx dy)
  (vl-load-com)
  
  ;; ST4 dosyasını seç
  (setq st4file (getfiled "STA4CAD ST4 Dosyası Seçin" "" "st4" 0))
  (if (not st4file)
    (progn
      (princ "\nİşlem iptal edildi.")
      (exit)
    )
  )
  
  ;; Dosyayı aç ve oku
  (setq f (open st4file "r"))
  (if (not f)
    (progn
      (alert "Dosya açılamadı!")
      (exit)
    )
  )
  
  (setq xlist '() ylist '() x-egim '() y-egim '()
        all-values '() all-slopes '() in-axis-section nil in-colaxis nil in-coldata nil
        col-list '() col-dim-list '()
        max-x-idx 1000 max-y-idx 2000
        header-n-x nil header-n-y nil line-num 0)
  
  ;; Dosyayı oku: header satır 5'ten eksen sayısı (27,9) + axis data + 10xx/20xx indeksleri
  (while (setq line (read-line f))
    (setq line-num (1+ line-num))
    (cond
      ;; Satır 5: 27,9,... -> X ve Y eksen sayıları (STA4CAD header)
      ((and (= line-num 5) (> (strlen line) 0))
       (setq header-parts (split-string line ","))
       (if (>= (length header-parts) 2)
         (progn
           (setq header-n-x (atoi (vl-string-trim " \t" (nth 0 header-parts))))
           (setq header-n-y (atoi (vl-string-trim " \t" (nth 1 header-parts))))
         )
       )
      )
      ;; /Axis data/ satırı sonrasında referans dosya yolu olabilir
      ((wcmatch (strcase line T) "/axis data/*")
       (setq in-axis-section T)
      )
      ((and in-axis-section (wcmatch (strcase line T) "/circle axis/*"))
       (setq in-axis-section nil)
      )
      ((wcmatch (strcase line T) "*column axis data*")
       (setq in-colaxis T)
      )
      ((and in-colaxis (wcmatch (strcase line T) "/*"))
       (setq in-colaxis nil)
      )
      ((and in-colaxis (> (strlen line) 2))
       (setq parts (split-string line ","))
       (if (and (>= (length parts) 3)
                (setq ax-id (atoi (vl-string-trim " \t" (nth 1 parts))))
                (setq ay-id (atoi (vl-string-trim " \t" (nth 2 parts))))
                (>= ax-id 1001) (<= ax-id 1999)
                (>= ay-id 2001) (<= ay-id 2999))
         (progn
           (setq off1 (if (>= (length parts) 4) (atoi (vl-string-trim " \t" (nth 3 parts))) -1))
           (setq off2 (if (>= (length parts) 5) (atoi (vl-string-trim " \t" (nth 4 parts))) -1))
           (setq col-ang (if (>= (length parts) 7) (atof (vl-string-trim " \t" (nth 6 parts))) 0.0))
           (setq col-list (append col-list (list (list ax-id ay-id off1 off2 col-ang))))
         )
       )
      )
      ((wcmatch (strcase line T) "*columns data*")
       (setq in-coldata T)
      )
      ((and in-coldata (wcmatch (strcase line T) "/*"))
       (setq in-coldata nil)
      )
      ((and in-coldata (> (strlen line) 2))
       (setq parts (split-string line ","))
       (if (and (>= (length parts) 3)
                (setq cw (atof (nth 1 parts)))
                (setq ch (atof (nth 2 parts)))
                (> cw 0) (> ch 0)
                (>= (atoi (vl-string-trim " \t" (nth 0 parts))) 100))
         (progn
           ;; Columns Data: boyutlar zaten cm cinsinden
           (setq col-dim-list (append col-dim-list (list (list cw ch))))
         )
       )
      )
      ((and in-axis-section (> (strlen line) 0))
       (setq parsed (parse-axis-line line))
       (if parsed
         (progn
           (setq all-values (append all-values (list (cadr parsed))))
           (setq all-slopes (append all-slopes (list (car parsed))))
         )
       )
      )
      ;; Column axis, Floors, Beams satırlarından 10xx ve 20xx indekslerini tara
      (T
       (setq max-x-idx (max-axis-idx line 1000 1999 max-x-idx))
       (setq max-y-idx (max-axis-idx line 2000 2999 max-y-idx))
      )
    )
  )
  (close f)
  
  ;; X ve Y eksen sayıları: header (27,9) öncelikli, yoksa column axis'tan
  (if (and header-n-x header-n-y (> header-n-x 0) (> header-n-y 0))
    (progn (setq n-x header-n-x) (setq n-y header-n-y))
    (progn
      (setq n-x (max 0 (- max-x-idx 1000)))
      (setq n-y (max 0 (- max-y-idx 2000)))
    )
  )
  
  ;; STA4CAD: satır 1-27 = X (1001-1027), satır 28-36 = Y (2001-2009)
  ;; X -> dikey çizgiler (1,2...27), Y -> yatay çizgiler (A,B...9)
  (if (and (> n-x 0) (> n-y 0) (>= (length all-values) (+ n-x n-y)))
    (progn
      (setq xlist '() ylist '() x-egim '() y-egim '())
      (setq i 0)
      (foreach v all-values
        (cond
          ((< i n-x)
           (setq xlist (append xlist (list v)))
           (setq x-egim (append x-egim (list (if (< i (length all-slopes)) (nth i all-slopes) 0.0))))
          )
          ((< i (+ n-x n-y))
           (setq ylist (append ylist (list v)))
           (setq y-egim (append y-egim (list (if (< i (length all-slopes)) (nth i all-slopes) 0.0))))
          )
        )
        (setq i (1+ i))
      )
    )
    ;; Fallback: monotonic split (egim bilgisi yok, 0 kabul)
    (if (> (length all-values) 1)
      (progn
        (setq result (split-axis-by-monotonic all-values))
        (setq xlist (car result))
        (setq ylist (cadr result))
        (setq x-egim (mapcar '(lambda (x) 0.0) xlist))
        (setq y-egim (mapcar '(lambda (x) 0.0) ylist))
      )
    )
  )
  
  ;; Boş listeleri kontrol et
  (if (and (null xlist) (null ylist))
    (progn
      (alert "Axis data bölümünde geçerli eksen verisi bulunamadı!")
      (exit)
    )
  )
  
  ;; Çizim sınırları (cm) - Y: - değerler yukarı, + değerler aşağı (y = -değer)
  (setq margin 50)
  (setq xmin (if xlist (- (apply 'min xlist) margin) 0))
  (setq xmax (if xlist (+ (apply 'max xlist) margin) 1000))
  (setq ymin (if ylist (- (- (apply 'max ylist)) margin) -1000))
  (setq ymax (if ylist (+ (- (apply 'min ylist)) margin) 1000))
  
  ;; Egimli akslarin uclarini da sinirlara dahil et - iceride kalan akslar disari uzansin
  (setq i 0)
  (while (< i (length ylist))
    (setq eg (nth i y-egim))
    (if (not (equal eg 0.0 1e-9))
      (progn
        (setq pos (nth i ylist))
        (setq ymin (min ymin (- (+ pos (* eg xmin))) (- (+ pos (* eg xmax)))))
        (setq ymax (max ymax (- (+ pos (* eg xmin))) (- (+ pos (* eg xmax)))))
      )
    )
    (setq i (1+ i))
  )
  (setq i 0)
  (while (< i (length xlist))
    (setq eg (nth i x-egim))
    (if (not (equal eg 0.0 1e-9))
      (progn
        (setq pos (nth i xlist))
        (setq xmin (min xmin (+ pos (* eg ymin)) (+ pos (* eg ymax))))
        (setq xmax (max xmax (+ pos (* eg ymin)) (+ pos (* eg ymax))))
      )
    )
    (setq i (1+ i))
  )
  (setq xmin (- xmin margin) xmax (+ xmax margin) ymin (- ymin margin) ymax (+ ymax margin))
  
  (if (null xlist) (setq xlist (list 0 1000)))
  (if (null ylist) (setq ylist (list 0 1000)))
  
  ;; Katmanları oluştur
  (setq layer-axes "ST4-AKS-CIZGILER")
  (setq layer-text "ST4-AKS-ETIKETLER")
  (setq layer-cols "ST4-KOLONLAR")
  (create-layer layer-axes 1)   ; Kırmızı
  (create-layer layer-text 3)  ; Yeşil
  (create-layer layer-cols 5)   ; Mavi
  
  ;; Mevcut çizimi sakla
  (setvar "CMDECHO" 0)
  (command "_.UNDO" "BEGIN")
  
  ;; X eksenlerini çiz (dikey) - egim=0 duz, egim!=0 egik (x = pos + egim*y)
  (setvar "CLAYER" layer-axes)
  (setq i 0)
  (while (< i (length xlist))
    (setq pos (nth i xlist)
          eg (nth i x-egim))
    (if (equal eg 0.0 1e-9)
      (progn
        (setq pt1 (list pos ymin 0))
        (setq pt2 (list pos ymax 0))
      )
      (progn
        (setq pt1 (list (+ pos (* eg ymin)) ymin 0))
        (setq pt2 (list (+ pos (* eg ymax)) ymax 0))
      )
    )
    (entmake (list '(0 . "LINE")
                   (cons 10 pt1)
                   (cons 11 pt2)
                   (cons 8 layer-axes)))
    (setq i (1+ i))
  )
  
  ;; Y eksenlerini çiz (yatay) - egim=0 duz, egim!=0 egik (y = pos + egim*x), çizimde y = -deger
  (setq i 0)
  (while (< i (length ylist))
    (setq pos (nth i ylist)
          eg (nth i y-egim))
    (if (equal eg 0.0 1e-9)
      (progn
        (setq ycoord (- pos))
        (setq pt1 (list xmin ycoord 0))
        (setq pt2 (list xmax ycoord 0))
      )
      (progn
        (setq pt1 (list xmin (- (+ pos (* eg xmin))) 0))
        (setq pt2 (list xmax (- (+ pos (* eg xmax))) 0))
      )
    )
    (entmake (list '(0 . "LINE")
                   (cons 10 pt1)
                   (cons 11 pt2)
                   (cons 8 layer-axes)))
    (setq i (1+ i))
  )
  
  ;; Kolonlar: Column axis kesişim noktalarında dikdörtgen
  (setvar "CLAYER" layer-cols)
  (setq i 0)
  (foreach col col-list
    (setq ax-id (car col) ay-id (cadr col)
          off1 (nth 2 col) off2 (nth 3 col) col-ang (if (>= (length col) 5) (nth 4 col) 0.0)
          ix (- ax-id 1001) iy (- ay-id 2001))
    (if (and (>= ix 0) (< ix (length xlist)) (>= iy 0) (< iy (length ylist)))
      (progn
        (setq cw 40.0 ch 40.0)
        (if (and col-dim-list (< i (length col-dim-list)))
          (setq cw (car (nth i col-dim-list)) ch (cadr (nth i col-dim-list)))
        )
        (setq hw (/ cw 2.0) hh (/ ch 2.0))
        (setq pos (nth ix xlist) eg (nth ix x-egim))
        (setq pos-x pos eg-x (if (< ix (length x-egim)) (nth ix x-egim) 0.0))
        (setq pos (nth iy ylist) eg (nth iy y-egim))
        (setq pos-y pos eg-y (if (< iy (length y-egim)) (nth iy y-egim) 0.0))
        (if (and (equal eg-x 0.0 1e-9) (equal eg-y 0.0 1e-9))
          (setq cx pos-x cy (- pos-y))
          (progn
            (setq denom (+ 1.0 (* eg-x eg-y)))
            (if (> (abs denom) 1e-12)
              (progn
                (setq cx (/ (- pos-x (* eg-x pos-y)) denom))
                (setq cy (- (+ pos-y (* eg-y cx))))
              )
              (setq cx pos-x cy (- pos-y))
            )
          )
        )
        ;; X yonu kaciklik (4.sutun): -1=aks solda, 1=aks sagda, 0=ortalı
        ;; diger: negatif (-300)=aks solda, kolon aksa 300mm girmis
        ;;        pozitif (300)=aks sagda, kolon kenari aksa 300mm girmis
        (cond
          ((= off1 -1) (setq loc-x hw))
          ((= off1 1) (setq loc-x (- hw)))
          ((and (numberp off1) (< off1 0)) (setq loc-x (+ (/ off1 10.0) hw)))
          ((and (numberp off1) (> off1 0)) (setq loc-x (- (/ off1 10.0) hw)))
          (T (setq loc-x 0.0))
        )
        ;; Y yonu kaciklik (5.sutun): -1=aks yukarda, 1=aks asagida, 0=ortalı
        ;; diger: negatif (-300)=aks yukarda, kolon aksa 300mm girmis
        ;;        pozitif (300)=aks asagida, kolon alt kenari aksa 300mm girmis
        (cond
          ((= off2 -1) (setq loc-y (- hh)))
          ((= off2 1) (setq loc-y hh))
          ((and (numberp off2) (< off2 0)) (setq loc-y (+ (/ off2 -10.0) (- hh))))
          ((and (numberp off2) (> off2 0)) (setq loc-y (+ (/ off2 -10.0) hh)))
          (T (setq loc-y 0.0))
        )
        ;; Acili kolonlar icin kaciklik kolon acisina gore uygulanir (local -> global)
        (if (and (numberp col-ang) (not (equal col-ang 0.0 1e-9)))
          (progn
            (setq ang-rad (* col-ang (/ pi 180.0))
                  cosa (cos ang-rad) sina (sin ang-rad))
            (setq dx (- (* loc-x cosa) (* loc-y sina))
                  dy (+ (* loc-x sina) (* loc-y cosa)))
            (setq cx (+ cx dx) cy (+ cy dy))
          )
          (setq cx (+ cx loc-x) cy (+ cy loc-y))
        )
        (setq p1 (list (- cx hw) (- cy hh) 0))
        (setq p2 (list (+ cx hw) (- cy hh) 0))
        (setq p3 (list (+ cx hw) (+ cy hh) 0))
        (setq p4 (list (- cx hw) (+ cy hh) 0))
        ;; 7.sutun: kolon acisi (derece) - merkez etrafinda dondur
        (if (and (numberp col-ang) (not (equal col-ang 0.0 1e-9)))
          (progn
            (setq ang-rad (* col-ang (/ pi 180.0))
                  cosa (cos ang-rad) sina (sin ang-rad))
            (setq p1 (list (+ cx (- (* (- (car p1) cx) cosa) (* (- (cadr p1) cy) sina)))
                        (+ cy (+ (* (- (car p1) cx) sina) (* (- (cadr p1) cy) cosa))) 0))
            (setq p2 (list (+ cx (- (* (- (car p2) cx) cosa) (* (- (cadr p2) cy) sina)))
                        (+ cy (+ (* (- (car p2) cx) sina) (* (- (cadr p2) cy) cosa))) 0))
            (setq p3 (list (+ cx (- (* (- (car p3) cx) cosa) (* (- (cadr p3) cy) sina)))
                        (+ cy (+ (* (- (car p3) cx) sina) (* (- (cadr p3) cy) cosa))) 0))
            (setq p4 (list (+ cx (- (* (- (car p4) cx) cosa) (* (- (cadr p4) cy) sina)))
                        (+ cy (+ (* (- (car p4) cx) sina) (* (- (cadr p4) cy) cosa))) 0))
          )
        )
        (entmake (list '(0 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       (cons 8 layer-cols)
                       '(100 . "AcDbPolyline")
                       '(90 . 4)
                       '(70 . 1)
                       (cons 10 (list (car p1) (cadr p1)))
                       (cons 10 (list (car p2) (cadr p2)))
                       (cons 10 (list (car p3) (cadr p3)))
                       (cons 10 (list (car p4) (cadr p4)))
                       )
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; Eksen etiketlerini yaz
  (setvar "CLAYER" layer-text)
  (setq i 0)
  (while (< i (length xlist))
    (setq pos (nth i xlist) eg (nth i x-egim))
    (if (equal eg 0.0 1e-9)
      (setq textpt (list pos (+ ymax 20) 0))
      (setq textpt (list (+ pos (* eg ymax)) (+ ymax 20) 0))
    )
    (entmake (list '(0 . "TEXT")
                   (cons 10 textpt) (cons 40 15) (cons 1 (itoa (1+ i)))
                   (cons 7 "Standard") (cons 8 layer-text) (cons 72 1) (cons 11 textpt) (cons 73 2)))
    (setq i (1+ i))
  )
  
  (setq i 0)
  (while (< i (length ylist))
    (setq pos (nth i ylist) eg (nth i y-egim))
    (if (equal eg 0.0 1e-9)
      (setq textpt (list (+ xmax 20) (- pos) 0))
      (setq textpt (list (+ xmax 20) (- (+ pos (* eg xmax))) 0))
    )
    (entmake (list '(0 . "TEXT")
                   (cons 10 textpt) (cons 40 15) (cons 1 (num-to-alpha (1+ i)))
                   (cons 7 "Standard") (cons 8 layer-text) (cons 72 0) (cons 11 textpt) (cons 73 2)))
    (setq i (1+ i))
  )
  
  ;; Koordinat değerlerini (cm) yaz
  (setq i 0)
  (while (< i (length xlist))
    (setq pos (nth i xlist) eg (nth i x-egim))
    (if (equal eg 0.0 1e-9)
      (setq textpt (list pos (- ymin 25) 0))
      (setq textpt (list (+ pos (* eg ymin)) (- ymin 25) 0))
    )
    (entmake (list '(0 . "TEXT")
                   (cons 10 textpt) (cons 40 8) (cons 1 (rtos pos 2 0))
                   (cons 7 "Standard") (cons 8 layer-text) (cons 72 1) (cons 11 textpt) (cons 73 2)))
    (setq i (1+ i))
  )
  
  (setq i 0)
  (while (< i (length ylist))
    (setq pos (nth i ylist))
    (setq textpt (list (- xmin 40) (- pos) 0))
    (entmake (list '(0 . "TEXT")
                   (cons 10 textpt) (cons 40 8) (cons 1 (rtos pos 2 0))
                   (cons 7 "Standard") (cons 8 layer-text) (cons 72 1) (cons 11 textpt) (cons 73 2)))
    (setq i (1+ i))
  )
  
  (command "_.UNDO" "END")
  (setvar "CMDECHO" 1)
  
  ;; Zoom extents
  (command "_.ZOOM" "_E")
  
  (princ (strcat "\n" (itoa (length xlist)) " X ekseni, " (itoa (length ylist)) " Y ekseni, "
                 (itoa (length col-list)) " kolon cizildi. (cm)"))
  (princ)
)

;;; Satırdan verilen aralıktaki (min-idx, max-idx) en büyük tam sayıyı bulur
(defun max-axis-idx (line min-idx max-idx current-max / parts num)
  (setq parts (split-string line ","))
  (foreach p parts
    (setq num (atoi (vl-string-trim " \t" p)))
    (if (and (>= num min-idx) (<= num max-idx) (> num current-max))
      (setq current-max num)
    )
  )
  current-max
)

;;; X aksları soldan sağa artan, Y aksları yukarıdan aşağıya artan
;;; İlk azalma noktasında X bloğu biter, Y bloğu başlar
;;; Returns: (xlist ylist)
(defun split-axis-by-monotonic (all-values / i prev xlist ylist split-idx)
  (setq split-idx nil prev nil i 0)
  (foreach v all-values
    (if (and prev (< v prev))
      (if (null split-idx)
        (setq split-idx i)
      )
    )
    (setq prev v i (1+ i))
  )
  (if split-idx
    (progn
      (setq xlist '() ylist '() i 0)
      (foreach v all-values
        (if (< i split-idx)
          (setq xlist (append xlist (list v)))
          (setq ylist (append ylist (list v)))
        )
        (setq i (1+ i))
      )
      (list xlist ylist)
    )
    ;; Hiç azalma yoksa tümü X
    (list all-values '())
  )
)

;;; Axis data satırını parse eder
;;; Format: "egim,deger,0,0,0" - 1.sütun egim (tanjant), 2.sütun orijine uzaklik (koordinat)
;;; ST4'te değerler metre cinsinden - *100 ile cm'ye çeviriyoruz
(defun parse-axis-line (line / parts egim val)
  (setq parts (split-string line ","))
  (if (>= (length parts) 2)
    (progn
      (setq egim (atof (vl-string-trim " \t" (nth 0 parts))))
      (setq val (atof (nth 1 parts)))
      ;; Değer 100'den küçükse metre kabul edip cm'ye çevir
      (if (< (abs val) 100)
        (setq val (* val 100))
      )
      (list egim val)
    )
    nil
  )
)

;;; Virgülle ayrılmış stringi listeye çevirir
(defun split-string (str delim / pos result)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (append result (list str))
)

;;; Sayıyı harfe çevirir: 1→A, 2→B, ..., 26→Z, 27→AA, ...
(defun num-to-alpha (n / result)
  (setq result "")
  (while (> n 0)
    (setq n (1- n))
    (setq result (strcat (chr (+ 65 (rem n 26))) result))
    (setq n (fix (/ n 26)))
  )
  result
)

;;; Katman oluşturur (yoksa)
(defun create-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   (cons 70 0)
                   (cons 62 color)))
  )
  (princ)
)

(princ "\nST4_Aks_Ciz yuklendi. Komut: ST4AKS")
(princ)
