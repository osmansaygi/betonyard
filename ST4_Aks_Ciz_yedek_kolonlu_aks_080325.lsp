;;; ST4_Aks_Ciz.lsp - STA4CAD ST4 dosyasindan akslari ve kolonlari AutoCAD'e cizer
;;; Kullanim: (load "ST4_Aks_Ciz.lsp") ardindan ST4AKS komutu
;;; Yedek: ST4_Aks_Ciz_yedek_kolonlu_aks_080325.lsp - Kolonlu/kolonsuz aks layer ayirimi

;;; Virgulle ayrilmis stringi listeye cevirir
(defun split-string (str delim / pos result)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (append result (list str))
)

;;; Poligon nokta listesinden LWPOLYLINE entmake listesi olusturur
(defun make-poly-entlist (pts layer / entlist pt)
  (setq entlist (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
    '(100 . "AcDbPolyline") (cons 90 (length pts)) '(70 . 1)))
  (foreach pt pts
    (setq entlist (append entlist (list (cons 10 (list (car pt) (cadr pt))))))
  )
  entlist
)

;;; Lokal poligon noktalarini global koordinata cevirir
;;; ST4 format: 1.sutun X (neg=sol), 2.sutun Y (neg=yukari, poz=asagi)
;;; Cizim: global_x=cx+x, global_y=cy-y (y neg->yukari)
(defun poly-local-to-global (pts cx cy / result p)
  (setq result nil)
  (foreach p pts
    (setq result (append result (list (list (+ cx (car p)) (- cy (cadr p)) 0))))
  )
  result
)

;;; Nokta listesini merkez etrafinda dondurur (derece)
(defun poly-rotate-pts (pts cx cy ang-deg / ang c s result p px py)
  (setq ang (* ang-deg (/ pi 180.0)) c (cos ang) s (sin ang) result nil)
  (foreach p pts
    (setq px (car p) py (cadr p))
    (setq result (append result (list (list
      (+ cx (- (* (- px cx) c) (* (- py cy) s)))
      (+ cy (+ (* (- px cx) s) (* (- py cy) c))) 0))))
  )
  result
)

(defun C:ST4AKS (/ st4file f line xlist ylist x-egim y-egim x y ycoord xmin xmax ymin ymax
                   margin pt1 pt2 i textpt layer-axes layer-text layer-cols layer-colnum
                   layer-axes-col layer-axes-nocol layer-axes-curr
                   x-axis-has-col y-axis-has-col
                   parsed all-values all-slopes in-axis-section in-colaxis result
                   max-x-idx max-y-idx n-x n-y header-n-x header-n-y
                   line-num header-parts pos eg col-list col-dim-list
                   ax-id ay-id off1 off2 col-ang col-num col-type parts col-xy cx cy cw ch dims p1 p2 p3 p4
                   ix iy pos-x pos-y eg-x eg-y denom hw hh ang-rad cosa sina
                   loc-x loc-y dx dy
                   in-polygon-section in-polygon-cols polygon-sections polygon-col-section-ids polygon-col-positions
                   polygon-section-offsets last-poly-sect-id pos-id
                   poly-pts poly-idx sect-id poly-sect-id pos-idx vx vy pt rot-pt poly-offset
                   in-story story-lines story-parts floor-list floor-num floor-name floor-short floor-elev
                   floor-idx floor-width floor-gap offset-x)
  (vl-load-com)
  
  ;; ST4 dosyasini sec
  (setq st4file (getfiled "STA4CAD ST4 Dosyasi Secin" "" "st4" 0))
  (if (not st4file)
    (progn
      (princ "\nIslem iptal edildi.")
      (exit)
    )
  )
  
  ;; Dosyayi ac ve oku
  (setq f (open st4file "r"))
  (if (not f)
    (progn
      (alert "Dosya acilamadi!")
      (exit)
    )
  )
  
  (setq xlist '() ylist '() x-egim '() y-egim '()
        all-values '() all-slopes '() in-axis-section nil in-colaxis nil in-coldata nil
        in-polygon-section nil in-polygon-cols nil in-story nil
        col-list '() col-dim-list '()
        polygon-sections '() polygon-col-section-ids '() polygon-col-positions '() polygon-section-offsets '()
        last-poly-sect-id nil story-lines '()
        max-x-idx 1000 max-y-idx 2000
        header-n-x nil header-n-y nil line-num 0)
  
  ;; Dosyayi oku: header satir 5'ten eksen sayisi (27,9) + axis data + 10xx/20xx indeksleri
  (while (setq line (read-line f))
    (setq line-num (1+ line-num))
    (cond
      ;; Satir 5 veya 6: n-x,n-y,100/1000,...,0,1 -> X ve Y eksen sayilari (1.sutun=X, 2.sutun=Y)
      ;; Format: 3. sutun 100 veya 1000 ise eksen sayisi satiridir (SL_02: 8,7 / FHM_GO_B: 41,43)
      ((and (or (= line-num 5) (= line-num 6)) (> (strlen line) 0))
       (setq header-parts (split-string line ","))
       (if (and (>= (length header-parts) 3)
                (member (vl-string-trim " \t" (nth 2 header-parts)) '("100" "1000")))
         (progn
           (setq header-n-x (atoi (vl-string-trim " \t" (nth 0 header-parts))))
           (setq header-n-y (atoi (vl-string-trim " \t" (nth 1 header-parts))))
         )
       )
      )
      ;; /Story/ bolumu - kat listesi
      ((wcmatch (strcase line T) "/story/*")
       (setq in-story T)
      )
      ((and in-story (wcmatch (strcase line T) "/axis data/*"))
       (setq in-story nil)
       (setq in-axis-section T)
      )
      ((and in-story (> (strlen line) 0))
       (setq story-lines (append story-lines (list line)))
      )
      ;; /Axis data/ - /Circle Axis/ arasi: egim,deger,0,0,0 formatinda axis satirlari
      ;; /Axis data/ satirinda &path olabilir (ornek: /Axis data/     &d:\...\AKS8.dwg)
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
           ;; 1.sutun: kesit tipi 1=dikdortgen, 2=yuvarlak, 3=poligon
           (setq col-type (atoi (vl-string-trim " \t" (nth 0 parts))))
           (if (or (< col-type 1) (> col-type 3)) (setq col-type 1))
           (setq col-num (1+ (length col-list)))
           (setq off1 (if (>= (length parts) 4) (atoi (vl-string-trim " \t" (nth 3 parts))) -1))
           (setq off2 (if (>= (length parts) 5) (atoi (vl-string-trim " \t" (nth 4 parts))) -1))
           (setq col-ang (if (>= (length parts) 7) (atof (vl-string-trim " \t" (nth 6 parts))) 0.0))
           (setq col-list (append col-list (list (list col-num col-type ax-id ay-id off1 off2 col-ang))))
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
                (setq sect-id (atoi (vl-string-trim " \t" (nth 0 parts))))
                (>= sect-id 100)
                (setq cw (atof (nth 1 parts)))
                (setq ch (atof (nth 2 parts)))
                (> cw 0) (> ch 0))
         (progn
           ;; Columns Data: section_id -> (cw ch) alist, boyutlar cm
           (setq col-dim-list (append col-dim-list (list (cons sect-id (list cw ch)))))
         )
       )
      )
      ((wcmatch (strcase line T) "*polygon columns*")
       (setq in-polygon-cols T)
      )
      ((wcmatch (strcase line T) "*polygon section*")
       (setq in-polygon-cols nil)
       (setq in-polygon-section T)
       (setq last-poly-sect-id nil)
      )
      ((and in-polygon-cols (wcmatch (strcase line T) "/*"))
       (setq in-polygon-cols nil)
      )
      ((and in-polygon-cols (> (strlen line) 2))
       (setq parts (split-string line ","))
       (if (and (>= (length parts) 2)
                (setq pos-id (atoi (vl-string-trim " \t" (nth 0 parts))))
                (>= pos-id 100))
         (progn
           (setq polygon-col-positions (append polygon-col-positions (list pos-id)))
           (setq polygon-col-section-ids (append polygon-col-section-ids
             (list (atoi (vl-string-trim " \t" (nth 1 parts))))))
         )
       )
      )
      ((and in-polygon-section (wcmatch (strcase line T) "/*"))
       (setq in-polygon-section nil)
      )
      ((and in-polygon-section (> (strlen line) 2))
       (setq parts (split-string line ","))
       (if (>= (length parts) 3)
         (progn
           (setq vx (atof (vl-string-trim " \t" (nth 0 parts))))
           (setq vy (atof (vl-string-trim " \t" (nth 1 parts))))
           (setq sect-id (atoi (vl-string-trim " \t" (nth 2 parts))))
           (if (= sect-id 0)
             ;; 9. satir: aks kesisiminden kaciklik (dx, dy) - 0,0,0 = kaciklik yok
             (if last-poly-sect-id
               (setq polygon-section-offsets (append polygon-section-offsets (list (cons last-poly-sect-id (list vx vy)))))
             )
             (progn
               (setq last-poly-sect-id sect-id)
               (setq poly-pts (assoc sect-id polygon-sections))
               (if poly-pts
                 (setq polygon-sections (subst (cons sect-id (append (cdr poly-pts) (list (list vx vy)))) poly-pts polygon-sections))
                 (setq polygon-sections (append polygon-sections (list (cons sect-id (list (list vx vy))))))
               )
             )
           )
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
      ;; Column axis, Floors, Beams satirlarindan 10xx ve 20xx indekslerini tara
      (T
       (setq max-x-idx (max-axis-idx line 1000 1999 max-x-idx))
       (setq max-y-idx (max-axis-idx line 2000 2999 max-y-idx))
      )
    )
  )
  (close f)
  
  ;; X ve Y eksen sayilari: header (27,9) oncelikli, yoksa column axis'tan
  (if (and header-n-x header-n-y (> header-n-x 0) (> header-n-y 0))
    (progn (setq n-x header-n-x) (setq n-y header-n-y))
    (progn
      (setq n-x (max 0 (- max-x-idx 1000)))
      (setq n-y (max 0 (- max-y-idx 2000)))
    )
  )
  
  ;; STA4CAD: X -> dikey cizgiler (1001...), Y -> yatay cizgiler (2001...)
  ;; Axis data: /Axis data/ ile /Circle Axis/ arasindaki tum egim,deger satirlari sayilir
  ;; En az n-x deger varsa header split kullan (1 eksik olsa bile calisir)
  (if (and (> n-x 0) (> n-y 0) (>= (length all-values) n-x))
    (progn
      (setq xlist '() ylist '() x-egim '() y-egim '())
      (setq i 0)
      (foreach v all-values
        (cond
          ((< i n-x)
           (setq xlist (append xlist (list v)))
           ;; X yonu aks egimi: ters cikiyorsa -1 ile carp
           (setq x-egim (append x-egim (list (- (if (< i (length all-slopes)) (nth i all-slopes) 0.0)))))
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
        (setq x-egim nil y-egim nil)
        (foreach v xlist (setq x-egim (append x-egim (list 0.0))))
        (foreach v ylist (setq y-egim (append y-egim (list 0.0))))
      )
    )
  )
  
  ;; Bos listeleri kontrol et
  (if (and (null xlist) (null ylist))
    (progn
      (alert "Axis data bolumunde gecerli eksen verisi bulunamadi!")
      (exit)
    )
  )
  
  ;; Story satirlarindan kat listesi: (floor_num name short elev) - floor_num>0
  ;; Format: data = "yukseklik,kat_no,..." - 1.sutun kota m, 2.sutun kat no
  ;; Iki format: [name,short,data] veya [short,data] - data satirindan onceki 2 satir name ve short
  (setq floor-list '())
  (if (and story-lines (>= (length story-lines) 3))
    (progn
      (setq i 0)
      (while (< i (length story-lines))
        (setq story-parts (split-string (if (nth i story-lines) (nth i story-lines) "") ","))
        (if (and (>= (length story-parts) 2)
                 (setq floor-num (atoi (vl-string-trim " \t" (nth 1 story-parts))))
                 (> floor-num 0)
                 (>= i 2))
          (progn
            (setq floor-name (vl-string-trim " \t" (nth (- i 2) story-lines)))
            (setq floor-short (vl-string-trim " \t" (nth (- i 1) story-lines)))
            (setq floor-elev (if (> (length story-parts) 0)
                              (atof (vl-string-trim " \t" (nth 0 story-parts)))
                              0.0))
            (setq floor-list (append floor-list (list (list floor-num floor-name floor-short floor-elev))))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (if (null floor-list)
    (setq floor-list (list (list 1 "KAT 1" "1" 0.0)))
  )
  
  ;; Cizim sinirlari (cm) - Y: - degerler yukari, + degerler asagi (y = -deger)
  (setq margin 50)
  (setq xmin (if xlist (- (apply 'min xlist) margin) 0))
  (setq xmax (if xlist (+ (apply 'max xlist) margin) 1000))
  (setq ymin (if ylist (- (- (apply 'max ylist)) margin) -1000))
  (setq ymax (if ylist (+ (- (apply 'min ylist)) margin) 1000))
  (setq floor-width (+ (- xmax xmin) 80))
  (setq floor-gap 1000)
  
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
  
  ;; Kolonlardan gecen akslari belirle (x-axis-has-col, y-axis-has-col)
  (setq x-axis-has-col nil i 0)
  (while (< i (length xlist))
    (setq has-col nil)
    (foreach col col-list
      (if (= (- (caddr col) 1001) i) (setq has-col T)))
    (setq x-axis-has-col (append x-axis-has-col (list has-col)) i (1+ i)))
  (setq y-axis-has-col nil i 0)
  (while (< i (length ylist))
    (setq has-col nil)
    (foreach col col-list
      (if (= (- (nth 3 col) 2001) i) (setq has-col T)))
    (setq y-axis-has-col (append y-axis-has-col (list has-col)) i (1+ i)))
  
  ;; Katmanlari olustur
  (setq layer-axes "ST4-AKS-CIZGILER")
  (setq layer-axes-col "ST4-AKS-KOLONLU")   ; Kolonlardan gecen akslar - kirmizi
  (setq layer-axes-nocol "ST4-AKS-KOLONSUZ") ; Kolonlardan gecmeyen akslar - koyu gri
  (setq layer-text "ST4-AKS-ETIKETLER")
  (setq layer-cols "ST4-KOLONLAR")
  (setq layer-colnum "ST4-KOLON-NUMARALARI")
  (create-layer layer-axes 1)   ; Kirmizi (eski, artik kullanilmiyor)
  (create-layer layer-axes-col 1)   ; Kirmizi - kolonlardan gecen
  (create-layer layer-axes-nocol 8) ; Koyu gri - kolonlardan gecmeyen
  (create-layer layer-text 3)  ; Yesil
  (create-layer layer-cols 5)   ; Mavi
  (create-layer layer-colnum 2) ; Sari
  
  ;; Mevcut cizimi sakla
  (setvar "CMDECHO" 0)
  (command "_.UNDO" "BEGIN")
  
  ;; Her kat icin akslar ve kolonlar (yan yana)
  (setq floor-idx 0)
  (foreach floor-info floor-list
    (setq floor-num (car floor-info)
          floor-name (cadr floor-info)
          floor-short (caddr floor-info)
          floor-elev (if (>= (length floor-info) 4) (nth 3 floor-info) 0.0)
          offset-x (* floor-idx (+ floor-width floor-gap)))
    
  ;; X eksenlerini ciz (dikey) - egim=0 duz, egim!=0 egik (x = pos + egim*y)
  ;; Kolonlardan gecen: ST4-AKS-KOLONLU (kirmizi), gecmeyen: ST4-AKS-KOLONSUZ (koyu gri)
  (setq i 0)
  (while (< i (length xlist))
    (setq pos (nth i xlist)
          eg (nth i x-egim)
          layer-axes-curr (if (nth i x-axis-has-col) layer-axes-col layer-axes-nocol))
    (if (equal eg 0.0 1e-9)
      (progn
        (setq pt1 (list (+ pos offset-x) ymin 0))
        (setq pt2 (list (+ pos offset-x) ymax 0))
      )
      (progn
        (setq pt1 (list (+ offset-x pos (* eg ymin)) ymin 0))
        (setq pt2 (list (+ offset-x pos (* eg ymax)) ymax 0))
      )
    )
    (entmake (list '(0 . "LINE")
                   (cons 10 pt1)
                   (cons 11 pt2)
                   (cons 8 layer-axes-curr)))
    (setq i (1+ i))
  )
  
  ;; Y eksenlerini ciz (yatay) - egim=0 duz, egim!=0 egik (y = pos + egim*x), cizimde y = -deger
  ;; Kolonlardan gecen: ST4-AKS-KOLONLU (kirmizi), gecmeyen: ST4-AKS-KOLONSUZ (koyu gri)
  (setq i 0)
  (while (< i (length ylist))
    (setq pos (nth i ylist)
          eg (nth i y-egim)
          layer-axes-curr (if (nth i y-axis-has-col) layer-axes-col layer-axes-nocol))
    (if (equal eg 0.0 1e-9)
      (progn
        (setq ycoord (- pos))
        (setq pt1 (list (+ xmin offset-x) ycoord 0))
        (setq pt2 (list (+ xmax offset-x) ycoord 0))
      )
      (progn
        (setq pt1 (list (+ xmin offset-x) (- (+ pos (* eg xmin))) 0))
        (setq pt2 (list (+ xmax offset-x) (- (+ pos (* eg xmax))) 0))
      )
    )
    (entmake (list '(0 . "LINE")
                   (cons 10 pt1)
                   (cons 11 pt2)
                   (cons 8 layer-axes-curr)))
    (setq i (1+ i))
  )
  
  ;; Kolonlar: Column axis kesisim noktalarinda (bu kat icin section_id = floor_num*100+col_num) (1=dikdortgen, 2=yuvarlak, 3=poligon)
  ;; Tip 1 ve 2: Sadece Columns Data'da tanimli kolonlari ciz (yoksa atla)
  (setvar "CLAYER" layer-cols)
  (setq i 0)
  (foreach col col-list
    (setq col-num (car col) col-type (cadr col) ax-id (caddr col) ay-id (nth 3 col)
          off1 (nth 4 col) off2 (nth 5 col) col-ang (if (>= (length col) 7) (nth 6 col) 0.0)
          ix (- ax-id 1001) iy (- ay-id 2001)
          sect-id (+ (* floor-num 100) col-num))
    ;; Tip 1 ve 2 icin: Columns Data'da yoksa bu kolonu cizme
    ;; Tip 3 (poligon) icin: Polygon columns'da bu kat/kolon yoksa cizme
    (if (and (<= col-type 2) col-dim-list (null (assoc sect-id col-dim-list)))
      (setq col nil)
    )
    (if (and (= col-type 3) col (null (member sect-id polygon-col-positions)))
      (setq col nil)
    )
    (if (and col (>= ix 0) (< ix (length xlist)) (>= iy 0) (< iy (length ylist)))
      (progn
        (setq cw 40.0 ch 40.0)
        ;; Section ID: floor_num*100 + kolon no (Columns Data ile eslesme, tip 1 ve 2 icin)
        (if (and col-dim-list (<= col-type 2))
          (if (setq dims (cdr (assoc sect-id col-dim-list)))
            (setq cw (car dims) ch (cadr dims))
          )
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
        ;; X yonu kaciklik (4.sutun): -1=aks solda, 1=aks sagda, 0=ortali (mm cinsinden)
        (cond
          ((= off1 -1) (setq loc-x hw))
          ((= off1 1) (setq loc-x (- hw)))
          ((and (numberp off1) (< off1 0)) (setq loc-x (+ (/ off1 10.0) hw)))
          ((and (numberp off1) (> off1 0)) (setq loc-x (- (/ off1 10.0) hw)))
          (T (setq loc-x 0.0))
        )
        ;; Y yonu kaciklik (5.sutun): -1=aks yukarda, 1=aks asagida, 0=ortali (mm cinsinden)
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
        ;; Kat offset uygula
        (setq cx (+ cx offset-x))
        (cond
          ;; Tip 3: Poligon kolon - position (sect-id) ile section lookup, aks kacikligi 0,0
          ((= col-type 3)
           (if (and polygon-col-positions polygon-col-section-ids
                    (setq pos-idx (vl-position sect-id polygon-col-positions))
                    (< pos-idx (length polygon-col-section-ids)))
             (progn
               (setq poly-sect-id (nth pos-idx polygon-col-section-ids))
               (setq poly-pts (cdr (assoc poly-sect-id polygon-sections)))
               ;; Poligon kolon aks kacikligi her zaman 0,0 - kesit her katta ayni
               (setq poly-offset (list 0.0 0.0))
               (if (and poly-pts (> (length poly-pts) 2))
                 (progn
                   (setq poly-pts (poly-local-to-global poly-pts cx cy))
                   (if (and (numberp col-ang) (not (equal col-ang 0.0 1e-9)))
                     (setq poly-pts (poly-rotate-pts poly-pts cx cy col-ang))
                   )
                   (entmake (make-poly-entlist poly-pts layer-cols))
                 )
                 ;; Poligon verisi yoksa dikdortgen ciz
                 (setq col-type 1)
               )
             )
             (setq col-type 1)
           )
          )
          ;; Tip 2: Yuvarlak kolon
          ((= col-type 2)
           (setq hw (max hw hh))
           (entmake (list '(0 . "CIRCLE")
                          (cons 8 layer-cols)
                          (cons 10 (list cx cy 0))
                          (cons 40 hw)))
          )
        )
        ;; Tip 1: Dikdortgen (veya poligon yoksa fallback)
        (if (= col-type 1)
          (progn
            (setq p1 (list (- cx hw) (- cy hh) 0))
            (setq p2 (list (+ cx hw) (- cy hh) 0))
            (setq p3 (list (+ cx hw) (+ cy hh) 0))
            (setq p4 (list (- cx hw) (+ cy hh) 0))
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
        ;; Kolon numarasini merkeze yaz
        (setvar "CLAYER" layer-colnum)
        (entmake (list '(0 . "TEXT")
                       (cons 10 (list cx cy 0))
                       (cons 40 8)
                       (cons 1 (itoa col-num))
                       (cons 7 "Standard")
                       (cons 8 layer-colnum)
                       (cons 72 1)
                       (cons 11 (list cx cy 0))
                       (cons 73 2)))
        (setvar "CLAYER" layer-cols)
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
      (setq textpt (list (+ pos offset-x) (+ ymax 20) 0))
      (setq textpt (list (+ offset-x pos (* eg ymax)) (+ ymax 20) 0))
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
      (setq textpt (list (+ xmax 20 offset-x) (- pos) 0))
      (setq textpt (list (+ xmax 20 offset-x) (- (+ pos (* eg xmax))) 0))
    )
    (entmake (list '(0 . "TEXT")
                   (cons 10 textpt) (cons 40 15) (cons 1 (num-to-alpha (1+ i)))
                   (cons 7 "Standard") (cons 8 layer-text) (cons 72 0) (cons 11 textpt) (cons 73 2)))
    (setq i (1+ i))
  )
  
  ;; Koordinat degerlerini (cm) yaz
  (setq i 0)
  (while (< i (length xlist))
    (setq pos (nth i xlist) eg (nth i x-egim))
    (if (equal eg 0.0 1e-9)
      (setq textpt (list (+ pos offset-x) (- ymin 25) 0))
      (setq textpt (list (+ offset-x pos (* eg ymin)) (- ymin 25) 0))
    )
    (entmake (list '(0 . "TEXT")
                   (cons 10 textpt) (cons 40 8) (cons 1 (rtos pos 2 0))
                   (cons 7 "Standard") (cons 8 layer-text) (cons 72 1) (cons 11 textpt) (cons 73 2)))
    (setq i (1+ i))
  )
  
  (setq i 0)
  (while (< i (length ylist))
    (setq pos (nth i ylist))
    (setq textpt (list (+ (- xmin 40) offset-x) (- pos) 0))
    (entmake (list '(0 . "TEXT")
                   (cons 10 textpt) (cons 40 8) (cons 1 (rtos pos 2 0))
                   (cons 7 "Standard") (cons 8 layer-text) (cons 72 1) (cons 11 textpt) (cons 73 2)))
    (setq i (1+ i))
  )
  
  ;; Kat adi ve kotu ustte
  (entmake (list '(0 . "TEXT")
                 (cons 10 (list (+ offset-x (/ (+ xmin xmax) 2.0)) (+ ymax 45) 0))
                 (cons 40 12)
                 (cons 1 (strcat floor-name " - " floor-short " (" (rtos floor-elev 2 0) "m)"))
                 (cons 7 "Standard") (cons 8 layer-text) (cons 72 1)
                 (cons 11 (list (+ offset-x (/ (+ xmin xmax) 2.0)) (+ ymax 45) 0)) (cons 73 2)))
  
  (setq floor-idx (1+ floor-idx))
  ) ; foreach floor-info
  
  (command "_.UNDO" "END")
  (setvar "CMDECHO" 1)
  
  ;; Zoom extents
  (command "_.ZOOM" "_E")
  
  (princ (strcat "\n" (itoa (length floor-list)) " kat, " (itoa (length xlist)) " X ekseni, "
                 (itoa (length ylist)) " Y ekseni, " (itoa (length col-list)) " kolon/kat cizildi. (cm)"))
  (princ)
)

;;; Satirdan verilen araliktaki (min-idx, max-idx) en buyuk tam sayiyi bulur
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

;;; X akslari soldan saga artan, Y akslari yukaridan asagiya artan
;;; Ilk azalma noktasinda X blogu biter, Y blogu baslar
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
    ;; Hic azalma yoksa tumu X
    (list all-values '())
  )
)

;;; Axis data satirini parse eder
;;; Format: "egim,deger,0,0,0" - 1.sutun egim (tanjant), 2.sutun orijine uzaklik (koordinat)
;;; ST4'te degerler metre cinsinden - *100 ile cm'ye ceviriyoruz
(defun parse-axis-line (line / parts egim val)
  (setq parts (split-string line ","))
  (if (>= (length parts) 2)
    (progn
      (setq egim (atof (vl-string-trim " \t" (nth 0 parts))))
      (setq val (atof (nth 1 parts)))
      ;; Deger 100'den kucukse metre kabul edip cm'ye cevir
      (if (< (abs val) 100)
        (setq val (* val 100))
      )
      (list egim val)
    )
    nil
  )
)

;;; Sayiyi harfe cevirir: 1->A, 2->B, ..., 26->Z, 27->AA, ...
(defun num-to-alpha (n / result)
  (setq result "")
  (while (> n 0)
    (setq n (1- n))
    (setq result (strcat (chr (+ 65 (rem n 26))) result))
    (setq n (fix (/ n 26)))
  )
  result
)

;;; Katman olusturur (yoksa)
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
