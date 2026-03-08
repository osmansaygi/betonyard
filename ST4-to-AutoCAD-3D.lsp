;;; ============================================================
;;; ST4-to-AutoCAD-3D.lsp
;;; STA4CAD ST4 dosyasindan 3 boyutlu modeli AutoCAD'e aktarir.
;;;
;;; YUKLEME: APPLOAD -> dosyayi sec -> Yukle
;;; KOMUT: ST4IMPORT veya ST4
;;; Aks cizimi C# Sta4CadImporterAutoCadCommand.cs mantigina gore.
;;; - ny/nx: Column axis verisinden max 10xx ve 20xx ID'leri
;;; - 2001 serisi: skew (1. sutun) + base (2. sutun)
;;; - Nodes: CAD X = y_coord, CAD Y = -x_coord (skew uygulanmis)
;;; - Akslar: node'lari birlestir, 2m uzat, Y=1,2,3... X=A,B,C...
;;; ============================================================

;;; Ilk once stub - dosya yuklenirken hata olursa bu komut calisir
(defun C:ST4IMPORT-STUB () (alert "LISP yuklendi. ST4IMPORT yazin."))

(defun C:ST4IMPORT ( / fn f line section axis-data axis-lines
                      axis-1001-list axis-2001-base axis-2001-skew
                      ny nx max-y-id max-x-id
                      col-list beam-list floor-list
                      story-list z-levels
                      nodes node-pt
                      pt1 pt2 n id1 id2 n1 n2 off1 off2
                      x y z idx i j xy
                      floor-id ax1 ax2 ay1 ay2 z-floor
                      b-id b-ax b-a1 b-a2 bp1 bp2
                      p1 p2 p3 p4
                      y-ids x-ids ordered-y ordered-x
                      axis-layer label-layer axis-z ext label-ht
                      family p1-2d p2-2d dx dy len ux uy s e
                      axis-label base-x skew sta-x x-cad y-cad iy ix y-val)
  (setq fn (getfiled "ST4 Dosyasi Secin" "" "st4" 0))
  (if (not fn)
    (progn (princ "\nIslem iptal edildi.") (princ))
    (progn
      (setq f (open fn "r"))
      (if (not f)
        (princ "\nDosya acilamadi!")
        (progn
          (princ "\nST4 dosyasi okunuyor...")
          (setq section nil
                axis-data nil
                axis-lines nil
                max-y-id 1000
                max-x-id 2000
                story-list nil
                z-levels (list 0.0)
                col-list nil
                floor-list nil
                beam-list nil)
          ;; --- Dosyayi satir satir oku ---
          (while (setq line (read-line f))
            (cond
              ((wcmatch (strcase line) "*AXIS DATA*")
                (setq section "axis") (setq axis-lines nil))
              ((wcmatch (strcase line) "*CIRCLE AXIS*")
                (setq section nil))
              ((wcmatch (strcase line) "*COLUMN AXIS*")
                (setq section "colaxis"))
              ((wcmatch (strcase line) "*STORY*")
                (setq section "story"))
              ((wcmatch (strcase line) "*FLOORS DATA*")
                (setq section "floors"))
              ((wcmatch (strcase line) "*FLOORS CONTINUOUS*")
                (setq section nil))
              ((wcmatch (strcase line) "*BEAMS DATA*")
                (setq section "beams"))
              ((wcmatch (strcase line) "*SLABS DATA*")
                (setq section nil))
              ;; Axis data: "0,coord,0,0,0" veya "skew,coord,0,0,0"
              ((and (= section "axis") (> (strlen line) 2))
                (if (and (not (wcmatch (strcase line) "/ *"))
                         (setq axis-data (parse-csv-line line))
                         (>= (length axis-data) 2))
                  (setq axis-lines (append axis-lines (list axis-data)))))
              ;; Column axis: "1,1001,2001,..." -> max 10xx ve 20xx bul
              ((and (= section "colaxis") (> (strlen line) 2))
                (if (and (not (wcmatch (strcase line) "/ *"))
                         (setq axis-data (parse-csv-line line))
                         (>= (length axis-data) 3))
                  (progn
                    (setq id1 (atoi (nth 1 axis-data)) id2 (atoi (nth 2 axis-data)))
                    (if (and (>= id1 1001) (<= id1 1099))
                      (setq max-y-id (max max-y-id id1)))
                    (if (and (>= id2 2001) (<= id2 2099))
                      (setq max-x-id (max max-x-id id2)))
                    (setq id1 (atoi (nth 1 axis-data)) id2 (atoi (nth 2 axis-data))
                          off1 (if (>= (length axis-data) 5) (atoi (nth 3 axis-data)) -1)
                          off2 (if (>= (length axis-data) 5) (atoi (nth 4 axis-data)) -1))
                    (if (or (and (>= id1 1001) (<= id1 1099) (>= id2 2001) (<= id2 2099))
                            (and (>= id1 1) (<= id1 99) (>= id2 1) (<= id2 99)))
                      (setq col-list (append col-list (list (list id1 id2 off1 off2))))))))
              ;; Story
              ((and (= section "story") (> (strlen line) 2))
                (if (and (setq axis-data (parse-csv-line line))
                         (>= (length axis-data) 1)
                         (numberp (read (nth 0 axis-data))))
                  (setq z-levels (append z-levels (list (atof (nth 0 axis-data)))))))
              ;; Floors
              ((and (= section "floors") (> (strlen line) 2))
                (if (and (not (wcmatch (strcase line) "/ *"))
                         (setq axis-data (parse-csv-line line))
                         (>= (length axis-data) 12))
                  (setq floor-id (atoi (nth 0 axis-data))
                        ax1 (atoi (nth 8 axis-data)) ax2 (atoi (nth 9 axis-data))
                        ay1 (atoi (nth 10 axis-data)) ay2 (atoi (nth 11 axis-data)))
                  (if (and (>= ax1 1) (>= ay1 1))
                    (setq floor-list (append floor-list (list (list floor-id ax1 ax2 ay1 ay2)))))))
              ;; Beams
              ((and (= section "beams") (> (strlen line) 2))
                (if (and (not (wcmatch (strcase line) "/ *"))
                         (setq axis-data (parse-csv-line line))
                         (>= (length axis-data) 7))
                  (setq b-id (atoi (nth 0 axis-data)) b-ax (atoi (nth 4 axis-data))
                        b-a1 (atoi (nth 5 axis-data)) b-a2 (atoi (nth 6 axis-data)))
                  (if (and (>= b-id 1) (>= b-ax 1) (>= b-a1 1) (>= b-a2 1))
                    (setq beam-list (append beam-list (list (list b-id b-ax b-a1 b-a2)))))))
              (T nil)
            )
          )
          (close f)

          ;; max-y-id, max-x-id - column, beams, floors'tan
          (foreach c col-list
            (if (and (>= (car c) 1001) (<= (car c) 1099))
              (setq max-y-id (max max-y-id (car c))))
            (if (and (>= (cadr c) 2001) (<= (cadr c) 2099))
              (setq max-x-id (max max-x-id (cadr c))))
          )
          (foreach b beam-list
            (foreach id (list (cadr b) (caddr b) (nth 3 b))
              (if (and (>= id 1001) (<= id 1099)) (setq max-y-id (max max-y-id id)))
              (if (and (>= id 2001) (<= id 2099)) (setq max-x-id (max max-x-id id)))
            )
          )
          (foreach fl floor-list
            (foreach id (list (cadr fl) (caddr fl) (cadddr fl) (nth 4 fl))
              (if (and (>= id 1001) (<= id 1099)) (setq max-y-id (max max-y-id id)))
              (if (and (>= id 2001) (<= id 2099)) (setq max-x-id (max max-x-id id)))
            )
          )
          (setq ny (max 0 (- max-y-id 1000))
                nx (max 0 (- max-x-id 2000)))

          ;; C# gibi: ilk ny satir -> yCoords (1001+i), sonraki nx satir -> xSkews, xBase (2001+i)
          (setq axis-1001-list nil axis-2001-base nil axis-2001-skew nil)
          (setq n (length axis-lines))
          (setq i 0)
          (while (and (< i ny) (< i n))
            (setq axis-1001-list (append axis-1001-list
              (list (atof (nth 1 (nth i axis-lines))))))
            (setq i (1+ i)))
          (setq i 0)
          (while (and (< i nx) (< (+ ny i) n))
            (setq axis-2001-skew (append axis-2001-skew
              (list (atof (nth 0 (nth (+ ny i) axis-lines))))))
            (setq axis-2001-base (append axis-2001-base
              (list (atof (nth 1 (nth (+ ny i) axis-lines))))))
            (setq i (1+ i)))

          ;; Bos listeler icin varsayilan
          (if (not axis-1001-list) (setq axis-1001-list (list 0.0)))
          (if (not axis-2001-base) (setq axis-2001-base (list 0.0)))
          (if (not axis-2001-skew) (setq axis-2001-skew (make-list (length axis-2001-base) 0.0)))


          ;; Nodes: (xId,yId) -> CAD Point2d (x_cad, y_cad)
          ;; C#: cadPt = (y, -staX), staX = baseX + skew*staY
          (setq nodes nil)
          (setq y-ids (range 1001 (+ 1001 ny)))
          (setq x-ids (range 2001 (+ 2001 nx)))
          (foreach y-id y-ids
            (setq iy (- y-id 1001))
            (if (and (>= iy 0) (< iy (length axis-1001-list)))
              (setq y-val (nth iy axis-1001-list))
              (setq y-val 0.0))
            (foreach x-id x-ids
              (setq ix (- x-id 2001))
              (if (and (>= ix 0) (< ix (length axis-2001-base)))
                (progn
                  (setq base-x (nth ix axis-2001-base))
                  (setq skew (if (and axis-2001-skew (< ix (length axis-2001-skew)))
                         (nth ix axis-2001-skew) 0.0))
                  (setq sta-x (if (> (abs skew) 1e-12) (+ base-x (* skew y-val)) base-x))
                  (setq x-cad y-val)
                  (setq y-cad (- sta-x))
                  (setq node-pt (list x-cad y-cad))
                  (setq nodes (append nodes (list (list x-id y-id node-pt)))))
                )
              )
            )

          (setq z-levels (vl-sort (unique-list z-levels) '<))

          ;; Kolon yoksa izgara
          (if (and (not col-list) axis-1001-list axis-2001-base
                   (> (length axis-1001-list) 0) (> (length axis-2001-base) 0))
            (progn
              (setq id1 1)
              (while (<= id1 (length axis-1001-list))
                (setq id2 1)
                (while (<= id2 (length axis-2001-base))
                  (setq col-list (append col-list (list (list (+ 1000 id1) (+ 2000 id2) -1 -1))))
                  (setq id2 (1+ id2)))
                (setq id1 (1+ id1)))
              (princ "\n(Kolon verisi yok; eksen kesisimlerinden izgara uretildi.)") ))

          (princ (strcat "\nEksen 1001 sayisi: " (itoa (length axis-1001-list))))
          (princ (strcat "\nEksen 2001 sayisi: " (itoa (length axis-2001-base))))
          (princ (strcat "\nNode sayisi: " (itoa (length nodes))))

          ;; Katmanlar (C# ile uyumlu)
          (if (not (tblsearch "LAYER" "KOLONLAR"))
            (command "._LAYER" "_M" "KOLONLAR" "_C" "1" "" ""))
          (if (not (tblsearch "LAYER" "KIRISLER"))
            (command "._LAYER" "_M" "KIRISLER" "_C" "5" "" ""))
          (if (not (tblsearch "LAYER" "AKSLAR"))
            (command "._LAYER" "_M" "AKSLAR" "_C" "2" "" ""))
          (if (not (tblsearch "LAYER" "ETIKETLER"))
            (command "._LAYER" "_M" "ETIKETLER" "_C" "4" "" ""))

          ;; --- AKS CIZIMI (C# DrawAxes mantigi) ---
          (setq axis-z (+ (if (and z-levels (> (length z-levels) 0)) (car (reverse z-levels)) 0.0) 0.02))
          (setq ext 2.0 label-ht 0.20)

          (if (and (> (length y-ids) 0) (> (length x-ids) 0) (> (length nodes) 0))
            (progn
              (setq ordered-y (vl-sort y-ids '<))
              (setq ordered-x (vl-sort x-ids '<))
              ;; Y-id aileleri (sayisal 1,2,3...)
              (setq idx 0)
              (foreach y-id ordered-y
                (setq family nil)
                (foreach x-id ordered-x
                  (setq node-pt (get-node nodes x-id y-id))
                  (if node-pt (setq family (append family (list (list x-id node-pt))))))
                (if (and family (>= (length family) 2))
                  (progn
                    (setq family (vl-sort family (function (lambda (a b) (< (car a) (car b))))))
                    (setq p1-2d (cadr (car family)))
                    (setq p2-2d (cadr (car (reverse family))))
                    (setq dx (- (car p2-2d) (car p1-2d)))
                    (setq dy (- (cadr p2-2d) (cadr p1-2d)))
                    (setq len (sqrt (+ (* dx dx) (* dy dy))))
                    (if (> len 1e-12)
                      (progn
                        (setq ux (/ dx len) uy (/ dy len))
                        (setq s (list (- (car p1-2d) (* ux ext)) (- (cadr p1-2d) (* uy ext)) axis-z))
                        (setq e (list (+ (car p2-2d) (* ux ext)) (+ (cadr p2-2d) (* uy ext)) axis-z))
                        (draw-3d-line s e "AKSLAR")
                        (setq axis-label (itoa (1+ idx)))
                        (draw-3d-text axis-label s label-ht "ETIKETLER")
                        (draw-3d-text axis-label e label-ht "ETIKETLER")
                        (setq idx (1+ idx))))))
              ;; X-id aileleri (harfli A,B,C...)
              (setq idx 0)
              (foreach x-id ordered-x
                (setq family nil)
                (foreach y-id ordered-y
                  (setq node-pt (get-node nodes x-id y-id))
                  (if node-pt (setq family (append family (list (list y-id node-pt))))))
                (if (and family (>= (length family) 2))
                  (progn
                    (setq family (vl-sort family (function (lambda (a b) (< (car a) (car b))))))
                    (setq p1-2d (cadr (car family)))
                    (setq p2-2d (cadr (car (reverse family))))
                    (setq dx (- (car p2-2d) (car p1-2d)))
                    (setq dy (- (cadr p2-2d) (cadr p1-2d)))
                    (setq len (sqrt (+ (* dx dx) (* dy dy))))
                    (if (> len 1e-12)
                      (progn
                        (setq ux (/ dx len) uy (/ dy len))
                        (setq s (list (- (car p1-2d) (* ux ext)) (- (cadr p1-2d) (* uy ext)) axis-z))
                        (setq e (list (+ (car p2-2d) (* ux ext)) (+ (cadr p2-2d) (* uy ext)) axis-z))
                        (draw-3d-line s e "AKSLAR")
                        (setq axis-label (axis-letter-from-index idx))
                        (draw-3d-text axis-label s label-ht "ETIKETLER")
                        (draw-3d-text axis-label e label-ht "ETIKETLER")
                        (setq idx (1+ idx))))))
              (princ "\nAkslar cizildi (C# mantigi: node baglantisi, 2m uzatma, 1,2,3 / A,B,C etiketleri).")
            )
          )

          ;; get-xy: (id1,id2) -> (x,y) CAD koordinatlari
          (defun get-xy (id1 id2 / ix iy base-x skew y-val)
            (setq ix (if (>= id2 2001) (- id2 2001) (- id2 1))
                  iy (if (>= id1 1001) (- id1 1001) (- id1 1)))
            (list
              (if (and axis-1001-list (>= iy 0) (< iy (length axis-1001-list)))
                (nth iy axis-1001-list) 0.0)
              (if (and axis-2001-base (>= ix 0) (< ix (length axis-2001-base)))
                (progn
                  (setq base-x (nth ix axis-2001-base))
                  (setq skew (if (and axis-2001-skew (< ix (length axis-2001-skew)))
                         (nth ix axis-2001-skew) 0.0))
                  (setq y-val (if (and axis-1001-list (>= iy 0) (< iy (length axis-1001-list)))
                         (nth iy axis-1001-list) 0.0))
                  (- (if (> (abs skew) 1e-12) (+ base-x (* skew y-val)) base-x))
                  )
                0.0)
            )
          )
          (defun get-xy-column (id1 id2 / xy x y off1 off2 c)
            (setq xy (get-xy id1 id2) x (car xy) y (cadr xy))
            (setq c (car (vl-remove-if-not (function (lambda (col) (and (= (car col) id1) (= (cadr col) id2)))) col-list)))
            (if c
              (progn
                (setq off1 (if (>= (length c) 3) (nth 2 c) -1)
                      off2 (if (>= (length c) 4) (nth 3 c) -1))
                (if (and (numberp off1) (>= off1 0)) (setq x (- x (/ off1 1000.0))))
                (if (and (numberp off2) (>= off2 0)) (setq y (- y (/ off2 1000.0))))
              )
            )
            (list x y)
          )
          (defun floor-to-z (fid / k)
            (setq k (cond ((< fid 100) 0) ((< fid 200) 1) ((< fid 300) 2) ((< fid 400) 3) (T 4)))
            (if (and z-levels (>= (1+ k) 0) (< (1+ k) (length z-levels)))
              (nth (1+ k) z-levels)
              (if z-levels (last z-levels) 0.0))
          )

          ;; 3D Cizim
          (setvar "CMDECHO" 0)
          (foreach col col-list
            (setq id1 (car col) id2 (cadr col)
                  off1 (if (>= (length col) 3) (nth 2 col) -1)
                  off2 (if (>= (length col) 4) (nth 3 col) -1))
            (setq xy (get-xy id1 id2) x (car xy) y (cadr xy))
            (if (and (numberp off1) (>= off1 0)) (setq x (- x (/ off1 1000.0))))
            (if (and (numberp off2) (>= off2 0)) (setq y (- y (/ off2 1000.0))))
            (setq i 0)
            (while (and (< (1+ i) (length z-levels)))
              (setq pt1 (list x y (nth i z-levels))
                    pt2 (list x y (nth (1+ i) z-levels)))
              (draw-3d-line pt1 pt2 "KOLONLAR")
              (setq i (1+ i))
            )
          )
          (foreach fl floor-list
            (setq z-floor (floor-to-z (car fl))
                  ax1 (cadr fl) ax2 (caddr fl) ay1 (cadddr fl) ay2 (nth 4 fl))
            (setq p1 (append (get-xy ax1 ay1) (list z-floor)))
            (setq p2 (append (get-xy ax1 ay2) (list z-floor)))
            (setq p3 (append (get-xy ax2 ay2) (list z-floor)))
            (setq p4 (append (get-xy ax2 ay1) (list z-floor)))
            (draw-3d-line p1 p2 "KIRISLER")
            (draw-3d-line p2 p3 "KIRISLER")
            (draw-3d-line p3 p4 "KIRISLER")
            (draw-3d-line p4 p1 "KIRISLER")
          )
          (foreach b beam-list
            (setq b-id (car b) b-ax (cadr b) b-a1 (caddr b) b-a2 (nth 3 b)
                  z-floor (floor-to-z b-id))
            (if (>= b-ax 2001)
              (setq bp1 (append (get-xy-column b-a1 b-ax) (list z-floor))
                    bp2 (append (get-xy-column b-a2 b-ax) (list z-floor)))
              (setq bp1 (append (get-xy-column b-ax b-a1) (list z-floor))
                    bp2 (append (get-xy-column b-ax b-a2) (list z-floor)))
            )
            (draw-3d-line bp1 bp2 "KIRISLER")
          )

          (setvar "CMDECHO" 1)
          (command "._ZOOM" "_E")
          (princ (strcat "\nKolon: " (itoa (length col-list)) " Kiris: " (itoa (length beam-list))))
          (princ "\nST4 modeli aktarildi. Katmanlar: KOLONLAR, KIRISLER, AKSLAR, ETIKETLER")
        )
      )
    )
  )
  (princ)
)

;;; (range start end) -> (start start+1 ... end-1)
(defun range (a b / r)
  (setq r '())
  (while (< a b) (setq r (append r (list a)) a (1+ a)))
  r
)

;;; nodes listesinden (x-id, y-id) icin 2d nokta dondur
(defun get-node (nodes x-id y-id / r)
  (setq r (car (vl-remove-if-not
    (function (lambda (n) (and (= (car n) x-id) (= (cadr n) y-id))))
    nodes)))
  (if r (caddr r) nil)
)

;;; A,B,C... AA,AB... (C# AxisLetterFromIndex)
(defun axis-letter-from-index (idx / letters val)
  (if (< idx 0) "?"
    (progn
      (setq letters "" val idx)
      (while (>= val 0)
        (setq letters (strcat (chr (+ 65 (rem val 26))) letters)
              val (- (fix (/ val 26)) 1))
      letters
    )
  )
)

(defun parse-csv-line (str / lst pos s)
  (setq str (vl-string-trim " \t" str) lst '())
  (while (> (strlen str) 0)
    (setq pos (vl-string-search "," str))
    (if pos
      (setq s (vl-string-trim " \t" (substr str 1 pos))
            str (vl-string-trim " \t" (substr str (+ pos 2))))
      (setq s (vl-string-trim " \t" str) str ""))
    (setq lst (append lst (list s)))
  )
  lst
)

(defun unique-list (lst / r)
  (setq r '())
  (foreach x lst (if (not (member x r)) (setq r (append r (list x)))))
  r
)

(defun draw-3d-line (pt1 pt2 lay / ent)
  (entmakex (list (cons 0 "LINE") (cons 8 lay) (cons 10 pt1) (cons 11 pt2)))
)

(defun draw-3d-text (str pt ht lay / ent)
  (entmakex (list
    (cons 0 "TEXT")
    (cons 8 lay)
    (cons 10 pt)
    (cons 40 ht)
    (cons 1 str)
    (cons 7 "Standard")
  ))
)

;;; Alternatif kisa komut
(defun C:ST4 () (C:ST4IMPORT))

(princ "\nST4 Import yuklendi. Komutlar: ST4IMPORT veya ST4")
(princ)
