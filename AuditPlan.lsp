;; =========================================================================
;; AUDITPLAN  -  audit PROFUND al entitatilor (extensie la AuditBlocuri)
;;
;; Fata de AuditBlocuri, scoate si GEOMETRIA completa, de care e nevoie ca
;; sa se poata reconstrui/genera automat un plan de fundatii:
;;   - LINE      : ambele capete
;;   - LWPOLYLINE: toate varfurile + bulge + inchisa/deschisa + latimi + arie
;;   - POLYLINE  : idem (polilinii vechi, cu varfuri VERTEX)
;;   - ARC/CIRCLE: centru, raza, unghiuri
;;   - ELLIPSE   : centru, axa mare, raport, parametri
;;   - TEXT/MTEXT: text, inaltime, rotatie, stil, aliniere, punct de aliniere
;;   - DIMENSION : TOATE punctele de definitie (10,11,12,13,14,15,16),
;;                 tipul cotei, unghiul, stilul + valorile stilului
;;                 (DIMSCALE/DIMLFAC/DIMTXT...), masuratoarea bruta,
;;                 distanta reala intre punctele de definitie si XDATA
;;                 cu suprascrierile de stil (ACAD_DSTYLE)
;;   - HATCH     : pattern, scara, unghi, solid, nr. de contururi, arie
;;   - INSERT    : ca in AuditBlocuri + atribute
;;   - WIPEOUT   : punctele de contur brute
;;   - orice alt tip: toate perechile DXF (RAW)
;;
;; In plus:
;;   - numele REAL al grupului din dictionarul ACAD_GROUP (ex. *A12), nu
;;     adresa volatila a entitatii, plus HANDLE-ul fiecarei entitati
;;   - antet cu variabilele de sistem, tabelele de layere, stiluri de cotare
;;     si stiluri de text
;;   - recapitulare pe TIP @ LAYER si gabaritul selectiei
;;   - DETECTOR DE SUSPECTI: segmente care nu sunt perfect ortogonale,
;;     lungimi ne-rotunde, cote ne-rotunde si noduri neunite (capete
;;     apropiate dar care nu coincid). La final se poate parcurge lista cu
;;     zoom pe fiecare, ca sa se vada in desen despre ce e vorba.
;;
;; Comenzi:  AUDITPLAN   - face auditul si scrie fisierul
;;           AUDITSUS    - reia inspectia vizuala a suspectilor
;; =========================================================================

(vl-load-com)

;; --------------------------------------------------------------------- ;;
;; Helperi                                                               ;;
;; --------------------------------------------------------------------- ;;

(defun ap:out (f s)
  (princ (strcat "\n" s))
  (if f (write-line s f))
  (princ)
)

(defun ap:str (v)
  (cond ((null v) "")
        ((= (type v) 'STR) v)
        (T (vl-princ-to-string v))
  )
)

;; numar -> text cu 4 zecimale
(defun ap:r (v)
  (if (numberp v) (rtos v 2 4) "")
)

;; punct -> "x,y" (sau "x,y,z" daca Z nu e 0)
(defun ap:pt (p)
  (if p
    (strcat (ap:r (car p)) "," (ap:r (cadr p))
            (if (and (caddr p) (> (abs (caddr p)) 1e-9))
              (strcat "," (ap:r (caddr p)))
              ""))
    "-"
  )
)

;; radiani -> grade, ca text
(defun ap:ang (v)
  (if (numberp v) (rtos (/ (* 180.0 v) pi) 2 4) "")
)

(defun ap:dxf (c e) (cdr (assoc c e)))

;; toate valorile unui cod DXF, in ordinea din entitate
(defun ap:allcodes (c e)
  (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= c (car x))) e))
)

;; lista de puncte -> "x,y; x,y; ..."
(defun ap:ptlist (l)
  (if l
    (substr (apply 'strcat (mapcar '(lambda (p) (strcat "; " (ap:pt p))) l)) 3)
    ""
  )
)

;; proprietate VLA, prinsa in catch
(defun ap:get (obj prop / v)
  (setq v (vl-catch-all-apply 'vlax-get (list obj prop)))
  (if (vl-catch-all-error-p v) nil v)
)

;; --------------------------------------------------------------------- ;;
;; Dictionarul de grupuri: lista de (ename nume_grup handle_grup)         ;;
;; --------------------------------------------------------------------- ;;

(defun ap:grouplist ( / d nm gd lst hnd)
  (setq lst '())
  (if (setq d (dictsearch (namedobjdict) "ACAD_GROUP"))
    (foreach itm d
      (cond
        ((= 3 (car itm)) (setq nm (cdr itm)))
        ((= 350 (car itm))
         (setq gd  (entget (cdr itm))
               hnd (cdr (assoc 5 gd)))
         (foreach x gd
           (if (= 340 (car x))
             (setq lst (cons (list (cdr x) nm hnd) lst))
           )
         )
        )
      )
    )
  )
  lst
)

;; --------------------------------------------------------------------- ;;
;; Tabele: stil de cotare, stil de text                                   ;;
;; --------------------------------------------------------------------- ;;

(defun ap:dimstyleinfo (nm / tb sty stynm)
  (if (setq tb (tblsearch "dimstyle" nm))
    (progn
      (setq stynm "")
      (if (setq sty (cdr (assoc 340 tb)))
        (setq stynm (ap:str (cdr (assoc 2 (entget sty)))))
      )
      (strcat "DIMSCALE=" (ap:r (cdr (assoc 40 tb)))
              " DIMLFAC=" (ap:r (cdr (assoc 144 tb)))
              " DIMTXT="  (ap:r (cdr (assoc 140 tb)))
              " DIMASZ="  (ap:r (cdr (assoc 41 tb)))
              " DIMEXE="  (ap:r (cdr (assoc 44 tb)))
              " DIMEXO="  (ap:r (cdr (assoc 42 tb)))
              " DIMGAP="  (ap:r (cdr (assoc 147 tb)))
              " DIMDLI="  (ap:r (cdr (assoc 43 tb)))
              " DIMDEC="  (ap:str (cdr (assoc 271 tb)))
              " DIMTAD="  (ap:str (cdr (assoc 77 tb)))
              " DIMTIH="  (ap:str (cdr (assoc 73 tb)))
              " DIMTOH="  (ap:str (cdr (assoc 74 tb)))
              " DIMTXSTY=" stynm)
    )
    "?"
  )
)

;; XDATA (suprascrieri de stil pe cota, etc.) ca sir compact
(defun ap:xdata (ent / ed xd s)
  (setq s "")
  (setq ed (entget ent '("*")))
  (if (setq xd (assoc -3 ed))
    (foreach app (cdr xd)
      (setq s (strcat s " XDATA[" (car app) "]={"))
      (foreach itm (cdr app)
        (setq s (strcat s (itoa (car itm)) ":" (ap:str (cdr itm)) " "))
      )
      (setq s (strcat s "}"))
    )
  )
  s
)

;; --------------------------------------------------------------------- ;;
;; Varfurile poliliniilor                                                 ;;
;; --------------------------------------------------------------------- ;;

;; LWPOLYLINE -> lista de (punct bulge)
(defun ap:lwverts (e / lst)
  (setq lst '())
  (foreach x e
    (cond
      ((= 10 (car x)) (setq lst (cons (list (cdr x) 0.0) lst)))
      ((and (= 42 (car x)) lst)
       (setq lst (cons (list (car (car lst)) (cdr x)) (cdr lst))))
    )
  )
  (reverse lst)
)

;; POLYLINE veche -> lista de (punct bulge)
(defun ap:plverts (ent / e v lst)
  (setq lst '() v (entnext ent))
  (while (and v (= "VERTEX" (cdr (assoc 0 (setq e (entget v))))))
    (setq lst (cons (list (cdr (assoc 10 e))
                          (cond ((cdr (assoc 42 e))) (0.0)))
                    lst))
    (setq v (entnext v))
  )
  (reverse lst)
)

;; lista (punct bulge) -> text
(defun ap:vstr (l)
  (if l
    (substr
      (apply 'strcat
        (mapcar '(lambda (v)
                   (strcat "; " (ap:pt (car v))
                           (if (> (abs (cadr v)) 1e-9)
                             (strcat "/b=" (ap:r (cadr v)))
                             "")))
                l))
      3)
    ""
  )
)

;; --------------------------------------------------------------------- ;;
;; Detectorul de suspecti                                                 ;;
;; --------------------------------------------------------------------- ;;
;; *ap:sus*   = lista de (ename categorie descriere)
;; *ap:nodes* = toate capetele de segmente, pentru testul de noduri neunite

(setq *ap:sus*   nil
      *ap:nodes* nil
      *ap:grid*  2.5      ;; pasul la care ar trebui sa "cada" lungimile (mm)
      *ap:gtol*  0.01     ;; toleranta fata de pas (mm)
      *ap:atol*  1e-7     ;; toleranta unghiulara pentru ortogonalitate (rad)
      *ap:jtol*  2.0      ;; sub distanta asta doua capete ar trebui sa coincida
)

(defun ap:addsus (ent cat desc)
  (setq *ap:sus* (cons (list ent cat desc) *ap:sus*))
)

;; e valoarea multiplu de pas?
(defun ap:offgrid (v / n)
  (setq n (* *ap:grid* (fix (+ (/ v *ap:grid*) (if (minusp v) -0.5 0.5)))))
  (> (abs (- v n)) *ap:gtol*)
)

;; deviatia unui segment fata de cea mai apropiata directie ortogonala (rad)
(defun ap:orthodev (p1 p2 / dx dy a)
  (setq dx (abs (- (car p2) (car p1)))
        dy (abs (- (cadr p2) (cadr p1))))
  (if (and (< dx 1e-9) (< dy 1e-9))
    nil
    (progn (setq a (atan dy dx)) (min a (- (/ pi 2.0) a)))
  )
)

;; verifica un segment; lay = layerul, idx = indicele varfului (pt. mesaj)
(defun ap:checkseg (ent lay p1 p2 idx / dev len)
  (setq len (distance p1 p2))
  (if (> len 1e-9)
    (progn
      (setq *ap:nodes* (cons (list p1 ent lay) *ap:nodes*))
      (setq *ap:nodes* (cons (list p2 ent lay) *ap:nodes*))
      (setq dev (ap:orthodev p1 p2))
      (if (and dev (> dev *ap:atol*))
        (ap:addsus ent "NEORTOGONAL"
          (strcat "segment " (itoa idx) " pe layer " lay
                  ": abatere " (rtos (/ (* 180.0 dev) pi) 2 6) " grade ("
                  (rtos (* len (sin dev)) 2 4) " mm pe lungimea de "
                  (rtos len 2 4) " mm), de la " (ap:pt p1) " la " (ap:pt p2)))
      )
      (if (ap:offgrid len)
        (ap:addsus ent "LUNGIME NEROTUNDA"
          (strcat "segment " (itoa idx) " pe layer " lay
                  ": lungime " (rtos len 2 4) " mm (nu e multiplu de "
                  (rtos *ap:grid* 2 2) " mm), de la " (ap:pt p1) " la " (ap:pt p2)))
      )
    )
  )
)

;; capete apropiate dar neunite (cauza tipica a cotelor ne-rotunde)
;; se sorteaza dupa X si se compara doar in fereastra de +/- *ap:jtol*
(defun ap:checknodes ( / l a b d rest)
  (if (> (length *ap:nodes*) 20000)
    (progn
      (princ (strcat "\n(Prea multe noduri ("
                     (itoa (length *ap:nodes*))
                     ") - testul de noduri neunite a fost sarit.)"))
      (setq *ap:nodes* nil))
  )
  (setq l (vl-sort *ap:nodes*
                   '(lambda (u v) (< (car (car u)) (car (car v))))))
  (while l
    (setq a    (car l)
          rest (cdr l)
          l    rest)
    (while (and rest
                (< (- (car (car (car rest))) (car (car a))) *ap:jtol*))
      (setq b    (car rest)
            rest (cdr rest))
      (setq d (distance (car a) (car b)))
      (if (and (> d 1e-6) (< d *ap:jtol*) (not (eq (cadr a) (cadr b))))
        (ap:addsus (cadr a) "NOD NEUNIT"
          (strcat "capat " (ap:pt (car a)) " (layer " (caddr a) ") la "
                  (rtos d 2 4) " mm de capatul " (ap:pt (car b))
                  " (layer " (caddr b) ") - probabil ar trebui sa coincida"))
      )
    )
  )
  (princ)
)

;; --------------------------------------------------------------------- ;;
;; Inspectia vizuala a suspectilor                                        ;;
;; --------------------------------------------------------------------- ;;

(defun c:AUDITSUS ( / lst i n itm ent k ss)
  (if (null *ap:sus*)
    (princ "\nNu exista lista de suspecti. Rulati intai AUDITPLAN.")
    (progn
      (setq lst (reverse *ap:sus*)
            n   (length lst)
            i   0)
      (princ (strcat "\n" (itoa n) " elemente suspecte. [Enter]=urmatorul, P=precedentul, X=iesire."))
      (while (and (>= i 0) (< i n))
        (setq itm (nth i lst)
              ent (car itm))
        (if (and ent (entget ent))
          (progn
            (princ (strcat "\n--- " (itoa (1+ i)) "/" (itoa n) " [" (cadr itm) "] "
                           (caddr itm)))
            (command "_.ZOOM" "_Object" ent "")
            (command "_.ZOOM" "0.4x")
            (redraw ent 3)
            (setq k (strcase (getstring "\n[Enter]=urmatorul / P=precedent / X=iesire: ")))
            (redraw ent 4)
            (cond ((= k "X") (setq i n))
                  ((= k "P") (setq i (max 0 (1- i))))
                  (T (setq i (1+ i))))
          )
          (setq i (1+ i))
        )
      )
      ;; la final lasam toti suspectii selectati (grips), ca sa fie usor de gasit
      (setq ss (ssadd))
      (foreach itm lst
        (if (and (car itm) (entget (car itm))) (ssadd (car itm) ss))
      )
      (if (> (sslength ss) 0) (sssetfirst nil ss))
      (princ (strcat "\nGata. " (itoa (sslength ss)) " entitati raman selectate."))
      (princ)
    )
  )
)

;; --------------------------------------------------------------------- ;;
;; Comanda principala                                                     ;;
;; --------------------------------------------------------------------- ;;

(defun c:AUDITPLAN ( / ss n i ent e etype lay hnd grp grpnm grphnd obj
                       fname f dwgname dwgpath defname line geo raw
                       counts key cnt minx miny maxx maxy p verts
                       rawmode ansr dimsty dimmeas d13 d14 dlf drot dper dpar tb sus
                       txt blkname atts a ae)

  (princ "\nSelectati entitatile pentru auditul profund (Enter = tot modelul): ")
  (setq ss (ssget))
  (if (null ss) (setq ss (ssget "_X" '((410 . "Model")))))

  ;; plasa de siguranta: pe un desen intreg auditul dureaza enorm si nu ajuta
  (if (and ss (> (sslength ss) 5000))
    (progn
      (initget "Da Nu")
      (if (= "Nu" (getkword (strcat "\n*** Ati selectat " (itoa (sslength ss))
                    " entitati (probabil tot desenul). Auditul profund e gandit"
                    " pentru un singur plan - selectati cu o fereastra doar zona"
                    " care va intereseaza.\n    Continuati totusi? [Da/Nu] <Nu>: ")))
        (setq ss nil)
      )
    )
  )

  (if (null ss)
    (progn (princ "\nRenuntat.") (princ))
    (progn

      (initget "Da Nu")
      (setq ansr (getkword "\nSa includ si toate perechile DXF brute (RAW)? [Da/Nu] <Nu>: "))
      (setq rawmode (= ansr "Da"))

      (initget 6)
      (setq p (getreal (strcat "\nPasul de rotunjime asteptat pentru lungimi, in mm <"
                               (rtos *ap:grid* 2 2) ">: ")))
      (if p (setq *ap:grid* p))

      ;; ---- fisierul de raport --------------------------------------------
      (setq dwgname (getvar "DWGNAME")
            dwgpath (getvar "DWGPREFIX"))
      (if (and dwgname (> (strlen dwgname) 4))
        (setq dwgname (substr dwgname 1 (- (strlen dwgname) 4)))
        (setq dwgname "Desen")
      )
      (setq defname (strcat (if dwgpath dwgpath "") dwgname "_AuditPlan.txt"))
      (setq fname (getfiled "Salvati raportul de audit profund" defname "txt" 1))
      (if fname
        (progn
          (setq f (open fname "w"))
          (if (null f)
            (princ (strcat "\n*** Nu pot scrie in " fname " - raportul apare doar pe ecran."))
          )
        )
        (princ "\n(Fara fisier - raportul apare doar pe ecran.)")
      )

      (setq grp      (ap:grouplist)
            n        (sslength ss)
            *ap:sus* nil
            *ap:nodes* nil
            counts   '())

      ;; ---- antet ----------------------------------------------------------
      (ap:out f "=========================================================")
      (ap:out f (strcat "AUDIT PROFUND - desen: " (ap:str (getvar "DWGNAME"))))
      (ap:out f (strcat "Data: " (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM:SS)")))
      (ap:out f (strcat "TOTAL ENTITATI SELECTATE: " (itoa n)))
      (ap:out f "---------------------------------------------------------")
      (ap:out f "[VARIABILE DE SISTEM]")
      (foreach v '("INSUNITS" "LUNITS" "LUPREC" "AUNITS" "AUPREC" "DIMSTYLE"
                   "DIMSCALE" "DIMLFAC" "DIMTXT" "DIMASZ" "TEXTSIZE" "TEXTSTYLE"
                   "CLAYER" "LTSCALE" "CANNOSCALE" "OSMODE" "ELEVATION" "UCSNAME")
        (ap:out f (strcat "  " v " = " (ap:str (getvar v))))
      )

      (ap:out f "[LAYERE]")
      (setq tb (tblnext "layer" T))
      (while tb
        (ap:out f (strcat "  LAYER=" (ap:str (cdr (assoc 2 tb)))
                          " CULOARE=" (ap:str (cdr (assoc 62 tb)))
                          " TIPLINIE=" (ap:str (cdr (assoc 6 tb)))
                          " FLAGS=" (ap:str (cdr (assoc 70 tb)))))
        (setq tb (tblnext "layer"))
      )

      (ap:out f "[STILURI DE COTARE]")
      (setq tb (tblnext "dimstyle" T))
      (while tb
        (ap:out f (strcat "  DIMSTYLE=" (ap:str (cdr (assoc 2 tb))) " : "
                          (ap:dimstyleinfo (cdr (assoc 2 tb)))))
        (setq tb (tblnext "dimstyle"))
      )

      (ap:out f "[STILURI DE TEXT]")
      (setq tb (tblnext "style" T))
      (while tb
        (ap:out f (strcat "  STYLE=" (ap:str (cdr (assoc 2 tb)))
                          " FONT=" (ap:str (cdr (assoc 3 tb)))
                          " BIGFONT=" (ap:str (cdr (assoc 4 tb)))
                          " H=" (ap:r (cdr (assoc 40 tb)))
                          " LATIME=" (ap:r (cdr (assoc 41 tb)))
                          " OBLIC=" (ap:ang (cdr (assoc 50 tb)))))
        (setq tb (tblnext "style"))
      )

      (ap:out f "---------------------------------------------------------")
      (ap:out f "[ENTITATI]")

      ;; ---- entitatile -----------------------------------------------------
      (setq i 0)
      (repeat n
        (setq ent   (ssname ss i)
              e     (entget ent)
              etype (cdr (assoc 0 e))
              lay   (cdr (assoc 8 e))
              hnd   (cdr (assoc 5 e))
              geo   ""
              txt   ""
              blkname "")

        ;; numele real al grupului
        (setq grpnm "-" grphnd "-")
        (if (setq key (assoc ent grp))
          (setq grpnm (cadr key) grphnd (caddr key))
        )

        ;; statistica
        (setq key (strcat etype " @ " lay))
        (if (setq cnt (assoc key counts))
          (setq counts (subst (cons key (1+ (cdr cnt))) cnt counts))
          (setq counts (cons (cons key 1) counts))
        )

        (cond

          ;; -------------------------------------------------- LINE
          ((= etype "LINE")
           (setq geo (strcat "P1=" (ap:pt (ap:dxf 10 e))
                             " P2=" (ap:pt (ap:dxf 11 e))
                             " LUNGIME=" (ap:r (distance (ap:dxf 10 e) (ap:dxf 11 e)))
                             " UNGHI=" (ap:ang (angle (ap:dxf 10 e) (ap:dxf 11 e)))))
           (ap:checkseg ent lay (ap:dxf 10 e) (ap:dxf 11 e) 1)
          )

          ;; -------------------------------------------------- LWPOLYLINE
          ((= etype "LWPOLYLINE")
           (setq verts (ap:lwverts e))
           (setq obj (vlax-ename->vla-object ent))
           (setq geo (strcat "NRVARF=" (itoa (length verts))
                             " INCHISA=" (if (= 1 (logand 1 (cond ((ap:dxf 70 e)) (0)))) "DA" "NU")
                             " ELEV=" (ap:r (ap:dxf 38 e))
                             " LATCONST=" (ap:r (ap:dxf 43 e))
                             " ARIE=" (ap:r (ap:get obj 'Area))
                             " LUNGTOT=" (ap:r (ap:get obj 'Length))
                             " VARFURI=[" (ap:vstr verts) "]"))
           (ap:auditverts ent lay verts (= 1 (logand 1 (cond ((ap:dxf 70 e)) (0)))))
          )

          ;; -------------------------------------------------- POLYLINE (veche)
          ((= etype "POLYLINE")
           (setq verts (ap:plverts ent))
           (setq geo (strcat "NRVARF=" (itoa (length verts))
                             " FLAGS=" (ap:str (ap:dxf 70 e))
                             " VARFURI=[" (ap:vstr verts) "]"))
           (ap:auditverts ent lay verts (= 1 (logand 1 (cond ((ap:dxf 70 e)) (0)))))
          )

          ;; -------------------------------------------------- ARC / CIRCLE
          ((= etype "ARC")
           (setq geo (strcat "CENTRU=" (ap:pt (ap:dxf 10 e))
                             " RAZA=" (ap:r (ap:dxf 40 e))
                             " START=" (ap:r (ap:dxf 50 e))
                             " STOP=" (ap:r (ap:dxf 51 e))))
          )
          ((= etype "CIRCLE")
           (setq geo (strcat "CENTRU=" (ap:pt (ap:dxf 10 e))
                             " RAZA=" (ap:r (ap:dxf 40 e))))
          )

          ;; -------------------------------------------------- ELLIPSE
          ((= etype "ELLIPSE")
           (setq geo (strcat "CENTRU=" (ap:pt (ap:dxf 10 e))
                             " AXAMARE=" (ap:pt (ap:dxf 11 e))
                             " RAPORT=" (ap:r (ap:dxf 40 e))
                             " PARSTART=" (ap:r (ap:dxf 41 e))
                             " PARSTOP=" (ap:r (ap:dxf 42 e))))
          )

          ;; -------------------------------------------------- TEXT
          ((= etype "TEXT")
           (setq txt (ap:str (ap:dxf 1 e)))
           (setq geo (strcat "INSERT=" (ap:pt (ap:dxf 10 e))
                             " ALINIERE=" (ap:pt (ap:dxf 11 e))
                             " H=" (ap:r (ap:dxf 40 e))
                             " ROT=" (ap:ang (ap:dxf 50 e))
                             " LATIME=" (ap:r (ap:dxf 41 e))
                             " OBLIC=" (ap:ang (ap:dxf 51 e))
                             " STIL=" (ap:str (ap:dxf 7 e))
                             " JUSTH=" (ap:str (ap:dxf 72 e))
                             " JUSTV=" (ap:str (ap:dxf 73 e))))
          )

          ;; -------------------------------------------------- MTEXT
          ((= etype "MTEXT")
           (setq obj (vlax-ename->vla-object ent))
           (setq txt (ap:str (ap:get obj 'TextString)))
           (setq geo (strcat "INSERT=" (ap:pt (ap:dxf 10 e))
                             " H=" (ap:r (ap:dxf 40 e))
                             " LATREF=" (ap:r (ap:dxf 41 e))
                             " LATREALA=" (ap:r (ap:dxf 42 e))
                             " INALTREALA=" (ap:r (ap:dxf 43 e))
                             " ROT=" (ap:ang (ap:dxf 50 e))
                             " ANCORA=" (ap:str (ap:dxf 71 e))
                             " DIRECTIE=" (ap:str (ap:dxf 72 e))
                             " STIL=" (ap:str (ap:dxf 7 e))
                             " BRUT=[" (ap:str (ap:dxf 1 e)) "]"))
          )

          ;; -------------------------------------------------- DIMENSION
          ((= etype "DIMENSION")
           (setq obj    (vlax-ename->vla-object ent)
                 dimsty (ap:str (ap:dxf 3 e))
                 dimmeas (ap:get obj 'Measurement)
                 d13    (ap:dxf 13 e)
                 d14    (ap:dxf 14 e))
           (setq txt (strcat "masurat=" (ap:r dimmeas)
                             " override=[" (ap:str (ap:dxf 1 e)) "]"))
           (setq geo (strcat "TIPCOTA=" (ap:str (ap:dxf 70 e))
                             " PLINIE=" (ap:pt (ap:dxf 10 e))
                             " PTEXT=" (ap:pt (ap:dxf 11 e))
                             " P12=" (ap:pt (ap:dxf 12 e))
                             " P13=" (ap:pt d13)
                             " P14=" (ap:pt d14)
                             " P15=" (ap:pt (ap:dxf 15 e))
                             " P16=" (ap:pt (ap:dxf 16 e))
                             " ROTCOTA=" (ap:ang (ap:dxf 50 e))
                             " ROTTEXT=" (ap:ang (ap:dxf 53 e))
                             " OBLIC=" (ap:ang (ap:dxf 52 e))
                             " BLOC=" (ap:str (ap:dxf 2 e))
                             " STIL=" dimsty
                             " DIST13_14=" (if (and d13 d14) (ap:r (distance d13 d14)) "-")
                             " {" (ap:dimstyleinfo dimsty) "}"
                             (ap:xdata ent)))
           ;; cote suspecte
           (if (and d13 d14)
             (progn
               ;; abaterea perpendiculara fata de directia cotei: cat de "stramb"
               ;; stau cele doua puncte de definitie unul fata de celalalt
               (setq dlf  (cond ((cdr (assoc 144 (tblsearch "dimstyle" dimsty)))) (1.0))
                     drot (cond ((ap:dxf 50 e)) (0.0))
                     dper (abs (- (* (- (car d14) (car d13)) (sin drot))
                                  (* (- (cadr d14) (cadr d13)) (cos drot))))
                     dpar (abs (+ (* (- (car d14) (car d13)) (cos drot))
                                  (* (- (cadr d14) (cadr d13)) (sin drot)))))
               (setq geo (strcat geo " PERP=" (ap:r dper) " PARAL=" (ap:r dpar)))
               (if (> dper *ap:gtol*)
                 (ap:addsus ent "COTA STRAMBA"
                   (strcat "punctele de definitie sunt decalate cu "
                           (rtos dper 2 4) " mm perpendicular pe directia cotei ("
                           (ap:pt d13) " - " (ap:pt d14) ")"))
               )
               ;; valoarea reala masurata, adusa in mm prin DIMLFAC
               (if (and dimmeas (> dlf 0.0))
                 (if (ap:offgrid (/ dimmeas dlf))
                   (ap:addsus ent "COTA NEROTUNDA"
                     (strcat "valoare reala " (rtos (/ dimmeas dlf) 2 4)
                             " mm (nu e multiplu de " (rtos *ap:grid* 2 2)
                             " mm); afisat: " (ap:r dimmeas)
                             ", DIMLFAC=" (ap:r dlf) ", stil " dimsty))
                 )
               )
             )
           )
          )

          ;; -------------------------------------------------- HATCH
          ((= etype "HATCH")
           (setq obj (vlax-ename->vla-object ent))
           (setq geo (strcat "PATTERN=" (ap:str (ap:dxf 2 e))
                             " SOLID=" (ap:str (ap:dxf 70 e))
                             " ASOCIATIV=" (ap:str (ap:dxf 71 e))
                             " NRCONTUR=" (ap:str (ap:dxf 91 e))
                             " SCARA=" (ap:r (ap:dxf 41 e))
                             " UNGHI=" (ap:ang (ap:dxf 52 e))
                             " ARIE=" (ap:r (ap:get obj 'Area))
                             " SEED=" (ap:ptlist (ap:allcodes 98 e))))
          )

          ;; -------------------------------------------------- INSERT
          ((= etype "INSERT")
           (setq blkname (ap:str (ap:dxf 2 e)))
           (setq obj (vlax-ename->vla-object ent))
           (setq geo (strcat "INSERT=" (ap:pt (ap:dxf 10 e))
                             " XS=" (ap:r (ap:get obj 'XScaleFactor))
                             " YS=" (ap:r (ap:get obj 'YScaleFactor))
                             " ROT=" (ap:ang (ap:get obj 'Rotation))
                             " EFFNAME=" (ap:str (ap:get obj 'EffectiveName))))
           ;; atribute
           (setq atts "" a (entnext ent))
           (while (and a (= "ATTRIB" (cdr (assoc 0 (setq ae (entget a))))))
             (setq atts (strcat atts " " (ap:str (cdr (assoc 2 ae)))
                                "=[" (ap:str (cdr (assoc 1 ae))) "]"))
             (setq a (entnext a))
           )
           (if (/= atts "") (setq geo (strcat geo " ATRIBUTE={" atts " }")))
          )

          ;; -------------------------------------------------- WIPEOUT
          ((= etype "WIPEOUT")
           (setq geo (strcat "P10=" (ap:pt (ap:dxf 10 e))
                             " VECTU=" (ap:pt (ap:dxf 11 e))
                             " VECTV=" (ap:pt (ap:dxf 12 e))
                             " MARIME=" (ap:pt (ap:dxf 13 e))
                             " CONTUR=[" (ap:ptlist (ap:allcodes 14 e)) "]"))
          )
        )

        ;; gabaritul selectiei
        (setq p (ap:dxf 10 e))
        (if (and p (or (/= etype "HATCH") (/= (ap:pt p) "0.0000,0.0000")))
          (progn
            (if (or (null minx) (< (car p) minx))  (setq minx (car p)))
            (if (or (null maxx) (> (car p) maxx))  (setq maxx (car p)))
            (if (or (null miny) (< (cadr p) miny)) (setq miny (cadr p)))
            (if (or (null maxy) (> (cadr p) maxy)) (setq maxy (cadr p)))
          )
        )

        ;; raw DXF, daca s-a cerut
        (setq raw "")
        (if rawmode
          (progn
            (setq raw " | RAW={")
            (foreach x e
              (if (not (member (car x) '(-1 5 100 102 330 360 410 420 430 440)))
                (setq raw (strcat raw (itoa (car x)) ":" (ap:str (cdr x)) " "))
              )
            )
            (setq raw (strcat raw "}"))
          )
        )

        (setq line (strcat "TIP=" (ap:str etype)
                           " | LAYER=" (ap:str lay)
                           " | HANDLE=" (ap:str hnd)
                           " | GRUP=" grpnm "/" grphnd
                           " | TEXT=[" txt "]"
                           " | NUME_BLOC=[" blkname "]"
                           " | GEO={" geo "}"
                           raw))
        (ap:out f line)
        (setq i (1+ i))
        (if (= 0 (rem i 100))
          (princ (strcat "\r  ... " (itoa i) " / " (itoa n) " entitati   "))
        )
      )

      ;; ---- noduri neunite --------------------------------------------------
      (ap:checknodes)

      ;; ---- recapitulare ----------------------------------------------------
      (ap:out f "---------------------------------------------------------")
      (ap:out f "[RECAPITULARE TIP @ LAYER]")
      (foreach c (vl-sort counts '(lambda (a b) (< (car a) (car b))))
        (ap:out f (strcat "  " (car c) " = " (itoa (cdr c))))
      )
      (if minx
        (ap:out f (strcat "[GABARIT PUNCTE DE INSERTIE] X: " (ap:r minx) " .. " (ap:r maxx)
                          "  |  Y: " (ap:r miny) " .. " (ap:r maxy)
                          "  |  DX=" (ap:r (- maxx minx)) " DY=" (ap:r (- maxy miny))))
      )

      (setq sus (reverse *ap:sus*))
      (ap:out f "---------------------------------------------------------")
      (ap:out f (strcat "[ELEMENTE SUSPECTE] - " (itoa (length sus))
                        " (pas de rotunjime folosit: " (rtos *ap:grid* 2 2) " mm)"))
      (setq i 1)
      (foreach s sus
        (ap:out f (strcat "  " (itoa i) ". [" (cadr s) "] HANDLE="
                          (ap:str (cdr (assoc 5 (entget (car s))))) " : " (caddr s)))
        (setq i (1+ i))
      )

      (ap:out f "---------------------------------------------------------")
      (ap:out f (strcat "SFARSIT AUDIT PROFUND - " (itoa n) " entitati."))

      (if f (progn (close f) (princ (strcat "\n\n*** Raport salvat in: " fname " ***"))))

      (if sus
        (progn
          (initget "Da Nu")
          (setq ansr (getkword (strcat "\n" (itoa (length sus))
                                 " elemente suspecte. Le parcurgem acum, cu zoom pe fiecare? [Da/Nu] <Da>: ")))
          (if (/= ansr "Nu") (c:AUDITSUS))
        )
        (princ "\nNu s-a gasit niciun element suspect.")
      )
      (princ)
    )
  )
)

;; verifica toate segmentele unei polilinii
(defun ap:auditverts (ent lay verts closed / i n p1 p2)
  (setq n (length verts) i 0)
  (while (< i (1- n))
    (setq p1 (car (nth i verts))
          p2 (car (nth (1+ i) verts)))
    (if (< (abs (cadr (nth i verts))) 1e-9)   ;; doar segmentele drepte
      (ap:checkseg ent lay p1 p2 (1+ i))
      (setq *ap:nodes* (cons (list p1 ent lay) *ap:nodes*))
    )
    (setq i (1+ i))
  )
  (if (and closed (> n 2))
    (progn
      (setq p1 (car (nth (1- n) verts)) p2 (car (nth 0 verts)))
      (if (< (abs (cadr (nth (1- n) verts))) 1e-9)
        (ap:checkseg ent lay p1 p2 n)
      )
    )
  )
  (princ)
)

(princ "\nAUDITPLAN  - audit profund (geometrie completa + detector de suspecti).")
(princ "\nAUDITSUS   - reia parcurgerea vizuala a elementelor suspecte.")
(princ)
