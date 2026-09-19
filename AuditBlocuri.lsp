;; =========================================================================
;; AUDITBLOCURI  (versiunea cu SALVARE IN FISIER)
;; La fel ca AuditFasonare, dar arata si NUMELE BLOCULUI pentru entitatile
;; INSERT (util pentru identificarea etrierilor de mansarda dupa numele
;; blocului lor, nu dupa pozitie).
;;
;; NOU: pe langa afisarea in fereastra de comenzi (F2), raportul complet
;;      este scris intr-un fisier text pe care il alegeti la inceput.
;;      Implicit fisierul se propune langa desen, cu numele
;;      <numedesen>_AuditBlocuri.txt
;; =========================================================================

;; --- helper: scrie o linie si pe ecran si in fisier -----------------------
(defun ab:out (f s)
  (princ (strcat "\n" s))
  (if f (write-line s f))
  (princ)
)

;; --- helper: text sigur (nil -> "") --------------------------------------
(defun ab:str (v)
  (cond ((null v) "")
        ((= (type v) 'STR) v)
        (T (vl-princ-to-string v))
  )
)

(defun c:AuditBlocuri ( / ss i ent edata etype lay grp grpname pt txt blkname obj
                          xsc ysc rotdeg dynprops effname isdyn proplist pname pval
                          dimmeas dimtxt fname f dwgname dwgpath defname ptstr line n)

  (vl-load-com)

  (princ "\nSelectati entitatile de verificat: ")
  (setq ss (ssget))
  (if (null ss)
    (progn (princ "\nNimic selectat.") (princ))
    (progn

      ;; ---- alegerea fisierului de raport ---------------------------------
      (setq dwgname (getvar "DWGNAME")
            dwgpath (getvar "DWGPREFIX"))
      (if (and dwgname (> (strlen dwgname) 4))
        (setq dwgname (substr dwgname 1 (- (strlen dwgname) 4)))
        (setq dwgname "Desen")
      )
      (setq defname (strcat (if dwgpath dwgpath "") dwgname "_AuditBlocuri.txt"))

      (setq fname (getfiled "Salvati raportul de audit" defname "txt" 1))

      (if fname
        (progn
          (setq f (open fname "w"))
          (if (null f)
            (princ (strcat "\n*** Nu pot scrie in fisierul: " fname " (este deschis in alt program?) Raportul apare doar pe ecran."))
          )
        )
        (princ "\n(Nu s-a ales fisier - raportul apare doar pe ecran.)")
      )

      ;; ---- antet ----------------------------------------------------------
      (setq n (sslength ss))
      (ab:out f "=======================================================")
      (ab:out f (strcat "AUDIT BLOCURI - desen: " (ab:str (getvar "DWGNAME"))))
      (ab:out f (strcat "Data: "
                        (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM:SS)")))
      (ab:out f (strcat "TOTAL ENTITATI SELECTATE: " (itoa n)))
      (ab:out f "-------------------------------------------------------")

      (setq i 0)
      (repeat n
        (setq ent   (ssname ss i))
        (setq edata (entget ent))
        (setq etype (cdr (assoc 0 edata)))
        (setq lay   (cdr (assoc 8 edata)))

        ;; grupid, la fel ca la AuditFasonare
        (setq grpname "-")
        (foreach item edata
          (if (= (car item) 330)
            (if (= (cdr (assoc 0 (entget (cdr item)))) "GROUP")
              (setq grpname (vl-princ-to-string (cdr item)))
            )
          )
        )

        (setq pt (cdr (assoc 10 edata)))
        (setq txt "")
        (setq blkname "")
        (setq xsc "") (setq ysc "") (setq rotdeg "") (setq dynprops "")

        (cond
          ((member etype '("TEXT" "MTEXT"))
           (setq obj (vlax-ename->vla-object ent))
           (setq txt (vla-get-textstring obj))
          )
          ((= etype "DIMENSION")
           (setq obj (vlax-ename->vla-object ent))
           (setq dimmeas (vl-catch-all-apply 'vlax-get (list obj 'Measurement)))
           (setq dimtxt (cdr (assoc 1 edata))) ;; text override, gol daca foloseste valoarea calculata
           (setq txt (strcat "masurat=" (if (vl-catch-all-error-p dimmeas) "?" (rtos dimmeas 2 2))
                             " override=[" (ab:str dimtxt) "]"))
          )
          ((= etype "INSERT")
           (setq blkname (ab:str (cdr (assoc 2 edata))))
           (setq obj (vlax-ename->vla-object ent))
           (setq xsc (rtos (vlax-get obj 'XScaleFactor) 2 4))
           (setq ysc (rtos (vlax-get obj 'YScaleFactor) 2 4))
           (setq rotdeg (rtos (/ (* (vlax-get obj 'Rotation) 180.0) pi) 2 2))
           (setq dynprops "")

           ;; nume real (EffectiveName), util cand blocname e anonim (*U...)
           (setq effname (vl-catch-all-apply 'vlax-get (list obj 'EffectiveName)))
           (if (not (vl-catch-all-error-p effname))
             (setq dynprops (strcat dynprops " EFFNAME=[" (vl-princ-to-string effname) "]"))
             (setq dynprops (strcat dynprops " EFFNAME=[ERR:" (vl-catch-all-error-message effname) "]"))
           )

           (setq isdyn (vl-catch-all-apply 'vlax-get (list obj 'IsDynamicBlock)))
           (if (vl-catch-all-error-p isdyn)
             (setq dynprops (strcat dynprops " ISDYN=[ERR:" (vl-catch-all-error-message isdyn) "]"))
             (progn
               (setq dynprops (strcat dynprops " ISDYN=[" (vl-princ-to-string isdyn) "]"))
               (if (and isdyn (not (eq isdyn 0)))
                 (progn
                   (setq proplist (vl-catch-all-apply 'vlax-invoke (list obj 'GetDynamicBlockProperties)))
                   (if (vl-catch-all-error-p proplist)
                     (setq dynprops (strcat dynprops " DYNPROPS=[ERR:" (vl-catch-all-error-message proplist) "]"))
                     (progn
                       (setq dynprops (strcat dynprops " DYNPROPS=["))
                       (foreach p proplist
                         (setq pname (vl-catch-all-apply 'vlax-get (list p 'PropertyName)))
                         (setq pval  (vl-catch-all-apply 'vlax-get (list p 'Value)))
                         (setq dynprops (strcat dynprops
                                         (if (vl-catch-all-error-p pname) "?" (vl-princ-to-string pname)) "="
                                         (if (vl-catch-all-error-p pval) "?" (vl-princ-to-string pval)) "; "))
                       )
                       (setq dynprops (strcat dynprops "]"))
                     )
                   )
                 )
               )
             )
           )
          )
        )

        (setq ptstr (if pt
                      (strcat (rtos (car pt) 2 3) "," (rtos (cadr pt) 2 3))
                      "-"))

        (setq line (strcat "TIP=" (ab:str etype) " | LAYER=" (ab:str lay) " | GRUPID=" grpname
                           " | PUNCT=" ptstr
                           " | TEXT=[" (ab:str txt) "]" " | NUME_BLOC=[" blkname "]"
                           " | XSCALE=" xsc " | YSCALE=" ysc " | ROT(deg)=" rotdeg
                           dynprops))

        (ab:out f line)

        (setq i (1+ i))
      )

      (ab:out f "-------------------------------------------------------")
      (ab:out f (strcat "SFARSIT AUDIT - " (itoa n) " entitati."))

      (if f
        (progn
          (close f)
          (princ (strcat "\n\n*** Raport salvat in: " fname " ***"))
        )
      )
      (princ)
    )
  )
)

(princ "\nScrieti 'AuditBlocuri' pentru diagnosticare (raportul se salveaza si intr-un fisier .txt).")
(princ)
