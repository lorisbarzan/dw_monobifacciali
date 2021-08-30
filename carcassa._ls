(defun c:regassetmp ()
  (setq cabina (getvar "dwgname"))
  (setq cabina (substr cabina 1 (- (strlen cabina) 4)))
  (SETQ CABINA (SUBSTR CABINA 1 10))
  (setq gr (ssget "x" (list  '(0 . "INSERT") '(2 . "$evidenzia*"))))
  (setq nr -1)
  (setq listacasse nil)
  (setq listapannelli nil)
  (setq pancascorr nil)
  (while (setq ent (ssname gr (setq nr (+ 1 nr))))
    (setq hande (cdr (assoc '5 (entget ent))))
    (setq codpas (cdr (assoc '1 (entget (entnext ent)))))
    (setq hand (cdr (assoc '1 (entget (entnext (entnext ent))))))
    (setq tipca (cdr (assoc '1 (entget (entnext (entnext (entnext ent)))))))
    (setq ncas (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext ent))))))))
    (setq desc (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext ent)))))))))
    (setq listacasse (cons (list hand codpas hande tipca ncas desc) listacasse))
    (setq listapannelli (cons codpas listapannelli))
    (setq disegno (vl-filename-base (getvar "dwgname")))
    (setq pancascorr (cons (list disegno codpas 1 "0" "NULL" "NULL" "NULL" "NULL" "NULL" "NULL" "NULL" (strcat CABINA "_" tipca "_" ncas)) pancascorr))
    )
  (setq lalf (ssget "X" (list '(0 . "INSERT") '(2 . "codpas"))))
  (if (/= (sslength lalf) (length listapannelli))
    (progn
      (alert "Non sono stati associati a casse tutti i pannelli!!!")
      (exit)
      )
    )
  (setq revisione (substr (getvar "dwgname") 16 1))
  (setq lista nil)
  (setq grpn (ssget "x" (list '(0 . "INSERT") '(2 . "codpas"))))
  (setq lista nil)
  (SETQ LISTA_P NIL)
  ; Creo copia bak e riscrivo cabpan
  (vl-file-delete (strcat modalita "cabpan_.bak"))
  (vl-file-copy  (strcat modalita "cabpan.txt")  (strcat modalita "cabpan_.bak"))
  (setq filer (S_OPEN (strcat modalita "cabpan_.bak") "r"))
  (setq filew (S_OPEN (strcat modalita "cabpan.txt") "w"))
  ;(setq pancascorr nil)
  (while (setq rig (read-line filer))
    (if (/= cabina (car (read (strcat "(" rig ")"))))
      (progn
	(write-line rig filew)
	)
      (progn
	(if (null (member (nth 1 (read (strcat "(" rig ")"))) listapannelli))
	  (setq pancascorr (cons (read (strcat "(" rig ")")) pancascorr))
	  )
	)
      )
    )
  (s_close filer (strcat modalita "cabpan_.bak"))
  (s_close filew (strcat modalita "cabpan.txt"))
  (setq filew (S_OPEN (strcat modalita "cabpan.txt") "a"))
  (if pancascorr
    (foreach m (reverse pancascorr)
      (setq nn 0)
      (setq mm nil)
      (repeat 12
	(if (nth nn m) (setq mm (reverse (cons (nth nn m) (reverse mm))))
	  (setq mm (reverse (cons "NULL" (reverse mm))))
	  )
	(setq nn (+ 1 nn))
	)
      (setq m mm)
      (setq rig (strcat "\"" (nth 0 m) "\"\t\"" (nth 1 m) "\"\t" (rtos (nth 2 m) 2 0) "\t\""  (nth 3 m) "\"\t\""  (nth 4 m) "\"\t\"" (nth 5 m) "\"\t\"" (nth 6 m) "\"\t\"" (nth 7 m) "\"\t\"" (nth 8 m) "\"\t\"" (nth 9 m) "\"\t\"" (nth 10 m) "\"\t\""	(nth 11 m) "\""))
      (write-line rig filew)
      )
    )
  (s_close filew (strcat modalita "cabpan.txt"))

  ; Creo copia bak e riscrivo casse vedi sopra
  (setq listacasse_ listacasse listacasse nil)
  (foreach n listacasse_
    (setq ncassa (strcat cabina "_" (nth 3 n) "_" (nth 4 n)))
    (if (null (assoc ncassa listacasse))
      (setq listacasse (cons (list ncassa (nth 3 n) (nth 5 n) (nth 4 n) 1) listacasse))
      (setq listacasse (subst (list ncassa (nth 3 n) (nth 5 n) (nth 4 n) (+ 1 (nth 4 (assoc ncassa listacasse)))) (assoc ncassa listacasse) listacasse))
      )
    )
  (vl-file-delete (strcat modalita "casse_.bak"))
  (vl-file-copy  (strcat modalita "casse.txt")  (strcat modalita "casse_.bak"))
  (setq filer (S_OPEN (strcat modalita "casse_.bak") "r"))
  (setq filew (S_OPEN (strcat modalita "casse.txt") "w"))
  (while (setq rig (read-line filer))
    (setq cfr (car (read (strcat "(" rig ")"))))
    (if (/= cabina cfr)
      (progn
	(write-line rig filew)
	)
      )
    )
  (s_close filer (strcat modalita "casse_.bak"))
  (s_close filew (strcat modalita "casse.txt"))
  (setq filew (S_OPEN (strcat modalita "casse.txt") "a"))
  (if listacasse
    (foreach m listacasse
      (setq rig (strcat "\"" cabina "\"\t\"" (nth 0 m) "\"\t\"" (nth 1 m) "\"\t\"" (nth 2 m) "\"\t" (nth 3 m) "\t" (rtos (nth 4 m) 2 0)))
      (write-line rig filew)
      )
    )
  (s_close filew (strcat modalita "casse.txt"))
  )

(defun c:regassedef ()
  (if (/= modalita (cadr (assoc 'percregdef parametri))) (C:ANNTAB))
  (setq modalita (cadr (assoc 'percregdef parametri)))
  (if (/= modalitac (cadr (assoc 'percregcom parametri))) (C:ANNTAB))
  (setq modalitac (cadr (assoc 'percregcom parametri)))
  (setq cabina (getvar "dwgname"))
  (setq cabina (substr cabina 1 (- (strlen cabina) 4)))
  (SETQ CABINA (SUBSTR CABINA 1 10))
  (setq gr (ssget "x" (list  '(0 . "INSERT") '(2 . "$evidenzia*"))))
  (setq nr -1)
  (setq listacasse nil)
  (setq listapannelli nil)
  (setq pancascorr nil)
  (while (setq ent (ssname gr (setq nr (+ 1 nr))))
    (setq hande (cdr (assoc '5 (entget ent))))
    (setq codpas (cdr (assoc '1 (entget (entnext ent)))))
    (setq hand (cdr (assoc '1 (entget (entnext (entnext ent))))))
    (setq tipca (cdr (assoc '1 (entget (entnext (entnext (entnext ent)))))))
    (setq ncas (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext ent))))))))
    (setq desc (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext ent)))))))))
    (setq listacasse (cons (list hand codpas hande tipca ncas desc) listacasse))
    (setq listapannelli (cons codpas listapannelli))
    (setq disegno (vl-filename-base (getvar "dwgname")))
    (setq pancascorr (cons (list disegno codpas 1 "0" "NULL" "NULL" "NULL" "NULL" "NULL" "NULL" "NULL" (strcat CABINA "_" tipca "_" ncas)) pancascorr))
    )
  (setq lalf (ssget "X" (list '(0 . "INSERT") '(2 . "codpas"))))
  (if (/= (sslength lalf) (length listapannelli))
    (progn
      (alert "Non sono stati associati a casse tutti i pannelli!!!")
      (exit)
      )
    )
  (setq revisione (substr (getvar "dwgname") 16 1))
  (setq lista nil)
  (setq grpn (ssget "x" (list '(0 . "INSERT") '(2 . "codpas"))))
  (setq lista nil)
  (SETQ LISTA_P NIL)
  ; Creo copia bak e riscrivo cabpan
  (vl-file-delete (strcat modalita "cabpan_.bak"))
  (vl-file-copy  (strcat modalita "cabpan.txt")  (strcat modalita "cabpan_.bak"))
  (setq filer (S_OPEN (strcat modalita "cabpan_.bak") "r"))
  (setq filew (S_OPEN (strcat modalita "cabpan.txt") "w"))
  ;(setq pancascorr nil)
  (while (setq rig (read-line filer))
    (if (/= cabina (car (read (strcat "(" rig ")"))))
      (progn
	(write-line rig filew)
	)
      (progn
	(if (null (member (nth 1 (read (strcat "(" rig ")"))) listapannelli))
	  (setq pancascorr (cons (read (strcat "(" rig ")")) pancascorr))
	  )
	)
      )
    )
  (s_close filer (strcat modalita "cabpan_.bak"))
  (s_close filew (strcat modalita "cabpan.txt"))
  (setq filew (S_OPEN (strcat modalita "cabpan.txt") "a"))
  (if pancascorr
    (foreach m (reverse pancascorr)
      (setq nn 0)
      (setq mm nil)
      (repeat 12
	(if (nth nn m) (setq mm (reverse (cons (nth nn m) (reverse mm))))
	  (setq mm (reverse (cons "NULL" (reverse mm))))
	  )
	(setq nn (+ 1 nn))
	)
      (setq m mm)
      (setq rig (strcat "\"" (nth 0 m) "\"\t\"" (nth 1 m) "\"\t" (rtos (nth 2 m) 2 0) "\t\""  (nth 3 m) "\"\t\""  (nth 4 m) "\"\t\"" (nth 5 m) "\"\t\"" (nth 6 m) "\"\t\"" (nth 7 m) "\"\t\"" (nth 8 m) "\"\t\"" (nth 9 m) "\"\t\"" (nth 10 m) "\"\t\""	(nth 11 m) "\""))
      (write-line rig filew)
      )
    )
  (s_close filew (strcat modalita "cabpan.txt"))

  ; Creo copia bak e riscrivo casse vedi sopra
  (setq listacasse_ listacasse listacasse nil)
  (foreach n listacasse_
    (setq ncassa (strcat cabina "_" (nth 3 n) "_" (nth 4 n)))
    (if (null (assoc ncassa listacasse))
      (setq listacasse (cons (list ncassa (nth 3 n) (nth 5 n) (nth 4 n) 1) listacasse))
      (setq listacasse (subst (list ncassa (nth 3 n) (nth 5 n) (nth 4 n) (+ 1 (nth 4 (assoc ncassa listacasse)))) (assoc ncassa listacasse) listacasse))
      )
    )
  (vl-file-delete (strcat modalita "casse_.bak"))
  (vl-file-copy  (strcat modalita "casse.txt")  (strcat modalita "casse_.bak"))
  (setq filer (S_OPEN (strcat modalita "casse_.bak") "r"))
  (setq filew (S_OPEN (strcat modalita "casse.txt") "w"))
  (while (setq rig (read-line filer))
    (setq cfr (car (read (strcat "(" rig ")"))))
    (if (/= cabina cfr)
      (progn
	(write-line rig filew)
	)
      )
    )
  (s_close filer (strcat modalita "casse_.bak"))
  (s_close filew (strcat modalita "casse.txt"))
  (setq filew (S_OPEN (strcat modalita "casse.txt") "a"))
  (if listacasse
    (foreach m listacasse
      (setq rig (strcat "\"" cabina "\"\t\"" (nth 0 m) "\"\t\"" (nth 1 m) "\"\t\"" (nth 2 m) "\"\t" (nth 3 m) "\t" (rtos (nth 4 m) 2 0)))
      (write-line rig filew)
      )
    )
  (s_close filew (strcat modalita "casse.txt"))
  (setq modalita (cadr (assoc 'percregtmp parametri)))
  )

(defun c:Carcassa ()
  (setq gr (ssget "x" (list  '(0 . "INSERT") '(2 . "$evidenzia*"))))
  (setq nr -1)
  (if gr
    (while
      (setq ent (ssname gr (setq nr (+ 1 nr))))
      (setq hande (cdr (assoc '5 (entget ent))))
      (setq codpas (cdr (assoc '1 (entget (entnext ent)))))
      (setq hand (cdr (assoc '1 (entget (entnext (entnext ent))))))
      (setq tipca (cdr (assoc '1 (entget (entnext (entnext ent))))))
      (setq ncas (cdr (assoc '1 (entget (entnext (entnext ent))))))
      (setq desc (cdr (assoc '1 (entget (entnext (entnext ent))))))
      (setq listacasse (cons (list hand codpas hande tipca ncas desc) listacasse))
      )
    )

  (setq parametri (carica "parametri" ""))
;;;  (if (null tipcas) (setq tipcas "LCA"))
  (setq val (cdr (assoc 'lnomicas parametri)))
  (setq lnomicas nil)
  (foreach v val
    (setq lnomicas (cons (list (nth 0 v) (nth 1 v)) lnomicas))
    (setq ldescricas (cons (list (nth 0 v) (nth 2 v)) ldescricas))
    )
  (setq lnomicas (reverse lnomicas))
;;;  (setq lnomicas '(("LCA" 0) ("LCF" 0) ("LPF" 0)))
  (if (null tipcas) (setq tipcas (car (car lnomicas))))
  (setq ini "")
  (setq ini_ "")
  (setq iniev nil)
  (foreach n lnomicas
    (setq ini (strcat (car n) " " ini))
    (setq ini_ (strcat (car n) "/" ini_))
    (setq iniev (cons (car n) iniev))
    )
  (setq ini_ (substr ini_ 1 (- (strlen ini_)1)))
  (setq casdescri nil)  
  (initget ini)
  (setq tipcas_ (getkword (strcat "\nSelezionare tipo Cassa? [" ini_ "]<"tipcas">:")))
  (if tipcas_ (setq tipcas tipcas_))
  (setq descrizcassa (cadr (assoc tipcas ldescricas)))
  (setq disegno (vl-filename-base (getvar "dwgname")))
  (setq ncas 1)
  (setq ncas_ (getint "\nNumero cassa <1>:"))
  (if ncas_ (setq ncas ncas_))
  (setq gr (ssget (list '(0 . "INSERT") '(8 . "siglepan") '(2 . "codpas"))))
  (setq nr -1)
  (while (setq ent (ssname gr (setq nr (+ 1 nr))))
    (setq codpas (cdr (assoc '1 (entget (entnext ent)))))
    (setq hand (cdr (assoc '1 (entget (entnext (entnext ent))))))
    (setq pevid (list (cadr (assoc '10 (entget ent))) (caddr (assoc '10 (entget ent))) (cadddr (assoc '10 (entget ent)))))
    (setq peviang (cdr (assoc '50 (entget ent))))
    (if (assoc hand listacasse)
      (progn
	(setvar "pickstyle" 1)
      (command"_select" (handent (nth 2 (assoc hand listacasse))) "")
      (command"_erase" "_p" "")))
    (cond
    ((= tipcas (car iniev)) (setq evidenzia "$evidenzia"))
    ((= tipcas (cadr iniev)) (setq evidenzia "$evidenziav"))
    ((= tipcas (caddr iniev)) (setq evidenzia "$evidenziar"))
    )
    (vai_l "tmp_selezione")
    (command "_insert" evidenzia "_non" pevid 1 1 (ANGTOS peviang) codpas hand tipcas (rtos ncas 2 0) descrizcassa)
    (torna_l)
    (setq ent1 (entlast))
    (vai_l "NumeriCassa")
    (command "_insert" "numcassa" "_non" pevid 1 1 (ANGTOS peviang) (strcat tipcas (rtos ncas 2 0) ))
    (torna_l)
    (command"_group" "_c" "*" "Cassa" (entlast) ent1 "")
    )
  )

(defun c:tabcasse ()
  (setq gr (ssget "X" (list '(0 . "INSERT") '(2 . "intestazionecasse"))))
  (if gr (command "_erase" gr ""))
  (setq gr (ssget "X" (list '(0 . "INSERT") '(2 . "rigacasse"))))
  (if gr (command "_erase" gr ""))
  (setq casse (carica "casse" modalita))
  (setq cabpan (carica "cabpan" modalita))
  (setq listacasse nil)
  (setq listapan nil)
  (foreach n casse
    (if (= (vl-filename-base (getvar "dwgname")) (nth 0 n))
      (setq listacasse (cons n listacasse))
      )
    )
  (foreach n cabpan
    (if (= (vl-filename-base (getvar "dwgname")) (nth 0 n))
      (setq listapan (cons n listapan))
      )
    )
  (setq pt (getpoint "\nPunto inserimento tabella:"))
  (foreach n listacasse
    (setq lpcc nil)
    (foreach p listapan
      (if (= (nth 1 n) (nth 11 p))
	(progn
	  (if (assoc (nth 1 p) lpcc)
	    (progn
	      (setq lpcc (subst (list (car (assoc (nth 1 p) lpcc)) (+ 1 (cadr (assoc (nth 1 p) lpcc)))) (assoc (nth 1 p) lpcc) lpcc))
	      )
	    (progn
	      (setq lpcc (cons (list (nth 1 p) 1) lpcc))
	      )
	    )
	  )
	)
      )
    (setq gr (ssadd))
    (command "_insert" "intestazionecasse" "_non" pt "1" "1" "0" (nth 1 n) (rtos (nth 4 n) 2 0) (nth 3 n)(rtos (nth 5 n) 2 0) )
    (setq gr (ssadd (entlast) gr))
    (setq pt1 pt)
    (foreach p lpcc
      (command "_insert" "rigacasse" "_non" pt1 "1" "1" "0" (nth 0 p) (nth 1 p))
      (setq gr (ssadd (entlast) gr))
      (setq pt1 (list (car pt1) (-  (cadr pt1) 119.7247)))
      )
    (command"_group" "_c" "*" "Cassa" gr "")
    (setq pt (polar pt 0 1400))
    )    
  )
  


(defun c:Carcassa_old ()

  (alert "carcassa")
  (setq parametri (carica "parametri" ""))
  ; definizione tipo casse
  (setq gr (ssget "X" (list '(0 . "INSERT") '(2 . "numcassa"))))
  (if gr (command "_erase" gr ""))
  (setq gr (ssget "x" (list '(8 . "tmp_selezione"))))
  (if gr (command "_erase" gr ""))
  (setvar "pickstyle" 1)
  (setq val (cdr (assoc 'lnomicas parametri)))
  (setq lnomicas nil)
  (foreach v val
    (setq lnomicas (cons (list (nth 0 v) (nth 1 v)) lnomicas))
    )
  (setq lnomicas (reverse lnomicas))
;;;  (setq lnomicas '(("LCA" 0) ("LCF" 0) ("LPF" 0)))
  (if (null tipcas) (setq tipcas (car (car lnomicas))))
  (setq ini "")
  (setq ini_ "")
  (setq iniev nil)
  (foreach n lnomicas
    (setq ini (strcat (car n) " " ini))
    (setq ini_ (strcat (car n) "/" ini_))
    (setq iniev (cons (car n) iniev))
    )
  (setq ini_ (substr ini_ 1 (- (strlen ini_)1)))
  (setq casdescri nil)  
  (initget ini)
  (setq tipcas_ (getkword (strcat "\nSelezionare tipo Cassa? [" ini_ "]<"tipcas">:")))
  (setq disegno (getvar "dwgname"))
  (cond
    ((= tipcas_ (car iniev)) (setq evidenzia "$evidenzia"))
    ((= tipcas_ (cadr iniev)) (setq evidenzia "$evidenziav"))
    ((= tipcas_ (caddr iniev)) (setq evidenzia "$evidenziar"))
    )    
  (setq disegno (vl-filename-base  disegno))
  (if tipcas_ (setq tipcas tipcas_))
  (setq cassacor (list tipcas))
  (if tipcas
    (setq casdescri (nth 2 (assoc tipcas val)))
;;;    (cond 
;;;      ((= tipcas "LCA")(setq casdescri "Cassa lato cabina"))
;;;	    ((= tipcas "LCF")(setq casdescri "Cassa lato cofano"))
;;;	    ((= tipcas "LPF")(setq casdescri "Cassa lato porta fuoco"))
;;;	  )
    )
    

; creo lista (lalf1) da selection set (lalf)
  
  (setq lalf (ssget "X" (list '(0 . "INSERT") '(2 . "codpas"))))
  (setq lalf1 nil)
  (setq nr -1)
  (while (and lalf (setq e (ssname lalf (setq nr (+ nr 1)))))
    (setq lalf1 (cons e lalf1))
    )

 ; setta variabili e acquisisce pt

  
  (setq listapancas (ssadd)) ; lista pannelli cassa
  (setq listcas nil) ; lista nomi casse per riscrizione casse.txt
  (setq casdiscor nil) ; lista completa casse per aggiunta a casse.txt
  (setq pancascorr nil) ; lista pannelli cassa corrente
  (setq ncasscorr 1) ; contatore
  (setq ncasscorr_ 1) ; contatore
  (setq npancascorr 0) ; contatore
  (setq pmaxc 10) ; numero max pannelli cassa
  (setq cambio t)
  (setq pt '(1 3))
  (SETQ NR 0)
  (SETQ POLD NIL)
  (SETQ GRDEL (SSADD))
  (setq listapannelli nil)
  (SETQ PALF_ NIL)
  (setq modo t)
  (getpoint "\nCliccare per iniziare la selezione:")
  (while (and (/= 2 (car pt)) (/= 255 (car pt)) (/= (LENGTH LALF1) (LENGTH PALF_)))

    (setq pt (grread t))
    (print pt)
    (if (null pold) (setq pold (cadr pt)))

; evento tasto DX cambio cassa
    
    (if (= '2 (car pt))
      (progn
;;;        (setq pt (grread nil))
;;;	    (print pt)
;;;
        (setq ini_1 ini_)(setq ini1 ini)
        (foreach c listcas (setq ini_1 (strcat ini_1 "/" (strcase (nth 1 c))) ) (setq ini1 (strcat ini1 " " (nth 1 c))))
        ;(initget ini1)
        (setq tipcas_ (getstring (strcat "\nSelezionare tipo Cassa? [" ini_1 "]<"tipcas">:")))
        (setq modo nil)
	(cond
	  ((= tipcas_ (car iniev)) (setq evidenzia "$evidenzia"))
	  ((= tipcas_ (cadr iniev)) (setq evidenzia "$evidenziav"))
	  ((= tipcas_ (caddr iniev)) (setq evidenzia "$evidenziar"))
	  )
        (if (and tipcas_ (/= 3 (strlen tipcas_)))
          (progn
            (setq vv t)
            (foreach ccc listcas
              (if (= casdesc (nth 1 ccc))
                (setq vv nil)
              )
            )
            (if vv (setq listcas (cons (list disegno casdesc tipcas casdescri ncasscorr) listcas)))
            (setq casdiscor (cons (list casdesc disegno tipcas casdescri ncasscorr npancascorr) casdiscor))
            (setq casdesc tipcas_)
            (setq cambio nil)
            (foreach cc casdiscor
              (if (= tipcas_ (nth 0 cc))
                (progn
                  (setq tipcas (nth 2 cc))
                  (setq casdescri (nth 3 cc))
                  (setq ncasscorr (nth 4 cc))
                  (setq npancascorr (nth 5 cc))
                  ;(alert (strcat "Cassa corrente: " tipcas))
                  (if (>= npancascorr pmaxc)
                    (alert "Numero massimo di pannelli per cassa raggiunto")
                  )
                )
              )
            )
          )
          (setq pt (grread t))
	  
        )
        (if (and tipcas_ (= 3 (strlen tipcas_)))
          (progn
            (setq pt (grread nil))
            (setq vv t)
            (foreach ccc listcas
              (if (= casdesc (nth 1 ccc))
                (setq vv nil))
            )
            (if vv (setq listcas (cons (list disegno casdesc tipcas casdescri ncasscorr) listcas)))
            (setq casdiscor (cons (list casdesc disegno tipcas casdescri ncasscorr npancascorr) casdiscor))
            (setq npancascorr 1)
            (setq ncasscorr (+ 1 ncasscorr))
            (setq cambio t)
            (setq tipcas tipcas_)
            (setq casdescri (nth 2 (assoc tipcas val)))
            (setq cassacor (list tipcas))
            ;(alert (strcat "Tipo cassa corrente: " tipcas))
          )
          (setq pt (grread t))
        )
      )
    )
    ; evento tasto SX cambio modo selezione
    (if (= '3 (car pt))
      (progn
	(setq pold nil)
        (if modo
          (setq modo nil)
          (setq modo t)
        )
      )
    )
    ; modo selezione attivo
    (IF (= '5 (CAR PT))
      (if modo
        (PROGN
          (SETQ P (CADR PT))
          (IF (AND POLD (< 10 (DISTANCE P POLD)))
            (PROGN
              (vai_l "tmp_selezione")
              (COMMAND "_LINE" "_non" POLD "_non" P "")
              (torna_l)
              (SETQ GRDEL (SSADD (ENTLAST) GRDEL))
;;;              (setq gr (ssget "_F" (LIST P POLD p) (list '(0 . "INSERT") '(2 . "codpas"))))
              (setq gr (ssget "_CP" (LIST (polar P (- (angle p pold) (/ 2 pi)) 120)
					 (polar pold (- (angle p pold) (/ 2 pi)) 120)
					 (polar pold (+ (angle p pold) (/ 2 pi)) 1200)
					 (polar P (+ (angle p pold) (/ 2 pi)) 120)) (list '(0 . "INSERT") '(2 . "codpas"))))
              (IF GR
                (PROGN
		  (setq nrk -1)
                  (while (SETQ ENT (SSNAME GR (setq nrk (+ 1 nrk))))
                  (IF (OR (NULL PALF_) (NULL (MEMBER ENT PALF_)))
                    (PROGN
                      (SETQ PALF_ (CONS ENT PALF_))
                      (setq npancascorr (+ 1 npancascorr))
                      (Setq ent1 (entget ent))
                      (setq pevid (list (cadr (assoc '10 ent1)) (caddr (assoc '10 ent1)) (cadddr (assoc '10 ent1))))
                      (setq peviang (cdr (assoc '50 ent1)))
                      (vai_l "tmp_selezione")
                      (command "_insert" evidenzia "_non" pevid 1 1 (ANGTOS peviang))
                      (torna_l)
		      (vai_l "NumeriCassa")
                      (command "_insert" "numcassa" "_non" pevid 1 1 (ANGTOS peviang) (rtos ncasscorr 2 0))
		      (torna_l)
                      (setq GRDEL (SSADD (ENTLAST) GRDEL))
                      (setq nn (entget (entnext ent)))
                      (setq codpancor (cdr (assoc '1 nn)))
                      (setq mm (entget (entnext (entnext ent))))
                      (setq progpancor(cdr (assoc '1 mm)))
                      (if cambio (setq casdesc (strcat (strcase disegno) ""(rtos ncasscorr) "" tipcas)))
                      (setq pancascorr (cons (list disegno codpancor 1 progpancor "NULL" "NULL" "NULL" "NULL" "NULL" "NULL" "NULL" casdesc)pancascorr))
                      (setq listapannelli (cons codpancor listapannelli))
                      (if (and cambio (>= npancascorr pmaxc))
                        (progn
                          (setq ncasscorr_ (+ 1 ncasscorr_))
                          (prompt (strcat "\nCassa n. "(rtos ncasscorr_ 2 0)))
                          (if (> ncasscorr_ ncasscorr)
                            (progn
                              (setq vv t)
                              (foreach ccc listcas
                                (if (= casdesc (nth 1 ccc))
                                  (setq vv nil)))
                              (if vv (setq listcas (cons (list disegno casdesc tipcas casdescri ncasscorr) listcas)))
                              (setq casdiscor (cons (list casdesc disegno tipcas casdescri ncasscorr npancascorr) casdiscor))
                              (setq ncasscorr (+ ncasscorr 1))
                              (setq npancascorr 0)
                            )
                          )
                        )
                      )
                    )
                  )
		    )
                )
              )
              (SETQ POLD P)
            )
          )
        )
        (progn
          (SETQ P (CADR PT))
        )
      )
    )
  )
  ; caso di selezione completa
    
  (if (= (LENGTH LALF1) (LENGTH PALF_))
    (Progn
      (Alert "Tutti i pannelli sono nelle casse")
      (setq vv t)
      (foreach ccc listcas
        (if (= casdesc (nth 1 ccc))
          (setq vv nil)))
      (if vv (setq listcas (cons (list disegno casdesc tipcas casdescri ncasscorr) listcas)))
      (setq casdiscor (cons (list casdesc disegno tipcas casdescri ncasscorr npancascorr) casdiscor))
    )
  )
  ; caso uscita a selezione incompleta
  (IF (/= (LENGTH LALF1) (LENGTH PALF_))
    (ALERT "Non sono stati selezionati tutti le posizioni !")
  )
  (COMMAND "_ERASE" GRDEL "")
  
  ; verifica numero massimo in casse

  (if listcas
    (foreach m listcas
      (setq nump 0)
      (foreach mn pancascorr
        (if (= (nth 1 m) (nth 11 mn))
          (setq nump (+ 1 nump)))
      )
      (if (> nump pmaxc)
        (progn
          (setq ccor (nth 1 m))
          (alert (strcat "\nAttenzione: numero pannelli eccedente in cassa " ccor))
        )
      )
    )
  )
  ; regirtrazione cabpan e casse  
  (initget "Si No")
  (setq regsn (getkword (strcat "\nProcedere alla registrazione? [Si/No]<Si>")))
  (if (/= regsn "No")
    (progn
      (setq cabina (getvar "dwgname"))
      (setq cabina (substr cabina 1 (- (strlen cabina) 4)))
      (SETQ CABINA (SUBSTR CABINA 1 10))
      (setq revisione (substr (getvar "dwgname") 16 1))
      (setq lista nil)
      (setq grpn (ssget "x" (list '(0 . "INSERT") '(2 . "codpas"))))
      (setq lista nil)
      (SETQ LISTA_P NIL)
      ; Creo copia bak e riscrivo cabpan
      (vl-file-delete (strcat modalita "cabpan_.bak"))
      (vl-file-copy  (strcat modalita "cabpan.txt")  (strcat modalita "cabpan_.bak"))
      (setq filer (S_OPEN (strcat modalita "cabpan_.bak") "r"))
      (setq filew (S_OPEN (strcat modalita "cabpan.txt") "w"))
      (while (setq rig (read-line filer))
        (if (/= cabina (car (read (strcat "(" rig ")"))))
          (progn
            (write-line rig filew)
          )
          (progn
            (if (null (member (nth 1 (read (strcat "(" rig ")"))) listapannelli))
              (setq pancascorr (cons (read (strcat "(" rig ")")) pancascorr))
            )
          )
        )
      )
      (s_close filer (strcat modalita "cabpan_.bak"))
      (s_close filew (strcat modalita "cabpan.txt"))
      (setq filew (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (if pancascorr
        (foreach m (reverse pancascorr)
          (setq nn 0)
          (setq mm nil)
          (repeat 12
            (if (nth nn m) (setq mm (reverse (cons (nth nn m) (reverse mm))))
              (setq mm (reverse (cons "NULL" (reverse mm))))
            )
            (setq nn (+ 1 nn))
          )
          (setq m mm)
          (setq rig (strcat "\"" (nth 0 m) "\"\t\"" (nth 1 m) "\"\t" (rtos (nth 2 m) 2 0) "\t\""  (nth 3 m) "\"\t\""  (nth 4 m) "\"\t\"" (nth 5 m) "\"\t\"" (nth 6 m) "\"\t\"" (nth 7 m) "\"\t\"" (nth 8 m) "\"\t\"" (nth 9 m) "\"\t\"" (nth 10 m) "\"\t\""	(nth 11 m) "\""))
          (write-line rig filew)
        )
      )
      (s_close filew (strcat modalita "cabpan.txt"))
      ; Creo copia bak e riscrivo casse vedi sopra
      (vl-file-delete (strcat modalita "casse_.bak"))
      (vl-file-copy  (strcat modalita "casse.txt")  (strcat modalita "casse_.bak"))
      (setq filer (S_OPEN (strcat modalita "casse_.bak") "r"))
      (setq filew (S_OPEN (strcat modalita "casse.txt") "w"))
      (while (setq rig (read-line filer))
        (setq cfr (car (read (strcat "(" rig ")"))))
        (if (/= cabina cfr)
          (progn
            (write-line rig filew)
          )
        )
      )
      (s_close filer (strcat modalita "casse_.bak"))
      (s_close filew (strcat modalita "casse.txt"))
      (setq filew (S_OPEN (strcat modalita "casse.txt") "a"))
      (if listcas
        (foreach m listcas
          (setq nump 0)
          (foreach mn pancascorr
            (if (= (nth 1 m) (nth 11 mn))
              (setq nump (+ 1 nump)))
          )
          (setq rig (strcat "\"" (nth 0 m) "\"\t\"" (nth 1 m) "\"\t\"" (nth 2 m) "\"\t\"" (nth 3 m) "\"\t\"" (rtos (nth 4 m)) "\"\t\"" (rtos nump) "\""))
          (write-line rig filew)
        )
      )
      (s_close filew (strcat modalita "casse.txt"))
    )
  )
)