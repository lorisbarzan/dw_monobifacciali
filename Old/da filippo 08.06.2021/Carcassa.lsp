(defun c:Carcassa ()

; definizione tipo casse

  (setq gr (ssget "x" (list '(8 . "tmp_selezione"))))
  (if gr (command "_erase" gr ""))
  (setvar "pickstyle" 1)
  (setq lnomicas '(("LCA" 0) ("LCF" 0) ("LPF" 0)))
  (if (null tipcas) (setq tipcas (car (car lnomicas))))
  (setq ini "")
  (setq ini_ "")
  (foreach n lnomicas
    (setq ini (strcat (car n) " " ini))
    (setq ini_ (strcat (car n) "/" ini_))
    )
  (setq ini_ (substr ini_ 1 (- (strlen ini_)1)))
  (setq casdescri nil)  
  (initget ini)
  (setq tipcas_ (getkword (strcat "\nSelezionare tipo Cassa? [" ini_ "]<"tipcas">:")))
  (setq disegno (strcase (getvar "dwgname")))
  (setq disegno (substr disegno 1 (- (strlen disegno) 4)))
  (if tipcas_ (setq tipcas tipcas_))
  (setq cassacor (list tipcas))
  (if tipcas
    (cond ((= tipcas "LCA")(setq casdescri "Cassa lato cabina"))
	  ((= tipcas "LCF")(setq casdescri "Cassa lato cofano"))
	  ((= tipcas "LPF")(setq casdescri "Cassa lato porta fuoco"))
	  )
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
    (if (null pold) (setq pold (cadr pt)))

; evento tasto DX cambio cassa
    
    (if (= '25 (car pt))
      (progn
	(setq ini_1 ini_)(setq ini1 ini)
	(foreach c listcas (setq ini_1 (strcat ini_1 "/" (strcase (nth 1 c))) ) (setq ini1 (strcat ini1 " " (nth 1 c))))
	;(initget ini1)
	(setq tipcas_ (getstring (strcat "\nSelezionare tipo Cassa? [" ini_1 "]<"tipcas">:")))
	(setq modo nil)
	(if (and tipcas_ (/= 3 (strlen tipcas_)))
	  (progn
	    (setq vv t)
	    (foreach ccc listcas
	      (if (= casdesc (nth 1 ccc))
		(setq vv nil)))
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
		  (alert (strcat "Cassa corrente: " tipcas))
		  (if (>= npancascorr pmaxc)
		    (alert "Numero massimo di pannelli per cassa raggiunto"))
		  )
		)
	      )
	    )
	  )
	(if (and tipcas_ (= 3 (strlen tipcas_)))
	  (progn
	    (setq vv t)
	    (foreach ccc listcas
	      (if (= casdesc (nth 1 ccc))
		(setq vv nil)))
	    (if vv (setq listcas (cons (list disegno casdesc tipcas casdescri ncasscorr) listcas)))
	    (setq casdiscor (cons (list casdesc disegno tipcas casdescri ncasscorr npancascorr) casdiscor))
	    (setq npancascorr 0)
	    (setq ncasscorr (+ 1 ncasscorr))
	    (setq tipcas tipcas_)
	    (cond
		((= tipcas "LCA")(setq casdescri "Cassa lato cabina"))
		((= tipcas "LCF")(setq casdescri "Cassa lato cofano"))
		((= tipcas "LPF")(setq casdescri "Cassa lato porta fuoco"))
		)
	    (setq cassacor (list tipcas))
	    (setq cambio t)
	    (alert (strcat "Tipo cassa corrente: " tipcas))
	    )
	  )
	)
      )

; evento tasto SX cambio modo selezione
    
    (if (= '3 (car pt))
      (progn
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
	(IF (AND POLD (< 30 (DISTANCE P POLD)))
	  (PROGN
	    (vai_l "tmp_selezione")
	    (COMMAND "_LINE" "_non" POLD "_non" P "")
	    (torna_l)
	    (SETQ GRDEL (SSADD (ENTLAST) GRDEL))
	    (setq gr (ssget "_F" (LIST P POLD) (list '(0 . "INSERT") '(2 . "codpas"))))
            (IF GR
              (PROGN
		(SETQ ENT (SSNAME GR 0))
		(IF (OR (NULL PALF_) (NULL (MEMBER ENT PALF_)))
		   (PROGN
		     (SETQ PALF_ (CONS ENT PALF_))
		     (setq npancascorr (+ 1 npancascorr))
		     (Setq ent1 (entget ent))
		     (setq pevid (list (cadr (assoc '10 ent1)) (caddr (assoc '10 ent1)) (cadddr (assoc '10 ent1))))
		     (setq peviang (cdr (assoc '50 ent1)))
		     (vai_l "tmp_selezione")
		     (command "_insert" "$evidenzia" "_non" pevid 1 1 (ANGTOS peviang))
		     (torna_l)
		     (setq GRDEL (SSADD (ENTLAST) GRDEL))
		     (setq nn (entget (entnext ent)))
		     (setq codpancor (cdr (assoc '1 nn)))
		     (setq mm (entget (entnext (entnext ent))))
		     (setq progpancor(cdr (assoc '1 mm)))
		     (if cambio (setq casdesc (strcat disegno ""(rtos ncasscorr) "" tipcas)))
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
	    (setq nump (+ 1 nump))))
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
	(setq rig (strcat "\"" (nth 0 m) "\"\t\"" (nth 1 m) "\"\t" (rtos (nth 2 m) 2 0) "\t\""  (nth 3 m)
			  "\"\t\""  (nth 4 m) "\"\t\"" (nth 5 m) "\"\t\"" (nth 6 m) "\"\t\""
			  (nth 7 m) "\"\t\"" (nth 8 m) "\"\t\"" (nth 9 m) "\"\t\"" (nth 10 m) "\"\t\""
			  (nth 11 m) "\""))
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
	    (setq nump (+ 1 nump))))	
	(setq rig (strcat "\"" (nth 0 m) "\"\t\"" (nth 1 m) "\"\t\"" (nth 2 m) "\"\t\"" (nth 3 m) "\"\t\"" (rtos (nth 4 m)) "\"\t\"" (rtos nump) "\""))
	(write-line rig filew)
	)
      )
      
   (s_close filew (strcat modalita "casse.txt"))
  )
)
  )