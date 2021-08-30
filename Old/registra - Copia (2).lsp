;Rev 2020
(defun carica_F+ (nfile perc)
  (if (findfile (strcat perc "$$" nfile ".txt"))
    (setq tbx (carica+ (strcat "$$" nfile) perc))
    (setq tbx (carica+ nfile perc))
    )
  tbx
  )
(defun c:regtmp ()
 
  (setq parametri (carica "parametri" ""))
  
;;;  (setq nfilea (strcat (cadr (assoc 'percregtmp parametri)) "anagraf.txt"))
;;;  (setq nfiled (strcat (cadr (assoc 'percregtmp parametri)) "distbase.txt"))
  (IF (/= MODALITA (cadr (assoc 'percregtmp parametri))) (C:ANNTAB))
  (setq modalita (cadr (assoc 'percregtmp parametri)))
  (registra)
)
(defun c:registra ()
;;;  (start)
;;;  (c:ver_lic)
;;;  (if (null (verkit)) (exit))
(setq lrig-nomi nil)

  ;(solalet (cadr (assoc 'percregdef parametri)))
  (setq parametri (carica "parametri" ""))
  (initget "Si No")
  (if (= "Si" (getkword "\nVuoi procedere con la registrazione definitiva [Si/No] <No>:"))
    (progn
;;;      (setq nfilea (cadr (assoc 'fileanagrafica parametri)))
;;;      (setq nfiled (cadr (assoc 'filedistbase parametri)))
;;;      (setq nfiled6 (strcat (cadr (assoc 'filedistbase parametri)) "6"))
;;;      (elim_cab_da_gaia)
      (IF (/= MODALITA (cadr (assoc 'percregdef parametri))) (C:ANNTAB))
      (setq modalita (cadr (assoc 'percregdef parametri)))
;;;      (if (= "No" (c:verfiltro)) (progn (alert "Filtro non Valido - Mancano altezze o finiture - All'uscita l'elenco delle altezze e finiture neccessarie !!")
;;;				   (print &lish) (print &lisf)
;;;				   (exit)
;;;				   ))
;;;
      (foreach n '("cabine" "cabpan" "cesoiati" "dibces"  "DIBLDR" "dibpan" "dibpas" "dibrin"  "DIBsvi" "mater" "pannelli"
		   "pannellil" "sviluppi" "tabrinf"
		   )
          (vl-file-delete (strcat modalita n ".bak"))
          (if (null (vl-file-copy (strcat modalita n ".txt") (strcat modalita n ".bak")))
	    (progn (alert (strcat "File " n " Non accessibile")) (exit))
	  )
      )
      (vl-file-delete
	(strcat (vl-filename-directory (cadr (assoc 'fileanagrafica parametri)))"/"(vl-filename-base (cadr (assoc 'fileanagrafica parametri)))".bak")
      )
		  
;;;      (if (null (vl-file-copy (cadr (assoc 'fileanagrafica parametri))
;;;	           (strcat (vl-filename-directory (cadr (assoc 'fileanagrafica parametri)))"/"(vl-filename-base (cadr (assoc 'fileanagrafica parametri)))".bak")
;;;                )
;;;	  )
;;;	  (progn (alert "File Anagrafica non accessibile") (exit))
;;;      )
;;;      (vl-file-delete
;;;	(strcat (vl-filename-directory (cadr (assoc 'filedistbase parametri)))"/"(vl-filename-base (cadr (assoc 'filedistbase parametri)))".bak")
;;;      )		  
;;;      (if (null (vl-file-copy (cadr (assoc 'filedistbase parametri))
;;;	           (strcat (vl-filename-directory (cadr (assoc 'filedistbase parametri)))"/"(vl-filename-base (cadr (assoc 'filedistbase parametri)))".bak")
;;;                )
;;;	  )
;;;	  (progn (alert "File Distbase non accessibile")(exit))
;;;      )
      (registra)
    )
  )
  (stop)
)
(defun S_OPEN (nome modo)
  (setq nff (findfile nome))
  (setq carnr 0)
  (while (null nff)
     (setq carnr (+ 1 carnr))
     (p_alert (strcat "Attesa scrittura prolungata " nome " !!!!"))
     (setq nff (findfile nome))
  )
  (print (strcat "attesa scrittura " nome " : " (rtos carnr 2 0)))
  (setq fperc (strcat (vl-filename-directory nff) "\\"))
  (setq fext (vl-filename-extension nff))
  (setq fbase (vl-filename-base nff))
  (while (null (vl-file-rename (strcat fperc fbase fext) (strcat fperc fbase ".tx_")))
      (p_alert (strcat "Attesa apertura " nome))
  )
  (while (null (findfile (strcat fperc fbase ".tx_"))))
  (open (strcat fperc fbase ".tx_") modo)
)
(DEFUN REGISTRA ()
    ;(start)
  (setq pregd (selezpar 'profrdoga "Selezionare Profilo reggidoga: "))
  (setq pprer (selezpar 'profiloper "Selezionare Profilo Perimetrale: "))
  (setq pprer1 (selezpar 'profiloper1 "Selezionare Profilo Perimetrale: "))
    (setq progiu (selezpar 'prof_giu_ele "Selezionare Profilo prof_giu_ele :"))
	  (setq profelet (selezpar 'Prof_elet "Selezionare Profilo Prof_elet :"))

  (command"_layer" "_t" "*" "")
  (command"_regen")
  (SETVAR"TILEMODE" 1)
  (COMMAND"_ZOOM" "_E")
;;;  (setq seltravinf (selezpar 'traversinf "traversina inferiore")) 
;;;  (setq seltravsup (selezpar 'traverssup "traversina Superiore"))
;;;  (setq selrinfpiede (selezpar 'codrinfpiedep "Rinforzo piede"))
;;;  (setq selvelovetro (selezpar 'velovetro "Velovetro"))
;;;  (setq sellanacer (selezpar 'lanaceramica "Lana Ceramica"))  
;;;  (setq revisione (getstring "\nRevisione nr [.00/.01/.02/.03/.04] <.00>:"))
;;;  (if (= "" revisione) (setq revisione ".00"))
  (setq revisione ".00")
  ;(setq filea (S_OPEN nfilea "a"))
  ;(setq filed (S_OPEN nfiled "a"))
  (setq orainizio (getvar"cdate"))
  (C:REGPAN)
  ;(C:REGKIT)
  (c:regcab_p)
  (if filea (s_close filea nfilea))
  (if filed (s_close filed nfiled))
  (setq orafine (getvar"cdate"))
  (prompt (strcat "\nSecondi impiegati per registrazione :" (rtos (* 1000000 (- (atof (rtos orafine 2 6)) (atof (rtos orainizio 2 6)))) 2 1)))
  
)
(defun controlla_pan (gr / nr ent lispt)
  (setq nr 0)
  (setq nr -1)
  (while (and gr (setq ent (ssname gr (setq nr (+ 1 nr)))))
    (setq lispt nil)
    (setq nopan nil)
     (FOREACH N (ENTGET ent) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))) )
     (setq lispt (reverse lispt))
    (if (= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
      (progn
	(setq A1 (distance (nth 0 lispt) (nth 1 lispt)))
	(SETQ MODOP (LEGGI ENT "PANTIP"))
	(if (or (= modop "STD") (= modop "MTD") (= modop "SPS") (= modop "MPS"))
	  (if (or (> 60 a1) (< 600 a1)) (setq nopan "pannello 60>A1>600"))
	  (if (or (> 85 a1) (< 590 a1)) (setq nopan "pannello 85>A1>590"))
	  )
	)
      (progn
	(setq A1 (distance (nth 0 lispt) (nth 1 lispt)))
	(setq A2 (distance (nth 1 lispt) (nth 2 lispt)))
	(SETQ MODOP (LEGGI ENT "PANTIP"))
	(if (or (= modop "STD") (= modop "MTD") (= modop "SPS") (= modop "MPS"))
	  (PROGN
	     (if (or (< 600 (+ a1 A2))) (setq nopan "pannello A1+A2>600"))
	     (if (or (> 80 a1) (< 430 a1)) (setq nopan "pannello 80>A1>430"))
	     (if (or (> 80 a2) (< 430 a2)) (setq nopan "pannello 80>A2>430"))
	    )
	  (PROGN
	     (if (or  (< 590 (+ a1 A2))) (setq nopan  "pannello A1+A2>590"))
	     (if (or (> 80 a1) (< 430 a1)) (setq nopan "pannello 80>A1>430"))
	     (if (or (> 80 a2) (< 430 a2)) (setq nopan "pannello 80>A2>430"))

	    )
	  )
	)
      )
    
    (IF NOPAN
      (progn
	(command"_zoom" "_C" "_non" (polar (nth 1 lispt) (angle (nth 1 lispt) (nth 2 lispt)) (/ (distance (nth 1 lispt) (nth 2 lispt)) 2)) "800")
	(command"_circle" "_non" (polar (nth 1 lispt) (angle (nth 1 lispt) (nth 2 lispt)) (/ (distance (nth 1 lispt) (nth 2 lispt)) 2)) "200")
	(ALERT (strcat "Pannello fuori standard produttivi !! CASO " nopan )))
      )


    )
  )
(defun c:regpan ()
  ;(start)
  (setq grd (ssget "_x" (list '(8 . "siglepan"))))
  (if grd (command"_erase" grd ""))
  (command"_layer" "_of" "3dsimb" "")
  (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (controlla_pan gr)
  (COMMAND"_ZOOM" "_E" )
  (regpan)
  (stop)
)
(defun carica_F (nfile perc)
  (if (findfile (strcat perc "$$" nfile ".txt"))
    (setq tbx (carica (strcat "$$" nfile) perc))
    (setq tbx (carica nfile perc))
    )
  tbx
  )
(defun C:ripris ()
  (setq *error* nil)
  (if file (close file))
  (if filea (close filea))
  (if filed (close filed))
  (if filer (close filer))
  (if filew (close filew))
  (if filet (close filet))
  (setq file (open "tmp.bat" "w"))
  (write-line (strcat "rename " modalita "*.tx_ " "*.txt") file)
  (close file)
  (command"_sh" "tmp.bat")
)
(defun regpan () ;dopo
    (setq *error* nil)

  (setq Profl (selezpar 'ProfFissLana "Profilo fissaggio lana: "))
  (setq giuntopan (selezpar 'giuntopan "Giunto pannello: "))
  (SETQ NR 0)
  (IF (NULL lispan) (progn (setq lispan (carica_f "pannellil" modalita) lispn $tb$)))
  (IF (NULL DIBpan) (progn (setq DIBpan (carica_f "DIBpan" modalita) DIBpn $tb$)))
  (IF (NULL lispas) (progn (setq lispas (carica_F "pannelli" modalita) lisps $tb$)))
  (IF (NULL DIBpaS) (progn (setq DIBpaS (carica_f "DIBpaS" modalita) DIBpS $tb$)))
  (IF (NULL sviluppi) (progn (setq sviluppi (carica_F "sviluppi" modalita) svilupp $tb$)))
  (IF (NULL DIBsvi) (progn (setq DIBsvi (carica_F "DIBsvi" modalita) DIBsv $tb$)))
  (IF (NULL cesoiati) (setq cesoiati (carica "cesoiati" modalita) cesoiat $tb$))
  (IF (NULL lanadir) (setq lanadir (carica "lanadir" "") lanadi $tb$))
  (IF (NULL mater) (setq mater (carica "mater" modalita) mate $tb$))
  (IF (NULL tabrinfor) (setq tabrinfor (carica "tabrinf" modalita) tabrinfo $tb$))
  (IF (NULL DISTFORI) (SETQ DISTFORI (CARICA "DISTFORI" "")))
  (IF (NULL POSPON) (setq POSPON (carica "POSPONTI" "")))
  (IF (NULL RINFORZI) (setq RINFORZI (carica "RINFORZI" "")))

  (IF (NULL lispan_r) (progn (setq lispan_R (carica_f "pannellil_R" modalita) lispn_R $tb$)))
  (IF (NULL DIBpan_r) (progn (setq DIBpan_R (carica_f "DIBpan_R" modalita) DIBpn_R $tb$)))
  (IF (NULL sviluppi_R) (progn (setq sviluppi_R (carica_F "sviluppi_r" modalita) svilupp_R $tb$)))
  (IF (NULL DIBsvi_r) (progn (setq DIBsvi_R (carica_F "DIBsvi_R" modalita) DIBsv_R $tb$)))
  (IF (NULL cesoiati_r) (setq cesoiati_R (carica "cesoiati_R" modalita) cesoiat_R $tb$))

  (setq nbasepas (substr (car (nth 1 LISPAS)) 1 3))
  (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (WHILE (SETQ ENT (SSNAME GR NR))
     (setq listap nil)
     (setq listap_r nil)
     (setq listapr nil)
     (setq listaps nil)
    (setq LISTAPpR nil)
    (setq LISTAPp nil)
     (setq codrp (cdr (assoc '5 (entget ent))))
     (SETQ LISPT NIL)
     (FOREACH N (ENTGET ent)
        (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT)))
     )
     (setq lispt (reverse lispt))
    (setq lispt_or lispt)
     (setq lung (distance (car lispt) (nth 1 lispt)))
     (if (= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
       (setq angpan 0 lung2 0 SPES (distance (nth 1 lispt) (nth 2 lispt)) cart_ang "")
       (progn
	 (command"_dimangular" "" "_non" (nth 1 lispt) "_non" (nth 0 lispt) "_non" (nth 2 lispt) "_non" (nth 2 lispt))
	 (setq angpan (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))
	 (setq lung2 (distance (nth 1 lispt) (nth 2 lispt)) SPES (distance (nth 2 lispt) (nth 3 lispt)))
	 (setq cart_ang "P")
       )
     )
     (SETQ HALT (CDR (ASSOC '39 (ENTGET ENT))))
;sviluppi
     (setq finit (leggi ent "PANFIN"))
    (if (null finit) (setq finit "A"))
     (SETQ RESIS (LEGGI ENT "PANRES"))    
     (SETQ MODOP (LEGGI ENT "PANTIP"))
     (SETQ MODOP_ NIL)
    
     (IF (NULL SVILPAN) (setq svilpan (carica "svilpan" "")))
     (if (null modop_) (setq modop_ modop))
     (setq hpan halt)
    (SETQ LUNGLAM NIL)
     (foreach m svilpan
       (if (and (= resis (nth 2 m)) (= modop_ (car m)) (= finit (cadr m)))
	 (progn
	   (setq halt_f (+ halt (nth 5 m) (nth 6 m)))
	   (setq lunglam (+ lung lung2 (nth 3 m) (nth 4 m)))
	   (setq halt_f_r (+ halt (nth 9 m) (nth 10 m)))
	   (setq lunglam_r (+ lung lung2 (nth 7 m) (nth 8 m)))
	   (setq svilcorr m)
	   (setq lung_r (+ lung (nth 7 m)))
	   (setq lung2_r (+ lung2 (nth 8 m)))
	   )
	 )
     )
     (if (= modop_ "MAN")
       (progn
	 (setq rigeman (read (setq rigemant (leggi ent "SVILMAN"))))
	 (setq halt_f (+ halt (atof (rtos (nth 2 rigeman) 2 1))  (atof (rtos (nth 3 rigeman) 2 1))))
	 (setq lunglam (+ lung lung2 (atof (rtos (nth 0 rigeman) 2 1)) (atof (rtos (nth 1 rigeman) 2 1))))
	 )
       )
      
           (SETQ FORISN "No")
      (command"_layer" "_off" "Hforo" "_off" "testo" "")

     (setq grf (ssget "_f" (list (nth 0 lispt) (nth 1 lispt) (nth 2 lispt)) (list '(0 . "INSERT") '(2 . "FORO*"))))
        (command"_layer" "_on" "Hforo" "_on" "testo" "")
     (if grf (setq forisn "Si") (setq forisn "No"))
     (SETQ MODOP (LEGGI ENT "PANTIP"))
    (setq modop_ modop)
    (SETQ Pconf (LEGGI ENT "TIPOMB"))
    (if (/= resis  "B0") (setq resisk "B15") (setq resisk "B0"))
    (foreach lanaT lanadir
      (if (and (= pconf (nth 0 lanat)) (= forisn (nth 1 lanat)) (= resisk (nth 2 lanaT)))
	(setq len_m lanat)
	)
      )
    (if (null len_m) (progn (alert "Riga mancante in lanadir.txt!!") (exit)))
    
    (setq deskldr (nth 5 len_m))
    (setq spmnan (nth 3 len_m))
    (SETQ COD_M (NTH 4 LEN_M))
    (if (/= angpan 0)
      (progn
         (setq lunglam_r (+ lunglam_r (* 2 (* spmnan (sin (* (/ angpan 180.0) pi))))))
	 
         (setq lung_r (+ lung_r (* 1 (* spmnan (sin (* (/ angpan 180.0) pi))))))
         (setq lung2_r (+ lung2_r (* 1 (* spmnan (sin (* (/ angpan 180.0) pi))))))
	)
      )
    (SETQ LEN_F NIL)
     (if (null lunglam) (progn (alert "Riga mancante in Svilpan.txt.!!") (exit)))
;cesoaiato frontale
    (setq len_f (list 1 2 3 (rtos lunglam)))
    ;rinfogli
     (setq rig_f (list (strcat "Pannello cesoiato " ) (nth 11 svilcorr) (atof (rtos lunglam 2 1)) (atof (rtos halt_f 2 1)) (nth 13 svilcorr)
		       (atoi (rtos (* (atof (rtos lunglam 2 1)) (atof (rtos halt_F 2 1))) 2 0)) "Nr" ))
     (if (setq percod_F (member rig_f cesoiat))
       (progn
         (setq nomepan_f (car (nth (- (LENGTH cesoiat) (length percod_f)) cesoiati)))
       )  
       (progn
	 (setq file (S_OPEN (strcat modalita "cesoiati.txt") "a"))
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last cesoiati)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq n_base (substr (car (last cesoiati)) 1 3))
         (setq rig-F (strcat "\"" n_base newcod "\"\t\"Pannello cesoiato " "\"\t\"" (NTH 1 RIG_F)"\"\t" (rtos (NTH 2 RIG_f) 2 1)
			     "\t" (rtos (NTH 3 RIG_f)2 1) "\t" (RTOS (NTH 4 RIG_f) 2 1) "\t" (rtos (NTH 5 RIG_f) 2 0)
			     "\t\"" (NTH 6 RIG_f) "\"")
	 )
         (SETQ nomepan_F (strcat n_base newcod))
         (write-line rig-f file)
         (s_close file (strcat modalita "cesoiati.txt"))
         (setq cesoiati (REVERSE (CONS (READ (STRCAT "(" rig-f ")")) (REVERSE cesoiati))))
         (setq cesoiat (REVERSE (CONS (CDR (READ (STRCAT "(" rig-f ")"))) (REVERSE cesoiat))))
	 
         (if filea (write-line rig_A-f filea))	 ; (inquad rig_A-f)
         (setq file (S_OPEN (strcat modalita "DIBces.txt") "a"))
         (setq rig (strcat "\"" nomePAN_f "\"\t\"" (nth 11 svilcorr) "\"\t" (RTOS (NTH 2 rig_f) 2 0)
                              "\t" (RTOS (NTH 3 rig_f) 2 0) "\t" (RTOS (NTH 4 rig_f) 2 1) "\t" (RTOS (NTH 5 rig_f) 2 0)))
         (write-line rig file)
	 
         (s_close file (strcat modalita "DIBces.txt"))
       )
     )
     (setq listap (cons (list nomepan_f 0 0 1 0 0) listap))
;cesoaiato retro
    (if (= Pconf "Bifacciale")
    (progn
    (setq len_f_r (list 1 2 3 (rtos lunglam_r)))
    ;rinfogli
     (setq rig_f (list (strcat "Retro cesoiato " ) (nth 12 svilcorr) (atof (rtos lunglam_r 2 1)) (atof (rtos halt_f_R 2 1)) (nth 14 svilcorr)
		       (atoi (rtos (* (atof (rtos lunglam_r 2 1)) (atof (rtos halt_F_r 2 1))) 2 0)) "Nr" ))
     (if (setq percod_F_r (member rig_f cesoiat_R))
       (progn
         (setq nomepan_f_r (car (nth (- (LENGTH cesoiat_R) (length percod_f_R)) cesoiati_R)))
       )  
       (progn
	 (setq file (S_OPEN (strcat modalita "cesoiati_R.txt") "a"))
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last cesoiati_R)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq n_base (substr (car (last cesoiati_R)) 1 3))
         (setq rig-F (strcat "\"" n_base newcod "\"\t\"Retro cesoiato " "\"\t\"" (NTH 1 RIG_F)"\"\t" (rtos (NTH 2 RIG_f) 2 1)
			     "\t" (rtos (NTH 3 RIG_f)2 1) "\t" (RTOS (NTH 4 RIG_f) 2 1) "\t" (rtos (NTH 5 RIG_f) 2 0)
			     "\t\"" (NTH 6 RIG_f) "\"")
	 )
         (SETQ nomepan_F_r (strcat n_base newcod))
         (write-line rig-f file)
         (s_close file (strcat modalita "cesoiati_R.txt"))
         (setq cesoiati_R (REVERSE (CONS (READ (STRCAT "(" rig-f ")")) (REVERSE cesoiati_R))))
         (setq cesoiat_r (REVERSE (CONS (CDR (READ (STRCAT "(" rig-f ")"))) (REVERSE cesoiat_R))))
 
         (setq file (S_OPEN (strcat modalita "DIBces_r.txt") "a"))
         (setq rig (strcat "\"" nomePAN_f_r "\"\t\"" (nth 12 svilcorr) "\"\t" (RTOS (NTH 2 rig_f) 2 0)
                              "\t" (RTOS (NTH 3 rig_f) 2 0) "\t" (RTOS (NTH 4 rig_f) 2 1) "\t" (RTOS (NTH 5 rig_f) 2 0)))
         (write-line rig file)
	 
         (s_close file (strcat modalita "DIBces_R.txt"))
       )
     )
     (setq listap_r (cons (list nomepan_f_R 0 0 1 0 0) listap_r))
    )
    )
;fori
     (SETQ FORISN "No")
      (command"_layer" "_off" "Hforo" "_off" "testo" "")

     (setq grf (ssget "_f" (list (nth 0 lispt) (nth 1 lispt) (nth 2 lispt)) (list '(0 . "INSERT") '(2 . "FORO*"))))
        (command"_layer" "_on" "Hforo" "_on" "testo" "")

     (IF (NULL PRESE) (setq prese (carica "prese" "")))
     (setq cart_for "")
     (IF GRF
       (PROGN
	      (setq cart_for "F")

         (SETQ FORISN "Si")
	 (SETQ NRF 0)
  	 (WHILE (SETQ ENTF (SSNAME GRF NRF))
	    (SETQ NFOR (CDR (ASSOC '2 (ENTGET ENTF))))
	    (setq nfor (substr nfor 1 8))
	    (if (> lung (DISTANCE (NTH 0 LISPT) (CDR (ASSOC '10 (ENTGET ENTF)))))
	      (SETQ XFOR (ATOF (RTOS (DISTANCE (NTH 0 LISPT) (CDR (ASSOC '10 (ENTGET ENTF)))) 2 1)))
	      (SETQ XFOR (ATOF (RTOS
				 (+ (DISTANCE (NTH 0 LISPT) (NTH 1 LISPT))
				    (DISTANCE (NTH 1 LISPT) (CDR (ASSOC '10 (ENTGET ENTF))))
				 )
				 2 1;massimofori
				)
			  )
      	      )
	    )
	    (SETQ YFOR (ATOF (CDR (ASSOC '1 (ENTGET (ENTNEXT ENTF))))))
	    (SETQ DFOR "Scatola portafrutti")
	    (SETQ NRF (+ 1 NRF))
           (if (/= "No scatola" (NTH 5 (ASSOC NFOR PRESE))) (SETQ scatoleSN "Si"))
	    (SETQ LFOR (LIST "000000000" (NTH 5 (ASSOC NFOR PRESE)) (NTH 6 (ASSOC NFOR PRESE)) 1
			       (ATOF (RTOS (+ (nth 3 svilcorr) XFOR) 2 1))
			       (ATOF (RTOS (+ 0 YFOR) 2 1))))
            (SETQ LFOR_R (LIST "000000000" (NTH 8 (ASSOC NFOR PRESE)) (NTH 9 (ASSOC NFOR PRESE)) 1
			     (ATOF (RTOS (+ (nth 5 svilcorr) (nth 7 svilcorr) XFOR) 2 1))
			     (ATOF (RTOS (+ (nth 9 svilcorr) YFOR) 2 1))))
            (SETQ LISTAP (CONS LFOR LISTAP))
            (SETQ LISTAP_R (CONS LFOR_R LISTAP_R))
	   	    (SETQ LFORp (LIST "000000000" (NTH 5 (ASSOC NFOR PRESE)) (NTH 6 (ASSOC NFOR PRESE)) 1
			       (ATOF (RTOS (+ 0 XFOR) 2 1))
			       (ATOF (RTOS (+ 0 YFOR) 2 1))))
            (SETQ LFOR_pR (LIST "000000000" (NTH 8 (ASSOC NFOR PRESE)) (NTH 9 (ASSOC NFOR PRESE)) 1
			     (ATOF (RTOS (+ 0  (nth 7 svilcorr) XFOR) 2 1))
			     (ATOF (RTOS (+ 0 YFOR) 2 1))))
            (SETQ LISTAPp (CONS LFORp LISTAPp))
            (SETQ LISTAPpR (CONS LFOR_pR LISTAPpR))
	  )     
       )
       (SETQ FORISN "No")
     )
    (setq cart_rinf "")
;fine fori
;INIZIO RINFOEZI

;;;     (if (= filtrafori "Si")
;;;       (if (= forisn "Si") (setq lispan_& lispan_sf lispn_& lispn_sf dibpan_& dibpan_sf dibpn_& dibpn_sf
;;;			       lispas_& lispas_sf lisps_& lisps_sf dibpas_& dibpas_sf dibps_& dibpn_sf
;;;			       sviluppi_& sviluppi_sf svilupp_& svilupp_sf dibsvi_& dibsvi_sf dibsv_& dibsv_sf
;;;			       )
;;;                          (setq lispan_& lispan_nf lispn_& lispn_nf dibpan_& dibpan_nf dibpn_& dibpn_nf
;;;			       lispas_& lispas_nf lisps_& lisps_nf dibpas_& dibpas_nf dibps_& dibpn_nf
;;;			       sviluppi_& sviluppi_nf svilupp_& svilupp_nf dibsvi_& dibsvi_nf dibsv_& dibsv_nf
;;;			       )
;;;       )
;;;       (setq lispan_& lispan lispn_& lispn dibpan_& dibpan dibpn_& dibpn
;;;			       lispas_& lispas lisps_& lisps dibpas_& dibpas dibps_& dibpn
;;;			       sviluppi_& sviluppi svilupp_& svilupp dibsvi_& dibsvi dibsv_& dibsv
;;;			       )
;;;       )
     (setq rinfsn "No")
     (setq angr (+ (angle (nth 0 lispt) (nth 1 lispt)) (/ pi 2)))
     (setq angr1 (+ (angle (nth 1 lispt) (nth 2 lispt)) (/ pi 2)))
     (if (= 0 lung2)
         (setq grr (ssget "_f" (list (polar (nth 0 lispt) angr 1.5) (polar (nth 1 lispt) angr 1.5)) (list '(0 . "MLINE") '(8 . "rinforzi"))))
         (setq grr (ssget "_f" (list (polar (nth 0 lispt) angr 1.5) (polar (nth 1 lispt) angr 1.5) (polar (nth 2 lispt) angr1 1.5)) (list '(0 . "MLINE") '(8 . "rinforzi"))))
     )
       
     (IF GRr
       (PROGN
         (SETQ rinfsn "Si")
(setq cart_rinf "R")
	  (SETQ NRF 0)
	  (setq listar nil)
	  (WHILE (SETQ ENTF (SSNAME GRr NRF))
	    (SETQ Nrin (STRCAT "" (substr (cdr (assoc '2 (entget entf))) 10)))
	    (SETQ LISPTf NIL)
	    (FOREACH Nf (ENTGET entf)
	        (IF (= '11 (CAR Nf)) (SETQ LISPTf (CONS (CDR Nf) LISPTf)))
	    )
	    (setQ lisptf (reverse lisptf))
	    (setq lgrin (atoi (rtos (distance (nth 0 lisptf) (nth 1 lisptf)) 2 0)))
	    (setq drin (strcat "Rinforzo " Nrin))
	    (setq lrin (list nrin drin lgrin))
	    (if (> lung (DISTANCE (NTH 0 LISPT) (nth 0 lisptf)))
	      (SETQ Xrin (ATOI (RTOS (DISTANCE (NTH 0 LISPT) (nth 0 lisptf)) 2 0)))
	      (SETQ Xrin (ATOI (RTOS
				 (+ (DISTANCE (NTH 0 LISPT) (NTH 1 LISPT))
				    (DISTANCE (NTH 1 LISPT) (nth 0 lisptf))
				 )
				 2 0
				)
			  )
	      )
    	    )
    	    (setq listar nil)
	    (setq hrinforzo (leggi entf "HRIN"))
	    (if hrinforzo (setq hrinforzo (atoi hrinforzo)) (setq hrinforzo 80))
;reg rinforzi
            (setq rig_p (list "Rinforzo pannello" nrin lgrin hrinforzo "HOR" "Nr"))
	    
            (if (setq percod_p (member rig_p tabrinfo))
                (progn
                   (setq nomerin (car (nth (- (LENGTH tabrinfo) (length percod_p)) tabrinfor)))
                )  
                (progn
  	           (setq file (S_OPEN (strcat modalita "tabrinf.txt") "a"))
  	           (setq newcod (rtos (+ 1 (atoi (substr (car (last tabrinfor)) 4))) 2 0))
	           (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
		   (setq n_base (substr (car (last tabrinfor)) 1 3))
                   (setq rig-p (strcat "\"" n_base newcod "\"\t\"Rinforzo pannello\"\t\"" (NTH 1 RIG_p)"\"\t" (rtos (NTH 2 RIG_p) 2 1) "\t" (rtos  (NTH 3 RIG_p) 2 0) "\t\"HOR\"\t\"Nr\""))
                   (SETQ nomerin (strcat n_base newcod))
                   (IF REGP (SETQ RIG_A-p (strcat nomerin "|Rinforzo pannello|Nr.|" (rtos (nth 2 rig_p) 2 1) "|||Rinforzo|||||0|0||||"))
		     (SETQ RIG_A-p (strcat nomerin "|Rinforzo pannello|Nr.|" (rtos (nth 2 rig_p) 2 1) "|||Rinforzo|||||"))
		     )
                   (write-line rig-p file)
                   (s_close file (strcat modalita "tabrinf.txt"))
		   (setq TABRINFor (REVERSE (CONS (READ (STRCAT "(" RIG-P ")")) (REVERSE TABRINFor))))
                   (setq TABRINfo (REVERSE (CONS (CDR (READ (STRCAT "(" RIG-P ")"))) (REVERSE TABRINfo))))
		  
                   (if filea (write-line rig_A-p filea)) ;(inquad rig_A-p)
                   (setq file (S_OPEN (strcat modalita "DIBrin.txt") "a"))
                   (setq rig (strcat "\"" nomerin "\"\t\"" (NTH 1 rig_p) "\"\t" (rtos (nth 2 rig_p) 2 1)))
                   (write-line rig file)
                   (s_close file (strcat modalita "DIBrin.txt"))
	           (SETQ RIG_A (STRCAT NOMErin "|" (NTH 1 rig_P) "|" (RTOS (/ (atof (rtos (NTH 2 rig_P))) 1000) 2 2) "||"))
		   (SETQ RIG_A6 (STRCAT NOMErin "|" (NTH 1 rig_P) "|" (RTOS (/ (atof (rtos (NTH 2 rig_P))) 1000) 2 2) "||||"))
		   
                   (if filed (WRITE-LINE RIG_A FILED));ok
                   (if filed (WRITE-LINE RIG_A6 FILED6));ok
                )
             )
             (setq LISTAPs (cons (list NOMERIN 1 XRIN (atoi (rtos (caddr (car lisptf)) 2 0)) 0 0) LISTAPs))
	    
    	    (SETQ NRF (+ 1 NRF))
  	  )         
       )
       (SETQ rinfsn "No")
     )
;FINE RINFORZI



;INIZIO RINFORZI PUNTIFORMI$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
     (setq rfPANsn "No")

     (if (= 0 lung2)
         (setq grr (ssget "_f" (list (setq pll1 (polar (nth 0 lispt) angr 1.5)) (setq pll2 (polar (nth 1 lispt) angr 1.5))) (list '(0 . "INSERT") '(2 . "rinforzo-*"))))
         (setq grr (ssget "_f" (list (setq pll1 (polar (nth 0 lispt) angr 1.5)) (setq pll2 (polar (nth 1 lispt) angr 1.5)) (setq pll3 (polar (nth 1 lispt) angr1 2)) (setq pll4 (polar (nth 2 lispt) angr1 2)))
			  (list '(0 . "INSERT") '(2 . "rinforzo-**"))))
     )
 	 
       
     (IF GRr
       (PROGN
	 (setq cart_rinf "R")
         (SETQ rfPANsn "Si")
	  (SETQ NRF 0)
	  (setq listar nil)
	  (WHILE (SETQ ENTF (SSNAME GRr NRF))
	    (SETQ +nomer (cdr (assoc '2 (entget entf))))
	    (setq +ptrf (cdr (assoc '10(entget entf))))
;;;	    (setq yrin (cdr (assoc '1 (entget (entnext  entf)))))
	    (if (> lung (DISTANCE (NTH 0 LISPT) +ptrf))
	      (SETQ Xrin (ATOI (RTOS (DISTANCE (NTH 0 LISPT) +ptrf) 2 0)))
	      (SETQ Xrin (ATOI (RTOS
				 (+ (DISTANCE (NTH 0 LISPT) (NTH 1 LISPT))
				    (DISTANCE (NTH 1 LISPT) +ptrf)
				 )
				 2 0
				)
			  )
	      )
    	    )
	        	    (setq listar nil)
	    (setq hrinforzo (leggi entf "HRIN"))
	    (if hrinforzo (setq hrinforzo (atoi hrinforzo)) (setq hrinforzo 80))
;reg rinforzi
	    (setq nrin (substr +nomer 10))
	    (setq lgrin (- hpan (cadr (assoc 'diffaltrinv parametri))))
            (setq rig_p (list "Rinforzo pannello" nrin lgrin hrinforzo "VER" "Nr"))
	    
            (if (setq percod_p (member rig_p tabrinfo))
                (progn
                   (setq nomerin (car (nth (- (LENGTH tabrinfo) (length percod_p)) tabrinfor)))
                )  
                (progn
  	           (setq file (S_OPEN (strcat modalita "tabrinf.txt") "a"))
  	           (setq newcod (rtos (+ 1 (atoi (substr (car (last tabrinfor)) 4))) 2 0))
	           (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
		   (setq n_base (substr (car (last tabrinfor)) 1 3))
                   (setq rig-p (strcat "\"" n_base newcod "\"\t\"Rinforzo pannello\"\t\"" (NTH 1 RIG_p)"\"\t" (rtos (NTH 2 RIG_p) 2 1) "\t" (rtos  (NTH 3 RIG_p) 2 0) "\t\"VER\"\t\"Nr\""))
                   (SETQ nomerin (strcat n_base newcod))
                   (IF REGP (SETQ RIG_A-p (strcat nomerin "|Rinforzo pannello|Nr.|" (rtos (nth 2 rig_p) 2 1) "|||Rinforzo|||||0|0||||"))
		     (SETQ RIG_A-p (strcat nomerin "|Rinforzo pannello|Nr.|" (rtos (nth 2 rig_p) 2 1) "|||Rinforzo|||||"))
		     )
                   (write-line rig-p file)
                   (s_close file (strcat modalita "tabrinf.txt"))
		   (setq TABRINFor (REVERSE (CONS (READ (STRCAT "(" RIG-P ")")) (REVERSE TABRINFor))))
                   (setq TABRINfo (REVERSE (CONS (CDR (READ (STRCAT "(" RIG-P ")"))) (REVERSE TABRINfo))))
		  
                   (if filea (write-line rig_A-p filea)) ;(inquad rig_A-p)
                   (setq file (S_OPEN (strcat modalita "DIBrin.txt") "a"))
                   (setq rig (strcat "\"" nomerin "\"\t\"" (NTH 1 rig_p) "\"\t" (rtos (nth 2 rig_p) 2 1)))
                   (write-line rig file)
                   (s_close file (strcat modalita "DIBrin.txt"))
	           (SETQ RIG_A (STRCAT NOMErin "|" (NTH 1 rig_P) "|" (RTOS (/ (atof (rtos (NTH 2 rig_P))) 1000) 2 2) "||"))
		   (SETQ RIG_A6 (STRCAT NOMErin "|" (NTH 1 rig_P) "|" (RTOS (/ (atof (rtos (NTH 2 rig_P))) 1000) 2 2) "||||"))
		   
                   (if filed (WRITE-LINE RIG_A FILED));ok
                   (if filed (WRITE-LINE RIG_A6 FILED6));ok
                )
             )
	                 (setq LISTAPs (cons (list NOMERIN 1 XRIN (atoi (rtos (/ (cadr (assoc 'diffaltrinv parametri)) 2) 2 0)) 0 0) LISTAPs))

    	    (SETQ NRF (+ 1 NRF))
  	  )         
       )
       (SETQ rfPANsn "No")
     )
      
;FINE RINFORZI PUNTIFORMI$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
     (setq listapl nil)
     (setq finit (leggi ent "PANFIN"))
     (SETQ RESIS (LEGGI ENT "PANRES"))    
     (SETQ MODOP (LEGGI ENT "PANTIP"))
     (SETQ Pconf (LEGGI ENT "TIPOMB"))
     (setq modop_ modop)
     (if (/= 0 lung2) (progn (setq valsv (strcat (rtos angpan 2 0) " " (rtos (+ lung2 (nth 4 svilcorr)) 2 1)))) (setq valsv "-"))
     (if (= "MAN" modop_) (setq nn rigemant) (setq nn "NN"))
  ;sviluppo frontale
     (if (= "Si" (leggi ent "BLOCCOSVI"))
         (setq lrig (list (strcat "Sviluppo Pannello " ) (atof (rtos lunglam 2 1)) (nth 13 svilcorr)
		      (atof (rtos halt_F 2 1)) (nth 11 svilcorr) forisn valsv 1500 MODOP_ "Nr"))
         (setq lrig (list (strcat "Sviluppo Pannello " ) (atof (rtos lunglam 2 1)) (nth 13 svilcorr)
		      (atof (rtos halt_F 2 1)) (nth 11 svilcorr) forisn valsv (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
       )
    
     (setq newpan nil)
     (SETQ LIS1 NIL)
     (FOREACH PANDL sviluppi
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
    (IF (NULL LIS1) (SETQ NEWPAN "T"))
    (SETQ LIS LIS1)
    (if (AND LISTAP (null newPAN))
      (progn
	(SETQ LIS LIS1)
	(FOREACH PN (reverse LISTAP);sono solo fori
          (SETQ LISZ NIL)
	  (FOREACH N (reverse LIS  )
	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) (nth 11 svilcorr) forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (IF (MEMBER PK dibsvi) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
    (IF LIS (SETQ NEWPAN NIL NOMEPAN (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last sviluppi)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "sviluppi.txt") "a"))
	 

	 (setq n_base (substr (car (last sviluppi)) 1 3))
	 (if (= "Si" forisn) (setq dessvi "SvilFor") (setq dessvi "SvilnonFor"))
	 (if (= "Si" (leggi ent "BLOCCOSVI"))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Pannello " "\"\t" (rtos lunglam 2 1) "\t" (rtos (nth 13 svilcorr) 2 1) "\t"
			   (rtos halt_F 2 1) "\t\"" (nth 11 svilcorr) "\"\t\"" forisn "\"\t\"" valsv "\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
			   )
	       nomepan (strcat n_base newcod))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Pannello " "\"\t" (rtos lunglam 2 1) "\t" (rtos (nth 13 svilcorr) 2 1) "\t"
			   (rtos halt_F 2 1) "\t\"" (nth 11 svilcorr) "\"\t\"" forisn "\"\t\"" valsv "\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
			   )
	       nomepan (strcat n_base newcod))
	   )
	 
	 
 	 (setq $sviluppi (cons nomepan $sviluppi))

         (write-line rig file)
	 (if filet (write-line  rig filet))

         (s_close file (strcat modalita "sviluppi.txt"))
	 

		   (setq sviluppi (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi))))
                   (setq svilupp (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp))))
		   
         (if filea (write-line rig_A filea));	 (inquad rig_A)
         (setq file (S_OPEN (strcat modalita "dibsvi.txt") "a"))

 	 (foreach n LISTAP
           (setq rig_& (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" (nth 11 svilcorr) "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
	   (setq rig (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))
           (write-line rig_& file)
	   

	   	   (setq dibsvi (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE dibsvi))))
                   (setq dibsv (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv))))
	   	   (if (= filtrafori "Si") (progn (setq dibsvi_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE dibsvi_sf))))
                   (setq dibsv_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_sf))))
	   	   (setq dibsvi_nf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE dibsvi_nf))))
                   (setq dibsv_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_nf))))
					     ))

	 )
         (s_close file (strcat modalita "dibsvi.txt"))
	 
       )
    )
    (setq nomesvi nomepan)
  ;sviluppo retro
      (if (= Pconf "Bifacciale")
    (progn
           (if (/= 0 lung2_r) (progn (setq valsv (strcat (rtos angpan 2 0) " " (rtos lung2_r 2 1)))) (setq valsv "-"))


     (if (= "Si" (leggi ent "BLOCCOSVI"))
         (setq lrig (list (strcat "Sviluppo Retro " ) (atof (rtos lunglam_r 2 1)) (nth 14 svilcorr)
		      (atof (rtos halt_F_R 2 1)) (nth 12 svilcorr) forisn valsv 1500 MODOP_ "Nr"))
         (setq lrig (list (strcat "Sviluppo Retro " ) (atof (rtos lunglam_r 2 1)) (nth 14 svilcorr)
		      (atof (rtos halt_F_R 2 1)) (nth 12 svilcorr) forisn valsv (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
       )
    
     (setq newpan nil)
      (setq listappr_ listappr listappr nil)
      (foreach r LISTAPpR_
	(if (> (nth 4 r) (- lunglam_r lung2_r))
	  (setq listappr (cons (list (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r) (+ (nth 4 r) (* 2 spmnan (sin (* (/ angpan 180.0) pi)))) (nth 5 r)) listappr))
	  (setq listappr (cons r listappr))
	  )
	)
		
     (SETQ LIS1 NIL)
     (FOREACH PANDL sviluppi_R
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
    (IF (NULL LIS1) (SETQ NEWPAN "T"))
    (SETQ LIS LIS1)
    (if (AND LISTAPpR (null newPAN))
      (progn
	(SETQ LIS LIS1)
	(FOREACH PN (reverse LISTAPpR);sono solo fori
          (SETQ LISZ NIL)
	  (FOREACH N (reverse LIS  )
	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) (nth 12 svilcorr) forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (IF (MEMBER PK dibsvi_R) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
    (IF LIS (SETQ NEWPAN_r NIL NOMEPAN_R (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last sviluppi_R)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "sviluppi_R.txt") "a"))

	 (setq n_base (substr (car (last sviluppi_R)) 1 3))
	 (if (= "Si" forisn) (setq dessvi "SvilFor") (setq dessvi "SvilnonFor"))
	 (if (= "Si" (leggi ent "BLOCCOSVI"))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Retro " "\"\t" (rtos lunglam_r 2 1) "\t" (rtos (nth 14 svilcorr) 2 1) "\t"
			   (rtos halt_F_R 2 1) "\t\"" (nth 12 svilcorr) "\"\t\"" forisn "\"\t\"" valsv "\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
			   )
	       nomepan_R (strcat n_base newcod))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Retro " "\"\t" (rtos lunglam_R 2 1) "\t" (rtos (nth 14 svilcorr) 2 1) "\t"
			   (rtos halt_F_R 2 1) "\t\"" (nth 12 svilcorr) "\"\t\"" forisn "\"\t\"" valsv "\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
			   )
	       nomepan_R (strcat n_base newcod))
	   )
	 
	 
 	 (setq $sviluppi (cons nomepan_R $sviluppi))

         (write-line rig file)
	 (if filet (write-line  rig filet))

         (s_close file (strcat modalita "sviluppi_r.txt"))

		   (setq sviluppi_r (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi_r))))
                   (setq svilupp_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp_r))))
		   
         (setq file (S_OPEN (strcat modalita "dibsvi_r.txt") "a"))

 	 (foreach n LISTAPpr
           (setq rig_& (strcat "\"" nomePAN_r "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" (nth 12 svilcorr) "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
	   (setq rig (strcat "\"" nomePAN_R "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))
           (write-line rig_& file)
	   (if filet (write-line  rig_& filet))

	   	   (setq dibsvi_R (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE dibsvi_R))))
                   (setq dibsv_r (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_R))))
	   	   

	   
	 )
         (s_close file (strcat modalita "dibsvi_r.txt"))
       )
    )
    (setq nomesvi_r nomepan_R)
     ))
  
;pannello-------------------------------------------------------------------------------------------------------------
    (setq listapx listap)
    (setq listapx_r listap_R)
    (setq listap nil)
    (setq listap_r nil)

    (foreach n listapx
      	   (IF (= "000000000" (NTH 0 N))
;	     (setq listap (cons n listap))
	     (setq listap (cons (list (nth 0 n)  (nth 1 n) (nth 2 n) (nth 3 n) (- (nth 4 n) (nth 3 svilcorr)) (nth 5 n)) listap))
	     (setq listap (cons (subst nomepan (car n) n) listap))
	   )
    )
        (foreach n listapx_r
      	   (IF (= "000000000" (NTH 0 N))
	     ;(setq listap_R (cons n listap_R))
	     (setq listap_r (cons (list (nth 0 n)  (nth 1 n) (nth 2 n) (nth 3 n) (- (nth 4 n) (nth 7 svilcorr)) (nth 5 n)) listap_r))
	     
	     (setq listap_R (cons (subst nomepan_R (car n) n) listap_R))
	   )
    )

;pieghe su giunti
    (if (= lung2 0)
      (progn
         (command"_dimangular" "" "_non" (nth 1 lispt) "_non" (nth 2 lispt) "_non" (nth 0 lispt) "_non" (nth 2 lispt))
	 (setq angpan2 (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))
	 (command"_dimangular" "" "_non" (nth 0 lispt) "_non" (nth 1 lispt) "_non" (last lispt) "_non" (nth 1 lispt))
	 (setq angpan1 (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))

      )
      (progn
         (command"_dimangular" "" "_non" (nth 2 lispt) "_non" (nth 3 lispt) "_non" (nth 1 lispt) "_non" (nth 3 lispt))
	 (setq angpan2 (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))
	 (command"_dimangular" "" "_non" (nth 0 lispt) "_non" (nth 1 lispt) "_non" (last lispt) "_non" (nth 1 lispt))
	 (setq angpan1 (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))

      )
      
    )
;pannello frontale
     (if (= lung2 0)
       (if (= "Si" (leggi ent "BLOCCOLAM"))
         (setq lrig (list "Pannello" (atoF (rtos lung 2 1)) (atoF (rtos lung2 2 1)) (nth 13 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
	 (setq lrig (list "Pannello" (atoF (rtos lung 2 1)) (atoF (rtos lung2 2 1)) (nth 13 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
	 )
       (if (= "Si" (leggi ent "BLOCCOLAM"))
         (setq lrig (list "Angolo" (atoF (rtos lung 2 1)) (atoF (rtos lung2 2 1)) (nth 13 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
	 (setq lrig (list "Angolo" (atoF (rtos lung 2 1)) (atoF (rtos lung2 2 1)) (nth 13 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
	 )
     )
     (setq newpan nil)
     (SETQ LIS1 NIL)
     (FOREACH PANDL LISPAN
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
     (IF (NULL LIS1) (SETQ NEWPAN "T"))
     (SETQ LIS LIS1)
    
    (if (AND LISTAP (null newPAN))
      (progn
	(SETQ LIS LIS1)
	(FOREACH PN LISTAP;sono solo fori
          (SETQ LISZ NIL)
	  (FOREACH N LIS  
	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) finit forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (IF (MEMBER PK DIBPAN) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
    (IF LIS (SETQ NEWPAN NIL NOMEPAN (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last lispan)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "pannellil.txt") "a"))
	 (setq n_base (substr (car (last lispan)) 1 3))
	 (if (= lung2 0)
	   (PROGN
	     (if (= "Si" (leggi ent "BLOCCOLAM"))
             (setq rig (strcat "\""n_base newcod "\"\t\"Pannello\"\t" (rtos lung 2 1) "\t" (rtos lung2 2 1) "\t" (rtos (nth 13 svilcorr) 2 1) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       (setq rig (strcat "\""n_base newcod "\"\t\"Pannello\"\t" (rtos lung 2 1) "\t" (rtos lung2 2 1) "\t" (rtos (nth 13 svilcorr) 2 1) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       )
	     
	   )
	   (PROGN
	     (if (= "Si" (leggi ent "BLOCCOLAM"))
             (setq rig (strcat "\"" n_base newcod "\"\t\"Angolo\"\t" (rtos lung 2 1) "\t" (rtos lung2 2 1) "\t" (rtos (nth 13 svilcorr) 2 1) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       (setq rig (strcat "\"" n_base newcod "\"\t\"Angolo\"\t" (rtos lung 2 1) "\t" (rtos lung2 2 1) "\t" (rtos (nth 13 svilcorr) 2 1) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       
	       )
	   
	   )
	 )
 	 (setq $pannellil (cons nomepan $pannellil))
         (write-line rig file)
	 (if filet (write-line rig filet))

         (s_close file (strcat modalita "pannellil.txt"))
	 
		   (setq lispan (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan))))
                   (setq lispn (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn))))
		   
	 
         (setq file (S_OPEN (strcat modalita "DIBpan.txt") "a"))
	 

 	 (foreach n LISTAP
           (setq rig_& (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
           (setq rig (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))	   
           (write-line rig_& file)
	   (if filet (write-line  rig_& filet))

  	   	   (setq DIBpan (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan))))
                   (setq DIBpn (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn))))
  	   	   

	 )
         (s_close file (strcat modalita "DIBpan.txt"))
	 
       )
    )

;pannello retro
    (if (> 0 lung2_r) (setq lung2_r 0))
      (if (= Pconf "Bifacciale")
    (progn

     (if (= lung2 0)
       (if (= "Si" (leggi ent "BLOCCOLAM"))
         (setq lrig (list "Pannello retro" (atoF (rtos lung_r 2 1)) (atoF (rtos lung2_R 2 1)) (nth 14 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
	 (setq lrig (list "Pannello retro" (atoF (rtos lung_r 2 1)) (atoF (rtos lung2_r 2 1)) (nth 14 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
	 )
       (if (= "Si" (leggi ent "BLOCCOLAM"))
         (setq lrig (list "Angolo retro" (atoF (rtos lung_R 2 1)) (atoF (rtos lung2_R 2 1)) (nth 14 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
	 (setq lrig (list "Angolo retro" (atoF (rtos lung_r 2 1)) (atoF (rtos lung2_R 2 1)) (nth 14 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
	 )
     )
     (setq newpan nil)
     (SETQ LIS1 NIL)
     (FOREACH PANDL LISPAN_r
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
     (IF (NULL LIS1) (SETQ NEWPAN "T"))
     (SETQ LIS LIS1)
    
    (if (AND LISTAP_r (null newPAN))
      (progn
	(SETQ LIS LIS1)
	(FOREACH PN LISTAP_R;sono solo fori
          (SETQ LISZ NIL)
	  (FOREACH N LIS  
	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) finit forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (IF (MEMBER PK DIBPAN_R) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
    (IF LIS (SETQ NEWPAN NIL NOMEPAN_r (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last lispan_R)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "pannellil_r.txt") "a"))
	 (setq n_base (substr (car (last lispan_R)) 1 3))
	 (if (= lung2 0)
	   (PROGN
	     (if (= "Si" (leggi ent "BLOCCOLAM"))
             (setq rig (strcat "\""n_base newcod "\"\t\"Pannello retro\"\t" (rtos lung_r 2 1) "\t" (rtos lung2_R 2 1) "\t" (rtos (nth 14 svilcorr) 2 1) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan_r (strcat n_base newcod))
	       (setq rig (strcat "\""n_base newcod "\"\t\"Pannello retro\"\t" (rtos lung_R 2 1) "\t" (rtos lung2_R 2 1) "\t" (rtos (nth 14 svilcorr) 2 1) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan_r (strcat n_base newcod))
	       )
	     
	   )
	   (PROGN
	     (if (= "Si" (leggi ent "BLOCCOLAM"))
             (setq rig (strcat "\"" n_base newcod "\"\t\"Angol retroo\"\t" (rtos lung_r 2 1) "\t" (rtos lung2_R 2 1) "\t" (rtos (nth 14 svilcorr) 2 1) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan_r (strcat n_base newcod))
	       (setq rig (strcat "\"" n_base newcod "\"\t\"Angolo retro\"\t" (rtos lung_R 2 1) "\t" (rtos lung2_r 2 1) "\t" (rtos (nth 14 svilcorr) 2 1) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan_R (strcat n_base newcod))
	       
	       )
	   
	   )
	 )
 	 (setq $pannellil_r (cons nomepan_r $pannellil_r))
         (write-line rig file)

         (s_close file (strcat modalita "pannellil_r.txt"))
	 
		   (setq lispan_R (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan_R))))
                   (setq lispn_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn_R))))
		   
	 
         (setq file (S_OPEN (strcat modalita "DIBpan_R.txt") "a"))
	 

 	 (foreach n LISTAP_R
           (setq rig_& (strcat "\"" nomePAN_R "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
           (setq rig (strcat "\"" nomePAN_r "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))	   
           (write-line rig_& file)

  	   	   (setq DIBpan_r (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan_R))))
                   (setq DIBpn_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn_r))))
  	   	   

	 )
         (s_close file (strcat modalita "DIBpan_R.txt"))
	 
       )
    )

))

;assieme pannelllo





;    integ01
;;;;;;;;;;;;;;;;;;;;;;;;
    (SETQ LIS NIL)
    (SETQ LIS1 NIL)
    (setq resisor resis)
    (If (/= "KIT" (cadr (assoc 'montaggiofori parametri)))
      (progn
	(SETQ ALTKIT halt)
	(setq listk nil)
	(setq pt (car lispt))
	(setq pt0x_f pt)
	(if (= 0 lung2)
	  (setq LISPTFO (list (nth 0 lispt) (nth 1 lispt)))
	  (progn
	    (setq LISPTFO (list (nth 0 lispt) (nth 1 lispt) (nth 3 lispt)))
	    (setq pt0x_f (nth 1 lispt))
	  )
	)
	(setq ldpos0_f lung)
	(setq hpan halt)
	(IF (= fORISN "Si") (DIBFORO) (setq listk nil))
	
	(foreach k listk (setq listaps (cons (list (nth 0 k)
						   (nth 2 k)
						   (nth 1 k)
						   (nth 3 k)
						   (nth 4 k)
						   (nth 5 k)
					     )
						   listaps)))
	
      )
    )
;;;    (if (/= resis  "B0") (setq resisk "B15") (setq resisk "B0"))
;;;    (foreach lanaT lanadir
;;;      (if (and (= pconf (nth 0 lanat)) (= forisn (nth 1 lanat)) (= resisk (nth 2 lanaT)))
;;;	(setq len_m lanat)
;;;	)
;;;      )
;;;    (if (null len_m) (progn (alert "Riga mancante in lanadir.txt!!") (exit)))
;;;    
;;;    (setq deskldr (nth 5 len_m))
;;;    (setq spmnan (nth 3 len_m))
	  
     (setq lung1 lung)
   (IF (/= (RTOS LUNG2 2 0) "0")
      (if (< 180 angpan)
	  (if (< lung lung2)
	    (SETQ LLUNG_M (LIST (- LUNG (nth 16 svilcorr))
                                (- LUNG2 spmnan (nth 15 svilcorr)))
            )
	    (SETQ LLUNG_M (LIST (- LUNG2 (nth 16 svilcorr))
                                (- LUNG spmnan (nth 15 svilcorr)))
            )
	  )
	  (if (< lung lung2)
	    (SETQ LLUNG_M (LIST (+ spmnan (- LUNG (nth 16 svilcorr)))
                                (- LUNG2 (nth 15 svilcorr)))
            )
	    (SETQ LLUNG_M (LIST (- LUNG (nth 16 svilcorr))
				(+ spmnan (- LUNG2 (nth 15 svilcorr)))
                          )
            )
	  )
       )
       (SETQ LLUNG_M (LIST (- LUNG (nth 16 svilcorr) (nth 15 svilcorr))))
    )

    
    (COND ((= ANGPAN 0) (SETQ LLUNG_M (LIST (LIST (- LUNG (nth 16 svilcorr) (nth 15 svilcorr)) 0  0 (ATOf (RTOS HPAN 2 0)) (nth 16 svilcorr) 0))))
	   ;[ang]=90;IIf([lung1]>[lung2];[lung1]-2+15;[lung1]-2);
	   ;[ang]=90;IIf([lung1]>[lung2];[lung2]-2;[lung2]-2+15);
	  
	  ((= ANGPAN 90) (IF (> LUNG1 LUNG2) (SETQ LLUNG_M (LIST (LIST (+ spmnan (- LUNG (nth 16 svilcorr))) 0 0 (ATOI (RTOS HPAN 2 0)) (nth 16 svilcorr) 0)
								 (LIST (+ 0  (- LUNG2 (nth 15 svilcorr)))  0  0 (ATOI (RTOS HPAN 2 0)) (atof (rtos lung 2 1)) 0)
								 ))
			                     (SETQ LLUNG_M (LIST (LIST (+ 0 (- LUNG (nth 16 svilcorr))) 0 0(ATOI (RTOS HPAN 2 0)) (nth 16 svilcorr) 0)
								 (LIST (+ spmnan  (- LUNG2 (nth 15 svilcorr)))  0  0 (ATOI (RTOS HPAN 2 0))
								       ;(atoi (rtos lung 2 0))
								       (atof (rtos (- (+ lung lung2) (nth 15 svilcorr) (+ spmnan  (- LUNG2 (nth 15 svilcorr)))) 2 1))
								       0)
								 ))))
	  ;[ang]=270;IIf([lung1]>[lung2];[lung1]-2-15;[lung1]-2);
	  ;[ang]=270;IIf([lung1]>[lung2];[lung2]-2;[lung2]-2-15);
	  ((= ANGPAN 270) (IF (> LUNG1 LUNG2) (SETQ LLUNG_M (LIST (LIST (- LUNG (nth 16 svilcorr) spmnan) 0 0 (ATOI (RTOS HPAN 2 0))(nth 16 svilcorr) 0)
								  (LIST (- LUNG2 (nth 15 svilcorr) 0)  0  0 (ATOI (RTOS HPAN 2 0))
									(atof (rtos (- (+ lung lung2) (nth 15 svilcorr) (- LUNG2 (nth 15 svilcorr) 0)) 2 1)) 0)
;									(atoi (rtos lung 2 0)) 0)
								 ))
			                     (SETQ LLUNG_M (LIST (LIST (- LUNG (nth 16 svilcorr) 0) 0 0 (ATOI (RTOS HPAN 2 0))(nth 16 svilcorr) 0)
								 (LIST (- LUNG2 (nth 15 svilcorr) spmnan)  0  0 (ATOI (RTOS HPAN 2 0))
								       ;(atoi (rtos lung 2 0)) 0) OK
								       (atof (rtos (- (+ lung lung2) (nth 15 svilcorr) (- LUNG2 (nth 15 svilcorr) spmnan)) 2 1))  0)
								 ))))
	  (T                           (SETQ LLUNG_M (LIST (LIST (- LUNG (nth 16 svilcorr) 0) 0 (ATOf (RTOS (- (/ ANGPAN 2) 90 0.01) 2 1)) (ATOI (RTOS HPAN 2 0))(nth 16 svilcorr) 0 (atof (rtos lung 2 1)))
						           (LIST (- LUNG2 (nth 15 svilcorr) 0)  (ATOf (RTOS (+ (- 0 (/ ANGPAN 2))90 0.01) 2 1))  0 (ATOI (RTOS HPAN 2 0))(atof (rtos lung 2 1)) 0)
								 ))
			                     ))

         (setq tagliver nil taglihor nil)
	 (foreach rf listaps (if (and (= "RB-" (substr (car rf) 1 3))
				      (or (= "R69" (nth 2 (assoc (car rf) tabrinfor)))
					  (= "R74" (nth 2 (assoc (car rf) tabrinfor)))
					  (= "R1" (nth 2 (assoc (car rf) tabrinfor)))
					  )
				      )
			       (if (= "VER" (nth 5 (assoc (car rf) tabrinfor)))
				 (setq tagliver (cons (list (nth 2 rf) (nth 4 (assoc (car rf) tabrinfor))) tagliver))
				 (setq taglihor (cons (list (nth 3 rf) (nth 4 (assoc (car rf) tabrinfor))) taglihor))
				 )
			       )
	   )
;   (setq taglihor '( (1770 80) (1270 80)))
    (setq taglihor (vl-sort taglihor
             (function (lambda (e1 e2)
                         (> (car e1) (car e2))))))
;;;    (setq taglihor mil)
   ;(setq taglihor (vl-sort taglihor '>)) 
    (foreach htag__ taglihor
      (setq htag (car htag__))
      (setq dtag (cadr htag__))
;      (setq LLUNG_M_to LLUNG_M)
      (setq llung_m_to nil)
      (foreach lung_m llung_m
	(if (< htag (nth 5 lung_m))
	  (setq LLUNG_M_to (cons lung_m LLUNG_M_to))
	  (progn
	    (setq LLUNG_M_to (cons (list (nth 0 lung_m) (nth 1 lung_m) (nth 2 lung_m) (- htag (/ dtag 2)) (nth 4 lung_m) 0) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (nth 0 lung_m) (nth 1 lung_m) (nth 2 lung_m) (- (nth 3 lung_m) htag (/ dtag 2)) (nth 4 lung_m) (+ (/ dtag 2) htag)) LLUNG_M_to))
	    )
	  )
	)
      (setq llung_m LLUNG_M_to)
      )
;;;    (setq tagliver '( (318 80) ));nota1
;;;    (setq tagliver '( (235 80)));nota1
;;;    (setq tagliver nil)
;;;    (setq tagliver (vl-sort tagliver '>)) 
    (setq tagliver (vl-sort tagliver
             (function (lambda (e1 e2)
                         (< (car e1) (car e2))))))

    (foreach dtag__ tagliver
      (setq dtag (car dtag__))
      (setq ltag (cadr dtag__))
       (setq llung_m_to nil)

       (foreach lung_m llung_m
;;;	 (if (/= lung2 6)
;;;         (if (> dtag (+ (nth 0 lung_m) (nth 4 lung_m)))
	   (if (> dtag lung)
	 (if (not (and (> dtag (nth 4 lung_m)) (< dtag (+ (nth 0 lung_m) (nth 4 lung_m)))))
  
        	(setq LLUNG_M_to (cons lung_m LLUNG_M_to))
	   (progn
;	    (setq LLUNG_M_to (cons (list (- (nth 0 lung_m)   (- (+ lung lung2) dtag 2 )(/ ltag 2)) (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (- (nth 0 lung_m) (- (+ lung lung2) dtag (/ ltag 2) 2) ltag)  (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)) LLUNG_M_to))
;	    (setq LLUNG_M_to (cons (list (- (+ lung lung2)  dtag 2 (/ ltag 2)) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m)) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (- (+ lung lung2) dtag (/ ltag 2) 2) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m)) LLUNG_M_to))
	    ))
	 (if (not (and (> dtag (nth 4 lung_m)) (< dtag (+ (nth 0 lung_m) (nth 4 lung_m)))))
  
        	(setq LLUNG_M_to (cons lung_m LLUNG_M_to))
	   (progn
;	    (setq LLUNG_M_to (cons (list (- (nth 0 lung_m)   (- (+ lung lung2) dtag 2 )(/ ltag 2)) (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (- (nth 0 lung_m) ltag (- (+ (nth 4 lung_m) (nth 0 lung_M) ) (/ ltag 2) dtag)) (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)) LLUNG_M_to))
;	    (setq LLUNG_M_to (cons (list (- (+ lung lung2)  dtag 2 (/ ltag 2)) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m)) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (- (+ (nth 4 lung_m) (nth 0 lung_M) ) (/ ltag 2) dtag) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m)) LLUNG_M_to))
	    ))
	     )

	   
;;;	   (if (< dtag (nth 4 lung_m))
;;;        	(setq LLUNG_M_to (cons lung_m LLUNG_M_to))
;;;	   (progn
;;;	    (setq LLUNG_M_to (cons (list (- dtag 2 (/ ltag 2)) (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)) LLUNG_M_to))
;;;	    (setq LLUNG_M_to (cons (list (- (+ 2 (nth 0 lung_m)) dtag (/ ltag 2)) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m)) LLUNG_M_to))
;;;	    ))
;;;	   )
	 )
         (setq llung_m LLUNG_M_to)

      )
			       
	 (SETQ LLUNG_M+H LLUNG_M)  
    (setq LLUNG_M+H_ nil)
    (foreach lm LLUNG_M+H
      (if (> (car lm) 1) (setq LLUNG_M+H_ (cons lm LLUNG_M+H_)))
      )
    (setq LLUNG_M+H LLUNG_M+H_)
    (FOREACH LUNG_M+H+A LLUNG_M+H
      (SETQ LUNG_M (CAR LUNG_M+H+A))
      
      (setq rig_m (list deskldr cod_m (atof (rtos lung_M 2 1)) (atof (rtos (- (NTH 3 LUNG_M+H+A)  (cadr (assoc 'diffaltldr parametri))) 2 1))
			spmnan (NTH 1 LUNG_M+H+A) (NTH 2 LUNG_M+H+A)
			(atoi (rtos (* (atoI (rtos lung_M 2 1))
				         (atoi (rtos (- (NTH 3 LUNG_M+H+A)  (cadr (assoc 'diffaltldr parametri))) 2 0))) 2 0)) "Nr" ))
      (if (setq percod_m (member rig_m mate))
	(setq NOMEPAN_m (car (nth (- (LENGTH MATE) (length percod_m)) mater)))
	(progn
	  (setq file (S_OPEN (strcat modalita "mater.txt") "a"))
	  (setq newcod (rtos (+ 1 (atoi (substr (car (last mater)) 4))) 2 0))
	  (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	  (setq n_base (substr (car (last mater)) 1 3))
	  (setq rig-m (strcat "\""n_base newcod "\"\t\"" deskldr "\"\t\"" (NTH 1 RIG_m)"\"\t" (rtos (NTH 2 RIG_m) 2 1)
			      "\t" (rtos (NTH 3 RIG_m)2 1) "\t" (RTOS (NTH 4 RIG_m) 2 0) "\t" (RTOS (NTH 5 RIG_m) 2 1)
			      "\t" (RTOS (NTH 6 RIG_m) 2 1)
			      "\t" (rtos (NTH 7 RIG_m) 2 0)
			      "\t\"" (NTH 8 RIG_m) "\"")
		)
	  (SETQ nomepan_m (strcat n_base newcod))
	  (if (= resis "B0+") (setq resis "B15"))
	  (if (= resis "B15+") (setq resis "B15"))
;;;	  (IF REGP (SETQ RIG_A-m (strcat nomepan_m "|" deskldr "|Nr.|" (rtos (nth 2 rig_m) 2 1) "|" (rtos (nth 3 rig_m) 2 1) "|"(rtos (nth 4 rig_m) 2 0)
;;;				"|materassino|" "|" resis "|" "|" "|0|0||||"
;;;				)
;;;		)
;;;	    (SETQ RIG_A-m (strcat nomepan_m "|" deskldr "|Nr.|" (rtos (nth 2 rig_m) 2 1) "|" (rtos (nth 3 rig_m) 2 1) "|"(rtos (nth 4 rig_m) 2 0)
;;;				"|materassino|" "|" resis "|" "|" "|"
;;;				)
;;;		)
;;;	  )
	  (write-line rig-m file)
	  (s_close file (strcat modalita "mater.txt"))
		   (setq mater (REVERSE (CONS (READ (STRCAT "(" rig-m ")")) (REVERSE mater))))
                   (setq mate (REVERSE (CONS (CDR (READ (STRCAT "(" rig-m ")"))) (REVERSE mate))))
	  (if filea (write-line rig_A-m filea)) ;(inquad rig_A-m)
	  (setq file (S_OPEN (strcat modalita "DIBldr.txt") "a"))
	  (setq rig (strcat "\"" nomePAN_m "\"\t\"" (NTH 1 rig_m) "\"\t" (RTOS (NTH 2 rig_m) 2 1)
			    "\t" (RTOS (NTH 3 rig_m) 2 1) "\t" (RTOS (NTH 4 rig_m) 2 1) "\t" (RTOS (NTH 7 rig_m) 2 1)))
	  (write-line rig file)
	  (SETQ RIG_A (STRCAT NOMEPAN_m "|" (NTH 1 rig_m) "|" (RTOS (/ (atof (rtos (NTH 5 rig_m))) 1000000) 2 2) "||"))
	  (SETQ RIG_A6 (STRCAT NOMEPAN_m "|" (NTH 1 rig_m) "|" (RTOS (/ (atof (rtos (NTH 5 rig_m))) 1000000) 2 2) "||||"))
	
	  (if filed (WRITE-LINE RIG_A FILED))
	  (if filed (WRITE-LINE RIG_A6 FILED6))
	  (s_close file (strcat modalita "DIBldr.txt"))
	  )
	)
      (setq listaps (cons (list nomepan_m 1 (NTH 4 LUNG_M+H+A) (NTH 5 LUNG_M+H+A) 0 0) listaps))
   )
;;;    (if (or (= modop "STD") (= modop "SPS") (= modop "MTD") (= modop "MPS"))
;;;      (progn
;;;;;;        (setq Profl (cadr (assoc 'ProfFissLana parametri)))
;;;        (setq listaps (cons (list Profl 1 0 (- halt 20.0) 0 0) listaps))
;;;        (setq listaps (cons (list Profl 1 0 20.0 0 0) listaps))
;;;	)
;;;      )
    (cond ((and (= resisor "B15+") (or (= rfPANsn "Si") (= "Si" forisn))) (setq desctec "19"))
          ((and (= resisor "B15") (or (= rfPANsn "Si") (= "Si" forisn))) (setq desctec "19"))
	  ((and (= resisor "B0+") (or (= rfPANsn "Si") (= "Si" forisn))) (setq desctec "17"))
	  ((and (= resisor "B0") (or (= rfPANsn "Si") (= "Si" forisn))) (setq desctec "17"))
          ((and (= resisor "B15") (= rfPANsn "No") (= "No" forisn))  (setq desctec "18"))
	  ((and (= resisor "B15+") (= rfPANsn "No") (= "No" forisn))  (setq desctec "18"))
          ((and (= resisor "B0+") (= rfPANsn "No") (= "No" forisn)) (setq desctec "16"))
          (t (setq desctec "16"))
    )
    (if (and (= resisor "B0+") (= "No" forisn)) (setq resis "B15"))
    (if (and (= resisor "B0+") (= "Si" forisn)) (setq resis "B0"))
    (if (= resis "B15+") (setq resis "B15"))
;;;    (if (or (= modop "SPD") (= modop "STD"))
;;;      (progn
;;;	(if (>= halt (cadr (assoc 'altgiuntoPanL parametri))) (setq hgiunto (cadr (assoc 'altgiuntopan1 parametri)))
;;;	  (setq hgiunto (cadr (assoc 'altgiuntopan2 parametri))))
;;;	(setq listaps (cons (list giuntopan hgiunto 0 0 0 0) listaps))
;;;	)
;;;      )
    (SETQ CART_SX "")
    (if (and (= "" cart_ang) (= "" cart_for) (= "" cart_rinf)
	     (or (= "SPC" MODOP) (= "SPD" MODOP) (= "MPC" MODOP) (= "MPD" MODOP) (= "MAN" MODOP)
	     ))
      (SETQ CART_SX "X")
      )
    (if (and (= "" cart_ang) (= "" cart_for) (= "" cart_rinf)
	     (or (= "STD" MODOP) (= "SPS" MODOP) (= "MPS" MODOP) (= "MPS" MODOP)
	     ))
      (SETQ CART_SX "S")
      )
    (if (and (= "" cart_ang) (= "" cart_for) (= "" cart_rinf)
	     (or (= "MPM" MODOP) (= "SPM" MODOP)
	     ))
      (SETQ CART_SX "Y")
      )
    (if (= Pconf "Bifacciale") (setq cart_conf "Bifacciale") (setq cart_conf "Monofacciale"))
    (if (= resis "B15") (setq cart_resis "B15") (setq cart_resis "B0"))
    (if (and (= resis "B15"))
      (progn
	(if (/= "" (nth 17 svilcorr))
		(progn
		    (setq profilo (arcprof (nth 17 svilcorr) "Grezzo" (nth 18 svilcorr) 52))
		      (setq hp1 (+ (/ (nth 18 svilcorr) 2) (/ (- halt (* (nth 18 svilcorr) 2)) 3)))
		      (setq hp2 (+ (nth 18 svilcorr) (/ (- halt (* (nth 18 svilcorr) 2)) 3) (/ (- halt (* (nth 18 svilcorr) 2)) 3) (/ (nth 18 svilcorr) 2)))
		  (if (= 2 (nth 19 svilcorr))
		    (progn
		      (setq LISTAPs (cons (list profilo 1 12 hp1 0 0) listaps))
		      (setq LISTAPs (cons (list profilo 1 12 hp2 0 0) listaps))
		      )
		    (progn
	 	      (setq LISTAPs (cons (list profilo 1 12 hp1 0 0) listaps))
        	      (setq LISTAPs (cons (list profilo 1 12 hp2 0 0) listaps))
		      (setq LISTAPs (cons (list profilo 1 (- (+ lung lung2) 12) hp1 0 0) listaps))
		      (setq LISTAPs (cons (list profilo 1 (- (+ lung lung2) 12) hp2 0 0) listaps))
		      )
		    )
		  )
	  )
	(if (/= "" (nth 20 svilcorr))
		(progn
		    (setq profilo (arcprof (nth 20 svilcorr) "Grezzo" (- halt 60) 52))
		    (setq hp1 (/ halt 2))
		  (if (= 1 (nth 22 svilcorr))
		    (progn
		      (setq LISTAPs (cons (list profilo 1 8 hp1 0 0) listaps))
		      )
		    (progn
	 	      (setq LISTAPs (cons (list profilo 1 8 hp1 0 0) listaps))
		      (setq LISTAPs (cons (list profilo 1 (- (+ lung lung2) 8) hp1 0 0) listaps))
		      )
		    )
		  )
	  )
	(if (and (= forisn "Si") (/= "" (nth 21 svilcorr)))
		(progn
		    (setq profilo (arcprof (nth 21 svilcorr) "Grezzo" halt 100))
		    (setq hp1 (/ halt 2))
		  (if (= 1 (nth 22 svilcorr))
		    (progn
		      (setq LISTAPs (cons (list profilo 1 50 hp1 0 0) listaps))
		      )
		    (progn
	 	      (setq LISTAPs (cons (list profilo 1 50 hp1 0 0) listaps))
		      (setq LISTAPs (cons (list profilo 1 (- (+ lung lung2) 50) hp1 0 0) listaps))
		      )
		    )
		  )
	  )
		    
		      
	(print "")
	)
      )
    (setq LISTAPs (cons (list nomepan 1 0 0 0 0) listaps))
    (if (= Pconf "Bifacciale") (setq LISTAPs (cons (list nomepan_r 1 0 0 0 0) listaps)))
      (if (= "Si" (leggi ent "BLOCCOPAN"))
    (setq lrigs (list nomepan (strcat "Assieme " cart_conf " " cart_resis " " modop " " CART_SX cart_for cart_ang cart_rinf) resis "Nr" 1500 desctec finit (atoi (rtos halt))
		      forisn modop))
	(setq lrigs (list nomepan (strcat "Assieme " cart_conf " " cart_resis " " modop " " CART_SX cart_for cart_ang cart_rinf) resis "Nr" (atoi (rtos (length listaps) 2 0)) desctec finit (atoi (rtos halt))
		      forisn modop))
	)
    (setq newpas nil)
    (FOREACH PANDL LISPAs
        (IF (equal (CDR PANDL) LRIGs)
          (SETQ LIS1 (CONS PANDL LIS1
			   ))
        )
     )
     (IF (NULL LIS1) (SETQ NEWPAs "T"))
     (SETQ LIS LIS1)
    
    (if (AND LISTAPs (null newPAs))
      (progn
        ;(alert "pp")
	(SETQ LIS LIS1)
	(FOREACH Ps LISTAPs
          (SETQ LISZ NIL)
	  (FOREACH N LIS
;;;	    (SETQ PK (CONS (CAR N) Ps))
	    (setq ps1 (list (car ps)))
	    (foreach v (cdr ps)
	      (if (= 0 (- v (atoi (rtos v 2 1)))) (setq ps1 (cons (atoi (rtos v 2 1)) ps1)) (setq ps1 (cons (atof (rtos v 2 1)) ps1)))
	      )
	    (setq ps (reverse ps1))

	    (setq pk (list (car n) (nth 0 ps) (atoi (rtos halt 2 0)) finit forisn (nth 1 ps) (nth 2 ps) (nth 3 ps) (nth 4 ps) (nth 5 ps)))
;;;	    (if (null (MEMBER PK DIBPAs)) (alert "pp"))
	    (IF (MEMBER PK DIBPAs) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
	  
    (IF LIS (SETQ NEWPAs NIL NOMEPAns (CAR (LAST LIS))) (SETQ NEWPAs "T"))
    (if newPAs

      (progn
        (setq newcodx (rtos (+ 1 (atoi (substr (car (last lispas)) 4))) 2 0))
	(repeat (- 5 (strlen newcodx)) (setq newcodx (strcat "0" newcodx)))
	(setq n_base (substr (car (last lispas)) 1 3))
	(if (= "Si" (leggi ent "BLOCCOPAN"))
          (setq rig (strcat "\"" n_base newcodx "\"\t\"" nomepan "\"\t\"Assieme " cart_conf " " cart_resis " " modop " " CART_SX cart_for cart_ang cart_rinf "\"\t\"" resis "\"\t\"Nr\"\t" "0" "\t\"" desctec "\"\t\"" finit "\"\t" (rtos halt 2 0) "\t\"" forisn "\"\t\"" modop "\""))
	  (setq rig (strcat "\"" n_base newcodx "\"\t\"" nomepan "\"\t\"Assieme " cart_conf " " cart_resis " " modop " " CART_SX cart_for cart_ang cart_rinf "\"\t\"" resis "\"\t\"Nr\"\t"(rtos (length listaps) 2 0)"\t\"" desctec "\"\t\"" finit "\"\t" (rtos halt 2 0) "\t\"" forisn "\"\t\"" modop "\""))
	  )
	(setq $pannelli (cons (strcat n_base newcodx) $pannelli))
        (setq nomepans (strcat n_base newcodx))
     	(setq file (S_OPEN (strcat modalita "pannelli.txt") "a"))
	

        (write-line rig file)
	
        (s_close file (strcat modalita "pannelli.txt"))
	(if filet (s_close filet (strcat modalita "$$pannelli.txt")))
		   (setq lispas (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispas))))
                   (setq lisps (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lisps))))
		   

	(pesopan (strcat n_base newcodx))
	
	(setq file (S_OPEN (strcat modalita "DIBpaS.txt") "a"))
	
	
;;;        (write-line  (setq rig_& (strcat "\"" n_base newcodx "\"\t\"" nomepan "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn "\"\t1")) file)
;;;  	   	   (setq DIBpas (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpas))))
;;;                   (setq DIBps (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBps))))
;;;  	   	   
;;;	
	

	
	(foreach db listaps
           (setq rig_& (strcat "\"" n_base newcodx "\"\t\"" (NTH 0 db) "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 db) 2 1) "\t" (RTOS (NTH 2 db) 2 1)
                              "\t" (RTOS (NTH 3 db) 2 1) "\t" (RTOS (NTH 4 db) 2 1) "\t" (RTOS (NTH 5 db) 2 1)))
           (setq rig (strcat "\"" n_base newcodx "\"\t\"" (NTH 0 db) "\"\t" (rtos (NTH 1 db) 2 1) "\t" (RTOS (NTH 2 db) 2 1)
                              "\t" (RTOS (NTH 3 db) 2 1) "\t" (RTOS (NTH 4 db) 2 1) "\t" (RTOS (NTH 5 db) 2 1)))

	  
           (write-line rig_& file)
	  (if filet (write-line rig_& filet))
  	   	   (setq DIBpas (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpas))))
                   (setq DIBps (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBps))))
  	   	   

	  
        )
        (s_close file (strcat modalita "DIBpaS.txt"))
	
      )	
    )

    
;;;    (setq ptn (polar (nth 0 lispt) (setq ang (angle (nth 0 lispt) (nth 1 lispt))) (/ lung 2)))
;;;    (setq ptn (polar ptn (+ ang (/ pi 2)) 200))
;;;    (setq ptna (polar ptn (+ ang (/ pi 2)) 100))
;;;    (setq ptns (polar ptn (+ ang (/ pi 2)) -100))
    (if (= Pconf "Bifacciale")
      (progn
	
	(setq ptn (polar (nth 0 lispt) (setq ang (angle (nth 0 lispt) (nth 1 lispt))) (/ lung 2)))
	(setq ptn (polar ptn (+ ang (/ pi 2)) 230))
	(setq ptna (polar ptn (+ ang (/ pi 2)) 120))
	(setq ptna_r (polar ptn (+ ang (/ pi 2)) 60))
	(setq ptns (polar ptn (+ ang (/ pi 2)) -60))
	(setq ptns_r (polar ptn (+ ang (/ pi 2)) -120))
	(command"_layer" "_t" "siglepan*" "")
	(command"_layer" "_m" "siglepan" "")
	(command"_insert" "codpas" "_non" ptn  "1" "1" (angtos ang) nomepans codrp)
	(setq gent1 (entlast))
	(command"_insert" "codpan" "_non" ptna "1" "1" (angtos ang) nomepan codrp)
	(setq gent2 (entlast))
	(command"_insert" "codsvi" "_non" ptns   "1" "1" (angtos ang) nomesvi codrp)
	(setq gent3 (entlast))
	(command"_insert" "codpan_r" "_non" ptna_r "1" "1" (angtos ang) nomepan_r codrp)
	(setq gent4 (entlast))
	(command"_insert" "codsvi_R" "_non" ptns_r   "1" "1" (angtos ang) nomesvi_r codrp)
	(setq gent5 (entlast))
        (command"_group" "_c" "*" "codici" gent1 gent2 gent3 gent4 gent5 "")

	)
      (progn
       (setq ptn (polar (nth 0 lispt) (setq ang (angle (nth 0 lispt) (nth 1 lispt))) (/ lung 2)))
	(setq ptn (polar ptn (+ ang (/ pi 2)) 200))
	(setq ptna (polar ptn (+ ang (/ pi 2)) 100))
	(setq ptns (polar ptn (+ ang (/ pi 2)) -100))
	(command"_layer" "_t" "siglepan*" "")
	(command"_layer" "_m" "siglepan" "")

	(command"_insert" "codpas" "_non" ptn  "1" "1" (angtos ang) nomepans codrp) (setq gent1 (entlast))
	(command"_insert" "codpan" "_non" ptna "1" "1" (angtos ang) nomepan codrp)(setq gent2 (entlast))
	(command"_insert" "codsvi" "_non" ptns   "1" "1" (angtos ang) nomesvi codrp)(setq gent3 (entlast))
	        (command"_group" "_c" "*" "codici" gent1 gent2 gent3   "")

	)
      )
    (SETQ NR (+ 1 NR))
  )
  (command"_layer" "_m" "0" "")
;;;  (close file)
  (stop)
)

(DEFUN C:ANNTAB ()
;;;  (alert "pp")
  (FOREACH N '(profili profil CABPAS cabpan CABPAS distcab lispan lispn DIBpan DIBpn lispas lisps DIBpaS PORTE PRESE DISTPAN TABRIN LISKIT LISKT kitpan kitpn POSPON POSPN travinf
travin travsup travsu lanacer lanace DISTFORI LISRIN fogli fogl sviluppi svilupp DIBsvi DIBsv cesoiati cesoiat lanadir lanadi
mater mate rinfpiede rinfpied tabrinfor tabrinfo POSPON svilpan datikit distpan distpaS pannelli
	       sviluppi_r svilupp_R DIBsvi_r DIBsv_R cesoiati_r cesoiat_r lispan_R lispn_R DIBpan_r DIBpn_R)
    (SET N NIL)
  )
)
(defun s_close (file nome)
  (close file)
  (setq fperc (strcat (vl-filename-directory nome) "\\"))
  (setq fext (vl-filename-extension nome))
  (setq fbase (vl-filename-base nome))
  (while (null (vl-file-rename (strcat fperc fbase ".tx_") (strcat fperc fbase fext)))
    (p_alert (strcat "Attesa chiusura " nome))
  )
)
(defun pesopan (rpan)
  (IF (NULL PESI) (setq pesi (carica "pesi" "")))
  (if (null lispas) (setq lispas (carica "pannelli" modalita)))
  (if (null lispan) (setq lispan (carica "pannellil" modalita)))
  (setq finit (nth 7 (assoc (nth 1 (assoc rpan lispas)) lispan)))
  (setq lung1  (nth 2 (assoc (nth 1 (assoc rpan lispas)) lispan)))
  (setq lung2 (nth 3 (assoc (nth 1 (assoc rpan lispas)) lispan)))
  (setq halt (nth 5 (assoc (nth 1 (assoc rpan lispas)) lispan)))
  (setq resis (nth 3 (assoc rpan lispas)))
  (if (or (= resis "B15") (= resis "B15+")) (setq resisk "B15"))
  (if (or (= resis "B0") (= resis "B0+")) (setq resisk "B0"))

  (FOREACH N PESI
    (IF (AND (= FINIT (NTH 0 N))
	     (= RESISk (NTH 1 N))
	)
      (SETQ pesouk (NTH 2 N))
    )
  )
  (setq mqpan (* (/ (+ lung1 lung2) 1000.0) (/ halt 1000.0)))
  
  (setq pesop (* mqpan pesouk))
)
(defun c:newfin ()
  (setq gr (ssget (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq fpan (getstring (strcat "\Finitura pannello [" (cadr (assoc 'Finiture parametri)) "]<"(cadr (assoc 'Finitura parametri)) ">:")))
  (if (= "" fpan) (setq fpan (cadr (assoc 'Finitura parametri))))

  (setq nr -1)
  (while (setq ent (ssname gr (setq nr (+ 1 nr))))
    (allega ent "PANFIN" fpan)
  )
)
(defun c:regcab_p ()

;;;      (setq alf (ssget "x" (list '(0 . "INSERT") '(2 . "$POS"))))
;;;  (SETQ LALF NIL)
;;;  (IF ALF
;;;    (PROGN
;;;      (SETQ NR -1)
;;;      (WHILE (SETQ ENT (SSNAME ALF (SETQ NR (+ 1 NR))))
;;;	(SETQ LALF (CONS (LIST (CDR (ASSOC '1 (ENTGET (ENTNEXT (ENTNEXT ENT)))))
;;;			       (CDR (ASSOC '1 (ENTGET (ENTNEXT ENT))))
;;;			       (CDR (ASSOC '1 (ENTGET (ENTNEXT (ENTNEXT (ENTNEXT ENT))))))
;;;			       (CDR (ASSOC '10 (ENTGET ENT)))
;;;			       )
;;;			 
;;;			 LALF))
;;;	)
;;;      )
;;;    )
  (setq cabina (getvar "dwgname"))
  (setq cabina (substr cabina 1 (- (strlen cabina) 4)))
  (SETQ CABINA (SUBSTR CABINA 1 10))
  (setq revisione (substr (getvar "dwgname") 16 1))
  (setq lista nil)
;;;  (setq gr (ssget "x" (list '(0 . "INSERT") '(2 . "codkit"))))
  (setq grpn (ssget "x" (list '(0 . "INSERT") '(2 . "codpas"))))
  (setq lista nil)
  (SETQ LISTA_P NIL)
;;;  (setq nr 0)
;;;  (while (setq ent (ssname gr nr))
;;;     (setq lista (cons
;;;		   (cdr (assoc '1 (entget (entnext ent))))
;;;		   lista
;;;		 )
;;;     )
;;;     (IF (AND LALF (ASSOC (cdr (assoc '1 (entget (ENTNEXT (entnext ent))))) LALF))
;;;       (SETQ POS (NTH 1 (ASSOC (cdr (assoc '1 (entget (ENTNEXT (entnext ent))))) LALF)))
;;;       (SETQ POS "0")
;;;     )
;;;     (setq lista_P (cons
;;;		   (LIST (cdr (assoc '1 (entget (entnext ent)))) POS)
;;;		   lista_P 
;;;		 )
;;;     )
;;;    
;;;    (setq nr (+ 1 nr))
;;;  )

  
;;;  (setq gr (ssget "x" (list '(0 . "INSERT") '(2 . "CONGKIT?"))))
;;;  (setq nr 0)
;;;  (while (AND GR (setq ent (ssname gr nr)))
;;;     (if (AND (= "ATTRIB" (cdr (assoc '0 (entget (entnext ent)))))
;;;	      (/= "" (cdr (assoc '1 (entget (entnext ent)))))
;;;	 )
;;;       
;;;       (progn
;;;         (setq lista (cons (setq codpez (cdr (assoc '1 (entget (entnext ent))))) lista))
;;;	 (IF (AND LALF (ASSOC (cdr (assoc '5 (entget ent))) LALF))
;;;            (SETQ POS (NTH 1 (ASSOC (cdr (assoc '5 (entget ent))) LALF)))
;;;            (SETQ POS "0")
;;;         )
;;;         (setq lista_P (cons (LIST (setq codpez (cdr (assoc '1 (entget (entnext ent))))) pos) lista_P))
;;;	 
;;;       )
;;;     )
;;;    (setq nr (+ 1 nr))
;;;  )



  
;copia bak
  (vl-file-delete (strcat modalita "cabine_.bak"))
  (vl-file-copy  (strcat modalita "cabine.txt")  (strcat modalita "cabine_.bak"))
  (setq filer (S_OPEN (strcat modalita "cabine_.bak") "r"))
  (setq filew (S_OPEN (strcat modalita "cabine.txt") "w"))
  (while (setq rig (read-line filer))
;    (print (car (read (strcat "(" rig ")"))))
    (if (/= cabina (car (read (strcat "(" rig ")"))))
      (progn
         (write-line rig filew)
;	 (print rig)
	(car (read (strcat "(" rig ")")))
      )
    )
  )
  (s_close filer (strcat modalita "cabine_.bak"))
  (s_close filew (strcat modalita "cabine.txt"))



  
  (vl-file-delete (strcat modalita "cabpan_.bak"))
  (vl-file-copy  (strcat modalita "cabpan.txt")  (strcat modalita "cabpan_.bak"))
  (setq filer (S_OPEN (strcat modalita "cabpan_.bak") "r"))
  (setq filew (S_OPEN (strcat modalita "cabpan.txt") "w"))
  (while (setq rig (read-line filer))
;    (print (car (read (strcat "(" rig ")"))))
    (if (/= cabina (car (read (strcat "(" rig ")"))))
      (progn
         (write-line rig filew)
;	 (print rig)
	(car (read (strcat "(" rig ")")))
      )
    )
  )
  (s_close filer (strcat modalita "cabpan_.bak"))
  (s_close filew (strcat modalita "cabpan.txt"))

  (setq liscabk lista)
  (SETQ liscabk_P LISTA_P)
;;;  (setq file (S_OPEN (strcat modalita "cabine.txt") "a"))
;;;  (write-line (strcat "\"" cabina "\"\t\"NR\"\t\"Assieme pareti\"") file)
;;;  (s_close file (strcat modalita "cabine.txt"))
  (setq nr -1)
  (setq lista_pn nil)
  (while (setq ent (ssname grpn (setq nr (+ 1 nr))))
    (setq lista_pn (cons (list (cdr (assoc '1 (entget (entnext ent)))) 1) lista_pn))
    )
  (setq grpr (ssget "x" (list '(0 . "INSERT") '(2 . "profilo1"))))
  (setq nr -1)
  (setq lista_pr nil)
  (while (and grpr (setq ent (ssname grpr (setq nr (+ 1 nr)))))
    (setq lista_pr (cons (list (cdr (assoc '2 (entget ent))) 1) lista_pr))
    )
  (if (null finit) (setq finit "Non Definita"))
  (if (null ALTKIT) (setq ALTKIT 0))
   (setq porte (carica "porte" ""))
 (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))

  (SETQ nrPnfor 0)
  (Setq ltrave 0)
  (foreach n lista_pn
     (if (= "Si" (nth 9 (assoc (car n) lispas))) (setq nrPnfor (+ 1 nrPnfor)))
     (setq ltrave (+ ltrave (nth 2 (assoc (nth 1 (Assoc (car n) lispas)) lispan)) (nth 3 (assoc (nth 1 (Assoc (car n) lispas)) lispan))))
     (setq rig_a (strcat cabina "|" (CAR n) "|" (rtos (cadr n) 2 0) "|" "0" "|"))
;;;     (setq rig_a6 (strcat cabina "|" (car (CAR n)) "|" (rtos (cdr n) 2 0)  "|" (CADR (CAR N)) "|||"))
    
    (IF (NULL NOREGORA )(if filed (write-line rig_A filed))) ;ok)
    (setq rig (strcat "\"" cabina "\"\t\"" (car n) "\"\t" (rtos (cadr n) 2 0) "\t\"" "0" "\""))
    (write-line rig file)
  )
  (setq nr- -1)
  (setq gr-p (ssget "x" (list '(0 . "INSERT") '(2 . "Porta*"))))
  (while (and gr-p (setq ent- (ssname gr-p (setq nr- (+ nr- 1)) )))
    (setq nome- (cdr (assoc '2  (entget ent-))))
    (if (assoc nome- porte) (setq lporta (nth 3 (assoc nome- porte))))
    (if lporta (setq ltrave (+ ltrave lporta)))
    )
 ; (setq pprer (cadr (assoc 'profiloper parametri)))
  ;rif selezpar
  (setq pprerlg  (cadr (assoc 'profiloperqnt parametri)))
  (setq profrdogaAgg  (cadr (assoc 'profrdogaAgg parametri)))
  (setq profiloperAgg  (cadr (assoc 'profiloperAgg parametri)))
  (setq qntprof (+ (fix (* 1 (/ ltrave pprerlg))) 0))
  
(s_close file (strcat modalita "cabpan.txt"))
(if (/= qntprof 0)
    (progn
      (setq profx (arcprof pprer "NN" pprerlg 0))
      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (rtos (+ profiloperAgg (/ qntprof 1)) 2 0) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))

      (setq profx (arcprof pprer1 "NN" pprerlg 0))
      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (rtos (+ profiloperAgg (/ qntprof 1)) 2 0) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))
      (if (assoc 'Fin_profrdoga parametri) (setq Fin_nn (cadr (assoc 'Fin_profrdoga parametri))) (setq fin_nn "NN"))
      (setq profx (arcprof pregd fin_NN pprerlg 0))
      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (rtos (+ profrdogaAgg (/ qntprof 1)) 2 0) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))
      )
    )
  
    ;rif selezpar

  (if (/= nrPnfor 0)
    (progn
      (setq rig_a (strcat cabina "|" progiu "|" (rtos (* 2 nrPnfor) 2 0) "|" "1" "|"))
      (IF (NULL NOREGORA )(if filed (write-line rig_A filed)))
      (setq rig (strcat "\"" cabina "\"\t\"" progiu "\"\t" (rtos (* 2 nrPnfor) 2 0) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))
      )
    )
    (setq grrif (ssget "x" (list '(0 . "INSERT") '(2 . "riferimento"))))
  (setq nr -1)
        (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))

  (while (and grrif (setq rif (ssname grrif (setq nr (+ 1 nr)))))
      (setq rig_a (strcat cabina "|" (cdr (assoc 1 (entget (entnext rif)))) "|" (cdr (assoc 1 (entget (entnext (entnext rif)))))  "|" "1" "|"))
      (IF (NULL NOREGORA )(if filed (write-line rig_A filed)))
      (if (and (entnext (entnext (entnext rif))) (cdr (assoc '1 (entget (entnext (entnext (entnext rif))))))
	       (/= (cdr (assoc '1 (entget (entnext (entnext (entnext rif)))))) ""))
	(progn
	  (setq __tip (cdr (assoc '1 (entget (entnext (entnext (entnext rif)))))))
	  (setq __fam (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext rif))))))))
	  (setq __dim (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext rif)))))))))
	  (setq __alt (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext (entnext rif))))))))))
	  (setq __cer (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext rif)))))))))))
	  (setq __fin (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext rif))))))))))))
	  (setq __cla (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext rif)))))))))))))
            (setq rig (strcat "\"" cabina "\"\t\"" (cdr (assoc 1 (entget (entnext rif)))) "\"\t" (cdr (assoc 1 (entget (entnext (entnext rif))))) "\t\"" "0" "\"\t\""
			      __tip "\"\t\"" __fam "\"\t\"" __dim "\"\t\"" __alt "\"\t\"" __cer "\"\t\"" __fin "\"\t\"" __cla "\""))
	  )
            (setq rig (strcat "\"" cabina "\"\t\"" (cdr (assoc 1 (entget (entnext rif)))) "\"\t" (cdr (assoc 1 (entget (entnext (entnext rif))))) "\t\"" "0" "\""))
	)
      (write-line rig file)
)
    (s_close file (strcat modalita "cabpan.txt"))

;;;  (setq grrif (ssget "x" (list '(0 . "INSERT") '(2 . "riferimento"))))
;;;  (setq nr -1)
;;;  (while (and grrif (setq rif (ssname grrif (setq nr (+ 1 nr)))))
;;;      (setq profx (arcprof (cdr (assoc 1 (entget (entnext rif))))
;;;			   (cdr (assoc 1 (entget (entnext (entnext (entnext rif))))))
;;;			   (atof (cdr (assoc 1 (entget (entnext (entnext(entnext (entnext rif))))))))
;;;			   0))
;;;
;;;      (setq rig_a (strcat cabina "|" profx "|" (cdr (assoc 1 (entget (entnext (entnext rif)))))  "|" "1" "|"))
;;;      (IF (NULL NOREGORA )(if filed (write-line rig_A filed)))
;;;      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (cdr (assoc 1 (entget (entnext (entnext rif))))) "\t\"" "0" "\""))
;;;      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
;;;    
;;;    
;;;      (write-line rig file)
;;;      (s_close file (strcat modalita "cabpan.txt"))
;;;)
    (setq grrif (ssget "x" (list '(0 . "INSERT") '(2 . "profilo"))))
  (setq nr -1)
  (while (and grrif (setq rif (ssname grrif (setq nr (+ 1 nr)))))
      (setq profx (arcprof (cdr (assoc 1 (entget (entnext (entnext rif)))))
			   (cdr (assoc 1 (entget (entnext (entnext (entnext rif))))))
			   (atof (cdr (assoc 1 (entget (entnext (entnext (entnext (entnext rif))))))))
			   (atoi (cdr (assoc 1 (entget (entnext  (entnext (entnext (entnext (entnext rif)))))))))
			   ))
      (setq entx (entnext rif))
      (setq entm (subst (cons '1 profx) (assoc '1 (entget entx)) (entget entx)))
      (entmod entm)
      (entupd entx)
      (setq rig_a (strcat cabina "|" profx "|"   "|" "1" "|"))
      (IF (NULL NOREGORA )(if filed (write-line rig_A filed)))
      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (cdr (assoc 1 (entget (entnext (entnext  (entnext (entnext (entnext (entnext rif))))))))) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
    
    
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))
)
;;;  (foreach n lista_pr
;;;     (setq rig_a (strcat cabina "|" (CAR n) "|" (rtos (cadr n) 2 0) "|" "1" "|"))
;;;;;;     (setq rig_a6 (strcat cabina "|" (car (CAR n)) "|" (rtos (cdr n) 2 0)  "|" (CADR (CAR N)) "|||"))
;;;    
;;;    (IF (NULL NOREGORA )(write-line rig_A filed)) ;ok)
;;;    (setq rig (strcat "\"" cabina "\"\t\""  (CAR n) "\"\t" (rtos (cadr n) 2 0) "\t\"" "1" "\""))
;;;    (write-line rig file)
;;;  )
  

  (pesocab cabina)
  (IF REGP (setq rig_ax (strcat cabina "|Cabina |Nr.|||" (rtos altkit 2 0) "|Cab||"finit "||" cabina revisione "|"(rtos mqcab 2 2) "|" (rtos pesoc 2 2) "||||")) ;calcolopeso
   (setq rig_ax (strcat cabina "|Cabina |Nr.|||" (rtos altkit 2 0) "|Cab||"finit "||" cabina revisione "|"))
  )
  (IF (NULL NOREGORA )
    (progn
       (if filea (write-line rig_ax filea)) ;(inquad rig_Ax)
       (setq file (S_OPEN (strcat modalita "cabine.txt") "a"))
       (if (null revisione) (setq revisione ""))
       (write-line (strcat "\"" cabina "\"\t\"NR\"\t\"Assieme pareti\"\t\"" finit "\"\t" (Rtos altkit 2 0) "\t" (rtos mqcab 2 1) "\t" (rtos pesoc 2 2) "\t\"" revisione "\"") file)
       (s_close file (strcat modalita "cabine.txt")))
			))

(defun arcprof (nomex finiturax lunghezzax larghezzax)

     ;(setq rig_f (list (strcat "Profilo " nomex " "finiturax " l:" (rtos lunghezzax 2 0)) nomex finiturax lunghezzax))
  (setq rig_f (list (strcat "Profilo " nomex) nomex finiturax lunghezzax larghezzax))
     (setq profili (carica "profili" modalita) profil $tb$)
     (if (setq percod_F (member rig_f profil))

       (progn
         (setq profilo (car (nth (- (LENGTH profil) (length percod_f)) profili)))
       )  
       (progn
	 (setq xfilexy (OPEN (strcat modalita "profili.txt") "a"))
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last profili)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq n_base (substr (car (last profili)) 1 3))
         (setq rig-F (strcat "\"" n_base newcod "\"\t\"" (nth 0 rig_f) "\"\t\"" (NTH 1 RIG_F)"\"\t\"" (NTH 2 RIG_f)
			     "\"\t" (rtos (NTH 3 RIG_f)2 1) "\t" (rtos (NTH 4 RIG_f)2 1) ))
         (SETQ profilo (strcat n_base newcod))
         (write-line rig-f xfilexy)
         (close xfilexy)
         (setq profili (REVERSE (CONS (READ (STRCAT "(" rig-f ")")) (REVERSE profili))))
         (setq profil (REVERSE (CONS (CDR (READ (STRCAT "(" rig-f ")"))) (REVERSE profil))))
	 )
	 
       )
     profilo
     )


  
 
(defun pesocab (cab)
  (IF (NULL PESI) (SETQ PESI (CARICA "PESI" "")))
  (IF (NULL lispas) (setq lispas (carica "pannelli" modalita)))
  (IF (NULL cabpan) (setq cabpan (carica "cabpan" modalita)))
    (setq nbasepas (substr (car (nth 1 lispas)) 1 3))

;;;  (setq nr 1)
;;;  (while (and (setq kit (nth nr cabkit))
;;;	      (/= cab (nth 0 kit))
;;;	 )
;;;         (setq nr (+ 1 nr))
;;;  )
;;;  (while (and (setq kit (nth nr cabkit))
;;;	      (= cab (nth 0 kit))
;;;              (/= nbasekit (substr (nth 1 kit) 1 3))
;;;	 )
;;;         (setq nr (+ 1 nr))
;;;  )
;;;  (setq kit (nth 1 (nth nr cabkit)))
  (setq lpas nil)
  (setq pesoc 0)
  (setq mqcab 0)
  (foreach pan (cdr cabpan)
    (if (and (= (strcase cab) (strcase (nth 0 pan)))
	     (= nbasepas (substr (nth 1 pan) 1 3))
	)
      (progn
        (setq lpas (cons (nth 1 pan) lpas))
	(setq pesoc (+ (pesopan (nth 1 pan)) pesoc))
	(setq mqcab (+ mqpan mqcab))
      )
    )
  )
  pesoc
)