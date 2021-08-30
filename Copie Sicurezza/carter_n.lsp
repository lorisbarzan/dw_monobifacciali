
(defun regcarter ()
  (alert "Carter")
  (setq finitc "D")
  (setq listap_rc nil)
  (setq halt_cs (atof (substr modoc 4)))
  (setq modoc (substr modoc 1 3))

  (if (= modoc "COR")
    (progn (setq halt_ces halt_CS) (setq lung_c lung) (setq lung2_c lung2)) 
    (progn (setq halt_ces (- halt 50.0)) (setq lung_c halt_cs) (setq lung2_c 0)))
  
  (foreach m svilpan
       (if (and (= resis (nth 2 m)) (= MODOC (car m)) (= finitc (cadr m)))
	 (progn
	   (setq halt_c (+ halt_ces (nth 11 m) (nth 12 m)))
	   (setq lunglam_C (+ lung_C lung2_C (nth 7 m) (nth 8 m)))
	   (setq lunglam_Cf (+ lung_C lung2_C (nth 9 m) (nth 10 m)))
	   (setq svilcorr_c m)
	   )
	 )
     )
  (if (/= "No" forisnlana) (Setq lunglam_c lunglam_Cf))
;;;    (setq len_f_r (list 1 2 3 (rtos lunglam_r)))
     (setq rig_f (list (strcat "Carter cesoiato " ) (nth 14 svilcorr_c) (atof (rtos lunglam_c 2 1)) (atof (rtos halt_c 2 1)) (nth 16 svilcorr_c)
		       (atoi (rtos (* (atof (rtos lunglam_c 2 1)) (atof (rtos halt_c 2 1))) 2 0)) "Nr" ))
     (if (setq percod_F_r (member rig_f cesoiat_R))
       (progn
         (setq nomepan_f_C (car (nth (- (LENGTH cesoiat_R) (length percod_f_R)) cesoiati_R)))
       )  
       (progn
	 (setq file (S_OPEN (strcat modalita "cesoiati_R.txt") "a"))
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last cesoiati_R)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq n_base (substr (car (last cesoiati_R)) 1 3))
         (setq rig-F (strcat "\"" n_base newcod "\"\t\"Carter cesoiato " "\"\t\"" (NTH 1 RIG_F)"\"\t" (rtos (NTH 2 RIG_f) 2 1)
			     "\t" (rtos (NTH 3 RIG_f)2 1) "\t" (RTOS (NTH 4 RIG_f) 2 1) "\t" (rtos (NTH 5 RIG_f) 2 0)
			     "\t\"" (NTH 6 RIG_f) "\"")
	 )
         (SETQ nomepan_F_C (strcat n_base newcod))
         (write-line rig-f file)
         (s_close file (strcat modalita "cesoiati_R.txt"))
         (setq cesoiati_R (REVERSE (CONS (READ (STRCAT "(" rig-f ")")) (REVERSE cesoiati_R))))
         (setq cesoiat_r (REVERSE (CONS (CDR (READ (STRCAT "(" rig-f ")"))) (REVERSE cesoiat_R))))
 
         (setq file (S_OPEN (strcat modalita "DIBces_r.txt") "a"))
         (setq rig (strcat "\"" nomePAN_f_c "\"\t\"" (nth 14 svilcorr_c) "\"\t" (RTOS (NTH 2 rig_f) 2 0)
                              "\t" (RTOS (NTH 3 rig_f) 2 0) "\t" (RTOS (NTH 4 rig_f) 2 1) "\t" (RTOS (NTH 5 rig_f) 2 0)))
         (write-line rig file)
	 
         (s_close file (strcat modalita "DIBces_R.txt"))
       )
     )
     (setq listap_rc (cons (list nomePAN_f_c 0 0 1 0 0) listap_rc))
  ;sviluppo Carter
    ;sviluppo retro
           (if (/= 0 lung2_c) (progn (setq valsv (strcat (rtos angpan 2 0) " " (rtos lung2_c 2 1)))) (setq valsv "-"))


;;;     (if (= "Si" (leggi ent "BLOCCOSVI"))
;;;         (setq lrig (list (strcat "Sviluppo Retro " ) (atof (rtos lunglam_r 2 1)) (nth 16 svilcorr)
;;;		      (atof (rtos halt_F_R 2 1)) (nth 14 svilcorr) forisn valsv 1500 MODOP_ "Nr"))
         (setq lrig (list (strcat "Sviluppo Carter " ) (atoi (rtos lunglam_c 2 0)) (nth 16 svilcorr_c)
		      (atof (rtos halt_c 2 1)) (nth 14 svilcorr_c) forisn valsv 1 modoc "Nr"))
;;;       )
    
;;;     (setq newpan nil)
;;;      (setq listappr_ listappr listappr nil)
;;;      (foreach r LISTAPpR_
;;;	(if (> (nth 4 r) (- lunglam_r lung2_r))
;;;	  (setq listappr (cons (list (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r) (+ (nth 4 r) (* 2 spmnan (sin (* (/ angpan 180.0) pi)))) (nth 5 r)) listappr))
;;;	  (setq listappr (cons r listappr))
;;;	  )
;;;	)
		
;;;     (SETQ LIS1 NIL)
;;;     (FOREACH PANDL sviluppi_R
;;;        (IF (equal (CDR PANDL) LRIG)
;;;          (SETQ LIS1 (CONS PANDL LIS1))
;;;        )
;;;     )
;;;    (IF (NULL LIS1) (SETQ NEWPAN "T"))
;;;    (SETQ LIS LIS1)
;;;    (if (AND LISTAPpR (null newPAN))
;;;      (progn
;;;	(SETQ LIS LIS1)
;;;	(FOREACH PN (reverse LISTAPpR);sono solo fori
;;;          (SETQ LISZ NIL)
;;;	  (FOREACH N (reverse LIS  )
;;;	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) (nth 14 svilcorr) forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
;;;	    (IF (MEMBER PK dibsvi_R) (PROGN  (SETQ LISZ (CONS N LISZ))))
;;;	    (SETQ LIS LISZ)
;;;	  )
;;;	)
;;;      )
;;;    )
;;;    (IF LIS (SETQ NEWPAN_r NIL NOMEPAN_R (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if (setq percod_F_r (member lrig svilupp_r))
       (progn
         (setq NOMEPAN_R_c (car (nth (- (LENGTH svilupp_r) (length percod_f_R)) sviluppi_r)))
       )  
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last sviluppi_R)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "sviluppi_R.txt") "a"))

	 (setq n_base (substr (car (last sviluppi_R)) 1 3))
;;;	 (if (= "Si" forisn) (setq dessvi "SvilFor") (setq dessvi "SvilnonFor"))
;;;	 (if (= "Si" (leggi ent "BLOCCOSVI"))
;;;	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Retro " "\"\t" (rtos lunglam_r 2 1) "\t" (rtos (nth 16 svilcorr) 2 1) "\t"
;;;			   (rtos halt_F_R 2 1) "\t\"" (nth 14 svilcorr) "\"\t\"" forisn "\"\t\"" valsv "\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
;;;			   )
;;;	       nomepan_R (strcat n_base newcod))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Carter " "\"\t" (rtos lunglam_c 2 0) "\t" (rtos (nth 16 svilcorr_c) 2 1) "\t"
			   (rtos halt_c 2 1) "\t\"" (nth 14 svilcorr_C) "\"\t\"" forisn "\"\t\"" valsv "\"\t" (rtos 1 2 0) "\t\"" MODoc "\"\t\"Nr\""
			   )
	       nomepan_R_c (strcat n_base newcod))
;;;	   )
	 
	 
 	 (setq $sviluppi (cons nomepan_R_c $sviluppi))

         (write-line rig file)
	 (if filet (write-line  rig filet))

         (s_close file (strcat modalita "sviluppi_r.txt"))

		   (setq sviluppi_r (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi_r))))
                   (setq svilupp_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp_r))))
		   
         (setq file (S_OPEN (strcat modalita "dibsvi_r.txt") "a"))
(setq listapc (list (list nomepan_F_C halt (nth 14 svilcorr_C) forisn 0	0 1 0 0)))
 	 (foreach n listap_rc
           (setq rig_& (strcat "\"" nomePAN_r_c "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" (nth 14 svilcorr) "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
	   (setq rig (strcat "\"" nomePAN_R_c "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))
           (write-line rig_& file)
	   (if filet (write-line  rig_& filet))

	   	   (setq dibsvi_R (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE dibsvi_R))))
                   (setq dibsv_r (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_R))))
	   	   

	   
	 )
         (s_close file (strcat modalita "dibsvi_r.txt"))
       )
    )
 (setq listaps_c (list (list nomepan_R_c 0 0 1 0 0)))
    (setq nomesvi_c nomepan_R_c)

  ;mater carter
 	(if (null lanacartn) (setq lanacart (carica "lanadir" "")))
  (foreach m lanacart
       (if (and (= "Carter" (nth 0 m)) (= forisnlana (nth 1 m)) (= resis (nth 2 m)))
	 (progn
	   (setq deskldr (nth 5 m))
	   (setq cod_m (nth 4 m))
	   (setq spmnan (nth 3 m))
	   )
	 )
    )  

  
      (setq rig_m_c (list deskldr ;da lanadir
			  cod_m ;da lanadir
			  (atoi (rtos (- lunglam_c (nth 17 svilcorr_c) (nth 18 svilcorr_c)) 2 0))
			  (atoi (rtos (- Halt_c (cadr (assoc 'diffaltldr parametri)) 2 0)))
			spmnan ;da lanadir
			  0 0 1
			(atoi (rtos (* (atoi (rtos (- lunglam_c (nth 17 svilcorr_c) (nth 18 svilcorr_c)) 2 0))
				       (atoi (rtos (- Halt_c (cadr (assoc 'diffaltldr parametri))) 2 0))) 2 0)) "Nr" ))

      	  (setq rig_C (strcat "\"" "NOME" "\"\t\"" (NTH 1 rig_m_c) "\"\t" (RTOS (NTH 2 rig_m_c) 2 1)
			    "\t" (RTOS (NTH 3 rig_m_c) 2 1) "\t" (RTOS (NTH 4 rig_m_c) 2 0) "\t" (RTOS (NTH 8 rig_m_c) 2 0)))

      (setq dibpasldr_C (list (list (NTH 1 rig_m_c) (atoi (rtos (NTH 2 rig_m_C) 2 0)) (atoi (rtos (NTH 3 rig_m_c) 2 0)) (atoi (rtos (NTH 4 rig_m_C) 2 0)) (atoi (rtos (NTH 8 rig_m_C) 2 0)))))

    

      
   (if (setq percod_F_r (member rig_m_c mate));(null (setq NOMEPAN_R (member rig_m_c mater)))
       (progn
         (setq NOMEPAN_m (car (nth (- (LENGTH mate) (length percod_f_R)) mater)))
       )  
	(progn
	  (setq file (S_OPEN (strcat modalita "mater.txt") "a"))
	  (setq newcod (rtos (+ 1 (atoi (substr (car (last mater)) 4))) 2 0))
	  (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	  (setq n_base (substr (car (last mater)) 1 3))
	  (setq rig-m (strcat "\""n_base newcod "\"\t\"" deskldr "\"\t\"" (NTH 1 RIG_m_C)"\"\t" (rtos (NTH 2 RIG_m_c) 2 1)
			      "\t" (rtos (NTH 3 RIG_m_c)2 1) "\t" (RTOS (NTH 4 RIG_m_c) 2 0) "\t" (RTOS (NTH 5 RIG_m_c) 2 1)
			      "\t" (RTOS (NTH 6 RIG_m_c) 2 1)
			      "\t" (rtos (NTH 7 RIG_m_c) 2 0)
			      "\t" (rtos (NTH 8 RIG_m_C) 2 0)
			      "\t\"" (NTH 9 RIG_m_C) "\"")
		)
	  (SETQ nomepan_m (strcat n_base newcod))
	  (if (= resis "B0+") (setq resis "B15"))
	  (if (= resis "B15+") (setq resis "B15"))
	  (write-line rig-m file)
	  (s_close file (strcat modalita "mater.txt"))
		   (setq mater (REVERSE (CONS (READ (STRCAT "(" rig-m ")")) (REVERSE mater))))
                   (setq mate (REVERSE (CONS (CDR (READ (STRCAT "(" rig-m ")"))) (REVERSE mate))))
          (foreach rg dibpasldr_c
	    (setq rig_l (list nomePAN_m (NTH 0 rg) (NTH 1 rg) (NTH 2 rg) (NTH 3 rg) (NTH 4 rg)))
	    (setq rig (strcat "\"" (nth 0 rig_l) "\"\t\"" (NTH 0 rg) "\"\t" (rtos (NTH 1 rg) 2 0) "\t" (rtos (NTH 2 rg) 2 0) "\t"
			      (rtos (NTH 3 rg) 2 0) "\t" (rtos (NTH 4 rg) 2 0)))
	    
	  (setq file (S_OPEN (strcat modalita "DIBldr.txt") "a"))
	  (write-line rig file)
	  (s_close file (strcat modalita "DIBldr.txt"))
      	   	   (setq DIBldr (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE DIBldr))))

	  )
	)
	)
  (setq nrlan 1)
  (if (or (= "CAS" (nth 0 svilcorr_c)) (= "CAD" (nth 0 svilcorr_c)))
    (progn
      (if (= forisnlana "Si")
	(setq nrlan 2)
	(setq nrlan 1)
	)
      )
    )
  (setq listaps_c (cons (list nomepan_m (nth 17 svilcorr_c) (/ (cadr (assoc 'diffaltldr parametri)) 2) (* nrlan 1) 0 0) listaps_c))
  (setq nrlan nil)



;pannello Carter con lana
      (if (> 0 lung2_r) (setq lung2_r 0))
      (if t
    (progn

;;;     (if (= lung2 0)
;;;       (if (= "Si" (leggi ent "BLOCCOLAM"))
;;;         (setq lrig (list "Pannello retro" (atoF (rtos lung_r 2 1)) (atoF (rtos lung2_R 2 1)) (nth 16 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))

      (setq lrig (list "Pannello Carter" (atoi (rtos lung_c 2 1)) (atoi (rtos lung2_c 2 0)) (nth 16 svilcorr_c) (atoi (rtos halt_ces 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos 2 2 0)) MODOc "Nr"))
      (if (= "COR" (nth 0 svilcorr_c))
	(setq lrig (list "Pannello Carter" (atof (rtos lung_c 2 1)) (atoi (rtos lung2_c 2 0)) (nth 16 svilcorr_c) (atoi (rtos halt_ces 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos 2 2 0)) MODOc "Nr"))
	)
;;;	 )
;;;       (if (= "Si" (leggi ent "BLOCCOLAM"))
;;;         (setq lrig (list "Angolo retro" (atoF (rtos lung_R 2 1)) (atoF (rtos lung2_R 2 1)) (nth 16 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
;;;	 (setq lrig (list "Angolo retro" (atoF (rtos lung_r 2 1)) (atoF (rtos lung2_R 2 1)) (nth 16 svilcorr) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
;;;	 )
;;;     )
     (setq newpan nil)
     (SETQ LIS1 NIL)
     (FOREACH PANDL LISPAN_r
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
     (IF (NULL LIS1) (SETQ NEWPAN "T"))
     (SETQ LIS LIS1)
    
    
    (IF LIS (SETQ NEWPAN NIL NOMEPAN_c (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last lispan_R)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "pannellil_r.txt") "a"))
	 (setq n_base (substr (car (last lispan_R)) 1 3))
;;;	 (if (= lung2 0)
;;;	   (PROGN
;;;	     (if (= "Si" (leggi ent "BLOCCOLAM"))
;;;             (setq rig (strcat "\""n_base newcod "\"\t\"Pannello retro\"\t" (rtos lung_r 2 1) "\t" (rtos lung2_R 2 1) "\t" (rtos (nth 16 svilcorr) 2 1) "\t"
;;;	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
;;;	               )
;;;	           nomepan_r (strcat n_base newcod))
	       (setq rig (strcat "\""n_base newcod "\"\t\"Pannello Carter\"\t" (rtos lung_c 2 1) "\t" (rtos lung2_C 2 1) "\t" (rtos (nth 16 svilcorr_c) 2 1) "\t"
	                  (rtos halt_ces 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos 2 2 0) "\t\"" MODOc "\"\t\"Nr\""
	               )
	           nomepan_c (strcat n_base newcod))
;;;	       )
;;;	     
;;;	   )
;;;	   (PROGN
;;;	     (if (= "Si" (leggi ent "BLOCCOLAM"))
;;;             (setq rig (strcat "\"" n_base newcod "\"\t\"Angol retroo\"\t" (rtos lung_r 2 1) "\t" (rtos lung2_R 2 1) "\t" (rtos (nth 16 svilcorr) 2 1) "\t"
;;;	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
;;;	               )
;;;	           nomepan_r (strcat n_base newcod))
;;;	       (setq rig (strcat "\"" n_base newcod "\"\t\"Angolo retro\"\t" (rtos lung_R 2 1) "\t" (rtos lung2_r 2 1) "\t" (rtos (nth 16 svilcorr) 2 1) "\t"
;;;	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
;;;	               )
;;;	           nomepan_R (strcat n_base newcod))
;;;	       
;;;	       )
;;;	   
;;;	   )
;;;	 )
 	 (setq $pannellil_r (cons nomepan_r $pannellil_r))
         (write-line rig file)

         (s_close file (strcat modalita "pannellil_r.txt"))
	 
		   (setq lispan_R (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan_R))))
                   (setq lispn_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn_R))))

	 (if (AND listaps_c (null newPAN))
	   (progn
	     (SETQ LIS LIS1)
	     (FOREACH PN listaps_c ;sono lamiera e lana
	       (SETQ LISZ NIL)
	       (FOREACH N LIS
		 (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt_c 2 0)) finit forisn (atoi (rtos (nth 1 pn)2 0)) (atoi (rtos (nth 2 pn)2 0))
				(atoi (rtos (nth 3 pn)2 0)) (atoi (rtos (nth 4 pn)2 0)) (atoi (rtos (nth 5 pn)2 0))))
		 (IF (MEMBER PK DIBPAN_R) (PROGN  (SETQ LISZ (CONS N LISZ))))
		 (SETQ LIS LISZ)
		 )
	       )
	     )
	   )
		   
	 
         (setq file (S_OPEN (strcat modalita "DIBpan_R.txt") "a"))
	 

 	 (foreach n listaps_c
           (setq rig_& (strcat "\"" nomePAN_c "\"\t\""(NTH 0 N) "\"\t" (rtos halt_c 2 0) "\t\"" finit "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 0) "\t" (RTOS (NTH 2 N) 2 0)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 0) "\t" (RTOS (NTH 5 N) 2 0)));massimofori
           	   
           (write-line rig_& file)

  	   	   (setq DIBpan_r (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan_R))))
                   (setq DIBpn_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn_r))))
  	   	   

	 )
         (s_close file (strcat modalita "DIBpan_R.txt"))
	 
       )
    )

))


  (if (= modoc "CAD")(setq listaps (cons (list nomepan_c 1 (+ lung lung2) 25 0 0) listaps)))
  (if (= modoc "CAS")(setq listaps (cons (list nomepan_c 1 0 25 0 0) listaps)))
  (if (= modoc "CVD")(setq listaps (cons (list nomepan_c 1 (+ lung lung2) 25 0 0) listaps)))
  (if (= modoc "CVS")(setq listaps (cons (list nomepan_c 1 0 25 0 0) listaps)))
  (if (= modoc "COR")(setq listaps (cons (list nomepan_c 1 0 0 0 0) listaps)))
  ;-------------------

    )
