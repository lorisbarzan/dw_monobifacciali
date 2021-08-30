(defun c:aggpos ()
    (setq Palf '("A" "B" "C" "D" "E" "F" "G" "H" "I" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "Z" "A1" "B1" "C1" "D1" "E1" "F1" "G1" "H1" 
 "I1" "L1" "M1" "N1" "O1" "P1" "Q1" "R1" "S1" "T1" "U1" "V1" "Z1" "A2" "B2" "C2" "D2" "E2" "F2" "G2" "H2" "I2" "L2" "M2" "N2" "O2" "P2" "Q2" "R2" "S2" "T2" "U2" "V2" "Z2" 
"A3" "B3" "C3" "D3" "E3" "F3" "G3" "H3" "I3" "L3" "M3" "N3" "O3" "P3" "Q3" "R3" "S3" "T3" "U3" "V3" "Z3" 
"A4" "B4" "C4" "D4" "E4" "F4" "G4" "H4" "I4" "L4" "M4" "N4" "O4" "P4" "Q4" "R4" "S4" "T4" "U4" "V4" "Z4" 
"A5" "B5" "C5" "D5" "E5" "F5" "G5" "H5" "I5" "L5" "M5" "N5" "O5" "P5" "Q5" "R5" "S5" "T5" "U5" "V5" "Z5" 
"A6" "B6" "C6" "D6" "E6" "F6" "G6" "H6" "I6" "L6" "M6" "N6" "O6" "P6" "Q6" "R6" "S6" "T6" "U6" "V6" "Z6"
"A7" "B7" "C7" "D7" "E7" "F7" "G7" "H7" "I7" "L7" "M7" "N7" "O7" "P7" "Q7" "R7" "S7" "T7" "U7" "V7" "Z7"

))  
(setq gr (ssget "x" '((0 . "INSERT") (2 . "Cassa"))))
  (if gr (command"_erase" gr ""))
  (setq alf (ssget "x" (list '(0 . "INSERT") '(2 . "pospan"))))
  (SETQ LALF NIL)
  (IF ALF
    (PROGN
      (SETQ NR -1)
      (WHILE (SETQ ENT (SSNAME ALF (SETQ NR (+ 1 NR))))
	(SETQ LALF (CONS ENT
			 LALF))
	)
      )
    )

  (setq pt '(1 3))
  (SETQ NR 0)
  (SETQ POLD NIL)
  (SETQ GRDEL (SSADD))
  (SETQ PALF_ NIL)
  (setq modo t)
  (getpoint "\nCliccare per iniziare la selezione:")
  (while (and (/= 2 (car pt)) (/= 255 (car pt))(/= (LENGTH LALF) (LENGTH PALF_)))
    
    (setq pt (grread t))
    (if (null pold) (setq pold (cadr pt)))
;    (PRINT PT)
    (if (= '25 (car pt)) (progn (command"'_zoom" "_c" (cadr (grread t)) 6000) (setq modo nil) (setq nmodo t)))
;    (PRINT PT)
;;;    (setq pt (grread t))
    (if (= '3 (car pt))
      (progn (if modo (setq modo nil) (setq modo t))))
    (IF (= '5 (CAR PT))
      (if modo
      (PROGN
        (SETQ P (CADR PT))
	(IF (AND POLD (< 10 (DISTANCE P POLD)))
	  (PROGN
;;;	    (command"'_zoom" "_c" p 5000)
	    (if (< 1 (distance pold p))
	      (COMMAND "_LINE" "_non" POLD "_non"P "")
	    )
	    (SETQ GRDEL (SSADD (ENTLAST) GRDEL))
	    (setq gr (ssget "_F" (LIST P POLD) (list '(0 . "INSERT") '(2 . "pospan"))))
            (IF GR
              (PROGN
		 (SETQ ENT (SSNAME GR 0))
		 (IF (OR (NULL PALF_) (NULL (MEMBER ENT PALF_)))
		   (PROGN
		     (SETQ PALF_ (CONS ENT PALF_))
		     (command "_insert" "cassa" "_non" (cdr (assoc '10 (entget ent)))  "1" "1" "0"(nth nr palf))
;;;		     (setq entx (entnext ent))
;;;		     (setq entg (subst (cons '1 (nth nr palf))
;;;				       (assoc '1 (entget entx))
;;;				       (entget entx)))
;;;		     (entmod entg)
;;;		     (entupd entx)
		     (setq nr ( + 1 nr))
  	             (Prompt (strcat "\nAssegnata posizione :" (nth (- nr 1) palf)))
		   )
		 )
	       ))(SETQ POLD P)
	    )
	  )
	
	)
	(progn
          (SETQ P (CADR PT))
	(IF (AND POLD (< 10 (DISTANCE P POLD)))
	  (PROGN
	    (if nmodo (setq modo t nmodo nil))
	  (if (< 1 (distance pold p))
	    (progn
	      (COMMAND "_LINE" "_non" POLD "_non" P "")
	      (command"_chprop" (entlast) "" "_co" 6 "")
	      (SETQ GRDEL (SSADD (ENTLAST) GRDEL))
	      )
	    )(SETQ POLD P)))
	  	

	  ))
      )
    )
  (IF (/= (LENGTH LALF) (LENGTH PALF_))
    (ALERT "Non sono stati selezionati tutti le posizioni !")
  )
  (COMMAND"_ERASE" GRDEL "")
  )