(defun GetFromWeb (strUrl / webObj stat res errobj)
  (vl-load-com)
  ;; Set up variables
  (setq webObj nil stat nil res nil)
  ;; Create a new reference to the WinHttp object
  (setq webObj (vlax-invoke-method (vlax-get-acad-object) 'GetInterfaceObject "WinHttp.WinHttpRequest.5.1"))
  ;; Fetch web page
  (vlax-invoke-method webObj 'Open "GET" strUrl :vlax-false)
  (setq errobj (vl-catch-all-apply 'vlax-invoke-method (list webObj 'Send)))
  (if (null (vl-catch-all-error-p errobj))
    (progn
      (setq stat (vlax-get-property webObj 'Status))
      (if (= stat 200)
        (progn
          (setq res (vlax-get-property webObj 'ResponseText));_ Return the response value // 'ResponseText
        )
        (princ (strcat "\n!!! WEB server error: " (vlax-get-property webObj 'StatusText) "!!!"))
      )
    )
    (princ (strcat "\n!!! WEB server error:\n" (vl-catch-all-error-message errobj)))
  )
  res 
)

(defun JSON->LIST (json / )
;json - string, as json data
;returns - list, converted from json
(if (eq 'STR (type json)) (read (vl-string-translate "[]{}:," "()()  " json)))
)

(defun LIST->PAIRS (lst / ret tmp)
;this function is recursive
;lst - list, the list returned by "JSON->LIST", or similar
;returns - dotted pair lists or, in the case of nested lists, lists similar to points found in entities (item val1 val2 ...)
(setq ret '())
(if (listp lst)
  (foreach i lst
    (if (listp i)
      (setq ret (cons (list (LIST->PAIRS i)) ret))
      (if (eq 0 (rem (length (member i lst)) 2))
        (setq ret (cons (cons i (if (listp (setq tmp (cadr (member i lst)))) (LIST->PAIRS tmp) tmp)) ret))
      )
    )
  )
  (setq ret "")
)
(reverse ret)
)

(defun c:CALL ( / url data)
(vl-load-com)
;this url is open to everybody, it contains example data for anybody to test
(setq url "https://jsonplaceholder.typicode.com/users")
(if (and (setq data (GetFromWeb url))
	 (setq data (JSON->LIST data)))
  (progn
    ;now that we have a list, manipulate however you want
    ;Here is the entire list (all json data)
    (prompt "\n---Here is the JSON->LIST list---\n")
    (princ data)
    (prompt "\n---------------------------------\n")
    ;we can use our list manipulation functions to get specific items
    ;for example, we can loop through all users and print their names & emails
    (prompt "\n---Here are the names & emails---\n")
    (mapcar '(lambda (user) (princ (strcat "\nName: " (cadr (member "name" user)) " // Email: " (cadr (member "email" user))))) data)
    (prompt "\n---------------------------------\n")
    ;sometimes it may be more useful to have "assoc" pairs to work with
    (prompt "\n---Here is the LIST->PAIRS list--\n")
    (princ (setq data (LIST->PAIRS data)))
    (prompt "\n---------------------------------\n")
    (prompt "\nCALL Complete...")
  )
  (prompt "\nError Getting / Converting JSON data.")
)
(princ)
)