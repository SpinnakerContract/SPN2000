** used for 337 statement insertions
* called from 337 entries
SET TALK OFF
RELE zpedro
PUBLIC zpedro
zpedro=[]
PUSH KEY CLEAR
CLEAR TYPEAHEAD
marea = SELE()
IF !USED('avbck337')
  SELE 0
  USE avbck337 ORDER name337
ENDIF
SELE avbck337
SET ORDER TO 1 IN avbck337
GO BOTT
GO TOP
SET CONF ON
*ON KEY LABEL ESCAPE DO Escapeit
zOldclick=_DBLCLICK
_DBLCLICK = .05
DEFI POPUP list337s FROM 1,2 TO 22,80 MARGIN ;
  PROMPT FIELD PADR(avbck337.name,35);
  FONT "Courier New",9 ;
  SCROLL TITLE [ 337  STATEMENTS  LIBRARY   ENTER=SELECT   ESC:REVERT  ]
ON SELE POPUP list337s DO deacphrase
ACTIVATE POPUP list337s
_DBLCLICK = zOldclick
*ON KEY LABEL ESCAPE
SELE avbck337
USE
SELE (marea)
IF LEN(ALLT(zpedro))>0
  =INSM(.T.)
  STORE zpedro TO _CLIPTEXT
  KEYB "{CTRL+V}"
ENDIF
POP KEY
RELE POPUP list337s
RETURN(zpedro)

PROCEDURE Escapeit &&========
STORE [] TO zpedro
DEAC POPUP list337s

PROCEDURE deacphrase &&================
IF !EMPT(avbck337.back)
  STORE ALLT(avbck337.back) TO zpedro
ELSE
  STORE ALLT(avbck337.back) TO zpedro
ENDIF
DEACT POPUP list337s


