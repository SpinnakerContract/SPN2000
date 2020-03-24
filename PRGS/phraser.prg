*Phraser
PARA zprog
PRIV zprog
SET TALK OFF
IF PARA()<1
  zprog = []
ENDIF
RELE zpedro
PUBLIC zpedro
zpedro=[]
PUSH KEY CLEAR
zOldclick=_DBLCLICK
_DBLCLICK = .05
CLEAR TYPEAHEAD
marea = SELE()
IF USED('AVPHRASE')
  SELE avphrase
ELSE
  SELE 0
  USE avphrase ORDER 1
ENDIF
SET ORDER TO 1 IN avphrase
GO BOTT
GO TOP
SET CONF ON
**SET PRINTER FONT 'Lucida Console', 8 
ON KEY LABEL RIGHTMOUSE KEYB "{ESC}"
DEFI POPUP pphrases FROM 1,2 TO 30,157 MARGIN ;
  PROMPT FIELD PADR(avphrase.phrase,85) +;
  IIF(!EMPT(avphrase.memphrase),[ BLOCK],[     ]) ;
  FONT "Lucida Console",8 ;
  SCROLL TITLE [  #PHRASER   LIST                   ENTER  to  SELECT            TYPE FIRST LETTER TO PICK             ESC  to  CANCEL]
ON SELE POPUP pphrases DO deacphrase
ACTIVATE POPUP pphrases
ON KEY LABEL RIGHTMOUSE
USE IN avphrase
SELE (marea)
IF LEN(ALLT(zpedro))>0
  =INSM(.T.)
  zpedro = SpifUp(zpedro)
  STORE zpedro TO _CLIPTEXT
  KEYBOARD "{CTRL+V}"
ENDIF
_DBLCLICK = zOldclick  &&.20
POP KEY
ON KEY LABEL F10 DO phraser WITH [ ]
RETURN

PROCEDURE Escapeit &&========
STORE [] TO zpedro
DEAC POPUP pphrases

PROCEDURE deacphrase &&================
IF !EMPT(avphrase.memphrase)
  STORE ALLT(avphrase.memphrase) TO zpedro
ELSE
  STORE ALLT(avphrase.phrase) TO zpedro
ENDIF
DO CASE
  CASE avphrase.prefix=1
    zpedro = [, ]+zpedro
  CASE avphrase.prefix=2
    zpedro = [. ]+zpedro
  CASE avphrase.prefix=3
    zpedro = [ ]+zpedro
  OTHERWISE
    *
ENDCASE
DO CASE
  CASE avphrase.suffix=1
    zpedro = zpedro+[, ]
  CASE avphrase.suffix=2
    zpedro = zpedro+[. ]
  CASE avphrase.suffix=3
    zpedro = zpedro+[ ]
  OTHERWISE
    *
ENDCASE
zPedro = zPedro + IIF(avphrase.SuffixCrLf,CHR(13)+CHR(10),[])
DEACT POPUP pphrases

PROCEDURE SpifUp &&========================
PARA SpifInn
PRIV SpifInn, SpifOut
SpifOut = SpifInn
IF psysserial ==[9999]
 SpifOut = STRTR(SpifOut,'~-1',LEFT(UPPE(CDOW(DATE()-1)),3)+[ ]+dd_mmm_yyyy(DATE()-1,1))
 SpifOut = STRTR(SpifOut,'~-2',LEFT(UPPE(CDOW(DATE()-2)),3)+[ ]+dd_mmm_yyyy(DATE()-2,1))
 SpifOut = STRTR(SpifOut,'~-3',LEFT(UPPE(CDOW(DATE()-3)),3)+[ ]+dd_mmm_yyyy(DATE()-3,1))
 SpifOut = STRTR(SpifOut,'~-4',LEFT(UPPE(CDOW(DATE()-4)),3)+[ ]+dd_mmm_yyyy(DATE()-4,1))
 SpifOut = STRTR(SpifOut,'~-5',LEFT(UPPE(CDOW(DATE()-5)),3)+[ ]+dd_mmm_yyyy(DATE()-5,1))
 SpifOut = STRTR(SpifOut,'~-6',LEFT(UPPE(CDOW(DATE()-6)),3)+[ ]+dd_mmm_yyyy(DATE()-6,1))
 SpifOut = STRTR(SpifOut,'~-7',LEFT(UPPE(CDOW(DATE()-7)),3)+[ ]+dd_mmm_yyyy(DATE()-7,1))
 SpifOut = STRTR(SpifOut,'~~',pcdow(DATE()))
 SpifOut = STRTR(SpifOut,'~',LEFT(UPPE(CDOW(DATE())),3)+[ ]+dd_mmm_yyyy(DATE(),1))
ELSE
 SpifOut = STRTR(SpifOut,'~~',pcdow(DATE()))
 SpifOut = STRTR(SpifOut,'~',DTOC(DATE()))
ENDIF
RETURN(SpifOut)


