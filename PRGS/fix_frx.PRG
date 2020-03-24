** 06/26/1998 06/14/1998
CLOSE DATA
CLEAR
SET SAFETY OFF
SELECT 0
curpath = SYS(5) + CURDIR()
CD reports
NumFiles = ADIR(FNames, '*.FRX')
FOR x=1 TO NumFiles
  IF FILE(TRIM(FNames[x,1]))
    FName = ALLTRIM(FNames[x,1])
    WAIT WINDOW "Clearing printer drivers from " + FName + "..." NOWAIT
    USE (FName)
    LOCATE FOR objtype=1 AND objcode = 53
    IF FOUND()
      REPLACE TAG  WITH "", ;
        tag2 WITH "", ;
        expr WITH IIF("ORIENTATION=2"$expr,"ORIENTATION=2","")
    ENDIF
    USE
    =INKEY(.2)
  ENDIF
ENDFOR
WAIT WINDOW "Zap of printer drivers complete !" TIME 1

ERASE C:\setwin16.prq

CLOSE DATA
USE savmelog EXCL
ZAP

CLOSE DATA
USE configav
REPLA configav.hmt        WITH 0
REPLA configav.frepairnam WITH SPAC(0)
REPLA configav.fcompname  WITH SPAC(0)
REPLA configav.fsysserial WITH [0000]
REPLA configav.prodstat   WITH [EVALUATION] 
REPLA configav.evrprname  WITH [First CONFIGURE your COMPANY Name ! ]
REPLA configav.evcmpname  WITH [First CONFIGURE your REPAIR Name !  ]
REPLA configav.regrprname WITH SPAC(0)
REPLA configav.regcmpname WITH SPAC(0)
REPLA configav.fire       WITH {}
REPLA configav.extendeval WITH .f.
REPLA configav.alt337name WITH .f.
REPLA configav.wbshow_neg WITH .t.
CLOSE DATA
USE configlc
REPLA configlc.prnpreview WITH [Prompt]
REPLA configlc.prnprompt  WITH .t.
REPLA configlc.showtips   WITH .t.
REPLA configlc.wblookups  WITH .t.
REPLA configlc.f813blk3   WITH .t.
REPLA configlc.page_tab   WITH .f.
CLOSE DATA
CD (curpath)

