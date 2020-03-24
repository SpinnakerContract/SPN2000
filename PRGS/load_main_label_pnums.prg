CLOSE DATA
SET EXCL OFF
USE V:\NACFOX\MAIN_M IN 0 ORDER PART
** cust_part2 is ACTUAL CUST PART#
SCAN
  IF EMPT(main_m.label_part)
    zactpart=_zdroptees(main_m.part)
    REPL label_part WITH ALLT(UPPE(zactpart)) IN main_m
  ENDIF
ENDSCAN
BROWS FIEL code,part,label_part

PROCEDURE _zdroptees
LPARAM zpart23
PRIV zret23
zret23 = zpart23
t2=RIGHT(ALLT(zpart23),2)
t3=RIGHT(ALLT(zpart23),3)
t4=RIGHT(ALLT(zpart23),4)
t5=RIGHT(ALLT(zpart23),5)
IF t2 = [-C]
  zret23 = STRTR(zpart23,[-C],[])
ENDIF
IF t2 = [-T]
  zret23 = STRTR(zpart23,[-T],[])
ENDIF
IF t2 = [-S]
  zret23 = STRTR(zpart23,[-S],[])
ENDIF
IF t2 = [-R]
  zret23 = STRTR(zpart23,[-R],[])
ENDIF
IF t3 = [-TT]
  zret23 = STRTR(zpart23,[-TT],[])
ENDIF
IF t3 = [-T1]
  zret23 = STRTR(zpart23,[-T1],[])
ENDIF
IF t3 = [-T2]
  zret23 = STRTR(zpart23,[-T2],[])
ENDIF
IF t3 = [-T3]
  zret23 = STRTR(zpart23,[-T3],[])
ENDIF
IF t4 = [-TTT]
  zret23 = STRTR(zpart23,[-TTT],[])
ENDIF
IF t4 = [-RMA]
  zret23 = STRTR(zpart23,[-RMA],[])
ENDIF
IF t5 = [-TEST]
  zret23 = STRTR(zpart23,[-TEST],[])
ENDIF
IF t5 = [-CONV]
  zret23 = STRTR(zpart23,[-CONV],[])
ENDIF
zret23 = PADR(ALLT(zret23),19)
RETURN(zret23)
