CLOSE DATA
USE V:\NACFOX\QGEN IN 0
** cust_part2 is ACTUAL CUST PART#
SCAN
  ** need to do the suffix thing here
  zactpart=_zdroptees(qgen.part)
  REPL cust_part2 WITH ALLT(UPPE(zactpart)),labelpart WITH ALLT(UPPE(zactpart)) IN qgen
  DO CASE
    CASE qgen.code=[NOY001] .and. qgen.part = [OPM5-54-0001PR]
      REPL custmodel WITH [OPM5-4D] IN qgen
    CASE qgen.code=[NOY001] .and. qgen.part = [OPM5-53-0001PR]
      REPL custmodel WITH [OPM5-3D] IN qgen
    CASE qgen.code=[NOY001] .and. qgen.part = [OPM5-52-0001PR]
      REPL custmodel WITH [OPM5-2D] IN qgen

    CASE qgen.code=[NOY001] .and. qgen.part = [OPM4-54-OCM1PR]
      REPL custmodel WITH [OPM4-4D] IN qgen
    CASE qgen.code=[NOY001] .and. qgen.part = [OPM4-53-OCM1PR]
      REPL custmodel WITH [OPM4-3D] IN qgen
    CASE qgen.code=[NOY001] .and. qgen.part = [OPM4-52-OCM1PR]
      REPL custmodel WITH [OPM4-2D] IN qgen
    CASE qgen.code=[NOY001] .and. qgen.part = [OPM4-51-OCM1PR]
      REPL custmodel WITH [OPM4-1D] IN qgen
  ENDCASE
ENDSCAN
USE IN qgen

USE V:\NACFOX\main_m IN 0
** cust_part2 is ACTUAL CUST PART#
SCAN
  ** need to do the suffix thing here
  zactpart=_zdroptees(main_m.part)
  REPL cust_part2 WITH ALLT(UPPE(zactpart)),label_part WITH ALLT(UPPE(zactpart)) IN main_m
  DO CASE
    CASE main_m.code=[NOY001] .and. main_m.part = [OPM5-54-0001PR]
      REPL custmodel WITH [OPM5-4D] IN main_m
    CASE main_m.code=[NOY001] .and. main_m.part = [OPM5-53-0001PR]
      REPL custmodel WITH [OPM5-3D] IN main_m
    CASE main_m.code=[NOY001] .and. main_m.part = [OPM5-52-0001PR]
      REPL custmodel WITH [OPM5-2D] IN main_m

    CASE main_m.code=[NOY001] .and. main_m.part = [OPM4-54-OCM1PR]
      REPL custmodel WITH [OPM4-4D] IN main_m
    CASE main_m.code=[NOY001] .and. main_m.part = [OPM4-53-OCM1PR]
      REPL custmodel WITH [OPM4-3D] IN main_m
    CASE main_m.code=[NOY001] .and. main_m.part = [OPM4-52-OCM1PR]
      REPL custmodel WITH [OPM4-2D] IN main_m
    CASE main_m.code=[NOY001] .and. main_m.part = [OPM4-51-OCM1PR]
      REPL custmodel WITH [OPM4-1D] IN main_m
  ENDCASE
ENDSCAN
USE IN main_m

USE V:\NACFOX\quote_m IN 0
** cust_part2 is ACTUAL CUST PART#
SCAN
  ** need to do the suffix thing here
  zactpart=_zdroptees(quote_m.pnum)
  REPL cust_part2 WITH ALLT(UPPE(zactpart)) ,label_part WITH ALLT(UPPE(zactpart)) IN quote_m
  DO CASE
    CASE quote_m.cmnem=[NOY001] .and. quote_m.pnum = [OPM5-54-0001PR]
      REPL custmodel WITH [OPM5-4D] IN quote_m
    CASE quote_m.cmnem=[NOY001] .and. quote_m.pnum = [OPM5-53-0001PR]
      REPL custmodel WITH [OPM5-3D] IN quote_m
    CASE quote_m.cmnem=[NOY001] .and. quote_m.pnum = [OPM5-52-0001PR]
      REPL custmodel WITH [OPM5-2D] IN quote_m

    CASE quote_m.cmnem=[NOY001] .and. quote_m.pnum = [OPM4-54-OCM1PR]
      REPL custmodel WITH [OPM4-4D] IN quote_m
    CASE quote_m.cmnem=[NOY001] .and. quote_m.pnum = [OPM4-53-OCM1PR]
      REPL custmodel WITH [OPM4-3D] IN quote_m
    CASE quote_m.cmnem=[NOY001] .and. quote_m.pnum = [OPM4-52-OCM1PR]
      REPL custmodel WITH [OPM4-2D] IN quote_m
    CASE quote_m.cmnem=[NOY001] .and. quote_m.pnum = [OPM4-51-OCM1PR]
      REPL custmodel WITH [OPM4-1D] IN quote_m
  ENDCASE
ENDSCAN
USE IN quote_m



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
