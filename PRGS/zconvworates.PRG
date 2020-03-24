CLOSE DATA
SELE 0
USE C:\SVM\DATA\configwo
SELE 0
USE C:\SVM\DATA\wo_rates
SELE configwo
FOR jjj = 1 TO 10
  zsuf = STRTR(STR(jjj,2),[ ],[0])
  zcost = [configwo.cost]+zsuf
  zrts  = [configwo.l_srts]+zsuf
  zrate = [configwo.rate]+zsuf
  ztype = [configwo.type]+zsuf
  zname = [configwo.typename]+zsuf
  IF (EMPT(zcost) .or. &zrate = 0)  .and. !BETW(jjj,1,6)
    LOOP
  ENDIF
  SELE wo_rates
  APPE BLANK
  REPL wo_rates.lcti     WITH IIF(BETW(jjj,1,6),.T.,.F.)
  REPL wo_rates.cost     WITH &zcost
  REPL wo_rates.l_srts   WITH &zrts
  REPL wo_rates.rate     WITH &zrate
  REPL wo_rates.type     WITH &ztype
  REPL wo_rates.typename WITH &zname
ENDFOR

