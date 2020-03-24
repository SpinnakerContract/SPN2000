SET EXCLUSIVE OFF
Clear
CLOSE DATABASES ALL
SET TALK OFF
CD C:\Server\nacfox
USE inv IN 0
USE qgen IN 0
USE quote_m IN 0
USE quote_b IN 0
USE qmat IN 0
USE parthist IN 0
USE aparthis IN 0
SET ORDER TO invmfg   IN inv
SET ORDER TO genquote IN qgen
SET ORDER TO pnum     IN quote_b
SET ORDER TO qalloc   IN qmat
SET ORDER TO qnum_num IN quote_m

SELE parthist
SET ORDER TO mfg

SELE quote_b
SET RELATION TO qnum  INTO quote_m

SELE qmat
SET RELATION TO quote INTO qgen

SELE inv
SET RELATION TO mfg  INTO qmat
SET RELATION TO mfg  INTO quote_b

*REPLACE ALL o_tot WITH 0,p_tot WITH 0,no_ext WITH 0
GO TOP
PUBLIC ptot,otot
STORE 0 TO ptot,otot
SELE inv
zcnt7 = 0
@ 1,1 SAY STR(RECCOUNT('inv'))+[  Inventory records]
SCAN
  zcnt7 = zcnt7 + 1
*SELE inv
*=Batch_Qoh(inv.mfg,.T.) &&==== freshen inv.onhand from batches
*REPLACE o_tot WITH 0,p_tot WITH 0,no_ext WITH 0
  IF MOD(RECNO('inv'),5)=0
    @ 2,2 SAY ALLT(STR(zcnt7))+[  Open Orders: ]+inv.mfg+[+]
  ENDIF
  SELE qmat
  otot = 0
  SEEK(SPACE(4)+inv.mfg)
  SCAN WHILE qmat.mfg=inv.mfg
*// may need to exclude if qgen.p_status=[DELIVERED]
*!*	    REPLACE inv.o_tot WITH ;
*!*	      inv.o_tot + MAX(0,((qgen.qty*qmat.perunit)-qmat.used-qmat.qvused))
    otot = otot + MAX(0,((qgen.qty*qmat.perunit)-qmat.used-qmat.qvused))
  ENDSCAN
  SELE inv
  IF inv.o_tot # otot
    REPL inv.o_tot WITH MIN(9999999,otot)
  ENDIF

  SELE quote_b
  IF MOD(RECNO('inv'),5)=0
    @ 4,2 SAY ALLT(STR(zcnt7))+[  Pre-Quotes: ]+inv.mfg
  ENDIF
  SEEK UPPER(inv.mfg)
  ptot = 0
  SCAN WHILE quote_b.pnum=UPPER(inv.mfg)
    ptot = ptot + MAX(0,((quote_m.qty-quote_m.sent2fox) * quote_b.qty))
  ENDSCAN
  SELE inv
  IF inv.p_tot # ptot
    REPL inv.p_tot WITH MIN(9999999,ptot)
  ENDIF

  SELE inv
  ztot = inv.o_tot + inv.p_tot
  IF tot_reqd # ztot
    REPLACE tot_reqd WITH MIN(9999999,ztot)
  ENDIF
  IF inv.onhand > ztot
    REPLACE inv.no_ext WITH (onhand-(ztot))* avgcost
  ELSE
    REPLACE inv.no_ext WITH 0
  ENDIF

  SET ORDER TO mfg DESC IN parthist
  zxcnt = 0
  SELE parthist
  SEEK inv.mfg
  zlu882 = {}
  IF MOD(RECNO('inv'),5)=0
    @ 6,2 SAY ALLT(STR(zcnt7))+[  PartHist LastUsed: ]+inv.mfg
  ENDIF
  SCAN WHILE parthist.partnum = inv.mfg && DESC order - find most recent
    jjlot=SUBS(parthist.ref_misc,4,6)
    IF LEN(ALLT(jjlot))<5 && no valid Lot#
      LOOP
    ENDIF
    IF parthist.qty < 0 .and. LEFT(parthist.ref_misc,4)=[LOT ]
      zlu882 = parthist.date
      EXIT
    ENDIF
  ENDSCAN
  SELE inv
  IF inv.lastused # zlu882
    REPL inv.lastused WITH zlu882
  ENDIF
  IF inv.lastused = {}
    SET ORDER TO mfg DESC IN aparthis
    SELE aparthis
    SEEK inv.mfg
    zlu883 = {}
    IF MOD(RECNO('inv'),5)=0
      @ 8,2 SAY ALLT(STR(zcnt7))+[ ARCHIVE PartHist LastUsed: ]+inv.mfg
    ENDIF
    SCAN WHILE aparthis.partnum = inv.mfg && DESC order - find most recent
      jjlot=SUBS(aparthis.ref_misc,4,6)
      IF LEN(ALLT(jjlot))<5 && no valid Lot#
        LOOP
      ENDIF
      IF aparthis.qty < 0 .and. LEFT(aparthis.ref_misc,4)=[LOT ]
        zlu883 = aparthis.date
        EXIT
      ENDIF
    ENDSCAN
    SELE inv
    IF inv.lastused # zlu883
      REPL inv.lastused WITH zlu882
    ENDIF
  ENDIF
ENDSCAN
CLOSE DATA
