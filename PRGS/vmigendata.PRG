CLOSE DATA ALL
SET SAFETY OFF
CREAT DBF C:\NACVFP\TEMP\VMIDETAILS (mfg C(25),desc C(35),manu C(30),qty N(14,2),needby D(8),ucost N(14,3),ext N(14,4),qnum N(10), lotnum C(6),po C(20), qty2 N(14,2),ext2 N(14,4))
SELE 0
USE F:\NACFOX\inv ORDER invmfg
SELE 0
USE F:\NACFOX\batchprt ORDER sku
SELE 0
USE F:\NACFOX\mcode ORDER mcode
SELE 0
USE F:\NACFOX\qgen
SELE 0
USE F:\NACFOX\qmat
SET ORDER TO mfg IN qmat
SELE 0
USE F:\NACFOX\quote_m
SELE 0
USE F:\NACFOX\quote_b
SET ORDER TO pnum IN quote_b
SELE 0
USE F:\NACFOX\vmi
SELE vmi
SCAN
  SELE quote_b
  SEEK vmi.mfg
  SCAN WHILE quote_b.pnum = vmi.mfg
    IF SEEK(STR(quote_b.qnum,10),[quote_m],[qnum]) .and.quote_m.cmnem=[CEL002    ].and.("2014CONTRACT" $ quote_m.po .or. "2014.1CONTRACT" $ quote_m.po)
      APPEND BLANK IN vmidetails
      REPL mfg WITH quote_b.pnum,qty WITH MAX(0,quote_m.qty-quote_m.sent2fox)*quote_b.qty,ext WITH (MAX(0,quote_m.qty-quote_m.sent2fox)*quote_b.qty) * vmi.ucost;
        ,needby WITH {02/01/2014},ucost WITH vmi.ucost, qnum WITH quote_b.qnum,po WITH quote_m.po IN vmidetails
      IF SEEK(quote_b.pnum,[inv],[invmfg])
        REPL desc WITH inv.desc IN vmidetails
        IF SEEK(inv.mcode,[mcode],[mcode])
          REPL manu WITH mcode.company IN vmidetails
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  SELE qmat
  SEEK vmi.mfg
  SCAN WHILE qmat.mfg = vmi.mfg
    IF SEEK(qmat.job,[qgen],[genjob]) .and.qgen.code=[CEL002    ].and.("2014CONTRACT" $ qgen.po .or. "2014.1CONTRACT" $ qgen.po)
      APPEND BLANK IN vmidetails
      REPL mfg WITH qmat.mfg,qty WITH MAX(0,qgen.bal)*qmat.perunit,ext WITH (MAX(0,qgen.bal)*qmat.perunit)* vmi.ucost;
        ,needby WITH qgen.ddate1,ucost WITH vmi.ucost, lotnum WITH qgen.job,po WITH qgen.po IN vmidetails
      REPL qty WITH MAX(0,(MAX(0,qgen.qty)*qmat.perunit)-qmat.used-qmat.qvused),ext WITH (MAX(0,(MAX(0,qgen.qty)*qmat.perunit)-qmat.used-qmat.qvused)) * vmi.ucost IN vmidetails
      IF SEEK(qmat.sku,[batchprt],[sku]) .and. SEEK(batchprt.mcode,[mcode],[mcode])
        REPL manu WITH mcode.company IN vmidetails
      ENDIF
      IF SEEK(qmat.mfg,[inv],[invmfg])
        REPL desc WITH inv.desc IN vmidetails
        IF SEEK(inv.mcode,[mcode],[mcode])
          REPL manu WITH mcode.company IN vmidetails
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  SELE vmi
ENDSCAN
SELE vmidetails
BROW
SELE DIST mfg, 000000000 AS qty, SPAC(35) AS desc, SPAC(30) AS manu,{} AS needby , 000000000.00 AS ucost;
  , 0000000000.00 AS ext FROM vmidetails INTO DBF C:\NACVFP\TEMP\VMIDETAILS2 ORDER BY mfg
SELE vmidetails2
SCAN && unique mfg
  SELE vmidetails
  STORE 0 TO q1,e1
  SCAN FOR vmidetails.mfg = vmidetails2.mfg
    q1 = q1 + vmidetails.qty && using values based on "Remaining to be kitted"
    e1 = e1 + vmidetails.ext
    IF EMPT(vmidetails2.desc)
      REPL desc WITH vmidetails.desc, manu WITH vmidetails.manu,ucost WITH vmidetails.ucost IN vmidetails2
    ENDIF
  ENDSCAN
  SELE vmidetails2
  REPL vmidetails2.qty WITH q1,ext WITH e1 IN vmidetails2
ENDSCAN
SELE vmidetails2
BROW
CREAT DBF C:\NACVFP\TEMP\VMIDETAILS3 (mfg C(25),desc C(35),manu C(30),needby D(8),qty N(10),ucost N(14,2),ext N(14,2))  &&, qty2 N(14,2),cost2 N(14,4))
SELE vmidetails2
SCAN
  STORE 0 TO q2,e2
  q2 = ROUND( CEIL(vmidetails2.qty / 11),0)
  FOR jjj = 2 TO 12
    d1 = CTOD(STRTR(STR(jjj,2),[ ],[0])+[/01/2014])
    SCAT MEMVAR
    SELE vmidetails3
    APPE BLANK
    GATH memvar
    REPL needby WITH d1,qty WITH q2,ext WITH q2*ucost IN vmidetails3
  ENDFOR
  SELE vmidetails2
ENDSCAN
SELE vmidetails3
COPY TO C:\spcovmi2014.XLS TYPE XLS
BROW