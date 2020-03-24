CLOSE DATA
CREATE DBF C:\caes_wBlankMcodeUsedInLastYear ( cae C(25),origmfg C(25), mcode c(10), dorder D(8), dpurch D(8), ncaerecno N(14))
SELE 0
USE F:\nacfox\poline ORDER pomfg
SELE 0
USE F:\nacfox\qgen ORDER genquote
SELE 0
USE F:\nacfox\qmat ORDER mfg
SELE 0
USE F:\nacfox\qvlxref ORDER qvlmfg
COUN FOR EMPT(mcode) TO n1
? [TO START,  CAE FILE CONTAINS ]+STR(n1,8)+[  blank mcodes]
SCAN
  IF !EMPT(mcode) .or. EMPT(linkmfg).or.EMPT(qvlmfg)
    LOOP
  ENDIF
  nrecno = RECN("qvlxref")
  SELE qmat
  SEEK qvlxref.linkmfg
  STORE .F. TO lfqmat,lfpol
  SCAN WHILE qmat.mfg = qvlxref.linkmfg
    IF SEEK(qmat.quote,[qgen],[genquote]) .and. BETW(qgen.ldate,GOMO(DATE(),-12),DATE())
      lfqmat = .T.
      EXIT
    ENDIF
  ENDSCAN
  SELE poline
  SEEK qvlxref.linkmfg
  SCAN WHILE poline.mfg = qvlxref.linkmfg
    IF BETW(poline.rdate,GOMO(DATE(),-12),DATE())
      lfpol = .T.
      EXIT
    ENDIF
  ENDSCAN
  IF lfqmat .or. lfpol
    SELE caes_wBlankMcodeUsedInLastYear
    APPEND BLANK
    REPL origmfg WITH qvlxref.linkmfg, cae WITH qvlxref.qvlmfg, ncaerecno WITH nrecno
    IF lfqmat
      REPL dorder WITH qgen.ldate IN caes_wBlankMcodeUsedInLastYear
    ENDIF
    IF lfpol
      REPL dpurch WITH poline.rdate IN caes_wBlankMcodeUsedInLastYear
    ENDIF
  ENDIF
  SELE qvlxref
ENDSCAN
SELE caes_wBlankMcodeUsedInLastYear
BROW
&& // now try to fill in mcodes if cae exists in inventory
SELE 0
USE F:\nacfox\inv ORDER invmfg
SELE caes_wBlankMcodeUsedInLastYear
SCAN
  IF SEEK(caes_wBlankMcodeUsedInLastYear.cae,[inv],[invmfg]).and.!EMPT(inv.mcode)
    REPL mcode WITH inv.mcode IN caes_wBlankMcodeUsedInLastYear
  ENDIF
ENDSCAN
SELE caes_wBlankMcodeUsedInLastYear
BROW

&& Now update the cae qvlxref file with the Inventory mcode
SELE caes_wBlankMcodeUsedInLastYear
SCAN
  nxr = caes_wBlankMcodeUsedInLastYear.ncaerecno
  SELE qvlxref
  GO nxr
  IF EMPT(qvlxref.mcode).and.qvlxref.linkmfg=caes_wBlankMcodeUsedInLastYear.origmfg .and. qvlxref.qvlmfg=caes_wBlankMcodeUsedInLastYear.cae
    REPL mcode WITH caes_wBlankMcodeUsedInLastYear.mcode IN qvlxref
  ENDIF
ENDSCAN
SELE qvlxref
COUN FOR EMPT(mcode) TO n2
? [FINISHED,   CAE FILE CONTAINS ]+STR(n2,8)+[  blank mcodes]
