CLOSE DATA
SELE 0
USE V:\nacfox\inv
SELE 0
USE V:\nacfox\attrit01
SELE 0
USE V:\nacfox\smt_data
SELE inv
STORE 0 TO n1,n2,n3
SCAN
  IF [SMT] $ inv.prod_code
    LOOP
  ENDIF
  Lsmt = .F.
  IF !Lsmt .and. [,SMT] $ inv.desc
    Lsmt = .T.
    n1 = n1 + 1
  ENDIF
  IF !Lsmt  
    SELE attrit01
    SCAN
      IF !EMPT(attrit01.desc).and. ALLT(UPPE(attrit01.desc)) $ UPPE(inv.desc)
        Lsmt = .T.
        n2 = n2 + 1
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  IF !Lsmt
    SELE smt_data
    SCAN
      IF INLI(inv.mfg,smt_data.mfg,smt_data.bom_mfg,smt_data.gun_mfg)
    *IF SEEK(inv.mfg,[smt_data],[MFG2]).or. SEEK(inv.mfg,[smt_data],[BOM_MFG2]).or. SEEK(inv.mfg,[smt_data],[GUN_MFG2])
        Lsmt = .T.
        n3 = n3 + 1
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  IF Lsmt
    REPL prod_code WITH [SMT] IN inv
    ? [SMT in Inv,Desc ]+STR(n1,8)+[   Attrit ]+STR(n2,8)+[  SMT DATA ]+STR(n3,8) +[   ]+ inv.mfg
  ENDIF
ENDSCAN
WAIT [SMT in Inv,Desc ]+STR(n1,8)+[   Attrit ]+STR(n2,8)+[  SMT DATA ]+STR(n3,8) WIND
CLOSE DATA
