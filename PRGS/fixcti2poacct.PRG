SELE 0
USE n:\SVM2000\data\avajacct
SELE 0
USE n:\SVM2000\data\avchartp
SELE avajacct
SCAN
  IF !EMPT(chitemnum)
    LOOP
  ENDIF
    SELE avchartp
    LOCATE FOR avchartp.chnum = avajacct.accntnum
    IF FOUND()
      zChitemnum = avchartp.chitemnum
    ELSE
      zChitemnum = 0
    ENDIF
    SELE avajacct
    REPLACE avajacct.chitemnum  WITH zChitemnum
ENDSCAN
CLOSE DATA
