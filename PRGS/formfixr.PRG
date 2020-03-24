CD C:\SVM\forms
CLOSE DATA ALL
zfile=SYS(2000,[*.scx])
? zfile
DO WHILE .t.
  zfile=SYS(2000,[*.scx],1)
  IF LEN(ALLT(zfile)) < 3
    EXIT
  ENDIF
  IF LEFT(zFile,4) # [SAVM]
    wtd=MESSAGEBOX([File   ]+zFile,3)
    IF wtd=2.or.LAST()=27
      EXIT
    ENDIF
    IF wtd=7
      LOOP
    ENDIF
    ? zfile
    USE (zfile)
    SCAN
      IF [..\..\VPM51\VPMFORMS]$UPPE(classloc)
        ?? [   CLASSLOC = ]+ALLT(classloc)
        *REPLACE classloc WITH [..\proforms.vcx]
        *=INKEY(1)
      ENDIF
      IF [..\..\VPM51\VPMCTRLS]$UPPE(classloc)
        ?? [   CLASSLOC = ]+ALLT(classloc)
        *REPLACE classloc WITH [..\proctrls.vcx]
        *=INKEY(1)
      ENDIF
      IF [..\..\VPM5\VPMFORMS]$UPPE(classloc)
        ?? [   CLASSLOC = ]+ALLT(classloc)
        *REPLACE classloc WITH [..\proforms.vcx]
        *=INKEY(1)
      ENDIF
      IF [..\..\VPM5\VPMCTRLS]$UPPE(classloc)
        ?? [   CLASSLOC = ]+ALLT(classloc)
        *REPLACE classloc WITH [..\proctrls.vcx]
        *=INKEY(1)
      ENDIF
    ENDSCAN
    USE
  ENDIF
ENDDO