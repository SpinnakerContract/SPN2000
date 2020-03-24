*// target table must be open
IF EMPT(ALIAS())
  =MESSAGEBOX([Target table must be open and selected])
  RETURN
ENDIF
zInitAlias = ALIAS()
IF !USED("savmdd2")
  SELE 0
  USE savmdd2
ENDIF
SET FILT TO zInitAlias+[.DBF] $ file_name
SELE (zInitAlias)
jCnt = FCOUNT()
FOR jjj = 1 TO jcnt
  SELE (zInitAlias)
  jField = ALLT(FIELD(jjj))
  SELE savmdd2
  LOCATE FOR jField $ ALLT(UPPE(savmdd2.field_name))
  IF !FOUND()
    ? LEFT(savmdd2.file_name,20)+[   ]+jField
  ENDIF
ENDFOR