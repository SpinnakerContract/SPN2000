PARA zdbf
CLOSE DATA
USE CONFIGLC IN 0
SELE 0
USE (zdbf)
zstr=[]
FOR ttt = 1 TO fcount()
  zstr=zstr+FIELD(ttt)+[,]
ENDFOR
REPLA configlc.fdumbmemo WITH zstr
MODI MEMO configlc.fdumbmemo
CLOSE DATA