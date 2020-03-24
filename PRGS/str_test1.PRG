LPARA jxrd
SET TALK OFF
CLEAR
? jxrd
jcnt = OCCUR(",",jxrd)
? STR(jcnt,2)+[ commas]
FOR ttt = 1 TO jcnt-1
  n1 = AT(",",jxrd,ttt)
  n2 = AT(",",jxrd,ttt+1)
  ? SUBS(jxrd,n1,n2-n1+1)
ENDFOR
