SET TALK ON

jTxt = [R123-R129]
c1 = LEFT(jtxt,1)
jtxt2 = STRTR(jtxt,c1,[])
jtxt3 = []
a1  = AT([-],jtxt2)
n1  = VAL( SUBS(jtxt2,1,a1-1) )
n2  = VAL( ALLT(SUBS(jtxt2,a1+1,10)) )
cnt = MAX(0,(n2-n1)+1)
FOR ttt = n1 TO n2
  jtxt3 = jtxt3 + c1+ALLT(STR(ttt))
  IF ttt<n2
    jTxt3 = jtxt3 + [,]
  ENDIF
ENDFOR
? jtxt
? jtxt3