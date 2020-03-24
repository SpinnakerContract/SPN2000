*!*	  REPL ref_desig WITH STRTR(ALLT(ref_desig),CHR(13),[])+[,] IN tmp_bom
*!*	  REPL ref_desig WITH STRTR(ALLT(ref_desig),CHR(10),[]) IN tmp_bom
*!*	  =ThisForm._zCleanRefDesigs()
*!*	  SELE tmp_bom
*!*	  IF !RIGHT(ALLT(ref_desig),1)=[,]
*!*	    REPL ref_desig WITH ref_desig+[,] IN tmp_bom
*!*	  ENDIF
LPARA zrd
SET SAFETY OFF
SET TALK ON
CLOSE DATA
SELE 0
USE c:\nacvfp\configlc
CREATE DBF c:\tmp_bom (ref_desig M(10))
SELE tmp_bom
APPE BLANK
REPL ref_desig WITH zrd

REPL ref_desig WITH STRTR(ALLT(UPPE(ref_desig)),CHR(13)+CHR(10),[]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,CHR(13),[]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,CHR(10),[]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,[ -],[-]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,[- ],[-]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,[--],[-]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,[   ],[ ]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,[  ],[ ]) IN tmp_bom
*// convert punctuation
REPL ref_desig WITH STRTR(ref_desig,[ ],[,]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,[,,],[,]) IN tmp_bom
IF RIGHT(tmp_bom.ref_desig,1)=[,] && strip trailing comma
  nLen = LEN(ALLT(tmp_bom.ref_desig))
  REPL ref_desig WITH LEFT(tmp_bom.ref_desig,nLen-1) IN tmp_bom
ENDIF
REPL ref_desig WITH ref_desig+[,] IN tmp_bom && add one at end on purpose, strip out again below
SELE configlc
REPL fdumbmem2 WITH [] IN configlc
nCommas = OCCUR([,],tmp_bom.ref_desig)
jtxt = []
IF nCommas>1  &&OCCURS([-],jtxt)>0
  FOR jjj = 1 TO nCommas
    DO CASE
      CASE jjj = 1
        jtxt = STRTR( SUBS( tmp_bom.ref_desig ,1 ,AT([,] ,tmp_bom.ref_desig,1) ), [,] ,[])
        *// starting @ 1 until comma, stripping the comma
      OTHERWISE
        c1 = AT([,] ,tmp_bom.ref_desig, jjj-1)
        c2 = AT([,] ,tmp_bom.ref_desig, jjj)
        jtxt = STRTR( SUBS( tmp_bom.ref_desig ,c1 ,(c2-c1)+1 ), [,] ,[])
    ENDCASE
    *// now we have individual chunks
    IF OCCURS([-],jtxt)=1 .and. ISALPHA(LEFT(jtxt,1))
      c1=[]
      FOR qqq = 1 TO LEN(jtxt)
        c2 = SUBS(jtxt,qqq,1)
        IF ISALPHA(c2)
          c1 = c1 + c2
        ELSE
          EXIT
        ENDIF
      NEXT
      jtxt2 = STRTR(jtxt,c1,[])
      *? jtxt2  && 1-7
      jtxt3 = []
      a1  = AT([-],jtxt2)
      n1  = VAL( SUBS(jtxt2,1,a1-1) )
      n2  = VAL( ALLT(SUBS(jtxt2,a1+1,10)) )
      cnt = MAX(0,(n2-n1)+1)
      *?[a1 = ]+STR(a1,3)+[  n1 = ]+STR(n1,3)+[  n2 = ]+STR(n2,4) 
      FOR ttt = n1 TO n2
        jtxt3 = jtxt3 + c1+ALLT(STR(ttt))
        IF ttt<n2
          jtxt3 = jtxt3 + [,]
        ENDIF
      ENDFOR
      jtxt = jtxt3
    ENDIF
    REPL fdumbmem2 WITH fdumbmem2 + IIF(jjj>1,[,],[])+jtxt IN configlc && build up in temp memo field
  ENDFOR
  REPL ref_desig WITH UPPE(ALLT(configlc.fdumbmem2)) IN tmp_bom
ELSE
  jtxt = ALLT(tmp_bom.ref_desig)
  IF OCCURS([-],jtxt)>0 && no commas
    IF OCCURS([-],jtxt)=1 .and. ISALPHA(LEFT(jtxt,1))
      *c1 = LEFT(jtxt,1)
      c1=[]
      FOR qqq = 1 TO LEN(jtxt)
        c2 = SUBS(jtxt,qqq,1)
        IF ISALPHA(c2)
          c1 = c1 + c2
        ELSE
          EXIT
        ENDIF
      NEXT
      jtxt2 = STRTR(jtxt,c1,[])
      *? jtxt2  && 1-7
      jtxt3 = []
      a1  = AT([-],jtxt2)
      n1  = VAL( SUBS(jtxt2,1,a1-1) )
      n2  = VAL( ALLT(SUBS(jtxt2,a1+1,10)) )
      cnt = MAX(0,(n2-n1)+1)
      *?[a1 = ]+STR(a1,3)+[  n1 = ]+STR(n1,3)+[  n2 = ]+STR(n2,4) 
      FOR ttt = n1 TO n2
        jtxt3 = jtxt3 + c1+ALLT(STR(ttt))
        IF ttt<n2
          jtxt3 = jtxt3 + [,]
        ENDIF
      ENDFOR
      jtxt = jtxt3
    ENDIF
    *? [jtxt ]+jtxt
  ENDIF
  REPL ref_desig WITH jtxt IN tmp_bom
ENDIF
IF RIGHT(tmp_bom.ref_desig,1)=[,] && strip trailing comma
  nLen = LEN(ALLT(tmp_bom.ref_desig))
  REPL ref_desig WITH LEFT(ref_desig,nLen-1) IN tmp_bom
ENDIF
SELE tmp_bom
*// convert punctuation
cnumstr = "01234567890"
cchrstr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
cpunstr = '-.()/\:;*_%^&$#@!+=<>' && no commas
jlen = LEN(ref_desig)
jrd  = ref_desig
IF jlen > 2
  FOR uuu = 1 TO jlen - 1
    str2 = SUBS(jrd,uuu,2)
    stra = LEFT(str2,1)
    strb = RIGHT(str2,1)
      *WAIT jrd+[      ]+str2+[      ]+stra+[    ]+strb WIND
    IF stra$cnumstr .and. strb$cpunstr
      *WAIT jrd+[      ]+str2+[      ]+stra+[    ]+strb WIND
      ** example  R13,R14(SIDE1)  put a comma after R14
      jrd = STUFF(jrd,uuu+1,1,","+strb)
    ENDIF
  ENDFOR
ENDIF
IF jrd # ref_desig
  REPL ref_desig WITH jrd IN tmp_bom
ENDIF
REPL ref_desig WITH STRTR(ref_desig,[  ],[ ]) IN tmp_bom
*? tmp_bom.ref_desig
*// now parse through and remove undesired items
jxrd1 = [,]+ALLT(ref_desig)+[,] && mvar of curr rd
jxrd2 = ALLT(ref_desig) && mvar of curr rd
*REPL fdumbmem2 WITH [] IN configlc
jcnt = OCCUR(",",jxrd1)
**? STR(jcnt,2)+[ commas]
FOR ttt = 1 TO jcnt-1
  n1 = AT(",",jxrd1,ttt)
  n2 = AT(",",jxrd1,ttt+1)
  jpiece = SUBS(jxrd1,n1,n2-n1+1) && now have single piece surrounded by commas
  jpiece2=jpiece
  jpiece = STRTR(jpiece,",","") && strip commas
  jlen   = LEN(jpiece)
  STORE 0 TO nnum,nchr,npun
  FOR yyy = 1 TO jlen && scan through all chars
    jstr = SUBS(jpiece,yyy,1) && 
    IF jstr=","
      LOOP
    ENDIF
    nnum = nnum + IIF(jstr$cnumstr,1,0)
    nchr = nchr + IIF(jstr$cchrstr,1,0)
    npun = npun + IIF(jstr$cpunstr,1,0)
  ENDFOR
  * now STRTR jpiece if not wanted
  IF npun>0.or.nnum=0.or.nchr=0 &&.or.nchr>2
    jxrd2 = STRTR(jxrd2,[,]+jpiece,[])
  ENDIF
ENDFOR
SELE tmp_bom
IF ALLT(jxrd2) # ALLT(tmp_bom.ref_desig)
  REPL ref_desig WITH ALLT(jxrd2) IN tmp_bom
ENDIF
REPL ref_desig WITH STRTR(ref_desig,[,,],[,]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,[T/H],[]) IN tmp_bom
REPL ref_desig WITH STRTR(ref_desig,[STEP],[]) IN tmp_bom
IF [(]$ref_desig.or.[)]$ref_desig.or.[/]$ref_desig.or.[\]$ref_desig
  *cpunstr = '-.()/\:;*_%^&$#@!+=<>' && no commas
  REPL ref_desig WITH STRTR(ref_desig,[(],[]) IN tmp_bom
  REPL ref_desig WITH STRTR(ref_desig,[)],[]) IN tmp_bom
  REPL ref_desig WITH STRTR(ref_desig,[/],[]) IN tmp_bom
  REPL ref_desig WITH STRTR(ref_desig,[\],[]) IN tmp_bom
ENDIF
? ALLT(tmp_bom.ref_desig)+[*]
CLOSE DATA