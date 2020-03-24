zLSwitchGL1 = .T.
RETURN
SET SAFETY OFF
SET TALK ON
SET DELE ON
SET CENT ON
CLOSE DATA ALL
IF 1=1  && ONLY for testing, don't put into UPDATER
  *// update codes in avinacct, avajacct, avpoacct, ccmacct, rcinacct, vadjacct
  *// avchartp, avmonthl, glaccnts, glmonth, glsegmnt
  SELE 0
  USE glaccnts
  ZAP
  USE IN glaccnts  
  SELE 0
  USE glsegmnt
  ZAP
  USE IN glsegmnt  
  SELE 0
  USE glmonth
  ZAP
  USE IN glmonth
  SELE 0
  USE avchartp
  ZAP
  APPE FROM c:\tmp\ctiavm2000\data\avchartp
  USE  
  SELE 0
  USE avmonthl
  ZAP
  APPE FROM c:\tmp\ctiavm2000\data\avmonthl
  USE  

  SELE 0
  USE avinacct
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\avinacct
  USE  
  SELE 0
  USE avajacct
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\avajacct
  USE  
  SELE 0
  USE avpoacct
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\avpoacct
  USE  
  SELE 0
  USE ccmacct
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\ccmacct
  USE  
  SELE 0
  USE rcinacct
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\rcinacct
  USE  
  SELE 0
  USE vadjacct
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\vadjacct
  USE  

  SELE 0
  USE avinvoic
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\avinvoic
  USE  

  SELE 0
  USE avipused
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\avipused
  USE  

  SELE 0
  USE avpayble
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\avpayble
  USE  

  SELE 0
  USE avpoline
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\avpoline
  USE  

  SELE 0
  USE custadjs
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\custadjs
  USE  

  SELE 0
  USE custcmem
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\custcmem
  USE  

  SELE 0
  USE vendadjs
  ZAP
  APPEN FROM c:\tmp\ctiavm2000\data\vendadjs
  USE  

  CLOSE DATA ALL
ENDIF

IF zLSwitchGL1 .and. FILE("glaccnts.dbf")  && one time prepare new g/l system  v4.75
  IF FILE("glsegmnt.dbf")
    SELE 0
    USE glsegmnt
    FOR jjtt = 1 TO 100
      jSeg = STRTR(STR(jjtt-1,2),[ ],[0])  && 00 - 99
      LOCA FOR glsegmnt.glseg = jSeg
      IF !FOUND()
        APPE BLANK
        REPL glsegmnt.glseg WITH jSeg IN glsegmnt
      ENDIF
      IF glseg = [00]
        REPLA glsegmnt.desc WITH [Main], glsegmnt.longdesc WITH [Reserved for MAIN account] IN glsegmnt
      ENDIF
    ENDFOR
  ENDIF
  USE IN glsegmnt
  
  SELE 0
  USE glaccnts
  SELE 0
  USE avchartp
  SELE avchartp  && copy existing coa's to new gl master
  SCAN
    IF avchartp.chnum = 9999
      LOOP
    ENDIF
    SELE glaccnts
    LOCA FOR glaccnts.chitemnum = avchartp.chitemnum 
    IF !FOUND()
      cNewMaj = STRTR(STR(avchartp.chnum,4),[ ],[0])
      SELE glaccnts
      APPE BLANK
      REPL chitemnum WITH avchartp.chitemnum , comments WITH avchartp.chcomm ;
        ,desc WITH avchartp.chdesc, glid WITH cNewMaj+[0-00-00], dlast WITH avchartp.chlused;
        ,grandtotal WITH avchartp.chsumamt, linactive WITH .F.;
        ,type WITH IIF(BETW(avchartp.chitemnum,100,175),[Expense],[Income])
    ENDIF
    SELE avchartp
  ENDSCAN
  USE IN glaccnts
  
  SELE 0
  USE avmonthl  && make a single record for each month / accnt instead of avmonthl
  SELE 0
  USE glmonth
  SELE avmonthl  && scan all months
  SCAN
    FOR qqq = 101 TO 250  && process all current 150 coas make a rec if any value
      SELE avmonthl
      mhcode = 'CH'+STR(qqq,3)  && create mvar/fieldname in monthly.dbf
      jAmt = avmonthl.&mhcode
      IF !EMPT(mhcode) .and. jAmt > 0
        SELE avchartp
        LOCA FOR avchartp.chitemnum = qqq  && get the users accnt #
        IF FOUND() .and. (chnum # 9999)
          SELE glmonth
          APPE BLANK
          cNewMaj = STRTR(STR(avchartp.chnum,4),[ ],[0])
          REPL yyyymm WITH avmonthl.mhy_m, chitemnum WITH qqq, glid WITH cNewMaj+[0-00-00], chamt WITH jAmt
        ENDIF
      ENDIF
    ENDFOR
    SELE avmonthl
  ENDSCAN
  USE IN avchartp
  USE IN glmonth
  USE IN avmonthl
  *// update codes in avinacct, avajacct, avpoacct, ccmacct, rcinacct, vadjacct

  SELE 0
  USE gltrans
  ZAP

  SELE 0
  USE avinvoic ORDER iinvnum
  SELE 0
  USE avinacct
  SCAN
    REPL avinacct.glid WITH STRTR(STR(avinacct.accNtnum,4),[ ],[0])+[0-00-00]
    SELE avinvoic
    LOCATE FOR avinvoic.iinvnum = avinacct.invnum
    IF FOUND() .and. avinvoic.dposted # {} &&SEEK(avinacct.invnum,[avinvoic],[iinvnum]) .and. avinvoic.dposted # {}
      REPL avinacct.dposted WITH avinvoic.idate, avinacct.yyyymm WITH LEFT(DTOS(avinvoic.idate),6)
    ENDIF
    SELE avinacct
    SCAT FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,usr_descr,yyyymm TO zp
    SELE gltrans
    APPE BLANK
    GATH FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,usr_descr,yyyymm FROM zp
    REPL source WITH [IN], idnum WITH avinacct.invnum
    SELE avinacct
  ENDSCAN
  USE IN avinacct
  USE IN avinvoic
  
  SELE 0
  USE custadjs
  SELE 0
  USE avajacct
  SCAN
    REPL avajacct.glid WITH STRTR(STR(avajacct.accNtnum,4),[ ],[0])+[0-00-00]
    SELE custadjs
    LOCATE FOR custadjs.idnum = avajacct.idnum
    IF FOUND() .and. custadjs.dposted # {} &&SEEK(avajacct.idnum,[custadjs],[idnum]) .and. custadjs.dposted # {}
      REPL avajacct.dposted WITH custadjs.tdate, avajacct.yyyymm WITH LEFT(DTOS(custadjs.tdate),6)
    ENDIF
    SELE avajacct
    SCAT FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,idnum,usr_descr,yyyymm TO zp
    SELE gltrans
    APPE BLANK
    GATH FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,idnum,usr_descr,yyyymm FROM zp
    REPL source WITH [CA]
    SELE avajacct
  ENDSCAN
  USE IN avajacct
  USE IN custadjs


  SELE 0
  USE avpayble 
  SELE 0
  USE avpoacct
  SCAN
    REPL avpoacct.glid WITH STRTR(STR(avpoacct.accNtnum,4),[ ],[0])+[0-00-00]
    SELE avpayble
    LOCATE FOR avpayble.ypaynum = avpoacct.po_link
    IF FOUND() .and. avpayble.dposted # {} &&SEEK(avpoacct.po_link,[avpayble],[ypaynum_a]) .and. avpayble.dposted # {}
      REPL avpoacct.dposted WITH avpayble.yitemdate, avpoacct.yyyymm WITH LEFT(DTOS(avpayble.yitemdate),6)
    ENDIF
    SELE avpoacct
    SCAT FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,usr_descr,yyyymm TO zp
    SELE gltrans
    APPE BLANK
    GATH FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,usr_descr,yyyymm FROM zp
    REPL source WITH [PE], idnum WITH avpoacct.po_link
    SELE avpoacct
  ENDSCAN
  USE IN avpoacct
  USE IN avpayble


  SELE 0
  USE custcmem 
  SELE 0
  USE ccmacct
  SCAN
    REPL ccmacct.glid WITH STRTR(STR(ccmacct.accNtnum,4),[ ],[0])+[0-00-00]
    SELE custcmem
    LOCATE FOR custcmem.idnum = ccmacct.idnum
    IF FOUND() .and. custcmem.dposted # {} &&SEEK(ccmacct.idnum,[custcmem],[idnum]) .and. custcmem.dposted # {}
      REPL ccmacct.dposted WITH custcmem.tdate, ccmacct.yyyymm WITH LEFT(DTOS(custcmem.tdate),6)
    ENDIF
    SELE ccmacct
    SCAT FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,idnum,usr_descr,yyyymm TO zp
    SELE gltrans
    APPE BLANK
    GATH FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,idnum,usr_descr,yyyymm FROM zp
    REPL source WITH [CM]
    SELE ccmacct
  ENDSCAN
  USE IN ccmacct
  USE IN custcmem


  SELE 0
  USE rcinvoic 
  SELE 0
  USE rcinacct
  SCAN
    REPL rcinacct.glid WITH STRTR(STR(rcinacct.accNtnum,4),[ ],[0])+[0-00-00]
    SELE rcinvoic
    LOCATE FOR rcinvoic.iinvnum = rcinacct.invnum
    IF FOUND() .and. rcinvoic.dposted # {} &&SEEK(rcinacct.invnum,[rcinvoic],[iinvnum]) .and. rcinvoic.dposted # {}
      REPL rcinacct.dposted WITH rcinvoic.idate, rcinacct.yyyymm WITH LEFT(DTOS(rcinvoic.idate),6)
    ENDIF
    SELE rcinacct
    SCAT FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,usr_descr,yyyymm TO zp
    SELE gltrans
    APPE BLANK
    GATH FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,usr_descr,yyyymm FROM zp
    REPL source WITH [RC], idnum WITH rcinacct.invnum
    SELE rcinacct
  ENDSCAN
  USE IN rcinacct
  USE IN rcinvoic


  SELE 0
  USE vendadjs
  SELE 0
  USE vadjacct
  SCAN
    REPL vadjacct.glid WITH STRTR(STR(vadjacct.accNtnum,4),[ ],[0])+[0-00-00]
    SELE vendadjs
    LOCATE FOR vendadjs.idnum = vadjacct.idnum
    IF FOUND() .and. vendadjs.dposted # {} &&SEEK(vadjacct.idnum,[vendadjs],[idnum]) .and. vendadjs.dposted # {}
      REPL vadjacct.dposted WITH vendadjs.tdate, vadjacct.yyyymm WITH LEFT(DTOS(vendadjs.tdate),6)
    ENDIF
    SELE vadjacct
    SCAT FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,idnum,usr_descr,yyyymm TO zp
    SELE gltrans
    APPE BLANK
    GATH FIEL accntnum,amt,chitemnum,demo,descrip,dposted,glid,idnum,usr_descr,yyyymm FROM zp
    REPL source WITH [VA]
    SELE vadjacct
  ENDSCAN
  USE IN vadjacct
  USE IN vendadjs


ENDIF
