***PROCEDURE ImportCti2 &&--------------------------------
RETURN
CLOSE DATA ALL
SET SAFETY OFF
SET PROC TO DAVMPRCS
DO ImportConfigs  &&//   MUST BE FIRST
DO ImportCustomers
DO ImportEmployees
DO ImportVendors
DO ImportAircraft
DO ImportAircraftTypes
DO ImportParts
DO ImportInvoices  &&// includes avchartp
DO ImportWorkOrders
DO ImportPhrases
DO ImportTestEquip
DO Import8130
DO ImportQuotes
SET SAFETY ON
RETURN

PROCEDURE ImportConfigs &&-----------------------------
CLOSE DATA ALL
SELE 0
USE conftax
ZAP
APPEND FROM n:\foxpro2\gardner\configav FOR RECNO()=1
SELE 0
USE n:\foxpro2\gardner\configav NOUP
SELE 0
USE configwo
REPLACE configwo.type07     WITH SUBS(configav.wo_otypes,01,02)
REPLACE configwo.typename07 WITH SUBS(configav.wo_otypes,04,12)
REPLACE configwo.type08     WITH SUBS(configav.wo_otypes,17,02)
REPLACE configwo.typename08 WITH SUBS(configav.wo_otypes,20,12)
REPLACE configwo.type09     WITH SUBS(configav.wo_otypes,33,02)
REPLACE configwo.typename09 WITH SUBS(configav.wo_otypes,36,12)
REPLACE configwo.type10     WITH SUBS(configav.wo_otypes,49,02)
REPLACE configwo.typename10 WITH SUBS(configav.wo_otypes,52,12)

REPLACE configwo.rate01 WITH configav.wtypeamt1
REPLACE configwo.rate02 WITH configav.wtypeamt2
REPLACE configwo.rate03 WITH configav.wtypeamt3
REPLACE configwo.rate04 WITH configav.wtypeamt4
REPLACE configwo.rate05 WITH configav.wtypeamt5
REPLACE configwo.rate06 WITH configav.wtypeamt6
REPLACE configwo.rate07 WITH configav.wtypeamt7
REPLACE configwo.rate08 WITH configav.wtypeamt8
REPLACE configwo.rate09 WITH configav.wtypeamt9
REPLACE configwo.rate10 WITH configav.wtypeamt10
CLOSE DATA


PROCEDURE ImportAircraftTypes &&-----------------------------

_zTargetDir = [c:\SVM\data\]

_zSourceDir = [n:\foxpro2\gardner\]
CLOSE DATA ALL
SELE 0
ztf = _zTargetDir+"avairtyp"
USE (ztf) EXCL ALIAS ztarget
SELE ztarget
CLEAR TYPE
BROWSE
ZAP
BROW
SELE 0
zsf = _zSourceDir+"avairtyp"
*USE (zsf) ALIAS zsource
SELE ztarget
APPEND FROM (zsf)
*!*	  SCAN
*!*	    SELE ztarget
*!*	    LOCATE FOR ALLT(UPPE(ztarget.airmanufac)) = ALLT(UPPE(zsource.airmanufac))
*!*	    IF !FOUND()
*!*	      SELE zsource
*!*	      SCAT To zpedro MEMO
*!*	      SELE ztarget
*!*	      APPE BLANK
*!*	      GATH FROM zpedro MEMO
*!*	    ENDIF
*!*	    SELE zsource
*!*	  ENDSCAN
*SELE zsource
*USE
SELE ztarget

BROW

SELE 0  &&--- populate with unique makes from their own aircraft table
zsf = _zSourceDir+"avaircra"
USE (zsf) ALIAS zsource
SCAN
  SELE ztarget
  LOCATE FOR ALLT(UPPE(ztarget.airmanufac)) = ALLT(UPPE(zsource.amake))
  IF !FOUND()
    zmake=zsource.amake
    SELE ztarget
    APPE BLANK
    REPLA airmanufac WITH zmake
  ENDIF
  SELE zsource
ENDSCAN
SELE ztarget
BROW
*DELE ALL FOR EMPT(airmanufac)
USE IN zsource
USE IN ztarget
CLOSE DATA
ENDIF


*!*	CLOSE DATA ALL
*!*	SELE 0
*!*	USE avairtyp ALIAS ztarget
*!*	SELE 0
*!*	USE n:\foxpro2\gardner\avairtyp ALIAS zsource
*!*	SCAN
*!*	  SELE ztarget
*!*	  LOCATE FOR ALLT(UPPE(ztarget.airmanufac)) = ALLT(UPPE(zsource.airmanufac))
*!*	  IF !FOUND()
*!*	    SELE zsource
*!*	    SCAT To zpedro MEMO
*!*	    SELE ztarget
*!*	    APPE BLANK
*!*	    GATH FROM zpedro MEMO
*!*	  ENDIF
*!*	  SELE zsource
*!*	ENDSCAN
*!*	SELE zsource
*!*	USE

*!*	SELE 0
*!*	USE n:\foxpro2\gardner\avaircra ALIAS zsource
*!*	SCAN
*!*	  SELE ztarget
*!*	  LOCATE FOR ALLT(UPPE(ztarget.airmanufac)) = ALLT(UPPE(zsource.amake))
*!*	  IF !FOUND()
*!*	    zmake=zsource.amake
*!*	    SELE ztarget
*!*	    APPE BLANK
*!*	    REPLA airmanufac WITH zmake
*!*	  ENDIF
*!*	  SELE zsource
*!*	ENDSCAN
*!*	CLOSE DATA

PROCEDURE ImportEmployees &&-----------------------------
CLOSE DATA ALL
SELE 0
USE avempl_m
ZAP
APPEND FROM n:\foxpro2\gardner\avempl_m
SCAN
  zfn = ALLT(f_name)
  zln = ALLT(l_name)
  zmn = ALLT(m_name)
  zzn = zfn
  DO CASE
    CASE LEN(zmn)=1
      zzn = zzn + [ ]+zmn+[.]
    CASE LEN(zmn)>1
      zzn = zzn + [ ]+zmn
  ENDCASE
  zzn = zzn + IIF(!EMPT(zln),[ ]+zln,[])
  REPLACE avempl_m.fullname WITH zzn
ENDSCAN
CLOSE DATA

PROCEDURE ImportParts &&-------------------------------
CLOSE DATA ALL
SELE 0
USE serparts ORDER p_s_num
ZAP

SELE 0
USE avprthst ORDER part_num EXCL
ZAP
*APPE FROM n:\foxpro2\cti2\avpartss
APPE FROM n:\foxpro2\gardner\avprthst
INDEX ON UPPE(partnum+serial+cat) TAG temppsc

SELE 0
USE avpartss  ORDER appnum EXCL
ZAP
APPE FROM n:\foxpro2\gardner\avpartss
SELE avpartss
*// build serial list
*zNewCnt = 1
zSku = 1
SCAN
  REPL sku WITH zSku, pclass WITH [STANDARD]
  IF EMPT(punit)
    REPL punit WITH [EACH]
  ENDIF
  zSku = zSku + 1
  IF EMPT(avpartss.pserial)
    LOOP
  ENDIF
  zPSC = UPPE(avpartss.ppartnum+avpartss.pserial+avpartss.cat)
  IF !EMPT(pserial)
    SELE serparts
    APPEND BLANK
    *? STR(zNewCnt)
    *zNewCnt = zNewCnt + 1
    REPLACE pnum WITH avpartss.ppartnum, pserial WITH avpartss.pserial, pcat WITH avpartss.cat
    REPL origqoh WITH avpartss.pqoh
    SELE avprthst
    SEEK zPSC
    STORE 0 TO zzRef,zzInv_WO   &&// recno hits
    SCAN WHILE partnum+serial+cat = zPSC
      DO CASE
        CASE ([PO_]$avprthst.ref_misc.or.[ref_]$avprthst.ref_misc) .and. zzRef = 0
          zzRef=RECNO()
        CASE ([INV_]$avprthst.ref_misc .or. [WO_]$avprthst.ref_misc) .and. zzInv_WO = 0
          zzInv_WO=RECNO()
      ENDCASE
    ENDSCAN
    SELE avprthst
    IF zzRef > 0
      GOTO zzRef IN avprthst  && first ref_ record
      zref  = avprthst.ref_misc
      IF [line_]$zref
        zpo   = SUBS(zref,5,AT([line_],zref)-6)
      ELSE
        zpo   = ALLT(SUBS(zref,AT([PO_],zref)+3,10))
      ENDIF
      zvend = SUBS(zref,AT([vend_],zref)+5,6)
      SELE serparts
      REPLACE intostk WITH avprthst.date, poref WITH zpo, details WITH zpo+[ ]+zvend
      REPLACE cost WITH avprthst.cost, list WITH avprthst.list
    ELSE
      SELE avprthst
      SEEK zPSC  && find first otherwise
      SCAN WHILE partnum+serial+cat = zPSC
        IF qty > 0
          zref  = avprthst.ref_misc
          *zref  = STRTR(avprthst.ref_misc,[RECORD CREATED],[])
          SELE serparts
          REPLACE intostk WITH avprthst.date, details WITH zref
          REPLACE cost    WITH avprthst.cost, list WITH avprthst.list
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
    *// still positioned at same serpart
    *// looking for same PSC combo but where IT WAS used
    SELE avprthst
    IF zzInv_WO > 0
      GOTO zzInv_WO
      zref  = avprthst.ref_misc
      IF [WO_]$avprthst.ref_misc
        zWI = [WO-]+SUBS(zref,4,AT([ ],zref)-1)
      ELSE
        zWI = [INV-]+SUBS(zref,5,AT([ ],zref)-1)
      ENDIF
      SELE serparts
      IF origqoh<=0
        REPLACE outofstk WITH avprthst.date, wo_in_ref WITH zWI
      ELSE
        REPLACE wo_in_ref WITH zWI
      ENDIF
      IF cost = 0
        REPLACE cost WITH avprthst.cost
      ENDIF
      IF list = 0
        REPLACE list WITH avprthst.list
      ENDIF
    ELSE
      SEEK zPSC
      SCAN WHILE partnum+serial+cat = zPSC
        IF qty < 0
          zref  = avprthst.ref_misc
          SELE serparts
          IF origqoh<=0
            REPLACE outofstk WITH avprthst.date, wo_in_ref WITH zref
          ELSE
            REPLACE wo_in_ref WITH zref
          ENDIF
          IF cost = 0
            REPLACE cost WITH avprthst.cost
          ENDIF
          IF list = 0
            REPLACE list WITH avprthst.list
          ENDIF
          EXIT
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
  SELE avpartss
ENDSCAN &&// finished building serial table

SELE 0
USE EXCL avpartss AGAIN ALIAS Parts2
SELE Parts2
INDEX ON UPPE(ppartnum+cat) TAG TEMP1
* scan serialized inventory
* determine total pqoh for same partnumber / cat
* find most recent priced item
* use the most recent item as the final target record

SELE avpartss  && get info for final single record
SCAN
  IF EMPT(pserial)  && only serialized items please
    LOOP
  ENDIF
  zPC  = UPPE(avpartss.ppartnum+avpartss.cat)
  zQty = 0
  zLP  = {}
  zRec = 0  &&  temp1  file
  zAlr = .F.
  SELE Parts2
  SEEK zPC
  SCAN WHILE Parts2.ppartnum+cat = zPC
    IF Parts2.plastpric # {} .and. Parts2.plastpric > zLP
      zLP  = Parts2.plastpric
      zRec = RECNO()
    ENDIF
    zQty  = zQty + IIF(Parts2.pqoh>0,Parts2.pqoh,0)
    zAlr = IIF([New Import Serial]$pserial,.T.,zAlr)
  ENDSCAN
  SELE Parts2
  IF zRec > 0 && found the most recent
    GOTO zRec
  ELSE        && no dates found using first record
    IF SEEK(zPC)
      *// may want to scan here to find "Best" record ?
      zRec = RECNO()  &&// keep this record
    ENDIF
  ENDIF
  IF zRec > 0 .and. !zAlr
    SCAT TO zpedro MEMO
    SELE avpartss
    zOldRec = RECNO()
    APPEN BLANK
    GATH FROM zpedro MEMO
    REPLACE pqoh WITH zQty
    REPLACE pserial WITH [New Import Serial]
    GOTO zOldRec
  ENDIF
  SELE avpartss
ENDSCAN

*// purge serialized records now that aren't the new ones
SET DELE ON
SELE avpartss
DELE FOR !EMPT(pserial) .and. ![New Import Serial]$pserial
*// delete duplicate part# records that have no serial#
SCAN FOR [New Import Serial] $ pserial
  SELE Parts2
  SET ORDER TO
  LOCATE FOR EMPT(Parts2.pserial) .and. Parts2.ppartnum = avpartss.ppartnum
  IF FOUND()
    DELE
  ENDIF
  SELE avpartss
ENDSCAN
SELE Parts2
DELE TAG TEMP1
USE
SELE avpartss
REPLACE ALL q_serial WITH [Y] FOR [New Import Serial] $ pserial
REPLACE ALL pserial WITH SPAC(0)
PACK
SELE avprthst
DELE TAG temppsc
CLOSE DATA ALL

PROCEDURE Import8130 &&---------------------------------
  SELE 0
  ztf = "c:\SVM\data\avwo_813"
  USE (ztf) EXCL
  GO BOTT
  BROW
  zMax = 0
  CALCULATE MAX(num_8130) TO zMax
  zsf = "c:\actools\data\avwo_813"
  APPEND FROM &zsf
  BROW
  SELE avwo_813
  LOCATE FOR num_8130 = zMax
  IF FOUND()
    SKIP 1  &&should be on first one from actools
    zid = zMax + 1
    DO WHILE !EOF()
      REPLACE num_8130 WITH zid, wo_num WITH ALLT(wo_num), block21 WITH ALLT(block21)
      zid = zid + 1
      IF EMPT(block22).and.!EMPT(block17)
        REPLACE block14 WITH 1
      ELSE
        REPLACE block14 WITH 3
      ENDIF
      SKIP
    ENDDO
  ENDIF
  USE IN avwo_813

PROCEDURE OldImport8130 &&---------------------------------
CLOSE DATA ALL
SELE 0
USE avwo_813
ZAP
APPEND FROM n:\foxpro2\gardner\avwo_813
zid = 1
SCAN
  REPLACE num_8130 WITH zid, wo_num WITH ALLT(wo_num), block21 WITH ALLT(block21)
  zid = zid + 1
  IF EMPT(block22).and.!EMPT(block17)
    REPLACE block14 WITH 1
  ELSE
    REPLACE block14 WITH 3
  ENDIF
ENDSCAN
CLOSE DATA

PROCEDURE ImportTestEquip &&----------------------------
CLOSE DATA ALL
SELE 0
USE tequip_m
ZAP
SELE 0
USE tequip_c
ZAP
SELE 0
USE n:\foxpro2\gardner\avtequip
STORE 1 TO zcnt
SCAN
  SELE tequip_m
  APPE BLANK
  REPLA autowarn WITH IIF(avtequip.earlywarn=[Y],.T.,.F.)
  REPLA comments WITH avtequip.comments, cost      WITH avtequip.cost
  REPLA date_due WITH avtequip.date_due, date_serv WITH avtequip.date_purc
  REPLA interval WITH avtequip.interval, model     WITH avtequip.model
  REPLA serial   WITH avtequip.serial,   status    WITH avtequip.status
  REPLA key_m    WITH STR(zcnt,6)
  zcnt = zcnt + 1
  FOR jjj = 0 TO 9
    zcd = [avtequip.cdate]+ALLT(STR(jjj))
    zdd = [avtequip.ddate]+ALLT(STR(jjj))
    zfc = [avtequip.facility]+ALLT(STR(jjj))
    znt = [avtequip.note]+ALLT(STR(jjj))
    IF &zcd = {}
      LOOP
    ENDIF
    SELE tequip_c
    APPEN BLANK
    REPLA key_c WITH tequip_m.key_m
    REPLACE date WITH &zcd,duedate WITH &zdd,facility WITH &zfc,note WITH &znt
  ENDFOR
ENDSCAN
CLOSE DATA ALL

PROCEDURE ImportPhrases ----------------------------
SELE 0
USE avphrase
ZAP
APPEND FROM n:\foxpro2\gardner\avphrase
SCAN
  REPLACE category WITH [ALL]
  IF !EMPT(memphrase)
    REPLACE type WITH [BLOCK]
  ELSE
    REPLACE type WITH []
  ENDIF
ENDSCAN
SELE 0
USE avbck337
SCAN
  IF EMPT(name).or.EMPT(back)
    LOOP
  ENDIF
  z1 = name
  z2 = back
  SELE avphrase
  APPEND BLANK
  REPLACE phrase WITH z1, memphrase WITH z2;
    ,category WITH [337 BACK TEXT], type WITH [BLOCK]
  SELE avbck337
ENDSCAN
CLOSE DATA ALL

PROCEDURE ImportVendors  &&-----------------------------
CLOSE DATA ALL
SELE 0
USE configav
SELE 0
USE avvendor
ZAP
APPEND FROM n:\foxpro2\gardner\avvendor
REPLA ALL approved WITH .T.
USE
SELE 0
USE avvahist
ZAP
APPE FROM n:\foxpro2\gardner\avvahist
USE
SELE 0
USE avpayble
ZAP
APPE FROM n:\foxpro2\gardner\avpayble
SELE 0
USE avpoacct
ZAP
USE avchartp IN 0
SELE avpayble
SCAN
  IF avpayble.yacctcode > 0
    SELE avchartp
    LOCATE FOR avchartp.chnum = avpayble.yacctcode
    IF FOUND()
      zdescrip   = avchartp.chdesc
      zChitemnum = avchartp.chitemnum
    ELSE
      zdescrip   = []
      zChitemnum = 0
    ENDIF
    SELE avpoacct
    APPE BLANK
    REPLACE po_link    WITH avpayble.ypaynum,   accntnum   WITH avpayble.yacctcode
    REPLACE amt        WITH avpayble.ygrossamt, descrip    WITH zdescrip
    REPLACE chitemnum  WITH zChitemnum
  ENDIF
  SELE avpayble
  REPLA dposted    WITH IIF(yposted=[Y],yitemdate,{})
  REPLA mhy_m      WITH IIF(dposted#{},LEFT(DTOS(dposted),6),[])
  REPLA dcleared   WITH IIF(ycleared=[Y],yitemdate,{})
ENDSCAN
USE IN avpayble
USE IN avchartp
USE IN avpoacct

SELE 0
USE avpoline
ZAP
APPE FROM n:\foxpro2\gardner\avpoline
USE
USE avvendor AGAIN IN 0 ALIAS zvends
USE vendcont IN 0
SELE vendcont
ZAP
SELE zvends
SCAN
  zcnt= 1
  IF !EMPT(vphone)
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH zvends.vmnem, descrip WITH MakeDialLbl(zvends.p1_label,1), num WITH MakeDialNum(zvends.vphone)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(vphone2)
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH zvends.vmnem, descrip WITH MakeDialLbl(zvends.p2_label,2), num WITH MakeDialNum(zvends.vphone2)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(vphone3)
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH zvends.vmnem, descrip WITH MakeDialLbl(zvends.p3_label,3), num WITH MakeDialNum(zvends.vphone3)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(vphone4)
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH zvends.vmnem, descrip WITH MakeDialLbl(zvends.p4_label,4), num WITH MakeDialNum(zvends.vphone4)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(vphone5)
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH zvends.vmnem, descrip WITH MakeDialLbl(zvends.p5_label,5), num WITH MakeDialNum(zvends.vphone5)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(vfaxnum)
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH zvends.vmnem, descrip WITH [FAX #], num WITH MakeDialNum(zvends.vfaxnum)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
  REPLACE vbalance WITH IIF(vbalance#0,vbalance,0)
ENDSCAN
USE IN zvends
USE IN vendcont

PROCEDURE ImportInvoices &&-----------------------------
CLOSE DATA ALL
SELE 0
USE avinacct
ZAP

SELE 0
USE avchartp
ZAP
APPE FROM n:\foxpro2\gardner\avchartp

SELE 0
USE avipused
ZAP
*APPE FROM n:\foxpro2\cti2\avipused
APPE FROM n:\foxpro2\gardner\avipused
REPLA ALL puselleach WITH pucosteach , lcharge with .F.
USE

SELE 0
USE avinvoic
ZAP
*APPE FROM n:\foxpro2\cti2\avinvoic
APPE FROM n:\foxpro2\gardner\avinvoic
SCAN
  IF avinvoic.iacctcode>0
    SELE avchartp
    LOCATE FOR chnum = avinvoic.iacctcode
    IF FOUND()
      zdescrip   = avchartp.chdesc
      zChitemnum = chitemnum
    ELSE
      zdescrip   = []
      zChitemnum = 0
    ENDIF
    SELE avinacct
    APPE BLANK
    REPLACE invnum WITH avinvoic.iinvnum, accntnum WITH avinvoic.iacctcode
    REPLACE amt    WITH avinvoic.itotal,   descrip WITH zdescrip
    REPLACE chitemnum  WITH zChitemnum
  ENDIF
  SELE avinvoic
  REPLA dposted    WITH idate, mhy_m WITH LEFT(DTOS(avinvoic.idate),6) && simply mark all as posted
  REPLA dcleared   WITH IIF(LEFT(iequip,7)=[CLEARED],idate,{})
  IF idiscount > 0
    zdiv = (100-idiscount) * .01
    zlbr = itlabor / zdiv
    zprt = itparts / zdiv
    REPLACE lbrdiscnt WITH zlbr - itlabor
    REPLACE prtsdiscnt WITH zprt - itparts
    REPLACE itlabor WITH zlbr
    REPLACE itparts WITH zprt
  ENDIF
  REPLA idisc_lbr  WITH idiscount
  REPLA idisc_prts WITH idiscount
  REPLA dprinted WITH IIF(iposted,idate,{})
  REPLA wonum WITH IIF( SUBS(iequip,11,2)=[WO] , VAL( SUBS(iequip,13,8) ),0)
ENDSCAN

CLOSE DATA ALL

PROCEDURE ImportAircraft &&-----------------------------
CLOSE DATA ALL
SELE 0
USE acinspec
ZAP
SELE 0
USE avaircra
ZAP
APPE FROM n:\foxpro2\gardner\avaircra
SCAN
  IF afardue # {}
    SELE acinspec
    APPE BLAN
    REPL tail_num WITH avaircra.annum, item WITH [FARS DUE],;
      lastdate WITH IIF(avaircra.last_fars={},GOMO(avaircra.afardue,-24),avaircra.last_fars), ;
      duedate  WITH avaircra.afardue, months_int WITH 24, lmonth_day WITH 1,;
      dayofmonth WITH IIF(avaircra.afardue=LastDOM(avaircra.afardue),[LAST],[SAME]),;
      remdays  WITH IIF(avaircra.afardue#{},avaircra.afardue-DATE(),remdays)
  ENDIF
  IF avaircra.next_annu # {}
    SELE acinspec
    APPE BLAN
    REPL tail_num WITH avaircra.annum, item WITH [ANNUAL INSPECTION],;
      lastdate WITH IIF(avaircra.last_annu={},GOMO(avaircra.next_annu,-12),avaircra.last_annu),;
      duedate  WITH avaircra.next_annu, months_int WITH 12,lmonth_day WITH 1,;
      dayofmonth WITH IIF(avaircra.next_annu =LastDOM(avaircra.next_annu ),[LAST],[SAME]),;
      remdays  WITH IIF(avaircra.next_annu#{},avaircra.next_annu-DATE(),remdays)
  ENDIF
  IF avaircra.next_100h # 0
    SELE acinspec
    APPE BLAN
    REPL tail_num WITH avaircra.annum, item WITH [100 HOUR INSPECTION],;
      lasthours WITH avaircra.last_100h, duehours  WITH avaircra.next_100h,;
      time_int WITH 100
  ENDIF
  IF avaircra.next_oil # 0
    SELE acinspec
    APPE BLAN
    REPL tail_num WITH avaircra.annum, item WITH [OIL CHANGE],;
      lasthours WITH avaircra.last_oil, duehours  WITH avaircra.next_oil,;
      time_int WITH 100
  ENDIF
  *!*	    TAIL_NUM,ITEM,INSTDATE,LASTDATE,MONTHS_INT,DUEDATE,;
  *!*	LASTHOURS,INSTTIME,TIME_INT,DUEHOURS,REMDAYS,REMHOURS,;
  *!*	L_SYS,L_REPORT,NOTES,LMONTH_DAY,PNUM,MODEL,;
  *!*	SERIAL,TRACKBY,

ENDSCAN
USE IN avaircra
CLOSE DATA ALL

PROCEDURE oldImportAircraftTypes &&-----------------------------
CLOSE DATA ALL
SELE 0
USE avairtyp ALIAS ztarget
SELE 0
USE n:\foxpro2\gardner\avairtyp ALIAS zsource
SCAN
  SELE ztarget
  LOCATE FOR ALLT(UPPE(ztarget.airmanufac)) = ALLT(UPPE(zsource.airmanufac))
  IF !FOUND()
    SELE zsource
    SCAT To zpedro MEMO
    SELE ztarget
    APPE BLANK
    GATH FROM zpedro MEMO
  ENDIF
  SELE zsource
ENDSCAN
SELE zsource
USE

*!*	SELE 0
*!*	USE n:\foxpro2\gardner\avaircra ALIAS zsource
*!*	SCAN
*!*	  SELE ztarget
*!*	  LOCATE FOR ALLT(UPPE(ztarget.airmanufac)) = ALLT(UPPE(zsource.amake))
*!*	  IF !FOUND()
*!*	    SELE zsource
*!*	    zmake=zsource.amake
*!*	    SELE ztarget
*!*	    APPE BLANK
*!*	    REPLA airmanufac WITH zmake
*!*	  ENDIF
*!*	  SELE zsource
*!*	ENDSCAN
*!*	CLOSE DATA

PROCEDURE ImportWorkOrders &&-----------------------------
CLOSE DATA ALL
SELE 0
USE avwo_tsk
ZAP
APPE FROM n:\foxpro2\gardner\avwo_tsk
SCAN
  REPLA wokey WITH STR(wo_num,8)+STR(task_num,3),corr_act WITH ALLT(discr_act),discr_act WITH SPAC(0);
    ,job_type WITH [STANDARD],status WITH IIF(EMPT(fini_date),[IN PROCESS],[FINISHED]), l_rts WITH .T.;
    ,priority WITH [NORMAL]
ENDSCAN

SELE 0
USE avwo_lbr
ZAP
APPE FROM n:\foxpro2\gardner\avwo_lbr
REPLA ALL wokey WITH STR(wo_num,8)+STR(task_num,3), l_ot WITH .F.,ot_multi WITH 1.5

SELE 0
USE avwo_eqp
ZAP
SELE 0
USE avwo_pu
ZAP
APPE FROM n:\foxpro2\gardner\avwo_pu FOR EMPT(serial)
REPLA ALL wokey WITH STR(wo_num,8)+STR(task_num,3), lcharge WITH .F.
SELE avwo_eqp
APPE FROM n:\foxpro2\gardner\avwo_pu FOR !EMPT(serial)
REPLA ALL wokey WITH STR(wo_num,8)+STR(task_num,3), lcharge WITH .F., model WITH pnum
REPLA ALL pnum WITH SPAC(0), markup WITH IIF(sell>0.and.cost>0,MIN(999.9,ROUN(((avwo_eqp.sell/avwo_eqp.cost)*100)-100,2)),0)

SELE 0
USE avwo_oth
ZAP
APPE FROM n:\foxpro2\gardner\avwo_oth
REPLA ALL wokey WITH STR(wo_num,8)+STR(task_num,3)

SELE 0
USE avwo_813
ZAP
APPE FROM n:\foxpro2\gardner\avwo_813
USE IN avwo_813

SELE 0 &&------------
USE avwo_po
ZAP
APPE FROM n:\foxpro2\gardner\avwo_po
REPLA ALL wokey WITH STR(wo_num,8)+STR(task_num,3)
USE IN avwo_po

SET ORDER TO wokey  IN avwo_lbr
SET ORDER TO wokey  IN avwo_pu
SET ORDER TO wokey  IN avwo_oth
SET ORDER TO wokey  IN avwo_eqp
SET ORDER TO wo_num IN avwo_tsk

SELE 0
USE avaircra
SELE 0
USE avcustom
SELE 0
USE avwo_mtr
ZAP
APPE FROM n:\foxpro2\gardner\avwo_mtr
SCAN
  IF SEEK(PADR(ALLT(UPPE(avwo_mtr.tail_num)),8),[avaircra],[aacnnum]).and.!EMPT(avaircra.annum)
    REPLA avwo_mtr.make   WITH avaircra.amake
    REPLA avwo_mtr.model  WITH avaircra.amodel
    REPLA avwo_mtr.serial WITH avaircra.aserial
  ENDIF
  IF SEEK(PADR(ALLT(UPPE(avwo_mtr.cmnem)),6),[avcustom],[accmnem]).and.!EMPT(avcustom.cmnem)
    REPLA avwo_mtr.cmnem    WITH avcustom.cmnem
    REPLA avwo_mtr.customer WITH IIF(avcustom.cperbus = 'B',avcustom.cname,avcustom.cperson)
    REPLA avwo_mtr.person   WITH avcustom.cperson
    REPLA avwo_mtr.addr     WITH avcustom.caddress
    REPLA avwo_mtr.addr2    WITH avcustom.caddress2
    REPLA avwo_mtr.city     WITH avcustom.ccity
    REPLA avwo_mtr.state    WITH avcustom.cstate
    REPLA avwo_mtr.country  WITH avcustom.ccountry
    REPLA avwo_mtr.phone    WITH avcustom.cphone
    REPLA avwo_mtr.FAX      WITH avcustom.cfax
    REPLA avwo_mtr.zip      WITH avcustom.czip
  ENDIF

  STORE 0 TO zzWOttlS
  SELE avwo_tsk
  SEEK avwo_mtr.wo_num
  SCAN WHILE avwo_tsk.wo_num = avwo_mtr.wo_num

    STORE 0 TO XXX,yyy,zzz
    SELE avwo_lbr
    SEEK avwo_tsk.wokey
    SCAN WHILE avwo_lbr.wokey = avwo_tsk.wokey
      yyy = yyy +  (hrs_billd * rate)  &&sell
    ENDSCAN
    SELE avwo_tsk
    yyy = IIF(avwo_tsk.isflatlbr,avwo_tsk.flat_lbr,yyy)
    REPLACE avwo_tsk.labor_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy,zzz  && include because import may put some items into equipment
    SELE avwo_eqp
    SEEK avwo_tsk.wokey
    SCAN WHILE avwo_eqp.wokey = avwo_tsk.wokey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avwo_tsk
    yyy = IIF(avwo_tsk.isflateqp,avwo_tsk.flat_eqp,yyy)
    REPLACE avwo_tsk.equip_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy
    SELE avwo_pu
    SEEK avwo_tsk.wokey
    SCAN WHILE avwo_pu.wokey = avwo_tsk.wokey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avwo_tsk
    yyy = IIF(avwo_tsk.isflatpts,avwo_tsk.flat_pts,yyy)
    REPLACE avwo_tsk.parts_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy,zzz
    SELE avwo_oth
    SEEK avwo_tsk.wokey
    SCAN WHILE avwo_oth.wokey = avwo_tsk.wokey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avwo_tsk
    yyy = IIF(avwo_tsk.isflatoth,avwo_tsk.flat_oth,yyy)
    REPLACE avwo_tsk.other_ttl WITH MIN(99999999.99,yyy)

    SELE avwo_tsk
    zGrandTotal = avwo_tsk.labor_ttl+avwo_tsk.equip_ttl+avwo_tsk.parts_ttl+avwo_tsk.other_ttl
    REPLACE total_ttl WITH zGrandTotal
    zzWOttlS=zzWOttlS + avwo_tsk.total_ttl &&(avwo_tsk.labor+avwo_tsk.equip+avwo_tsk.parts - avwo_tsk.trade)
  ENDSCAN
  SELE avwo_mtr
  REPLACE avwo_mtr.sell_ttl WITH zzWOttlS
ENDSCAN
USE IN avwo_mtr
USE IN avwo_tsk
USE IN avwo_lbr
USE IN avwo_eqp
USE IN avwo_pu
USE IN avwo_oth
USE IN avaircra
USE IN avcustom

CLOSE DATA ALL

PROCEDURE ImportCustomers &&-----------------------------
CLOSE DATA ALL
SELE 0
USE configav
SELE 0
USE avmonthl
ZAP
APPE FROM n:\foxpro2\gardner\avmonthl
USE IN avmonthl
USE avcustom
ZAP
APPE FROM n:\foxpro2\gardner\avcustom
SELE 0
USE avcahist
ZAP
APPE FROM n:\foxpro2\gardner\avcahist
USE avcustom AGAIN IN 0 ALIAS zcusts
SELE 0
USE custcont
ZAP
SELE zcusts
SCAN
  zcnt= 1
  IF !EMPT(cphone)
    SELECT custcont
    APPE BLANK
    REPLA cmnem WITH zcusts.cmnem, descrip WITH MakeDialLbl(zcusts.p1_label,1), num WITH MakeDialNum(zcusts.cphone)
    SELE zcusts
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(cphone2)
    SELECT custcont
    APPE BLANK
    REPLA cmnem WITH zcusts.cmnem, descrip WITH MakeDialLbl(zcusts.p2_label,2), num WITH MakeDialNum(zcusts.cphone2)
    SELE zcusts
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(cphone3)
    SELECT custcont
    APPE BLANK
    REPLA cmnem WITH zcusts.cmnem, descrip WITH MakeDialLbl(zcusts.p3_label,3), num WITH MakeDialNum(zcusts.cphone3)
    SELE zcusts
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(cphone4)
    SELECT custcont
    APPE BLANK
    REPLA cmnem WITH zcusts.cmnem, descrip WITH MakeDialLbl(zcusts.p4_label,4), num WITH MakeDialNum(zcusts.cphone4)
    SELE zcusts
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(cphone5)
    SELECT custcont
    APPE BLANK
    REPLA cmnem WITH zcusts.cmnem, descrip WITH MakeDialLbl(zcusts.p5_label,5), num WITH MakeDialNum(zcusts.cphone5)
    SELE zcusts
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(cfax)
    SELECT custcont
    APPE BLANK
    REPLA cmnem WITH zcusts.cmnem, descrip WITH [FAX #], num WITH MakeDialNum(zcusts.cfax)
    SELE zcusts
    zcnt = zcnt + 1
  ENDIF
  REPLACE taxexempt1 WITH IIF(!EMPT(ctexnum1),.T.,.F.) , taxexempt2 WITH IIF(!EMPT(ctexnum2),.T.,.F.)
  REPLACE taxexempt3 WITH IIF(!EMPT(ctexnum3),.T.,.F.) , taxexempt4 WITH IIF(!EMPT(ctexnum4),.T.,.F.)
  REPLACE taxexemptO WITH .F.
  REPLACE cperbus    WITH IIF(LEFT(ALLT(cperbus),1)=[P],[Person],[Business])
  REPLACE cbalance   WITH IIF(cbalance#0,cbalance,0)
  REPLACE ccredlim   WITH IIF(ccredlim #0,ccredlim ,0)
  REPLACE tax_prcnt1 WITH 0,tax_prcnt2 WITH 0,tax_prcnt3 WITH 0,tax_prcnt4 WITH 0,tax_prcntO WITH 0
  REPLACE disc_lbr   WITH 0,disc_prts WITH 0,disc_ow WITH 0
  IF BETWEEN(VAL(cserial),2000,2999)
    REPLACE actserial WITH cserial, actbought WITH cbought
    REPLACE cserial   WITH SPAC(0), cbought WITH {}
  ENDIF
ENDSCAN
USE IN zcusts
USE IN custcont
CLOSE DATA ALL

FUNCTION MakeDialNum &&-----------------------------------
PARA Num_In
PRIV Num_In,num_out,cus_ac,our_ac,DigNum,NortAmer
IF EMPTY(Num_In) .or. !ISDIG(Num_In)
  RETURN(SPACE(25))
ENDIF
NorthAmer= IIF(OCCUR([-],Num_In)=2 .and. LEN(ALLT(Num_In))=12,.t.,.f.)
DigNum7 = SUBS(Num_In,5,8)
cus_c   = SUBS(Num_In,1,3)
our_c   = LEFT(ALLTR(configav.fphone),3)
IF cus_c == our_c  &&& in this A/C
  IF SUBS(Num_In,5,3) $ configav.flocalexch  &&& not toll call
    num_out = PADR(DigNum7,25)
  ELSE
    DO CASE
      CASE LEN(ALLT(Num_In))<7
        num_out = SPACE(25)
      CASE 'Dial  1 + 7digit' $ configav.fmisc003
        num_out = PADR('1-'+DigNum7,25)
      CASE 'Dial  7digits only' $ configav.fmisc003
        num_out = PADR(DigNum7,25)
      CASE 'Dial  1 + A/C + 7digits' $ configav.fmisc003
        num_out = PADR('1-'+cus_c+[-]+DigNum7,25)
      OTHERWISE
        num_out = PADR('1-'+DigNum7,25)
    ENDCASE
  ENDIF
ELSE  &&& outside A/C
  IF !NorthAmer
    num_out = Num_In
  ELSE
    num_out = IIF(LEN(ALLT(Num_In))<5,SPACE(25),PADR('1-'+cus_c+[-]+DigNum7,25))
  ENDIF
ENDIF
RETURN(num_out)

FUNCTION MakeDialLbl &&---------------------------
PARA zlbl,znum
PRIV zlbl,znum,lblout
lblout=[]
DO CASE
  CASE EMPT(zlbl).and.znum=1
    lblout = [1st Phone]
  CASE EMPT(zlbl).and.znum=2
    lblout = [2nd Phone]
  CASE EMPT(zlbl).and.znum=3
    lblout = [3rd Phone]
  CASE EMPT(zlbl).and.znum=4
    lblout = [4th Phone]
  CASE EMPT(zlbl).and.znum=5
    lblout = [5th Phone]
  OTHERWISE
    lblout = zlbl
ENDCASE
RETURN(lblout)


PROCEDURE MakeBigFpt &&----------------------------------
RETURN
CLOSE DATA
CREATE TABLE _dumbmem (fdumbmemo M(4))      &&CREATE TABLE friends (FirstName C(20)
SELE _dumbmem
APPEND BLANK
REPLACE _dumbmem.fdumbmemo WITH []
zStr=[]
jjj=[ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890]
FOR ttt = 1 TO 10000
  zStr = zStr + jjj+[ * ]+jjj+[ * ]+jjj+CHR(13)+CHR(10)
ENDFOR
REPLACE _dumbmem.fdumbmemo WITH zStr
FLUSH
CHAN
WAIT STR(LEN(zStr)) WIND
CLOSE DATA

PROCEDURE ImportQuotess &&-----------------------------
CLOSE DATA ALL
SELE 0
USE avqu_itm
ZAP
APPE FROM n:\foxpro2\gardner\avqu_itm
SCAN
  REPLA qkey WITH STR(wo_num,8)+STR(task_num,3),corr_act WITH ALLT(discr_act),discr_act WITH SPAC(0);
    ,job_type WITH [STANDARD],status WITH IIF(EMPT(fini_date),[IN PROCESS],[FINISHED]), l_rts WITH .T.;
    ,priority WITH [NORMAL], lfollowup WITH .T.
ENDSCAN

SELE 0
USE avqu_lbr
ZAP
APPE FROM n:\foxpro2\gardner\avqu_lbr
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3), l_ot WITH .F.,ot_multi WITH 1.5

SELE 0
USE avqu_eqp
ZAP
SELE 0
USE avqu_pu
ZAP
APPE FROM n:\foxpro2\gardner\avqu_pu FOR EMPT(serial)
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3), lcharge WITH .F.
SELE avqu_eqp
APPE FROM n:\foxpro2\gardner\avqu_pu FOR !EMPT(serial)
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3), lcharge WITH .F., model WITH pnum
REPLA ALL pnum WITH SPAC(0), markup WITH IIF(sell>0.and.cost>0,MIN(999.9,ROUN(((avqu_eqp.sell/avqu_eqp.cost)*100)-100,2)),0)

SELE 0
USE avqu_oth
ZAP
APPE FROM n:\foxpro2\gardner\avqu_oth
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3)

SELE 0
USE avqu_813
ZAP
APPE FROM n:\foxpro2\gardner\avqu_813
USE IN avqu_813

SELE 0 &&------------
USE avqu_po
ZAP
APPE FROM n:\foxpro2\gardner\avqu_po
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3)
USE IN avqu_po

SET ORDER TO qkey  IN avqu_lbr
SET ORDER TO qkey  IN avqu_pu
SET ORDER TO qkey  IN avqu_oth
SET ORDER TO qkey  IN avqu_eqp
SET ORDER TO qnum IN avqu_itm

SELE 0
USE avcustom
SELE 0
USE avqu_mtr
ZAP
APPE FROM n:\foxpro2\gardner\avqu_mtr
SCAN
  IF SEEK(PADR(ALLT(UPPE(avqu_mtr.cmnem)),6),[avcustom],[accmnem]).and.!EMPT(avcustom.cmnem)
    REPLA avqu_mtr.cmnem    WITH avcustom.cmnem
    REPLA avqu_mtr.customer WITH IIF(avcustom.cperbus = 'B',avcustom.cname,avcustom.cperson)
    REPLA avqu_mtr.person   WITH avcustom.cperson
    REPLA avqu_mtr.addr     WITH avcustom.caddress
    REPLA avqu_mtr.addr2    WITH avcustom.caddress2
    REPLA avqu_mtr.city     WITH avcustom.ccity
    REPLA avqu_mtr.state    WITH avcustom.cstate
    REPLA avqu_mtr.country  WITH avcustom.ccountry
    REPLA avqu_mtr.phone    WITH avcustom.cphone
    REPLA avqu_mtr.FAX      WITH avcustom.cfax
    REPLA avqu_mtr.zip      WITH avcustom.czip
  ENDIF

  STORE 0 TO zzWOttlS
  SELE avqu_itm
  SEEK avqu_mtr.wo_num
  SCAN WHILE avqu_itm.wo_num = avqu_mtr.wo_num

    STORE 0 TO XXX,yyy,zzz
    SELE avqu_lbr
    SEEK avqu_itm.qkey
    SCAN WHILE avqu_lbr.qkey = avqu_itm.qkey
      yyy = yyy +  (hrs_billd * rate)  &&sell
    ENDSCAN
    SELE avqu_itm
    yyy = IIF(avqu_itm.isflatlbr,avqu_itm.flat_lbr,yyy)
    REPLACE avqu_itm.labor_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy,zzz  && include because import may put some items into equipment
    SELE avqu_eqp
    SEEK avqu_itm.qkey
    SCAN WHILE avqu_eqp.qkey = avqu_itm.qkey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avqu_itm
    yyy = IIF(avqu_itm.isflateqp,avqu_itm.flat_eqp,yyy)
    REPLACE avqu_itm.equip_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy
    SELE avqu_pu
    SEEK avqu_itm.qkey
    SCAN WHILE avqu_pu.qkey = avqu_itm.qkey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avqu_itm
    yyy = IIF(avqu_itm.isflatpts,avqu_itm.flat_pts,yyy)
    REPLACE avqu_itm.parts_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy,zzz
    SELE avqu_oth
    SEEK avqu_itm.qkey
    SCAN WHILE avqu_oth.qkey = avqu_itm.qkey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avqu_itm
    yyy = IIF(avqu_itm.isflatoth,avqu_itm.flat_oth,yyy)
    REPLACE avqu_itm.other_ttl WITH MIN(99999999.99,yyy)

    SELE avqu_itm
    zGrandTotal = avqu_itm.labor_ttl+avqu_itm.equip_ttl+avqu_itm.parts_ttl+avqu_itm.other_ttl
    REPLACE total_ttl WITH zGrandTotal
    zzWOttlS=zzWOttlS + avqu_itm.total_ttl &&(avqu_itm.labor+avqu_itm.equip+avqu_itm.parts - avqu_itm.trade)
  ENDSCAN
  SELE avqu_mtr
  REPLACE avqu_mtr.sell_ttl WITH zzWOttlS
ENDSCAN
USE IN avqu_mtr
USE IN avqu_itm
USE IN avqu_lbr
USE IN avqu_eqp
USE IN avqu_pu
USE IN avqu_oth
USE IN avaircra
USE IN avcustom
CLOSE DATA ALL

PROCEDURE ImportQuotes &&-----------------------------
CLOSE DATA ALL
SELE 0
USE n:\foxpro2\gardner\avquotes
SET ORDER TO
SELE 0
USE avqu_prt
ZAP
SELE 0
USE avqu_trd
ZAP
SELE 0
USE avqu_mtr
ZAP
SELE 0
USE avqu_itm
ZAP
SELE 0
USE avqu_lbr
ZAP
SELE 0
USE avqu_eqp
ZAP
SELE 0
USE n:\foxpro2\gardner\avqudata
SET ORDER TO
SELE avquotes
qcnt = 1
SCAN
  SELE avqu_mtr
  APPEND BLANK
  REPLA qnum       WITH qcnt, qdate WITH avquotes.qdate, qmnem WITH avquotes.qmnem
  REPLA qstarttext WITH ALLT(avquotes.qtext), qcity WITH avquotes.qcity, qstate WITH avquotes.qstate
  REPLA qzip       WITH avquotes.qzip, qcust WITH avquotes.qcust, qaddr WITH avquotes.qaddr
  REPLA labor_ttl  WITH avquotes.labor_ttl, qmake WITH avquotes.qmake, qmodel WITH avquotes.qmodel
  REPLA qnnum      WITH avquotes.qnnum, qperson WITH avquotes.qperson, total_ttl  WITH avquotes.total_ttl
  qcnt = qcnt + 1
  SELE avqudata
  LOCATE FOR quodnum = avquotes.quotenum
  qIcnt = 0
  IF FOUND()
    SCAN FOR quodnum = avquotes.quotenum  && not while because un ordered
      qIcnt = qIcnt + 1
      SELE avqu_itm
      APPE BLANK
      REPL qnum WITH avqu_mtr.qnum ,qitem WITH qIcnt, qkey WITH STR(qnum,8)+STR(qitem,3)
      REPL labor WITH avqudata.labor, equip WITH avqudata.sell, descrip WITH avqudata.descrip
      IF avqudata.labor>0
        SELE avqu_lbr
        APPEND BLANK
        REPLA qnum  WITH avqu_itm.qnum, qitem WITH avqu_itm.qitem, qkey WITH avqu_itm.qkey
        REPLA hours WITH avqudata.hours, bill_hr WITH avqudata.rate, bill_ext WITH avqudata.hours * avqudata.rate
        REPLA cost_hr WITH avqudata.rate, cost_ext WITH avqudata.hours * avqudata.rate
      ENDIF
      IF !EMPT(avqudata.model).and. avqudata.sell > 0
        SELE avqu_eqp
        APPEND BLANK
        REPLA qnum  WITH avqu_itm.qnum, qitem WITH avqu_itm.qitem, qkey WITH avqu_itm.qkey
        REPLA model WITH avqudata.model, partnum WITH avqudata.partnum, descrip WITH avqudata.descrip
        REPLA list WITH avqudata.list, cost WITH avqudata.cost, sell WITH avqudata.sell, qty WITH avqudata.qty
        REPLA markup WITH avqudata.markup, discount WITH avqudata.discount, sell_ext WITH avqudata.sell * avqudata.qty
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
CLOSE DATA ALL

PROCEDURE ImportQuotes2 &&-----------------------------
** from ACTOOLS?
CLOSE DATA ALL
SELE 0
USE avqu_itm
ZAP
APPE FROM n:\foxpro2\gardner\avqu_itm
SCAN
  REPLA qkey WITH STR(wo_num,8)+STR(task_num,3),corr_act WITH ALLT(discr_act),discr_act WITH SPAC(0);
    ,job_type WITH [STANDARD],status WITH IIF(EMPT(fini_date),[IN PROCESS],[FINISHED]), l_rts WITH .T.;
    ,priority WITH [NORMAL]
ENDSCAN

SELE 0
USE avqu_lbr
ZAP
APPE FROM n:\foxpro2\gardner\avqu_lbr
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3), l_ot WITH .F.,ot_multi WITH 1.5

SELE 0
USE avqu_eqp
ZAP
SELE 0
USE avqu_pu
ZAP
APPE FROM n:\foxpro2\gardner\avqu_pu FOR EMPT(serial)
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3), lcharge WITH .F.
SELE avqu_eqp
APPE FROM n:\foxpro2\gardner\avqu_pu FOR !EMPT(serial)
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3), lcharge WITH .F., model WITH pnum
REPLA ALL pnum WITH SPAC(0), markup WITH IIF(sell>0.and.cost>0,MIN(999.9,ROUN(((avqu_eqp.sell/avqu_eqp.cost)*100)-100,2)),0)

SELE 0
USE avqu_oth
ZAP
APPE FROM n:\foxpro2\gardner\avqu_oth
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3)

SELE 0
USE avqu_813
ZAP
APPE FROM n:\foxpro2\gardner\avqu_813
USE IN avqu_813

SELE 0 &&------------
USE avqu_po
ZAP
APPE FROM n:\foxpro2\gardner\avqu_po
REPLA ALL qkey WITH STR(wo_num,8)+STR(task_num,3)
USE IN avqu_po

SET ORDER TO qkey  IN avqu_lbr
SET ORDER TO qkey  IN avqu_pu
SET ORDER TO qkey  IN avqu_oth
SET ORDER TO qkey  IN avqu_eqp
SET ORDER TO qnum IN avqu_itm

SELE 0
USE avcustom
SELE 0
USE avqu_mtr
ZAP
APPE FROM n:\foxpro2\gardner\avqu_mtr
SCAN
  IF SEEK(PADR(ALLT(UPPE(avqu_mtr.cmnem)),6),[avcustom],[accmnem]).and.!EMPT(avcustom.cmnem)
    REPLA avqu_mtr.cmnem    WITH avcustom.cmnem
    REPLA avqu_mtr.customer WITH IIF(avcustom.cperbus = 'B',avcustom.cname,avcustom.cperson)
    REPLA avqu_mtr.person   WITH avcustom.cperson
    REPLA avqu_mtr.addr     WITH avcustom.caddress
    REPLA avqu_mtr.addr2    WITH avcustom.caddress2
    REPLA avqu_mtr.city     WITH avcustom.ccity
    REPLA avqu_mtr.state    WITH avcustom.cstate
    REPLA avqu_mtr.country  WITH avcustom.ccountry
    REPLA avqu_mtr.phone    WITH avcustom.cphone
    REPLA avqu_mtr.FAX      WITH avcustom.cfax
    REPLA avqu_mtr.zip      WITH avcustom.czip
  ENDIF

  STORE 0 TO zzWOttlS
  SELE avqu_itm
  SEEK avqu_mtr.wo_num
  SCAN WHILE avqu_itm.wo_num = avqu_mtr.wo_num

    STORE 0 TO XXX,yyy,zzz
    SELE avqu_lbr
    SEEK avqu_itm.qkey
    SCAN WHILE avqu_lbr.qkey = avqu_itm.qkey
      yyy = yyy +  (hrs_billd * rate)  &&sell
    ENDSCAN
    SELE avqu_itm
    yyy = IIF(avqu_itm.isflatlbr,avqu_itm.flat_lbr,yyy)
    REPLACE avqu_itm.labor_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy,zzz  && include because import may put some items into equipment
    SELE avqu_eqp
    SEEK avqu_itm.qkey
    SCAN WHILE avqu_eqp.qkey = avqu_itm.qkey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avqu_itm
    yyy = IIF(avqu_itm.isflateqp,avqu_itm.flat_eqp,yyy)
    REPLACE avqu_itm.equip_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy
    SELE avqu_pu
    SEEK avqu_itm.qkey
    SCAN WHILE avqu_pu.qkey = avqu_itm.qkey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avqu_itm
    yyy = IIF(avqu_itm.isflatpts,avqu_itm.flat_pts,yyy)
    REPLACE avqu_itm.parts_ttl WITH MIN(99999999.99,yyy)

    STORE 0 TO XXX,yyy,zzz
    SELE avqu_oth
    SEEK avqu_itm.qkey
    SCAN WHILE avqu_oth.qkey = avqu_itm.qkey
      yyy = yyy + (ROUN(qty*sell,2))
    ENDSCAN
    SELE avqu_itm
    yyy = IIF(avqu_itm.isflatoth,avqu_itm.flat_oth,yyy)
    REPLACE avqu_itm.other_ttl WITH MIN(99999999.99,yyy)

    SELE avqu_itm
    zGrandTotal = avqu_itm.labor_ttl+avqu_itm.equip_ttl+avqu_itm.parts_ttl+avqu_itm.other_ttl
    REPLACE total_ttl WITH zGrandTotal
    zzWOttlS=zzWOttlS + avqu_itm.total_ttl &&(avqu_itm.labor+avqu_itm.equip+avqu_itm.parts - avqu_itm.trade)
  ENDSCAN
  SELE avqu_mtr
  REPLACE avqu_mtr.sell_ttl WITH zzWOttlS
ENDSCAN
USE IN avqu_mtr
USE IN avqu_itm
USE IN avqu_lbr
USE IN avqu_eqp
USE IN avqu_pu
USE IN avqu_oth
USE IN avaircra
USE IN avcustom

CLOSE DATA ALL

PROC  MoahWOStuff &&===================
&&&  zzcalc
PROCEDURE BuildQuoteDummy&&--------------------------------------
RETURN
USE avqu_mtr IN 0
USE avqu_itm IN 0
USE avqu_lbr IN 0
USE avqu_eqp IN 0
USE avqu_prt IN 0
USE avqu_trd IN 0

FOR ttt = 1000 TO 2000 STEP 1
  SELE avqu_mtr
  APPE BLANK
  REPL qnum WITH ttt
  SELE avqu_itm
  FOR XXX = 1 TO 10
    SELE avqu_itm
    APPEND BLANK
    REPLA qnum WITH ttt, qitem WITH XXX, qkey WITH STR(ttt,8)+STR(XXX,3),descrip WITH [ITEM ]+STR(ttt,8)+STR(XXX,3)
    SELE avqu_lbr
    FOR lll = 1 TO 15
      SELE avqu_lbr
      APPEND BLANK
      REPLA qnum WITH ttt, qitem WITH XXX, qkey WITH STR(ttt,8)+STR(XXX,3),descrip WITH [LABOR ]+STR(ttt,8)+STR(XXX,3)
      REPLA hours WITH  lll, cost_hr WITH lll, cost_ext WITH hours * cost_hr
    ENDFOR
    SELE avqu_eqp
    FOR eee = 1 TO 15
      SELE avqu_eqp
      APPEND BLANK
      REPLA qnum WITH ttt, qitem WITH XXX, qkey WITH STR(ttt,8)+STR(XXX,3),descrip WITH [EQUIP ]+STR(ttt,8)+STR(XXX,3)
      REPLA cost WITH eee, sell WITH eee * 10
    ENDFOR
    SELE avqu_prt
    FOR ppp = 1 TO 15
      SELE avqu_prt
      APPEND BLANK
      REPLA qnum WITH ttt, qitem WITH XXX, qkey WITH STR(ttt,8)+STR(XXX,3),descrip WITH [PARTS ]+STR(ttt,8)+STR(XXX,3)
      REPLA cost WITH ppp, sell WITH ppp * 10
    ENDFOR
  ENDFOR
ENDFOR
CLOSE DATA

