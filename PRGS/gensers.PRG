*\\ 24Sep18 28Feb18 31Jan18 04Oct17 28Aug17 28Feb17 23Feb17 08Feb17 17Jan17 10Jan17 03Jan17 29Oct16 28Oct16 27Oct16 18Oct16 15Oct16 14Sep16 03Aug16 07Apr16 05Apr16 19Mar16 24Sep15 14Sep15 28Apr15 14Apr15 13Apr15 07Nov14 22Oct14 09Sep2014 12Aug14
**DO GenSers WITH qgen.part,qgen.rev,qgen.job,qtyneed,custcode,.F.,DATE(),qgen.kit_date &&==========
PARA jPart,jRev,jJob,jQty,jCode,joverride,jdate,jkitdate,jcomms,juser,jsilent,jvariant,jforce
PRIV jPart,jRev,jJob,jQty,jCode,joverride,jdate,jkitdate,jCust,jBody,y172,woy172,nextnoyes,nextgeo,cfam55,jcomms,juser,jorigpart,jsilent,nextsun4,jvariant,jstatus,jforce
jstatus = [Part ]+jpart+zCrLf+[Code ]+jcode+zCrLf+[Lot ]+jjob+zCrLf

IF EMPT(jkitdate)  && 01May14  now using jkitdate, but should always behave like normal when called from kitting form, because it is today
  jkitdate = jdate &&   if called after the fact from Test Data, then it should use the kitdate rather than today
ENDIF
IF jJob = [ 68917] && already done as CEL002 fake - don't do again 
  RETURN
ENDIF
IF [DEPOT] $ jvariant .and. jJob # [ 87565] 
  RETURN
ENDIF
*!*	    IF FILE("C:\Pete_Dev.VLD")
*!*	      WAIT jpart + Jcode WIND
*!*	    ENDIF
IF joverride && this is always used for VFP > Kitting module  and  VFP > Test Data
  IF !jForce .and. ( ([RMA ] $ qgen.prod_code .and. !ALLT(jcode)==[CEL002]) .OR. [CAB ] $ qgen.prod_code .OR. [MSC ] $ qgen.prod_code )   && 22Oct14 added RMA exclusion
    SELE qgen
    REPL qgen.serhist WITH ALLT(qgen.serhist)+IIF(!EMPT(qgen.serhist),zCrLf,[]);
      +[(1) Gensers called  ]+DTOC(DATE())+[ ]+LEFT(TIME(),5)+[  ]+ALLT(juser)+[  no serials created because Product Code = ]+ALLT(qgen.prod_code )
    IF jsilent
      _gserback=[]
      RETURN
    ELSE
      _gserback = [NO serials required for Product Code  ]+ALLT(qgen.prod_code)+[. ]
      RETURN
    ENDIF
  ENDIF
ELSE
&& should just look to see if this lot has serials already and not depend on qgen.nserxxxxx
  IF !jForce .and. (([RMA ] $ qgen.prod_code .and. !ALLT(jcode)==[CEL002]) $ qgen.prod_code .OR. [MSC ] $ qgen.prod_code .OR. qgen.nserstart > 0 .OR. qgen.nserend > 0 )   && 22Oct14 added RMA exclusion
    SELE qgen
    REPL qgen.serhist WITH ALLT(qgen.serhist)+IIF(!EMPT(qgen.serhist),zCrLf,[]);
      +[(2) Gensers called  ]+DTOC(DATE())+[ ]+LEFT(TIME(),5)+[  ]+ALLT(juser)+[  no serials created because Product Code = ]+ALLT(qgen.prod_code )
    IF jsilent
      _gserback=[]
      RETURN
    ELSE
      _gserback=[NO serials required for Product Code  ]+ALLT(qgen.prod_code)+[. ]
      RETURN
    ENDIF
  ENDIF
ENDIF
IF FILE("C:\Pete_Dev.VLD")
  *WAIT [JQTY  ]+n_s(jqty) WIND
ENDIF
jCust = IIF(SEEK(jCode,[customer],[code]),customer.company,[])
=Open_Dbf("serlog")
=Open_Dbf("main_m")
=Open_Dbf("mainsers")
zjtop_part=[]

lcs127128 = [CEL002] $ jCode .and. ([CS18-115-127] $ jpart.or.[CS18-115-128] $ jpart.or.[CS18-200-101] $ jpart) &&02Jan17
*lcs10     = [CEL002] $ jCode .and. (LEFT(jPart,5) == [CS10-].or.LEFT(jPart,5) == [CS01-]) &&10Jan17
lcs10     = [CEL002] $ jCode .and. (LEFT(jPart,5) == [CS10-].or.LEFT(jPart,5) == [CS01-].or.LEFT(jPart,5) == [CS33-]) &&10Jan17 Added CS33- per Guy 01/10/2020 DG
la90poe   = [CEL002] $ jCode .and. (LEFT(jPart,8) == [A90-POE-])
SELE 0
USE F:\NACFOX\main_m AGAIN ALIAS main_m22
SET ORDER TO top_part IN main_m22
SET ORDER TO part_nser IN serlog
SET ORDER TO prv IN main_m
y172=[-]
DO getyear7272 && YY
**woy172 = WeekOfYear(jdate)
**moy172 = SUBS(DTOS(DATE()),5,2)
woy172 = WeekOfYear(jkitdate)
moy172 = SUBS(DTOS(jkitdate),5,2)
yy172  = SUBS(DTOS(jkitdate),3,2)  && 20161028
STORE [] TO cfull_start,cfull_stop,cactions
STORE 0 TO actlabels67
STORE .F. TO lforce_cs
jorigpart = jPart
IF SEEK( jPart + jRev + jvariant ,[main_m],[prv])  &&&& .and. main_m.misc05_ &&#############################################  << flag for needing serials
    IF FILE("C:\Pete_Dev.VLD")
      *WAIT jpart + Jcode WIND
    ENDIF
  IF 111=222  &&!EMPT(main_m.top_part).and.SEEK(main_m.top_part,[main_m22],[top_part])
    *// old way of getting to rootpart, don't use any more
    *!*	    zjtop_part= main_m22.top_part
    *!*	    mxNextSer = MAX(1,main_m22.nextser)
    *!*	    jStart    = MAX(1,main_m22.nextser)
    *!*	    jStop     = main_m22.nextser + jQty - 1
  ELSE
    IF EMPT(main_m.rootpart)  &&&&&&&&&&&&&&&&&&&   needs enhanced routine here to find root part
      d7  = xDropTees(main_m.PART)
      IF d7 # main_m.PART && this part has an undesireable suffix in list, so process to find root, this essential filters candidates
        SELE main_m
        p7 = ALLT(main_m.PART)
        nl7= LEN(ALLT(p7))
        r7 = main_m.PART
        FOR jjj = nl7 TO 1 STEP -1  && this routine can often find the wrong root part because it digs too deeply
          p6 = LEFT(p7,jjj)         && should only do this if
          **WAIT p7+[  ]+p6 WIND
          IF SEEK(PADR(p6,19),[main_m2],[part])
            r7  = main_m22.PART
          ELSE
            **EXIT
          ENDIF
        ENDFOR
        REPL rootpart WITH r7 IN main_m
        cactions = cactions + [Generated new RootPart: ]+ALLT(r7)+zCrLf
      ELSE
        REPL rootpart WITH main_m.PART IN main_m
      ENDIF
    ELSE
      cactions = cactions + [Main RootPart: ]+ALLT(main_m.rootpart)+[  already exists]+zCrLf
    ENDIF
    IF SEEK(main_m.rootpart,[mainsers],[part])  &&SEEK(main_m.rootpart,[main_m22],[part])
      IF [EFI001] $ ALLT(jcode) .and. mainsers.nextser < 100 && this should only be needed the 1st lot for each assembly
        REPLACE mainsers.nextser WITH 100 IN mainsers
      ENDIF
      dlast7 = mainsers.dserlast
      IF jkitdate > mainsers.dserlast
        REPL dserlast WITH jkitdate IN mainsers && update mainsers WITH this kitdate - if newer
        **WAIT [Updating  mainsers.dserlast   ]+DTOC(mainsers.dserlast)+[   jKitDate   ]+DTOC(jKitDate) WIND
      ENDIF
      IF [FIR002] $ ALLT(jcode) .and. mainsers.dserlast > dlast7 && this group of serials is newer than currently stored
        * need to reset ser# back to 1 if year of jkitdate > year of mainsers.dserlast
        IF YEAR(mainsers.dserlast) > YEAR(dlast7) && have now transitioned from one year to another
          REPL mainsers.nextser WITH 1 IN mainsers
          **WAIT [FIR002    mainsers.nextser  ]+n_s(mainsers.nextser) WIND
        ENDIF
      ENDIF
      mxNextSer = MAX(1,mainsers.nextser) && ignored for certain customers with special reqs
      jStart    = MAX(1,mainsers.nextser)
      jStop     = mainsers.nextser + jQty - 1 &&07Mar06 fixed by subtracting 1
      cactions = cactions + [Used existing MAINSERS entry RootPart: ]+ALLT(mainsers.PART)+[  Nextser: ]+ALLT(STR(mxNextSer))+zCrLf
      zjtop_part= []
    ELSE
      SELE mainsers  && root not found in mainsers so make an entry
      APPEND BLANK
      REPLACE PART WITH main_m.rootpart IN mainsers
      IF [EFI001] $ ALLT(jcode) .and. mainsers.nextser < 100 && this should only be needed the 1st lot for each assembly
        REPLACE mainsers.nextser WITH 100 IN mainsers
      ENDIF
      REPL dserlast WITH jkitdate IN mainsers
      mxNextSer = MAX(1,mainsers.nextser) && ignored for certain customers with special reqs
      jStart    = MAX(1,mainsers.nextser)
      jStop     = mainsers.nextser + jQty - 1 &&07Mar06 fixed by subtracting 1
      cactions = cactions + [Created new MAINSERS entry RootPart: ]+ALLT(mainsers.PART)+[  Nextser: ]+ALLT(STR(mxNextSer))+zCrLf
      *!*	      mxNextSer = MAX(1,main_m.nextser) && ignored for certain customers with special reqs
      *!*	      jStart    = MAX(1,main_m.nextser)
      *!*	      jStop     = main_m.nextser + jQty - 1 &&07Mar06 fixed by subtracting 1
      zjtop_part= []
    ENDIF
  ENDIF
  ctext17 = []
  IF [CEL002] $ jCode
    ** started again 26Sep2105 - serlog entry part# disagree with parent qgen part#, Fixing shipping to ignore Part#
    ** stopped again 24Sep2105 - causing issues where serlog entry part# disagree with parent qgen part#,
    *  places like shipping stop when part is marked TEST=Y
    jorigpart = zDropTees(jorigpart) && 14Sep15 stop duplicates   function below
  ENDIF

  SELE serlog && 01Aug14 added this to find highest serial used and bump up what was in main_m.nextser
  SET ORDER TO part_nser DESC IN serlog && in case it wasn't updated correctly
  SET KEY TO                && the tag above in desc will always find highest ser even if gaps exist
  SEEK jorigpart  && still using orig part to check for pushout
  IF FOUND() .AND. serlog.PART == jorigpart .AND. serlog.sernum > mxNextSer .and. ![FIR002] $ jcode 
  
    *// FIR002 requires reset to ser# 1 each calendar year (handled above) so can't use this to look at prev serials
    
    *// 05Aug14 push was on wrong record ifr no serials ever,  added  part=part
    ctext17 = ctext17 + [ Pushed Next Ser# from main_m.nextser ]+n_s(mxNextSer)+[ to ]+n_s(serlog.sernum + 1)+[ ]
    mxNextSer = serlog.sernum + 1
    jStart    = mxNextSer
    jStop     = mxNextSer + jQty - 1 &&07Mar06 fixed by subtracting 1
  ENDIF


** test 01/26/17
  IF !EMPT(main_m.label_part) .AND. [CEL002] $ jCode .AND. LEFT(jPart,2) # [CS] .AND. LEFT(main_m.label_part,2) == [CS]
    lforce_cs = .T. && force any part which has a CS # in main_m.label_part to mimic that part for creating serials
    jPart = PADR(ALLT(UPPE(main_m.label_part)),19)
  ENDIF  && stopped 28Oct16

  IF !lcs127128 .and. !EMPT(main_m.label_part) .AND. [CEL002] $ jCode .AND. LEFT(jPart,4) # [CS18] .AND. LEFT(main_m.label_part,4) == [CS18]
    *// 28Oct16 changed this to only be for CS18  so skips CS10 
    *// 02Jan17 skips if 127 or 128 
    lforce_cs = .T. && force any part which has a CS # in main_m.label_part to mimic that part for creating serials
    jPart = PADR(ALLT(UPPE(main_m.label_part)),19)
  ENDIF

  SELE serlog
  SET ORDER TO part_nser ASCE IN serlog  &&  < important to set back to ASCE
  SET KEY TO &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&& certain cust / assemblies get serial range by looking at exist serials in serlog and override what is in main_m
&& most of these candidates use the second serial#  sernum2

  STORE 0 TO nextnoyes,nextgeo,nextcsi,nextoztek,currsun4,nextsun4
  IF [FIP001] $ jCode   &&  FIPLEX  overwrite above mainsers
    jpart2 = jPart
    jpart3 = jPart
    ***jorigPart = jpart2
    IF SEEK(PADR([FIP001_ALL],19),[mainsers],[part]) && maintaining a single serial# stream for FIP001 for all parts
      mxNextSer = MAX(1,mainsers.nextser) && using an entry in mainsers with [FIP001_ALL] as part#
      jStart    = MAX(1,mainsers.nextser)
      jStop     = mainsers.nextser + jQty - 1 &&07Mar06 fixed by subtracting 1
      cactions = cactions + [] &&
      REPLACE nextser WITH jStop+1 IN mainsers
    ELSE  && didn't find in mainsers
    ENDIF
  ENDIF

  IF [OZT001] $ jCode   &&  overwrite above mainsers
    jpart2 = jPart
    jpart3 = jPart
    IF [10879-01] $ jPart .OR. [10879-02] $ jPart && looking for 10879-01 & 10879-02 .or. 30-10879-01 & 30-10879-02 to sequence serials across both parts
&& might have 30- prefixes  so 10879-01,10879-02,30-1879-01,30-10879-02  will all use 10879-01 mainsers
      jpart2 = [10879-01           ]  && force
      jpart3 = [10879-02           ]
    ELSE
      *!*	       IF SUBS(jpart,3,1)=[-]  && ex: 30-10123-01  && 14Oct16 stopped - only trap 10879-01,10879-02,30-1879-01,30-10879-02 for special scheme
      *!*	         jpart2 = ALLT(SUBS(jpart,4,20))
      *!*	       ENDIF
    ENDIF
    ***jorigPart = jpart2
    IF SEEK(jpart2,[mainsers],[part])  &&SEEK(main_m.rootpart,[main_m22],[part])
      mxNextSer = MAX(1,mainsers.nextser) && ignored for certain customers with special reqs
      jStart    = MAX(1,mainsers.nextser)
      jStop     = mainsers.nextser + jQty - 1 &&07Mar06 fixed by subtracting 1
      cactions = cactions + [Used existing MAINSERS entry RootPart: ]+ALLT(mainsers.PART)+[  Nextser: ]+ALLT(STR(mxNextSer))+zCrLf
      REPLACE nextser WITH jStop+1 IN mainsers
    ELSE  && didn't find in mainsers
    ENDIF

    SELE serlog
    SET ORDER TO code_fser ASCE IN serlog  && looking thru previous serials
    SET KEY TO [OZT001]
    SEEK [OZT001]
    SCAN
      IF serlog.CODE = [OZT001] .AND. serlog.sernum > 0 .AND. (ALLT(jpart2) $ serlog.PART.OR.ALLT(jpart3) $ serlog.PART) .AND. BETW(serlog.sernum,jStart,jStop)
        mxNextSer = mxNextSer  + 1   && IMPORTANT, above is looking for 10879-01.or.10879-02 contained in part#
        jStart    = MAX(1,mxNextSer )
        jStop     = mxNextSer + jQty - 1 &&07Mar06 fixed by subtracting 1
      ENDIF
    ENDSCAN
    **WAIT jpart2+[  nextoztek  ]+STR(nextoztek) WIND
  ENDIF


  **IF [CEL002] $ jCode .AND. (LEFT(jPart,2) == [CS] .OR. lforce_cs) && start sernum2 at last sernum2 + 1 if any PARTS in same month
  IF !lcs127128 .and. [CEL002] $ jCode .AND. (LEFT(jPart,4) == [CS18] .OR. lforce_cs) && 28Oct16 changed to CS18  && start sernum2 at last sernum2 + 1 if any PARTS in same month
    ** 28Apr2015
    ** example if origpart = 031-115-101 .and. main_m.label_part is [CS18-115-101] because of forcing a change to label part# (above)
    ** then jpart is now  CS18-115-101 and the below code won't find any existing because serlog.part is still 031-115-101
    jsearch92 = jorigpart
    IF LEN(ALLT(jpart)) = 13 .and. LEFT(jpart,9) = [CS18-115-] .and. ISALPHA(SUBS(jpart,13,1))  .and. ! ISALPHA(SUBS(jpart,12,1))
      *// 23Feb17 example CS18-115-148S  caused duplicates WITH CS18-115-148Q - need to check against root part CS18-115-148 
      jsearch92 = LEFT(jpart,12) && override
    ENDIF
    ** need to increment nextcsi properly
    ** must gen temp serial (insp2) and see if it exists
    ** may need to modify code below so it is NOT found while looping
    cs333 = Cs3DigModl(jPart)
    jrev1 = RIGHT(ALLT(jRev),1) && in case might not be a single digit alpha  ex: 005
    IF EMPT(jrev1)
      jrev1=[?]
    ENDIF
    srchser = [S]+cs333+ALLT(jrev1)+MoProd()+SUBS(DTOS(jkitdate),3,2)  && EX: S148LB17
    SELE serlog
    SET ORDER TO insp2 DESC
    SET KEY TO srchser  && only 1st 8 char
    GO BOTT
    LOCATE
    nextcsi = serlog.sernum2
    jstatus = jstatus+[NextCsi ]+n_s(nextcsi)+zCrLf

*!*	    IF FILE("C:\PyroPete.VLD").or.FILE("C:\Pete_Dev.VLD")
*!*	      WAIT [Highest ser# rec found  ]+DTOC(serlog.date)+[  ]+ALLT(serlog.part)+[  ]+n_s(serlog.sernum2)+[  nextcsi ]+n_s(nextcsi)+[  ]+serlog.insp2+[   key= ]+srchser WIND
*!*	      *BROW
*!*	    ENDIF
    SELE serlog
    SET ORDER TO part_nser ASCE
    SET KEY TO

*!*	    SELE serlog
*!*	    SET ORDER TO part_nser IN serlog  && looking thru previous serials
*!*	    SEEK jorigpart  && this "could be" faked if lforce_cs
*!*	    SCAN WHILE serlog.PART = jorigpart
*!*	      IF serlog.sernum2 > 0 .AND. BETWEEN(serlog.DATE,jkitdate-31,jkitdate+31) .AND. SUBS(DTOS(serlog.DATE),5,2) = moy172 && same month as today
*!*	        nextcsi = IIF(serlog.sernum2 > nextcsi, serlog.sernum2 ,nextcsi)
*!*	      ENDIF
*!*	    ENDSCAN
    jStart = nextcsi
    jStop  = nextcsi + jQty - 1
  ENDIF

  IF [NOY001] $ jCode && start sernum2 at last sernum2 + 1 if any PARTS in same week
    SELE serlog
    SET ORDER TO part_nser IN serlog
    SEEK jPart
    SCAN WHILE serlog.PART = jPart
      **IF serlog.sernum2 > 0 .and. BETWEEN(serlog.DATE,DATE()-10,DATE()+10) .and. WeekOfYear(serlog.DATE) = woy172 && same work week
      **  nextnoyes = IIF(serlog.sernum2 > nextnoyes, serlog.sernum2 ,nextnoyes)
      **ENDIF
      IF serlog.sernum2 > 0 .AND. BETWEEN(serlog.DATE,jkitdate-10,jkitdate+10) .AND. WeekOfYear(serlog.DATE) = woy172 && same work week
        nextnoyes = IIF(serlog.sernum2 > nextnoyes, serlog.sernum2 ,nextnoyes)
      ENDIF
    ENDSCAN
    jStart = nextnoyes
    jStop  = nextnoyes + jQty - 1
  ENDIF

  IF [GEO004] $ jCode && start sernum2 at last sernum2 + 1 if any PARTS in same week that have the same FAMILY
    *// FF = 2 digit "family"  Ex A28-005  28 is the family
    *// so need to scan here by code not part
    cfam55 = []
    nstart55 = 0
    DO WHILE nstart55 < 20 &&EMPT(cfam55)
      nstart55 = nstart55 + 1
      IF ISDI(SUBS(jPart,nstart55,1)).AND.ISDI(SUBS(jPart,nstart55+1,1))
        cfam55 = SUBS(jPart,nstart55,2) && find first pair of digits
        EXIT
      ENDIF
    ENDDO
    cfam55 = IIF(EMPT(cfam55),SUBS(jPart,2,2),cfam55) && use 2nd+3rd char of part# if no twin digits found
    SELE serlog
    SET ORDER TO code_fser IN serlog
    SET KEY TO jCode
    SEEK jCode
    SCAN FOR serlog.CODE = jCode .AND. LEFT(serlog.full_ser,2) = cfam55
      **IF serlog.sernum2 > 0 .and. BETWEEN( serlog.DATE ,DATE()-10 ,DATE()+10 ) .and. WeekOfYear(serlog.DATE) = woy172 && same work week
      **  nextgeo = IIF(serlog.sernum2 > nextgeo, serlog.sernum2 ,nextgeo)
      **ENDIF
      IF serlog.sernum2 > 0 .AND. BETWEEN( serlog.DATE ,jkitdate-10 ,jkitdate+10 ) .AND. WeekOfYear(serlog.DATE) = woy172 && same work week
        nextgeo = IIF(serlog.sernum2 > nextgeo, serlog.sernum2 ,nextgeo)
      ENDIF
    ENDSCAN
    jStart = nextgeo
    jStop  = nextgeo + jQty - 1
    IF FILE("C:\PyroPete.VLD")
      *WAIT [CTI  Only !      GEO004 ]+STR(nextgeo,4) WIND
    ENDIF
  ENDIF

  IF [SUN004] $ jCode && start sernum at last sernum + 1 regardless of part or lot  SUNRISE LABS  &&12Apr15
    SELE serlog
    SET ORDER TO code_fser IN serlog
    SET KEY TO jCode
    SEEK jCode
    SCAN FOR serlog.CODE = jCode
      IF serlog.sernum > 0
        currsun4 = IIF(serlog.sernum > currsun4, serlog.sernum ,currsun4)
      ENDIF
    ENDSCAN
    IF currsun4 < 29 &&12Apr15  Guy wants ser# to start at 30 - some already in sserlog < 30
      currsun4 = 29
    ENDIF
    mxNextSer = IIF(currsun4 > 0, currsun4 + 1, qqq )  && qqq is current # in for/endfor   currsun4 + 1
    jStart = mxNextSer
    jStop  = mxNextSer + jQty - 1
    IF FILE("C:\PyroPete.VLD")
      WAIT [CTI  Only !     NEXT   SUN004  ]+STR(mxNextSer,4) WIND
    ENDIF
  ENDIF

    IF FILE("C:\Pete_dev.VLD") &&.and. jfound 
      **WAIT [CTI  Only !     jQty= ]+n_s(jqty)+[  ]+jpart+[   ]+STR(mxNextSer,4) WIND
    ENDIF
  cxserlist82 = []
  FOR qqq = 1 TO jQty  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    SELE serlog
&&  mxNextSer gets incremented at end of this  for/endfor
    DO CASE
      CASE [SUN004] $ jCode
        SET ORDER TO code_fser DESC IN serlog
        SET KEY TO && remove any keys
        SEEK jCode+[S]+STRTR(STR(mxNextSer,5),[ ],[0])  && this is checking for CODE/Fullser for SUN004
      CASE [OZT001] $ jCode
        SET ORDER TO part_nser ASCE IN serlog
        SET KEY TO && remove any keys   &&####  14Oct16 perhaps an issue because jpart2 could have been forced to 10879-01
        **SEEK jpart2+STR(mxNextSer,14)  &&####  && stopped 14Oct16 shouldn't look for jpart2 because can be force to 10879-01  USE jorigpart
        SEEK jorigpart+STR(mxNextSer,14)  &&####  same as below
      OTHERWISE
        SET ORDER TO part_nser ASCE IN serlog
        SET KEY TO && remove any keys
        SEEK jorigpart+STR(mxNextSer,14)  && this is checking the original part# in case was faked for CSI see lforce_cs
    ENDCASE
    *!*	    IF [SUN004] $ jcode
    *!*	      SET ORDER TO code_fser DESC IN serlog
    *!*	      SET KEY TO && remove any keys
    *!*	      SEEK jcode+[S]+STRTR(STR(mxNextSer,5),[ ],[0])  && this is checking for CODE/Fullser for SUN004
    *!*	    ELSE
    *!*	      SET ORDER TO part_nser ASCE IN serlog
    *!*	      SET KEY TO && remove any keys
    *!*	      SEEK jorigPart+STR(mxNextSer,14)  && this is checking the original part# in case was faked for CSI see lforce_cs
    *!*	    ENDIF
    **  SUN004 doesn't care wich Part# so this could get in trouble
    ***WAIT jorigPart+STR(mxNextSer,14) WIND TIME .1
    jfound = FOUND() &&&&&&&&&&.and. ![SUN004] $ jcode               &&.and. [CEL002] $ jcode .and. lforce_cs
    IF FILE("C:\Pete_dev.VLD") &&.and. jfound 
      **WAIT [CTI  Only !     jfound()= ]+IIF(jfound,[T],[F])+[   ]+STR(mxNextSer,4) WIND
    ENDIF
    IF [CEL002] $ jCode .and. ( lcs10 .or. lcs127128 .or. la90poe )
      cs10key = yy172+[SP]+moy172  && just the YYLLMM portion yy172 and moy172 assigend above from kitdate

      *// filter to just items matching 1st 6 chars YYSPMM
      *// set orde to DESC and find latest entry which has highest number - then add 1 to it for next
      *// ################  must prevent any duplicates across all part# involved
      *// ######################################################################
      SELE serlog
      SET ORDER TO insp2 DESC IN serlog
      SET KEY TO cs10key
      LOCATE
      mxNextSer = MAX(1,serlog.sernum2 + 1)
      
      jfound = .F. &&&&&&&&&&& force below to work when lcs10 .or. lcs127128
      
      *WAIT jCode +[   jorigpart   ]+ALLT(jorigpart)+[    jqty  ]+STR(jqty)+[   mxnextser  ]+STR(mxNextSer) WIND
      SELE serlog
      SET ORDER TO part_nser ASCE IN serlog
      SET KEY TO
      LOCATE
    ENDIF 
    IF [FIR002] $ ALLT(jcode)
      jfound = .F. &&&&&&&&&&& force below to work when Fireye
    ENDIF
    IF !jfound  && don't make it if it already exists
      *DO wwindon WITH [Issuing Serial # ]+STR(mxNextSer,10)
      SELE serlog
      APPE BLANK
      actlabels67 = actlabels67 + 1
      REPL sernum WITH mxNextSer, lotnum WITH jJob,comments WITH jcomms + IIF(lforce_cs,[ for ]+ALLT(jorigpart),[]);
        ,PART WITH jorigpart, prefix WITH main_m.serprefix;
        ,suffix WITH main_m.sersuffix;
        ,full_ser WITH ALLT(serlog.prefix)+ALLT(STR(serlog.sernum,14))+ALLT(serlog.suffix);
        ,CODE WITH jCode,rev WITH jRev;
        ,DATE WITH jkitdate,TIME WITH [00:00];
        ,customer WITH jCust,top_part WITH zjtop_part;
        ,employee WITH [AutoGenerated] IN serlog   &&21May14  started using kit_date rather than DATE()
      
      ** && NOW   OVERWRITE  some of above fields if needed 
      
      **IF [CEL002] $ jCode .AND. (LEFT(jPart,2) == [CS] .OR. lforce_cs)  && overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      IF !lcs127128 .and. [CEL002] $ jCode .AND. (LEFT(jPart,5) == [CS18-] .OR. lforce_cs)  && 28OCT16 just for CS18  now      overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        *// ref: CSI QMS-C-007
        jpartcsi = zDropTees(jPart) && function below
        nextcsi = IIF(nextcsi> 0, nextcsi + 1, qqq )
        REPL sernum2 WITH nextcsi IN serlog
        cs333 = Cs3DigModl(jPart)
        IF 1=2 &&ALLT(LEN(cs333)) # 3
          cs333 = RIGHT(ALLT(jpartcsi),3)
        ENDIF
        jrev1 = RIGHT(ALLT(jRev),1) && in case might not be a single digit alpha  ex: 005
        IF EMPT(jrev1)
          jrev1=[?]
        ENDIF
        ***IF 111=111 &&!EMPT(serlog.prefix).and.!EMPT(serlog.suffix)
        cextraG=[]
        IF INLI(ALLT(jPart),[CS18-115-106],[CS18-115-116],[CS18-115-125],[CS18-115-126])
          cextraG=[G] && per GuyN 27May15  means  Gain adjustment
        ENDIF
        L34a=LEFT(jpart,9)==[CS18-115-] .and. INLI(SUBS(jpart,10,4),[141S],[142S],[143S],[144S],[145S],[146S],[147S],[148S])
        L34b=INLI(ALLT(jPart),[CS18-115-141Q],[CS18-115-142Q],[CS18-115-144Q],[CS18-115-145Q],[CS18-115-147Q],[CS18-115-148Q])
        IF L34a.or.L34b
          cextraG=[H] && 145Q,147Q,148Q added 27Oct16 per GuyN    141Q,142Q,144Q added per GuyN 03Aug16
        ENDIF
        SELE serlog
        REPL insp2 WITH [S]+cs333+ALLT(jrev1)+MoProd()+SUBS(DTOS(jkitdate),3,2)+STRTR(STR(serlog.sernum2,4),[ ],[0])+cextraG IN serlog
        jstatus = jstatus+[Sernum2 / Insp2 ]+n_s(serlog.sernum2)+[ ]+ALLT(serlog.insp2)+zCrLf
*!*	    IF FILE("C:\PyroPete.VLD").or.FILE("C:\Pete_Dev.VLD")
*!*	      WAIT [Serial Added  ]+DTOC(serlog.date)+[  ]+ALLT(serlog.part)+[  ]+n_s(serlog.sernum2)+[  nextcsi ]+n_s(nextcsi)+[  ]+serlog.insp2;
*!*	        +[   key= ]+srchser+[  S/N]+[S]+cs333+ALLT(jrev1)+MoProd()+SUBS(DTOS(jkitdate),3,2)+STRTR(STR(serlog.sernum2,4),[ ],[0])+cextraG WIND
*!*	      *BROW
*!*	    ENDIF
      ENDIF

      IF [CEL002] $ jCode .AND. ( lcs10 .or. lcs127128 .or. la90poe )  && 28OCT16 just for CS10 or 127/128  overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        REPL sernum2 WITH mxNextSer IN serlog
        c10serial = yy172+[SP]+moy172+STRTR(STR(mxNextSer,6),[ ],[0])  &&moy172 = SUBS(DTOS(jkitdate),5,2)  yy172  = SUBS(DTOS(jkitdate),3,2)
        REPL insp2 WITH c10serial, full_ser WITH c10serial IN serlog
        ***ENDIF
      ENDIF

      IF [NOR008]$jCode && overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        *  [SPN-]+yymm
        *  Ex:  SPN-12345
        REPL full_ser WITH [SPN-]+STRTR(STR(serlog.sernum,5),[ ],[0]) IN serlog
      ENDIF
      IF [EFI001]$jCode && overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        *  [CM-]+yymm+[-]+5dig erial
        *  Ex:  49-1804-12345
        REPL full_ser WITH ALLT(jorigpart)+[ S]+STRTR(STR(serlog.sernum,8),[ ],[0]) IN serlog
      ENDIF
      IF [DEG001]$jCode && overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        ** need to reset serials to 1 each Jan 1st
        *  [CM-]+yymm+[-]+5dig erial
        *  Ex:  49-1804-12345
        REPL full_ser WITH [49-]+ALLT(yy172)+ALLT(woy172)+[-]+STRTR(STR(serlog.sernum,5),[ ],[0]) IN serlog
      ENDIF
      IF [FIR002]$jCode && overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        ** need to reset serials to 1 each Jan 1st
        *  CM+part#+rev+yymm+ser
        *  strip out - hyphen if part# has one in 3rd position  ex 61-73797-1  becomes  6173797-1
        mlblpnum=ALLT(main_m.label_part)
        IF SUBS(mlblpnum,3,1) == [-]
          mlblpnum = ALLT(STUFF(mlblpnum,3,1,[]))   
        ENDIF
        *  Ex:  18617397-1218010001
        REPL full_ser WITH [18]+ALLT(mlblpnum)+ALLT(serlog.rev)+ALLT(yy172)+ALLT(woy172)+STRTR(STR(serlog.sernum,4),[ ],[0]) IN serlog
      ENDIF

      IF [NOY001]$jCode && overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        *//  need to check if this is there is any other lot for this ASSEMBLY/PART - this week.
        *// if so, start the new sernum2 at the next highest #
        nextnoyes = IIF(nextnoyes > 0, nextnoyes + 1, qqq )
        REPL sernum2 WITH nextnoyes IN serlog
        IF !EMPT(serlog.prefix).AND.!EMPT(serlog.suffix)
          REPL full_ser WITH ALLT(serlog.prefix)+ALLT(y172)+ALLT(woy172)+ALLT(serlog.suffix)+STRTR(STR(serlog.sernum2,3),[ ],[0]) IN serlog
        ENDIF
      ENDIF
      IF [GEO004]$jCode && overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        *//  need to check if this is there is any other lot for this ASSEMBLY/PART - this week.
        *// if so, start the new sernum2 at the next highest #
        nextgeo = IIF(nextgeo > 0, nextgeo + 1, qqq )
        REPL sernum2 WITH nextgeo IN serlog
        IF 111=111  &&!EMPT(serlog.prefix).and.!EMPT(serlog.suffix)  && FFWWYYNNNNVV  S4 is SCMI vendor code to GEO004
          REPL full_ser WITH cfam55 + ALLT(woy172) + SUBS(DTOS(jkitdate),3,2) +STRTR(STR(serlog.sernum2,4),[ ],[0])+[S4] IN serlog
        ENDIF
      ENDIF
      IF [SUN004]$jCode && overwrite &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        *// SUNRISE LABS  need to always use one seq for all lots / all ASSEMBLY/PART - never reset
        **nextsun4 = IIF(nextsun4> 0, nextsun4 + 1, qqq )  && qqq is current # in for/endfor
        REPL sernum   WITH mxNextSer IN serlog
        REPL full_ser WITH [S]+STRTR(STR(mxNextSer ,5),[ ],[0]) IN serlog
      ENDIF
      cfull_start = IIF(qqq=1,ALLT(serlog.full_ser),cfull_start)
      cfull_stop  = IIF(qqq=jQty,ALLT(serlog.full_ser),cfull_stop)
      WAIT [Issuing SER# ]+ALLT(STR(mxNextSer,10)) WIND NOWAIT
    ENDIF
    mxNextSer = mxNextSer + 1
    cxserlist82 = cxserlist82 +IIF(!EMPT(serlog.insp2),ALLT(serlog.insp2),ALLT(STR(serlog.sernum)))+zCrLf
  ENDFOR
  IF ![OZT001]$jCode  &&111=111 &&EMPT(zjtop_part)
    SELE main_m
    REPL nextser WITH mxNextSer
    cactions = cactions + [Updated nextser in MAIN_M Part: ]+ALLT(mainsers.PART)+[  ]+ALLT(STR(mxNextSer))+zCrLf
    IF mainsers.PART == main_m.rootpart
      cactions = cactions + [Updated nextser in MAINSERS Part: ]+ALLT(mainsers.PART)+[  ]+ALLT(STR(mxNextSer))+zCrLf
      REPLACE nextser WITH mxNextSer IN mainsers
    ELSE
      SELE mainsers
      IF SEEK(main_m.rootpart,[mainsers],[part])
        cactions = cactions + [Updated nextser in MAINSERS Part: ]+ALLT(mainsers.PART)+[  ]+ALLT(STR(mxNextSer))+zCrLf
        REPLACE nextser WITH mxNextSer IN mainsers
      ENDIF
    ENDIF
  ELSE  && must be OZTEK
    IF jCode = [OZT001] .AND. ;
      (ALLT(jPart)==[10879-01].OR.ALLT(jPart)==[10879-02];
      .OR.ALLT(jPart)==[30-10879-01].OR.ALLT(jPart)==[30-10879-02])
      xqpart= [10879-01           ]
    ELSE
      xqpart= jorigpart
    ENDIF
    IF mainsers.PART == xqpart
      cactions = cactions + [Updated nextser in MAINSERS Part: ]+ALLT(mainsers.PART)+[  ]+ALLT(STR(mxNextSer))+zCrLf
      REPLACE nextser WITH mxNextSer IN mainsers
    ELSE
      SELE mainsers
      IF SEEK(xqpart,[mainsers],[part])
        cactions = cactions + [Updated nextser in MAINSERS Part: ]+ALLT(mainsers.PART)+[  ]+ALLT(STR(mxNextSer))+zCrLf
        REPLACE nextser WITH mxNextSer IN mainsers
      ENDIF
    ENDIF
  ENDIF
  IF FILE("C:\Pete_Dev.Vld") .OR. BETW(DATE(),{04/07/2016},{04/12/2016})
    *WAIT cactions WIND
    *_CLIPTEXT = cactions
  ENDIF
&& should now scan all for curr lot to get actual jstart/jstop values
  SELE qgen
  REPL qgen.nserstart WITH jStart, qgen.nserend WITH jStop IN qgen && actlabels67
  IF EMPT(ALLT(qgen.serhist))
    REPL qgen.serhist WITH [VFP gensers.prg ]+DTOC(DATE())+[ ]+LEFT(TIME(),5)+[  ADDED  actual ];
      +ALLT(STR(actlabels67))+[  requested ]+ALLT(STR(jQty))+[  new serials ]+ALLT(STR(jStart))+[ thru ]+ALLT(STR(jStop))+[ ]+ctext17
  ELSE
    REPL qgen.serhist WITH ALLT(qgen.serhist) + zCrLf ;
      +[VFP gensers.prg ]+DTOC(DATE())+[ ]+LEFT(TIME(),5)+[  ADDED  actual ];
      +ALLT(STR(actlabels67))+[  requested ]+ALLT(STR(jQty))+[ new serials ]+ALLT(STR(jStart))+[ thru ]+ALLT(STR(jStop))+[ ]+ctext17
  ENDIF
*!*	  IF BETW(DATE(),{08/01/2014},{04/30/2017}) &&.and.[GEO004] $ jcode
*!*	    **PARA jPart,jRev,jJob,jQty,jCode,joverride,jdate,jkitdate,jcomms,juser,jsilent,jvariant

*!*	    jBody = DTOC(DATE())+[ ]+TIME()+[  LOT ]+qgen.job+[ AUTO GEN  ];
*!*	      +ALLT(STR(jQty))+[  SERIALS  ]+zCrLf;
*!*	      +cfull_start+[  thru  ]+cfull_stop+zCrLf;
*!*	      +[Emailing Pete until 04/30/2017]+zCrLf;
*!*	      +[Code ]+jCode+[   Ser # Count ]+ALLT(STR(actlabels67))+zCrLf;
*!*	      +[Part ]+jPart+IIF(lforce_cs,[  faked for ]+jorigpart,[])+zCrLf;
*!*	      +[User Details ]+ALLT(juser)+[ ]+ALLT(jcomms)+zCrLf;
*!*	      +[Top_Part ]+zjtop_part+zCrLf+[ADDED  actual ];
*!*	      +ALLT(STR(actlabels67))+[   requested  ]+ALLT(STR(jQty))+[  new serials  ]+ALLT(STR(jStart))+[ thru ]+ALLT(STR(jStop))+[  ]+ctext17+zCrLf;
*!*	      +[Serials ]+zCrLf+cxserlist82
*!*	    DO send_email WITH [pete@tangoware.com],[peter@spinnakercontract.com];
*!*	      ,[peter@spinnakercontract.com];
*!*	      ,[LOT]+qgen.job+[  ]+ALLT(STR(jQty))+[  AUTOSERIALS ]+zjtop_part,jBody,.F.
*!*	    *DO sndemail WITH [patrick@spinnakercontract.com];
*!*	    * ,[peter@spinnakercontract.com];
*!*	    * ,[peter@spinnakercontract.com];
*!*	    *,[LOT]+qgen.job+[ ]+ALLT(STR(jQty))+[ AUTOSERIALS],jBody,.F.
*!*	  ENDIF
  SELE serlog
  SET ORDER TO part_nser IN serlog
  SET KEY TO
ENDIF
IF USED('main_m22')
  USE IN main_m22
ENDIF
WAIT CLEAR
IF FILE("C:\Pete_Dev.VLD")
  _cliptext = jstatus
ENDIF
IF jsilent
ELSE
  IF actlabels67 > 0
    _gserback = ALLT(STR(actlabels67))+[  Serials  generated - new serials  ]+ALLT(STR(jStart))+[ thru ]+ALLT(STR(jStop))+[ . ]
    RETURN
  ELSE
    _gserback = [NO Serials  generated  - this may be because they already exist . ]
    RETURN
  ENDIF
ENDIF

*=Close_Dbf("serlog")
*=Close_Dbf("main_m")

FUNCTION Cs3DigModl &&=============================
  LPARA jPart
  c3str = [###]
  IF LEFT(jPart,2)==[CS]  && Ex CS118-115-100-P
    dashcnt = OCCURS([-],jPart)
    okstr = [0123456789]
    d1 = AT([-],jPart,1)
    d2 = AT([-],jPart,2)
    IF dashcnt > 1 .AND. d1=5 .AND. d2=9  && verify 3 numerics after 2nd dash
      n1 = 0
      c1 = SUBS(jPart, d2 + 1 , 1)
      c2 = SUBS(jPart, d2 + 2 , 1)
      c3 = SUBS(jPart, d2 + 3 , 1)
      IF c1 $ okstr .AND. c2 $ okstr .AND. c3 $ okstr
        c3str = c1+c2+c3
      ENDIF
    ENDIF
  ENDIF
  RETURN(c3str)

FUNCTION xDropTees &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  LPARAM zpart23
  PRIV zpart23,zret23
  zret23 = zpart23
  t2 =RIGHT(ALLT(zpart23),2)
  t3 =RIGHT(ALLT(zpart23),3)
  t4 =RIGHT(ALLT(zpart23),4)
  t5 =RIGHT(ALLT(zpart23),5)
  t6 =RIGHT(ALLT(zpart23),6)
  t7 =RIGHT(ALLT(zpart23),7)
  t8 =RIGHT(ALLT(zpart23),8)
  t9 =RIGHT(ALLT(zpart23),9)
  t10=RIGHT(ALLT(zpart23),10)
  t11=RIGHT(ALLT(zpart23),11)
  t12=RIGHT(ALLT(zpart23),12)
  DO CASE
    CASE t2 == [-C]
      zret23 = STRTR(zpart23,[-C],[])
    CASE t2 == [-T]
      zret23 = STRTR(zpart23,[-T],[])
    CASE t2 == [-S]
      zret23 = STRTR(zpart23,[-S],[])
    CASE t2 == [-R]
      zret23 = STRTR(zpart23,[-R],[])
    CASE t3 == [-C1]
      zret23 = STRTR(zpart23,[-C1],[])
    CASE t3 == [-C2]
      zret23 = STRTR(zpart23,[-C2],[])
    CASE t3 == [-R2]
      zret23 = STRTR(zpart23,[-R2],[])
    CASE t3 == [-R3]
      zret23 = STRTR(zpart23,[-R3],[])
    CASE t3 == [-RR]
      zret23 = STRTR(zpart23,[-RR],[])
    CASE t3 == [-R1]
      zret23 = STRTR(zpart23,[-R1],[])
    CASE t3 == [-TT]
      zret23 = STRTR(zpart23,[-TT],[])
    CASE t3 == [-T1]
      zret23 = STRTR(zpart23,[-T1],[])
    CASE t3 == [-T2]
      zret23 = STRTR(zpart23,[-T2],[])
    CASE t3 == [-T3]
      zret23 = STRTR(zpart23,[-T3],[])
    CASE t3 == [-T4]
      zret23 = STRTR(zpart23,[-T4],[])
    CASE t3 == [-ND]
      zret23 = STRTR(zpart23,[-ND],[])
    CASE t3 == [-CC]
      zret23 = STRTR(zpart23,[-CC],[])
    CASE t4 == [-CCC]
      zret23 = STRTR(zpart23,[-CCC],[])
    CASE t4 == [-CAL]
      zret23 = STRTR(zpart23,[-CAL],[])
    CASE t4 == [-RWK]
      zret23 = STRTR(zpart23,[-RWK],[])
    CASE t4 == [-LBL]
      zret23 = STRTR(zpart23,[-LBL],[])
    CASE t4 == [-MOD]
      zret23 = STRTR(zpart23,[-MOD],[])
    CASE t4 == [/MOD]
      zret23 = STRTR(zpart23,[/MOD],[])
    CASE t4 == [-PCB]
      zret23 = STRTR(zpart23,[-PCB],[])
    CASE t4 == [-TTT]
      zret23 = STRTR(zpart23,[-TTT],[])
    CASE t4 == [-RMA]
      zret23 = STRTR(zpart23,[-RMA],[])
    CASE t4 == [-ENG]
      zret23 = STRTR(zpart23,[-ENG],[])
    CASE t4 == [-KIT]
      zret23 = STRTR(zpart23,[-KIT],[])
    CASE t4 == [-BUD]
      zret23 = STRTR(zpart23,[-BUD],[])
    CASE t4 == [-CSI]
      zret23 = STRTR(zpart23,[-CSI],[])
    CASE t4 == [-SMT]
      zret23 = STRTR(zpart23,[-SMT],[])
    CASE t4 == [-SUB]
      zret23 = STRTR(zpart23,[-SUB],[])
    CASE t4 == [/SUB]
      zret23 = STRTR(zpart23,[/SUB],[])
    CASE t5 == [-TEST]
      zret23 = STRTR(zpart23,[-TEST],[])
    CASE t5 == [-PROG]
      zret23 = STRTR(zpart23,[-PROG],[])
    CASE t5 == [-PROD]
      zret23 = STRTR(zpart23,[-PROD],[])
    CASE t5 == [-RWK1]
      zret23 = STRTR(zpart23,[-RWK1],[])
    CASE t5 == [-RWK3]
      zret23 = STRTR(zpart23,[-RWK2],[])
    CASE t5 == [-RWK3]
      zret23 = STRTR(zpart23,[-RWK3],[])
    CASE t5 == [-RWK4]
      zret23 = STRTR(zpart23,[-RWK4],[])
    CASE t5 == [-RWK5]
      zret23 = STRTR(zpart23,[-RWK5],[])
    CASE t5 == [-RWK6]
      zret23 = STRTR(zpart23,[-RWK6],[])
    CASE t5 == [-RWK7]
      zret23 = STRTR(zpart23,[-RWK7],[])
    CASE t5 == [-RWK8]
      zret23 = STRTR(zpart23,[-RWK8],[])
    CASE t5 == [-RWK9]
      zret23 = STRTR(zpart23,[-RWK9],[])
    CASE t6 == [-RWK10]
      zret23 = STRTR(zpart23,[-RWK10],[])
    CASE t5 == [-PROG]
      zret23 = STRTR(zpart23,[-PROG],[])
    CASE t5 == [-MATL]
      zret23 = STRTR(zpart23,[-MATL],[])
    CASE t5 == [-ROHS]
      zret23 = STRTR(zpart23,[-ROHS],[])
    CASE t5 == [-CONV]
      zret23 = STRTR(zpart23,[-CONV],[])
    CASE t5 == [-EVAL]
      zret23 = STRTR(zpart23,[-EVAL],[])
    CASE t5 == [-CANC]
      zret23 = STRTR(zpart23,[-CANC],[])
    CASE t5 == [-STEN]
      zret23 = STRTR(zpart23,[-STEN],[])
    CASE t5 == [-COST]
      zret23 = STRTR(zpart23,[-COST],[])
    CASE t6 == [-CABLE]
      zret23 = STRTR(zpart23,[-CABLE],[])
    CASE t6 == [-CONV1]
      zret23 = STRTR(zpart23,[-CONV1],[])
    CASE t6 == [-CONV2]
      zret23 = STRTR(zpart23,[-CONV2],[])
    CASE t6 == [-MAT'L]
      zret23 = STRTR(zpart23,[-MAT'L],[])
    CASE t6 == [-ALIGN]
      zret23 = STRTR(zpart23,[-ALIGN],[])
    CASE t6 == [-LABEL]
      zret23 = STRTR(zpart23,[-LABEL],[])
    CASE t6 == [-PARTS]
      zret23 = STRTR(zpart23,[-PARTS],[])
    CASE t6 == [-LABOR]
      zret23 = STRTR(zpart23,[-LABOR],[])
    CASE t6 == [-SCRAP]
      zret23 = STRTR(zpart23,[-SCRAP],[])
    CASE t7 == [-LABELS]
      zret23 = STRTR(zpart23,[-LABELS],[])
    CASE t7 == [-CANCEL]
      zret23 = STRTR(zpart23,[-CANCEL],[])
    CASE t6 == [REWORK]
      zret23 = STRTR(zpart23,[REWORK],[])
    CASE t7 == [-REWORK]
      zret23 = STRTR(zpart23,[-REWORK],[])
    CASE t7 == [ REWORK]
      zret23 = STRTR(zpart23,[ REWORK],[])
    CASE t7 == [-BUDGET]
      zret23 = STRTR(zpart23,[-BUDGET],[])
    CASE t7 == [-EXCESS]
      zret23 = STRTR(zpart23,[-EXCESS],[])
    CASE t7 == [-REPAIR]
      zret23 = STRTR(zpart23,[-REPAIR],[])
    CASE t7 == [ EXCESS]
      zret23 = STRTR(zpart23,[ EXCESS],[])
    CASE t8 == [-INSPECT]
      zret23 = STRTR(zpart23,[-INSPECT],[])
    CASE t8 == [-SPECIAL]
      zret23 = STRTR(zpart23,[-SPECIAL],[])
    CASE t8 == [-MACHINE]
      zret23 = STRTR(zpart23,[-MACHINE],[])
    CASE t8 == [-CONSIGN]
      zret23 = STRTR(zpart23,[-CONSIGN],[])
    CASE t8 == [-HARNESS]
      zret23 = STRTR(zpart23,[-HARNESS],[])
    CASE t8 == [-REWORK2]
      zret23 = STRTR(zpart23,[-REWORK2],[])
    CASE t8 == [-STENCIL]
      zret23 = STRTR(zpart23,[-STENCIL],[])
    CASE t8 == [-FIXTURE]
      zret23 = STRTR(zpart23,[-FIXTURE],[])
    CASE t8 == [-TOOLING]
      zret23 = STRTR(zpart23,[-TOOLING],[])
    CASE t8 == [/TOOLING]
      zret23 = STRTR(zpart23,[/TOOLING],[])
    CASE t9 == [-FIXTURES]
      zret23 = STRTR(zpart23,[-FIXTURES],[])
    CASE t10 == [-TEST FIXT]
      zret23 = STRTR(zpart23,[-TEST FIXT],[])
    CASE t10 == [ TEST FIXT]
      zret23 = STRTR(zpart23,[ TEST FIXT],[])
    CASE t9 == [-MODIFIED]
      zret23 = STRTR(zpart23,[-MODIFIED],[])
    CASE t9 == [-EXPOSURE]
      zret23 = STRTR(zpart23,[-EXPOSURE],[])
    CASE t9 == [-PCB ONLY]
      zret23 = STRTR(zpart23,[-PCB ONLY],[])
    CASE t9 == [-BREAKOUT]
      zret23 = STRTR(zpart23,[-BREAKOUT],[])
    CASE t10 == [-MISC MATL]
      zret23 = STRTR(zpart23,[-MISC MATL],[])
    CASE t10 == [-COST DIFF]
      zret23 = STRTR(zpart23,[-COST DIFF],[])
    CASE t11 == [-MISC MAT'L]
      zret23 = STRTR(zpart23,[-MISC MAT'L],[])
    CASE t12 == [-CANCELATION]
      zret23 = STRTR(zpart23,[-CANCELATION],[])
  ENDCASE
  zret23 = PADR(ALLT(zret23),19)
  RETURN(zret23)


FUNCTION MoProd &&&==================================================
  **moy172 = SUBS(DTOS(DATE()),5,2) && 20140430   ref: CSI QMS-C-007
  moy172 = SUBS(DTOS(jkitdate),5,2) && 20140430   ref: CSI QMS-C-007
  mop7 = [$]
  DO CASE
    CASE moy172 = [01]
      mop7 = [A]
    CASE moy172 = [02]
      mop7 = [B]
    CASE moy172 = [03]
      mop7 = [C]
    CASE moy172 = [04]
      mop7 = [D]
    CASE moy172 = [05]
      mop7 = [E]
    CASE moy172 = [06]
      mop7 = [F]
    CASE moy172 = [07]
      mop7 = [G]
    CASE moy172 = [08]
      mop7 = [H]
    CASE moy172 = [09]
      mop7 = [J]
    CASE moy172 = [10]
      mop7 = [K]
    CASE moy172 = [11]
      mop7 = [L]
    CASE moy172 = [12]
      mop7 = [M]
  ENDCASE
  RETURN(mop7)

FUNCTION getyear7272 &&& =============================================
  **cy = LEFT(DTOS(DATE()),4)
  cy = LEFT(DTOS(jkitdate),4)
  DO CASE
    CASE cy=[2013]
      y172 = [B]
    CASE cy=[2014]
      y172 = [C]
    CASE cy=[2015]
      y172 = [D]
    CASE cy=[2016]
      y172 = [E]
    CASE cy=[2017]
      y172 = [F]
    CASE cy=[2018]
      y172 = [G]
    CASE cy=[2019]
      y172 = [H]
    CASE cy=[2020]
      y172 = [J]
    CASE cy=[2021]
      y172 = [K]
    CASE cy=[2022]
      y172 = [L]
    CASE cy=[2023]
      y172 = [M]
    CASE cy=[2024]
      y172 = [N]
    CASE cy=[2025]
      y172 = [P]
    CASE cy=[2026]
      y172 = [Q]
    CASE cy=[2027]
      y172 = [R]
    CASE cy=[2028]
      y172 = [S]
    CASE cy=[2029]
      y172 = [T]
    CASE cy=[2030]
      y172 = [U]
    CASE cy=[2031]
      y172 = [V]
    CASE cy=[2032]
      y172 = [W]
  ENDCASE


  *!*	  14Sep16 stopped, see aboveIF [OZT001] $ jcode .and. [10879-0] $ jpart && looking for 10879-01 & 10879-02 to sequence serials across both parts
  *!*	    && 10879-01 & 10879-02   MUST use same serial sequence not separate sequences,  must ignore what is in  main_m
  *!*	    && lot part#'s likely to start with 30-   need to strip
  *!*	    jPart2 = [30-10879-0]
  *!*	    && leave the 30- in serlog to be consistent with existing,  the SPECIAL data form prints these and it uses special LABEl PART# there
  *!*	    && now jpart should be either 10879-01 or 10879-02
  *!*	    && per Oct27'14 John Moisan - this sequencing should start at 7712
  *!*	    SELE serlog
  *!*	    SET ORDER TO part_nser IN serlog  && looking thru previous serials
  *!*	    SET KEY TO jpart2
  *!*	    SEEK jPart2
  *!*	    SCAN
  *!*	      IF serlog.sernum > 0
  *!*	        nextoztek = IIF(serlog.sernum > nextoztek, serlog.sernum ,nextoztek)
  *!*	      ENDIF
  *!*	    ENDSCAN
  *!*	    **WAIT jpart2+[  nextoztek  ]+STR(nextoztek) WIND
  *!*	    SELE serlog
  *!*	    SET ORDER TO part_nser ASCE IN serlog  &&  < important to set back to ASCE
  *!*	    SET KEY TO &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  *!*	    IF nextoztek < 7712  && get started @ 7712 per Oct27'14 John Moisan - this sequencing should start at 7712
  *!*	      mxNextSer = 7712
  *!*	      jstart    = 7712
  *!*	      jstop     = 7712 + jqty - 1
  *!*	    ELSE
  *!*	      mxNextSer = nextoztek + 1
  *!*	      jstart    = nextoztek + 1
  *!*	      jstop     = nextoztek + jqty - 1
  *!*	    ENDIF
  *!*	  ENDIF


*// find highest serial by part# and update mainsers.nextser
*// if OZT001 10879-01,10879-02,30-1879-01,30-10879-02  test for highest of ALL 4
*// use routine in Test data Renumber  for code
*!*	_zHighSer=0
*!*	IF jCode = [OZT001] .AND. ;
*!*	    (ALLT(jPart)==[10879-01].OR.ALLT(jPart)==[10879-02];
*!*	    .OR.ALLT(jPart)==[30-10879-01].OR.ALLT(jPart)==[30-10879-02])
*!*	  STORE 0 TO n1,n2,n3,n4,n5,n6,n7,n8
*!*	  SELE serlog
*!*	  SET ORDE TO part_nser IN serlog
*!*	  SET KEY TO [10879-01           ]
*!*	  CALC MAX(serlog.sernum) TO n1
*!*	  SELE serlog
*!*	  SET ORDE TO part_nser IN serlog
*!*	  SET KEY TO [10879-02           ]
*!*	  CALC MAX(serlog.sernum) TO n2
*!*	  SELE serlog
*!*	  SET ORDE TO part_nser IN serlog
*!*	  SET KEY TO [30-10879-01        ]
*!*	  CALC MAX(serlog.sernum) TO n3
*!*	  SELE serlog
*!*	  SET ORDE TO part_nser IN serlog
*!*	  SET KEY TO [30-10879-02        ]
*!*	  CALC MAX(serlog.sernum) TO n4
*!*	  _zHighSer=MAX(n1,n2,n3,n4) + 1
*!*	  IF SEEK([10879-01           ],[mainsers],[part]) .and. mainsers.nextser < _zHighSer
*!*	    IF FILE("C:\Pete_Dev.VLD")
*!*	      WAIT jpart +[Oztek 4 group   Want to set to  ]+n_s(_zHighSer)+[  now  ]+n_s(mainsers.nextser ) WIND
*!*	    ENDIF
*!*	    *REPL nextser WITH _zHighSer IN manisers
*!*	  ENDIF
*!*	ELSE
*!*	  IF jCode = [OZT001]
*!*	    SELE serlog
*!*	    SET ORDE TO part_nser IN serlog
*!*	    SET KEY TO main_m.PART
*!*	    CALC MAX(serlog.sernum) TO _zHighSer
*!*	    _zHighSer = _zHighSer + 1
*!*	    IF SEEK(jpart,[mainsers],[part]) .and. mainsers.nextser < _zHighSer
*!*	      IF FILE("C:\Pete_Dev.VLD")
*!*	        WAIT jpart +[Oztek general part   Want to set to  ]+n_s(_zHighSer)+[  now  ]+n_s(mainsers.nextser ) WIND
*!*	      ENDIF
*!*	    *REPL nextser WITH _zHighSer IN manisers
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF


    *cs10Nextser = mxNextSer

