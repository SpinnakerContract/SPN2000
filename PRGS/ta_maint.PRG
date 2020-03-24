LPARAM PeDat,xEmnem,xdailyot,xweeklyot,xl78,xl54,xdaysper,xAuthorizing,zLsvpcOT,xAllowOT,xLweekOt,xLDayOt
DO CASE
  CASE [B] $ config2.tt_rndahrs
    zRound = 1
  CASE [C] $ config2.tt_rndahrs
    zRound = 5
  OTHERWISE
    zRound = 0
ENDCASE
zRound = 0
xdailyot  = IIF(xdailyot =0, 24, xdailyot)
xweeklyot = IIF(xweeklyot=0, 112, xweeklyot) &&22Nov05 changed from 40 to 112(7x16)  &&   STATE LAW ?
xxtarget  = xEmnem+DTOS(PeDat)               && was causing OT to be given with configure OT off
lDayOt    = xLweekOt && 22Nov05 xDailyOT  > 0
lWeekOt   = xLDayOt  && 22Nov05 xWeeklyOT > 0
RELE ALL LIKE Dat_Arr  , Reg_Arr, OT_Arr, Gt_Arr
DIME Dat_Arr(xdaysper) , Reg_Arr(xdaysper), Gt_Arr(xdaysper), OT_Arr(xdaysper)
STOR {}   TO Dat_Arr, Last_Date
STOR [00:00:00] TO Last_Time
STOR  0   TO Gt_Arr, Reg_Arr, OT_Arr, PayRec_Cnt, zHolCount, Last_Index
STOR []   TO PaidHDays
STOR .T.  TO zLEOD
FOR jjj=1 TO xdaysper
  STORE PeDat-xdaysper + jjj TO Dat_Arr(jjj)
ENDFOR
SELE empclock
xRecno = RECNO()
STORE .F. TO xHit, zAnomolies, lAuth, zOverLaps
SET ORDER TO emp_dat ASCE
SEEK xxtarget
SCAN WHILE empclock.Emnem+DTOS(empclock.per_end) = xxtarget
  SELE empclock
  xHit       = IIF(xRecno = RECNO(), .T., xHit)
  zHolCount  = zHolCount  + IIF(category=[Holiday],1,0)
  PayRec_Cnt = PayRec_Cnt + 1
  IF EMPT(authorized)   &&--- blank out the data if un-authorized
        _date_o = IIF(empclock.date_o={}.and.empclock.date_i=DATE(),empclock.date_i,empclock.date_o)
        _time_o = IIF(EMPT(empclock.time_o).and.empclock.date_i=DATE(),TIME(),empclock.time_o)
    REPLA leod WITH .F., feod WITH .F., gross_hrs WITH 0, reg_hrs WITH 0, ot_hrs WITH 0, unpaid_hrs WITH 0 IN empclock
    REPLA day_total WITH 0, nindex WITH PayRec_Cnt, nOverLap WITH 0 IN empclock  &&group WITH 0, grp_item WITH 0,
    brkNote = []
    znote = []
    IF lpayable &&// GROSS is labeled as entry TOTAL
      IF 1=1 &&TYPE("psysserial")="C".and.INLI(psysserial,[4000],[8888])
        IF !EMPT(_date_o).and.!EMPT(_Time_o)
          qtime_o = IIF(empclock.laddbreak,Tweak_Time(_time_o,10),_time_o)
          REPL gross_hrs WITH ;
            IIF( nflathrs > 0, nflathrs, TimeUdf78(LEFT(time_i,5),LEFT(qtime_o,5)) ) IN empclock
        ENDIF
      ELSE
*!*	        REPL gross_hrs WITH ;
*!*	          IIF( nflathrs > 0, nflathrs, TimeUdf1(time_i,time_o,date_i,date_o,zRound) ) IN empclock
      ENDIF
      IF nflathrs = 0 .and. Last_date#{} .and. Last_Index>0 .and. date_i # {} .and. date_i = last_date ;
        .and. time_i < Last_Time
        REPL nOverLap WITH Last_Index IN empclock
      ENDIF
    ELSE
      jHrsFromTime = TimeUdf1(time_i,_time_o,date_i,_date_o,zRound)
      REPL unpaid_hrs WITH IIF( nflathrs > 0, nflathrs, jHrsFromTime ) IN empclock
    ENDIF
    IF empclock.laddbreak
      brknote = brknote + [10mBrk,]
    ENDIF
    STORE _time_o TO Last_Time
    STORE _date_o TO Last_Date
    STORE nindex TO Last_Index
    IF 1=1 &&TYPE("psysserial")="C".and.INLI(psysserial,[4000],[8888])
      IF empclock.gross_hrs>0.and.INLI(DOW(_date_o),1,2,3,4,5,6).and. empclock.nflathrs=0 && not SVPCH
        znote = [ ]+IIF(empclock.lunch_hrs > 0,ALLT(STR(gross_hrs,14,2))+[-]+ALLT(STR(empclock.lunch_hrs,5,2))+[ Lunch],[])
        REPL note WITH brknote+znote, gross_hrs WITH MAX(0,gross_hrs-MAX(0,lunch_hrs)) IN empclock
      ELSE
        REPL note WITH brknote IN empclock
      ENDIF
*!*	      znote = [ ]+ALLT(STR(gross_hrs,14,2))+[-0.5 Lunch]
*!*	      REPL note WITH brknote IN empclock
*!*	      IF time_o > [12:00:00].and.!empclock.lnolunch.and.gross_hrs>0.and.INLI(DOW(date_o),2,3,4,5,6).and. nflathrs=0 && not SVPCH
*!*	        REPL note WITH brknote + znote, gross_hrs WITH MAX(0,gross_hrs-.5) IN empclock
*!*	      ENDIF
    ENDIF
*!*	    IF [SALARIED H] $ UPPE(empclock.category)
*!*	      REPL gross_hrs WITH 8.0 IN empclock
*!*	    ENDIF
  ELSE
    lAuth = .T.
  ENDIF
ENDSCAN

IF lAuth
  RETURN(.F.) &&---------
ENDIF

IF 1=2 &&FILE("C:\ThisFileOnlyOn.Cti")
  WAIT WIND NOWAIT [CTI  ONLY    TA-MAINT   RE-CALC]
ENDIF
*!*	IF zAnomolies .and. xAuthorizing
*!*	  WAIT [Anom  ]+IIF(zAnomolies ,[Yes],[No])+[       Auth  ]+IIF(xAuthorizing,[Yes],[No]) WIND
*!*	  RETURN(.T.)
*!*	ENDIF
STORE {}  TO sd  &&-- startdate mvar
STORE []  TO st  &&-- starttime mvar
STORE  0  TO ttlcnt,gri, jPeCnt,nRunWeekGross
STORE  1  TO grp
STORE .F. TO WeeklyOtIsTripped
FOR xyz = 1 TO xdaysper  && loops thru each day in period
  STORE .F. TO zFodfound
  STORE  0  TO zFullDayCnt, nTotalFlat
  zTargDate = Dat_Arr(xyz)
  SELE empclock
  SET ORDER TO emp_dat DESC
  SEEK xxtarget
  SCAN WHILE empclock.Emnem+DTOS(empclock.per_end) = xxtarget
        _date_o = IIF(empclock.date_o={}.and.empclock.date_i=DATE(),empclock.date_i,empclock.date_o)
        _time_o = IIF(EMPT(empclock.time_o).and.empclock.date_i=DATE(),TIME(),empclock.time_o)
    IF _date_o={} .or. _date_o # zTargDate
      LOOP && skip if not part of this day
    ENDIF
    zFullDayCnt = zFullDayCnt + 1
  ENDSCAN

  SELE empclock
  SET ORDER TO emp_dat ASCE
  SEEK xxtarget
  STORE 0 TO zCurrDayCnt, nRunReg, nRunOT, nRunDayGross, zActGross, nRunDayTotal  
  STORE {} TO zDate_prev
  STORE "00:00:00" TO zTime_prev
  DailyOtIsTripped = .F.
  SCAN WHILE empclock.Emnem+DTOS(empclock.per_end) = xxtarget  &&&   O N E      D A Y S    W O R T H  
        _date_o = IIF(empclock.date_o={},empclock.date_i,empclock.date_o)
        _time_o = IIF(EMPT(empclock.time_o),TIME(),empclock.time_o)
    IF _date_o={} .or. _date_o # zTargDate
      LOOP && skip if not part of this day
    ENDIF
    zCurrDayCnt = zCurrDayCnt + 1
    IsSVPCH = INLI(ALLT(category),"Sick Time","Vacation","Personal Time","Comp Time","Holiday")

    *// don't build up trip points if not allowing SVPCH items to contribute to OT
    IF IsSVPCH .and. !zLsvpcOT
    ELSE  && contib to trip point
      nRunDayGross  = MAX( 0 , nRunDayGross   + Gross_hrs  )
      nRunWeekGross = MAX( 0 , nRunWeekGross  + Gross_hrs  )
    ENDIF

    nRunDayTotal  = MAX( 0 , nRunDayTotal + Gross_hrs  )

    DO CASE  && entry by entry  NOT at end of day
    
      CASE [SALARIED H] $ UPPE(empclock.category)
        ** do nothing  SALARIED
        
      CASE xAllowOT .and. lWeekOT .and. nRunWeekGross > xWeeklyOT .and. WeeklyOtIsTripped &&  Weekly OT  TRIPPED TAKES PRECEDENCE

        REPL reg_hrs WITH 0 IN empclock
        nRunReg    = nRunReg  + reg_hrs

        REPL ot_hrs  WITH Gross_hrs IN empclock  
        nRunOT     = nRunOT   + ot_hrs    

      CASE xAllowOT .and. lWeekOT .and. nRunWeekGross > xWeeklyOT .and. !WeeklyOtIsTripped
        WeeklyOtIsTripped = .T.
        OverWeekly = nRunWeekGross - xWeeklyOT

        REPL reg_hrs WITH Gross_hrs - OverWeekly IN empclock  &&gross_hrs  - Ot2Carry 
        nRunReg    = nRunReg  + reg_hrs

        REPL ot_hrs  WITH OverWeekly IN empclock  
        nRunOT     = nRunOT   + ot_hrs    

      CASE xAllowOT .and. lDayOT .and. nRunDayGross > xDailyOT .and. DailyOtIsTripped &&  OT  TRIPPED

        REPL reg_hrs WITH 0 IN empclock
        nRunReg    = nRunReg  + reg_hrs

        REPL ot_hrs  WITH Gross_hrs IN empclock  
        nRunOT     = nRunOT   + ot_hrs    

      CASE xAllowOT .and. lDayOT .and. nRunDayGross > xDailyOT .and. !DailyOtIsTripped && This is a daily OT trigger and the running gross exceeds it
        DailyOtIsTripped = .T.
        OverDaily  = nRunDayGross - xDailyOT

        REPL reg_hrs WITH Gross_hrs - OverDaily IN empclock  &&gross_hrs  - Ot2Carry 
        nRunReg    = nRunReg  + reg_hrs

        REPL ot_hrs  WITH OverDaily IN empclock
        nRunOT     = nRunOT   + ot_hrs    

      OTHERWISE
        REPL reg_hrs WITH gross_hrs IN empclock
        nRunReg    = nRunReg  + reg_hrs

    ENDCASE

    *// only executes  on  L A S T   entry of day
    IF zCurrDayCnt = zFullDayCnt   &&.and. date_o = empclock.date_o
      *WAIT DTOC(ThisForm._zPerEnd)+[    TTL ]+STR(zFullDayCnt,2)+[  CURR   ]+STR(zCurrDayCnt,5) + [   ]+DTOC(date_i) WIND
      REPL leod      WITH .T.  IN empclock
      REPL day_total WITH nRunDayTotal  IN empclock &&nRunReg + nRunOT
    ENDIF

  ENDSCAN
ENDFOR
SELE empclock
=TABLEUPDATE(.T.,.T.,"empclock")
SET ORDER TO emp_dat
IF xRecno > 0 .and. xHit
  GOTO xRecno
ENDIF


RETURN





*!*	****  P R E P R O C E S S O R  3 3 3 3 3 ********
*!*	*****************************************
*!*	SEEK xxtarget
*!*	zGroup=1
*!*	STORE {} TO zdin,zdl
*!*	STORE [] TO ztin,ztl
*!*	SCAN WHILE empclock.Emnem+DTOS(empclock.per_end) = xxtarget
*!*	  IF zAnomolies.or.!EMPT(authorized) &&--- already auth
*!*	    LOOP
*!*	  ENDIF
*!*	  IF date_o={}.or.time_o=[  :  :  ]
*!*	    REPL leod WITH .F., feod WITH .F.
*!*	    LOOP
*!*	  ENDIF
*!*	  IF 111=222  &&RECNO()=308337
*!*	    CHAN TITLE [3rd PreProcessor  1/2]
*!*	  ENDIF
*!*	  IF feod
*!*	    zdin = date_i
*!*	    ztin = time_i
*!*	    zdl  = date_o
*!*	    ztl  = time_o
*!*	  ENDIF
*!*	  IF !feod.and.!leod
*!*	    zlmin = ROUN( 60 * (TimeUdf1(ztl,time_i,zdl,date_i,[*])) , 0 )
*!*	    zdl  = date_o
*!*	    ztl  = time_o
*!*	  ENDIF
*!*	  IF leod
*!*	    IF !feod
*!*	      zlmin = ROUN( 60 * (TimeUdf1(ztl,time_i,zdl,date_i,[*])) , 0 )
*!*	    ENDIF
*!*	    REPL grp_date_i WITH IIF(zdin={}   ,date_i,zdin)
*!*	    REPL grp_time_i WITH IIF(EMPT(ztin),time_i,ztin)
*!*	    REPL grp_date_o WITH date_o,grp_time_o WITH time_o
*!*	    STOR {} TO zdin,zdl
*!*	    STOR [] TO ztin,ztl
*!*	    zLunch = 0
*!*	  ENDIF
*!*	ENDSCAN

*!*	*!*	FOR ttt=7 TO 1 STEP -1
*!*	*!*	  PaidHDays = PaidHDays+IIF(SEEK(DTOC(PeDat+(7-ttt)-6,1),'holidays'),','+;
*!*	*!*	    LEFT(UPPE(CDOW(PeDat+(7-ttt)-6)),3)+[ ]+DTOC(PeDat+(7-ttt)-6),'')
*!*	*!*	ENDFOR
*!*	*!*	NumPHolidays=(LEN(ALLT(PaidHdays))/15) && was 13 now 15 becuz YYYY
*!*	OtTripHrs = xweeklyot - 0  &&IIF(avempl_m.full_part#[F],0,(NumPHolidays*8))
*!*	*OtTripHrs = IIF(INLI(avempl_m.e_cat,'SS','SP'),40,OtTripHrs)
*!*	*// Can't set trip hours based solely on a Holiday being declared
*!*	*// Only count if actual Hol entries found
*!*	**WAIT STR(ottriphrs,2)+[   HolCount ]+STR(zHolCount,1)+[   Before] WIND TIME 2
*!*	IF zHolCount = 0 &&// counted above in pre-processor
*!*	  OtTripHrs = xweeklyot &&// force back to 40 if no holidays entered
*!*	ENDIF
*!*	**WAIT STR(ottriphrs,2)+[  HolCount ]+STR(zHolCount,1)+[   After] WIND TIME 2
*!*	*!*	@ 12+srow,23+scol SAY PADR(IIF(NumPHolidays>0,n_s(NumPHolidays)+;
*!*	*!*	  [ PAID HOLIDAYS ]+PaidHdays,[]),48) COLOR G+/BG

*!*	STORE 0   TO WeekOt_Trip,g_time,r_time,o_time,tg_time,CntLeod
*!*	STORE .F. TO ShiftWeek
*!*	*!*	@ 18+srow,0
*!*	*!*	PrimColor = IIF(!EMPT(empclock.authorized),'N/BG','R/BG')
*!*	*!*	SET COLOR TO (PrimColor)
*!*	********************** MAIN LOOP **************
*!*	SEEK xxtarget
*!*	SCAN WHILE empclock.Emnem+DTOS(empclock.per_end) = xxtarget

*!*	  IF !EMPT(date_o) &&-------- process only if logged OUT
*!*	    IF zAnomolies
*!*	      *?? UPPE(LEFT(CDOW(date_o),2))+[ ]+LEFT(DTOC(date_o),5)+[ ]+time_o
*!*	      LOOP
*!*	    ENDIF
*!*	    STORE 0 TO tg_time,to_time
*!*	    pay_str=REPLI([0],18)  &&STR(0,6)+STR(0,6)+STR(0,6)
*!*	    zLEOD    = empclock.leod
*!*	    IF zLEOD .or.INLI(class,'C','H','V')
*!*	      tg_time = TimeUdf1(grp_time_i,grp_time_o,grp_date_i,grp_date_o,[*]) && gross
*!*	      pay_str = TimeUdf2(grp_time_i,grp_time_o,class,tg_time,0) && pay values as str
*!*	    ENDIF
*!*	    *WAIT pay_str WIND TIME 2
*!*	    tr_time  = VAL(LEFT(pay_str,6))
*!*	    tc_time  = 0  &&VAL(SUBS(pay_str,7,6))
*!*	    **tr_time  = IIF(tc_time>0,tc_time,tr_time) && forces call into reg
*!*	    **WAIT STR(tg_time,6,2)+STR(tr_time,6,2) WIND
*!*	    IF 11=22  &&INLI(avempl_m.e_cat,'SS','SP')
*!*	      WeekOt_Trip = WeekOt_Trip ;
*!*	        + IIF(!INLI(class,'C',[O],[E]),tr_time,0)
*!*	    ELSE
*!*	      WeekOt_Trip = WeekOt_Trip ;
*!*	        + IIF(!INLI(class,'C','H','V',[O],[E]),tr_time,0)
*!*	    ENDIF
*!*	    *WAIT [Weekly ot trip ]+STR(WeekOt_Trip,8,2) WIND
*!*	    STORE 0 TO ShiftW_OT    &&,ShiftD_OT
*!*	    DO CASE
*!*	      CASE WeekOt_Trip>=OtTripHrs.and.!ShiftWeek .and. !INLI(class,'C','H','N','V',[O]) && avempl_m.e_cat=[HR].and.
*!*	        ShiftWeek = .T.
*!*	        ShiftW_OT = (WeekOt_Trip-OtTripHrs)
*!*	        tr_time   = tr_time - ShiftW_OT
*!*	        to_time   = to_time + ShiftW_OT
*!*	      CASE WeekOt_Trip >= OtTripHrs.and.ShiftWeek .and.!INLI(class,'C','H','N','V',[O]) && avempl_m.e_cat=[HR].and.
*!*	        ShiftWeek = .T.
*!*	        ShiftW_OT = tr_time
*!*	        to_time   = to_time + tr_time
*!*	        tr_time   = 0
*!*	    ENDCASE
*!*	    WeekOt_Trip = WeekOt_Trip - ShiftW_OT
*!*	    SELE empclock
*!*	    *!*	    IF empclock.class=[O] && personal time
*!*	    *!*	      STORE 0 TO tr_time,to_time
*!*	    *!*	      STORE CEIL(tg_time) TO tg_time
*!*	    *!*	      IF EMPT(empclock.authorized)
*!*	    *!*	        REPLA empclock.per_hrs WITH CEIL(tg_time)
*!*	    *!*	      ENDIF
*!*	    *!*	    ENDIF
*!*	    *IF 1=2 &&'LOAD' $ Load_Say       &&--- insert into fields
*!*	    * IF empclock.class # [O]  &&--- not personal time
*!*	    *   IF reg_hrs = 0
*!*	    REPLACE reg_hrs  WITH tr_time
*!*	    *   ENDIF
*!*	    *   IF ot_hrs = 0
*!*	    REPLACE ot_hrs   WITH to_time
*!*	    *   ENDIF
*!*	    * ENDIF
*!*	  ELSE
*!*	    IF !EMPT(authorized) && ALREADY AUTH.
*!*	      tr_time = empclock.reg_hrs
*!*	      to_time = empclock.ot_hrs
*!*	    ELSE  && not authorized
*!*	      tr_time = IIF(empclock.reg_hrs>0,empclock.reg_hrs,tr_time)
*!*	      to_time = IIF(empclock.ot_hrs>0,empclock.ot_hrs,to_time)
*!*	    ENDIF
*!*	  ENDIF
*!*	  g_time = g_time + IIF(class=[O],0,tg_time)
*!*	  r_time = r_time + tr_time
*!*	  o_time = o_time + to_time
*!*	  *?? loca_o+[-]
*!*	  IF class#'H'.and.(date_o = {} .or. INLI(time_o,'  :  ','     ').or.;
*!*	      tg_time>18.or.date_i>date_o.or.(date_i=date_o.and.time_o<time_i))
*!*	    *?? [INVALID Entry!!]
*!*	  ELSE
*!*	    *?? UPPE(LEFT(CDOW(date_o),2))+[ ]+LEFT(DTOC(date_o),5)+[ ]+time_o
*!*	  ENDIF
*!*	  IF zLEOD
*!*	    CntLeod = CntLeod + 1
*!*	    ?
*!*	  ENDIF
*!*	ENDSCAN
*!*	IF !zAnomolies
*!*	ELSE
*!*	  *  @ 43,00 SAY PADC([Incomplete or erroneous entries  -  please repair !],80)
*!*	ENDIF
*!*	RELE ALL LIKE Dat_Arr, Reg_Arr, OT_Arr, Gt_Arr
*!*	SELE empclock
*!*	RETURN

*DO ta_maint WITH ThisForm._zPerEnd, avempl_m.Emnem, ThisForm._ztt_ndayot ;
*  ,ThisForm._ztt_nweekot , ThisForm._ztt_l78, ThisForm._ztt_l54
* //  TA_MAINT
* WeekOt_Trip  = weekly runtotal of regular hrs, EXCEPT call-ins and HOliday and Vacations
* payrec_cnt= # of entries - total physical
* g_time    = total running gross displayed at bottom summary
* r_time    = total running regular displayed summary
* o_time    = total running overtime displayed summary
* tg_time   = single line calculated gross time
* tr_time   = single line calculated reg time
* to_time   = single line calculated over
* ShiftWeek = flag
* Dat_Arr   = array for 7 dates possibly worked
* Reg_Arr   = array for 7 days reg
* Gt_Arr    = array for 7 days GT
* OT_Arr    = array for 7 days OT


*!*	    IF !INLI(class,'Reg') && reg call partial get pay
*!*	      REPLACE feod WITH .T., leod WITH .T.
*!*	      SKIP - 1
*!*	      IF empclock.Emnem = xEmnem .and. per_end = PeDat
*!*	        REPLACE leod WITH .T.
*!*	      ENDIF
*!*	      SKIP
*!*	      LOOP
*!*	    ENDIF
*!*	    IF date_o={}.or.time_o=[  :  :  ].or.(time_i=[00:00:00].and.time_o=[00:00:00].and.INLI(class,'C',[H],'V',[O]))
*!*	      SKIP - 1
*!*	      IF empclock.Emnem = xEmnem .and. per_end = PeDat
*!*	        REPLACE leod WITH .T.
*!*	      ENDIF
*!*	      SKIP
*!*	      LOOP
*!*	    ENDIF  &&--- proceed - have a valid entry
*!*	    IF ttlcnt = 0
*!*	      REPLACE feod WITH .T.
*!*	      st = time_i  &&-- uncondit. done on very first entry of week only
*!*	      sd = date_i
*!*	    ENDIF
*!*	    ttlcnt   = ttlcnt + 1
*!*	    SpanTime = TimeUdf1(st,time_o,sd,date_o,[*])
*!*	    IF SpanTime > 18
*!*	      sd  = date_i  &&-- new group
*!*	      st  = time_i
*!*	      grp = grp + 1
*!*	      gri = 1
*!*	      REPLACE feod WITH .T.
*!*	      SKIP - 1
*!*	      IF empclock.Emnem = xEmnem .and. per_end = PeDat
*!*	        REPLACE leod WITH .T.
*!*	      ENDIF
*!*	      SKIP
*!*	    ELSE
*!*	      gri = gri + 1
*!*	    ENDIF
*!*	    IF date_o # {}
*!*	      REPLA empclock.group WITH MIN(grp,9),empclock.grp_item WITH MIN(gri,9)
*!*	    ENDIF
*!*	  ENDSCAN
*!*	  SKIP - 1  &&-- mark very last entry as a LEOD
*!*	  IF EMPT(authorized).and.empclock.Emnem = xEmnem .and. per_end = PeDat
*!*	    REPLACE leod WITH .T.
*!*	  ENDIF
