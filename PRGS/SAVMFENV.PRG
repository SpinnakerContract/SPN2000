******************************************************************
*      	ProMatrix Corporation
*
*      	Copyright © 1995-1998 ProMatrix Corporation
*
*      	5225 Ehrlich Road, Suite C
*      	Tampa, FL  33624
*      	U.S.A.
*
*	This computer program is protected by copyright law and
*	international treaties.  No part of this program may be
*	reproduced or transmitted in any form or by any means,
*	electronic or mechanical, for any purpose, without the
*	express written permission of ProMatrix Corporation.
*	Unauthorized reproduction or distribution of this program,
*	or any portion of it, may result in severe civil and
*	criminal penalties, and will be prosecuted to the maximum
*	extent possible under the law.
*
******************************************************************

***************
* S<Prefix>FEnv - Set the environment for this application.  This program
***************   is called from the application's main program.

LOCAL slcTempPath,slnPathLength,slnDir,slnNumDir,slnNumADIR,slnFor,;
  slcApD,slcIcon,slnBS,slcDDir,slcRDir,slnCurArea,slcOnError,sllError,;
  slnNumApp,slnNumSys,slnNumOpened,slcOrigDBC

LOCAL ARRAY slaDir(1),slaADIR(1,5),slaOpen(1,3)

* Get the path to the Windows Temporary File Directory and store it in
* sgcTempPath.
RELEASE sgcTempPath
PUBLIC sgcTempPath
slcTempPath = SPACE(128)
DECLARE LONG GetTempPath IN Kernel32 ;
  LONG nBufferLength, STRING lpBuffer
slnPathLength = GetTempPath(128,@slcTempPath)
IF slnPathLength < 3
  sgcTempPath = ''
ELSE
  sgcTempPath = LEFT(slcTempPath,slnPathLength)
ENDIF

* Set the path for this application.  The path will include the current
* default directory and any subdirectories.
sgcPjHome  = SYS(5)+SYS(2003)					&& Default directory
sgcHomeDir = sgcPjHome
cstartdir  = sgcPjHome
cstartdrv  = LEFT(ALLT(sgcPjHome),2)
slnDir = 1
slnNumDir = 1
DIMENSION slaDir(1)
slaDir(1) = sgcPjHome
DO WHILE slnDir <= slnNumDir
  SET DEFAULT TO (slaDir(slnDir))
  DIMENSION slaADIR(1,5)
  slnNumADIR = ADIR(slaADIR,'','D')
  IF slnNumADIR > 0
    FOR slnFor = 1 TO slnNumADIR
      IF LEFT(slaADIR(slnFor,1),1) <> '.'
        slnNumDir = slnNumDir+1
        DIMENSION slaDir(slnNumDir)
        slaDir(slnNumDir) = slaDir(slnDir)+'\'+;
          ALLTRIM(slaADIR(slnFor,1))
      ENDIF
    ENDFOR
  ENDIF
  slnDir = slnDir+1
ENDDO
STORE 0 TO ndatacnt,nrprtcnt
sgcPath = ''
lBadPunc = .F.
FOR slnFor = 1 TO slnNumDir
  zTarg = UPPE(ALLT(slaDir(slnFor)))
  ndatacnt = ndatacnt + IIF("\DATA" $ zTarg ,1 ,0)
  nrprtcnt = nrprtcnt + IIF("\REPORTS" $ zTarg ,1 ,0)
  IF slnFor = 1
    sgcPath = slaDir(slnFor)
  ELSE
    Ladd2path = .T.
    DO CASE
      CASE FILE("C:\ThisFileOnlyOn.Cti")
        IF [\DOCS] $ zTarg  && eliminate docs pathing when CTI
          Ladd2path = .F.
        ENDIF
      CASE [\DOCS]$zTarg .or. [\DATA\DATA]$zTarg .or. ndatacnt > 2
        Ladd2path = .F. && eliminate docs pathing for all others
      CASE cstartdrv = [C:] && user started up from C:

        *!*	      IF FILE("C:\ThisFileOnlyOn.Cti")
        *!*	        IF ![\DOCS]$slaDir(slnFor) && only eliminate docs pathing when CTI
        *!*	          sgcPath = sgcPath+';'+slaDir(slnFor)
        *!*	        ENDIF
        *!*	      ELSE  && when not cti only allow home, data, and reports
        *!*	        DO CASE
        *!*	          CASE ALLT(slaDir(slnFor)) == ALLT(sgcPjHome)
        *!*	             sgcPath = sgcPath+';'+slaDir(slnFor)
        *!*	          CASE [\DATA]$ALLT(slaDir(slnFor))
        *!*	             sgcPath = sgcPath+';'+slaDir(slnFor)
        *!*	          CASE [\REPORTS]$ALLT(slaDir(slnFor))
        *!*	             sgcPath = sgcPath+';'+slaDir(slnFor)
        *!*	        ENDCASE
        *!*	      ENDIF
      CASE cstartdrv # [C:] && user did not start up from C:
      OTHERWISE
        Ladd2path = .T. && default to add to path
    ENDCASE
    IF Ladd2path
      sgcPath = sgcPath+';'+slaDir(slnFor) && this was the single VPM original line
    ENDIF
  ENDIF
ENDFOR
**WAIT sgcpath WIND
SET PATH TO (sgcPath)
SET DEFAULT TO (sgcPjHome)
zzChar=[]
zzchar = LBadPunc(ALLT(sgcPath))
**WAIT zzchar WIND
IF LEN(zzchar)>0
  zzchar = IIF(zzchar=[ ],[SPACE],zzchar)
  =pwaitMB("An  AVM  folder contains undesirable punctuation character        "+zzchar+"   "+CHR(13)+CHR(13);
   +"Folder characters should only be Alpha, Numeric and underscores ( _ )   and   NO  spaces  "+CHR(13)+CHR(13);
   +"Ex:  ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"+CHR(13)+CHR(13);
   +"AVM may perform erratically if not corrected,  Contact  Tangoware for help")
ENDIF
RELE zzchar
* Open the Application Definition table.
IF NOT FILE('S'+sgcAppPre+'FApD.APD')
  ?? CHR(7)
  WAIT WINDOW 'Unable to find S'+sgcAppPre+'FApD.APD, press any key.'
  ON SHUTDOWN
  QUIT
ENDIF

slcApD = FULLPATH('S'+sgcAppPre+'FApD.APD')
IF NOT EVAL(sgcUUse+"(slcApD,'fapd')")
  ?? CHR(7)
  WAIT WINDOW 'Unable to open S'+sgcAppPre+'FApD.APD, press any key.'
  ON SHUTDOWN
  QUIT
ENDIF

* Put this application's name and version in the FoxPro window title.
slcIcon = ALLTRIM(fapd.logo_ico)
slnBS = RAT('\',slcIcon)
IF slnBS > 0
  slcIcon = SUBSTR(slcIcon,slnBS+1)
ENDIF
sgcAppTitle = ALLTRIM(fapd.name)+' '+ALLTRIM(fapd.version)
_SCREEN.Caption = sgcAppTitle
_SCREEN.Closable = .T.
IF NOT EMPTY(slcIcon)
  _SCREEN.Icon = slcIcon
ENDIF

* Store the Form Icon to the global memvar sgcFormIcon.  The icon
* specified in this memvar will be used by all forms in the application
* that are based on the Form_ class or a subclass.
sgcFormIcon = ALLTRIM(fapd.form_ico)
slnBS = RAT('\',sgcFormIcon)
IF slnBS > 0
  sgcFormIcon = SUBSTR(sgcFormIcon,slnBS+1)
ENDIF

* Add the tables and reports directories to the path.
sgcPath = sgcPath+';'
slcDDir = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.dbf_dir)),'SUBSTITUTE')")
IF NOT EMPTY(slcDDir)
  IF RIGHT(slcDDir,1) = '\'
    slcDDir = LEFT(slcDDir,LEN(slcDDir)-1)
  ENDIF
  IF NOT slcDDir+';' $ sgcPath
    sgcPath = sgcPath+slcDDir+';'
  ENDIF
ENDIF
slcRDir = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.rpt_dir)),'SUBSTITUTE')")
IF NOT EMPTY(slcRDir)
  IF RIGHT(slcRDir,1) = '\'
    slcRDir = LEFT(slcRDir,LEN(slcRDir)-1)
  ENDIF
  IF NOT slcRDir+';' $ sgcPath
    sgcPath = sgcPath+slcRDir+';'
  ENDIF
ENDIF
sgcPath = LEFT(sgcPath,LEN(sgcPath)-1)
SET PATH TO (sgcPath)

* Set global variables from the Application Definition table.
sgcPjDBF = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.dbf_dir)),'SUBSTITUTE')")
IF EMPTY(sgcPjDBF)
  sgcPjDBF = sgcPjHome
ENDIF
sgcPjRpt = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.rpt_dir)),'SUBSTITUTE')")
IF EMPTY(sgcPjRpt)
  sgcPjRpt = sgcPjHome
ENDIF
IF EMPTY(fapd.dd_dbc) OR EMPTY(fapd.dd_file) OR EMPTY(fapd.dd_field) OR ;
    EMPTY(fapd.dd_index)
  sgcPDD0 = 'S'+sgcAppPre+'DD0.DBF'
  sgcPDD1 = 'S'+sgcAppPre+'DD1.DBF'
  sgcPDD2 = 'S'+sgcAppPre+'DD2.DBF'
  sgcPDInd = 'S'+sgcAppPre+'DIND.DBF'
  sgcPDF3 = 'S'+sgcAppPre+'DF3.DBF'
ELSE
  sgcPDD0 = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.dd_dbc)),'SUBSTITUTE')")
  IF AT('.',sgcPDD0) = 0
    sgcPDD0 = sgcPDD0+'.DBF'
  ENDIF
  sgcPDD1 = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.dd_file)),'SUBSTITUTE')")
  IF AT('.',sgcPDD1) = 0
    sgcPDD1 = sgcPDD1+'.DBF'
  ENDIF
  sgcPDD2 = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.dd_field)),'SUBSTITUTE')")
  IF AT('.',sgcPDD2) = 0
    sgcPDD2 = sgcPDD2+'.DBF'
  ENDIF
  sgcPDInd = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.dd_index)),'SUBSTITUTE')")
  IF AT('.',sgcPDInd) = 0
    sgcPDInd = sgcPDInd+'.DBF'
  ENDIF
  sgcPDF3 = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.dd_f3)),'SUBSTITUTE')")
  IF AT('.',sgcPDF3) = 0
    sgcPDF3 = sgcPDF3+'.DBF'
  ENDIF
ENDIF
IF EMPTY(fapd.help_dbf)
  sgcHelpDBF = 'S'+sgcAppPre+'HELP.HLP'
ELSE
  sgcHelpDBF = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.help_dbf)),'SUBSTITUTE')")
  IF NOT EMPTY(sgcHelpDBF)
    IF AT('.',sgcHelpDBF) = 0
      sgcHelpDBF = sgcHelpDBF+'.HLP'
    ENDIF
  ENDIF
ENDIF
IF NOT EMPTY(sgcHelpDBF)
  IF FILE(sgcHelpDBF)
    SET HELP TO (sgcHelpDBF)
  ELSE
    sgcHelpDBF = ''
  ENDIF
ENDIF
IF EMPTY(fapd.user_dbf)
  sgcUserDBF = 'S'+sgcAppPre+'SUSR.DBF'
ELSE
  sgcUserDBF = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.user_dbf)),'SUBSTITUTE')")
  IF AT('.',sgcUserDBF) = 0
    sgcUserDBF = sgcUserDBF+'.DBF'
  ENDIF
ENDIF
IF EMPTY(fapd.error_dbf)
  sgcErrDBF = 'S'+sgcAppPre+'ELOG.DBF'
ELSE
  sgcErrDBF = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.error_dbf)),'SUBSTITUTE')")
  IF AT('.',sgcErrDBF) = 0
    sgcErrDBF = sgcErrDBF+'.DBF'
  ENDIF
ENDIF
IF EMPTY(fapd.audit_dbf)
  sgcAudDBF = 'S'+sgcAppPre+'UAUD.DBF'
ELSE
  sgcAudDBF = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.audit_dbf)),'SUBSTITUTE')")
  IF AT('.',sgcAudDBF) = 0
    sgcAudDBF = sgcAudDBF+'.DBF'
  ENDIF
ENDIF
IF EMPTY(fapd.act_dbf)
  sgcActDBF = 'S'+sgcAppPre+'UACT.DBF'
ELSE
  sgcActDBF = EVAL(sgcFDS3+"(ALLTRIM(UPPER(fapd.act_dbf)),'SUBSTITUTE')")
  IF AT('.',sgcActDBF) = 0
    sgcActDBF = sgcActDBF+'.DBF'
  ENDIF
ENDIF

* Set up the environment for this application.
IF fapd.autosave
  SET AUTOSAVE ON
ELSE
  SET AUTOSAVE OFF
ENDIF
IF fapd.bell
  IF fapd.bell_freq >= 19 AND fapd.bell_freq <= 10000 AND;
      fapd.bell_f_len >= 1 AND fapd.bell_f_len <= 19
    SET BELL TO fapd.bell_freq,fapd.bell_f_len
    SET BELL ON
  ELSE
    SET BELL ON
  ENDIF
ELSE
  SET BELL OFF
ENDIF
SET BLOCKSIZE TO fapd.blocksize
IF fapd.century
  SET CENTURY ON
ELSE
  SET CENTURY OFF
ENDIF
IF fapd.cent < 1
  SET CENTURY TO VAL(LEFT(LTRIM(STR(YEAR(DATE()))),2)) ROLLOVER fapd.ROLLOVER
ELSE
  SET CENTURY TO fapd.cent ROLLOVER fapd.ROLLOVER
ENDIF
IF fapd.clear
  SET CLEAR ON
ELSE
  SET CLEAR OFF
ENDIF
IF fapd.status_bar
  SET STATUS BAR ON
  SET STATUS TIMEOUT TO (fapd.status_mto)
ELSE
  SET STATUS BAR OFF
ENDIF
IF fapd.clock
  IF fapd.clock_sbar AND SET('STATUS BAR') = 'ON'
    SET CLOCK STATUS
  ELSE
    IF fapd.clock_row = 0 AND fapd.clock_col = 0
      SET CLOCK ON
    ELSE
      SET CLOCK TO (fapd.clock_row),(fapd.clock_col)
    ENDIF
  ENDIF
  IF fapd.clock_hrs = 2
    SET HOURS TO 24
  ELSE
    SET HOURS TO 12
  ENDIF
ELSE
  SET CLOCK OFF
ENDIF
IF fapd.confirm
  SET CONFIRM ON
ELSE
  SET CONFIRM OFF
ENDIF
SET DECIMALS TO fapd.cur_dec
SET CURRENCY TO fapd.cur_symbol
IF fapd.cur_left
  SET CURRENCY LEFT
ELSE
  SET CURRENCY RIGHT
ENDIF
IF EMPTY(fapd.cur_sep)
  SET SEPARATOR TO
ELSE
  SET SEPARATOR TO fapd.cur_sep
ENDIF
IF EMPTY(fapd.cur_point)
  SET POINT TO
ELSE
  SET POINT TO fapd.cur_point
ENDIF
DO CASE
  CASE fapd.date_formt = 2
    SET DATE TO ANSI
  CASE fapd.date_formt = 3
    SET DATE TO BRITISH
  CASE fapd.date_formt = 4
    SET DATE TO FRENCH
  CASE fapd.date_formt = 5
    SET DATE TO GERMAN
  CASE fapd.date_formt = 6
    SET DATE TO ITALIAN
  CASE fapd.date_formt = 7
    SET DATE TO JAPAN
  CASE fapd.date_formt = 8
    SET DATE TO USA
  CASE fapd.date_formt = 9
    SET DATE TO MDY
  CASE fapd.date_formt = 10
    SET DATE TO DMY
  CASE fapd.date_formt = 11
    SET DATE TO YMD
  OTHERWISE
    SET DATE TO AMERICAN
ENDCASE
IF EMPTY(fapd.date_sep)
  SET MARK TO
ELSE
  SET MARK TO fapd.date_sep
ENDIF
IF fapd.debug
  SET DEBUG ON
ELSE
  SET DEBUG OFF
ENDIF
IF fapd.developmnt
  SET DEVELOPMENT ON
ELSE
  SET DEVELOPMENT OFF
ENDIF
IF fapd.escape
  SET ESCAPE ON
ELSE
  SET ESCAPE OFF
ENDIF
IF fapd.heading
  SET HEADING ON
ELSE
  SET HEADING OFF
ENDIF
IF fapd.logo
  sglLogo = .T.
ELSE
  sglLogo = .F.
ENDIF
IF fapd.notify
  SET NOTIFY ON
ELSE
  SET NOTIFY OFF
ENDIF
SET ODOMETER TO fapd.odometer
IF fapd.quit_conf
  sglQuitConf = .T.
ELSE
  sglQuitConf = .F.
ENDIF
SET REFRESH TO fapd.refresh
IF fapd.reprocess > 0
  IF fapd.rep_sec
    SET REPROCESS TO fapd.reprocess SECONDS
  ELSE
    SET REPROCESS TO fapd.reprocess
  ENDIF
ELSE
  SET REPROCESS TO fapd.reprocess
ENDIF
IF fapd.resource
  * If this application is being run in runtime mode (not test mode),
  * the resource file is set through S<Prefix>FCnf.FPW (config.fpw).
  IF TYPE('slcMemPre') <> 'U'			&& Test mode
    * The first user to run this application in the test mode will use
    * the application's resource file.  If a user runs this application
    * in the test mode but someone else is already using the application's
    * resource file, the user will continue using the same resource file
    * that they were using in ProMatrix.
    slnCurArea = SELECT()
    IF EVAL(sgcUUse+"()")				&& Find unused work area
      slcOnError = ON('ERROR')
      sllError = .F.
      ON ERROR sllError = .T.
      USE ('S'+sgcAppPre+'FRes')
      ON ERROR &slcOnError
      IF NOT sllError				&& Resource file is available
        USE
        SET RESOURCE TO ('S'+sgcAppPre+'FRes')
      ENDIF
      SELECT (slnCurArea)
    ENDIF
  ENDIF
ELSE
  SET RESOURCE OFF
ENDIF
IF fapd.safety
  SET SAFETY ON
ELSE
  SET SAFETY OFF
ENDIF
SET TYPEAHEAD TO fapd.typeahead

USE IN fapd				&& Close the Application Definition table

* Open the application and system tables and views that remain open.
slnNumApp = 0			&& Number of application tables to open
slnNumSys = 1			&& Number of system tables to open
slnNumOpened = 0
IF EVAL(sgcUUse+"(sgcPDD1)")
  * Determine the number of tables to be opened.
  SCAN FOR NOT EMPTY(file_name) AND open_start
    slnNumApp = slnNumApp+1
    DIMENSION slaOpen(slnNumApp,3)
    slaOpen(slnNumApp,1) = ALLTRIM(file_name)
    slaOpen(slnNumApp,2) = sqlview
    slaOpen(slnNumApp,3) = ALLTRIM(dbc_name)
  ENDSCAN
  USE
  IF slnNumApp > 0
    DO FORM ('S'+sgcAppPre+'UTh') NAME oThermometer LINKED WITH 'Opening Tables'
    * Open application tables that are identified in the Data Dictionary
    * as tables that should be opened upon application startup.
    FOR slnFor = 1 TO slnNumApp
      IF slaOpen(slnFor,2)		&& View
        IF NOT EMPTY(slaOpen(slnFor,3)) AND FILE(slaOpen(slnFor,3)+'.DBC')
          * Open the database or make it the current database
          * if already open.
          slcOrigDBC = ALLTRIM(DBC())
          IF DBUSED(slaOpen(slnFor,3))
            SET DATABASE TO (slaOpen(slnFor,3))
          ELSE
            OPEN DATABASE (slaOpen(slnFor,3))
          ENDIF
          IF DBUSED(slaOpen(slnFor,3)) AND INDBC(slaOpen(slnFor,1),'VIEW')
            SELECT 0
            USE (slaOpen(slnFor,1))
          ENDIF
          IF EMPTY(slcOrigDBC)
            SET DATABASE TO
          ELSE
            SET DATABASE TO (slcOrigDBC)
          ENDIF
        ENDIF
      ELSE						&& Table
        = EVAL(sgcUUse+"(slaOpen(slnFor,1))")
      ENDIF
      slnNumOpened = slnNumOpened+1
      oThermometer.Update(slnNumOpened/(slnNumApp+slnNumSys)*100)
    ENDFOR
  ENDIF
ENDIF
* Open the system tables.
= EVAL(sgcUUse+"('S'+sgcCurPre+'SEC','mcfsec')")
IF slnNumApp > 0		&& Thermometer only shown if app tables were opened
  oThermometer.Complete()
ENDIF
