IF 0 < is_run32([SPN-2000]) &&.or. 0 < is_run32([SCMI]) &&.and. 0 < is_run32([Licensee:])
  IF FILE("C:\THISFILEONLYON.CTI")
    ** Aborts if another instance running
    = MESSAGEBOX('ADVISORY ONLY   SPN-2000 already running.   CTI Development Machine ',0+16,[SPN-2000])
  ELSE
    IF !FILE("C:\MULTISPN.OK")
      wtd = MESSAGEBOX('SPN-2000 already running.  Unable to open another instance ',0+16,[SPN-2000])
      RETURN
    ENDIF
  ENDIF
ENDIF

*** Start of new check by DG ***
DECLARE INTEGER GetActiveWindow ;
   IN Win32API
DECLARE INTEGER GetWindow IN Win32API ;
   INTEGER hWnd, INTEGER nType
DECLARE INTEGER GetWindowText IN Win32API ;
   INTEGER hWnd, STRING @cText, INTEGER nType
hNext = GetActiveWindow()
DO WHILE hNext<>0
   cText = REPLICATE(CHR(0),80)
   GetWindowText(hNext,@cText,80)
   IF ATC('SCMI - DATA MANAGEMENT',cText) > 0
      MESSAGEBOX('ADVISORY NOTICE: SPN2000 is already running',0+16,'SECOND COPY OF SPN2000?')
   ENDIF 
   hNext = GetWindow(hNext,2)
ENDDO
*** End of new check by DG ***

IF SYSM(1) < 800 .and. SYSM(2) < 600
  = MESSAGEBOX('Your Video resolution   '+ALLT(STR(SYSM(1)))+[ X ]+ALLT(STR(SYSM(2)))+'   is too low to run SPN2000'+CHR(13);
    +[You will need a higher Video resolution to see all SPN screens  ]+CHR(13)+CHR(13);
    +[Click on  File  then  Exit   -  to leave the SPN2000, ]+CHR(13);
    +[adjust your resolution to   800 X 600   or higher and try again  ],0+16,[SPN Notice])
ENDIF
**** ! net file | findstr wboarddat > F:\NACFOX\wbrdopen.txt

*!*	IF FILE("C:\ThisFileOnlyOn.CTI").and.FILE("C:\nacdev\spn_avmdev.bat")
*!*	  ! C:\nacdev\spn_avmdev.bat
*!*	ENDIF
IF !FILE("C:\nacvfp\spn_init2.cmd").and.FILE("F:\nacfox\spn_init2.cmd")
  COPY FILE F:\nacfox\spn_init2.cmd TO C:\nacvfp\spn_init2.cmd
ENDIF
IF !FILE("C:\ThisFileOnlyOn.CTI").and.FILE("C:\nacvfp\spn_init2.cmd")
  IF FILE("c:\windows\system32\shell32.dll")
    DECLARE INTEGER ShellExecute IN shell32.dll ;
      INTEGER hndWin, ;
      STRING cAction, ;
      STRING cFileName, ;
      STRING cParams, ;
      STRING cDir, ;
      INTEGER nShowWin
      =ShellExecute(0,"OPEN","C:\NACVFP\spn_init2.cmd","","",0) && updates local time from server
  ENDIF
ELSE
  IF !FILE("C:\ThisFileOnlyOn.CTI").and.FILE("F:\nacfox\spn_init2.cmd")
    IF FILE("c:\windows\system32\shell32.dll")
      DECLARE INTEGER ShellExecute IN shell32.dll ;
        INTEGER hndWin, ;
        STRING cAction, ;
        STRING cFileName, ;
        STRING cParams, ;
        STRING cDir, ;
        INTEGER nShowWin
        =ShellExecute(0,"OPEN","F:\nacfox\spn_init2.cmd","","",0) && updates local time from server
    ENDIF
  ENDIF
ENDIF
*!*	WAIT [Prog Chain   1 ]+PROG(1)+[  *2  ]+PROG(2)+[  *3  ]+PROG(3)+[  *4  ]+PROG(4);
*!*	 +[  *5  ]+PROG(5)+[  *6  ]+PROG(6)+[  *7  ]+PROG(7) WIND
*!*	_ClipText = SET('PATH')
=SYS(3050,1,24000000) && FOREGROUND MEMORY set to 24 meg
SET PROCEDURE TO davmprcs
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
zzStartDrv = ALLT(SYS(5))
zzStartDir = ALLT(SYS(5))+ALLT(SYS(2003))
IF !FILE("C:\ThisFileOnlyOn.CTI") .and. INLI(DOW(DATE()),3,6).and.![SPN2000]$zzStartDir .and. [C:]$zzStartDrv ;
    .and.[SAVMFMAN]$UPPE(PROG(1)) .and. FILE(zzStartDir+"\STARTSPN.EXE");
    .and.!FILE(zzStartDir+"\DATA\configav.dbf")
  ***FILE("C:\ThisIsThe2400.CTI") .and. FILE("C:\ThisIsThe8400.CTI")
  *!*	  WAIT [Prog Chain   1 ]+PROG(1)+[  *2  ]+PROG(2)+[  *3  ]+PROG(3)+[  *4  ]+PROG(4);
  *!*	    +[  *5  ]+PROG(5)+[  *6  ]+PROG(6)+[  *7  ]+PROG(7) WIND
  *// startSPN.exe is being distributed w/both updaters (since Jan2003)
  *// use this to make a string of programs.  Test to see if STARTSPN is contained in string   Nov 11 2003
  *// if not and STARTSPN.EXE exists then advise user and or use to install to a local drive
  *// use to make sure not executing SPN2000.EXE first, when STARTSPN exists
  *// or make sure not executing SPN2000.EXE directly from server   etc.
  =pwaitMB([SPN has the capability to automatically Update this local workstation if your SPN ICON]+CHR(13);
    +[points to the file called  STARTSPN.EXE  which resides in your SPN starting folder])
ENDIF
SET SAFETY OFF
IF [6.0] $ OS(3) .and. DOW(DATE())=2 && W7 mondays
  wtd=MESSAGEBOX([Please leave your computer on tonight for updates, ]+CHR(13)+CHR(13)+[Thanx - Spinnaker IT Department],0+48,[MONDAY UPDATE])
*1=OK
ENDIF

SET TALK OFF

***** DOCASSOCIATIONS *****
cFileName = zzStartDir+'\docassociations.dbf'
IF !FILE((cFileName)) .and. FILE("F:\nacfox\docassociations.DBF")
  USE F:\nacfox\docassociations.DBF SHARED IN 0 NOUPDATE ALIAS prodfile
  SELE prodfile
  COPY TO (cFileName)
  SELECT prodfile
  USE
ENDIF

***** CONFIGLC *****
cFileName = zzStartDir+'\configlc.dbf'
IF !FILE((cFileName)) .and. FILE("F:\nacfox\configlc.DBF")
  USE F:\nacfox\configlc.DBF SHARED IN 0 NOUPDATE ALIAS prodfile
  SELE prodfile
  COPY TO (cFileName)
  SELECT prodfile
  USE
ENDIF

***** CONFLOC *****
cFileName = zzStartDir+'\confloc.dbf'
IF !FILE((cFileName)) .and. FILE("F:\nacfox\confloc.DBF")
  **WAIT cFileName  WIND
  USE F:\nacfox\confloc.DBF SHARED IN 0 NOUPDATE ALIAS prodfile
  SELE prodfile
  COPY TO (cFileName)
  SELECT prodfile
  USE
ENDIF
IF !USED('confloc')
  SELE 0
  USE (cFileName) ALIAS confloc EXCL
  IF FSIZE('fdefmenu') < 1
    ALTER TABLE confloc ADD COLUMN fdefmenu C(6)
  ENDIF
  IF FSIZE('lastvend') < 10 && 19Sep19 was 4 char
    ALTER TABLE confloc ALTER COLUMN lastvend C(10)
  ENDIF
  IF FSIZE('lastcust') < 10 && 19Sep19 was 4 char
    ALTER TABLE confloc ALTER COLUMN lastcust C(10)
  ENDIF
  IF FSIZE('last_po') < 6 && 19Sep19 was 5 char
    ALTER TABLE confloc ALTER COLUMN last_po C(6)
  ENDIF
  IF FSIZE('lastmfgr') < 10 && 19Sep19 was 5 char
    ALTER TABLE confloc ALTER COLUMN lastmfgr C(10)
  ENDIF
  SELECT confloc
  USE
ENDIF

*!*	oWsh=CREATEOBJECT("wscript.shell")
*!*	IF TYPE("oWsh")=[O]
*!*	  *IF FILE(zzStartDir+"\SPN2000.EXE") && found exe
*!*	    cDeskPath = oWsh.SpecialFolders("desktop")
*!*	    Oshort    = oWsh.CreateShortCut(cDeskPath+"\SPN2000.LNK")
*!*	    *IF FILE(zzStartDir+"\_START_SPN.EXE")
*!*	      Oshort.TargetPath  = "\_START_SPN_2.EXE"
*!*	      Oshort.Description = "SPN2000"
*!*	    *ELSE
*!*	    *  Oshort.TargetPath  = zzStartDir+"\SPN2000.EXE"
*!*	    *  Oshort.Description = "SPN2000 WorkStation"
*!*	    *ENDIF
*!*	    Oshort.WorkingDirectory = cdeskPath+"\"
*!*	    Oshort.Save
*!*	    *=pwaitMB([Desktop Shortcut called    "SPN2000 WorkStation"    has been created.])
*!*	  *ELSE
*!*	  *  =pwaitMB([Unable to create a DeskTop Icon.])
*!*	  *ENDIF
*!*	ENDIF
*!*	IF TYPE("oWsh")=[O]
*!*	  RELEASE oWsh
*!*	ENDIF

*!*	Set Shell = CreateObject("WScript.Shell") 
*!*	Set FSO = CreateObject("Scripting.FileSystemObject") 
*!*	DesktopPath = Shell.SpecialFolders("Desktop") 
*!*	FSO.DeleteFile DesktopPath & "\test.lnk" 

*!*	cd (desktoppath)
*!*	erase _start_spn.exe


*!*	  =AFONT(ZFONTS)  && THESE FONT NAMES ARE CASE SENSITIVE !
*!*	  IF 0=ASCAN(ZFONTS,"3 of 9 Barcode").and. FILE("F:\NACFOX\3OF9.TTF")
*!*	    =FontInstaller("F:\NACFOX","3OF9.TTF","3 of 9 Barcode (True Type)")
*!*	    CLEAR DLLS
*!*	  ENDIF
*!*	  IF 0=ASCAN(ZFONTS,"Lucida Console").and. FILE("F:\NACFOX\LUCON.TTF")
*!*	    =FontInstaller("F:\NACFOX","LUCON.TTF","Lucida Console (True Type)")
*!*	    CLEAR DLLS
*!*	  ENDIF
*!*	  IF 0=ASCAN(ZFONTS,"Arial").and. FILE("F:\NACFOX\ARIAL.TTF")
*!*	    =FontInstaller("F:\NACFOX","ARIAL.TTF","Arial (True Type)")
*!*	    CLEAR DLLS
*!*	  ENDIF
*!*	*!*	  IF 0=ASCAN(ZFONTS,"Monotype Sorts").and. FILE("F:\NACFOX\mtsorts.TTF")
*!*	*!*	    =FontInstaller("F:\NACFOX","mtsorts.TTF","MonoType Sorts (True Type)")
*!*	*!*	    CLEAR DLLS
*!*	*!*	  ENDIF
*!*	*!*	  IF 0=ASCAN(ZFONTS,"MONACO").and. FILE("F:\NACFOX\MONACO.TTF")  && ANDAM___.TTF
*!*	*!*	    =FontInstaller("F:\NACFOX","MONACO.TTF","Monaco (True Type)")
*!*	*!*	    CLEAR DLLS
*!*	*!*	  ENDIF
*!*	  IF 0=ASCAN(ZFONTS,"Andale Mono").and. FILE("F:\NACFOX\ANDAM___.TTF")  && ANDAM___.TTF
*!*	    =FontInstaller("F:\NACFOX","ANDAM___.TTF","Andale Mono (True Type)")
*!*	    CLEAR DLLS
*!*	  ENDIF
*!*	  IF 0=ASCAN(ZFONTS,"Tahoma Slashed").and. FILE("F:\NACFOX\tahoma_slashed.ttf")
*!*	    =FontInstaller("F:\NACFOX","tahoma_slashed.ttf","Tahoma Slashed (True Type)")
*!*	    CLEAR DLLS
*!*	  ENDIF
SET FULLPATH ON
FOR ww = 2 TO 12 && sets all function keys config.fp
  ON KEY LABEL 'F'+ALLTRIM(STR(ww))
NEXT ww
PUBLIC psysvers,_pUserId,_pSecIsOn,_OkFpReset,__pWS_Single
STORE .F. TO L1,L2,L3,L4
STORE [] TO __pWS_Single
L1 = FILE(zzStartDir+"\DATA\CONFIGAV.DBF")
L2 = FILE(zzStartDir+"\DATA\AVCUSTOM.DBF")
L3 = FILE(zzStartDir+"\DATA\SAVMDIND.DBF")
L4 = FILE(zzStartDir+"\DATA\SAVMDD2.DBF")
__pWS_Single = IIF(L1.and.L2.and.L3.and.L4,[S],[W])
IF 1=1  &&__pWS_Single = [W]
  SET PATH TO ALLT(zzStartDir)+[;]+zzStartDir+"\DATA"+[;F:\NACFOX] && needed, to "get started on right path"
&& // 05Aug2004 found that workstations were opening the server's configlc b4 VPM established path
ENDIF
SET FULLPATH ON
*WAIT [1    ]+SET('PATH') WIND
*WAIT [1    ]+FULLPATH("configlc.dbf") WIND
RELEASE L1,L2,L3,L4
ERASE C:\nacvfp\temp\tmpWBall.*

*###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*###################%%%%%%%%%%%%%%%%%%%%%\%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*###################               DO  PREP_BUILD                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

STORE                              [32.66  Build: Mar-23-2020]        TO psysvers &&+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*////                              [1234567890123456789012345]  NEVER CHANGE FORMAT or LEngth (c25) of psysvers.  SysInfo and other forms parse and analyze
*###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
=NUMLOCK(.T.)
*// printer error reset per MicroSoft
IF (FILE("c:\ThisIsThe2400.CTI").or.FILE("C:\ThisIsThe1500.CTI")).and.FILE("c:\vpm51\data\ssyspl10.dbf")
  SELE 0
  USE c:\vpm51\data\ssyspl10 AGAIN ALIAS hfywef
  SELE hfywef
  LOCATE FOR [SPN-2000]$name_ver
  IF FOUND()
    REPLA name_ver WITH [SPN-2000 ]+LEFT(psysvers  ,6)
  ENDIF
  USE IN hfywef
ENDIF
IF !FILE('STOPFP').and.(FILE('MSVCRT20.DLL').or.FILE('C:\WINDOWS\MSVCRT20.DLL').or.FILE('C:\WINDOWS\SYSTEM\MSVCRT20.DLL');
    .or.FILE('C:\WINDOWS\SYSTEM32\MSVCRT20.DLL').or.FILE('C:\WINNT\MSVCRT20.DLL').or.FILE('C:\WINNT\SYSTEM32\MSVCRT20.DLL'))
  _OkFpReset=.T.
  DECLARE _fpreset IN MSVCRT20.DLL
ENDIF
=CtiFpReset()
*IF _OkFpReset &&// call dll FloatingPoint Processor reset
*  =fpreset()
*ENDIF
* Main program for SPN project.
#INCLUDE "MAIN.H"
SET TALK OFF
_screen.WindowState=2 &&--- full size regardless of res prr 04-25-98

* Refox lines used when branding application.
IF .F.
  _ReFox_=(9876543210)
  _ReFox_=(9876543210)
ENDIF

* DECLARE DLL statements for reading/writing to private INI files
DECLARE INTEGER GetPrivateProfileString IN Win32API  AS GetPrivStr ;
  String cSection, String cKey, String cDefault, String @cBuffer, ;
  Integer nBufferSize, String cINIFile
DECLARE INTEGER WritePrivateProfileString IN Win32API AS WritePrivStr ;
  String cSection, String cKey, String cValue, String cINIFile

* Make sure that the project manager is closed.
DEACTIVATE WINDOW 'Project Manager'

* Set initial environment.
PUSH MENU _MSYSMENU
CLEAR
SET BELL OFF
SET DELETED ON
SET EXACT OFF
SET EXCLUSIVE OFF
SET MULTILOCKS ON
* Display blank menu.
SET SYSMENU TO

* Initialize global variables.
STORE 'AVM' TO sgcAppPre,sgcCurPre,sgcMenu
STORE '' TO sgcErrDBF,sgcHelpDBF,sgcUserDBF,;
  sgcSetList,sgcPath,sgcPDD0,sgcPDD1,sgcPDD2,;
  sgcPjHome,sgcPjDBF,sgcPjRpt,sgcPrompt,;
  sgcHomeDir,sgcAudDBF,sgcActDBF,sgcAppTitle,;
  sgcFormIcon,sgcPDInd,sgcPDF3
STORE .T. TO sglLogo,sglQuitConf,sglNoLogo

* Indirect referencing variables.
sgcCDef = 'SAVMCDef'
sgcCRI = 'SAVMCRI'
sgcCWhn = 'SAVMCWhn'
sgcEEH = 'SAVMEEH'
sgcFCT = 'SAVMFCT'
sgcFCTA = 'SAVMFCTA'
sgcFDS2 = 'SAVMFDS2'
sgcFDS3 = 'SAVMFDS3'
sgcFEnv = 'SAVMFEnv'
sgcFMan = 'SAVMFMan'
sgcHF1 = 'SAVMHF1'
sgcHF2 = 'SAVMHF2'
sgcHF2R = 'SAVMHF2R'
sgcHF2S = 'SAVMHF2S'
sgcHF3 = 'SAVMHF3'
sgcHF3F = 'SAVMHF3F'
sgcSInt = 'SAVMSInt'
sgcSMCF = 'SAVMSMCF'
sgcSSA = 'SAVMSSA.SPR'
sgcUAct = 'SAVMUAct'
sgcUAl = 'SAVMUAl'
sgcUCpy = 'SAVMUCpy'
sgcUDir = 'SAVMUDir'
sgcULgo = 'SAVMULgo'
sgcUMv = 'SAVMUMv'
sgcUND = 'SAVMUND'
sgcUPop = 'SAVMUPop'
sgcUPsh = 'SAVMUPsh'
sgcURL = 'SAVMURL'
sgcURP = 'SAVMURP'
sgcUSP = 'SAVMUSP'
sgcUUse = 'SAVMUUse'
sgcVFld = 'SAVMVFld'

ON SHUTDOWN KEYBOARD '{CTRL+Y}'

* If an error occurs prior to the user gaining access to the
* application, the error handler will make sure a QUIT is issued.
* Note: The actual ON ERROR command is issued in (sgcFCTA).
sglErrQuit = .T.
ON ERROR DO &sgcEEH WITH ERROR(),MESSAGE(),SYS(2018),MESSAGE(1),;
  SYS(16),PROGRAM(),LINENO(),LASTKEY(),VARREAD(),WONTOP()

* Load the Drive Swapping array.
PUBLIC ARRAY sgaDrvSwap(1,2)
DIMENSION sgaDrvSwap(1,2)
STORE '' TO sgaDrvSwap
= EVAL(sgcFDS2+"(1)")

CAPSLOCK(.F.)
zCurrAlias=ALIAS()
**WAIT zzStartDrv+zzStartDir+[\] WIND TIME 3
STORE [] TO zCurrDbf_dir,zFileLoc
IF 1=1  &&FILE("savmfapd.apd")  && zzStartDir does NOT have a backslash on end  ex: C:\SPN
  SELE 0
  USE savmfapd.apd AGAIN ALIAS zavmfapd
  *// 15Apr2000 was prev lokking for just C:\SPN but messed up on AVM_2000 or SPN2000 because it matched
  *// added backslash to ensure it works
  *// used to reset directory from CTI's dev dir on other users computers
  IF [C:\SPN\]$UPPE(zavmfapd.dbf_dir).and.![SPN\]$UPPE(zzStartDir+[\])  &&--- dev dir, but not in the dev dir
    REPLACE zavmfapd.dbf_dir WITH UPPE(STRTR(zavmfapd.dbf_dir,[C:\SPN\],zzStartDir+[\]))
  ENDIF
  IF [C:\SPN\]$UPPE(zavmfapd.rpt_dir).and.![SPN\]$UPPE(zzStartDir+[\])  &&--- dev dir, but not in the dev dir
    REPLACE zavmfapd.rpt_dir WITH UPPE(STRTR(zavmfapd.rpt_dir,[C:\SPN\],zzStartDir+[\]))
  ENDIF
  IF zavmfapd.rollover=0.or.zavmfapd.cent=0  &&--- dev dir, but not in the dev dir
    REPLACE zavmfapd.rollover WITH 33
    REPLACE zavmfapd.cent WITH 19
  ENDIF
  IF zavmfapd.rollover=0.or.!BETW(zavmfapd.rollover,2,99)
    REPLACE zavmfapd.rollover WITH 33
  ENDIF
  IF zavmfapd.cent=0 .or.!BETW(zavmfapd.cent,19,22) && C:\SPN\BITMAPS\A2K-LOGO.BMP
    REPLACE zavmfapd.cent WITH 19
  ENDIF
*!*	  IF ![A2K-LOGO.BMP]$zavmfapd.logo_bmp
*!*	    REPLACE zavmfapd.logo_bmp WITH "C:\SPN\BITMAPS\A2K-LOGO.BMP"
*!*	  ENDIF
  IF ![CA.BMP]$zavmfapd.logo_bmp
    REPLACE zavmfapd.logo_bmp WITH "C:\SPN\BITMAPS\CA.BMP"
  ENDIF
  IF zavmfapd.cur_dec # 3
    REPLACE zavmfapd.cur_dec WITH 3
  ENDIF
  IF !resource  &&!FILE("C:\THISFILEONLYON.CTI").or.![C:\SPN\]$UPPE(zzStartDir+[\])  && need foxuser for diary calendar and report pos
    REPLA resource WITH .T.
  ENDIF
  zCurrDbf_dir = zavmfapd.dbf_dir
  SET CURRENCY TO zavmfapd.cur_symbol
  USE IN zavmfapd
ENDIF
zFoundConfigav=FILE(ALLT(zCurrDbf_dir)+[\CONFIGAV.DBF])    &&.or.FILE(zzStartDir+[\data\configav.dbf])
IF 111=2222&&!zFoundConfigav
  ** returned value ends with \  ex: C:\ACTOOLS\
  zFileLoc=ALLT(GETDIR([\SPN2000\],[Find \SPN2000 Folder]))
ENDIF
jApdDataDir=[]
IF 1=1  &&FILE("savmfapd.apd")
  *zFileLoc = STRTR(zFileLoc,"\DATA\","\")  && strip \data then add in below
  SELE 0
  USE savmfapd.apd AGAIN ALIAS zavmfapd
  REPLACE zavmfapd.version WITH LEFT(psysvers,6)
  IF 1=1 &&!EMPT(zFileLoc)  &&--- update dir locs if needed to use the get dir function above
    REPLACE zavmfapd.dbf_dir WITH [F:\NACFOX] &&STRTR(ALLT(zFileLoc)+[DATA],[\\],[\])
    REPLACE zavmfapd.rpt_dir WITH [F:\NACFOX\REPORTS] &&STRTR(ALLT(zFileLoc)+[REPORTS],[\\],[\])
  ENDIF
  jApdDataDir = zavmfapd.dbf_dir
  USE IN zavmfapd
ENDIF
IF !EMPT(zCurrAlias)
  SELE (zCurrAlias)
ENDIF
RELE zFileLoc,zFoundConfigav,zCurrDbf_dir
zCrCr = CHR(13)+CHR(13)
zCrLf = CHR(13)+CHR(10)
IF !FILE("C:\StopMapSearch.SPN") .and. !FILE(jApdDataDir+[\configav.dbf])  &&.or.!FILE(zzStartDir+[\data\configav.dbf]))
  =pwaitMB([The main data folder   ]+ALLT(jApdDataDir)+[    or its primary files cannot be located]+zCrCr;
    +[Networked users may not have a drive letter mapped. Use Windows Explorer to locate]+zCrLf;
    +[and or map a drive letter to the drive which holds the SPN2000 folder],[ Aborting Application])
  RETURN
ENDIF
*!*	IF !FILE(zzStartDir+[\data\browcfgs.dbf])
*!*	  zDbfFile=zzStartDir+[\data\browcfgs] && parent
*!*	  CREATE DBF &zDbfFile (FORMNAME C(20) ,PAGENAME C(10) ,GRIDNAME C(10) ,COLUMNCNT N(2) ,ORDER01 N(2) ,WIDTH01 N(4) ,;
*!*	    ORDER02 N(2) ,WIDTH02 N(4) ,ORDER03 N(2) ,WIDTH03 N(4) ,ORDER04 N(2) ,WIDTH04 N(4) ,;
*!*	    ORDER05 N(2) ,WIDTH05 N(4) ,ORDER06 N(2) ,WIDTH06 N(4) ,ORDER07 N(2) ,WIDTH07 N(4) ,;
*!*	    ORDER08 N(2) ,WIDTH08 N(4) ,ORDER09 N(2) ,WIDTH09 N(4) ,ORDER10 N(2) ,WIDTH10 N(4) ,;
*!*	    ORDER11 N(2) ,WIDTH11 N(4) ,ORDER12 N(2) ,WIDTH12 N(4) ,ORDER13 N(2) ,WIDTH13 N(4) ,;
*!*	    ORDER14 N(2) ,WIDTH14 N(4) ,ORDER15 N(2) ,WIDTH15 N(4) ,ORDER16 N(2) ,WIDTH16 N(4) ,;
*!*	    ORDER17 N(2) ,WIDTH17 N(4) ,ORDER18 N(2) ,WIDTH18 N(4) ,ORDER19 N(2) ,WIDTH19 N(4) ,;
*!*	    ORDER20 N(2) ,WIDTH20 N(4) )
*!*	ENDIF
IF USED("browcfgs")
  USE IN browcfgs
ENDIF
*WAIT [1] WIND
*!*	IF !FILE(zzStartDir+[\DATA\RPT_DEFS.DBF])
*!*	  zDbfFile=zzStartDir+[\DATA\RPT_DEFS] && parent
*!*	  CREATE DBF &zDbfFile ( REPORT C(30), DEFAULTS C(254) )
*!*	ENDIF
*!*	IF USED("rpt_defs")
*!*	  USE IN rpt_defs
*!*	ENDIF

* Set environment for this application.
DO (sgcFEnv) &&==============
*WAIT [2] WIND
IF FILE("lockout.dbf")
  SELE 0
  USE lockout
  jLock=lockedout
  USE IN lockout
  IF jLock
    =pwaitMB([Someone in your organization has LOCKED  SPN  from use at this time.]+CHR(13)+CHR(13);
      +[This is typically done in order to run specific Maintenance functions]+CHR(13);
      +[such as Data Backups, Indexing etc...  ]+CHR(13)+CHR(13);
      +[See your System Administrator regarding access.])
    = siExitApp()
    RETURN
  ENDIF
ENDIF
IF FILE("C:\NACVFP\WIN10.TST")
  *WAIT [WIN 10 TEST 1  ]+SET("PATH") WIND TIME 5
ENDIF

IF !USED("configlc").and.FILE("C:\nacvfp\configlc.dbf")  && buffered
  USE C:\nacvfp\configlc EXCL
  IF FSIZE("n2olrma") = 0
    ALTER TABLE configlc ADD COLUMN n2olrma L(1)
  ENDIF
  USE IN configlc
ENDIF

IF FILE("C:\NACVFP\WIN10.TST")
  *WAIT [WIN 10 TEST 2  ]+SET("PATH") WIND TIME 5
ENDIF

SELE 0
USE configav
xxhmt = configav.hmt
zNver = VAL(LEFT(ALLT(psysvers),4))
jCver = LEFT(ALLT(psysvers),5)
*!*	IF zNver > VAL(LEFT(ALLT(configav.lastupdate),4))
*!*	  REPLACE configav.lastupdate WITH LEFT(ALLT(psysvers),4)
*!*	ENDIF
*!*	IF zNver < VAL(LEFT(ALLT(configav.lastupdate),4))
*!*	  = MESSAGEBOX('Your AVM 2000 version is   '+LEFT(ALLT(psysvers),4)+;
*!*	    '   yet the System shows a more recent version   '+LEFT(ALLT(configav.lastupdate),4)+[ ]+CHR(13)+CHR(13)+;
*!*	    'You should update to version   '+LEFT(ALLT(configav.lastupdate),4)+'   to avoid problems';
*!*	    ,0+16,[ VERSION CONFLICT ADVISORY!])
*!*	ENDIF
*!*	IF configav.fmisc007 = "Y"  && don't use here because Level 1 users could never shut it off, rely on individual warnings in inv,invoices,P&E,wo
*!*	  =pwaitMB([ADVISORY:  A system update was completed recently.  ]+zCrLf;
*!*	    +[Inventory usage and functions may be inaccurate until   ]+zCrLf;
*!*	    +[an inventory report is run. This is required one time   ]+zCrLf;
*!*	    +[after a system update is performed.   ]+zCrCr;
*!*	    +[Run the report  from  Reports > Inventory > 1 ])
*!*	ENDIF

*!*	IF FSIZE("lastupdate")>1 .and. !EMPT(jCver) .and. !EMPT(configav.lastupdate).and.!FILE("c:\ThisFileOnlyOn.CTI")
*!*	  CurrVer = LEFT(ALLT(configav.lastupdate),5)
*!*	  nVer=VAL(LEFT(jCver ,4))
*!*	  nCur=VAL(LEFT(configav.lastupdate,4))
*!*	  cVer=SUBS(jCver,5,1)
*!*	  cCur=SUBS(configav.lastupdate,5,1)
*!*	  IF jCver # CurrVer  &&nVer < nCur .or. (nVer=nCur.and.cVer<cCur)
*!*	    wtd=MESSAGEBOX('Your SPN 2000 program version is   '+jCver+;
*!*	      '   yet the System data file shows a version   '+LEFT(ALLT(configav.lastupdate),5)+[ ]+CHR(13)+CHR(13)+;
*!*	      'You should consider updating to version   '+LEFT(ALLT(configav.lastupdate),5)+'  so they match.',0+16,[ VERSION CONFLICT ADVISORY])
*!*	  ENDIF
*!*	ENDIF

IF FILE("C:\NACVFP\WIN10.TST")
  *WAIT [WIN 10 TEST 2a  ]+SET("PATH") WIND TIME 5
ENDIF

*WAIT [4] WIND
USE IN configav
zzzCapsLock = CAPSLOCK()
IF 111=222 && xxhmt < 30
*!*	  CAPSLOCK(.T.)
*!*	  zFontTxt1 = [SPN has detected a missing Windows System Font called... ]+CHR(13)+CHR(13)
*!*	  zFontTxt2 = CHR(13)+CHR(13)+[SPN can still be used but some functionality may be lost.  ]+CHR(13)+CHR(13)
*!*	  =AFONT(ZFONTS)  && THESE FONT NAMES ARE CASE SENSITIVE !
*!*	  zFontTxt3 = [See your System Administrator about installing fonts. ]+CHR(13)
*!*	  zFontTxt4 = [] &&[It is also in your hard drive's  ]+ALLT(zzStartDir)+[  folder.  ]+CHR(13)
*!*	  zFontTxt5 = {} &&[Instructions for installation via your Control Panel are in the FONTS.TXT file  ]+CHR(13)+CHR(13);
*!*	    ***+[Quit SPN2000 before for installing.]
*!*	IF FILE("C:\NACVFP\WIN10.TST")
*!*	  *WAIT [WIN 10 TEST 2b  ]+SET("PATH") WIND TIME 5
*!*	ENDIF
*!*	  IF 0=ASCAN(ZFONTS,"3 of 9 Barcode")
*!*	    zf4 = IIF(FILE(zzStartDir+[\3OF9.TTF]),zFontTxt4,[])
*!*	    = MESSAGEBOX(zFontTxt1+'      3 of 9 BARCODE'+zFontTxt2+zFontTxt3+zf4;
*!*	      +zFontTxt5,0+48,[SPN2000 FONT MISSING])
*!*	  ENDIF
*!*	IF FILE("C:\NACVFP\WIN10.TST")
*!*	  *WAIT [WIN 10 TEST 2c  ]+SET("PATH") WIND TIME 5
*!*	ENDIF
*!*	  IF 0=ASCAN(ZFONTS,"Lucida Console")
*!*	    zf4 = IIF(FILE(zzStartDir+[\LUCON.TTF]),zFontTxt4,[])
*!*	    = MESSAGEBOX(zFontTxt1+'      LUCIDA CONSOLE'+zFontTxt2+zFontTxt3+zf4;
*!*	      +zFontTxt5,0+48,[SPN2000 FONT MISSING])
*!*	  ENDIF
*!*	IF FILE("C:\NACVFP\WIN10.TST")
*!*	  *WAIT [WIN 10 TEST 2d  ]+SET("PATH") WIND TIME 5
*!*	ENDIF
*!*	  IF 0=ASCAN(ZFONTS,"Arial")
*!*	    zf4 = IIF(FILE(zzStartDir+[\ARIAL.TTF]),zFontTxt4,[])
*!*	    = MESSAGEBOX(zFontTxt1+'      ARIAL'+zFontTxt2+zFontTxt3+zf4;
*!*	      +zFontTxt5,0+48,[SPN2000 FONT MISSING])
*!*	  ENDIF
*!*	IF FILE("C:\NACVFP\WIN10.TST")
*!*	  *WAIT [WIN 10 TEST 2e  ]+SET("PATH") WIND TIME 5
*!*	ENDIF
*!*	  IF 0=ASCAN(ZFONTS,"Monotype Sorts")
*!*	    zf4 = IIF(FILE(zzStartDir+[\MTSORTS.TTF]),zFontTxt4,[])
*!*	    = MESSAGEBOX(zFontTxt1+'      MONOTYPE SORTS'+zFontTxt2+zFontTxt3+zf4;
*!*	      +zFontTxt5,0+48,[SPN2000 FONT MISSING])
*!*	  ENDIF
*!*	IF FILE("C:\NACVFP\WIN10.TST")
*!*	  *WAIT [WIN 10 TEST 2f  ]+SET("PATH") WIND TIME 5
*!*	ENDIF
*!*	  RELE zFontTxt1,zFontTxt2,zFontTxt3,zFontTxt4,zFontTxt5,ZFONTS, zFontMessage
*!*	  =CAPSLOCK(.F.)
*!*	IF FILE("C:\NACVFP\WIN10.TST")
*!*	  *WAIT [WIN 10 TEST 2g  ]+SET("PATH") WIND TIME 5
*!*	ENDIF
ENDIF

IF FILE("C:\NACVFP\WIN10.TST")
  *WAIT [WIN 10 TEST 3  ]+SET("PATH") WIND TIME 5
ENDIF

*\\ recurse path and test for duplicate files
*CREATE CURSOR zpaths ( drive c(2), fullpath c(50), semis n(2), datadirs n(2), colons n(2) )
*zSeco=SECO()
*!*	CREATE CURSOR zpaths ( drive c(2), fullpath c(50))
*!*	PRIV nDatDirs, nSemis, nColons, zCFGAV, ZXDRIVES
*!*	nDat1    = OCCUR([\DATA\],sgcpath)  && only want to count data dirs at end of paths
*!*	nDat2    = OCCUR([\DATA],sgcpath)   && don't want to count data dirs embeded
*!*	nDatDirs = nDat2-nDat1
*!*	*nSemis   = OCCUR([;],sgcpath)
*!*	nColons  = OCCUR([:],sgcpath)
*!*	FOR jjj = 1 TO nColons
*!*	  DO CASE
*!*	    CASE jjj < ncolons
*!*	      zStart = AT([:],sgcpath,jjj) - 1
*!*	      zStop  = AT([:],sgcpath,jjj + 1) - 2
*!*	    CASE jjj = ncolons
*!*	      zStart = AT([:],sgcpath,jjj ) - 1
*!*	      zStop  = LEN(ALLT(sgcpath)) + 1
*!*	  ENDCASE
*!*	  APPE BLANK
*!*	  *REPLA semis WITH nsemis, datadirs WITH ndatdirs, colons WITH ncolons
*!*	  REPLA drive WITH SUBS(sgcpath,zstart,2)
*!*	  REPLA fullpath WITH SUBS(sgcpath,zstart,zstop-zstart)
*!*	ENDFOR
*!*	SELE zpaths
*!*	STORE 0 TO zCFGAV,zNdrives
*!*	STORE [ ] TO zXDRIVES
*!*	SCAN
*!*	  IF !zpaths.drive $ zXDRIVES
*!*	    znDrives = zndrives + 1
*!*	    zXDRIVES = zXDRIVES + zpaths.drive+[ ]
*!*	  ENDIF
*!*	  IF FILE(zpaths.fullpath+[\configav.dbf])
*!*	    zCFGAV = zCFGAV + 1
*!*	  ENDIF
*!*	ENDSCAN
*!*	**// NOTE:  warn if   1)  More than 1 configav   2) more than 1 DATA dir per drive letter
*!*	IF nDatDirs # zNdrives .or. zcfgav > 1
*!*	  = MESSAGEBOX('There may be a problem with your data files ! '+CHR(13)+CHR(13);
*!*	    +[You should NOT continue using SPN2000 until you contact CTI ! ]+CHR(13)+CHR(13);
*!*	    +[Data Directories = ]+ALLT(STR(nDatDirs,3))+[    Drives = ]+zXDrives;
*!*	    +IIF(zCFGAV>1,[    Duplicate files = ]+ALLT(STR(zCFGAV,3)),[]),[SPN2000 PROBLEM])
*!*	ENDIF
*!*	*ztime = ALLT(STR(ABS(zseco-SECO()),6,3))
*!*	*ztitle = ztime+[   ]+STR(nDatDirs,3)+[ Data Dirs   ]+zXDrives+[   ]+STR(zCFGAV,3)+[ Configav       INSPECTING  PATH  INFO      ESC:DONE]
*!*	*BROW FIELDS drive:6:H=[Drive],fullpath:50:H=[Path] TITLE ztitle
*!*	USE IN zpaths
*!*	RELE nDatDirs, nSemis, nColons, zCFGAV, ZXDRIVES, zndrives

* Save the SET ESCAPE setting for use below.
slcEscape = SET('ESCAPE')

* Create Control Tools array sgaTools.  Issue ON ERROR command
* and set Help on or off.
sllErrSet = EVAL(sgcFCTA+"()")

* Define function keys.
PUSH KEY
ON KEY LABEL F1 do &sgcFCT with 'HELP_F1'
ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
ON KEY LABEL F4 *
ON KEY LABEL F5 *
ON KEY LABEL F6 *
ON KEY LABEL F7 *
ON KEY LABEL F8 *
ON KEY LABEL F9 *
ON KEY LABEL F10 *
ON KEY LABEL F11 Do HardCopy WITH "SPN2000"
ON KEY LABEL F12 *

* Always display the logo and copyright notice when the user
* initially enters the application.  Only display for 4 seconds
* if the user has turned off the display of the logo on the
* Environment window.

DO (sgcULgo)
IF NOT sglLogo
  SET CURSOR OFF
  WAIT '' TIMEOUT 1
  CLEAR
  * added balance of this if/endif per VPM fix   06201998
  =RemoveLogo()
  SET CURSOR ON
ENDIF
* If this application is being run as an executable (.EXE),
* the FOXTOOLS.FLL file should reside in the application's
* home directory.  If this application is being run as an
* application (.APP), the FOXTOOLS.FLL file can reside in
* the FoxPro root directory.
IF FILE('foxtools.fll')
  slcFoxTools = FULLPATH('foxtools.fll')
  SET LIBRARY TO (slcFoxTools) ADDITIVE
ELSE
  IF FILE(SYS(2004)+'foxtools.fll')
    slcFoxTools = SYS(2004)+'foxtools.fll'
    SET LIBRARY TO (slcFoxTools) ADDITIVE
  ENDIF
ENDIF
SET PROCEDURE TO davmprcs
* Instantiate the Security object.
SET CLASSLIB TO ProSec ADDITIVE
SET CLASSLIB TO VPMSec ADDITIVE
PUBLIC oSec
oSec = CREATEOBJECT('ProSec')
IF TYPE('oSec') <> 'O'
  = siExitApp()
  RETURN
ENDIF
ON KEY LABEL F12 *
* Run the Login form.  This section of code can be placed after
* the introductory form code below if the introductory form
* should be displayed prior to the Login form.
slnLogOnStatus = oSec.UserLogOn()
*WAIT STR(slnLogOnStatus ) WIND
IF slnLogOnStatus > 0
  DO CASE
    CASE slnLogOnStatus = 1
      = MESSAGEBOX('User Log-on error.  Unable to open the '+;
        'table S'+sgcAppPre+'FAPD.APD.',MB_ICONEXCLAMATION+MB_OK,;
        sgcAppTitle)
    CASE slnLogOnStatus = 2
      = MESSAGEBOX('User Log-on error.  Unable to open the '+;
        'table '+sgcUserDBF+'.',MB_ICONEXCLAMATION+MB_OK,;
        sgcAppTitle)
    CASE slnLogOnStatus = 3
      = MESSAGEBOX('User Log-on error.  There are no records in '+;
        'User table.',MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
    CASE slnLogOnStatus = 4
      = MESSAGEBOX('User Log-on error.  Check sum validation '+;
        'error.',MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
    CASE slnLogOnStatus = 5
      = MESSAGEBOX('You have exhausted all attempts to enter a '+;
        'valid User ID and Password.',MB_ICONEXCLAMATION+MB_OK,;
        sgcAppTitle)
    CASE slnLogOnStatus = 6
      * The user selected "cancel" on the Login form or did not
      * enter a new password.  There is no need to display a message.
  ENDCASE
  = siExitApp()
  RETURN
ENDIF

* Load the Drive Swapping array.
= EVAL(sgcFDS2+"(2)")

* Run the introductory form.
IF FILE("C:\NACVFP\WIN10.TST")
  *WAIT [WIN 10 TEST 4  ]+SET("PATH") WIND TIME 5
ENDIF

IF NOT EMPTY(sgcSInt)
  slcBuffer = ' '+CHR(0)
  sllShowIntro = .T.
  IF GetPrivStr('Defaults','ShowIntroForm','',@slcBuffer,;
      LEN(slcBuffer),sgcPjHome+'\main'+ALLT(oSec.cCurrentUserId)+'.ini') > 0
    slcOnError = ON('ERROR')
    sllError = .F.
    ON ERROR sllError = .T.
    slnShowIntro = VAL(slcBuffer)
    ON ERROR &slcOnError
    IF NOT sllError AND slnShowIntro <> 1
      sllShowIntro = .F.
    ENDIF
  ENDIF
  IF sllShowIntro
    * Run the introductory form.
    DO FORM (sgcSInt) TO sllExit
    IF sllExit
      = siExitApp()
      RETURN
    ENDIF
  ENDIF
ENDIF

*WAIT [5] WIND
IF EMPTY(oSec.cCurrentUserId)
  * This user will be a "default" user with all rights.
  ON KEY LABEL F1 do &sgcFCT with 'HELP_F3','F3',.T.
  ON KEY LABEL Alt+F3 do &sgcFCT with 'HELP_F3','AF3',.T.
  RELE _pUserId
  PUBLIC _pUserId
  zpuse = ALLT(STRTR(SYS(0),[#],[]))  && machine/net name
  IF LEN(zpuse)>3
    _pUserId = zpuse
  ELSE
    _pUserId = [Unknown]
  ENDIF
ELSE
  _pSecIsOn = .T.
  _pUserId  = oSec.cCurrentUserId
ENDIF
= EVAL(sgcFCT+"('ACTIVITY','Application Entered',sgcAppTitle)")

IF FILE("C:\NACVFP\WIN10.TST")
  *WAIT [WIN 10 TEST 5  ]+SET("PATH") WIND TIME 5
ENDIF
sglErrQuit = .F.
IF NOT sllErrSet
  * If there isn't supposed to be an active error handler,
  * turn off the error handler that was run to catch errors
  * that occur prior to the user gaining access to the
  * application.
  ON ERROR
ENDIF
IF !EMPT(oSec.cCurrentUserId)
  SELE 0
  USE seclog
  LOCATE FOR ALLT(UPPE(userid)) == ALLT(UPPE(oSec.cCurrentUserId))
  IF !FOUND()
    APPEND BLANK
    REPL seclog.userid WITH UPPE(oSec.cCurrentUserId)
  ENDIF
  REPL lastdate WITH DATE(), lasttime WITH TIME()
  USE IN seclog
ENDIF

IF FILE("C:\NACVFP\WIN10.TST")
  *WAIT [WIN 10 TEST 6  ]+SET("PATH") WIND TIME 5
ENDIF
**&CAPSLOCK(.T.)

ON KEY LABEL F1 do &sgcFCT with 'HELP_F3','F3',.T.
**ON KEY LABEL F10 DO phraser2 WITH [] && IN   &&davmprcs &&WITH PROG()
**************************************************************************************
**************************************************************************************
*  TangoWare extras
************************

*!*	=CopyUtilityFiles() && davmprcs && attempts to copy & move files from startup dir to data directory

*!*	IF FILE("C:\ThisFileOnlyOn.XXX").and.!FILE("C:\ThisFileOnlyOn.CTI")
*!*	  RENA C:\ThisFileOnlyOn.XXX TO C:\ThisFileOnlyOn.CTI
*!*	ENDIF
IF FILE("C:\THISFILEONLYON.CTI")
  SET PRINT FONT [Lucida Console], 8
  _screen.fontname = "Lucida Console"
  _screen.fontsize = 9
ENDIF
ON KEY LABEL F3 KEYB "{Ctrl+F4}"
SET TALK OFF
zCurrAlias=ALIAS()
RELEASE zCrCr,zCrLf
PUBLIC psysserial,pcompname,pcrs_name,zzAppStatus,zzKeyCode,zzEvalRemain,zzInstalled,zzCommTab,_pAVMPrinter
PUBLIC CrCr,zCrCr,pexpiredat,zGenQueryRetVal,zVpmRightClickOk,zCrLf
PUBLIC _pBrowFont,zzStat1,zzstat2,zzstat3,zzstatM,zzstatS,p__time,p__pageno
PUBLIC _zL1Remain,_zL2Remain,_zL3Remain,_zSRemain,_zMRemain,_zRecoverLC,_zRecoverAV
*// keep pquery and plastqry so that Query Builder remembers last query data
PUBLIC _pqueryDat,_pLastQryDbf,_zpcopies,_pEmpTasks,_p12Clock,_pNoClockSec
DIME _pqueryDat(3,6)  && store query builder parameters
STORE SPAC(0) TO _pqueryDat
STORE .F. TO zVpmRightClickOk &&// forces vpm ctrls not to use the VPM rightclick menus / referenced in the VPMCTLS
STORE [^%*^&] TO _pLastQryDbf && remember last table queried
STORE ['Lucida Console', 8] TO _pBrowFont   && popup browses  ALSO in    VPMFORMS form_toolbar activate
CrCr  = CHR(13)+CHR(13)
zCrCr = CHR(13)+CHR(13)
zCrLf = CHR(13)+CHR(10)
SET NEAR ON
STORE 1         TO _PCopies && VFP mvar
STORE 0         TO zzEvalRemain,_zL1Remain,_zL2Remain,_zL3Remain,_zSRemain,_zMRemain,p__time,p__pageno
STORE 9         TO _zRecoverLC,_zRecoverAV
STORE [UnKnown] TO zzAppStatus,zzStat1,zzstat2,zzstat3,zzstatM,zzstatS
STORE [0000]    TO psysserial

*WAIT [6] WIND
ERASE zzStartDir+[\DATA\RECOVREP.TXT]
IF FILE(zzStartDir+[\DATA\RECOVREP.TXT])
  RENAME zzStartDir+[\DATA\RECOVREP.TXT] TO zzStartDir+[\DATA\RECOVREP.PRV]
ENDIF
**WAIT [2    ]+SET('PATH') WIND
**WAIT [2    ]+FULLPATH("configlc.dbf") WIND
_zRecoverLC=RECOVER("configlc.dbf",[],[TTFFTTTTTTTTTTTTTFFTT]) && no user msgs
IF FILE(zzStartDir+[\DATA\RECOVREP.TXT])
  ERASE zzStartDir+[\DATA\RECOVREP.TXT]
ENDIF
IF FILE(zzStartDir+[\DATA\RECOVREP.PRV])
  RENAME zzStartDir+[\DATA\RECOVREP.PRV] TO zzStartDir+[\DATA\RECOVREP.TXT]
ENDIF

ERASE zzStartDir+[\DATA\TMPRINV.DBF] && recurr invoices temp file
ERASE zzStartDir+[\DATA\TMPRINV.FPT] && recurr invoices temp file

**sgcpath = [C:\NACVFP;]+sgcpath &&F:\NACFOX;;C:\NACVFP\TEMP
SET SAFETY OFF
IF !FILE("F:\nacfox\confmirq.dbf")
  CREATE DBF F:\nacfox\confmirq (fincre_r1 N(10))
  USE IN confmirq
ENDIF
IF !FILE("C:\nacvfp\docassociations.dbf")
  CREATE DBF c:\nacvfp\docassociations ( cext c(5), apploc c(254), cmisc01 C(254), cmisc02 C(254), lflag01 L(1), dcreate T(8) )
  USE IN docassociations
ENDIF
IF !FILE("C:\nacvfp\csmloccfg.dbf")
  CREATE DBF c:\nacvfp\csmloccfg (lastrma N(10), lastpa N(10), lastca N(10), lastcf N(10), lastscar N(10), last02 N(10), logic01 L(1), text1 C(200), lastav N(10))
  APPEND BLANK IN csmloccfg
  USE IN csmloccfg
ELSE
  SELE 0
  USE C:\nacvfp\csmloccfg.dbf EXCL
  IF FSIZE('lastav')<1
    ALTER TABLE csmloccfg ADD COLUMN lastav C(10)
  ENDIF
  USE IN csmloccfg
ENDIF

ERASE C:\nacvfp\docstemp\*.*  &&28Mar14 per GuyN so none hang around, users would need to use the docs copy function again in INVT FERRET or JOB MANAGEMENT

IF !FILE("C:\nacvfp\n2o_cfg")
  CREATE DBF c:\nacvfp\n2o_cfg ( clots c(254), cassemblys c(254), ccustcodes c(254);
    , lthr L(1), lcab L(1), lsmt L(1), lem L(1), lastmfg c(25), dstop D(8) )
  SELE n2o_cfg
  APPE BLANK
  REPL dstop WITH DATE()+180, lcab WITH .T., lsmt WITH .T., lem WITH .T., lthr WITH .T. IN n2o_cfg
  USE IN n2o_cfg
ENDIF
IF !FILE("C:\nacvfp\temp\n2o_last.dbf")
  CREATE DBF c:\nacvfp\temp\n2o_last ( lastdate D(8), lasttime C(8))
  SELE n2o_last
  APPE BLANK
  USE IN n2o_last
ENDIF
IF 1=1 &&!FILE("C:\nacvfp\tmpinvt3")
  CREATE DBF c:\nacvfp\temp\tmpinvt3 ( mfg c(25), loc c(10);
    , onhand N(9,2), avgcost N(10,4), extended N(12,2), desc c(30), misc C(30))
  SELE tmpinvt3
  *APPE BLANK
  *REPL mfg WITH [*******************] IN tmpinvt2
  COPY TO c:\nacvfp\temp\tmpinvt2
  USE IN tmpinvt3
ENDIF
IF !FILE("F:\NACFOX\DOCS_CFG.DBF")
  CREATE DBF F:\NACFOX\DOCS_CFG (primaryloc C(100) )
  SELE docs_cfg
  APPE BLANK
  REPL primaryloc WITH [F:\DRAWINGS\] IN docs_cfg
  USE IN docs_cfg
ENDIF
IF 1=1 && NEED To ORDER   !FILE("C:\nacvfp\temp\shorts.dbf").or. !FILE("C:\nacvfp\temp\shortdet.dbf")
  *#############################   CAREFUL CHANGING STRUCTURES HERE, COULD MESS UP N2O MODULE ##############################
  SET TALK OFF
  SET SAFETY OFF  && _START_SPN_3.EXE
SET SAFETY OFF
IF FILE("F:\NACFOX\_START_SPN_3.EXE") .and.!FILE("C:\NACVFP\_START_SPN_3.EXE")
  *// new start exe
  COPY FILE F:\NACFOX\_START_SPN_3.EXE TO C:\NACVFP\_START_SPN_3.EXE
  IF .T. && delete orig desktop icon _START_SPN.EXE
    Shell = CreateObject("WScript.Shell") 
    FSO = CreateObject("Scripting.FileSystemObject") 
    DesktopPath = Shell.SpecialFolders("Desktop") 
    *WAIT desktoppath+[\SPN2000.*] WIND TIME 0.3
    ERASE (desktoppath+[\SPN2000.*]) && no error mssg if not there
    WAIT [DELETE DESKTOP ICON for ]+desktoppath+[\SPN2000.*] WIND TIME 1
    ERASE (desktoppath+[\SPN2000.EXE])
    ERASE (desktoppath+[\SPN2000.EXE])
    *Shell.SendKeys "{F5}"
    WAIT desktoppath+[\SPN2000.*] WIND TIME 1
  ENDIF

  IF .T.
    oWsh=CREATEOBJECT("wscript.shell")
    IF TYPE("oWsh")=[O]
        cDeskPath = oWsh.SpecialFolders("desktop")
        Oshort    = oWsh.CreateShortCut(cDeskPath+"\SPN2000-3.LNK")
        Oshort.TargetPath  = "C:\NACVFP\_START_SPN_3.EXE"
        Oshort.Description = "New ShortCut SPN2000-3 07Jan2019"
        Oshort.WorkingDirectory = "C:\NACVFP\"  &&cdeskPath+"\"
        Oshort.Save
     *     oWsh.SendKeys "{F5}"
    ENDIF
    ***  Description is really the comment
    IF TYPE("oWsh")=[O]
      RELEASE oWsh
    ENDIF
  ENDIF
ENDIF
IF DATE() > {01/09/2019} .and. FILE("C:\NACVFP\_START_SPN.EXE").and.FILE("C:\NACVFP\_START_SPN_3.EXE")
  ERASE C:\NACVFP\_START_SPN.EXE
ENDIF
IF FILE("F:\NACFOX\wsc16.dll") &&.and.!FILE("C:\NACVFP\zlib.dll")
  COPY FILE F:\NACFOX\wsc16.dll TO C:\NACVFP\wsc16.dll
ENDIF
IF FILE("F:\NACFOX\wsc32.dll") &&.and.!FILE("C:\NACVFP\zlib.dll")
  COPY FILE F:\NACFOX\wsc32.dll TO C:\NACVFP\wsc32.dll
ENDIF
IF FILE("F:\NACFOX\wsc32con.fox") &&.and.!FILE("C:\NACVFP\zlib.dll")
  COPY FILE F:\NACFOX\wsc32con.fox TO C:\NACVFP\wsc32con.fox
ENDIF
IF FILE("F:\NACFOX\wsc32fun.fox") &&.and.!FILE("C:\NACVFP\zlib.dll")
  COPY FILE F:\NACFOX\wsc32fun.fox TO C:\NACVFP\wsc32fun.fox
ENDIF
IF FILE("F:\NACFOX\zlib.dll").and.!FILE("C:\NACVFP\zlib.dll")
  COPY FILE F:\NACFOX\zlib.dll TO C:\NACVFP\zlib.dll
ENDIF
IF FILE("F:\NACFOX\zlib1.dll").and.!FILE("C:\NACVFP\zlib1.dll")
  COPY FILE F:\NACFOX\zlib1.dll TO C:\NACVFP\zlib1.dll
ENDIF
IF FILE("F:\NACFOX\xym16.dll").and.!FILE("C:\NACVFP\xym16.dll")
  COPY FILE F:\NACFOX\xym16.dll TO C:\NACVFP\xym16.dll
ENDIF
IF FILE("F:\NACFOX\xym32.dll").and.!FILE("C:\NACVFP\xym32.dll")
  COPY FILE F:\NACFOX\xym32.dll TO C:\NACVFP\xym32.dll
ENDIF
IF FILE("F:\NACFOX\xfrxlib.fll").and.!FILE("C:\NACVFP\xfrxlib.dll")
  COPY FILE F:\NACFOX\xfrxlib.fll TO C:\NACVFP\xfrxlib.fll
ENDIF
IF FILE("F:\NACFOX\xfrx.fxp").and.!FILE("C:\NACVFP\xfrx.fxp") &&hndlib.dll
  COPY FILE F:\NACFOX\xfrx.fxp TO C:\NACVFP\xfrx.fxp
ENDIF
IF FILE("F:\NACFOX\hndlib.dll").and.!FILE("C:\NACVFP\hndlib.dll") &&
  COPY FILE F:\NACFOX\hndlib.dll TO C:\NACVFP\hndlib.dll
ENDIF
  IF !FILE("c:\nacvfp\TEMP\tshortdet.dbf")
    CREATE DBF c:\nacvfp\TEMP\tshortdet (LOT C(6) ,PART C(19) ,MFG C(25) ,QTY N(14,2) ,KIT_DATE D(8) ,ONORDER N(14,2) ,;
    LOT_CNT N(3) ,KITTED N(14,2) ,QUOTE C(6) ,ON_HAND N(14,2) ,AVAILABLE N(14,2) ,TWO_LOT C(6) ,;
    TWO_KIT D(8) ,CAE C(25) ,CUSTOMER C(10) ,CAE_OH N(14,2) ,CAE_OO N(14,2) ,PO_OO N(14,2) ,;
    CAE_PO C(5) ,CAE_DUE D(8) ,ONHAND N(14,2) ,ON_ORDER N(14,2) ,CREATE_PO C(1) ,PO_CREATED L(1) ,;
    VCODE C(10) ,CAES L(1) ,UCOST N(14,4) ,DESIREDDAT D(8) ,DESIREDQTY N(14,3) ,DESIREDCST N(14,4) ,;
    LEXTRA L(1) ,LEADFREE C(1) ,ROHS C(1) ,UL C(1) ,CSA C(1) ,CEC C(1) ,;
    BUYAM C(1) ,LINCLUDE L(1) ,LINCL_CAES L(1) ,RFQNUM N(10) ,LCAEEXTRA L(1) ,TRBL_QTY N(14,2) ,;
    TROUBLES M(4) ,NEED_ALL N(14,2) ,NEXTDUE D(8) ,LAMORT L(1) )
  ENDIF
  IF !FILE("c:\nacvfp\TEMP\shortdet.dbf")
    CREATE DBF c:\nacvfp\TEMP\shortdet;
     ( LOT C(6) ,PART C(19) ,MFG C(25) ,QTY N(12,2) ,KIT_DATE D(8) ,ONORDER N(14,2) ,;
       LOT_CNT N(3) ,KITTED N(14,2) ,QUOTE C(6) ,ON_HAND N(14,2) ,AVAILABLE N(14,2) ,TWO_LOT C(6) ,;
       TWO_KIT D(8) ,CAE C(25) ,CUSTOMER C(10) ,CAE_OH N(14,2) ,CAE_OO N(12) ,PO_OO N(14,2) ,;
       CAE_PO C(5) ,CAE_DUE D(8) ,ONHAND N(14,2) ,ON_ORDER N(14,2) ,CREATE_PO C(1) ,PO_CREATED L(1) ,;
       VCODE C(10) ,CAES L(1) ,UCOST N(14,4) ,DESIREDDAT D(8) ,DESIREDQTY N(14,3) ,DESIREDCST N(14,4) ,;
       LEXTRA L(1) ,LEADFREE C(1) ,ROHS C(1) ,UL C(1) ,CSA C(1) ,CEC C(1) ,;
       BUYAM C(1) ,LINCLUDE L(1) ,LINCL_CAES L(1) ,RFQNUM N(10) ,LCAEEXTRA L(1) ,LAMORT L(1) ,;
       MCODE C(10)  )
      SELE shortdet
      INDEX ON mfg+DTOC(kit_date,1) TAG mfg
      INDEX ON lot TAG lot
  ENDIF
  
  IF !FILE("c:\nacvfp\TEMP\shorts.dbf")
    SELE linclude,MFG,rohs,QUOTE,part,UCOST,VAL(TRAN(SUM(shortdet.QTY),'9999999.99')) AS totneed,000000000.00 AS net_need,;
      MIN(KIT_DATE) AS need_date, LOT,desiredqty,desiredcst,0000000.00 AS tot_oo,0000000.00 AS tot_oh,000000.00 AS pq_need, .F. AS lfrompq,;
      CREATE_PO, PO_CREATED, VCODE,MCODE, CAES,CAE, CUSTOMER,lincl_caes,;
      CAE_OH, CAE_OO,PO_OO, VCODE AS Session, LEFT(VCODE,5) AS ponum,rfqnum;
      FROM c:\nacvfp\TEMP\shortdet GROUP BY shortdet.MFG WHERE linclude INTO DBF c:\nacvfp\TEMP\shorts
      SELE shorts
  ENDIF
  IF !FILE("c:\nacvfp\TEMP\shortsexcl.dbf")
    SELE linclude,MFG,rohs,QUOTE,part,UCOST,VAL(TRAN(SUM(shortdet.QTY),'9999999.99')) AS totneed,000000000.00 AS net_need,;
      MIN(KIT_DATE) AS need_date, LOT,desiredqty,desiredcst,0000000.00 AS tot_oo,0000000.00 AS tot_oh, SPAC(254) AS reason,;
      CREATE_PO, PO_CREATED, VCODE,MCODE, CAES,CAE, CUSTOMER,lincl_caes,;
      CAE_OH, CAE_OO,PO_OO, VCODE AS Session, LEFT(VCODE,5) AS ponum,rfqnum;
      FROM c:\nacvfp\TEMP\shortdet GROUP BY shortdet.MFG WHERE linclude INTO DBF c:\nacvfp\TEMP\shortsexcl
      SELE shortsexcl
      *INDEX ON mfg                 FOR !EMPT(need_date) .and. !EMPT(mfg) TAG mfg
      *INDEX ON lot+mfg             FOR !EMPT(need_date) .and. !EMPT(mfg) TAG lot_mfg
      *INDEX ON DTOS(need_date)+mfg FOR !EMPT(need_date) .and. !EMPT(mfg) TAG kit_mfg
  ENDIF
  IF USED("shorts")
    USE IN shorts
  ENDIF
  IF USED("shortsexcl")
    USE IN shortsexcl
  ENDIF
  IF USED("shortdet")
    USE IN shortdet
  ENDIF
ENDIF
SET SAFETY OFF
IF 1=1 &&!FILE("c:\nacvfp\temp\tmpbom_b.dbf")
  SELE 0
  USE F:\NACFOX\main_b
  COPY STRU TO c:\nacvfp\temp\tmpbom_b
  USE IN main_b
  SELE 0
  USE F:\NACFOX\main_m
  COPY STRU TO c:\nacvfp\temp\tmpbom_m
  USE IN main_m
ENDIF
IF LEFT(zzStartDir,3)=="C:\" .and.!DIRECTORY(zzStartDir+"\TEMP")
  MD zzStartDir+"\TEMP"
ENDIF
IF LEFT(zzStartDir,3)=="C:\" .and.!DIRECTORY(zzStartDir+"\TEMP2")
  MD zzStartDir+"\TEMP2"
ENDIF
IF LEFT(zzStartDir,3)=="C:\" .and.!DIRECTORY(zzStartDir+"\TEMP3")
  MD zzStartDir+"\TEMP3"
ENDIF
IF LEFT(zzStartDir,3)=="C:\" .and.!DIRECTORY(zzStartDir+"\TEMP3\BACKUP")
  MD zzStartDir+"\TEMP3\BACKUP"
ENDIF
IF LEFT(zzStartDir,3)=="C:\" .and.!DIRECTORY(zzStartDir+"\TEMP4")
  MD zzStartDir+"\TEMP4"
ENDIF
IF LEFT(zzStartDir,3)=="C:\" .and.!DIRECTORY(zzStartDir+"\TEMP4\BACKUP")
  MD zzStartDir+"\TEMP4\BACKUP"
ENDIF
IF LEFT(zzStartDir,3)=="C:\" .and.!DIRECTORY(zzStartDir+"\WESTELL")
  MD zzStartDir+"\WESTELL"
ENDIF
*WAIT LEFT(sgcpath,200) WIND
*!*	IF !FILE(zzStartDir+"\TEMP\tmpmsales.dbf")
*!*	  zdbf=GenTempFile([tmpmsales])
*!*	  CREATE DBF &zDbf (name c(25), amt n(14,2), level n(1))
*!*	  =Close_Dbf("tmpmsales")
*!*	ENDIF
*!*	IF !FILE(zzStartDir+"\TEMP\tmpmpurch.dbf")
*!*	  zdbf=GenTempFile([tmpmpurch])
*!*	  CREATE DBF &zDbf (name c(25), amt n(14,2), level n(1))
*!*	  =Close_Dbf("tmpmpurch")
*!*	ENDIF
*// TEST FOR EXCLUSIVE  CONFIGAV
zOnError = ON([ERROR])
ON ERROR zzerrnum = ERROR( )
SET SAFETY OFF
zExcl=SET([EXCL])
SET EXCL ON
zzerrnum = 0
SELE 0
USE configav EXCL
IF zzerrnum = 1705  && excl error
  SET EXCL &zExcl
  ON ERROR &zOnError
ELSE                && was able to open excl
  USE IN configav   && so shut before recover
  IF FILE(zzStartDir+[\DATA\RECOVREP.TXT])
    RENAME zzStartDir+[\DATA\RECOVREP.TXT] TO zzStartDir+[\DATA\RECOVREP.PRV]
  ENDIF
  _zRecoverLC=RECOVER("configav.dbf",[],[TTFFTTTTTTTTTTTTTFFTT]) && no user msgs
  IF FILE(zzStartDir+[\DATA\RECOVREP.TXT])
    ERASE zzStartDir+[\DATA\RECOVREP.TXT]
  ENDIF
  IF FILE(zzStartDir+[\DATA\RECOVREP.PRV])
    RENAME zzStartDir+[\DATA\RECOVREP.PRV] TO zzStartDir+[\DATA\RECOVREP.TXT]
  ENDIF
ENDIF
SET EXCL &zExcl
ON ERROR &zOnError
SELE 0
=open_dbf("configav",3)  && buffered
SELE configav
IF FSIZE("clock12_24")>0
  _p12Clock    =  configav.clock12_24 # 2
ENDIF
IF FSIZE("tt_showsec")>0
  _pNoClockSec = !configav.tt_showsec
ENDIF
IF FSIZE("frepairlbl")>0 .and. EMPT(configav.frepairlbl)
  REPL configav.frepairlbl WITH [FAA Certified Repair Station #] IN configav
ENDIF
*WAIT [7] WIND
PUBLIC _zReinNote
*!*	_zReinNote=[]
*!*	IF FSIZE("lastindex") > 1  && field exists
*!*	  zhmd = ABS(configav.lastindex - DATE() )
*!*	  IF EMPT(configav.lastindex) .or. zhmd > 10 &&.and. configav.hmt > 10
*!*	    IF !EMPT(configav.lastindex)
*!*	      _zReinNote = [It has been  ]+ALLT(STR(zhmd))+[  days  since a  REINDEX & PACK  has been performed on SPN2000]
*!*	    ELSE
*!*	      _zReinNote = [A  REINDEX & PACK maintenance  has never been performed on SPN2000]
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
zEvalPeriod = 45
REPLACE configav.fmisc010 WITH IIF(configav.fmisc010<1,1,configav.fmisc010)
STORE configav.fmisc010 TO zzCommTab
IF configav.fire1={}  &&--- fresh install
  IF FILE([C:\setwin16.prq]) &&--- run already
    REPLACE configav.fire1 WITH DATE()-555-25 &&--- only 20 days if already run
    *!*	    IF FILE([C:\ThisFileOnlyOn.CTI])
    *!*	      WAIT [Marker already in place during fresh install - CTI notice only ] WIND NOWAIT
    *!*	      =INKEY(1)
    *!*	    ENDIF
  ELSE
    COPY FILE dumbtabl.dbf TO c:\setwin16.prq  &&--- marker shows already installed
    REPLACE configav.fire1 WITH DATE()-555
  ENDIF
ENDIF
IF 1=1   &&!EMPT(configav.regserial).and.!EMPT(configav.regcmpname)  &&.and.!EMPT(configav.keyl1)
  *zzKeyCode   = GKC(configav.regserial,configav.regrprname,[LEVEL 1]) && use repair name
  zzAppStatus = [REGISTERED] &&IIF( ALLT(zzKeyCode) == ALLT(configav.keyl1) , [REGISTERED], zzAppStatus)
  zzStat1 = zzAppStatus
  RELE zzKeyCode
ENDIF
IF zzAppStatus # [REGISTERED]
  zzEvalRemain = MIN(60, (configav.fire1+555)-(DATE()-zEvalPeriod) ) && ensure 60 day max
  _zL1Remain   = zzEvalRemain
  zzAppStatus  = IIF(zzEvalRemain < 0,[PAST EVALUATION],[EVALUATION ]+ALLT(STR(zzEvalRemain,2))+[ day(s) remain] )
  zzStat1      = zzAppStatus
ENDIF
pexpiredat = IIF(zzAppStatus = [REGISTERED],{},DATE()+zzEvalRemain )
REPL configav.regrprname WITH [Spinnaker Contract Manufacturing, Inc.] IN configav
REPL configav.regcmpname WITH [Spinnaker Contract Manufacturing, Inc.], configav.regserial WITH [4000] IN configav
DO CASE
  CASE zzAppStatus == [REGISTERED]
    pcrs_name = configav.regrprname
    pcompname = configav.regcmpname
    psysserial= configav.regserial
  CASE LEFT(zzAppStatus,10) == [EVALUATION] .and. ![PAST]$zzAppStatus
    pcrs_name = configav.evrprname
    pcompname = configav.evcmpname
    psysserial= [0000]
  OTHERWISE
    pcrs_name = [TangoWare SPN-2000 Evaluation Product]
    pcompname = [TangoWare SPN-2000 Evaluation Product]
    psysserial= [0000]
ENDCASE
STORE [REGISTERED] TO zzstart2,zzstat3,zzstatM,zzstatS
RELE zzER
SELE configav
REPLA configav.hmt        WITH configav.hmt + 1
IF configav.hmt< 3 .and. FILE("C:\ThisFileOnlyOn.CTI")
  =pwaitMB("ADVISORY  CTI      SPN2000 has been run   "+n_s(configav.hmt)+[   time(s)  ]+CHR(13)+CHR(13);
    +[Check the  SYS  INFO  PAGE !])
ENDIF
REPLA configav.frepairnam WITH pcrs_name
REPLA configav.fcompname  WITH pcompname
REPLA configav.fsysserial WITH psysserial
REPLA configav.prodstat   WITH zzAppStatus
REPLA configav.stat1      WITH zzStat1, configav.stat2 WITH zzstat2 ,configav.stat3 WITH zzstat3,configav.statM WITH zzstatM,configav.statS WITH zzstatS
IF zzAppStatus == [REGISTERED]
  REPLA configav.evrprname WITH pcrs_name
  REPLA configav.evcmpname WITH pcompname
ENDIF
SELE configav
AnyTEWarns = .f.
IF FSIZE("manwarnday") > 0 .and. configav.manwarnday > 0
  DO manrev_alert && procs =========
ENDIF
SELE configav
IF FSIZE("tequipwarn") > 0 .and. configav.tequipwarn > 0
  DO t_eq_alert  && test equipment warning    procs =========
ENDIF
IF !FILE("c:\nopartswarn.LL") .and. INLI(psysserial,[4131]) && schwans
  DO lowlim_warn  && parts low limit warning    procs =========
ENDIF
*!*	WAIT IIF(wopendwarn,[ON ],[OFF ])+STR(FSIZE("wopendwarn","configav"))+[  ]+LEFT(UPPE(_pUserID),6)+[  ]+ALLT(UPPE(configav.woinsplist));
*!*	  +[   ]+IIF(LEFT(UPPE(_pUserID),6)$UPPE(configav.woinsplist),[CONTAINS],[NOT CONTAINED]) WIND
SELE configav
IF FSIZE("wopendwarn") > 0 .and. configav.wopendwarn .and. !EMPT(_pUserId).and.ALLT(LEFT(UPPE(_pUserId),6))$UPPE(configav.woinsplist) && logical
  DO wopndalert  && procs =========
ENDIF

USE IN configav && ======
ON KEY LABEL F11 Do HardCopy WITH "SPN2000"

zxAlias = ALIAS()
zOnError = ON([ERROR])
ON ERROR zzerrnum = ERROR()
SET SAFETY OFF
zzerrnum = 0
IF [PAST EVALUATION] $ zzstatM  &&======= should crash if multiple users
  SELE 0
  USE mtest EXCL
  IF zzerrnum # 0  && excl
    =INKEY(3) && to show splash for a little bit
    *WAIT [Inside if  ]+STR(zzerrnum) WIND TIME 2
    =pwaitMB([Thank you for using Tangoware products.  Currently your copy of SPN]+zCrCr;
      +[is not licensed for MULTI-USER access.  Another user is in the system.]+zCrLf+zCrLf;
      +[Contact  Tangoware  for MULTI-USER License information. ])
    RETURN
  ENDIF
ELSE
  SELE 0
  USE mtest && will be using mtest for updates, updater program will use mtest exclusively to no one can get in
  IF zzerrnum # 0
    =INKEY(1) && to show splash for a little bit
    *WAIT [Inside if  ]+STR(zzerrnum) WIND TIME 2
    =pwaitMB([Thank you for using Tangoware products.  Currently your SPN software is locked]+zCrCr;
      +[for maintenance.  Try again in a while or contact your Administrator.])
    RETURN
  ENDIF
ENDIF
ON ERROR &zOnError
IF !EMPT(zxAlias)
  SELE (zxAlias)
ENDIF
SET SAFETY ON
FOR ttq = 1 TO 3
  PUBLIC _JUserFirst,_JUserLast,_JUserFull,_JUserPass,_JUserGoals,_JGoalsPass
  STORE [] TO _JUserFirst,_JUserLast,_JUserFull,_JUserPass
  STORE [&%*&Y(] TO _JGoalsPass
  STORE .F. TO kCont,_JUserGoals
  SELE 0
  USE pass
  zmess = [Good Day]
  DO CASE
    CASE BETW(SECO(),0,43199)
      zMess = [Good Morning]
    CASE BETW(SECO(),43200,63799)
      zMess = [Good Afternoon]
    OTHERWISE
      zMess = [Good Evening]
  ENDCASE
  DO CASE
    CASE ttq = 2
      nleft= -160
      ntop = 260
    CASE ttq = 3
      nleft= -200
      ntop = 360
    OTHERWISE
      nleft= 160
      ntop = 0
  ENDCASE
  DO FORM get_1_line WITH zMess+zCrLf+[Enter  your  SCMI  password  please  ],[];
    ,ntop,nleft,[ SCMI PassWord  (]+n_s(ttq)+[/3)],15,[],.T. TO _JUserPass
  _JUserPass = ALLT(_JUserPass)
  IF LEN(ALLT(_JUserPass))>1
    SELE pass
    SET ORDER TO first
    lok92=.F.
    IF INLI(ALLT(_JUserPass),[(((((],[)))))]) &&[*****]
      DO FORM get_1_line WITH zMess+zCrLf+[PASSWORD  password  please  ],[];
        ,0,160,[ SCMI PassWord  (]+n_s(ttq)+[/3)],15,[],.T. TO _JPass21
      lshow_pw = [5544] == ALLT(_jpass21)
      ON KEY LABEL ENTER KEYB CHR(23)
      BROWSE FIELD first,last,xcq3=Reverseit(Lower(pass),lshow_pw):15 NOED NOAPPE NODELE FONT 'Verdana', 8
      ON KEY LABEL ENTER *
      lok92 = .T.
      _JUserPass = pass.pass
      LOCATE FOR UPPE(ALLT(pass.pass)) == UPPE(ALLT(_JUserPass))
    ELSE
      LOCATE FOR UPPE(ALLT(pass.pass)) == UPPE(ALLT(_JUserPass))
    ENDIF
    *BROW
    IF FOUND() .or. lok92
      kCont = .T.
      _JUserFirst = ALLT(pass.first)
      _JUserLast  = ALLT(pass.last)
      _JUserFull  = ALLT(pass.first)+[ ]+ALLT(Pass.Last)
      _JUserGoals = pass.flag74
      _JGoalsPass = LEFT(pass.cflag01,10)
      =zwindon(zMess+[  ]+_JUserFull)
      =zwindoff(.8)
      c82 = [Mach: ]+STRTR(ALLT(SYS(0)),[ # ],[  User: ])+[ ]+TTOC(DATET()) && +[   IP: ]+cip23+[   ]
      REPLACE flagdet WITH ALLT(c82)+[  ]+_JUserFull+[  ]+ALLT(STR(SYSM(1)))+[-]+ALLT(STR(SYSM(2))) IN pass
    ENDIF
  ENDIF
  RELE zMess
  USE IN pass
  IF !kCont
   IF ttq = 3
      =pwaitMB([Invalid  SCMI  Password, access denied])
      =siExitApp()
      RETURN
    ELSE
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDFOR
*WAIT [A] WIND TIME 1.777
IF _JUserGoals .and.!FILE("C:\NoUserGoals.STP")
  ** _JUserPass
  WAIT [CHECKING GOAL ITEMS ...] WIND NOWAIT
  SET SAFETY OFF
  SELE 0
  SELE * FROM F:\nacfox\empgoals WHERE pass = [&@$$!!] INTO DBF C:\NACVFP\TEMP3\teg1 && shell
  SELE 0
  SELE * FROM F:\nacfox\empgoals WHERE pass = _JGoalsPass .and. EMPT(dcomplete) INTO DBF C:\NACVFP\TEMP3\teg2
  ncnt = _Tally
  SELE 0
  SELE * FROM F:\nacfox\empgoals WHERE pass = _JGoalsPass .and. !EMPT(dcomplete) INTO DBF C:\NACVFP\TEMP3\teg3
  ncnt = ncnt + _Tally
  IF USED('empgoals')
    USE IN empgoals
  ENDIF
  IF ncnt > 0
    SELE teg1
    APPEND FROM C:\NACVFP\TEMP3\teg2
    APPEND BLANK
    APPEND FROM C:\NACVFP\TEMP3\teg3
    SELE teg1
    LOCATE
    DEFINE WINDOW teq FROM 1,1 TO 25,110 SHAD CLOSE FLOAT COLOR SCHEME 23
    BROWSE FIELDS ;
      year:8:H=[YEAR],;
      brief:62:H=[GOAL  BRIEF DESCRIPTION],;
      empname:40:H=[Employee],;
      xx=IIF(EMPT(dcomplete),[],DTOC(dcomplete)):13:H=[Completed];
      WIND teq NODELETE NOMENU NOAPPEND NOEDIT TIME 120;
      TITLE [ UPCOMING GOALS        ESC:Exit] FONT "Verdana", 9
    RELE WIND teq
    USE IN teg1
    USE IN teg2
    USE IN teg3
  ENDIF
  **USE IN empgoals
ENDIF
*** should put configlc fields updater here

****USE configlc IN 0
**WAIT [3    ]+SET('PATH') WIND
**WAIT [3    ]+FULLPATH("configlc.dbf") WIND
*WAIT [B] WIND TIME 1.777
=open_dbf("configlc",3)  && buffered
SELE configlc
IF FSIZE("fmisc004") > 0 .and.ISALPHA(ALLT(configlc.fmisc004))
  REPLA configlc.fmisc004 WITH REPLICATE("1",254)
ENDIF
IF FSIZE("lallowclr") > 0 &&ISALPHA(ALLT(fmisc004))
  REPLA configlc.lallowclr WITH .F.
ENDIF
IF FSIZE("lbnknotice") > 0
  REPLA configlc.lbnknotice WITH .T.  &&& atc maint give bank notice
ENDIF
IF FSIZE("lallowseq") > 0
  REPLA configlc.lallowseq WITH .F.
ENDIF
IF FSIZE("lcogsonce") > 0
  REPLA configlc.lcogsonce WITH .F.
ENDIF
REPLACE configlc.woplu_type WITH IIF(!BETW(configlc.woplu_type,1,2),2,configlc.woplu_type)
=TABLEUPDATE(.T.)
SELE configlc
IF FSIZE("nprncopies") > 1
  _zpcopies = IIF(BETW(configlc.nprncopies,2,9),configlc.nprncopies,1)
ELSE
  _zpcopies = 1
ENDIF
IF FSIZE("lshowtutor") > 0  && field exists
  __zShowTutor=configlc.lshowtutor
ELSE
  __zShowTutor=.F.
ENDIF
*!*	IF !USED("configlc")
*!*	  SELE 0
*!*	  USE configlc
*!*	ENDIF
*WAIT [B2] WIND TIME 1.777
SELE configlc
=pSetFromAddress() &&============
zzExact=SET("EXACT")
SET EXACT ON
*// Set SPN defa printer if is a valid printer on this Computer
jjj = 0 &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& APRINTERS(zzPrnList) &&// build an array of printers
***_zWindDefPrinter = ALLT(SET('PRINTER',2))
=CtiFpReset()
*WAIT [B3] WIND TIME 1.777
*!*	IF 11=22 &&jjj > 0
*!*	  zPrnElem = ALEN(zzPrnList) / 2
*!*	  zRetPrn = [NO Windows Default Printer...]
*!*	  FOR xxx = 1 TO zPrnElem            && VFP PRN,2 returns uppercase  -  need to get proper case here
*!*	    zAprn = ALLT(zzPrnList(xxx,1))   && by scanning printers list for a match and reassign
*!*	    IF ALLT(UPPE(_zWindDefPrinter))==ALLT(UPPE(zAprn))
*!*	      zRetPrn = zAprn
*!*	      EXIT
*!*	    ENDIF
*!*	  ENDFOR
*!*	  IF ![NO Windows Default Printer...] $ zRetPrn
*!*	    _zWindDefPrinter = zRetPrn
*!*	  ENDIF
*!*	  RELEASE xxx,xRetPrn,zAprn,zPrnElem
*!*	ENDIF
*WAIT [B3a] WIND TIME 1.777
IF 11=22  &&jjj > 0 && only if at least one Windows printer installed && July.19.2000
*!*	  SELE configlc
*!*	  IF FSIZE("DEFPRINTER") > 0 && is field in table
*!*	    IF EMPT(configlc.defprinter ) .or. [No printer selected...] $ configlc.defprinter
*!*	      REPLACE configlc.defprinter WITH IIF(EMPT(_zWindDefPrinter),[No printer selected...],_zWindDefPrinter)
*!*	    ENDIF
*!*	    IF ASCAN(zzPrnList,ALLT(configlc.defprinter)) # 0  && valid if found in list  && returns 1 if null
*!*	      IF !EMPT(configlc.defprinter)
*!*	        SET PRINT TO NAME ALLT(configlc.defprinter)
*!*	        =CtiFpReset()
*!*	      ELSE
*!*	        =pwaitMB([No default printer configured,  please select one in ]+zCrCr+[ADMIN > LOCAL CONFIG > PRINTERS ])
*!*	      ENDIF
*!*	    ELSE
*!*	      REPLACE configlc.defprinter WITH IIF(EMPT(_zWindDefPrinter),[No printer selected...],_zWindDefPrinter) && don't change this because being evaluated
*!*	      *=pwaitMB([No default printer selected,  please select one in ]+zCrCr+[Admin\Configure Local\Printers ])
*!*	    ENDIF
*!*	  ENDIF
*!*	  _pAVMPrinter=UPPE(IIF([No printer ]$ALLT(configlc.defprinter),ALLT(SET('PRINTER',2)),ALLT(configlc.defprinter)))

*!*	  SELE configlc
*!*	  IF FSIZE("LBLPRNTR01") > 0
*!*	    IF EMPT(configlc.lblprntr01) .or. [No printer selected...] $ configlc.lblprntr01
*!*	      REPLACE configlc.lblprntr01 WITH IIF(EMPT(_zWindDefPrinter),[No printer selected...],_zWindDefPrinter)
*!*	    ENDIF
*!*	    IF ASCAN(zzPrnList,ALLT(configlc.lblprntr01)) # 0  && valid if found in list  && returns 1 if null
*!*	      *// stopped Jun-11-2000 - was setting default printer to the label printer
*!*	      *!*	    IF !EMPT(configlc.lblprntr01)
*!*	      *!*	      SET PRINT TO NAME ALLT(configlc.lblprntr01)
*!*	      *!*	    ELSE
*!*	      *!*	      =pwaitMB([No Label printer selected,  please select one in ]+zCrCr+[ADMIN > LOCAL CONFIG > PRINTERS ])
*!*	      *!*	    ENDIF
*!*	    ELSE
*!*	      REPLACE configlc.lblprntr01 WITH IIF(EMPT(_zWindDefPrinter),[No printer selected...],_zWindDefPrinter)  && don't change this because being evaluated
*!*	      *=pwaitMB([No default printer selected,  please select one in ]+zCrCr+[Admin\Configure Local\Printers ])
*!*	    ENDIF
*!*	  ENDIF
*!*	  RELEASE zzPrnList
ELSE
*!*	  SELE configlc
*!*	  REPLACE configlc.defprinter WITH [No printer selected...]
*!*	  REPLACE configlc.lblprntr01 WITH [No printer selected...]
*!*	  =pwaitMB("SPN2000 cannot find any installed WINDOWS printers"+zCrLf;
*!*	    +[The SPN200 operation may be unstable even if you do not plan to print.]+zCrLf+zCrLf;
*!*	    +[You should install at least one WINDOWS printer driver even if you will not be using it.]+zCrLf;
*!*	    +[New print drivers are installed under WINDOWS START > SETTINGS > PRINTERS  section])
ENDIF
*WAIT [C] WIND TIME 1.777
=CtiFpReset()
SET EXACT &zzExact
RELEASE jjj,zzExact,zEvalPeriod
SELE configlc
lneed912=FSIZE("n2olthr")<1.or.FSIZE("n2olcab")<1.or.FSIZE("n2olsmt")<1.or.FSIZE("n2olem")<1.or.FSIZE("n2olmsc")<1
=TABLEUPDATE(.T.)
USE IN configlc  &&===========
IF lneed912
  SELE 0
  USE C:\NACVFP\configlc EXCL
  IF FSIZE("n2olthr")<1
    ALTER TABLE configlc ADD COLUMN n2olthr L(1)
  ENDIF
  IF FSIZE("n2olcab")<1
    ALTER TABLE configlc ADD COLUMN n2olcab L(1)
  ENDIF
  IF FSIZE("n2olsmt")<1
    ALTER TABLE configlc ADD COLUMN n2olsmt L(1)
  ENDIF
  IF FSIZE("n2olem")<1
    ALTER TABLE configlc ADD COLUMN n2olem L(1)
  ENDIF
  IF FSIZE("n2olmsc")<1
    ALTER TABLE configlc ADD COLUMN n2olmsc L(1)
  ENDIF
  USE IN configlc
ENDIF
IF !EMPT(zCurrAlias)
  SELE (zCurrAlias)
ENDIF
RELE zSafety,zzKeyCode,zCurrAlias
DO CASE
  CASE [REGISTERED]$zzAppStatus
    zAppTitle = [    Licensee:  ]+ALLT(pcompname)
  CASE [PAST EVALUATION]$zzAppStatus
    zAppTitle = [   Expired Evaluation for:  ]+ALLT(pcompname)
  CASE [EVALUA]$zzAppStatus .and. [CONFIGURE]$pcompname
    zAppTitle = [  Evaluation Copy - not configured yet]
  CASE [EVALUA]$zzAppStatus .and. ![CONFIGURE]$pcompname
    zAppTitle = [   Evaluation copy for:  ]+ALLT(pcompname)
  OTHERWISE
    zAppTitle = [    Licensee:  ]+ALLT(pcompname)
ENDCASE
sgcAppTitle = sgcAppTitle+zAppTitle
SET CENT ON
dfdoy    = CTOD("01/01/"+SUBS(DTOS(DATE()),1,4))
dnextsat = DATE()
DO WHILE DOW(dnextsat)<7
  dnextsat = dnextsat + 1
ENDDO
IF YEAR(dnextsat)>YEAR(DATE()) && prod week is in next year
  mdc = SUBS(DTOS(dnextsat),3,2)+[01]
ELSE
  nweeks = CEIL((dnextsat-dfdoy)/7)
  mdc = SUBS(DTOS(dnextsat),3,2)+STRTR(STR(nweeks,2),[ ],[0])
ENDIF
dt = DTOS(DATE())
sgcAppTitle = sgcAppTitle +[    ]+UPPE(LEFT(CDOW(DATE()),3))+[    ]+DTOC(DATE())+[    D/C: ]+mdc
_SCREEN.Caption = sgcAppTitle
_SCREEN.Closable = .T.

***  excercize pasting for better phraser ops
*!*	_ClipText = IIF(EMPT(_cliptext),[*]+CHR(13)+CHR(10),_ClipText)
*!*	DEFINE WIND cliptest FROM -40,-40 TO -20,-20
*!*	ACTI WIND cliptest NOSHOW
*!*	KEYB "{Ctrl+V}{Ctrl+V}{Ctrl+V}"
*!*	RELE WIND cliptest
*!*	DEFINE WIND cliptest FROM -40,-40 TO -20,-20
*!*	ACTI WIND cliptest NOSHOW
*!*	KEYB "{Ctrl+V}{Ctrl+V}{Ctrl+V}"
*!*	RELE WIND cliptest
**_ClipText = []

*CAPSLOCK(zzzCapsLock)

ON KEY LABEL F12 DO FORM sysinfo  && 03Jul2003 systats1
psysver5 = LEFT(psysvers,5)
*!*	IF FILE("updatlog.dbf") .and. !FILE("NoStartLog.mss") .and. !ALLT(zzStartDir)==[C:\SPN]
*!*	  CLEAR TYPEAHEAD
*!*	  CLEAR TYPEAHEAD
*!*	  SELE 0
*!*	  USE updatlog
*!*	  IF RECCO()>0  && only if records
*!*	    GO BOTT
*!*	    jjLast    = LEFT(ALLT(updatlog.ver),5)
*!*	    IF !EMPT(jjLast)
*!*	      LOCATE FOR UPPE(LEFT(ALLT(updatlog.ver),5)) = UPPE(psysver5)
*!*	      Match  = FOUND()
*!*	      bBlank = LEN(ALLT(jjLast)) < 5
*!*	      ItsCti = FILE("C:\ThisFileOnlyOn.CTI")
*!*	      Newer  = UPPE(psysver5) # UPPE(jjLast)  &&LEFT(ALLT(psysver5),4) > LEFT(jjLast,4)
*!*	      IF !bBlank .and. ( (!Match .and. !ItsCti .and. Newer) .or. (ItsCti .and. !Match) )
*!*	        *// if last entry NOT blank and ( no match and not CTI and EXE is newer, or CTI and no match)
*!*	        jjCurr = ALLT(psysver5)
*!*	        =pwaitMB([SPN has determined that an Update may be needed.]+zCrCr;
*!*	          +[The Update log shows last Update was:   ]+jjLast+[    ]+zCrCr;
*!*	          +[The current SPN executable file  Version is:   ]+jjCurr+[     ]+zCrCr;
*!*	          +[Perform an Update or advise your System Administrator.],[ UPDATER ADVISORY  ]+IIF(ItsCti,[(CTI)],[]))
*!*	      ENDIF
*!*	    ENDIF
*!*	    RELE jjLast,jjCurr,Match,ItsCti,Newer,bBlank
*!*	  ENDIF
*!*	  USE IN updatlog
*!*	ENDIF
ldom = LastDom(DATE())
IF BETW(DATE(), ldom - 4, ldom + 4)  && only in 10 day window and if not done
  SELE 0
  USE config2
  IF FSIZE("lwarncogs")>0 .and. config2.lwarncogs .and. FILE("cogsdet.dbf")
    lWarn = .T.
    IF !EMPT(config2.cogssecids) .and. TYPE("_pUserId")#"U".and. TYPE("oSec.cCurrentUserId")#"U".and.!EMPT(oSec.cCurrentUserId)
      IF !UPPE(ALLT(oSec.cCurrentUserId)) $ UPPE(config2.cogssecids)
        lWarn = .F.
      ENDIF
    ENDIF
    IF lWarn
      SELE 0
      USE cogsdet
      LOCATE FOR cogsdet.yyyymm = LEFT(DTOS(ldom),6)
      IF FOUND().and.!EMPT(cogsdet.end_amt)
      ELSE
        =pwaitMB([ADVISORY:   ]+CMONTH(ldom)+[   ]+LEFT(DTOS(ldom),4)+[   COGS  for the Monthly P&L reporting has not been done yet.  ])
      ENDIF
      USE IN cogsdet
    ENDIF
  ENDIF
  USE IN config2
ENDIF
RELEASE ldom
*WAIT [D] WIND TIME 1.777
************************************************
* end TangoWare
************************************************
DO WHILE sgcMenu = 'AVM'
  SET PROC TO davmprcs
  *=CtiFpReset()

  IF sglNoLogo
    sglNoLogo = .F.
  ELSE
    IF sglLogo
      DO (sgcULgo)
    ENDIF
  ENDIF
  SET CLASSLIB TO ProGen ADDITIVE
  SET CLASSLIB TO ProCtrls ADDITIVE
  SET CLASSLIB TO ProForms ADDITIVE
  SET CLASSLIB TO ProApp ADDITIVE
  SET CLASSLIB TO VPMGen ADDITIVE
  SET CLASSLIB TO VPMCtrls ADDITIVE
  SET CLASSLIB TO VPMForms ADDITIVE
  SET CLASSLIB TO VPMApp ADDITIVE

  PUBLIC oApp
  oApp = CREATEOBJECT('ProApp')

  *!*	  IF TYPE("__zShowTutor")#[U] .and. __zShowTutor
  *!*	    DO FORM tutor_form WITH 10,10 &&=====   CTI
  *!*	    RELE __zShowTutor
  *!*	  ENDIF

  *!*	  IF TYPE("_zReinNote")#[U] .and. !EMPT(_zReinNote)   &&FSIZE("lastindex") > 1  && field exists
  *!*	    *zhmd = ABS(configav.lastindex - DATE() )
  *!*	    *IF EMPT(configav.lastindex) .or. zhmd > 10 &&.and. configav.hmt > 10
  *!*	    *IF !EMPT(configav.lastindex)
  *!*	    *zrt = [It has been  ]+ALLT(STR(zhmd))+[  days  since a Reindex & Pack has been performed on SPN2000]+CHR(13)+;
  *!*	    *ELSE
  *!*	    *zrt = [ or A  Reindex & Pack  has never been performed on SPN2000]
  *!*	    *ENDIF
  *!*	    =pwaitMB(_zReinNote+zCrCr;
  *!*	      +[This procedure should be periodically performed on all databases to ensure reliability]+zCrCr;
  *!*	      +[1)  Ensure no other users are in the system,  ]+zCrLf;
  *!*	      +[2)  Go to the FILE menu and run the Reindex & Pack routines]+zCrLf;
  *!*	      +[     Per the supplied documentation by CTI / TangoWare])
  *!*	    *ENDIF
  *!*	  ENDIF
  RELEASE _zReinNote
  *SET PATH TO C:\NACVFP;F:\NACFOX ADDITIVE
  IF TYPE('oApp') = 'O'
    oApp.Do()
    RELEASE oApp
  ENDIF

  * When an error occurs that requires the READ EVENTS
  * to be cleared, SET ESCAPE could be set on in the process
  * that generated the error.  Therefore, restore the initial
  * SET ESCAPE setting.
  SET ESCAPE &slcEscape
  ON KEY LABEL ESC

  IF TYPE('oSec') = 'O' AND TYPE('sgcMenu') <> 'U' AND sgcMenu <> 'AVM'
    * Make sure the user is recorded as being logged-off.
    oSec.LogOn_LogOff('OFF','Exit Application','Exit '+sgcAppTitle)
    EXIT
  ENDIF

  SET SYSMENU TO
  CLEAR
  SET CLASSLIB TO
  CLEAR MENUS
  CLEAR POPUPS
  CLEAR PROMPT
  CLEAR TYPEAHEAD
  CLEAR WINDOWS
  CLOSE ALL
  IF FILE("C:\THISFILEONLYON.CTI")
    SET PRINT FONT [Lucida Console], 8
    _screen.fontname = "Lucida Console"
    _screen.fontsize = 9
  ENDIF

ENDDO
IF !USED("tshorts").and.FILE("c:\nacvfp\temp\tshorts.dbf")
  SET SAFETY OFF
  SELE 0
  USE c:\nacvfp\temp\tshorts EXCL
  ZAP
  USE
ENDIF
= siExitApp()

RETURN

* Refox line used when branding application.
= _ANTIFROG_

******************
FUNCTION siExitApp
******************
* Exit the application.

POP KEY
POP MENU _MSYSMENU
CLEAR
SET CLASSLIB TO
CLEAR TYPEAHEAD
CLEAR WINDOWS
CLOSE ALL

* Release the Security object.
IF TYPE('oSec') = 'O'
  RELEASE oSec
ENDIF
* Remove objects added to the Application Window.
ON KEY LABEL F11 Do HardCopy WITH "SPN2000"
=RemoveLogo()

* Release the Foxtools library.
IF 'FOXTOOLS.FLL' $ SET('LIBRARY')
  RELEASE LIBRARY (slcFoxTools)
ENDIF

* If this application was entered directly from Visual ProMatrix,
* the memory variable slcMemPre will be defined.  Otherwise,
* slcMemPre will be undefined.
IF TYPE('slcMemPre') = 'U'
  * This application was not entered from Visual ProMatrix.
  * Simply exit the application.
  ON SHUTDOWN
ELSE
  * This application was entered from Visual ProMatrix.
  * Exit the application and return to Visual ProMatrix.
  WAIT WINDOW NOWAIT 'Returning to Visual ProMatrix...'
ENDIF
ON KEY LABEL F10 KEYB [MODI COMM davmprcs]
ON KEY LABEL F12 KEYB [MODI COMM savmfman]
ON KEY LABEL F11 DO HARDCOPY WITH [AVM]
ON KEY LABEL CTRL+U KEYB [USE F:\NACFOX\]
SET TALK ON
RESTORE MACROS
SET EXCL OFF
SET CENT ON
SET PRINT FONT [Lucida Console], 9
SET CENT ON
_screen.fontname = "Lucida Console"
_screen.fontsize = 9
SET CPDIALOG OFF

*********************
FUNCTION FormIsObject
*********************
* Make sure that _SCREEN.ActiveForm is an object, is a form
* and has the specified name.

LPARAM stcFormName

IF TYPE('_SCREEN.ActiveForm') = 'O' AND ;
    ALLTRIM(UPPER(_SCREEN.ActiveForm.BaseClass)) == 'FORM'
  IF EMPTY(stcFormName)
    RETURN .T.
  ELSE
    IF ALLTRIM(UPPER(_SCREEN.ActiveForm.Name)) == ;
        ALLTRIM(UPPER(stcFormName))
      RETURN .T.
    ELSE
      RETURN .F.
    ENDIF
  ENDIF
ELSE
  RETURN .F.
ENDIF

ON KEY LABEL F11 Do HardCopy WITH "SPN2000"
FUNCTION RemoveLogo  &&========================================
IF TYPE('_SCREEN.imglogo')='O'
  _SCREEN.RemoveObject('imglogo')
ENDIF
IF TYPE('_SCREEN.lblproduct')='O'
  _SCREEN.RemoveObject('lblproduct')
ENDIF
IF TYPE('_SCREEN.lblCopyRight')='O'
  _SCREEN.RemoveObject('lblCopyRight')
ENDIF
IF TYPE('_SCREEN.lblCompany')='O'
  _SCREEN.RemoveObject('lblCompany')
ENDIF

FUNCTION MakeCSZ &&=======================
LPARAM zc,zs,zz,cc
PRIV zstr
zstr = []
zstr = zstr + IIF(!EMPT(zc),ALLT(zc)+[, ],[])
zstr = zstr + IIF(!EMPT(zs),ALLT(zs)+[  ],[])
zstr = zstr + IIF(!EMPT(zz),ALLT(zz)+[  ],[])
zstr = zstr + IIF(!EMPT(cc),ALLT(cc),[])
RETURN(ALLT(zstr))

