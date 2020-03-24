*===========================================================
* Print a hardcopy of the current form.
* Usage: Hardcopy( [<print job title>] )
* Or:    ON KEY LABEL F12 DO Hardcopy.prg
*===========================================================
LPARAMETERS tcJobName

IF !FILE("DIBAPI32.DLL")
  =pwaitMB([The file named   DIBAPI32.DLL   is missing.  It needs to be available for "PrintScreen" to work.]+CHR(13)+CHR(13);
    +[Locate    DIBAPI32.DLL   and copy it into your   ]+ALLT(zzstartdir)+[  folder.])
  RETURN
ENDIF
*--------------------------------------------
* DECLARE functions if not already done.
*--------------------------------------------
IF NOT TYPE( "PrintWindow( 0, 0, 0, 0, 0, 0 )" ) == "N"
	DECLARE INTEGER PrintWindow IN DibApi32 ;
		INTEGER HWnd, ;
		INTEGER fPrintArea, ;
		INTEGER fPrintOpt, ;
		INTEGER wxScale, ;
		INTEGER wyScale, ;
		STRING szJobName
ENDIF
IF NOT TYPE( "MainHWnd()" ) == "N"
	SET LIBRARY TO FoxTools.fll ADDITIVE
ENDIF

*--------------------------------------------
* Get the active form's window handle and
* get the print job name.
*--------------------------------------------
LOCAL lnHWnd, lcJobName
IF TYPE( "_Screen.oApp.Name" ) == "C"
	* We are within VPME or a VPME application.
	IF _Screen.oApp.GetActiveForm()
		lnHWnd = _WhToHwnd(_WGetPort())
		lcJobName = "Hardcopy - " + _Screen.oApp.oActiveForm.Caption
	ELSE
		lnHWnd = MainHWnd()
		lcJobName = "Hardcopy - " + _Screen.Caption
	ENDIF
ELSE
	* Standard VFP.
	IF TYPE( "_Screen.ActiveForm.Caption" ) == "C"
		lnHWnd = _WhToHwnd(_WGetPort())
		lcJobName = "Hardcopy - " + _Screen.ActiveForm.Caption
	ELSE
		lnHWnd = MainHWnd()
		lcJobName = "Hardcopy - " + _Screen.Caption
	ENDIF
ENDIF
IF TYPE( m.tcJobName ) == "C"
	lcJobName = ALLTRIM( m.tcJobName )
ENDIF

*--------------------------------------------
* The actual hardcopy call.
*--------------------------------------------
* PW_WINDOW: Print the whole window.
* PW_CLIENT: Print the window's client area (i.e. without titlebar and 3D borders).
#DEFINE PW_WINDOW 1
#DEFINE PW_CLIENT 2
* Stretch behaviour, much like VFP's Image.Stretch property.
* PW_BESTFIT: stretch as large as the page allows, but stretch isometric (like Image.Stretch=1)
* PW_STRETCHTOPAGE: stretch as large as the page allows, don't care about proportions (like Image.Stretch=2)
* PR_SCALE: don't stretch, clip if necessary  (like Image.Stretch=0)
#DEFINE PW_BESTFIT 1
#DEFINE PW_STRETCHTOPAGE 2
#DEFINE PW_SCALE 3
LOCAL lnRetVal
lnRetVal = PrintWindow( m.lnHWnd, PW_WINDOW, PW_BESTFIT, 0, 0, m.lcJobName )

*--------------------------------------------
* Error handling.
*--------------------------------------------
#INCLUDE FoxPro.h
#DEFINE _SUCCESS_ 0
#DEFINE _CANCEL_ 6
IF NOT INLIST( m.lnRetVal, _SUCCESS_, _CANCEL_ )
	=pwaitMB("The current Window could not be printed."  +CHR(13)+ ;
		"Error code was  : " + ALLT(STR( m.lnRetVal ) ) )
ENDIF

