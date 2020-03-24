*===============================================================
* GetDirectory.prg, revised version 2002-08-05
* (now you can set the initial selected directory
* and the dialog's caption using my SHBrowse.fll).
*
* VFP's GETDIR() function on steroids <g>.
* Opens a Treeview control that displays the available
* drives, directories and the computers in network
* neighborhood, just like Windows Explorer does.
*
* Author: Markus Winhard, mw@bingo-ev.de
*
* This PRG is based on a MS Knowledge Base article written
* for Visual Basic. The original VB code can be found at
* http://support.microsoft.com/support/kb/articles/Q179/4/97.ASP
****************************************************************
* This file is provided 'as is' without warranty of any        *
* kind. In no event shall its authors, contributors, or        *
* distributors be liable for any damages.                      *
****************************************************************
* As this is for free, use it on your own risk <g>.
*===============================================================
LPARAMETERS tcDirectory, tcText, tcCaption

*--------------------------------------------
* Setup.
*--------------------------------------------
LOCAL lcBrowseInfo, lnIDList, lcBuffer, lnCallbackAddress
#DEFINE BIF_RETURNONLYFSDIRS 1
#DEFINE BIF_USENEWUI 64
#DEFINE MAX_PATH 260
IF NOT TYPE( [lstrcat("","")] ) == "N"
	DECLARE INTEGER SHBrowseForFolder IN "shell32" STRING @ BrowseInfo
	DECLARE INTEGER SHGetPathFromIDList IN "shell32" INTEGER pidList, STRING @ lpBuffer
	DECLARE INTEGER lstrcat IN "kernel32" STRING lpString1, STRING lpString2
	DECLARE CoTaskMemFree IN "ole32" INTEGER hMemory
ENDIF
IF NOT TYPE( "MainHwnd()" ) == "N"
	SET LIBRARY TO FoxTools.fll ADDITIVE
ENDIF
IF FILE( "SHBROWSE.FLL" )
	SET LIBRARY TO SHBrowse.fll ADDITIVE
ENDIF

*--------------------------------------------
* Check Parameter.
*--------------------------------------------
IF NOT TYPE( m.tcCaption ) == "C"
	tcCaption = ""
ENDIF
IF NOT TYPE( m.tcDirectory ) == "C" ;
		OR NOT DIRECTORY( m.tcDirectory )
	tcDirectory = ""
ENDIF
IF "SHBROWSE.FLL" $ UPPER(SET( "LIBRARY" ))
	*--------------------------------------------
	* Call the FLL function. It sets two global
	* variables of the FLL and returns the
	* address of the callback function.
	* Later, when SHBrowseForFolder() has
	* initialized the directory selection dialog,
	* it calls the callback function of our FLL
	* which in turn sets the dialog's caption and
	* the text above the treeview.
	*--------------------------------------------
	lnCallbackAddress = SetSHBrowseDir( m.tcDirectory, m.tcCaption )
ELSE
	lnCallbackAddress = 0
ENDIF
IF NOT TYPE( m.tcText ) == "C"
	tcText = ""
ENDIF
tcText = PADR( m.tcText, MAX_PATH, CHR(0) )

*--------------------------------------------
* Fill BrowseInfo structure.
*--------------------------------------------
lcBrowseInfo = ;
	ToIntel( MainHwnd() ) + ;							&& hWndOwner.
	ToIntel( 0 ) + ;									&& pIDLRoot.
	ToIntel( 0 ) + ;									&& pszDisplayName.
	ToIntel( lstrcat( m.tcText, "" ) ) + ;				&& lpszTitle.
	ToIntel( BIF_RETURNONLYFSDIRS + BIF_USENEWUI ) + ;	&& ulFlags.
	ToIntel( m.lnCallbackAddress ) + ;					&& lpfnCallback.
	ToIntel( 0 ) + ;									&& lParam.
	ToIntel( 0 )										&& iImage.

*--------------------------------------------
* Display folder TreeView.
*--------------------------------------------
lnIDList = SHBrowseForFolder( @lcBrowseInfo )

IF m.lnIDList > 0
	*--------------------------------------------
	* Extract the chosen path.
	*--------------------------------------------
	lcBuffer = REPLICATE( CHR(0), MAX_PATH )
	SHGetPathFromIDList( m.lnIDList, @lcBuffer )
	CoTaskMemFree( m.lnIDList )
	lcBuffer = LEFT( m.lcBuffer, AT( CHR(0), m.lcBuffer ) -1 )
ELSE
	lcBuffer = ""
ENDIF

RETURN m.lcBuffer

*===========================================================
* Helper functions.
*===========================================================
FUNCTION ToIntel    
	LPARAMETERS tnValue, tnLength
	*--------------------------------------------
	* Convert to Intel 8086 format numbers.
	*--------------------------------------------
	IF PCOUNT()==1
		tnLength = 4
	ENDIF
	LOCAL lcResult, i
	lcResult = ""
	FOR i = 1 TO m.tnLength
		lcResult = m.lcResult + CHR( m.tnValue % 256 )
		m.tnValue = INT( m.tnValue / 256 )
	ENDFOR
	RETURN m.lcResult
ENDFUNC
