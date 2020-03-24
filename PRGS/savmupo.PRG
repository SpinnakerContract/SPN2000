lparameters lnReasonCode

* The reason codes are:
*     1 - The project was opened from the Open Project form.
*     2 - A new project was created.
*     3 - A project was copied.
*     4 - VPM was restarted after testing an application.
*     5 - VPM was restarted after exiting to VFP.
sedwefd=yedhyeyhe
jcPath=SET("PATH")
SET PATH TO jcPath+";C:\SPN\TEMP"
IF 1=1 &&INLIST(lnReasonCode,1,4,5)

*  * Set the Project's caption using the project's prefix.

  *_VFP.CAPTION = _SCREEN.oApp.cCurPre +[ - ]+ _VFP.CAPTION
  _Screen.WindowState= 2 && max
  _VFP.CAPTION = [SPN2000 Dev -+- ]+ _VFP.CAPTION

  DO c:\spn\prgs\e

  WAIT [SAVMUPO] WIND TIME .5
ENDIF
