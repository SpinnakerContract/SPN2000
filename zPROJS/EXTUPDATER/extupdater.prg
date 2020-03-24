CLOSE DATA ALL
SET SAFETY OFF
SET EXCL OFF
SET TALK OFF 
SET RESO OFF
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=SYS(3050,1,10000000) && FOREGROUND MEMORY set to 10 meg
_Screen.Visible=.T. 
_Screen.Height=510 
_Screen.Width=800
_Screen.Closable=.F.
_Screen.Movable=.F. 
_Screen.MaxButton=.F. 
_Screen.MinButton=.F. 
_Screen.ControlBox=.F. 
_Screen.windowstate=0 && 0 normal 1 minimized  2 max

DO FORM ExtUpdater &&============

CLEAR MENUS
CLEAR POPUPS
CLEAR PROMPT
CLEAR TYPEAHEAD
CLEAR WINDOWS
CLOSE ALL
ON SHUTDOWN

*WAIT [Done] WIND TIME 1
*READ EVENTS
*CLEAR EVENTS
