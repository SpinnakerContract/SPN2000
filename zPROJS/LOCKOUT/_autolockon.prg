CLOSE DATA ALL
SET CENT ON
SET SAFETY OFF
SET DELE ON
SET EXCL OFF
SET TALK OFF 
SET RESO OFF
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=sys(3050,1,10000000) && FOREGROUND MEMORY set to 10 meg
_Screen.Visible=.T. && minimized
_screen.WindowState=2 &&--- full size regardless of res prr 04-25-98
IF !FILE("lockout.dbf")
  WAIT [CAN'T LOCATE LOCKOUT FILE,  THIS EXE MUST RESIDE IN AVM DATA FOLDER ] WIND TIME 3
ELSE
  SELE 0
  USE lockout
  GO TOP
  REPL lockout.lockedout WITH .T. IN lockout
  CLOSE DATA ALL
  WAIT [AUTO  LOCK  IS  ON ] WIND TIME 2
ENDIF