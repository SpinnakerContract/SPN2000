CLOSE DATA ALL
SET SAFETY OFF
SET EXCL OFF
SET TALK OFF 
SET RESO OFF
SET DELE ON
SET CENT ON
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=sys(3050,1,10000000) && FOREGROUND MEMORY set to 10 meg
IF 0 < is_run32([QUICK ATC]) .and. 0 < is_run32([Licensee:])
  =MESSAGEBOX("Can't run Quick ATC again, it is alreadyrunning. "+CHR(13)+CHR(13);
    +"Quit AVM and try again.")
  RETURN
ENDIF
_Screen.Visible=.F. && minimized
_Screen.windowstate=1 && 1 0r 2
IF !FILE("configav.dbf").or.!FILE("lockout.dbf")
  =MESSAGEBOX("Can't locate Q_ATC tables,  QUICK_ATC.EXE must be located in the PRIMARY Data Folder"+CHR(13)+CHR(13);
    +[The folder is typically  F:\NACFOX\DATA  ])
  RETURN
ENDIF

DO FORM Q_ATC2

READ EVENTS
CLEAR EVENTS