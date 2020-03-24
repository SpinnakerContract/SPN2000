CLOSE DATA ALL
SET SAFETY OFF
SET EXCL OFF
SET DELE ON
SET TALK OFF 
SET RESO OFF
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=sys(3050,1,10000000) && FOREGROUND MEMORY set to 10 meg
IF 0 < is_run32([AVM-20]) .and. 0 < is_run32([Licensee:])
  =MESSAGEBOX("Can't run  External Repair  function when  AVM  itself is running. "+CHR(13)+CHR(13);
    +"Quit AVM and try again.")
  RETURN
ENDIF
_Screen.Visible=.F. && minimized
_Screen.windowstate=1 && minimized
STORE .F. TO L1,L2,L3,L4,L5,L6,L7,L8
L1 = FILE("AVPARTSS.DBF")
L2 = FILE("AVVENDOR.DBF")
L3 = FILE("CONFIGAV.DBF")
L4 = FILE("AVCUSTOM.DBF")
L5 = FILE("GKC.FXP")
L6 = FILE("SAVMDIND.DBF")
L7 = FILE("SAVMDD2.DBF")
L8 = FILE("MTEST.DBF")
IF ! (L1.and.L2.and.L3.and.L4.and.L5.and.L6.and.L7.and.L8)
  =MESSAGEBOX([Can not find the necessary files in the current folder. ]+CHR(13)+CHR(13);
    +[Unable to perform any repairs.]+CHR(13)+CHR(13);
    +[Must execute this from the SVM2000\DATA folder.])
  RETURN
ENDIF
ON ERROR zzerrnum = ERROR( )
SET SAFETY OFF
zzerrnum = 0
SELE 0
USE configav EXCL
*WAIT STR(zzerrnum) WIND TIME 2
IF zzerrnum # 0  && excl
  =MESSAGEBOX([Can not find the necessary files in the current folder. ]+CHR(13)+CHR(13);
    +[Unable to perform any repairs.]+CHR(13)+CHR(13);
    +[Must execute this from the SVM2000\DATA folder.])
  RETURN
ENDIF
IF USED('configav')
  USE IN configav
ENDIF

DO FORM ExtRepair &&===========

READ EVENTS
CLEAR EVENTS