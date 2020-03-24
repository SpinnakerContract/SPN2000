CLOSE DATA ALL
SET CENT ON
SET SAFETY OFF
SET EXCL OFF
SET TALK OFF 
SET RESO OFF
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=sys(3050,1,10000000) && FOREGROUND MEMORY set to 10 meg
SET PRINT FONT "Lucida Console",8
_Screen.Visible=.T. && minimized
_Screen.FontName = "Lucida Console"
_Screen.FontSize=9
_screen.WindowState=2 &&--- full size regardless of res prr 04-25-98
IF !FILE("savmelog.dbf")
  =MESSAGEBOX("Can't locate Errors table,  Viewer must be in Data Directory")
ELSE
  SELE 0
  USE savmelog
  GO BOTT
  BROW FIELDS ;
    err_date:H=[Date],err_time:H=[Time],err_no:H=[Err#],err_mess:H=[Mssg],cur_prog:H=[Curr Prg],line_no:H=[Line#],;
    line_code:H=[Code],cur_memory:H=[Memory],cur_status:H=[Status], user:H=[User] NOEDIT NODELE NOAPPE TITLE "Errors Double-Click  MEMO's to see"
ENDIF