=sys(3050,1,10000000) && FOREGROUND MEMORY set to 10 meg
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
_sCreen.Visible=.F. && minimized
_sCreen.windowstate=1 && minimized
oWsh=CREATEOBJECT("wscript.shell")
IF TYPE("oWsh")=[O]
  IF FILE("C:\SVM2KWS\SVM2000.EXE")
    cDeskPath = oWsh.SpecialFolders("desktop")
    Oshort    = oWsh.CreateShortCut(cDeskPath+"\SVM2000 WorkStation.LNK")
    Oshort.TargetPath  = "C:\SVM2KWS\SVM2000.EXE"
    Oshort.Description = "SVM2000 WorkStation"
    Oshort.WorkingDirectory = "C:\SVM2KWS\"
    Oshort.Save
  ENDIF
  IF FILE("C:\SVM2000\SVM2000.EXE")
    cDeskPath = oWsh.SpecialFolders("desktop")
    Oshort    = oWsh.CreateShortCut(cDeskPath+"\SVM2000 Primary.LNK")
    Oshort.TargetPath  = "C:\SVM2000\SVM2000.EXE"
    Oshort.Description = "SVM2000 Primary"
    Oshort.WorkingDirectory = "C:\SVM2000\"
    Oshort.Save
  ENDIF
  RELEASE oWsh
ENDIF
QUIT