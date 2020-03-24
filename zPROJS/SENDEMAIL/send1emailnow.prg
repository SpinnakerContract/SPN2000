LPARA SmtpServer,SmtpFrom,SmtpTo,SmtpReply,jSubject,jBody,AttachStr,jRetRecpt
CLEAR
CLOSE DATA ALL
SET SAFETY OFF
SET EXCL OFF
SET TALK OFF 
SET RESO OFF
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=SYS(3050,1,5000000) && FOREGROUND MEMORY set to 5 meg
*_Screen.Visible=.F. 
*_Screen.windowstate=0 && 0 normal 1 minimized  2 max
#INCLUDE KEYCODE.FOX
#INCLUDE SEE32CON.FOX
*!*	#DEFINE SEE_KEY_CODE 1916019042
*!*	DO See32Con && === converted to a prg
DECLARE INTEGER seeAttach in SEE32.DLL INTEGER NbrChans, INTEGER KeyCode
DECLARE INTEGER seeClose in SEE32.DLL INTEGER Chan
DECLARE INTEGER seeErrorText in SEE32.DLL INTEGER Chan, INTEGER Kode, STRING @Buffer, INTEGER BufLen
DECLARE INTEGER seeIntegerParam in SEE32.DLL INTEGER Chan, INTEGER Param, INTEGER Value
DECLARE INTEGER seeRelease in SEE32.DLL
DECLARE INTEGER seeSendEmail in SEE32.DLL INTEGER Chan, STRING @To, STRING @CC, STRING @BCC, STRING @Subj, STRING @Msg, STRING @Attach
DECLARE INTEGER seeSmtpConnect in SEE32.DLL INTEGER Chan, STRING @Server, STRING @From, STRING @Reply
DECLARE INTEGER seeStringParam in SEE32.DLL INTEGER Chan, INTEGER Param, STRING @Value
zCrLf=CHR(13)+CHR(10)
EmptyStr = CHR(0)
DiagFile = "C:\SEND.LOG"
? DTOC(DATE())+[  ]+TIME(1)
Kode = seeAttach(1, SEE_KEY_CODE)
IF Kode < 0
  ? "Cannot attach SEE (check key code)"
  RETURN
ENDIF
IF !EMPT(jRetRecpt)
  r1   = "Return-Receipt-To:"+jRetRecpt
  r2   = "Disposition-Notification-To:"+jRetRecpt
  K    = seeStringParam(0, SEE_ADD_HEADER,r1) 
  K    = seeStringParam(0, SEE_ADD_HEADER,r2) 
ENDIF
Kode = seeStringParam(0, SEE_LOG_FILE, @DiagFile)
? "Connecting to " + SmtpServer
Kode = seeSmtpConnect(0, @SmtpServer, @SmtpFrom, @SmtpReply)
IF Kode < 0
  Temp = SPACE(128)
  Kode = seeErrorText(0,Kode,@Temp,128)
  ? Left(Temp,Kode)
ELSE
  ? "Sending email to " + SmtpTo
  Kode = seeSendEmail(0,SmtpTo,EmptyStr,EmptyStr,jSubject,jBody,AttachStr)
  if Kode < 0
    Temp = SPACE(128)
    Kode = seeErrorText(0,Kode,@Temp,128)
    ? Left(Temp,Kode)
  else
    ? "Email has been sent."
    Kode = seeClose(0)
  endif
endif
? "Done."
Kode = seeRelease()
=INKEY(3)
CLEAR DLLS
CLEAR ALL
RETURN



