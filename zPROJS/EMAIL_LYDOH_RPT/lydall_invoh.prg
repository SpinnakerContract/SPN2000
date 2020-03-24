&& 03Jan2011
CLEAR
CLOSE DATA ALL
SET SAFETY OFF
SET EXCL OFF
SET TALK OFF 
SET RESO OFF
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=SYS(3050,1,5000000) && FOREGROUND MEMORY set to 5 meg
SET SAFETY OFF
CLOSE DATA ALL
SET EXCL OFF
lcti=FILE("C:\ThisFileOnlyOn.cti")
IF lcti
  SELE 0
  USE F:\nacfox\vendor NOUP
  SELE 0
  USE F:\nacfox\inv NOUP
ELSE
  SELE 0
  USE D:\server\nacfox\vendor NOUP
  SELE 0
  USE D:\server\nacfox\inv NOUP
ENDIF
SELE vendor
LOCATE FOR ALLT(vendor.code)==[LYD001]
IF !FOUND() .or. EMPT(vendor.poemail)
  CLOSE DATA ALL
  RETURN
ENDIF
SELE inv
CREATE DBF c:\temp\lydinvoh (mfgnumber C(25),descrip C(40),qonhand N(10,2), qty_rec N(14,3), qty_used N(14,3);
  ,ref_misc C(50),costeach N(8,3),extcost N(12,3),sku N(8), rxdate D(8), unitmsr C(6), ponumber C(6))
IF lcti
  SELE * FROM F:\NACFOX\batchprt WHERE vendowned=[STKLYD    ] .and. qty_rec>0 .and. qty_rec>qty_used INTO DBF C:\temp\tmp723622 ORDER BY mfg
ELSE
  SELE * FROM D:\SERVER\NACFOX\batchprt WHERE vendowned=[STKLYD    ] .and. qty_rec>0 .and. qty_rec>qty_used INTO DBF C:\temp\tmp723622 ORDER BY mfg
ENDIF
SELE tmp723622
*!*	BROW
SCAN
  zzdescr = []
  IF SEEK(tmp723622.mfg,[inv],[invmfg])
    zzdescr = inv.desc
  ENDIF
  SELE lydinvoh
  APPEND BLANK IN lydinvoh
  REPL mfgnumber WITH tmp723622.mfg, qonhand WITH tmp723622.qty_rec-tmp723622.qty_used, sku WITH tmp723622.sku;
   , qty_rec WITH tmp723622.qty_rec, qty_used WITH tmp723622.qty_used, rxdate WITH tmp723622.rdate;
   , ponumber WITH tmp723622.po, descrip WITH zzdescr;
   , costeach WITH tmp723622.ucost, extcost WITH (tmp723622.qty_rec-tmp723622.qty_used)*tmp723622.ucost IN lydinvoh
   SELE tmp723622
ENDSCAN
SELE lydinvoh
LOCATE
jjtxt   = [LYD_INVTOH_]+DTOS(DATE())+[_]+STRTR(TIME(),[:],[_])
zattachfile  = [C:\TEMP\]+jjtxt+[.XLS]
COPY FIELD mfgnumber,descrip,sku,unitmsr,qonhand,costeach,extcost,ponumber,rxdate,qty_rec,qty_used TO [C:\TEMP\]+jjtxt TYPE XLS
? [Exported to ]+zattachfile
=INKEY(2)
CLOSE DATA ALL
zCrLf = CHR(13)+CHR(10)
xbody=[Lydall Inventory on hand at Spinnaker ]+DTOC(DATE())+[_]+TIME()+zCrLf+zCrLf
xbody = xbody + [Please find the attached inventory report of Lydall material on hand at Spinnaker. The prices in the attached are raw material cost. To purchase, please add 3% to each part and email a PO to laura@spinnakercontract.com]+zCrLf
xbody = xbody + zCrLf+[Sincerely,]
xbody = xbody + zCrLf+[Guy Nickerson]
xbody = xbody + zCrLf+[Spinnaker Contract Manufacturing, INC]
xbody = xbody + zCrLf+[95 Business Park Drive]
xbody = xbody + zCrLf+[Tilton, NH 03276]
xbody = xbody + zCrLf+[603-286-4366 x 150]
xbody = xbody + zCrLf+[603-286-7857 fax]
*DO send1emailnow WITH "smtp1.dsci-net.com","<guy@spinnakercontract.com>;<pete@tangoware.com>","<guy@spinnakercontract.com>","<guy@spinnakercontract.com>",[Lydall Inventory on hand at Spinnaker ]+DTOC(DATE()) ,jbody2 ,zattachfile ,[]
*DO send1emailnow WITH "smtp1.dsci-net.com","<popee1@metrocast.net>;<pete@tangoware.com>","<guy@spinnakercontract.com>","<guy@spinnakercontract.com>",[Lydall Inventory on hand at Spinnaker ]+DTOC(DATE()) ,jbody2 ,zattachfile ,[]
jSubject = [Lydall Inventory on hand at Spinnaker ]+DTOC(DATE())+[_]+TIME()
IF lcti
  SmtpServer=[smtp.metrocast.net]
  SmtpFrom  = "<pete@tangoware.com>"
  SmtpTo    = "<popee1@metrocast.net>;<pete@tangoware.com>;<sales@tangoware.com>;<support@tangoware.com>"
  SmtpReply = "<pete@tangoware.com>"
ELSE
  SmtpServer=[smtp1.dsci-net.com]
  SmtpFrom  = "<guy@spinnakercontract.com>"
  SmtpTo    = ALLT(vendor.poemail) &&03Jan2011 &&"<guy@spinnakercontract.com>;<CHale@lydall.com>;<KSmith@lydall.com>;<RStrese@lydall.com>;<MLongmuir@lydall.com>"
  SmtpReply = "<guy@spinnakercontract.com>"
ENDIF
SELE 0
USE D:\Server\nacfox\confnet NOUP
SmtpT2 =  ALLT(confnet.lydemails)
USE
IF !EMPT(SmtpT2)
  SmtpTo = SmtpT2
ENDIF
? SmtpTo
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
jRetRecpt = []
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
  Kode = seeSendEmail(0,@SmtpTo,@EmptyStr,@EmptyStr,@jSubject,@xBody,@zAttachFile)
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
=INKEY(5)
CLEAR DLLS
CLEAR ALL
RETURN


**DO send1emailnow WITH "smtp1.dsci-net.com","Pete Ryder<pete@tangoware.com>";
**  ,"<pete@tangoware.com>","<pete@tangoware.com>",[LYDALL INVT OH ]+DTOC(DATE()),[LYDALL INVT OH ]+DTOC(DATE()),zattachfile,[]
*ERASE (zattachfile)
**LPARA SmtpServer,SmtpFrom,SmtpTo,SmtpReply,jSubject,jBody,AttachStr,jRetRecpt
*!*  CHale@lydall.com 
*!*  KSmith@lydall.com 
*!*  RStrese@lydall.com 
*!*  MLongmuir@lydall.com 
*!*  guy@spinnakercontract.com 
*!*  Subject Header: Lydall Inventory on hand at Spinnaker
*!*  Please find the attached inventory report of Lydall material on hand at Spinnaker. The prices in the attached are raw material cost. To purchase, please add 3% to each part and email a PO to laura@spinnakercontract.com

*!*  Sincerely,  
*!*  Guy Nickerson
*!*  Spinnaker Contract Manufacturing, INC
*!*  95 Business Park Drive
*!*  Tilton, NH 03276
*!*  603-286-4366 x 150
*!*  603-286-7857 fax

