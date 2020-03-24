*// 02Dec19 01Aug18 03Apr18 27Mar18 04Jan17 08MAr2013
CLOSE DATA ALL
SET CENT ON
SET SAFETY OFF
SET EXCL OFF
SET TALK OFF
SET RESO OFF
SET DELETED ON
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=SYS(3050,1,10000000) && FOREGROUND MEMORY set to 10 meg
SET PRINT FONT "Lucida Console",8
SET MEMO TO 150
_SCREEN.VISIBLE=.T. && minimized
_SCREEN.WIDTH=600
_SCREEN.HEIGHT=800
_SCREEN.FONTNAME = "Lucida Console"
_SCREEN.FONTSIZE=10
_SCREEN.WINDOWSTATE=2 &&--- full size regardless of res prr 04-25-98
CLEAR
lforce = DTOS(DATE())=[20180327].AND.SECO() < (11 * 3600)
?
? [PM 30 check]
jcont = .T.
IF DOW(DATE()) # 4 .AND. DATE()>{02/13/2008} && wed
  ? [Today is NOT Wednesday, cancelling. (]+ALLT(STR(DOW(DATE())))+[)]
  =INKEY(2.0)
  jcont = .F.
ENDIF

IF !BETW(DAY(DATE()), 1, 7) .AND. DATE()>{02/13/2008} && not first wed of month
  ? [Today is NOT the FIRST Wednesday of the Month,  cancelling.  (]+ALLT(STR(DAY(DATE())))+[)]
  =INKEY(2.0)
  jcont = .F.
ENDIF
IF !jcont .AND. !lforce
  RETURN
ENDIF

*// these are remaining PM's in PM, most PM's were moved to ASSETS 26Mar18
At_DT410 = FILE("C:\DT410.TXT")
zCrLf = CHR(13)+CHR(10)
DO CASE
  CASE At_DT410 .OR. FILE("C:\Pete_Dev.VLD")
    CD F:\nacfox
    SELE 0
    USE F:\nacfox\PASS
    SELE 0
    USE F:\nacfox\eqpm_m
    SELE 0
    USE F:\nacfox\eqpm_c
  CASE FILE("c:\GR2016CDRIVE.TXT") .AND. DIRECTORY("F:\NACFOX") && gertrude Win2016  ##############################################
    CD F:\nacfox
    SELE 0
    USE F:\nacfox\PASS
    SELE 0
    USE F:\nacfox\tequip_m
    SELE 0
    USE F:\nacfox\tequip_pm
  CASE FILE("c:\GR2016CDRIVE.TXT") .AND. DIRECTORY("D:\SERVER\NACFOX") && gertrude Win2016
    CD D:\Server\nacfox
    SELE 0
    USE D:\Server\nacfox\PASS
    SELE 0
    USE D:\Server\nacfox\eqpm_m
    SELE 0
    USE D:\Server\nacfox\eqpm_c
  OTHERWISE
    SELE 0
    USE F:\nacfox\PASS
    SELE 0
    USE F:\nacfox\eqpm_m
    SELE 0
    USE F:\nacfox\eqpm_c
ENDCASE

*!*	IF At_DT410 .OR. FILE("C:\Pete_Dev.VLD")
*!*	  CD F:\nacfox
*!*	  SELE 0
*!*	  USE F:\nacfox\PASS
*!*	  SELE 0
*!*	  USE F:\nacfox\eqpm_m
*!*	  SELE 0
*!*	  USE F:\nacfox\eqpm_c
*!*	ELSE
*!*	  CD D:\Server\nacfox
*!*	  SELE 0
*!*	  USE D:\Server\nacfox\PASS
*!*	  SELE 0
*!*	  USE D:\Server\nacfox\eqpm_m
*!*	  SELE 0
*!*	  USE D:\Server\nacfox\eqpm_c
*!*	ENDIF
etext = [MODEL                     LAST PM     DUE AGAIN   DAYS  PM BY]+zCrLf
SET ORDER TO key_c_date DESC IN eqpm_c
SELE eqpm_m
SET ORDER TO mod_ser
SCAN
  IF eqpm_m.INTERVAL # 30 .OR. EMPT(eqpm_m.model) .OR. [INACTIVE] $ eqpm_m.STATUS
    LOOP
  ENDIF
  SELE eqpm_c
  SEEK eqpm_m.key_m
  IF FOUND()
    etext = etext + eqpm_m.model+[ ]+DTOC(eqpm_c.DATE)+[  ]+DTOC(eqpm_c.duedate);
      +STR(eqpm_c.duedate-DATE(),6)+[  ]+eqpm_c.facility + zCrLf
  ENDIF
ENDSCAN
? etext
n876 = 0
SELE PASS
SCAN FOR flag25 .AND. !EMPTY(PASS) .AND. [@] $ PASS.email
  n876  = n876 + 1
  DO send_email WITH ALLT(PASS.email),[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[PM 30 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ PM 30 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+ALLT(PASS.email)
  =INKEY(1)
ENDSCAN
IF n876 < 1
  DO send_email WITH [pete@tangoware.com],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[PM 30 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ PM 30 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+[pete@tangoware.com]
  =INKEY(1)
  DO send_email WITH [guy@spinnakercontract.com],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[PM 30 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ PM 30 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  =INKEY(1)
  ? [Email: ]+[guy@spinnakercontract.com]
  DO send_email WITH [dan@spinnakercontract.com],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[PM 30 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ PM 30 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  =INKEY(1)
  ? [Email: ]+[dan@spinnakercontract.com]
  DO send_email WITH [trevor@spinnakercontract.com],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[PM 30 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ PM 30 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  =INKEY(1)
  ? [Email: ]+[trevor@spinnakercontract.com]
  DO send_email WITH [carlos@spinnakercontract.com],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[PM 30 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ PM 30 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+[carlos@spinnakercontract.com]
  =INKEY(1)
ENDIF
CLOSE DATA

*// these are PM's in ASSETS
At_DT410 = FILE("C:\DT410.TXT")
zCrLf = CHR(13)+CHR(10)
DO CASE
  CASE At_DT410 .OR. FILE("C:\Pete_Dev.VLD")
    CD F:\nacfox
    SELE 0
    USE F:\nacfox\PASS
    SELE 0
    USE F:\nacfox\tequip_m
    SELE 0
    USE F:\nacfox\tequip_pm
  CASE FILE("c:\GR2016CDRIVE.TXT") .AND. DIRECTORY("F:\NACFOX") && gertrude Win2016  ##############################################
    CD F:\nacfox
    SELE 0
    USE F:\nacfox\PASS
    SELE 0
    USE F:\nacfox\tequip_m
    SELE 0
    USE F:\nacfox\tequip_pm
  CASE FILE("c:\GR2016CDRIVE.TXT") .AND. DIRECTORY("D:\SERVER\NACFOX") && gertrude Win2016
    CD D:\Server\nacfox
    SELE 0
    USE D:\Server\nacfox\PASS
    SELE 0
    USE D:\Server\nacfox\tequip_m
    SELE 0
    USE D:\Server\nacfox\tequip_pm
  OTHERWISE
    CD F:\nacfox
    SELE 0
    USE F:\nacfox\PASS
    SELE 0
    USE F:\nacfox\tequip_m
    SELE 0
    USE F:\nacfox\tequip_pm
ENDCASE
*!*	IF At_DT410 .or. FILE("C:\Pete_Dev.VLD")
*!*	  CD F:\nacfox
*!*	  SELE 0
*!*	  USE F:\nacfox\pass
*!*	  SELE 0
*!*	  USE F:\nacfox\tequip_m
*!*	  SELE 0
*!*	  USE F:\nacfox\tequip_pm
*!*	ELSE
*!*	  CD D:\Server\nacfox
*!*	  SELE 0
*!*	  USE D:\Server\nacfox\pass
*!*	  SELE 0
*!*	  USE D:\Server\nacfox\tequip_m
*!*	  SELE 0
*!*	  USE D:\Server\nacfox\tequip_pm
*!*	ENDIF
etext = [ASSET#  MODEL                     LAST PM     DUE AGAIN   DAYS  PM BY]+zCrLf
SET ORDER TO key_c_date DESC IN tequip_pm
SELE tequip_m
SET ORDER TO mod_ser
SCAN
  IF tequip_m.pminterval # 30 .OR. EMPT(tequip_m.model) .OR. [IN-ACTIVE] $ tequip_m.activestat
    LOOP
  ENDIF
  SELE tequip_pm
  SEEK tequip_m.key_m
  IF FOUND()
    etext = etext + tequip_m.key_m+[  ]+tequip_m.model+[ ]+DTOC(tequip_pm.DATE)+[  ]+DTOC(tequip_pm.duedate);
      +STR(tequip_pm.duedate-DATE(),6)+[  ]+tequip_pm.facility + zCrLf
  ENDIF
  SELECT tequip_m
ENDSCAN
? etext
n876 = 0
SELE PASS
SCAN FOR flag25 .AND. !EMPTY(PASS) .AND. [@] $ PASS.email
  n876  = n876 + 1
  DO send_email WITH ALLT(PASS.email),[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[ASSET PM 30 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ ASSET PM 30 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+ALLT(PASS.email)
  =INKEY(1)
ENDSCAN
IF n876 < 1
  DO send_email WITH [<pete@tangoware.com>;<guy@spinnakercontract.com>;<dan@spinnakercontract.com>;<carlos@spinnakercontract.com>;<trevor@spinnakercontract.com>],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[ASSET PM's 30 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ ASSET PM's 30 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+[GROUP@spinnakercontract.com]
  =INKEY(1)
ENDIF
CLOSE DATA
RETURN

PROCEDURE send_email &&-----------------------------------------------
  PARA qto,qfrom,qreply,qsubject,qbody,qkeep
  qqFile=[EMAIL]+RIGHT(SYS(2015),3)
  SET SAFETY OFF
  CREATE CURSOR Qem ( TO c(250), FROM c(100), reply c(100);
    , subject c(150), body m(10), keep l(1), sent c(20) )
  SELE Qem
  APPE BLANK
  REPL Qem.TO WITH ALLT(qto), Qem.FROM WITH ALLT(qfrom), Qem.reply WITH ALLT(qreply),;
    Qem.subject WITH ALLT(qsubject)+[  ]+qqFile,;
    Qem.body WITH ALLT(qbody)+CHR(13)+CHR(10)+qqFile
  IF At_DT410 .OR. FILE("C:\Pete_Dev.VLD") .or. (FILE("c:\GR2016CDRIVE.TXT") .AND. DIRECTORY("F:\NACFOX"))
    COPY TO [F:\RFQ\RFQ_OUT\]+qqFile
  ELSE
    COPY TO [D:\Server\RFQ\RFQ_OUT\]+qqFile
  ENDIF
  USE IN Qem

FUNCTION LastDOM &&-----------------------
  PARA d1
  PRIV d1
  RETURN GOMO(d1+1-DAY(d1),1)-1

FUNCTION DaysInMo &&-----------------------
  PARA d1
  PRIV d1
  RETURN DAY(GOMO(d1+1-DAY(d1),1)-1)


