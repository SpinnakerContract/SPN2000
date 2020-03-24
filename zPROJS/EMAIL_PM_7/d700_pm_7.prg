*// 02Dec19 01Aug18 04Apr18 03Apr18 04Jan17 08Mar2013
CLOSE DATA ALL
SET CENT ON
SET SAFETY OFF
SET EXCL OFF
SET TALK OFF
SET RESO OFF
SET DELETED ON
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
=sys(3050,1,10000000) && FOREGROUND MEMORY set to 10 meg
SET PRINT FONT "Lucida Console",8
_Screen.Visible=.T. && minimized
_Screen.Width  = 600
_Screen.Height = 800
_Screen.FontName = "Lucida Console"
_Screen.FontSize=9
_screen.WindowState=2 &&--- full size regardless of res prr 04-25-98

*// from actual PM module.  Most PM's from here were moved to ASSETS module 26Mar18, these are run separately below
At_DT410 = FILE("C:\DT410.TXT")
At_G2016 = FILE("C:\GR2016CDRIVE.TXT") .and. DIRECTORY("F:\NACFOX")
zCrLf = CHR(13)+CHR(10)
IF At_DT410 .or. At_G2016 .or. FILE("C:\Pete_Dev.VLD")
  CD F:\nacfox
  SELE 0
  USE F:\nacfox\pass
  SELE 0
  USE F:\nacfox\eqpm_m
  SELE 0
  USE F:\nacfox\eqpm_c
ELSE
  CD D:\Server\nacfox
  SELE 0
  USE D:\Server\nacfox\pass
  SELE 0
  USE D:\nacfox\eqpm_m
  SELE 0
  USE D:\Server\nacfox\eqpm_c
ENDIF
etext = [MODEL                     LAST PM     DUE AGAIN   DAYS  PM BY]+zCrLf
SET ORDER TO key_c_date DESC IN eqpm_c
SELE eqpm_m
SET ORDER TO mod_ser
SCAN
  IF eqpm_m.interval # 7 .or. EMPT(eqpm_m.model) .or. [INACTIVE] $ eqpm_m.status
    LOOP
  ENDIF
  SELE eqpm_c
  SEEK eqpm_m.key_m
  IF FOUND()
    etext = etext + eqpm_m.model+[ ]+DTOC(eqpm_c.date)+[  ]+DTOC(eqpm_c.duedate);
      +STR(eqpm_c.duedate-DATE(),6)+[  ]+eqpm_c.facility + zCrLf
  ENDIF
ENDSCAN
? etext
=INKEY(2)
n876 = 0
SELE pass
SCAN FOR flag24 .and. !EMPTY(pass) .and. [@] $ pass.email
  n876  = n876 + 1
  ? [To: ]+ALLT(pass.email)
  DO send_email WITH ALLT(pass.email),[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[PM 7 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ PM 7 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+ALLT(pass.email)
  =INKEY(1)
ENDSCAN
IF n876 < 1
  DO send_email WITH [<pete@tangoware.com>;<guy@spinnakercontract.com>;<dan@spinnakercontract.com>;<carlos@spinnakercontract.com>;<trevor@spinnakercontract.com>],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[ASSET PM's 7 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[PM 7 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+[Group@spinnakercontract.com]
  =INKEY(1)
ENDIF
CLOSE DATA

*// from ASSETS module.  Most PM's in Assets were moved from PM module 26Mar18, ##############################
At_DT410 = FILE("C:\DT410.TXT")
zCrLf = CHR(13)+CHR(10)
IF At_DT410 .or. At_G2016 .or. FILE("C:\Pete_Dev.VLD")
  CD F:\nacfox
  SELE 0
  USE F:\nacfox\pass
  SELE 0
  USE F:\nacfox\tequip_m
  SELE 0
  USE F:\nacfox\tequip_pm
ELSE
  CD D:\Server\nacfox
  SELE 0
  USE D:\Server\nacfox\pass
  SELE 0
  USE D:\nacfox\tequip_m
  SELE 0
  USE D:\Server\nacfox\tequip_pm
ENDIF
etext = [ASSET#  MODEL                     LAST PM     DUE AGAIN   DAYS  PM BY]+zCrLf
SELECT tequip_pm
SET ORDER TO key_c_date DESC IN tequip_pm
SELE tequip_m
SET ORDER TO mod_ser
SCAN
  IF tequip_m.pminterval # 7 .or. EMPT(tequip_m.model) .or. [INACTIVE] $ tequip_m.activestat
    LOOP
  ENDIF
  SELE tequip_pm
  SEEK tequip_m.key_m
  IF FOUND()
    etext = etext + tequip_m.key_m+[  ]+tequip_m.model+[ ]+DTOC(tequip_pm.date)+[  ]+DTOC(tequip_pm.duedate);
      +STR(tequip_pm.duedate-DATE(),6)+[  ]+tequip_pm.facility + zCrLf
  ENDIF
  SELE tequip_m
ENDSCAN
? etext
=INKEY(2)
n876 = 0
SELE pass
SCAN FOR flag24 .and. !EMPTY(pass) .and. [@] $ pass.email
  n876  = n876 + 1
  ? [To: ]+ALLT(pass.email)
  DO send_email WITH ALLT(pass.email),[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[ASSET PM 7 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ ASSET PM 7 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+ALLT(pass.email)
  =INKEY(1)
ENDSCAN
IF n876 < 1
  DO send_email WITH [<pete@tangoware.com>;<guy@spinnakercontract.com>;<dan@spinnakercontract.com>;<carlos@spinnakercontract.com>;<trevor@spinnakercontract.com>],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[ASSET PM's 7 DAY STATUS REPORT ];
    ,DTOC(DATE())+[ ]+TIME()+[ ASSET PM's 7 DAY STATUS REPORT ]+CHR(13)+CHR(10)+etext,.F.
  =INKEY(0.5)
ENDIF
CLOSE DATA

*// from ASSETS module.  CALIBRATIONS DUE ##############################
*  go back several years and out 30 days
*  include note if OUT for cal
At_DT410 = FILE("C:\DT410.TXT")
zCrLf = CHR(13)+CHR(10)
IF At_DT410 .or. At_G2016 .or.FILE("C:\Pete_Dev.VLD")
  CD F:\nacfox
  SELE 0
  USE F:\nacfox\pass
  SELE 0
  USE F:\nacfox\tequip_m
  SELE 0
  USE F:\nacfox\tequip_c
ELSE
  CD D:\Server\nacfox
  SELE 0
  USE D:\Server\nacfox\pass
  SELE 0
  USE D:\nacfox\tequip_m
  SELE 0
  USE D:\Server\nacfox\tequip_c
ENDIF
etext = [ASSET#  MODEL                     LAST CAL    DUE AGAIN   DAYS  CAL BY          OUT of BUILDING]+zCrLf
SELECT tequip_c
SET ORDER TO key_c_date DESC IN tequip_c
SELE tequip_m
SET ORDER TO mod_ser
SCAN
  IF EMPT(tequip_m.model) .or. [IN-ACTIVE] $ tequip_m.activestat .or. ![STANDARD CALIBRATION] $ tequip_m.status
    LOOP
  ENDIF  && 
  SELE tequip_c
  SEEK tequip_m.key_m
  IF FOUND() .and. BETW( tequip_c.duedate , {01/01/2000} , GOMO(DATE(),1) ) &&&&&& forever ago - out 30 days    include note if OUT for cal
    etext = etext + tequip_m.key_m+[  ]+tequip_m.model+[ ]+DTOC(tequip_c.date)+[  ]+DTOC(tequip_c.duedate);
      +STR(tequip_c.duedate-DATE(),6)+[  ]+LEFT(tequip_c.facility,15)+[ ]+IIF(tequip_m.loutforcal,DTOC(tequip_m.doutforcal),[]) + zCrLf
  ENDIF
  SELE tequip_m
ENDSCAN
? etext
=INKEY(2)
n876 = 0
SELE pass
SCAN FOR flag92 .and. !EMPTY(pass) .and. [@] $ pass.email
  n876  = n876 + 1
  ? [To: ]+ALLT(pass.email)
  DO send_email WITH ALLT(pass.email),[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[CALIBRATIONS 7 DAY STATUS, LOOKS OUT 30 DAYS (ASSETS) ];
    ,DTOC(DATE())+[ ]+TIME()+[ CALIBRATIONS 7 DAY STATUS, LOOKS OUT 30 DAYS (ASSETS) ]+CHR(13)+CHR(10)+etext,.F.
  ? [Email: ]+ALLT(pass.email)
  =INKEY(0.5)
ENDSCAN
IF n876 < 1
  DO send_email WITH [<pete@tangoware.com>;<guy@spinnakercontract.com>;<dan@spinnakercontract.com>],[peter@spinnakercontract.com];
    ,[peter@spinnakercontract.com],[ASSET CALIBRATIONS 7 DAY STATUS REPORT, LOOKS OUT 30 DAYS];
    ,DTOC(DATE())+[ ]+TIME()+[ ASSET  CALIBRATIONS 7 DAY STATUS REPORT, LOOKS OUT 30 DAYS]+CHR(13)+CHR(10)+etext,.F.
  =INKEY(1)
ENDIF
CLOSE DATA
RETURN

PROCEDURE send_email &&-----------------------------------------------
  PARA qto,qfrom,qreply,qsubject,qbody,qkeep
  qqFile=[EMAIL]+RIGHT(SYS(2015),3)
  SET SAFETY OFF
  CREATE CURSOR Qem ( to c(250), from c(100), reply c(100);
    , subject c(150), body m(10), keep l(1), sent c(20) )
  SELE Qem
  APPE BLANK
  REPL Qem.to WITH ALLT(qto), Qem.from WITH ALLT(qfrom), Qem.reply WITH ALLT(qreply),;
    Qem.subject WITH ALLT(qsubject)+[  ]+qqFile,;
    Qem.body WITH ALLT(qbody)+CHR(13)+CHR(10)+qqFile
  IF At_DT410 .or. At_G2016 .or. FILE("C:\Pete_Dev.VLD")
    COPY TO [F:\RFQ\RFQ_OUT\]+qqFile
  ELSE
    COPY TO [D:\Server\RFQ\RFQ_OUT\]+qqFile
  ENDIF
  USE IN Qem

