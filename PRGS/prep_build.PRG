
*//   S C M I    Prep_build  *****************************

PARA zload
IF PARAM()<1
  zload = [NO]
ENDIF

COMP savmfman

CD C:\SPN
SELE 0
USE C:\SPN\savmfapd.apd
jTextVer = ALLT(STRTR(version,[.],[]))
jTextVerDot = version
USE

_screen.fontname = "Lucida Console"
_screen.fontsize = 9

ERASE C:\SPN\_*.frx
ERASE C:\SPN\_*.frt
ERASE C:\SPN\data\_*.frx
ERASE C:\SPN\data\_*.frt
*!*	ERASE C:\SPN\reports\_*.frx
*!*	ERASE C:\SPN\reports\_*.frt

*!*	IF FILE("C:\ThisIsThe1500.CTI") .and. FILE("c:\_avm_misc\mscomm32.ocx")
*!*	  COPY FILE c:\_avm_misc\mscomm32.ocx TO c:\winnt\mscomm32.ocx
*!*	  WAIT [D1500      COPIED    M S C O M M 3 2 . O C X     TO  WINDOWS] WIND TIME .7
*!*	ENDIF

*!*	IF FILE("C:\ThisIsThe2400.CTI") .and. FILE("c:\_avm_misc\mscomm32.ocx")
*!*	  COPY FILE c:\_avm_misc\mscomm32.ocx TO c:\windows\system32\mscomm32.ocx
*!*	  WAIT [D2400      COPIED    M S C O M M 3 2 . O C X     TO  WINDOWS] WIND TIME .7
*!*	ENDIF

*!*	IF FILE("C:\ThisIsThe2400-2.CTI") .and. FILE("C:\c:\_avm_misc\mscomm32.ocx")
*!*	  COPY FILE c:\_avm_misc\mscomm32.ocx TO c:\windows\system32\mscomm32.ocx
*!*	  WAIT [D2400-2      COPIED    M S C O M M 3 2 . O C X     TO  WINDOWS] WIND TIME .7
*!*	ENDIF

CLOSE DATA ALL
SELE 0
USE savmfapd.apd
REPL cur_symbol WITH [$]
USE
USE savmfct2 && set audit trail to .F.
LOCA FOR ID=[SEC_CTRL]
IF FOUND()
  REPL control_on WITH .F. 
ENDIF
LOCA FOR ID=[SEC_MENU]
IF FOUND()
  REPL control_on WITH .F. 
ENDIF
LOCA FOR ID=[SEC_SYSTEM]
IF FOUND()
  REPL control_on WITH .F. 
ENDIF
USE IN savmfct2

USE savmsec EXCL  && set audit trail to .F.
PACK
USE IN savmsec

CLOSE DATA ALL
USE savmsmnu EXCL  && set audit trail to .F.
PACK
USE IN savmsmnu

CLOSE DATA ALL
USE savmsfrm EXCL  && set audit trail to .F.
PACK
USE IN savmsfrm

CLOSE DATA ALL
USE savmsfg EXCL  && set audit trail to .F.
PACK
USE IN savmsfg

CLOSE DATA ALL
USE savmscgd EXCL  && set audit trail to .F.
PACK
USE IN savmscgd

*!*	CLOSE DATA ALL
*!*	USE savmdind EXCL  && set audit trail to .F.
*!*	PACK
*!*	USE IN savmdind

*!*	USE savmdd1 EXCL && set audit trail to .F.
*!*	REPLA ALL open_start WITH .F.
*!*	PACK
*!*	USE IN savmdd1
SET EXCL ON
SELE 0
USE savmdind ORDER file_name EXCL
SELE 0
USE savmdd1 ORDER 1 EXCL  && update index tags into DD so DD report has them
SCAN                      && also loads AVM version into cti_memo field
  SELE savmdind
  SEEK LEFT(savmdd1.file_name,230)     && Builds/replaces with new string each time run
  zIdxTxt = IIF(!EMPT(jTextVer),[SPN]+jTextVer+CHR(13),[])
  SCAN WHILE LEFT(savmdind.file_name,230) = LEFT(savmdd1.file_name,230)
    zIdxTxt = zIdxTxt + tag_name+[  ]+LEFT(index_expr,60);
      +[  ]+LEFT(for_expr,70)+CHR(13)
  ENDSCAN
  SELE savmdd1
  *? zIdxTxt
  WAIT WINDOW "Updating CTI DataDict      " + LEFT(savmdd1.file_name,30) NOWAIT  &&TIME .01
  REPLACE savmdd1.cti_memo WITH zIdxTxt IN savmdd1
ENDSCAN
SELE savmdd1
PACK
SELE savmdind
PACK
USE IN savmdind
USE IN savmdd1

USE savmdd2 EXCL && set audit trail to .F.
REPLA ALL audit_trl WITH .F. FOR LEFT(file_name,4) # [SAVM]
IF 1=1  &&[LOAD] $ zload
  PACK
ENDIF
USE IN savmdd2

*!*	SELE LEFT(file_name,15) ,LEFT(field_name,15) , TYPE, WIDTH FROM savmdd2;
*!*	  WHERE LEFT(file_name,4) # 'SAVM' .and. TYPE='M'.and.WIDTH = 4 INTO CURSOR memprob
*!*	IF _tally>0
*!*	  BROW TITLE " MEMO  FIELDS  CONCERNS" NOEDIT NODELE 
*!*	ENDIF
IF USED("savmdd2")
  USE IN savmdd2
ENDIF

CD C:\SPN
CREATE CURSOR tempdbfs (file c(25))
wtd = SYS(2000,[*.*])
IF LEN(wtd)>12
  SELE tempdbfs
  APPE BLANK
  REPLA file WITH wtd
ENDIF
DO WHILE .T.
  wtd = SYS(2000,[*.*],1)
  WAIT WINDOW "Testing LONG filenames " + wtd + "..." NOWAIT
  IF LEN(wtd)<4
    EXIT
  ENDIF
  IF LEN(wtd)>12
    SELE tempdbfs
    APPE BLANK
    REPLA file WITH wtd
  ENDIF
ENDDO
SELE tempdbfs
IF RECCO()>0
  ON KEY LABEL F11 Do HardCopy WITH "SPN2000"
  BROW TITLE " C:\SPN       FILES WITH  MORE THAN   8.3        F11:PrintScreen" TIME 10  NOEDIT NODELE 
ELSE
  WAIT WINDOW "Testing LONG filenames  NONE FOUND" TIME .3
ENDIF

USE IN tempdbfs

CD C:\SPN\data
ERASE C:\SPN\data\custlbls1.dbf  && don't need, and its 9.3
CREATE CURSOR tempdbfs (file c(25))
wtd = SYS(2000,[*.*])
IF LEN(wtd)>12
  SELE tempdbfs
  APPE BLANK
  REPLA file WITH wtd
ENDIF
DO WHILE .T.
  wtd = SYS(2000,[*.*],1)
  WAIT WINDOW "Testing LONG filenames " + wtd + "..." NOWAIT
  IF LEN(wtd)<4
    EXIT
  ENDIF
  IF LEN(wtd)>12
    SELE tempdbfs
    APPE BLANK
    REPLA file WITH wtd
  ENDIF
ENDDO
SELE tempdbfs
IF RECCO()>0
  ON KEY LABEL F11 Do HardCopy WITH "SPN2000"
  BROW TITLE " SPN \ DATA       FILES WITH  MORE THAN   8.3        F11:PrintScreen"  TIME 10  NOEDIT NODELE 
ELSE
  WAIT WINDOW "Testing LONG filenames  NONE FOUND" TIME .3
ENDIF

USE IN tempdbfs

SELECT 0
curpath = SYS(5) + CURDIR()
CD C:\SPN\reports
zCrLf=CHR(13)+CHR(10)
NumFiles = ADIR(FNames, '*.FRX')  &&// reports
FOR x=1 TO NumFiles
  IF FILE(TRIM(FNames[x,1]))
    FName = ALLTRIM(FNames[x,1])
    WAIT WINDOW "Clearing printer drivers from " + FName + "..." NOWAIT
    USE (FName) EXCL
    LOCATE FOR objtype=1 AND objcode = 53
    IF FOUND() && 2742 for CapAv small labels
      IF ([ELTRON]$UPPE(expr).and.[2742]$UPPE(expr)) ;
        .or. ([ZEBRA]$UPPE(expr).and.[2443]$UPPE(expr)) .or. ([BCLBLS]$UPPE(fname))
         *// leave values as is to make it work  01Dec2001
*!*	        REPLACE TAG  WITH "", tag2 WITH ""
*!*	        REPLA EXPR WITH ;
*!*	          [DEVICE=Eltron TLP2642/2742]+CHR(13)+;
*!*	          [ORIENTATION=0]+CHR(13)+;
*!*	          [PAPERSIZE=256]+CHR(13)+;
*!*	          [DEFAULTSOURCE=342]+CHR(13)+;
*!*	          [PRINTQUALITY=200]+CHR(13)+;
*!*	          [YRESOLUTION=200]
      ELSE
        IF ! EMPT(tag)
          REPLACE TAG  WITH ""
        ENDIF
        IF ! EMPT(tag2)
          REPLACE tag2 WITH ""
        ENDIF
        DO CASE &&// Keep landscapes
          CASE [lot_matusd1] $ LOWER(fname) .or. [repairrpt1] $ LOWER(fname)
           * WAIT [3   ]+fname WIND
           c9245 = [ORIENTATION=1]+CHR(13)+[PAPERSIZE=1]+CHR(13)+[COPIES=1]+CHR(13)+[PRINTQUALITY=600]+CHR(13)+[DUPLEX=1]+CHR(13)+[YRESOLUTION=600]+CHR(13)+[TTOPTION=2]+CHR(13)+[COLLATE=1]
           IF ! ALLT(expr) == c9245
             REPL expr WITH c9245
           ENDIF
          CASE "ORIENTATION=1"$expr
            IF ! ALLT(expr)=="ORIENTATION=1"
              REPLACE expr WITH "ORIENTATION=1"  && landscape
            ENDIF
          CASE "ORIENTATION=2"$expr
            IF ! ALLT(expr) == "ORIENTATION=2"
              REPLACE expr WITH "ORIENTATION=2"  && landscape too
            ENDIF
          OTHERWISE
            IF ! EMPT(expr)
              REPLACE expr WITH ""  &&// 0 = portrait so delete it
            ENDIF
        ENDCASE
      ENDIF
    ENDIF
    LOCATE FOR objtype=25
    IF FOUND() .and. ![SAVM]$UPPE(FName)
      IF EMPT(tag)
        WAIT FName +[ missing tag ] WIND TIME 4
        IF 1=1
          c9246 = [PROCEDURE Init]+zCrLf+;
            [SET PROC TO DAVMPRCS]+zCrLf+;
            [IF WEXIST("Report Designer")]+zCrLf+;
            [  ZOOM WIND "Report Designer" MAX]+zCrLf+;
            [ENDIF]+zCrLf+;
            [ENDPROC]+zCrLf
          IF ! ALLT(tag)==c9246
            REPLACE tag WITH c9246
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    *PACK MEMO
    USE
  ENDIF
ENDFOR

NumFiles = ADIR(FNames, '*.LBX')  && LABELS
FOR x=1 TO NumFiles
  IF FILE(TRIM(FNames[x,1]))
    FName = ALLTRIM(FNames[x,1])
    WAIT WINDOW "Clearing printer drivers from " + FName + "..." NOWAIT
    USE (FName) EXCL
    LOCATE FOR objtype=1 AND objcode = 53
    IF FOUND()
      IF [ELTRON]$UPPE(expr).and.[2442]$UPPE(expr) .or. ([BCLBLS]$UPPE(fname))
        IF ! EMPT(tag)
          REPLACE TAG  WITH ""
        ENDIF
        IF ! EMPT(tag2)
          REPL tag2 WITH ""
        ENDIF
        c9827 = [DEVICE=ELTRON 2442]+CHR(13)+;
          [ORIENTATION=1]+CHR(13)+;
          [PAPERSIZE=263]+CHR(13)+;
          [DEFAULTSOURCE=342]+CHR(13)+;
          [PRINTQUALITY=200]+CHR(13)+;
          [YRESOLUTION=200]
        IF ! ALLT(expr)==c9827
          REPL expr WITH c9827
        ENDIF
      ELSE
        IF ! EMPT(tag)
          REPLACE TAG  WITH ""
        ENDIF
        IF ! EMPT(tag2)
          REPL tag2 WITH ""
        ENDIF
        *WAIT [2   ]+fname WIND
        DO CASE &&// Keep landscapes
          CASE [lot_matusd1] $ LOWER(fname) .or. [repairrpt1] $ LOWER(fname)
           * WAIT [3   ]+fname WIND
         
           * REPL expr WITH [ORIENTATION=1]+CHR(13)+[PAPERSIZE=1]+CHR(13)+[COPIES=1]+CHR(13)+[PRINTQUALITY=600]+CHR(13)+[DUPLEX=1]+CHR(13)+[YRESOLUTION=600]+CHR(13)+[TTOPTION=2]+CHR(13)+[COLLATE=1]

          CASE "ORIENTATION=1"$expr
            IF ! ALLT(expr)=="ORIENTATION=1"
              REPLACE expr WITH "ORIENTATION=1"  && landscape
            ENDIF
          CASE "ORIENTATION=2"$expr
            IF ! ALLT(expr) == "ORIENTATION=2"
              REPLACE expr WITH "ORIENTATION=2"  && landscape too
            ENDIF
          OTHERWISE
            IF ! EMPT(expr)
              REPLACE expr WITH ""  &&// 0 = portrait so delete it
            ENDIF

        ENDCASE
      ENDIF
    ENDIF
    *PACK MEMO
    USE
  ENDIF
ENDFOR
WAIT WINDOW "Zap of printer drivers complete !" TIME 0.2

CD C:\SPN\forms
NumFiles = ADIR(FNames, '*.SCX')  && FORMS
FOR x=1 TO NumFiles
  IF FILE(TRIM(FNames[x,1]))
    FName = ALLTRIM(FNames[x,1])
    WAIT WINDOW "Checking  form   " + FName + "..." NOWAIT
    USE (FName)
    SCAN
      zstr = UPPE(ALLT(classloc))
      IF !LEFT(FName,4)=[SAVM].and.([C:\]$zstr .or. [VPM]$zstr .or. [APP]$zstr)
        BROW TITLE ALLT(fname)+[  ]+zstr+[  Contains  SAVM, VPM  or  APP ] NOEDIT NODELE 
        EXIT
      ENDIF
    ENDSCAN
    USE
  ENDIF
ENDFOR
WAIT WINDOW "Zap of printer drivers complete !" TIME 0.2
CLEAR TYPE
KEYB "{ALT+F}"+'T'
RETURN

IF [111111LOAD] $ zload  &&// ONLY WHEN BUILD &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  RD C:\AVM2000\DATA
  RD C:\AVM2000\TEMP
  RD C:\AVM2000
  CD C:\AVM\DATA
  *!*	  DELE FILE ????qry.*
  *!*	  DELE FILE ???qry.dbf
  *!*	  DELE FILE ???qry.fpt
  *!*	  DELE FILE ?????qry.*
  *!*	  DELE FILE ????temp.*
  *!*	  DELE FILE ???temp.*

  SET SAFETY OFF
  USE savmelog EXCL  && zaps error log
  ZAP
  USE IN savmelog
  SET SAFETY ON

  ERASE c:\setwin16.prq

  SET SAFETY OFF
  CD C:\AVM\data
  wtd = UPPE(SYS(2000,[*.dbf]))
  zSource = [C:\_avm_misc\distrdat\data\]+wtd
  zTarget = [C:\AVM2000\DATA\]+wtd
  IF ![WORDS]$wtd .and. ![QRY]$wtd .and. LEFT(wtd,4)#[SAVM] .and. LEN(wtd) > 4 .and. FILE(zSource)
    WAIT WINDOW "LOADING FRESH DATA FOR   " + wtd + "..." TIME .01
    SELE 0
    USE (wtd) EXCL
    COPY TO (zTarget) WITH CDX
    USE
    SELE 0
    USE (ztarget)
    ZAP
    APPE FROM &zSource
    *SELE 
    USE
  ENDIF
  DO WHILE .T.
    wtd = ALLT(UPPE(SYS(2000,[*.dbf],1)))
    zSource = [C:\_avm_misc\distrdat\data\]+wtd
    IF LEN(wtd) < 4
      EXIT
    ENDIF
    IF ![WORDS]$wtd .and. ![QRY]$wtd .and. LEFT(wtd,4)#[SAVM].and. FILE(zSource)
      *WAIT WINDOW "LOADING FRESH DATA FOR   " + wtd + "..." TIME .1
      WAIT WINDOW wtd+"  LOADING FRESH DATA FOR..." TIME .05
      SELE 0
      USE (wtd) EXCL
      ZAP
      APPE FROM &zSource
      USE IN (wtd)
    ENDIF
  ENDDO
  SET SAFETY ON

  SELE 0
  USE configav
  REPLA configav.hmt        WITH 0
  REPLA configav.frepairnam WITH SPAC(0)
  REPLA configav.fcompname  WITH SPAC(0)
  REPLA configav.fsysserial WITH [0000]
  REPLA configav.prodstat   WITH [EVALUATION]
  REPLA configav.evrprname  WITH [First CONFIGURE your REPAIR Name ! ]
  REPLA configav.evcmpname  WITH [First CONFIGURE your COMPANY Name !  ]
  REPLA configav.regrprname WITH SPAC(0)
  REPLA configav.regcmpname WITH SPAC(0)
  REPLA configav.fire1      WITH {}
  REPLA configav.fire2      WITH {}
  REPLA configav.fire3      WITH {}
  REPLA configav.fireM      WITH {}
  REPLA configav.fireS      WITH {}
  REPLA configav.fire4      WITH {}
  REPLA configav.fire5      WITH {}
  REPLA configav.stat1      WITH [EVALUATION]
  REPLA configav.stat2      WITH [EVALUATION]
  REPLA configav.stat3      WITH [EVALUATION]
  REPLA configav.statM      WITH [EVALUATION]
  REPLA configav.statS      WITH [EVALUATION]
  REPLA configav.stat4      WITH [EVALUATION]
  REPLA configav.stat5      WITH [EVALUATION]
  REPLA configav.keyl1      WITH SPAC(0)
  REPLA configav.keyl2      WITH SPAC(0)
  REPLA configav.keyl3      WITH SPAC(0)
  REPLA configav.keyM       WITH SPAC(0)
  REPLA configav.keyS       WITH SPAC(0)
  REPLA configav.key4       WITH SPAC(0)
  REPLA configav.key5       WITH SPAC(0)
  REPLA configav.extendeval WITH .F.
  REPLA configav.alt337name WITH .F.
  REPLA configav.wbshow_neg WITH .T.
  REPLA configav.lastimport WITH {}
  REPLA configav.qvaliddays WITH 30
  REPLA configav.manwarnday WITH 0
  REPLA configav.lastindex  WITH {}
  REPLA configav.b337font   WITH [Proportional]
  REPLA configav.llbslogo   WITH .T.
  USE IN configav
  SELE 0
  USE configlc
  REPLA configlc.prnpreview WITH [Prompt]
  REPLA configlc.prnprompt  WITH .T.
  REPLA configlc.showtips   WITH .T.
  REPLA configlc.wblookups  WITH .T.
  REPLA configlc.f813blk3   WITH .T.
  REPLA configlc.page_tab   WITH .F.
  REPLA configlc.browheight WITH 5
  REPLA configlc.multicopys WITH .T.
  REPLA configlc.lshowtutor WITH .T.
  REPLA configlc.nprncopies WITH 1
  REPLA configlc.lthermlbls WITH .F.
  REPLA configlc.leditfocus WITH .F.
  USE IN configlc
  SELE 0
  USE configwo
  REPLA configwo.wo_usage   WITH [MULTIPLE]
  REPLA configwo.lquicksers WITH .F.
  REPLA configwo.cores_due  WITH 30
  IF FSIZE("cores_txt")> 0 .and. EMPT(configwo.cores_txt)
    sstr = [Our records indicate that the Core listed below is due by the date specified.]+zCrLf
    sstr = sstr + zCrLf+[Please return the core along with this Core Due Notice before the Core Due Date.  This will ensure proper credit to your account.]+zCrLf
    sstr = sstr + zCrLf+[Thank you.]
    REPLACE configwo.cores_txt WITH sstr
  ENDIF
  IF FSIZE("taskduetxt")> 0 .and. EMPT(configwo.taskduetxt)
    sstr = [Our records indicate that the item listed below is due for calibration and / or maintenance on or before the date specified.]+zCrLf
    sstr = sstr + zCrLf+[Please call our office for details or to make an appointment for this work.]+zCrLf
    sstr = sstr + zCrLf+[Thank you.]
    REPLACE configwo.taskduetxt WITH sstr 
  ENDIF
  IF FSIZE("cores_due")> 0 .and. EMPT(configwo.cores_due)
    REPLACE configwo.cores_due WITH 30 
  ENDIF
  USE IN configwo
  SELE 0
  USE config2
  REPLA config2.lbatchtrac WITH .F.
  REPLA config2.lbtracknum WITH .F.
  REPLA config2.lbreqtknum WITH .F.
  REPLA config2.lbmainlist WITH .F.
  REPLA config2.lbatchison WITH .F.
  REPLA config2.lthermlbls WITH .F.
  REPLA config2.lpetaxprt  WITH .T.
  REPLA config2.lpetaxlbr  WITH .T.
  REPLA config2.lpopart2wo WITH .F.
  REPLA config2.lpopart2wo WITH .F.
  REPLA config2.eds_reg    WITH []
  REPLA config2.bat_reg    WITH []
  REPLA config2.lbl_reg    WITH []
  REPLA config2.tt_reg     WITH []
  REPLA config2.chk_reg    WITH []
  REPLA config2.tbo_reg    WITH []
  REPLA config2.ltt        WITH .T.
  REPLA config2.lchk       WITH .T.
  REPLA config2.ltbo       WITH .T.
  REPLA config2.lprntvpost WITH .T.
  REPLA config2.lprntcpost WITH .T.
  USE IN config2

  SELE 0  && syncronize inventory
  USE serparts ORDER p_s_num
  SELE 0
  USE batchprt ORDER pnum_cat
  SELE 0
  USE avpartss
  SELE avpartss
  SCAN
    REPL serialized WITH .F.,batched WITH .F.
    STORE 0 TO zQoh
    STORE .F. TO zIsSer,zIsBat
    SELE serparts
    SEEK avpartss.PPARTNUM+avpartss.CAT
    SCAN WHILE serparts.pnum + serparts.pcat = avpartss.PPARTNUM + avpartss.CAT
      zIsSer = .T.
      IF serparts.outofstk={}
        zQoh = zQoh + 1
      ENDIF
    ENDSCAN
    STORE 0 TO zcnt,zlistval,zcostval
    SELE batchprt
    SEEK UPPE(avpartss.PPARTNUM+avpartss.CAT)
    SCAN WHILE UPPE(batchprt.pnum+batchprt.pcat) = UPPE(avpartss.PPARTNUM+avpartss.CAT)
      zIsBat = .T.
      zQoh = zQoh + (batchprt.qty_rec-batchprt.qty_used)
    ENDSCAN
    zQoh = IIF(zIsBat,zQoh,avpartss.pqoh)
    SELE avpartss
    REPL avpartss.pqoh WITH zQoh
    REPL avpartss.serialized WITH zIsSer
    REPL avpartss.batched WITH zIsBat
  ENDSCAN
  USE IN serparts
  USE IN batchprt
  USE IN avpartss
  SELE 0
  USE glaccnts
  REPL ALL dlast WITH {} IN glaccnts
  USE IN glaccnts
ENDIF

*!*	SET SAFETY OFF
*!*	SELE 0
*!*	USE c:\_avm_misc\avmmisc
*!*	REPL updat_5trg  WITH STRTR(updat_5   , [*XXX*], jTextVer)
*!*	REPL wsse_5trg   WITH STRTR(wsse_5    , [*XXX*], jTextVer)
*!*	REPL update_trg  WITH STRTR(update_mtr, [*XXX*], jTextVer)
*!*	REPL wsse_trg    WITH STRTR(wsse_mtr  , [*XXX*], jTextVer)
*!*	REPL primse_trg  WITH STRTR(primse_mtr, [*XXX*], jTextVer)

*!*	COPY MEMO updat_5trg  TO C:\_avm_misc\CREATE_UPDATE_5.cmd
*!*	COPY MEMO wsse_5trg   TO C:\_avm_misc\update5wse.txt

*!*	COPY MEMO update_trg  TO C:\_avm_misc\SPN_CREATE_UPDATES.cmd
*!*	COPY MEMO wsse_trg    TO C:\_avm_misc\wswse.txt
*!*	COPY MEMO primse_trg  TO C:\_avm_misc\primewse.txt
*!*	USE IN avmmisc

*!*	MODI FILE C:\_avm_misc\CREATE_UPDATE_5.cmd       NOED
*!*	MODI FILE C:\_avm_misc\update5wse.txt          NOED
*!*	MODI FILE C:\_avm_misc\SPN_CREATE_UPDATES.cmd NOED
*!*	MODI FILE C:\_avm_misc\primewse.txt           NOED
*!*	MODI FILE C:\_avm_misc\wswse.txt              NOED
*!*	CREAT DBF C:\_avm_misc\SPNvers (vers c(5))
*!*	SELE avmvers
*!*	APPE BLANK
*!*	REPL vers WITH jTextVerDot
*!*	BROW
*!*	CLOSE DATA

RETURN


