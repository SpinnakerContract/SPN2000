CLOSE DATA ALL
SET EXCL ON
SET SAFETY OFF
SET TALK OFF 
SET COLOR TO N/BG
*// look for sets of files to scope out correct location
*// should be sated in the SVM2000 primary directory
*// test for exclusive use of configav  and mtest.dbf
*// make sure can open exclusively  configav savmdind,savmdd1,savmdd2  then close all but configav
_screen.WindowState=2 &&--- full size regardless of res prr 04-25-98
=sys(3050,1,24000000) && FOREGROUND MEMORY set to 30 meg
SET RESO OFF
SET CPDIALOG OFF && ensure that tables without CodePage do not get prompts
SET COMPATIBLE OFF && ensures that FSIZE returns the field size not file size
zzStartDir = ALLT(SYS(5))+ALLT(SYS(2003))
=zWindOn([Testing  AVM  File  Structures & Locations])
zWtime = 1.5
STORE .F. TO L1,L2,L3,L4,L5,L6,L7,L8
L1 = FILE(zzstartdir+"\SVM2000.EXE")
L2 = FILE(zzstartdir+"\SAVMFAPD.APD")
L3 = FILE(zzstartdir+"\DATA\CONFIGAV.DBF")
L4 = FILE(zzstartdir+"\DATA\AVCUSTOM.DBF")
L5 = FILE(zzstartdir+"\DATA\GKC.FXP")
L6 = FILE(zzstartdir+"\DATA\SAVMDIND.DBF")
L7 = FILE(zzstartdir+"\DATA\SAVMDD2.DBF")
L8 = FILE(zzstartdir+"\DATA\MTEST.DBF")
=zWindOff(zwtime)
IF ! (L1.and.L2.and.L3.and.L4.and.L5.and.L6.and.L7.and.L8)
  =MESSAGEBOX([Can not find the necessary files in the current folder ]+CHR(13);
    +[The desired folder must be the   Primary / Parent    SVM2000 folder ]+CHR(13);
    +[and can not be the SVM2000\DATA folder]+CHR(13)+CHR(13);
    +[Current Folder   ]+zzStartDir+CHR(13)+CHR(13);
    +[Typical Folders   C:\SVM2000 or  F:\SVM2000 ( substitue your drive for C: or F: ) ],0+48,[UPDATE SVM2000      FOLDER: ]+zzStartDir)
  RETURN
ENDIF
IF !FILE(zzstartdir+"\UP_PRIM.ZIP")
  =MESSAGEBOX([Can not find the necessary ZIP file ]+CHR(13);
    +[The desired file is   UP_PRIM.ZIP and needs to be in the   SVM2000 folder ]+CHR(13);
    +[and can not be the SVM2000\DATA folder],0+48,[UPDATE SVM2000      FOLDER: ]+zzStartDir)
  RETURN
ENDIF
STORE 0 TO e1,e2,e3,e4
*!*	ON ERROR e1 = ERROR()
*!*	SELE 0
*!*	USE zzStartDir+"\DATA\savmdind" EXCL
*!*	IF USED("savmdind")
*!*	  USE IN savmdind
*!*	ENDIF
*!*	ON ERROR e2 = ERROR()
*!*	SELE 0
*!*	USE zzStartDir+"\DATA\savmdd1" EXCL
*!*	IF USED("savmdd1")
*!*	  USE IN savmddd1
*!*	ENDIF
*!*	ON ERROR e3 = ERROR()
*!*	SELE 0
*!*	USE zzStartDir+"\DATA\savmdd2" EXCL
*!*	IF USED("savmdd2")
*!*	  USE IN savmdd2
*!*	ENDIF

ON ERROR e4 = ERROR()
USE zzStartDir+"\DATA\configav" EXCL

SET PATH TO zzStartDir+[;]+zzStartDir+[\DATA]
IF e1+e2+e3+e4 > 0
  =MESSAGEBOX([Found all files, but CAN NOT open files exclusively  ]+STR(zerrnum),0+48,[UPDATE SVM2000     FOLDER: ]+zzStartDir)
  RETURN
ELSE
  =MESSAGEBOX([Found all files, opened configav exclusively and ready to proceed  ]+STR(zerrnum),0+48,[UPDATE SVM2000      FOLDER: ]+zzStartDir)
ENDIF
IF 6#MESSAGEBOX([Ready to proceed with Updater ?],3+48+256,[UPDATE SVM2000      FOLDER: ]+zzStartDir+[;]+zzStartDir+[\DATA])
*6=Yes 7=No 2=Cancel
  RETURN
ENDIF

!pkunzip.exe -d -e -o up_prim -s470524


**************************
*// WILL  NEED USE "CREATE TABLE" TO DISTRIBUTE NEW DBFS
*// BASE THE NEW TABLES ON THE DD  PERHAPS ALREADY DOES THIS
*// MUST GUARD AGAINST TABLE NOT ALREADY EXISTING IN ROUTINES BELOW

*// 21=Feb-2000  20-Feb-2000
*// TEST FOR EXCLUSIVE  CONFIGAV USE ******************
*// uses savmdd1 savmdd2 to create empty new shells
*// then makes a backup copy of each table
*// copies shell on top of existing table which effectively zaps it
*// then appends back in the data from the backup

SET SAFETY OFF &&//////////////////////////

=zWindOn([Creating Backup Directories])
IF DIRECTORY("C:\_AVM2K\BACKUP\LOCAL")  && delete backup files and directories
  DELE FILE C:\_AVM2K\BACKUP\LOCAL\*.*
  RD C:\_AVM2K\BACKUP\LOCAL
ENDIF
IF DIRECTORY("C:\_AVM2K\SHELLS\LOCAL")
  DELE FILE C:\_AVM2K\SHELLS\LOCAL\*.*
  RD C:\_AVM2K\SHELLS\LOCAL
ENDIF
IF DIRECTORY("C:\_AVM2K\BACKUP")  && delete backup files and directories
  DELE FILE C:\_AVM2K\BACKUP\*.*
  RD C:\_AVM2K\BACKUP
ENDIF
IF DIRECTORY("C:\_AVM2K\SHELLS")
  DELE FILE C:\_AVM2K\SHELLS\*.*
  RD C:\_AVM2K\SHELLS
ENDIF
IF DIRECTORY("C:\_AVM2K")
  RD C:\_AVM2K
ENDIF
MD C:\_AVM2K
MD C:\_AVM2K\BACKUP
MD C:\_AVM2K\SHELLS
ztempbackup = "C:\_AVM2K\BACKUP\"
ztempshells = "C:\_AVM2K\SHELLS\"
=zWindOff(zwtime)

zDataDir   = ALLT(zzStartDir)+[\DATA\]  && system main data directory

=zWindOn([Gathering  information  from  ]+zDataDir)

zLSetPeTax1 =.T. && needs new fileds set to checked off  assumes they s/b true unless already there
IF FILE("config2.dbf")
  SELE 0
  USE config2 AGAIN ALIAS zconfig2
  zLSetPeTax1 = FSIZE("lpetaxprt") < 1  && field does not exist
  USE IN zconfig2
ENDIF

zLWoStat =.T. && 
IF !FILE("wostatus.dbf")
  zLWoStat =.F. && 
ENDIF
SELE 0
USE avinvoic
zLoadInvoicePrint = FSIZE("lprntprice") < 1  && field does not exist
USE IN avinvoic
SELE 0
USE avwo_po
zLoadNeedPO = FSIZE("lneedpo") < 1  && field does not exist
USE IN avwo_po
SELE 0
USE avqu_prt  &&avqu_prt.lforwo
zLoadlforwo = FSIZE("lforwo") < 1  && field does not exist
USE IN avqu_prt

*//  NOW include even the SAVM files for backup only
SELE LEFT(file_name,30) AS FILE,desc,comments,primarytag FROM savmdd1;
  WHERE !"#$@%"$UPPE(file_name) INTO DBF zzstartdir+"\zDbfs" ORDER BY file
USE IN savmdd1
SELE zDbfs
SCAN
  REPLA comments WITH [],primarytag WITH []
ENDSCAN
GO TOP
***BROW
=zWindOff(zwtime)
SELE LEFT(file_name,30) AS FILE,LEFT(field_name,30) AS field,type,width,decimals,order FROM savmdd2;
  WHERE !"SAVM"$UPPE(file_name) INTO DBF zzstartdir+"\zFields" ORDER BY order   &&file,                      F I E L D S  
USE IN savmdd2
SELE zFields
***BROW
zz=0
SELE zDbfs  && unique list of tables
GO TOP
SCAN   && scan all tables create shells
  zCurrFile = ALLT(zDbfs.file)
  LConfigav=.F.
  IF EMPT(zCurrFile) .or. [DUMBTA]$UPPE(zCurrFile) &&.or. [ACINSPEC]$UPPE(zCurrFile )
    LOOP
  ENDIF
  *// ACINSPEC IS FIRST FILE  WHICH IS GETTING ALL OF zDbfs OR zFields FIELDS IN IT ??
  =zWindOn([Backing Up  ]+ALLT(zDbfs.desc),zDataDir+zCurrFile)
  SELE zFields  && data dict with fields
  LOCATE FOR ALLT(zFields.file) = ALLT(zCurrFile)
  IF !FOUND()
    SELE zDbfs
    LOOP
  ENDIF
  IF [CONFIGAV]$UPPE(zCurrFile)
    LOOP
    SELE zDbfs  && tables
  ENDIF
*!*	  IF [CONFIGAV]$UPPE(zCurrFile)  && have to close it to do it
*!*	    IF USED("configav")
*!*	      SELE configav
*!*	      USE
*!*	      SELE zDbfs
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF ![CONFIGAV]$UPPE(zCurrFile).and.!USED("configav")  && have to close it to do it
*!*	    SELE 0
*!*	    USE configav EXCL
*!*	    SELE zDbfs
*!*	  ENDIF
  STORE 0 TO zddfcnt,zAVMfcnt,zddfsiz,zAVMfsiz
  STORE 0 TO zCcnt,zNcnt,zDcnt,zLcnt,zAVMCcnt,zAVMNcnt,zAVMDcnt,zAVMLcnt
  zzstr = [(]
  SCAN FOR ALLT(zFields.file) = ALLT(zCurrFile )  &&// scan and accum all data
    zddfcnt = zddfcnt + 1
    zddfsiz = zddfsiz + width
    zCcnt   = zCcnt + IIF(ALLT(type)=[C],1,0)
    zNcnt   = zNcnt + IIF(ALLT(type)=[N],1,0)
    zDcnt   = zDcnt + IIF(ALLT(type)=[D],1,0)
    zLcnt   = zLcnt + IIF(ALLT(type)=[L],1,0)
    zzstr = zzstr + [ ]+ALLT(field)+[ ]+ALLT(type)+[(]+ALLT(STR(width));
      +IIF(decimals>0,[,]+ALLT(STR(decimals)),[])+[),]
  ENDSCAN
  SELE zDbfs

  ** create shells **************
  zLen    = LEN(ALLT(zzstr))
  zzstr   = LEFT(zzstr,zLen-1) + [)]
  zshell  = ztempshells+[~]+zCurrFile+zzstr
  zfp2x   = ztempshells+zCurrFile
  CREATE TABLE &zshell      && opens and leaves file open VFP FORMAT
  *// new file now current table
  COPY TO &zfp2x TYPE FOX2X  && make a FOX 2X equivalent
  USE
  IF USED(zCurrFile)
    WAIT zCurrFile+[ was in use   SHELLS] WIND TIME 2
    SELE (zCurrFile)
    USE
  ENDIF

  ** backup file IF EXISTS **************
  ** loop if does not exist

  zsf = STRTR(zDataDir+zCurrFile,[.DBF],[.*])
  ztf = ztempbackup+STRTR(zCurrFile,[.DBF],[.*])
  WAIT [Looking for   ]+zDataDir+zCurrFile WIND TIME 1
  IF !FILE(zDataDir+zCurrFile)  &&// bail out if file doesn't exist  26Mar2000
    SELE zDbfs    &&// shell will become new empty for
    REPLACE primarytag WITH [NOEXIST]
    LOOP
  ENDIF
  COPY FILE &zsf TO &ztf
  =zWindOn([ from  ]+ALLT(zsf)+[       to  ]+ztf)
  =INKEY(1)
  =zWindOff(.5)

  =zWindOn([ Analyzing  ]+ALLT(zDbfs.desc),zDataDir+zCurrFile)
  SELE 0
  USE (zCurrFile) &&
  ***** compare and mark differences
  zAVMfcnt = FCOUNT()
  FOR jjj = 1 TO FCOUNT()
    xyz = UPPE(TYPE(FIELD(jjj)))
    zAVMCcnt   = zAVMCcnt + IIF(ALLT(xyz)=[C],1,0)
    zAVMNcnt   = zAVMNcnt + IIF(ALLT(xyz)=[N],1,0)
    zAVMDcnt   = zAVMDcnt + IIF(ALLT(xyz)=[D],1,0)
    zAVMLcnt   = zAVMLcnt + IIF(ALLT(xyz)=[L],1,0)
    xxx = FSIZE(FIELD(jjj))
    zAVMfsiz = zAVMfsiz + xxx
  ENDFOR
  USE
  SELE zDbfs
  IF zddfcnt # zAVMfcnt .or. zddfsiz # zAVMfsiz ;
      .or. zCcnt # zAVMCcnt .or. zNcnt # zAVMNcnt .or. zDcnt # zAVMDcnt .or. zLcnt # zAVMLcnt
    REPLACE primarytag WITH [DIFF]
  ENDIF
  REPLA comments WITH zzstr+CHR(13)+CHR(10);
    +[ DD FIELDS ]+STR(zddfcnt ,4)+[   DD SIZE ]+STR(zddfsiz ,4)+CHR(13)+CHR(10);
    +[AVM FIELDS ]+STR(zAVMfcnt,4)+[  AVM SIZE ]+STR(zAVMfsiz,4)+CHR(13)+CHR(10);
    +[DD   C ]+STR(zCcnt,4)+[ N ]+STR(zNcnt,4)+[ D ]+STR(zDcnt,4)+[ L ]+STR(zLcnt,4)+CHR(13)+CHR(10);
    +[AVM  C ]+STR(zAVMCcnt,4)+[ N ]+STR(zAVMNcnt,4)+[ D ]+STR(zAVMDcnt,4)+[ L ]+STR(zAVMLcnt,4)
  zz = zz + 1
  SELE zDbfs
  =zWindOff(zWtime)
ENDSCAN
zoldshells = ztempshells+[~*.*]
DELE FILE &zoldshells
SELE 0
USE savmdind AGAIN ALIAS zdind ORDER file_name


RETURN

SELE zDbfs
CLEAR TYPEAHEAD
COUNT FOR ALLT(primarytag) = [DIFF] .or. ALLT(primarytag) = [NOEXIST] TO zNeedUpdate
=zwindon(STR(zNeedUpdate))+[  Files  need  updating])
SELE zDbfs
GO TOP
SCAN
  zNewFile = ALLT(zDbfs.file)
  IF EMPT(zNewFile) .or. [DUMBTA]$UPPE(zNewFile )
    LOOP
  ENDIF
  IF EMPT(primarytag) .or. !INLI(ALLT(primarytag),[DIFF],[NOEXIST])
    LOOP
  ENDIF
  IF USED(zNewFile)
    WAIT zNewFile+[ was in use ] WIND TIME 2
    SELE (zNewFile)
    USE
  ENDIF
  =zWindOn([Updating file  ]+[  (]+ALLT(zDbfs.desc)+[)],zDataDir + zNewFile)
  ztd = zDataDir + zNewFile                      && target DBF is now local data file
  ztf = STRTR(zDataDir + zNewFile,[.DBF],[.*])   && target files are now local data file
  zsf = ztempshells + STRTR(zNewFile,[.DBF],[.*]) && source files are now shell file
  zbd = ztempbackup + zNewFile                    && backup file
  zfn = STRTR(zNewFile,[.DBF],[])
  *// test to see that all three files exist before updating
  zTargExists   = FILE(zDataDir   + zNewFile)
  zShellExists  = FILE(ztempshells + zNewFile)
  zBackUpExists = FILE(ztempbackup + zNewFile)
  *// For non existing files - copy shell but do NOT append from backup
  *// because it does not exist   SHELL created from DD
  IF zShellExists   &&.and. zBackUpExists && more processing if not exist  .and. zTargExists
    COPY FILE &zsf TO &ztf  && copy empty shell  FILESSSSS  on top of previous good data
    SELE 0
    USE (ztd) EXCL       && use target file now a shell
    IF zBackUpExists     && forget it if never existed  //26Mar2000
      APPEND FROM (zbd)  && append from backup
    ENDIF
    SELE (zfn)           && needs just simple name for newfile
    SELE zdind
    SEEK(UPPE(zNewFile))
    SCAN WHILE UPPE(zdind.file_name) = UPPE(zNewFile)
      =zWindOn([Indexing file  ]+[  (]+ALLT(zDbfs.desc)+[)],zDataDir + zNewFile)
      ztag = ALLT(zdind.tag_name)
      zexp = ALLT(zdind.index_expr)
      zfor = ALLT(zdind.for_expr)
      SELE (zfn)
      IF !EMPT(zfor)
        INDEX ON &zexp TAG &ztag FOR &zfor
      ELSE
        INDEX ON &zexp TAG &ztag
      ENDIF
      SELE zdind
    ENDSCAN
    USE IN (zfn) && close it
  ENDIF
  SELE zDbfs
  =zWindOff(.3)
ENDSCAN

SET SAFETY ON
USE IN zdind
USE IN zDbfs
USE IN zFields

SELE 0
USE serparts &&         zNewSku = Skupk2() &&=========
SCAN
  IF EMPT(serparts.pserial)
    DELE
    LOOP
  ENDIF
  IF sku = 0
    IF zNeedUpdate = 0
      zNeedUpdate = 1
    ENDIF
    zNewSku = Skupk2() &&=========
    REPL sku WITH zNewSku
    *ThisForm._zCurrOP=ALLT(serparts.pnum)+[  ]+ALLT(pserial)+[ ]+ALLT(STR(zNewSku))
  ENDIF
ENDSCAN
USE

SELE 0
USE configwo
IF [SING]$UPPE(configwo.wo_usage)
  REPLACE configwo.wo_usage WITH [SINGLE]
ELSE
  REPLACE configwo.wo_usage WITH [MULTIPLE]
ENDIF
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
SELE 0
USE wo_rates
SELE wo_rates
IF RECCO() < 6  && only once
  DELE ALL
  FOR jjj = 1 TO 10
    zsuf = STRTR(STR(jjj,2),[ ],[0])
    zcost = [configwo.cost]+zsuf
    zrts  = [configwo.l_srts]+zsuf
    zrate = [configwo.rate]+zsuf
    ztype = [configwo.type]+zsuf
    zname = [configwo.typename]+zsuf
    IF (EMPT(zcost) .or. &zrate = 0)  .and. !BETW(jjj,1,6)
      LOOP
    ENDIF
    SELE wo_rates
    APPE BLANK
    REPL wo_rates.lcti     WITH IIF(BETW(jjj,1,6),.T.,.F.)
    REPL wo_rates.cost     WITH &zcost
    REPL wo_rates.l_srts   WITH &zrts
    REPL wo_rates.rate     WITH &zrate
    REPL wo_rates.type     WITH &ztype
    REPL wo_rates.typename WITH &zname
  ENDFOR
ENDIF
SELE wo_rates
jjj = 0
SCAN
  jjj = jjj + 1
  REPLA number WITH jjj
ENDSCAN
USE
SELE configwo
USE

SELE 0
USE config2
COUN FOR !DELE() TO zrcnt
IF zrcnt = 0
  APPEN BLANK
  REPLA config2.lbatchtrac WITH .F.
  REPLA config2.lbtracknum WITH .F.
  REPLA config2.lbreqtknum WITH .F.
  REPLA config2.lbmainlist WITH .F.
  REPLA config2.lbatchison WITH .F.
ENDIF
IF zLSetPeTax1
  REPLA config2.lpetaxprt WITH .T.
  REPLA config2.lpetaxlbr WITH .T.
ENDIF
USE IN config2

*ThisForm._zCurrFile=[Updating  Inventory]
SELE 0
USE avpartss  && remove xref data erroneously entered from Auto PO -- 14Dec2000
SCAN
  IF LEN(ALLT(pxref))=16.and.AT([/],pxref,1)=3.and.AT([/],pxref,2)=6.and.AT([ ],pxref)=11.and.AT([:],pxref)=14
    REPLACE pxref WITH SPAC(0)
  ENDIF
  IF avpartss.paeayn .and. EMPT(avpartss.aeatype) && 02Feb2001 enforce aeatype if aea = .T.
    REPLACE avpartss.aeatype WITH [EQUIPMENT SALE]
  ENDIF
ENDSCAN
USE

*ThisForm._zCurrFile=[Updating  Aircraft File]
SELE 0
USE avaircra  && populate new currtime -- 04Feb2001
SCAN
  IF currtime = 0 .and. prevtime = 0 .and. aftime > 0
    REPLACE currtime WITH aftime
  ENDIF
ENDSCAN
USE

IF zLoadInvoicePrint
  =zWindOn([Updating Invoices])
  SELE 0
  USE avinvoic
  REPLA ALL lprntprice WITH .T.
  USE IN avinvoic
ENDIF

IF !zlWostat
  =zWindOn([Updating  WO  STATUS])
  SELE 0
  USE wostatus
  APPEND BLANK
  REPLA idnum WITH 99999, descrip WITH "END SCAN(s)"
ENDIF

IF zLoadNeedPO && one time
  =zWindOn([Updating Requisitions])
  SELE 0
  USE avwo_po
  REPLA ALL lneedpo WITH .T.
  USE IN avwo_po
ENDIF

IF zLoadlforwo && one time
  =zWindOn([Updating Quotes])
  SELE 0
  USE avqu_prt
  REPLA ALL lforwo WITH .T. IN avqu_prt
  USE IN avqu_prt
  SELE 0
  USE avqu_eqp
  REPLA ALL lforwo WITH .T. IN avqu_eqp
  USE IN avqu_eqp
ENDIF

******=CopyUtilityFiles() && davmprcs

IF zNeedUpdate = 0
  =zWindOn([Update complete ...])
ELSE
  =zWindOn([Updating complete on   ]+ALLT(STR(zNeedUpdate))+[  files])
ENDIF

*!*	IF TYPE("psysvers") =[C]
*!*	  SELE 0
*!*	  USE configav
*!*	  REPLA lastupdate WITH psysvers
*!*	  USE
*!*	ENDIF
CLOSE DATA ALL
RETURN

PROCEDURE zwindon &&++++++++++++++++++++++++++++++++++++++++++++++++
* from lwwprocs
PARA wtext,zTop,zLeft
PRIV wlen,woff,wtext,zTop,zLeft,ztoff,zloff
IF PARA()>0
  wlen = LEN(ALLT(wtext))
  woff = IIF(wlen>12,CEILING(wlen/2)-4,1)
ELSE
  woff = 1
ENDIF
zTop =IIF(TYPE("zTop") =[L],0,zTop)
zLeft=IIF(TYPE("zLeft")=[L],0,zLeft)
zTop =IIF(PARA()>1,zTop,0)
zLeft=IIF(PARA()>2,zLeft,0)
zToff=FLOOR(zTop/14)
zLoff=FLOOR(zLeft/6)
DEFINE WIND zmesswind FROM 9+zToff,35+zLoff-woff TO 14+zToff,57+zLoff+woff DOUB SHAD COLOR SCHEME 13
ACTI WIND zmesswind
@ 1,0 SAY PADC(IIF(EMPT(wtext),'WORKING',wtext),WCOLS())  &&[WORKING]
@ 2,0 SAY PADC('Please wait ...',WCOLS())
RELEASE p__time
PUBLIC p__time
p__time = SECO()
RETURN

PROCEDURE zwindoff && ++++++++++++++++++++++++++++++++++++++++++++++++++
PARA wdel
PRIV wdel
IF !WEXIST('zmesswind')
  RETURN
ENDIF
IF PARA() > 0 .and. TYPE("p__time") # [U] .and. TYPE("wdel") = "N"
  DO WHILE SECO() < p__time + wdel
  ENDDO
ENDIF
IF WEXIST('zmesswind')
  RELE WIND zmesswind
ENDIF
RELEASE p__time
RETURN

FUNCTION SkuPk2 &&--------------------------------------------------
PRIV NextNum,zAlias,zfp,zfs,zfb,znp,zns,znb
zAlias=ALIAS()
USE seq_nums AGAIN IN 0
USE avpartss AGAIN IN 0 ALIAS zvpartss   ORDER sku NOUP
USE serparts AGAIN IN 0 ALIAS zvserparts ORDER sku NOUP
USE batchprt AGAIN IN 0 ALIAS zvbatchprt ORDER sku NOUP
SELE seq_nums
STOR seq_nums.sku + 1 TO NextNum
STOR .F. TO zfp,zfs,zfb
STOR 0   TO znp,zns,znb

SELE zvpartss
GO BOTT
znp = zvpartss.sku
SELE zvserparts
GO BOTT
zns = zvserparts.sku
SELE zvbatchprt
GO BOTT
znb = zvbatchprt.sku
*// get highest each and force nextnum up to it
zMaxAll = MAX(0,znp,zns,znb) + 1
zMaxAll = IIF(zMaxAll > 9999900, 0, zMaxAll )  && exclude cti demo recs
*WAIT [Next ]+STR(NextNum,7) + [     MaxMax  ]+STR(zMaxAll,7)+[     Part MAX ]+STR(znp,7)+[     Ser MAX ]+STR(zns,7)+[     Batch MAX ]+STR(znb,7) WIND
NextNum = MAX(0,NextNum,zMaxAll)
NextNum = IIF(NextNum>9999900,1,NextNum)
DO WHILE .T.
  zfp = SEEK(NextNum,[zvpartss]  ,[sku])
  zfs = SEEK(NextNum,[zvserparts],[sku])
  zfb = SEEK(NextNum,[zvbatchprt],[sku])
  IF zfp.or.zfs.or.zfb
    NextNum = NextNum + 1
    LOOP
  ELSE
    EXIT
  ENDIF
ENDDO
USE IN zvbatchprt
USE IN zvserparts
USE IN zvpartss
SELE seq_nums
REPLACE seq_nums.sku       WITH NextNum IN seq_nums
REPLACE seq_nums.sku_hold  WITH NextNum IN seq_nums && no specific need to protect
USE IN seq_nums
IF !EMPT(zAlias)
  SELE (zAlias)
ENDIF
RETURN(NextNum)


*wtd=MESSAGEBOX([You need to choose Yes or No],4+48+256,[ADVISORY])
*6=Yes 7=No *256=default 2nd button *0=def first button
*wtd=MESSAGEBOX([You need to choose Yes or No or Cancel],3+48+256,[ADVISORY])
*6=Yes 7=No 2=Cancel
*wtd=MESSAGEBOX([You need to choose OK],0+48,[ADVISORY])
*1=OK
*wtd=MESSAGEBOX([You need to choose OK or Cancel],1+48,[ADVISORY])
*2=Cancel
