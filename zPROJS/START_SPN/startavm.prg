*............................................
*                |
*   Program Name | APPSTART.PRG
*                |
*   Created      | 03/21/2001  18:47:27
*   Language     | Microsoft Visual FoxPro Version 06.00.8492.00
*                |
*   Author       | Jan (James R.) Smith
*                | PC Data Systems, Inc.
*                | (651) 636-2993 - voice and fax
*                | Internet: jansmith@attbi.com
*                |
*   Copyright    | (c) 2001 Jan Smith
*                | All Rights Reserved
*                |
*   Purpose      | This file compares the date/time of the EXE file
*                | found in the same directory as this program, with
*                | the date/time of a file with the same name on the
*                | network. If the network one is newer, this program
*                | will download it to the local directory. Then this
*                | program runs the local EXE file.
*                |
*   Syntax       | DO Appstart
*                |
*   Parameters   | none
*                |
*............................................
*
* Revised: 01/05/2019  09:39:28 - PR & AR
* Revised: 05/10/2001  08:49:28 - jrs - see below
* Revised: 12/06/2001  20:51:38 - jrs - completely re-written to use an
*          INI file or an APD file. If using an APD file, will now work
*          with versions 5, 6, or 7 of SxxxFAPD.APD.
*
*!*	IF FILE("C:\nacvfp\spn_init.bat")
*!*	  RUN C:\nacvfp\spn_init.bat
*!*	ENDIF
*!*	IF FILE("C:\nacvfp\spn_init2.cmd")
*!*	  RUN C:\nacvfp\spn_init2.cmd
*!*	ENDIF
SET TALK OFF
SET SAFETY OFF
SET EXCL OFF
SET CENT ON  && cti
IF ! FILE("SPN2000.EXE").and.FILE("F:\nacfox\SPN2000.EXE")
  WAIT WIND [SPN2000.EXE missing,      Copying  Program File  from  Server  Please  Wait!] NOWAIT AT 200,200
  COPY FILE F:\nacfox\spn2000.exe TO spn2000.exe  && copy exe
  WAIT CLEAR
ENDIF
LOCAL ARRAY dlaAPDfile[1,1], dlaEXE_L[1,1], dlaEXE_N[1,1]
LOCAL dlcExePath, dlcProgName, dlcEXEfile_L, dlcEXEfile_N, dltDT_L, dltDT_N, cdatadir
cdatadir = []
DO CASE
  CASE 1=1  &&FILE("SAVMFAPD.APD") && cti
    ** DECLARE DLL statements for reading/writing to private INI files
    *DECLARE INTEGER GetPrivateProfileString IN Win32API  AS GetPrivStr ;
    *	STRING cSection, STRING cKey, STRING cDefault, ;
    *	STRING @cBuffer, INTEGER nBufferSize, STRING cINIFile

    * Get the path to the (potentially) newer EXE file on the network.
    *
    dlcExePath = "F:\NACFOX"

    *= GETPRIVSTR('NewExePath','ExePath','', ;
    *  @dlcExePath, LEN(dlcExePath), SYS(5)+SYS(2003)+"\APPSTART.INI")

    dlcExePath = ALLTRIM(STRTRAN(dlcExePath, CHR(0),""))
    dlcExePath = IIF(RIGHT(dlcExePath,1) = "\", dlcExePath, dlcExePath + "\")

    * Now get the name of the program.
    *
    dlcProgName = "SPN2000.EXE"

    *= GETPRIVSTR('NewExePath','ProgramName','', ;
    *  @dlcProgName, LEN(dlcProgName), SYS(5)+SYS(2003)+"\APPSTART.INI")

    dlcEXEfile_L = ALLTRIM(STRTRAN(dlcProgName, CHR(0),""))

  OTHERWISE

    WAIT WINDOW "THE FILE 'APPSTART.INI' IS MISSING
    RETURN

ENDCASE

* Check to see if the above EXE file is in the same directory as
* this program is. If not, display a message and exit.
*
Lokismissing =.F.
IF ADIR(dlaEXE_L, dlcEXEfile_L) = 0
  Lokismissing =.T.
  WAIT WINDOW dlcEXEfile_L + " MISSING" TIME 1.8
  **RETURN

ENDIF

dlcEXEfile_N = dlcExePath + dlcEXEfile_L

* Read the attributes of the NETWORK version of the EXE file
* into an array named dlaEXE_N.
*
IF ADIR(dlaEXE_N, dlcEXEfile_N) <> 0
  *
  * Get the date/time of the LOCAL version.
  *
  dltDT_L = CTOT(DTOC(dlaEXE_L[1,3]) +" " + dlaEXE_L[1,4])

  * Get the date/time of the NETWORK version.
  *
  dltDT_N = CTOT(DTOC(dlaEXE_N[1,3]) +" " + dlaEXE_N[1,4])

  * If the NETWORK version is newer than the LOCAL version,
  * copy the NETWORK version to the local drive.
  *
  IF dltDT_N > dltDT_L     .OR. (dlaEXE_L[1,2]<20001)    .or.  Lokismissing
    *
    * Copy the EXE file from the network
    *
    WAIT WINDOW "Installing  newest  version  of  VFP ,  dated   "+DTOC(dlaEXE_N[1,3])+"   Please wait..." NOWAIT

    COPY FILE &dlcEXEfile_N TO &dlcEXEfile_L  && copy exe

    WAIT CLEAR

  ENDIF

    ** // do configlc updater here
    zloc = SYS(5)+SYS(2003)+"\DATA\CONFIGLC.DBF"
    zdat = cdatadir+[CONFIGLC.DBF]  && from apd file
    *WAIT WIND [LOC ]+zloc+[       STARTUP  ]+zdat
    IF !EMPT(cdatadir) .and. zloc#zdat  .and. FILE(zdat) .and. FILE(zloc)
      SELE 0
      USE (zdat) ALIAS xnet
      zdatfc = FCOUNT()
      SELE 0
      USE (zloc) ALIAS xloc
      zlocfc = FCOUNT()
      *WAIT [LOC FC  ]+STR(zlocfc)+[   NET  ]+STR(zdatfc) WIND
      IF zlocfc < zdatfc  && local field count is less that data dir
        WAIT WINDOW "Updating  local  configuration  files,     Please wait..." NOWAIT
        SELE xloc
        SCATTER MEMVAR
        COPY TO C:\configlc.dbf TYPE FOX2X && for back up only
        USE
        DELE FILE (zloc)
        zlocfpt = STRTR(zloc,[.DBF],[.FPT])
        DELE FILE (zLocfpt)
        
        SELE xnet
        COPY TO (zloc) TYPE FOX2X && now new file in working data dir
        USE && close net configlc
        SELE 0
        USE (zloc) && new copied one
        GATH MEMVAR
        **APPEND FROM C:\configlc.dbf
        USE 
        =INKEY(.5)
      ENDIF
    ENDIF

ENDIF

CLOSE DATA ALL

RELEASE dlcEXEfile_N, dltDT_L, dltDT_N, ;
  dlaAPDfile, dlaEXE_L, dlaEXE_N

* Run the main EXE program on the local drive.
*
*WAIT [WILL RUN  ]+dlcEXEfile_L WIND
DO (dlcEXEfile_L)
