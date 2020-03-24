*LPARAM zRootDrive
*zRootDrive = [C:\]
CLEAR
*zDefDrive =SYS(5)
CREATE CURSOR zDrives (drive c(2), status c(15))
FOR jjj = 67 TO 90
  zRootDrive = CHR(jjj)+[:\]
  zWriteTestFile = ALLT(zRootDrive)+[AVMOK.DBF]
  @ jjj-66,2 SAY zRootDrive
  *WAIT zRootDrive + zWriteTestFile WIND
  IF 111 = 111
    ON ERROR zzerrnum = ERROR()
    zzerrnum = 0
    CREATE DBF &zWriteTestFile (test c(1))
    IF zzerrnum # 0 .or. !FILE(zWriteTestFile)
      @ jjj-66,6 SAY zRootDrive+[  Error (]+STR(zzerrnum,4)+[)]
      SELE zDrives
      APPEN BLANK
      REPLA drive WITH zRootDrive, status WITH [Not Available]
    ELSE
      USE IN avmok
      DELE FILE &zWriteTestFile
      ERASE (zWriteTestFile)
      SELE zDrives
      APPE BLANK
      REPLA drive WITH zRootDrive, status WITH [OK]
      *CD (zRootDrive)
      @ jjj-66,6 SAY zRootDrive+[  Appears OK  ]&&+TRAN(DISKSPACE()/1000,[999,999,999])
      *SET DEFA TO (zDefDrive)
    ENDIF
  ENDIF
ENDFOR
SET DEFA TO (zDefDrive)
SELE zDrives
BROW FIELD drive:5:H=[DRIVE],status:15:H=[STATUS] NOEDIT NOAPPE NODELE FONT "Lucida Console",9
