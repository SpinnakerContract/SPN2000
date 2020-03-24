*//  recurses avm data dir and builds table of files NOT contained in the standard Data Dictionary
CLOSE DATA ALL
SET SAFETY OFF
CREATE DBF c:\SVM\Non_DD_File_List ( flag1 L(1), flag2 L(1), filename c(30), extension c(3) )
ERASE C:\SVM\DATA\*.BAK
ERASE C:\SVM\DATA\*.TBK
SELE 0
USE savmdd1 NOUP
OK=.T.
Fcnt = 1
DO WHILE OK
  IF Fcnt = 1
    wtd = ALLT(UPPE(SYS(2000,"C:\SVM\DATA\*.*")))
  ELSE
    wtd = ALLT(UPPE(SYS(2000,"C:\SVM\DATA\*.*",1)))
  ENDIF
  IF LEFT(wtd,4) = [SAVM] .or.[_EL60]$wtd.or.[.FOX]$wtd.or.[.DLL]$wtd.or.[GKC.FXP]$wtd.or.[MTEST.DBF]$wtd .or. ([_]#wtd.and.[.EXE]$wtd)
    LOOP
  ENDIF
  Fcnt = Fcnt + 1
  IF EMPT(wtd)
    OK=.F.
    LOOP
  ENDIF
  jExt = RIGHT(wtd,3)
  Srch = STRTR(wtd,[.]+jExt,[.DBF]) && so non dbf files can still be found
  SELE savmdd1
  LOCATE FOR Srch $ UPPE(file_name)
  IF !FOUND() 
    SELE Non_DD_File_List 
    APPEND BLANK
    REPL filename WITH wtd, extension WITH jExt, flag1 WITH .T.
  ENDIF
ENDDO
USE IN savmdd1
SELE Non_DD_File_List 
INDEX ON extension+filename TAG e_f
*INDEX ON filename+extension TAG f_e
GO TOP
BROW
SET SAFETY OFF
SCAN FOR Non_DD_File_List.flag1
  xFile = ALLT(filename)
  IF FILE("C:\SVM\DATA\"+xFile)
    COPY FILE "C:\SVM\DATA\"+xFile TO "C:\SVM\TEMP\"+xFile
    ERASE "C:\SVM\DATA\"+xFile
  ENDIF
ENDSCAN