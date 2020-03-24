CLOSE DATA
SET EXCL ON
SELE 0
USE C:\ATC_SPN\DATA\avempl_m EXCL
SELE 0
USE C:\SPN\TEMP\Employee_Info_2 ALIAS EI
SCAN
  SELE avempl_m
  APPE BLANK
  REPL emnem WITH ALLT(ei.empno)
  REPL f_name WITH ei.first
  REPL l_name WITH ei.last
  REPL fullname WITH ALLT(ei.first)+[ ]+ALLT(ei.last)
  REPL comments WITH ALLT(ei.address)+CHR(13)+CHR(10)+ALLT(ei.emergency_)
  REPL date_hire WITH ei.doh
  REPL dvac_start WITH GOMO(ei.doh,12)
ENDSCAN