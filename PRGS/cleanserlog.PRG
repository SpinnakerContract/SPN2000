CLOSE DATA ALL
USE F:\nacfox\qgen IN 0 ORDER genjob
USE F:\nacfox\serlog IN 0
*s12 = s12 + [Start R/C ]+STR(RECCOUN)
? 0
SELE serlog
SCAN
  *?? RECNO()
  IF SEEK(serlog.lotnum,[qgen],[genjob]) .and. qgen.prod_code = [MSC ]
    REPL serlog.endnum WITH 9 IN serlog
  ENDIF
  SELE serlog
ENDSCAN
