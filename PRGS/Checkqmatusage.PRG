** 09Sep2014 check if any qmat entries have usage where the corresponding qmat_s doesn't
** which could be the result of Laura using  BOM Mass Updater.
** BOM mass updater updated to always blank out the usage in the target BOM's
CLOSE DATA ALL
SELE 0
USE F:\NACFOX\qmat_s ORDER job_sku
SELE 0
USE F:\NACFOX\qmat ORDER job_mfg
SELE 0
USE F:\NACFOX\qgen
SELE qgen
PUBLIC lotstr
lotstr = []
SCAN FOR qgen.bal>0
  SELE qmat
  SET KEY TO qgen.job
  SEEK qgen.job
  SCAN 
    IF used+qvused>0 
      SELE qmat_s
      SET KEY TO qgen.job
      SEEK qgen.job
      n1=0
      SCAN
        n1 = n1 + 1
      ENDSCAN
      SELE qmat
      IF n1=0
        lotstr = lotstr + qgen.job+[,]
        ? qgen.job+CHR(13)
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN
