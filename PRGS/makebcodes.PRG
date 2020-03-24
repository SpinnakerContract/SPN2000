CLOSE DATA ALL
SELE 0
USE bcodes EXCL
ZAP
SELE 0
USE F:\NACFOX\serlog
SCAN FOR lotnum = [ 66976]
  APPEND BLANK IN bcodes
  REPL data WITH [1614]+CHR(9)+[S]+STRTR(STR(serlog.sernum,5),[ ],[0])+CHR(9);
    +UPPE(serlog.part)+CHR(9)+ALLT(UPPE(serlog.rev));
    ,line1 WITH [1614  S]+STRTR(STR(serlog.sernum,5),[ ],[0]);
    ,line2 WITH ALLT(UPPE(serlog.part));
    ,line3 WITH [Rev ]+ALLT(UPPE(serlog.rev))+[  Lot ]+ALLT(UPPE(serlog.lotnum)) IN bcodes
ENDSCAN
SCAN FOR lotnum = [ 67032]
  APPEND BLANK IN bcodes
  REPL data WITH [1614]+CHR(9)+[S]+STRTR(STR(serlog.sernum,5),[ ],[0])+CHR(9);
    +UPPE(serlog.part)+CHR(9)+ALLT(UPPE(serlog.rev));
    ,line1 WITH [1614  S]+STRTR(STR(serlog.sernum,5),[ ],[0]);
    ,line2 WITH ALLT(UPPE(serlog.part));
    ,line3 WITH [Rev ]+ALLT(UPPE(serlog.rev))+[  Lot ]+ALLT(UPPE(serlog.lotnum)) IN bcodes
ENDSCAN
