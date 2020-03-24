** 22Jan07 this ver for SPN2000
PARA qto,qfrom,qreply,qsubject,qbody,qkeep
qqFile=[EMAIL]+RIGHT(SYS(2015),3)
jPedro = [pete@tangoware.com]
qto   =IIF(EMPT(qto),[pete@tangoware.com],qto)
qfrom =IIF(EMPT(qfrom),[peter@spinnakercontract.com],qfrom)
qreply=IIF(EMPT(qreply),[pete@tangoware.com],qreply)
SET SAFETY OFF
CREATE DBF C:\nacvfp\temp\tmpqem ( to c(100), from c(100), reply c(100);
 , subject c(150), body m(10), keep l(1), sent c(20) )
SELE tmpqem
APPE BLANK
REPL tmpqem.to WITH ALLT(qto), tmpqem.from WITH ALLT(qfrom), tmpqem.reply WITH ALLT(qreply),;
  tmpqem.subject WITH ALLT(qsubject),;
  tmpqem.body WITH ALLT(qbody)+CHR(13)+CHR(10)+qqFile
COPY TO [F:\RFQ\RFQ_OUT\]+qqFile
USE IN tmpqem

*DO sndemail WITH [pete@tangoware.com],[peter@spinnakercontract.com],[peter@spinnakercontract.com],[Test Gen Email],[Test Body of Email here],.F.
