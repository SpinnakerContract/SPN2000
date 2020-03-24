** 31Jan2012 to send gen email with attchment
PARA qto,qfrom,qreply,qsubject,qbody,qkeep,qattach
qqFile=[ATTACH]+RIGHT(SYS(2015),10)
jPedro = [pete@tangoware.com]
qto   =IIF(EMPT(qto),[pete@tangoware.com],qto)
qfrom =IIF(EMPT(qfrom),[peter@spinnakercontract.com],qfrom)
qreply=IIF(EMPT(qreply),[pete@tangoware.com],qreply)
SET SAFETY OFF
CREATE DBF C:\nacvfp\temp\tmpqem2 ( to c(100), from c(100), reply c(100);
 , subject c(150), body m(10), keep l(1), sent c(20), attachment C(100) )
SELE tmpqem2
APPE BLANK
REPL tmpqem2.to WITH ALLT(qto), tmpqem2.from WITH ALLT(qfrom), tmpqem2.reply WITH ALLT(qreply),;
  tmpqem2.subject WITH ALLT(qsubject),;
  tmpqem2.body WITH ALLT(qbody), tmpqem2.attachment WITH qattach IN tmpqem2   &&+CHR(13)+CHR(10)+qqFile
COPY TO [F:\RFQ\RFQ_OUT\]+qqFile
USE IN tmpqem2

*DO sndemail WITH [pete@tangoware.com],[peter@spinnakercontract.com],[peter@spinnakercontract.com],[Test Gen Email],[Test Body of Email here],.F.
