zLSwitchGL1 = .T.
SET SAFETY OFF
SET TALK ON
SET DELE ON
SET CENT ON
SET EXCL ON
CLOSE DATA ALL
IF 1=1  && ONLY for testing, don't put into UPDATER
  *// update codes in avinacct, avajacct, avpoacct, ccmacct, rcinacct, vadjacct
  *// avchartp, avmonthl, glaccnts, glmonth, glsegmnt
*!*	  SELE 0
*!*	  USE glaccnts
*!*	  ZAP
*!*	  USE IN glaccnts  
*!*	  SELE 0
*!*	  USE glsegmnt
*!*	  ZAP
*!*	  USE IN glsegmnt  
*!*	  SELE 0
*!*	  USE glmonth
*!*	  ZAP
*!*	  USE IN glmonth
  SELE 0
  USE banks
  ZAP
  APPE FROM n:\ctiavm2000\data\banks
  USE  

  SELE 0
  USE dpinvlnk
  ZAP
  APPE FROM n:\ctiavm2000\data\dpinvlnk
  USE  

  SELE 0
  USE chkpelnk
  ZAP
  APPE FROM n:\ctiavm2000\data\chkpelnk
  USE  

  SELE 0
  USE depdet
  ZAP
  APPE FROM n:\ctiavm2000\data\depdet
  USE  

  SELE 0
  USE checkdet
  ZAP
  APPE FROM n:\ctiavm2000\data\checkdet
  USE  

  SELE 0
  USE checkbk
  ZAP
  APPE FROM n:\ctiavm2000\data\checkbk
  USE  

  SELE 0
  USE avvahist
  ZAP
  APPE FROM n:\ctiavm2000\data\avvahist
  USE  

  SELE 0
  USE avvendor
  ZAP
  APPE FROM n:\ctiavm2000\data\avvendor
  USE  

  SELE 0
  USE vendcont
  ZAP
  APPE FROM n:\ctiavm2000\data\vendcont
  USE  

  SELE 0
  USE avcahist
  ZAP
  APPE FROM n:\ctiavm2000\data\avcahist
  USE  

  SELE 0
  USE avcustom
  ZAP
  APPE FROM n:\ctiavm2000\data\avcustom
  USE  

  SELE 0
  USE custcont
  ZAP
  APPE FROM n:\ctiavm2000\data\custcont
  USE  

  SELE 0
  USE glsegmnt
  ZAP
  APPE FROM n:\ctiavm2000\data\glsegmnt
  USE  

  SELE 0
  USE glsegmnt
  ZAP
  APPE FROM n:\ctiavm2000\data\glsegmnt
  USE  

  SELE 0
  USE glaccnts
  ZAP
  APPE FROM n:\ctiavm2000\data\glaccnts
  USE  

  SELE 0
  USE gltrans
  ZAP
  APPE FROM n:\ctiavm2000\data\gltrans
  USE  

  SELE 0
  USE avchartp
  ZAP
  APPE FROM n:\ctiavm2000\data\avchartp
  USE  

  SELE 0
  USE avmonthl
  ZAP
  APPE FROM n:\ctiavm2000\data\avmonthl
  USE  

  SELE 0
  USE avinacct
  ZAP
  APPEN FROM n:\ctiavm2000\data\avinacct
  USE  
  SELE 0
  USE avajacct
  ZAP
  APPEN FROM n:\ctiavm2000\data\avajacct
  USE  
  SELE 0
  USE avpoacct
  ZAP
  APPEN FROM n:\ctiavm2000\data\avpoacct
  USE  
  SELE 0
  USE ccmacct
  ZAP
  APPEN FROM n:\ctiavm2000\data\ccmacct
  USE  
  SELE 0
  USE rcinacct
  ZAP
  APPEN FROM n:\ctiavm2000\data\rcinacct
  USE  
  SELE 0
  USE vadjacct
  ZAP
  APPEN FROM n:\ctiavm2000\data\vadjacct
  USE  

  SELE 0
  USE avinvoic
  ZAP
  APPEN FROM n:\ctiavm2000\data\avinvoic
  USE  

  SELE 0
  USE avipused
  ZAP
  APPEN FROM n:\ctiavm2000\data\avipused
  USE  

  SELE 0
  USE avpayble
  ZAP
  APPEN FROM n:\ctiavm2000\data\avpayble
  USE  

  SELE 0
  USE avpoline
  ZAP
  APPEN FROM n:\ctiavm2000\data\avpoline
  USE  

  SELE 0
  USE custadjs
  ZAP
  APPEN FROM n:\ctiavm2000\data\custadjs
  USE  

  SELE 0
  USE custcmem
  ZAP
  APPEN FROM n:\ctiavm2000\data\custcmem
  USE  

  SELE 0
  USE vendadjs
  ZAP
  APPEN FROM n:\ctiavm2000\data\vendadjs
  USE  

  CLOSE DATA ALL
ENDIF
