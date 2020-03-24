CLOSE DATA ALL
SET EXCL ON
CREATE DBF C:\nsidupl (mfg C(25),misc C(100),sku N(10))
USE F:\nacfox\inv IN 0
SELE inv
DELETE ALL FOR RECNO()>43522
PACK
SET ORDER TO invmfg IN inv
USE F:\nacfox\batchprt IN 0
SELE batchprt
DELETE ALL FOR RECNO()>125806
PACK
SET ORDER TO invmfg
USE F:\nacfox\parthist IN 0
SELE parthist
DELETE ALL FOR RECNO()>759630
PACK
SET ORDER TO mfg
USE F:\nacfox\nsihist  IN 0 ORDER mfg
APPEND BLANK IN inv
SELE nsihist
APPEN BLANK IN batchprt
SCAN
  IF SEEK(nsihist.partnum,[inv],[invmfg])
    REPLACE lnsi WITH .T., cat WITH [NSI] IN inv
    APPEND BLANK IN nsidupl
    REPL mfg WITH nsihist.partnum, misc WITH [nsi found in INV],sku WITH nsihist.sku  IN nsidupl

    APPEN BLANK IN batchprt
    REPL mfg WITH nsihist.partnum,qty_rec WITH nsihist.qty,qty_used WITH nsihist.qty,sku WITH nsihist.sku,lnsi WITH .T.,rdate WITH nsihist.date,dexpire WITH nsihist.dexpire,notes WITH [NSI COMBINED TO INVT] IN batchprt
    REPL leadfree WITH nsihist.rxleadfree ,ul WITH nsihist.rxul,csa WITH nsihist.rxcsa, cec WITH nsihist.rxcec, rohs WITH nsihist.rxrohs, buyam WITH nsihist.rxbuyam IN batchprt

    APPEN BLANK IN parthist
    REPL partnum WITH nsihist.partnum,sku WITH nsihist.sku, ref_misc WITH [NSI combined to std Inv (qty=]+ALLT(STR(nsihist.qty,14,2))+[)],lnsi WITH .T. IN parthist
  ELSE
    APPEND BLANK IN inv
    REPL mfg WITH nsihist.partnum,lnsi WITH .T.,cat WITH [NSI] IN inv

    APPEN BLANK IN batchprt
    REPL mfg WITH nsihist.partnum,qty_rec WITH nsihist.qty,qty_used WITH nsihist.qty,sku WITH nsihist.sku,rdate WITH nsihist.date,dexpire WITH nsihist.dexpire,notes WITH [NSI ADDED TO INVT] IN batchprt
    REPL leadfree WITH nsihist.rxleadfree ,ul WITH nsihist.rxul,csa WITH nsihist.rxcsa, cec WITH nsihist.rxcec, rohs WITH nsihist.rxrohs, buyam WITH nsihist.rxbuyam IN batchprt

    APPEN BLANK IN parthist
    REPL partnum WITH nsihist.partnum,sku WITH nsihist.sku, ref_misc WITH [NSI ADDED to Inv (qty=]+ALLT(STR(nsihist.qty,14,2))+[)] IN parthist
  ENDIF
  SELE nsihist
ENDSCAN
SELE nsidupl
LOCATE
BROW

TEXT
batchprt PO ,SKU ,LOT ,MFG ,LOC ,UTNUM ,UCOST ,QTY_REC ,;
QTY_USED ,RDATE ,VCODE ,UM ,MCODE ,QUOTE ,MRB ,USE_NOW ,;
AMORT ,LEADFREE ,UL ,CSA ,CEC ,ROHS ,BUYAM ,NOTES ,;
VENDOWNED ,KEY1 ,FLAG1 ,TRACEABLE ,CHOLD ,NHOLD ,DEXPIRE ,LNSI ,;

parthist PARTNUM ,DATE ,QTY ,REF_MISC ,UCOST ,TIME ,UTNUM ,SKU ,;
VENDOWNED ,LOT ,MDR_CODE ,LATTRIT ,KEY1 ,FLAG1 ,PREVMFG ,LQSMTSCAN ,;
DELTACOST ,DELTA_QTY ,USERID ,KITJOB ,CAEQTY ,CAE ,DEXPIRE ,LNSI ,;

nsihist PARTNUM ,DATE ,QTY ,REF_MISC ,UCOST ,TIME ,UTNUM ,SKU ,;
FLAG1 ,USERID ,PO ,DEXPIRE ,LVSLAVAIL ,LEADFREE ,ROHS ,UL ,;
CSA ,CEC ,BUYAM ,RXLEADFREE ,RXROHS ,RXUL ,RXCSA ,RXCEC ,;
RXBUYAM ,MSDS_FLAM ,MSDS_REAC ,MSDS_HEAL ,MSDS_PROT ,

inv VCODE ,NACPART ,MFG ,DESC ,UCOST ,ONHAND ,ALLOC ,ORDER ,;
CAT ,LOC ,MINORDER ,UM ,OLD_MCODE ,NEW_MFG ,NEW_DESC ,LEAD ,;
VENDORS ,OLD_OH ,INSP_REQ ,LASTUSED ,LASTPURCH ,LASTPO ,EXT_COST ,LAST_VEND ,;
LAST_CNT ,MRB ,PROD_CODE ,O_TOT ,P_TOT ,NO_EXT ,TOT_REQD ,CLASS ,;
NOTES ,NEW_LOC ,PRINTED ,CUSTOMER ,MCODE ,RECQTY ,STOCKQTY ,STATUS ,;
CRIMP_CNT ,TOTAL_CC ,LAST_MAINT ,REDO_DESC ,DESC1 ,DESC2 ,DESC3 ,DESC4 ,;
DESC5 ,DESC6 ,AVGCOST ,CC_REQ ,DNOHOME1 ,NNOHOME1 ,NNH_REQ1 ,NNH_OH1 ,;
DNOHOME2 ,NNOHOME2 ,NNH_REQ2 ,NNH_OH2 ,DNOHOME3 ,NNOHOME3 ,NNH_REQ3 ,NNH_OH3 ,;
DNOHOME4 ,NNOHOME4 ,NNH_OH4 ,NNH_REQ4 ,MARK ,LAST_CNT2 ,DWHOME ,NWHOME ,;
LASTCCUSR ,SMT_TYPE ,NHOLD ,LVSL ,MSDS_FLAM ,MSDS_REAC ,MSDS_HEAL ,MSDS_PROT ,;
LNSI ,

ENDTEXT