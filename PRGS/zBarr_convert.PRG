CLOSE DATA ALL
SELE 0
USE c:\downloads\vend_db4 ALIAS vs

SELE 0
USE c:\SVM\data\avvendor ALIAS zvends
ZAP

USE c:\SVM\data\vendcont IN 0
SELE vendcont
ZAP

SELE vs
SCAN
  SELE zvends
  APPEN BLANK
  REPL vmnem WITH ALLT(STR(vs.vendnum)), vname WITH vs.company, vcontact WITH vs.contact, vaddress WITH vs.address1, vaddress2 WITH vs.address2
  REPL vcity WITH vs.city, vstate WITH vs.state, vzip WITH vs.zip, vmemcomm WITH ALLT(vs.notes), vcountry WITH [USA]
  zcnt= 1
  SELE vs
  IF !EMPT(vs.phone1).and. ALLT(vs.phone1) # [(   )]
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH ALLT(zvends.vmnem), descrip WITH ALLT(vs.phonenote1), num WITH ALLT(vs.phone1)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(vs.phone2).and. ALLT(vs.phone2) # [(   )]
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH ALLT(zvends.vmnem), descrip WITH ALLT(vs.phonenote2), num WITH ALLT(vs.phone2)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
  IF !EMPT(vs.phone3).and. ALLT(vs.phone3) # [(   )]
    SELECT vendcont
    APPE BLANK
    REPLA vmnem WITH ALLT(zvends.vmnem), descrip WITH ALLT(vs.phonenote3), num WITH ALLT(vs.phone3)
    SELE zvends
    zcnt = zcnt + 1
  ENDIF
ENDSCAN
USE IN zvends
USE IN vendcont
USE IN vs

SELE 0
USE c:\downloads\invt_db4 ALIAS vs

SELE 0
USE c:\SVM\data\avpartss ALIAS zparts
ZAP

SELE 0
USE c:\SVM\data\serparts ALIAS zserials
ZAP

SELE vs
zsku = 1
SCAN
  SELE zparts
  APPE BLANK
  REPL ppartnum WITH ALLT(UPPE(vs.part_no)) ,pdescrip WITH ALLT(vs.desc) ,pqoh WITH vs.qty_onhand ;
    ,ploc WITH vs.location , sku WITH zsku
  zsku = zsku + 1
  DO CASE
    CASE !EMPT(vs.serial_no) .and. [NEW]$ vs.status
      REPL pclass WITH [NEW EQUIPMENT]
    CASE EMPT(vs.serial_no) .and. [NEW]$ vs.status
      REPL pclass WITH [NEW PARTS]
    OTHERWISE
      REPL pclass WITH [STANDARD]
  ENDCASE
  IF !EMPT(vs.serial_no)
    REPLACE zparts.pqoh WITH 1
    REPLACE zparts.serialized WITH .T.
    SELE zserials
    APPE BLANK
    REPLACE sku WITH zsku
    zsku = zsku + 1
    REPLACE pserial WITH vs.serial_no
    REPLACE pnum WITH zparts.ppartnum
    REPLACE intostk WITH DATE()
    REPL details WITH [Imported ]+DTOC(DATE())
    IF vs.qty_onhand < 1
      REPLACE outofstk WITH DATE()
    ENDIF
  ENDIF
ENDSCAN

close data
