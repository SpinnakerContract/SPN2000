CLOSE DATA
SELE 0
USE C:\SVM\data\auxparts
*USE n:\SVM_459\data\auxparts
SELE 0
USE C:\SVM\data\avpartss
*USE n:\SVM_459\data\avpartss
j=0
SCAN
  IF SEEK(avpartss.ppartnum,[auxparts],[auxpnum])
    IF auxparts.plist # avpartss.plist
      ? STR(j,4)+[  ]+STR(avpartss.sku,8)+[     ]+avpartss.ppartnum+[  ]+STR(avpartss.plist,14,2)+STR(auxparts.plist,14,2)
      j = j + 1
    ENDIF
  ENDIF
ENDSCAN