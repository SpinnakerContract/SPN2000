CLOSE DATA
SELE 0
USE savmdind ORDER file_name
SELE 0
USE savmdd1 ORDER 1
SCAN
  SELE savmdind
  SEEK LEFT(savmdd1.file_name,230)
  zIdxTxt = []
  SCAN WHILE LEFT(savmdind.file_name,230) = LEFT(savmdd1.file_name,230)
    zIdxTxt = zIdxTxt + tag_name+[  ]+LEFT(index_expr,60);
      +[  ]+LEFT(for_expr,70)+CHR(13)
  ENDSCAN
  SELE savmdd1
  ? zIdxTxt
  REPLACE savmdd1.cti_memo WITH zIdxTxt
ENDSCAN