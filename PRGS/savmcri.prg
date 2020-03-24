******************************************************************
*      	ProMatrix Corporation                                
*                                                             
*      	Copyright © 1995-1998 ProMatrix Corporation    
*         
*      	5225 Ehrlich Road, Suite C                                
*      	Tampa, FL  33624                                        
*      	U.S.A.   
*
*	This computer program is protected by copyright law and 
*	international treaties.  No part of this program may be 
*	reproduced or transmitted in any form or by any means, 
*	electronic or mechanical, for any purpose, without the 
*	express written permission of ProMatrix Corporation.  
*	Unauthorized reproduction or distribution of this program, 
*	or any portion of it, may result in severe civil and 
*	criminal penalties, and will be prosecuted to the maximum 
*	extent possible under the law.  
*                                       
******************************************************************

**************
* S<Prefix>CRI - Check the referential integrity of the record that has
*                been  added, changed or deleted.  If the record passes
*                the duplicate key and referential integrity checks, add
**************   records to the audit trail table.

* Values returned by this program are:
* 0 - Passed all checks
* 1 - Failed duplicate key check
* 2 - Failed RI check
* 3 - Primary key field has null or blank value

#INCLUDE "MAIN.H"

LPARAM stcAction,stcMode,stlNoRICheck,stlNoAuditTrail,slnAudWA

LOCAL slnFor,slnNumAud,slcTime,sllAuditOK,slnElem

LOCAL ARRAY slaAud(1,10)

PRIVATE sxnCurArea

* Parameter stcAction: Start, OK, Cancel
IF EMPTY(stcAction)
   IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
      _SCREEN.ActiveForm.cRIMessage = 'There was an invalid parameter '+;
         'passed to the Referential Integrity routine.'
   ENDIF
   RETURN 2
ENDIF

sxnCurArea = SELECT()

IF ALLTRIM(UPPER(stcAction)) == 'OK'		&& Save RI changes
   IF TYPE('sgnRIUpd') <> 'U' AND TYPE('sgaRIUpd') <> 'U' AND ;
      USED('riworkfile')
      IF sgnRIUpd > 0
         FOR slnFor = 1 TO sgnRIUpd
            IF USED(sgaRIUpd(slnFor,2))
               SELECT (sgaRIUpd(slnFor,2))
               IF TXNLEVEL() = 0
                  = TABLEUPDATE(.T.,.T.)
               ENDIF
               USE
            ENDIF
         ENDFOR
      ENDIF
      * Add records to the audit trail table.
      sllAuditOK = .F.
      = EVAL(sgcUPsh+"('EXACT','OFF')")
      slnElem = ASCAN(sgaTools,'AUDIT')
      = EVAL(sgcUPop+"('EXACT')")
      IF slnElem > 0
         * If an audit trail row was found in the array sgaTools, this
         * means that the audit trail was chosen to be "included" in the
         * application by the developer.
         IF sgaTools(slnElem+2)
            * Tool is set "on" by user.
            sllAuditOK = .T.
         ENDIF
      ENDIF
      IF sllAuditOK
         SELECT riworkfile
         oApp.First(SET('DATASESSION'))
         IF NOT EOF()
            IF EVAL(sgcUUse+"(sgcAudDBF)")
               slnAudWA = SELECT()
               slnNumAud = 0
               slcTime = TIME()
               SELECT riworkfile
               SCAN FOR riworkfile.audit_trl
                  slnNumAud = slnNumAud+1
                  DIMENSION slaAud(slnNumAud,10)
                  slaAud(slnNumAud,1) = oSec.cCurrentUserID
                  slaAud(slnNumAud,2) = sgcCurPre
                  slaAud(slnNumAud,3) = DATE()
                  slaAud(slnNumAud,4) = slcTime
                  slaAud(slnNumAud,5) = riworkfile.action
                  slaAud(slnNumAud,6) = riworkfile.key_value
                  IF EMPTY(riworkfile.view_table)
                     slaAud(slnNumAud,7) = ALLTRIM(SUBSTR(riworkfile.file_name,;
                        RAT('\',riworkfile.file_name)+1))
                  ELSE
                     slaAud(slnNumAud,7) = ALLTRIM(SUBSTR(riworkfile.view_table,;
                        RAT('\',riworkfile.view_table)+1))
                  ENDIF
                  IF EMPTY(riworkfile.view_field)
                     slaAud(slnNumAud,8) = ALLTRIM(riworkfile.field_name)
                  ELSE
                     slaAud(slnNumAud,8) = ALLTRIM(riworkfile.view_field)
                  ENDIF
                  slaAud(slnNumAud,9) = riworkfile.old_value
                  slaAud(slnNumAud,10) = riworkfile.new_value
               ENDSCAN
               IF slnNumAud > 0
                  SELECT (slnAudWA)
                  APPEND FROM ARRAY slaAud
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      * Close/delete the RI work file.
      USE IN riworkfile
      SELECT (sxnCurArea)
      RELEASE sgnRIUpd,sgaRIUpd
   ENDIF
   RETURN 0
ENDIF

IF ALLTRIM(UPPER(stcAction)) == 'CANCEL'	&& Cancel RI changes
   IF TYPE('sgnRIUpd') <> 'U' AND TYPE('sgaRIUpd') <> 'U' AND ;
      USED('riworkfile')
      = siRICancel()
   ENDIF
   RETURN 0
ENDIF

LOCAL slcMode,sllNoRICheck,slnModifiedFields,sllDisplayWaitWindow,;
   slnModifiedWorkarea,slcModifiedWithPath,slcModifiedWithoutPath,;
   slcModifiedAlias,slcOriginalModifiedWithPath,slcDD1,slcDD2,slcDDToUse,;
   slcPKString,slnVFNum,slnComma,sllMore,slnComma1,slnComma2,slnRecNo,;
   slcKeyFld,slnFor,slcCurField,slOldValue,slcFieldType,slnSave_ridd2_RecNo,;
   slnFieldDecimals,slnFieldWidth,slcTables,slcVFields,slcVField,slnSpace,;
   slnExcl,slnDot,slcVFld,slcVAlias,slcActualFld,slcCS,slcActualTable,;
   slnCommaX,sllMoreX,slnCommaY,sllMoreY,slnWorkFileRecs,slNewValue,;
   slcOldValue,slcNewValue,slnFor2,slnNumCur,slcSave_riworkfile_file_name,;
   slcPriFld,slcSave_ridd1_primaryfld,sllDoRICheck,sllFirstField,;
   slnModifiedRecNo,slnSave_riworkfile_RecNo,slcSave_riworkfile_action,;
   slcModifiedDDFile_Name,slcModifiedDDField_Name,slcSave_ridd2_val_data,;
   slnQuote1,slnQuote2,slcPossibleChildTable,sllMatched,slcFor,slSeekValue,;
   slcForFlds,slcFK,slcPKFlds,slnUpdCtrl,slnDelCtrl,slcRTName,slcRTFor,;
   slcEqual,slcOldVal,slcNewVal,slcFld,slcValFld,sllOpen,slnNumFlds,slcForFld,;
   slcPriFlds,slcPK,sllAddToArray,slcRTAlias,slcRTDBFP,slcRTDBF,slcRTKeyExpr,;
   slnRTFCount,slcRTField,slcAddToExpr,slcRTKeyVal,slnRTRec,slRTOldVal,;
   slcRTFType,slRTNewVal,slnRTDec,slcRTOldVal,slcRTNewVal,slnWidth,slcRTFld,;
   slcRTFile,sllOK,sllCancelRICheck,sllKeyFldChanged,slcPrimaryTag,slcCurTag,;
   slcSeekExpr,sllUpdate1,sllUpdate2,sllUpdate3,sllUpdate4,sllUpdate5,;
   sllUpdate6,sllUpdate7,sllUpdate8,sllUpdate9,slVal1,slVal2,slVal3,slVal4,;
   slVal5,slVal6,slVal7,slVal8,slVal9,slNewVal1,slNewVal2,slNewVal3,slNewVal4,;
   slNewVal5,slNewVal6,slNewVal7,slNewVal8,slNewVal9,slcPKFld1,slcPKFld2,;
   slcPKFld3,slcPKFld4,slcPKFld5,slcPKFld6,slcPKFld7,slcPKFld8,slcPKFld9,;
   slcFKFld1,slcFKFld2,slcFKFld3,slcFKFld4,slcFKFld5,slcFKFld6,slcFKFld7,;
   slcFKFld8,slcFKFld9,slnParentTableNo,slcParentAlias,sllOldNothing,;
   sllNewNothing,slKeyFldValue,slcKeyFldValue,slcPKFields,sllDel_AllFld

LOCAL ARRAY slaRef(1,13),slaVF(1,3)

PRIVATE sxlDD2Found,sxlAudit,sxlNoAuditTrail

IF TYPE('sgnRIUpd') = 'U'
   PUBLIC sgnRIUpd
ENDIF
sgnRIUpd = 0
IF TYPE('sgaRIUpd') = 'U'
   PUBLIC ARRAY sgaRIUpd(1,2)
ENDIF

* Parameter stcMode: Delete, New, Change
IF EMPTY(stcMode)
   IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
      _SCREEN.ActiveForm.cRIMessage = 'There was an invalid parameter '+;
         'passed to the Referential Integrity routine.'
   ENDIF
   RETURN 2
ENDIF
slcMode = ALLTRIM(UPPER(stcMode))

* Parameter stlNoRICheck: .T. (perform check), .F. (do not perform check)
IF EMPTY(stlNoRICheck)
   sllNoRICheck = .F.
ELSE
   sllNoRICheck = .T.
ENDIF

* Parameter stlNoAuditTrail: .T. (add records), .F. (do not add records)
IF EMPTY(stlNoAuditTrail)
   sxlNoAuditTrail = .F.
ELSE
   sxlNoAuditTrail = .T.
ENDIF

slcModifiedAlias = ALIAS()
slnModifiedFields = FCOUNT()
slcModifiedWithPath = ALLTRIM(DBF())
slcModifiedWithoutPath = SUBSTR(slcModifiedWithPath,;
   RAT('\',slcModifiedWithPath)+1)
slnModifiedWorkarea = sxnCurArea
slcOriginalModifiedWithPath = slcModifiedWithPath
sllDisplayWaitWindow = .T.
sxlAudit = .F.
sxlDD2Found = .F.

* Open the DD1 and DD2 tables.
IF sgcAppPre == 'APP'
   IF sglTestForm
      sxlNoAuditTrail = .T.				&& No audit trail while testing
      slcDD1 = sgcPDD1
      slcDD2 = sgcPDD2
      slcDDToUse = 'PDD'
   ELSE
      slcDD1 = 'SAPPDD1'
      slcDD2 = 'SAPPDD2'
      slcDDToUse = ''
   ENDIF
ELSE
   slcDD1 = sgcPDD1
   slcDD2 = sgcPDD2
   slcDDToUse = ''
ENDIF
IF NOT EVAL(sgcUUse+"(slcDD2,'ridd2','AGAIN','','',slcDDToUse)")
   SELECT (sxnCurArea)
   IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
      _SCREEN.ActiveForm.cRIMessage = ;
         'The table '+slcDD2+' could not be opened.'
   ENDIF
   RETURN 2
ENDIF
oApp.SetOrder(ALIAS(),SET('DATASESSION'),'file_name')
IF NOT EVAL(sgcUUse+"(slcDD1,'ridd1','AGAIN','','',slcDDToUse)")
   USE IN ridd2
   SELECT (sxnCurArea)
   IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
      _SCREEN.ActiveForm.cRIMessage = ;
         'The table '+slcDD1+' could not be opened.'
   ENDIF
   RETURN 2
ENDIF
oApp.SetOrder(ALIAS(),SET('DATASESSION'),'file_name')

* Create a character string of the key field value(s) from the current
* record.  The string is placed in audit trail records to identify the
* record where the changes were made.
slcPKString = ''
slcPKFields = ''
sllDel_AllFld = .F.
slnVFNum = 0
IF CURSORGETPROP('SourceType',sxnCurArea) = 3
   * Current table is a free table or a table in a database.
   IF SEEK(PADR(LEFT(slcModifiedWithoutPath,112),112))
      sllKeyFldChanged = .F.
      slcPKFlds = ''
      IF NOT EMPTY(ridd1.primaryfld)
         sllDel_AllFld = ridd1.del_allfld
         IF EMPTY(ridd1.primaryfld)
            slcPKFields = ''
         ELSE
            slcPKFields = ','+ALLTRIM(UPPER(ridd1.primaryfld))+','
         ENDIF
         SELECT (sxnCurArea)
         slnComma = 0
         sllMore = .T.
         DO WHILE sllMore
            slnComma = slnComma+1
            slnComma1 = AT(',',slcPKFields,slnComma)
            slnComma2 = AT(',',slcPKFields,slnComma+1)
            IF slnComma1 = 0 OR slnComma2 = 0
               EXIT
            ENDIF
            slcKeyFld = ALLTRIM(SUBSTR(slcPKFields,slnComma1+1,;
               slnComma2-slnComma1-1))
            FOR slnFor = 1 TO slnModifiedFields
               slcCurField = FIELD(slnFor)
               IF slcKeyFld == slcCurField
                  IF slcMode = 'NEW'
                     * For new records, the key value will be taken
                     * from the buffer, not the disk.
                     slKeyFldValue = EVALUATE(slcCurField)
                     sllKeyFldChanged = .T.
                  ELSE
                     * For changed and deleted records, the key value
                     * will be taken from the disk, not the buffer.
                     slKeyFldValue = OLDVAL(slcCurField)
                     IF NOT EVALUATE(slcCurField) == slKeyFldValue
                        * Key field value has changed.
                        sllKeyFldChanged = .T.
                     ENDIF
                  ENDIF
                  * Find the field's record in DD2.
                  = siFindDD2Rec(slcModifiedWithoutPath,slcCurField)
                  IF sxlDD2Found AND NOT EMPTY(ridd2.label)
                     slcPKFlds = slcPKFlds+ALLTRIM(ridd2.label)+CRLF
                  ELSE
                     slcPKFlds = slcPKFlds+PROPER(slcCurField)+CRLF
                  ENDIF
                  IF ISNULL(slKeyFldValue)
                     slcKeyFldValue = ''
                  ELSE
                     slcFieldType = TYPE('slKeyFldValue')
                     DO CASE
                        CASE slcFieldType $ 'CM'
                           slcKeyFldValue = slKeyFldValue
                        CASE slcFieldType = 'N'
                           * Get width and decimals from DD2.
                           IF sxlDD2Found
                              slnFieldDecimals = ridd2.decimals
                              DO CASE
                                 CASE ridd2.type = 'B'
                                    slnFieldWidth = 20
                                 CASE ridd2.type = 'F'
                                    slnFieldWidth = 20
                                 CASE ridd2.type = 'I'
                                    slnFieldWidth = 10
                                 OTHERWISE
                                    slnFieldWidth = ridd2.width
                              ENDCASE
                           ELSE
                              slnFieldWidth = 20
                              slnFieldDecimals = 7
                           ENDIF
                           slcKeyFldValue = ALLTRIM(STR(slKeyFldValue,;
                              slnFieldWidth,slnFieldDecimals))
                        CASE slcFieldType = 'Y'
                           slcKeyFldValue = STR(MTON(slKeyFldValue),20,4)
                        CASE slcFieldType = 'D'
                           slcKeyFldValue = DTOC(slKeyFldValue)
                        CASE slcFieldType = 'T'
                           slcKeyFldValue = TTOC(slKeyFldValue)
                        CASE slcFieldType = 'L'
                           IF slKeyFldValue
                              slcKeyFldValue = 'T'
                           ELSE
                              slcKeyFldValue = 'F'
                           ENDIF
                        OTHERWISE
                           slcKeyFldValue = ''
                     ENDCASE
                  ENDIF
                  slcPKString = slcPKString+ALLTRIM(slcKeyFldValue)+','
                  EXIT
               ENDIF
            ENDFOR
         ENDDO
         IF NOT EMPTY(slcPKString)
            * Remove the trailing comma.
            slcPKString = LEFT(slcPKString,LEN(slcPKString)-1)
         ENDIF
      ENDIF
      IF sllKeyFldChanged AND slcMode <> 'DELETE'
         * For new records and records whose key field(s) were changed,
         * perform the blank and duplicate PK checks.
         SELECT ridd1
         IF EMPTY(ridd1.primarytag) OR ;
            ALLTRIM(UPPER(ridd1.primarytag)) == 'NONE'
            * Cannot perform blank and duplicate PK checks if a primary
            * key tag has not been specified.
            IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
               _SCREEN.ActiveForm.cRIMessage = slcPKFlds
               IF NOT EMPTY(ridd1.desc)
                  _SCREEN.ActiveForm.cRIMessage = ;
                     _SCREEN.ActiveForm.cRIMessage+;
                     CRLF+'Table: '+ALLTRIM(ridd1.desc)+CRLF
               ENDIF
            ENDIF
            USE IN ridd1
            USE IN ridd2
            SELECT (sxnCurArea)
            RETURN 1
         ELSE
            * Perform blank and duplicate key checks.
            slcPrimaryTag = ridd1.primarytag
            SELECT (sxnCurArea)
            slcCurTag = ORDER()
            oApp.SetOrder(ALIAS(),SET('DATASESSION'),slcPrimaryTag)
            slcSeekExpr = KEY(VAL(SYS(21)))
            slSeekValue = EVALUATE(slcSeekExpr)
            IF EMPTY(slcCurTag)
               oApp.SetOrder(ALIAS(),SET('DATASESSION'),'')
            ELSE
               oApp.SetOrder(ALIAS(),SET('DATASESSION'),slcCurTag)
            ENDIF
            IF (ISNULL(slSeekValue) OR EMPTY(slSeekValue))
               * The PK is blank or null.
               IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                  _SCREEN.ActiveForm.cRIMessage = slcPKFlds
                  IF NOT EMPTY(ridd1.desc)
                     _SCREEN.ActiveForm.cRIMessage = ;
                        _SCREEN.ActiveForm.cRIMessage+;
                        CRLF+'Table: '+ALLTRIM(ridd1.desc)+CRLF
                  ENDIF
               ENDIF
               USE IN ridd1
               USE IN ridd2
               SELECT (sxnCurArea)
               RETURN 3
            ENDIF
            * Open the table AGAIN because moving the record pointer in 
            * the current table would commit the changes.
            slnRecNo = RECNO()
            IF EVAL(sgcUUse+"(slcModifiedWithPath,'ridupcheck','AGAIN',"+;
               "'','',slcDDToUse)")
               oApp.SetOrder(ALIAS(),SET('DATASESSION'),slcPrimaryTag)
               IF SEEK(slSeekValue) AND slnRecNo <> RECNO()
                  * There is a duplicate key value.  Since the key value 
                  * was changed only in the buffer, this SEEK found a record
                  * other than the changed record.
                  IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                     _SCREEN.ActiveForm.cRIMessage = slcPKFlds
                     IF NOT EMPTY(ridd1.desc)
                        _SCREEN.ActiveForm.cRIMessage = ;
                           _SCREEN.ActiveForm.cRIMessage+;
                           CRLF+'Table: '+ALLTRIM(ridd1.desc)+CRLF
                     ENDIF
                  ENDIF
                  USE IN ridupcheck
                  USE IN ridd1
                  USE IN ridd2
                  SELECT (sxnCurArea)
                  RETURN 1
               ELSE
                  * There isn't a non-deleted duplicate key value.
                  USE IN ridupcheck
                  SELECT ridd1
               ENDIF
            ELSE
               IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                  _SCREEN.ActiveForm.cRIMessage = 'The following table could '+;
                     'not be opened in order to perform the duplicate primary '+;
                     'key check: '
                  IF EMPTY(ridd1.desc)
                     _SCREEN.ActiveForm.cRIMessage = ;
                        _SCREEN.ActiveForm.cRIMessage+slcModifiedWithPath+CRLF
                  ELSE
                     _SCREEN.ActiveForm.cRIMessage = ;
                        _SCREEN.ActiveForm.cRIMessage+ALLTRIM(ridd1.desc)+CRLF
                  ENDIF
               ENDIF
               USE IN ridd1
               USE IN ridd2
               SELECT (sxnCurArea)
               RETURN 1
            ENDIF
         ENDIF
      ENDIF
   ENDIF
ELSE
   * Current table is a view cursor.  Create an array of view fields.
   slcTables = ','
   slcCurrentView = ALLTRIM(UPPER(CURSORGETPROP('SourceName',sxnCurArea)))
   SELECT ridd2
   SCAN FOR ALLTRIM(ridd2.file_name) == slcCurrentView AND ;
      NOT EMPTY(ridd2.viewsrctbl) AND NOT EMPTY(ridd2.viewsrcfld)
      slnVFNum = slnVFNum+1
      DIMENSION slaVF(slnVFNum,3)
      slaVF(slnVFNum,1) = ALLTRIM(UPPER(ridd2.field_name))
      slaVF(slnVFNum,2) = ALLTRIM(UPPER(ridd2.viewsrcfld))
      slaVF(slnVFNum,3) = ALLTRIM(UPPER(ridd2.viewsrctbl))
      IF AT(slaVF(slnVFNum,3),slcTables) = 0
         slcTables = slcTables+slaVF(slnVFNum,3)+','
      ENDIF
   ENDSCAN
   SELECT ridd1
   IF LEN(slcTables) > 1
      slnCommaX = 0
      sllMoreX = .T.
      DO WHILE sllMoreX
         slnCommaX = slnCommaX+1
         slnComma1 = AT(',',slcTables,slnCommaX)
         slnComma2 = AT(',',slcTables,slnCommaX+1)
         IF slnComma1 = 0 OR slnComma2 = 0
            EXIT
         ENDIF
         slcActualTable = ALLTRIM(UPPER(SUBSTR(slcTables,slnComma1+1,;
            slnComma2-slnComma1-1)))
         IF EMPTY(slcActualTable)
            LOOP
         ENDIF
         SELECT ridd1
         IF SEEK(PADR(LEFT(slcActualTable,112),112))
            IF EMPTY(ridd1.primaryfld)
               LOOP
            ENDIF
            slcPKFlds = ''
            sllDel_AllFld = ridd1.del_allfld
            IF EMPTY(ridd1.primaryfld)
               slcPKFields = ''
            ELSE
               slcPKFields = ','+ALLTRIM(UPPER(ridd1.primaryfld))+','
            ENDIF
            SELECT (sxnCurArea)
            sllKeyFldChanged = .F.
            slnCommaY = 0
            sllMoreY = .T.
            DO WHILE sllMoreY
               slnCommaY = slnCommaY+1
               slnComma1 = AT(',',slcPKFields,slnCommaY)
               slnComma2 = AT(',',slcPKFields,slnCommaY+1)
               IF slnComma1 = 0 OR slnComma2 = 0
                  EXIT
               ENDIF
               slcKeyFld = ALLTRIM(SUBSTR(slcPKFields,slnComma1+1,;
                  slnComma2-slnComma1-1))
               FOR slnFor = 1 TO slnVFNum
                  IF slcActualTable == slaVF(slnFor,3) AND ;
                     slcKeyFld == slaVF(slnFor,2)
                     slcCurField = slaVF(slnFor,1)
                     IF slcMode = 'NEW'
                        * For new records, the key value will be taken from
                        * the buffer, not the disk.
                        slKeyFldValue = EVALUATE(slcCurField)
                        sllKeyFldChanged = .T.
                     ELSE
                        * For changed and deleted records, the key value
                        * will be taken from the disk, not the buffer.
                        slKeyFldValue = OLDVAL(slcCurField)
                        IF NOT EVALUATE(slcCurField) == slKeyFldValue
                           sllKeyFldChanged = .T.
                        ENDIF
                     ENDIF
                     * Find the field's record in DD2.
                     = siFindDD2Rec(slcActualTable,slcKeyFld)
                     IF sxlDD2Found AND NOT EMPTY(ridd2.label)
                        slcPKFlds = slcPKFlds+ALLTRIM(ridd2.label)+CRLF
                     ELSE
                        slcPKFlds = slcPKFlds+PROPER(slcCurField)+CRLF
                     ENDIF
                     IF ISNULL(slKeyFldValue)
                        slcKeyFldValue = ''
                     ELSE
                        slcFieldType = TYPE('slKeyFldValue')
                        DO CASE
                           CASE slcFieldType $ 'CM'
                              slcKeyFldValue = slKeyFldValue
                           CASE slcFieldType = 'N'
                              * Get width and decimals from DD2.
                              IF sxlDD2Found
                                 slnFieldDecimals = ridd2.decimals
                                 DO CASE
                                    CASE ridd2.type = 'B'
                                       slnFieldWidth = 20
                                    CASE ridd2.type = 'F'
                                       slnFieldWidth = 20
                                    CASE ridd2.type = 'I'
                                       slnFieldWidth = 10
                                    OTHERWISE
                                       slnFieldWidth = ridd2.width
                                 ENDCASE
                              ELSE
                                 slnFieldWidth = 20
                                 slnFieldDecimals = 7
                              ENDIF
                              slcKeyFldValue = ALLTRIM(STR(slKeyFldValue,;
                                 slnFieldWidth,slnFieldDecimals))
                           CASE slcFieldType = 'Y'
                              slcKeyFldValue = STR(MTON(slKeyFldValue),20,4)
                           CASE slcFieldType = 'T'
                              slcKeyFldValue = TTOC(slKeyFldValue)
                           CASE slcFieldType = 'D'
                              slcKeyFldValue = DTOC(slKeyFldValue)
                           CASE slcFieldType = 'L'
                              IF slKeyFldValue
                                 slcKeyFldValue = 'T'
                              ELSE
                                 slcKeyFldValue = 'F'
                              ENDIF
                           OTHERWISE
                              slcKeyFldValue = ''
                        ENDCASE
                     ENDIF
                     slcPKString = slcPKString+ALLTRIM(slcKeyFldValue)+','
                     EXIT
                  ENDIF
               ENDFOR
            ENDDO
            IF sllKeyFldChanged AND slcMode <> 'DELETE'
               * For new records and records whose key field(s) were changed,
               * perform the blank and duplicate PK checks.
               SELECT ridd1
               IF EMPTY(ridd1.primarytag) OR ;
                  ALLTRIM(UPPER(ridd1.primarytag)) == 'NONE'
                  * Cannot perform blank and duplicate PK checks if a primary
                  * key tag has not been specified.
                  IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                     _SCREEN.ActiveForm.cRIMessage = slcPKFlds
                     IF NOT EMPTY(ridd1.desc)
                        _SCREEN.ActiveForm.cRIMessage = ;
                           _SCREEN.ActiveForm.cRIMessage+;
                           CRLF+'Table: '+ALLTRIM(ridd1.desc)+CRLF
                     ENDIF
                  ENDIF
                  USE IN ridd1
                  USE IN ridd2
                  SELECT (sxnCurArea)
                  RETURN 1
               ELSE
                  * Open the view's source table.
                  IF EVAL(sgcUUse+"(slcActualTable,'ridupcheck','AGAIN',"+;
                     "'','',slcDDToUse)")
                     * Perform blank and duplicate key checks.
                     slcPrimaryTag = ridd1.primarytag
                     oApp.SetOrder(ALIAS(),SET('DATASESSION'),slcPrimaryTag)
                     slcSeekExpr = KEY(VAL(SYS(21)))
                     * Replace the source field names in the PK tag expression
                     * with the view field names.
                     SELECT (sxnCurArea)
                     FOR slnFor = 1 TO slnVFNum
                        IF slcActualTable == slaVF(slnFor,3)
                           slcSeekExpr = STRTRAN(slcSeekExpr,slaVF(slnFor,2),;
                              slaVF(slnFor,1))
                        ENDIF
                     ENDFOR
                     slSeekValue = EVALUATE(slcSeekExpr)
                     IF (ISNULL(slSeekValue) OR EMPTY(slSeekValue))
                        * The PK is blank or null.
                        IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                           _SCREEN.ActiveForm.cRIMessage = slcPKFlds
                           IF NOT EMPTY(ridd1.desc)
                              _SCREEN.ActiveForm.cRIMessage = ;
                                 _SCREEN.ActiveForm.cRIMessage+;
                                 CRLF+'Table: '+ALLTRIM(ridd1.desc)+CRLF
                           ENDIF
                        ENDIF
                        USE IN ridupcheck
                        USE IN ridd1
                        USE IN ridd2
                        SELECT (sxnCurArea)
                        RETURN 3
                     ENDIF
                     SELECT ridupcheck
                     IF SEEK(slSeekValue)
                        * There is a duplicate key value.  Since the key value 
                        * was changed only in the buffer, this SEEK found a
                        * record other than the changed record.
                        IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                           _SCREEN.ActiveForm.cRIMessage = slcPKFlds
                           IF NOT EMPTY(ridd1.desc)
                              _SCREEN.ActiveForm.cRIMessage = ;
                                 _SCREEN.ActiveForm.cRIMessage+;
                                 CRLF+'Table: '+ALLTRIM(ridd1.desc)+CRLF
                           ENDIF
                        ENDIF
                        USE IN ridupcheck
                        USE IN ridd1
                        USE IN ridd2
                        SELECT (sxnCurArea)
                        RETURN 1
                     ELSE
                        * There isn't a non-deleted duplicate key value.
                        USE IN ridupcheck
                        SELECT ridd1
                     ENDIF
                  ELSE
                     IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                        _SCREEN.ActiveForm.cRIMessage = 'The following table could '+;
                           'not be opened in order to perform the duplicate primary '+;
                           'key check: '
                        IF EMPTY(ridd1.desc)
                           _SCREEN.ActiveForm.cRIMessage = ;
                              _SCREEN.ActiveForm.cRIMessage+slcModifiedWithPath+CRLF
                        ELSE
                           _SCREEN.ActiveForm.cRIMessage = ;
                              _SCREEN.ActiveForm.cRIMessage+ALLTRIM(ridd1.desc)+CRLF
                        ENDIF
                     ENDIF
                     USE IN ridd1
                     USE IN ridd2
                     SELECT (sxnCurArea)
                     RETURN 1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF NOT EMPTY(slcPKString)
         * Remove the trailing comma.
         slcPKString = LEFT(slcPKString,LEN(slcPKString)-1)
      ENDIF
   ENDIF
ENDIF

* Create the referential integrity work file.
CREATE CURSOR riworkfile ;
   (file_name C(254), field_name C(128), action C(6), rec_num N(9,0),;
   type C(1), width N(6,0) NULL, decimals N(2,0), old_value C(254) NULL,;
   new_value C(254), key_value C(254), audit_trl L, view_table C(254),;
   view_field C(128))

SELECT (sxnCurArea)

* Add a record to the RI work file for each changed field.  Add a record
* to the RI work file for each field in the current record if the current
* record has been deleted or is new (added).
DO CASE
   CASE slcMode = 'CHANGE'
      slnWorkFileRecs = 0
      FOR slnFor = 1 TO slnModifiedFields
         slcCurField = FIELD(slnFor)
         IF TYPE(slcCurField) = 'G'
            LOOP
         ENDIF
         slOldValue = OLDVAL(slcCurField)
         IF ISNULL(slOldValue)
            sllOldNothing = .T.
         ELSE
            IF EMPTY(slOldValue)
               sllOldNothing = .T.
            ELSE
               sllOldNothing = .F.
            ENDIF
         ENDIF
         slNewValue = EVALUATE(slcCurField)
         IF ISNULL(slNewValue)
            sllNewNothing = .T.
         ELSE
            IF EMPTY(slNewValue)
               sllNewNothing = .T.
            ELSE
               sllNewNothing = .F.
            ENDIF
         ENDIF
         IF NOT (sllOldNothing AND sllNewNothing) AND ;
            ((sllOldNothing AND NOT sllNewNothing) OR ;
            (NOT sllOldNothing AND sllNewNothing) OR ;
            NOT slNewValue == slOldValue)
            * Field value has changed
            slcFieldType = TYPE('slNewValue')
            slnFieldDecimals = 0
            * Locate the field's DD2 record.
            IF slnVFNum > 0
               = siFindDD2Rec(slcCurrentView,slcCurField)
            ELSE
               = siFindDD2Rec(slcModifiedWithoutPath,slcCurField)
            ENDIF
            DO CASE
               CASE slcFieldType $ 'CM'
                  IF ISNULL(slOldValue)
                     slcOldValue = ''
                  ELSE
                     slcOldValue = slOldValue
                  ENDIF
                  IF ISNULL(slNewValue)
                     slcNewValue = ''
                  ELSE
                     slcNewValue = slNewValue
                  ENDIF
               CASE slcFieldType = 'N'
                  * Get width and decimals from DD2.
                  IF sxlDD2Found
                     slnFieldDecimals = ridd2.decimals
                     DO CASE
                        CASE ridd2.type = 'B'
                           slnFieldWidth = 20
                        CASE ridd2.type = 'F'
                           slnFieldWidth = 20
                        CASE ridd2.type = 'I'
                           slnFieldWidth = 10
                        OTHERWISE
                           slnFieldWidth = ridd2.width
                     ENDCASE
                  ELSE
                     slnFieldWidth = 20
                     slnFieldDecimals = 7
                  ENDIF
                  IF ISNULL(slOldValue)
                     slcOldValue = ''
                  ELSE
                     slcOldValue = ALLTRIM(STR(slOldValue,slnFieldWidth,;
                        slnFieldDecimals))
                  ENDIF
                  IF ISNULL(slNewValue)
                     slcNewValue = ''
                  ELSE
                     slcNewValue = ALLTRIM(STR(slNewValue,slnFieldWidth,;
                        slnFieldDecimals))
                  ENDIF
               CASE slcFieldType = 'Y'
                  IF ISNULL(slOldValue)
                     slcOldValue = ''
                  ELSE
                     slcOldValue = STR(MTON(slOldValue),20,4)
                  ENDIF
                  IF ISNULL(slNewValue)
                     slcNewValue = ''
                  ELSE
                     slcNewValue = STR(MTON(slNewValue),20,4)
                  ENDIF
               CASE slcFieldType = 'T'
                  IF ISNULL(slOldValue)
                     slcOldValue = ''
                  ELSE
                     slcOldValue = TTOC(slOldValue)
                  ENDIF
                  IF ISNULL(slNewValue)
                     slcNewValue = ''
                  ELSE
                     slcNewValue = TTOC(slNewValue)
                  ENDIF
               CASE slcFieldType = 'D'
                  IF ISNULL(slOldValue)
                     slcOldValue = ''
                  ELSE
                     slcOldValue = DTOC(slOldValue)
                  ENDIF
                  IF ISNULL(slNewValue)
                     slcNewValue = ''
                  ELSE
                     slcNewValue = DTOC(slNewValue)
                  ENDIF
               CASE slcFieldType = 'L'
                  IF ISNULL(slOldValue)
                     slcOldValue = ''
                  ELSE
                     IF slOldValue
                        slcOldValue = 'T'
                     ELSE
                        slcOldValue = 'F'
                     ENDIF
                  ENDIF
                  IF ISNULL(slNewValue)
                     slcNewValue = ''
                  ELSE
                     IF slNewValue
                        slcNewValue = 'T'
                     ELSE
                        slcNewValue = 'F'
                     ENDIF
                  ENDIF
            ENDCASE
            slnWorkFileRecs = slnWorkFileRecs+1
            DIMENSION slaRef(slnWorkFileRecs,13)
            slaRef(slnWorkFileRecs,1) = slcModifiedWithPath
            slaRef(slnWorkFileRecs,2) = slcCurField
            slaRef(slnWorkFileRecs,3) = 'CHANGE'
            slaRef(slnWorkFileRecs,4) = RECNO()
            slaRef(slnWorkFileRecs,5) = slcFieldType
            slaRef(slnWorkFileRecs,6) = LEN(slcOldValue)
            slaRef(slnWorkFileRecs,7) = slnFieldDecimals
            slaRef(slnWorkFileRecs,8) = slcOldValue
            slaRef(slnWorkFileRecs,9) = slcNewValue
            slaRef(slnWorkFileRecs,10) = slcPKString
            slaRef(slnWorkFileRecs,11) = sxlAudit
            slaRef(slnWorkFileRecs,12) = ''   && Source table name for view
            slaRef(slnWorkFileRecs,13) = ''   && Source field name for view
            IF slnVFNum > 0
               * Put the actual table and field names from the view into
               * the array.
               FOR slnFor2 = 1 TO slnVFNum
                  IF slcCurField == slaVF(slnFor2,1)
                     slaRef(slnWorkFileRecs,12) = slaVF(slnFor2,3)
                     slaRef(slnWorkFileRecs,13) = slaVF(slnFor2,2)
                     EXIT
                  ENDIF
               ENDFOR
            ENDIF
         ENDIF
      ENDFOR
      IF slnWorkFileRecs > 0
         SELECT riworkfile
         APPEND FROM ARRAY slaRef
      ENDIF
   CASE slcMode = 'DELETE'
      slnWorkFileRecs = 0
      FOR slnFor = 1 TO slnModifiedFields
         slcCurField = FIELD(slnFor)
         IF TYPE(slcCurField) = 'G'
            LOOP
         ENDIF
         * Unless the "Del Audit Trail - All Flds" checkbox on the Table
         * Properties page of the Data Builder is checked, only primary
         * key fields will be included in the audit trail for deleted
         * records.
         IF NOT sllDel_AllFld AND NOT EMPTY(slcPKFields) AND ;
            AT(','+slcCurField+',',slcPKFields) = 0
            LOOP
         ENDIF
         slOldValue = OLDVAL(slcCurField)
         slcFieldType = TYPE('slOldValue')
         slnFieldDecimals = 0
         IF ISNULL(slOldValue)
            slcOldValue = ''
            sxlAudit = .F.
         ELSE
            * Locate the field's DD2 record.
            IF slnVFNum > 0
               = siFindDD2Rec(slcCurrentView,slcCurField)
            ELSE
               = siFindDD2Rec(slcModifiedWithoutPath,slcCurField)
            ENDIF
            DO CASE
               CASE slcFieldType $ 'CM'
                  IF EMPTY(slOldValue)
                     sxlAudit = .F.
                  ENDIF
                  slcOldValue = slOldValue
               CASE slcFieldType = 'N'
                  IF EMPTY(slOldValue)
                     sxlAudit = .F.
                  ENDIF
                  * Get width and decimals from DD2.
                  IF sxlDD2Found
                     slnFieldDecimals = ridd2.decimals
                     DO CASE
                        CASE ridd2.type = 'B'
                           slnFieldWidth = 20
                        CASE ridd2.type = 'F'
                           slnFieldWidth = 20
                        CASE ridd2.type = 'I'
                           slnFieldWidth = 10
                        OTHERWISE
                           slnFieldWidth = ridd2.width
                     ENDCASE
                  ELSE
                     slnFieldWidth = 20
                     slnFieldDecimals = 7
                  ENDIF
                  slcOldValue = ALLTRIM(STR(slOldValue,slnFieldWidth,;
                     slnFieldDecimals))
               CASE slcFieldType = 'Y'
                  IF EMPTY(MTON(slOldValue))
                     sxlAudit = .F.
                  ENDIF
                  slcOldValue = STR(MTON(slOldValue),20,4)
               CASE slcFieldType = 'T'
                  IF EMPTY(slOldValue)
                     sxlAudit = .F.
                  ENDIF
                  slcOldValue = TTOC(slOldValue)
               CASE slcFieldType = 'D'
                  IF EMPTY(slOldValue)
                     sxlAudit = .F.
                  ENDIF
                  slcOldValue = DTOC(slOldValue)
               CASE slcFieldType = 'L'
                  IF slOldValue
                     slcOldValue = 'T'
                  ELSE
                     slcOldValue = 'F'
                  ENDIF
            ENDCASE
         ENDIF
         slnWorkFileRecs = slnWorkFileRecs+1
         DIMENSION slaRef(slnWorkFileRecs,13)
         slaRef(slnWorkFileRecs,1) = slcModifiedWithPath
         slaRef(slnWorkFileRecs,2) = slcCurField
         slaRef(slnWorkFileRecs,3) = 'DELETE'
         slaRef(slnWorkFileRecs,4) = RECNO()
         slaRef(slnWorkFileRecs,5) = slcFieldType
         slaRef(slnWorkFileRecs,6) = LEN(slcOldValue)
         slaRef(slnWorkFileRecs,7) = slnFieldDecimals
         slaRef(slnWorkFileRecs,8) = slcOldValue
         slaRef(slnWorkFileRecs,9) = ''
         slaRef(slnWorkFileRecs,10) = slcPKString
         slaRef(slnWorkFileRecs,11) = sxlAudit
         slaRef(slnWorkFileRecs,12) = ''	&& Source table name for view
         slaRef(slnWorkFileRecs,13) = ''	&& Source field name for view
         IF slnVFNum > 0
            * Put the actual table and field names from the view into
            * the array.
            FOR slnFor2 = 1 TO slnVFNum
               IF slcCurField == slaVF(slnFor2,1)
                  slaRef(slnWorkFileRecs,12) = slaVF(slnFor2,3)
                  slaRef(slnWorkFileRecs,13) = slaVF(slnFor2,2)
                  EXIT
               ENDIF
            ENDFOR
         ENDIF
      ENDFOR
      IF slnWorkFileRecs > 0
         SELECT riworkfile
         APPEND FROM ARRAY slaRef
      ENDIF
   CASE slcMode = 'NEW' AND NOT sxlNoAuditTrail
      slnWorkFileRecs = 0
      FOR slnFor = 1 TO slnModifiedFields
         slcCurField = FIELD(slnFor)
         IF TYPE(slcCurField) = 'G'
            LOOP
         ENDIF
         slNewValue = EVALUATE(slcCurField)
         slcFieldType = TYPE('slNewValue')
         slnFieldDecimals = 0
         IF ISNULL(slNewValue)
            slcNewValue = ''
            sxlAudit = .F.
         ELSE
            * Locate the field's DD2 record.
            IF slnVFNum > 0
               = siFindDD2Rec(slcCurrentView,slcCurField)
            ELSE
               = siFindDD2Rec(slcModifiedWithoutPath,slcCurField)
            ENDIF
            DO CASE
               CASE slcFieldType $ 'CM'
                  IF EMPTY(slNewValue)
                     sxlAudit = .F.
                  ENDIF
                  slcNewValue = slNewValue
               CASE slcFieldType = 'N'
                  IF EMPTY(slNewValue)
                     sxlAudit = .F.
                  ENDIF
                  * Get width and decimals from DD2
                  IF sxlDD2Found
                     slnFieldDecimals = ridd2.decimals
                     DO CASE
                        CASE ridd2.type = 'B'
                           slnFieldWidth = 20
                        CASE ridd2.type = 'F'
                           slnFieldWidth = 20
                        CASE ridd2.type = 'I'
                           slnFieldWidth = 10
                        OTHERWISE
                           slnFieldWidth = ridd2.width
                     ENDCASE
                  ELSE
                     slnFieldWidth = 20
                     slnFieldDecimals = 7
                  ENDIF
                  slcNewValue = ALLTRIM(STR(slNewValue,slnFieldWidth,;
                     slnFieldDecimals))
               CASE slcFieldType = 'Y'
                  IF EMPTY(MTON(slNewValue))
                     sxlAudit = .F.
                  ENDIF
                  slcNewValue = STR(MTON(slNewValue),20,4)
               CASE slcFieldType = 'T'
                  IF EMPTY(slNewValue)
                     sxlAudit = .F.
                  ENDIF
                  slcNewValue = TTOC(slNewValue)
               CASE slcFieldType = 'D'
                  IF EMPTY(slNewValue)
                     sxlAudit = .F.
                  ENDIF
                  slcNewValue = DTOC(slNewValue)
               CASE slcFieldType = 'L'
                  IF slNewValue
                     slcNewValue = 'T'
                  ELSE
                     slcNewValue = 'F'
                  ENDIF
            ENDCASE
         ENDIF
         IF sxlAudit
            slnWorkFileRecs = slnWorkFileRecs+1
            DIMENSION slaRef(slnWorkFileRecs,13)
            slaRef(slnWorkFileRecs,1) = slcModifiedWithPath
            slaRef(slnWorkFileRecs,2) = slcCurField
            slaRef(slnWorkFileRecs,3) = 'NEW'
            slaRef(slnWorkFileRecs,4) = RECNO()
            slaRef(slnWorkFileRecs,5) = slcFieldType
            slaRef(slnWorkFileRecs,6) = LEN(slcNewValue)
            slaRef(slnWorkFileRecs,7) = slnFieldDecimals
            slaRef(slnWorkFileRecs,8) = ''
            slaRef(slnWorkFileRecs,9) = slcNewValue
            slaRef(slnWorkFileRecs,10) = slcPKString
            slaRef(slnWorkFileRecs,11) = .T.
            slaRef(slnWorkFileRecs,12) = ''	&& Source table name for view
            slaRef(slnWorkFileRecs,13) = ''	&& Source field name for view
            IF slnVFNum > 0
               * Put the actual table and field names from the view into
               * the array.
               FOR slnFor2 = 1 TO slnVFNum
                  IF slcCurField == slaVF(slnFor2,1)
                     slaRef(slnWorkFileRecs,12) = slaVF(slnFor2,3)
                     slaRef(slnWorkFileRecs,13) = slaVF(slnFor2,2)
                     EXIT
                  ENDIF
               ENDFOR
            ENDIF
         ENDIF
      ENDFOR
      IF slnWorkFileRecs > 0
         SELECT riworkfile
         APPEND FROM ARRAY slaRef
      ENDIF
ENDCASE

slcModifiedWithoutPath = ''		&& No longer used
SELECT riworkfile
oApp.First(SET('DATASESSION'))
slnNumCur = RECCOUNT()

IF slcMode <> 'NEW'	AND NOT sllNoRICheck
   * No referential integrity check for new records.

   slcSave_ridd1_primaryfld = ''
   slcSave_riworkfile_file_name = ''
   sllCancelRICheck = .F.
   sllDoRICheck = .T.
   sllFirstField = .T.
   slnParentTableNo = 0

   DO WHILE sllDoRICheck AND NOT EOF()
      * Loop through the RI work file where each record represents a field
      * that has been changed or is in a deleted record.  If the field is a
      * PK field the following RI check must be performed to see if there
      * are any related child tables that need to be modified.

      slcSave_riworkfile_action = riworkfile.action
      slnSave_riworkfile_RecNo = RECNO()
      slnModifiedRecNo = riworkfile.rec_num
      IF NOT EMPTY(riworkfile.view_table) AND NOT EMPTY(riworkfile.view_field)
         slcModifiedDDFile_Name = ALLTRIM(riworkfile.view_table)
         slcModifiedDDField_Name = ALLTRIM(riworkfile.view_field)
      ELSE
         slcModifiedDDFile_Name = ALLTRIM(SUBSTR(riworkfile.file_name,;
            RAT('\',riworkfile.file_name)+1))
         slcModifiedDDField_Name = ALLTRIM(riworkfile.field_name)
      ENDIF
      IF NOT ALLTRIM(riworkfile.file_name) == slcSave_riworkfile_file_name
         IF NOT SEEK(PADR(LEFT(slcModifiedDDFile_Name,112),112),'ridd1')
            * Field's table cannot be found in DD so skip it.
            SELECT riworkfile
            oApp.Next(1,SET('DATASESSION'))
            LOOP
         ENDIF
         slcSave_ridd1_primaryfld = ','+ALLTRIM(ridd1.primaryfld)+','
         slcSave_riworkfile_file_name = ALLTRIM(riworkfile.file_name)
      ENDIF
      IF NOT ','+slcModifiedDDField_Name+',' $ slcSave_ridd1_primaryfld
         * Field is not in PK so skip it.
         SELECT riworkfile
         oApp.Next(1,SET('DATASESSION'))
         LOOP
      ENDIF
      
      IF sllDisplayWaitWindow
         sllDisplayWaitWindow = .F.
         WAIT WINDOW NOWAIT 'Checking referential integrity...'
      ENDIF

      * Make sure that the table identified in riworkfile.file_name is
      * in use and is positioned on the modified record.
      IF ALLTRIM(riworkfile.file_name) == slcOriginalModifiedWithPath
         SELECT (sxnCurArea)
         * Avoid GOTO below so changes to original record are not
         * automatically saved when record pointer is moved.
      ELSE
         IF ALLTRIM(riworkfile.file_name) == slcModifiedWithPath
            * Table is already open.
            SELECT (slnModifiedWorkarea)
         ELSE
            * Open table.
            slcModifiedWithPath = ALLTRIM(riworkfile.file_name)
            slnParentTableNo = slnParentTableNo+1
            IF NOT EVAL(sgcUUse+"(slcModifiedWithPath,'PARTBL'+"+;
               "ALLTRIM(STR(slnParentTableNo)),'AGAIN','','',slcDDToUse)")
               SELECT riworkfile
               oApp.Next(1,SET('DATASESSION'))
               LOOP
            ENDIF
            IF TXNLEVEL() = 0
               = CURSORSETPROP('Buffering',5)
            ENDIF
            slcModifiedAlias = ALIAS()
            slcModifiedWithPath = ALLTRIM(DBF())
            slnModifiedWorkarea = SELECT()
         ENDIF
         GOTO slnModifiedRecNo
      ENDIF

      IF sllFirstField
         * If no FK fields are found, exit the RI check.
         sllDoRICheck = .F.
      ENDIF
      SELECT ridd2
      SCAN FOR ridd2.val_type = 3
         * Only check fields using Referential type Integrity.
         IF NOT EMPTY(ridd2.viewsrcfld)
            * Bypass views.
            LOOP
         ENDIF
         slnSave_ridd2_RecNo = RECNO()
         slcSave_ridd2_val_data = ALLTRIM(ridd2.val_data)
         slnQuote1 = AT('"',slcSave_ridd2_val_data,1)
         slnQuote2 = AT('"',slcSave_ridd2_val_data,2)
         IF slnQuote1 = 0 OR slnQuote2 = 0
            * There is no PK table specified.
            LOOP
         ENDIF
         slcPossibleChildTable = UPPER(ALLTRIM(SUBSTR(slcSave_ridd2_val_data,;
            slnQuote1+1,slnQuote2-slnQuote1-1)))
         IF EMPTY(slcPossibleChildTable)
            * There is no PK table specified.
            LOOP
         ENDIF
         IF LEFT(slcPossibleChildTable,2) == 'M.' AND ;
            NOT slcPossibleChildTable == 'M.DBF'
            * Name of PK table is stored in memvar.
            slcPossibleChildTable = ALLTRIM(UPPER(EVALUATE(slcPossibleChildTable)))
         ENDIF
         slcPossibleChildTable = SUBSTR(slcPossibleChildTable,;
            RAT('\',slcPossibleChildTable)+1)
         IF NOT slcModifiedDDFile_Name == slcPossibleChildTable
            * Field is not a FK of the modified table.
            LOOP
         ENDIF
         IF sllFirstField
            * For the first PK field in riworkfile (in the original modified
            * record), there is a field in DD2 that uses that PK field's
            * table for referential integrity.  Therefore, the check for a
            * "related" table needs to be performed for the other PK fields
            * in riworkfile.
            sllDoRICheck = .T.
         ENDIF
         * See if any of the Primary Key fields identified in the
         * referential integrity for this DD2 record match the field
         * identified in slcModifiedDDField_Name (a changed/deleted field).
         * If a match is found, there is a table that is "related" to
         * the table (riworkfile.file_name).
         sllMatched = .F.
         STORE '' TO slcPKFld1,slcPKFld2,slcPKFld3,slcPKFld4,slcPKFld5,;
            slcPKFld6,slcPKFld7,slcPKFld8,slcPKFld9,slcFKFld1,slcFKFld2,;
            slcFKFld3,slcFKFld4,slcFKFld5,slcFKFld6,slcFKFld7,slcFKFld8,;
            slcFKFld9
         slcForFlds = ''
         FOR slnFor = 1 TO 4
            slnQuote1 = AT('"',slcSave_ridd2_val_data,slnFor*2+1)
            slnQuote2 = AT('"',slcSave_ridd2_val_data,slnFor*2+2)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               slcFK = SUBSTR(slcSave_ridd2_val_data,slnQuote1+1,;
                  slnQuote2-slnQuote1-1)
               IF NOT EMPTY(slcFK)
                  slcForFlds = slcForFlds+','+slcFK
               ENDIF
            ENDIF
         ENDFOR
         IF NOT EMPTY(slcForFlds)
            IF LEFT(slcForFlds,1) <> ','
               slcForFlds = ','+slcForFlds
            ENDIF
            IF RIGHT(slcForFlds,1) <> ','
               slcForFlds = slcForFlds+','
            ENDIF
         ENDIF
         slcPriFlds = ''
         FOR slnFor = 1 TO 4
            slnQuote1 = AT('"',slcSave_ridd2_val_data,slnFor*2+9)
            slnQuote2 = AT('"',slcSave_ridd2_val_data,slnFor*2+10)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               slcPK = SUBSTR(slcSave_ridd2_val_data,slnQuote1+1,;
                  slnQuote2-slnQuote1-1)
               IF NOT EMPTY(slcPK)
                  slcPriFlds = slcPriFlds+','+slcPK
               ENDIF
            ENDIF
         ENDFOR
         IF NOT EMPTY(slcPriFlds)
            IF LEFT(slcPriFlds,1) <> ','
               slcPriFlds = ','+slcPriFlds
            ENDIF
            IF RIGHT(slcPriFlds,1) <> ','
               slcPriFlds = slcPriFlds+','
            ENDIF
         ENDIF
         slnNumFlds = 0
         sllMore = .T.
         DO WHILE sllMore
            slnComma1 = AT(',',slcForFlds,1)
            slnComma2 = AT(',',slcForFlds,2)
            IF slnComma1 > 0 AND slnComma2 > 0
               slcForFld = SUBSTR(slcForFlds,slnComma1+1,;
                  slnComma2-slnComma1-1)
               slcForFlds = SUBSTR(slcForFlds,slnComma2)
            ELSE
               EXIT
            ENDIF
            slnComma1 = AT(',',slcPriFlds,1)
            slnComma2 = AT(',',slcPriFlds,2)
            IF slnComma1 > 0 AND slnComma2 > 0
               slcPriFld = SUBSTR(slcPriFlds,slnComma1+1,;
                  slnComma2-slnComma1-1)
               slcPriFlds = SUBSTR(slcPriFlds,slnComma2)
            ELSE
               EXIT
            ENDIF
            IF NOT EMPTY(slcPriFld) AND NOT EMPTY(slcForFld)
               slnNumFlds = slnNumFlds+1
               IF slnNumFlds = 10
                  EXIT
               ENDIF
               slcFor = STR(slnNumFlds,1)
               slcFKFld&slcFor = slcForFld
               * Memory variables (as denoted by the "m." prefix) that are
               * used in the referential validation lookup fields must have
               * the same name as the "related" table field that they
               * represent.  We can then determine the name of the field in
               * the "related" table by simply removing the "m.".
               IF AT('M.',slcFKFld&slcFor) = 1
                  slcFKFld&slcFor = SUBSTR(slcFKFld&slcFor,3)
               ENDIF
               slcPKFld&slcFor = slcPriFld
               IF slcModifiedDDField_Name == slcPKFld&slcFor
                  sllMatched = .T.
               ENDIF
            ENDIF
         ENDDO
         IF NOT sllMatched
            LOOP
         ENDIF

         * Find the Update and Delete Rules.
         slnQuote1 = AT('"',slcSave_ridd2_val_data,23)
         slnQuote2 = AT('"',slcSave_ridd2_val_data,24)
         IF slnQuote1 > 0 AND slnQuote2 > 0
            slnUpdCtrl = VAL(SUBSTR(slcSave_ridd2_val_data,slnQuote1+1,;
               slnQuote2-slnQuote1-1))
            IF slnUpdCtrl < 1 OR slnUpdCtrl > 4
               slnUpdCtrl = 1				&& Default to Restrict
            ENDIF
         ELSE
            slnUpdCtrl = 1
         ENDIF
         IF slcSave_riworkfile_action = 'CHANGE' AND slnUpdCtrl = 4		&& No Change
            LOOP
         ENDIF
         slnQuote1 = AT('"',slcSave_ridd2_val_data,25)
         slnQuote2 = AT('"',slcSave_ridd2_val_data,26)
         IF slnQuote1 > 0 AND slnQuote2 > 0
            slnDelCtrl = VAL(SUBSTR(slcSave_ridd2_val_data,slnQuote1+1,;
               slnQuote2-slnQuote1-1))
            IF slnDelCtrl < 1 OR slnDelCtrl > 4
               slnDelCtrl = 1				&& Default to Restrict
            ENDIF
         ELSE
            slnDelCtrl = 1
         ENDIF
         IF slcSave_riworkfile_action = 'DELETE' AND slnDelCtrl = 4		&& No Change
            LOOP
         ENDIF

         * Create the search criteria that will find any of the records in
         * the "related" table that match the table's modified record.
         * Note:  For the match, use the values in the table record's fields
         * prior to their modification.  This means taking the value of
         * riworkfile.old_value if the table record's field has been
         * modified or the value of the table record's field if it hasn't
         * been modified.
         slcRTName = ALLTRIM(ridd2.file_name)
         SELECT riworkfile
         slcRTFor = ''
         STORE .F. TO sllUpdate1,sllUpdate2,sllUpdate3,sllUpdate4,;
            sllUpdate5,sllUpdate6,sllUpdate7,sllUpdate8,sllUpdate9
         FOR slnFor = 1 TO 9
            slcFor = STR(slnFor,1)
            IF EMPTY(slcPKFld&slcFor)
               LOOP
            ENDIF
            slcEqual = '='
            * If the PK is a compound expression, more than one of the
            * PK fields could have been changed.  If so, try to find the
            * records for the other PK fields in riworkfile so that the
            * correct values can be used in the search criteria.
            LOCATE FOR ALLTRIM(riworkfile.file_name) == slcModifiedWithPath AND ;
               (ALLTRIM(riworkfile.field_name) == slcPKFld&slcFor OR ;
               ALLTRIM(riworkfile.view_field) == slcPKFld&slcFor) AND ;
               riworkfile.rec_num = slnModifiedRecNo
            IF FOUND()
               sllUpdate&slcFor = .T.
               slcOldVal = riworkfile.old_value
               slcNewVal = riworkfile.new_value
               DO CASE
                  CASE riworkfile.type $ 'CM'
                     slVal&slcFor = LEFT(slcOldVal,riworkfile.width)
                     slNewVal&slcFor = LEFT(slcNewVal,riworkfile.width)
                     slcEqual = '=='
                  CASE riworkfile.type $ 'BFIN'
                     slVal&slcFor = VAL(slcOldVal)
                     slNewVal&slcFor = VAL(slcNewVal)
                  CASE riworkfile.type = 'Y'
                     slVal&slcFor = NTOM(VAL(slcOldVal))
                     slNewVal&slcFor = NTOM(VAL(slcNewVal))
                  CASE riworkfile.type = 'T'
                     slVal&slcFor = CTOT(slcOldVal)
                     slNewVal&slcFor = CTOT(slcNewVal)
                  CASE riworkfile.type = 'D'
                     IF SET('CENTURY') = 'ON'
                        slVal&slcFor = CTOD(LEFT(slcOldVal,10))
                        slNewVal&slcFor = CTOD(LEFT(slcNewVal,10))
                     ELSE
                        slVal&slcFor = CTOD(LEFT(slcOldVal,8))
                        slNewVal&slcFor = CTOD(LEFT(slcNewVal,8))
                     ENDIF
                  CASE riworkfile.type = 'L'
                     IF LEFT(slcOldVal,1) = 'T'
                        slVal&slcFor = .T.
                     ELSE
                        slVal&slcFor = .F.
                     ENDIF
                     IF LEFT(slcNewVal,1) = 'T'
                        slNewVal&slcFor = .T.
                     ELSE
                        slNewVal&slcFor = .F.
                     ENDIF
               ENDCASE
            ELSE
               IF CURSORGETPROP('SourceType',slcModifiedAlias) = 3	&& Table
                  slcValFld = slcModifiedAlias+'.'+slcPKFld&slcFor
               ELSE													&& View
                  * Find the name of the PK field in the view.
                  slcFld = ''
                  IF slnVFNum > 0
                     FOR slnFor2 = 1 TO slnVFNum
                        IF slcPKFld&slcFor == slaVF(slnFor2,2) AND ;
                           slcModifiedDDFile_Name == slaVF(slnFor2,3)
                           slcFld = slaVF(slnFor2,1)
                           EXIT
                        ENDIF
                     ENDFOR
                  ENDIF
                  IF EMPTY(slcFld)
                     sllCancelRICheck = .T.
                     IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                        _SCREEN.ActiveForm.cRIMessage = 'The related '+;
                           'table search conditions could not be determined.'
                     ENDIF
                     EXIT
                  ENDIF
                  slcValFld = slcModifiedAlias+'.'+slcFld
               ENDIF
               
               slVal&slcFor = EVALUATE(slcValFld)
               IF TYPE(slcValFld) = 'C'
                  slcEqual = '=='
               ENDIF
            ENDIF
            IF NOT EMPTY(slcRTFor)
               slcRTFor = slcRTFor+' AND '
            ENDIF
            slcRTFor = slcRTFor+slcFKFld&slcFor+' '+slcEqual+;
               ' slVal'+slcFor
         ENDFOR
         IF sllCancelRICheck
            EXIT
         ENDIF
         GOTO slnSave_riworkfile_RecNo
         IF EMPTY(slcRTFor)
            SELECT ridd2
            LOOP
         ENDIF
         
         * Open the "related" table.
         sllOpen = .T.
         sllAddToArray = .T.
         IF sgnRIUpd > 0
            FOR slnFor = 1 TO sgnRIUpd
               IF slcRTName == sgaRIUpd(slnFor,1)
                  slcRTAlias = sgaRIUpd(slnFor,2)
                  IF USED(sgaRIUpd(slnFor,2))
                     sllOpen = .F.
                  ELSE
                     sllAddToArray = .F.
                  ENDIF
                  EXIT
               ENDIF
            ENDFOR
         ENDIF
         IF sllOpen
            IF sllAddToArray
               sgnRIUpd = sgnRIUpd+1
               slcRTAlias = 'RELTBL'+ALLTRIM(STR(sgnRIUpd))
               DIMENSION sgaRIUpd(sgnRIUpd,2)
               sgaRIUpd(sgnRIUpd,1) = slcRTName
               sgaRIUpd(sgnRIUpd,2) = slcRTAlias
            ENDIF
            IF NOT EVAL(sgcUUse+"(slcRTName,slcRTAlias,'AGAIN','','',"+;
               "slcDDToUse)")
               sllCancelRICheck = .T.
               IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                  _SCREEN.ActiveForm.cRIMessage = ;
                     'The table '+slcRTName+siTableDesc(slcRTName)+;
                     ' could not be opened.'
               ENDIF
               EXIT
            ENDIF
            IF TXNLEVEL() = 0
               = CURSORSETPROP('Buffering',5)
            ENDIF
         ELSE
            SELECT (slcRTAlias)
         ENDIF
         slcRTDBFP = ALLTRIM(DBF())
         slcRTDBF = SUBSTR(slcRTDBFP,RAT('\',slcRTDBFP)+1)

         * Determine the key fields of the "related" table.
         SELECT ridd1
         IF SEEK(PADR(LEFT(slcRTDBF,112),112))
            sllDel_AllFld = ridd1.del_allfld
            IF EMPTY(ridd1.primaryfld)
               slcPKFields = ''
            ELSE
               slcPKFields = ','+ALLTRIM(UPPER(ridd1.primaryfld))+','
            ENDIF
         ELSE
            sllDel_AllFld = .F.
            slcPKFields = ''
         ENDIF
         SELECT (slcRTAlias)

         * Create the expression used to obtain the key field values in
         * the "related" table record.
         slcRTKeyExpr = ''
         slnRTFCount = FCOUNT()
         IF NOT EMPTY(slcPKFields)
            slnComma = 0
            sllMore = .T.
            DO WHILE sllMore
               slnComma = slnComma+1
               slnComma1 = AT(',',slcPKFields,slnComma)
               slnComma2 = AT(',',slcPKFields,slnComma+1)
               IF slnComma1 = 0 OR slnComma2 = 0
                  EXIT
               ENDIF
               slcKeyFld = ALLTRIM(SUBSTR(slcPKFields,slnComma1+1,;
                  slnComma2-slnComma1-1))
               FOR slnFor = 1 TO slnRTFCount
                  slcRTField = FIELD(slnFor)
                  IF slcKeyFld == slcRTField
                     * Locate the field's DD2 record.
                     = siFindDD2Rec(slcRTDBF,slcRTField)
                     IF sxlDD2Found
                        slcAddToExpr = ''
                        DO CASE
                           CASE ridd2.type $ 'CM'
                              slcAddToExpr = 'ALLTRIM('+slcRTField+')'
                           CASE ridd2.type = 'N'
                              slcAddToExpr = 'ALLTRIM(STR('+slcRTField+;
                                 ','+ALLTRIM(STR(ridd2.width))+','+;
                                 ALLTRIM(STR(ridd2.decimals))+'))'
                           CASE ridd2.type $ 'BF'
                              slcAddToExpr = 'ALLTRIM(STR('+slcRTField+;
                                 ',20,'+ALLTRIM(STR(ridd2.decimals))+'))'
                           CASE ridd2.type = 'I'
                              slcAddToExpr = 'ALLTRIM(STR('+slcRTField+;
                                 ',10,'+ALLTRIM(STR(ridd2.decimals))+'))'
                           CASE ridd2.type = 'Y'
                              slcAddToExpr = 'ALLTRIM(STR(MTON('+slcRTField+;
                                 '),20,4))'
                           CASE ridd2.type = 'T'
                              slcAddToExpr = 'TTOC('+slcRTField+')'
                           CASE ridd2.type = 'D'
                              slcAddToExpr = 'DTOC('+slcRTField+')'
                           CASE ridd2.type = 'L'
                              slcAddToExpr = 'IIF('+slcRTField+',"T","F")'
                        ENDCASE
                        IF NOT EMPTY(slcAddToExpr)
                           IF EMPTY(slcRTKeyExpr)
                              slcRTKeyExpr = slcAddToExpr
                           ELSE
                              slcRTKeyExpr = slcRTKeyExpr+'+","+'+slcAddToExpr
                           ENDIF
                        ENDIF
                     ENDIF
                     EXIT
                  ENDIF
               ENDFOR
            ENDDO
         ENDIF
         
         * Update the fields (foreign keys) in the "related" table.  Use
         * macro substitution in the SCAN line instead of EVAL() so that
         * Rushmore Technology is used to locate related records.
         SCAN FOR &slcRTFor
            slnRTRec = RECNO()
            IF slcSave_riworkfile_action = 'CHANGE'
               IF slnUpdCtrl = 1			&& Restrict
                  sllCancelRICheck = .T.
                  IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                     _SCREEN.ActiveForm.cRIMessage = 'There is a related '+;
                        'record in the table '+slcRTName+siTableDesc(slcRTName)+'.'
                  ENDIF
                  EXIT
               ENDIF
               
               * Create the key field value string for the current "related"
               * table record.
               IF EMPTY(slcRTKeyExpr)
                  slcRTKeyVal = ''
               ELSE
                  slcRTKeyVal = EVALUATE(slcRTKeyExpr)
               ENDIF
               
               * If any of the foreign key fields in the "related" table
               * record already has a record in the RI work file, bypass it
               * because it has already been modified.
               FOR slnFor = 1 TO 9
                  slcFor = STR(slnFor,1)
                  IF EMPTY(slcFKFld&slcFor)
                     LOOP
                  ENDIF
                  IF slnUpdCtrl = 3 AND NOT sllUpdate&slcFor
                     * IF Cascade and primary key field was not changed,
                     * there is nothing to do to the foreign key field.
                     LOOP
                  ENDIF
                  SELECT riworkfile
                  LOCATE FOR ALLTRIM(riworkfile.file_name) == slcRTDBFP AND ;
                     ALLTRIM(riworkfile.field_name) == slcFKFld&slcFor AND ;
                     riworkfile.rec_num = slnRTRec
                  IF FOUND()
                     GOTO slnSave_riworkfile_RecNo
                     SELECT (slcRTAlias)
                     LOOP
                  ENDIF
                  SELECT (slcRTAlias)
                  IF NOT RLOCK()
                     sllCancelRICheck = .T.
                     IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                        _SCREEN.ActiveForm.cRIMessage = 'A record in the '+;
                           'table '+slcRTName+siTableDesc(slcRTName)+;
                           ' could not be locked.'
                     ENDIF
                     EXIT
                  ENDIF
                  slRTOldVal = EVALUATE(slcFKFld&slcFor)
                  slcRTFType = TYPE('slRTOldVal')
                  IF slnUpdCtrl = 2			&& Set Null
                     DO CASE
                        CASE slcRTFType $ 'CM'
                           REPLACE (slcFKFld&slcFor) WITH ''
                        CASE slcRTFType = 'N'
                           REPLACE (slcFKFld&slcFor) WITH 0
                        CASE slcRTFType = 'T'
                           REPLACE (slcFKFld&slcFor) WITH $0
                        CASE slcRTFType = 'Y'
                           REPLACE (slcFKFld&slcFor) WITH CTOT('  /  /  ')
                        CASE slcRTFType = 'D'
                           REPLACE (slcFKFld&slcFor) WITH CTOD('  /  /  ')
                        CASE slcRTFType = 'L'
                           REPLACE (slcFKFld&slcFor) WITH .F.
                     ENDCASE
                  ELSE						&& Cascade
                     REPLACE (slcFKFld&slcFor) WITH slNewVal&slcFor
                  ENDIF
                  UNLOCK
                  slRTNewVal = EVALUATE(slcFKFld&slcFor)
                  slnRTDec = 0
                  * Locate the field's DD2 record.
                  = siFindDD2Rec(slcRTDBF,slcFKFld&slcFor)
                  DO CASE
                     CASE slcRTFType $ 'CM'
                        slcRTOldVal = slRTOldVal
                        slcRTNewVal = slRTNewVal
                     CASE slcRTFType = 'N'
                        * Get width and decimals from DD2.
                        IF sxlDD2Found
                           slnRTDec = ridd2.decimals
                           DO CASE
                              CASE ridd2.type = 'B'
                                 slnWidth = 20
                              CASE ridd2.type = 'F'
                                 slnWidth = 20
                              CASE ridd2.type = 'I'
                                 slnWidth = 10
                              OTHERWISE
                                 slnWidth = ridd2.width
                           ENDCASE
                        ELSE
                           slnWidth = 20
                           slnRTDec = 7
                        ENDIF
                        slcRTOldVal = ALLTRIM(STR(slRTOldVal,slnWidth,;
                           slnRTDec))
                        slcRTNewVal = ALLTRIM(STR(slRTNewVal,slnWidth,;
                           slnRTDec))
                     CASE slcRTFType = 'Y'
                        slcRTOldVal = STR(MTON(slRTOldVal),20,4)
                        slcRTNewVal = STR(MTON(slRTNewVal),20,4)
                     CASE slcRTFType = 'T'
                        slcRTOldVal = TTOC(slRTOldVal)
                        slcRTNewVal = TTOC(slRTNewVal)
                     CASE slcRTFType = 'D'
                        slcRTOldVal = DTOC(slRTOldVal)
                        slcRTNewVal = DTOC(slRTNewVal)
                     CASE slcRTFType = 'L'
                        IF slRTOldVal
                           slcRTOldVal = 'T'
                        ELSE
                           slcRTOldVal = 'F'
                        ENDIF
                        IF slRTNewVal
                           slcRTNewVal = 'T'
                        ELSE
                           slcRTNewVal = 'F'
                        ENDIF
                  ENDCASE
                  SELECT riworkfile
                  APPEND BLANK
                  REPLACE riworkfile.file_name WITH slcRTDBFP,;
                          riworkfile.field_name WITH slcFKFld&slcFor,;
                          riworkfile.action WITH 'CHANGE',;
                          riworkfile.rec_num WITH slnRTRec,;
                          riworkfile.type WITH slcRTFType,;
                          riworkfile.width WITH LEN(slcRTOldVal),;
                          riworkfile.decimals WITH slnRTDec,;
                          riworkfile.old_value WITH slcRTOldVal,;
                          riworkfile.new_value WITH slcRTNewVal,;
                          riworkfile.key_value WITH slcRTKeyVal,;
                          riworkfile.audit_trl WITH sxlAudit
                  GOTO slnSave_riworkfile_RecNo
                  SELECT (slcRTAlias)
               ENDFOR
               IF sllCancelRICheck
                  EXIT
               ENDIF
            ELSE							&& slcSave_riworkfile_action = 'DELETE'
               IF slnDelCtrl = 1			&& Restrict
                  sllCancelRICheck = .T.
                  IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                     _SCREEN.ActiveForm.cRIMessage = ;
                        'There is a related record in the table '+slcRTName+;
                        siTableDesc(slcRTName)+'.'
                  ENDIF
                  EXIT
               ENDIF
               
               * Create the key field value string for the current "related"
               * table record.
               IF EMPTY(slcRTKeyExpr)
                  slcRTKeyVal = ''
               ELSE
                  slcRTKeyVal = EVALUATE(slcRTKeyExpr)
               ENDIF
               
               IF slnDelCtrl = 2			&& Set Null
                  * If any of the foreign key fields in the "related" table
                  * already has a record in the RI work file, bypass it
                  * because it has already been modified.
                  FOR slnFor = 1 TO 9
                     slcFor = STR(slnFor,1)
                     IF EMPTY(slcFKFld&slcFor)
                        LOOP
                     ENDIF
                     SELECT riworkfile
                     LOCATE FOR ALLTRIM(riworkfile.file_name) == slcRTDBFP ;
                        AND ALLTRIM(riworkfile.field_name) == slcFKFld&slcFor AND ;
                        riworkfile.rec_num = slnRTRec
                     IF FOUND()
                        GOTO slnSave_riworkfile_RecNo
                        SELECT (slcRTAlias)
                        LOOP
                     ENDIF
                     SELECT (slcRTAlias)
                     IF NOT RLOCK()
                        sllCancelRICheck = .T.
                        IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                           _SCREEN.ActiveForm.cRIMessage = 'A record in '+;
                              'the table '+slcRTName+siTableDesc(slcRTName)+;
                              ' could not be locked.'
                        ENDIF
                        EXIT
                     ENDIF
                     slRTOldVal = EVALUATE(slcFKFld&slcFor)
                     slcRTFType = TYPE('slRTOldVal')
                     DO CASE
                        CASE slcRTFType $ 'CM'
                           REPLACE (slcFKFld&slcFor) WITH ''
                        CASE slcRTFType = 'N'
                           REPLACE (slcFKFld&slcFor) WITH 0
                        CASE slcRTFType = 'T'
                           REPLACE (slcFKFld&slcFor) WITH $0
                        CASE slcRTFType = 'Y'
                           REPLACE (slcFKFld&slcFor) WITH CTOT('  /  /  ')
                        CASE slcRTFType = 'D'
                           REPLACE (slcFKFld&slcFor) WITH CTOD('  /  /  ')
                        CASE slcRTFType = 'L'
                           REPLACE (slcFKFld&slcFor) WITH .F.
                     ENDCASE
                     UNLOCK
                     slRTNewVal = EVALUATE(slcFKFld&slcFor)
                     slnRTDec = 0
                     * Locate the field's DD2 record.
                     = siFindDD2Rec(slcRTDBF,slcFKFld&slcFor)
                     DO CASE
                        CASE slcRTFType $ 'CM'
                           slcRTOldVal = slRTOldVal
                           slcRTNewVal = slRTNewVal
                        CASE slcRTFType = 'N'
                           * Get width and decimals from DD2.
                           IF sxlDD2Found
                              slnRTDec = ridd2.decimals
                              DO CASE
                                 CASE ridd2.type = 'B'
                                    slnWidth = 20
                                 CASE ridd2.type = 'F'
                                    slnWidth = 20
                                 CASE ridd2.type = 'I'
                                    slnWidth = 10
                                 OTHERWISE
                                    slnWidth = ridd2.width
                              ENDCASE
                           ELSE
                              slnWidth = 20
                              slnRTDec = 7
                           ENDIF
                           slcRTOldVal = ALLTRIM(STR(slRTOldVal,;
                              slnWidth,slnRTDec))
                           slcRTNewVal = ALLTRIM(STR(slRTNewVal,;
                              slnWidth,slnRTDec))
                        CASE slcRTFType = 'Y'
                           slcRTOldVal = STR(MTON(slRTOldVal),20,4)
                           slcRTNewVal = STR(MTON(slRTNewVal),20,4)
                        CASE slcRTFType = 'T'
                           slcRTOldVal = TTOC(slRTOldVal)
                           slcRTNewVal = TTOC(slRTNewVal)
                        CASE slcRTFType = 'D'
                           slcRTOldVal = DTOC(slRTOldVal)
                           slcRTNewVal = DTOC(slRTNewVal)
                        CASE slcRTFType = 'L'
                           IF slRTOldVal
                              slcRTOldVal = 'T'
                           ELSE
                              slcRTOldVal = 'F'
                           ENDIF
                           IF slRTNewVal
                              slcRTNewVal = 'T'
                           ELSE
                              slcRTNewVal = 'F'
                           ENDIF
                     ENDCASE
                     SELECT riworkfile
                     APPEND BLANK
                     REPLACE riworkfile.file_name WITH slcRTDBFP,;
                             riworkfile.field_name WITH slcFKFld&slcFor,;
                             riworkfile.action WITH 'CHANGE',;
                             riworkfile.rec_num WITH slnRTRec,;
                             riworkfile.type WITH slcRTFType,;
                             riworkfile.width WITH LEN(slcRTOldVal),;
                             riworkfile.decimals WITH slnRTDec,;
                             riworkfile.old_value WITH slcRTOldVal,;
                             riworkfile.new_value WITH slcRTNewVal,;
                             riworkfile.key_value WITH slcRTKeyVal,;
                             riworkfile.audit_trl WITH sxlAudit
                     GOTO slnSave_riworkfile_RecNo
                     SELECT (slcRTAlias)
                  ENDFOR
                  IF sllCancelRICheck
                     EXIT
                  ENDIF
               ELSE							&& Cascade
                  FOR slnFor = 1 TO slnRTFCount
                     slcRTFld = FIELD(slnFor)
                     IF TYPE(slcRTFld) = 'G'
                        LOOP
                     ENDIF
                     * Unless the "Del Audit Trail - All Flds" checkbox on
                     * the Table Properties page of the Data Builder is
                     * checked, only primary key fields will be included in
                     * the audit trail for deleted records.
                     IF NOT sllDel_AllFld AND NOT EMPTY(slcPKFields) AND ;
                        AT(','+slcRTFld+',',slcPKFields) = 0
                        LOOP
                     ENDIF
                     * If this field already has a record in the RI work
                     * file, bypass it because it has already been modified.
                     SELECT riworkfile
                     LOCATE FOR ALLTRIM(riworkfile.file_name) == slcRTDBFP ;
                        AND ALLTRIM(riworkfile.field_name) == slcRTFld AND ;
                        riworkfile.rec_num = slnRTRec
                     IF FOUND()
                        GOTO slnSave_riworkfile_RecNo
                        SELECT (slcRTAlias)
                        LOOP
                     ENDIF
                     SELECT (slcRTAlias)
                     slRTOldVal = EVALUATE(slcRTFld)
                     slcRTFType = TYPE('slRTOldVal')
                     slnRTDec = 0
                     * Locate the field's DD2 record.
                     = siFindDD2Rec(slcRTDBF,slcRTFld)
                     DO CASE
                        CASE slcRTFType $ 'CM'
                           IF EMPTY(slRTOldVal)
                              sxlAudit = .F.
                           ENDIF
                           slcRTOldVal = slRTOldVal
                        CASE slcRTFType = 'N'
                           IF slRTOldVal = 0
                              sxlAudit = .F.
                           ENDIF
                           * Get width and decimals from DD2.
                           IF sxlDD2Found
                              slnRTDec = ridd2.decimals
                              DO CASE
                                 CASE ridd2.type = 'B'
                                    slnWidth = 20
                                 CASE ridd2.type = 'F'
                                    slnWidth = 20
                                 CASE ridd2.type = 'I'
                                    slnWidth = 10
                                 OTHERWISE
                                    slnWidth = ridd2.width
                              ENDCASE
                           ELSE
                              slnWidth = 20
                              slnRTDec = 7
                           ENDIF
                           slcRTOldVal = ALLTRIM(STR(slRTOldVal,slnWidth,;
                              slnRTDec))
                        CASE slcRTFType = 'Y'
                           IF EMPTY(slRTOldVal)
                              sxlAudit = .F.
                           ENDIF
                           slcRTOldVal = STR(MTON(slRTOldVal),20,4)
                        CASE slcRTFType = 'T'
                           IF EMPTY(slRTOldVal)
                              sxlAudit = .F.
                           ENDIF
                           slcRTOldVal = TTOC(slRTOldVal)
                        CASE slcRTFType = 'D'
                           IF EMPTY(slRTOldVal)
                              sxlAudit = .F.
                           ENDIF
                           slcRTOldVal = DTOC(slRTOldVal)
                        CASE slcRTFType = 'L'
                           IF slRTOldVal
                              slcRTOldVal = 'T'
                           ELSE
                              slcRTOldVal = 'F'
                           ENDIF
                     ENDCASE
                     SELECT riworkfile
                     APPEND BLANK
                     REPLACE riworkfile.file_name WITH slcRTDBFP,;
                             riworkfile.field_name WITH slcRTFld,;
                             riworkfile.action WITH 'DELETE',;
                             riworkfile.rec_num WITH slnRTRec,;
                             riworkfile.type WITH slcRTFType,;
                             riworkfile.width WITH LEN(slcRTOldVal),;
                             riworkfile.decimals WITH slnRTDec,;
                             riworkfile.old_value WITH slcRTOldVal,;
                             riworkfile.key_value WITH slcRTKeyVal,;
                             riworkfile.audit_trl WITH sxlAudit
                     GOTO slnSave_riworkfile_RecNo
                     SELECT (slcRTAlias)
                  ENDFOR
               ENDIF
            ENDIF							&& IF slcSave_riworkfile_action = 'CHANGE'
            SELECT (slcRTAlias)
         ENDSCAN							&& SCAN FOR &slcRTFor
         IF sllCancelRICheck
            EXIT
         ENDIF
         SELECT ridd2
         GOTO slnSave_ridd2_RecNo
      ENDSCAN								&& SCAN FOR val_type = 3
      IF sllCancelRICheck
         * SCANs must be exited before the tables being SCANned can be
         * closed.
         = siRICancel()
         RETURN 2
      ENDIF
      sllFirstField = .F.
      SELECT riworkfile
      oApp.Next(1,SET('DATASESSION'))
   ENDDO									&& DO WHILE NOT EOF()

   IF slnParentTableNo > 0
      FOR slnFor = 1 TO slnParentTableNo
         slcParentAlias = 'PARTBL'+ALLTRIM(STR(slnFor))
         IF USED(slcParentAlias)
            USE IN (slcParentAlias)
         ENDIF
      ENDFOR
   ENDIF

   * Delete records in "related" table(s).
   IF sgnRIUpd > 0
      slcRTFile = ''
      slcRTAlias = ''
      slnRTRec = 0
      SELECT riworkfile
      SCAN FOR riworkfile.action = 'DELETE' AND RECNO() > slnNumCur AND ;
         NOT EMPTY(riworkfile.file_name) AND riworkfile.rec_num > 0
         IF NOT ALLTRIM(riworkfile.file_name) == slcRTFile OR ;
            riworkfile.rec_num <> slnRTRec
            slnRTRec = riworkfile.rec_num
            IF ALLTRIM(riworkfile.file_name) == slcRTFile
               SELECT (slcRTAlias)
               sllOK = .T.
            ELSE
               sllOK = .F.
               FOR slnFor = 1 TO sgnRIUpd
                  IF ALLTRIM(SUBSTR(riworkfile.file_name,;
                     RAT('\',riworkfile.file_name)+1)) == sgaRIUpd(slnFor,1)
                     IF USED(sgaRIUpd(slnFor,2))
                        slcRTFile = ALLTRIM(riworkfile.file_name)
                        slcRTAlias = sgaRIUpd(slnFor,2)
                        SELECT (slcRTAlias)
                        sllOK = .T.
                     ENDIF
                     EXIT
                  ENDIF
               ENDFOR
            ENDIF
            IF sllOK
               GOTO slnRTRec
               IF RLOCK()
                  DELETE
                  UNLOCK
               ELSE
                  sllCancelRICheck = .T.
                  IF TYPE('_SCREEN.ActiveForm.cRIMessage') <> 'U'
                     _SCREEN.ActiveForm.cRIMessage = 'A record in the '+;
                        'table '+slcRTFile+siTableDesc(slcRTFile)+;
                        ' could not be locked.'
                  ENDIF
                  EXIT
               ENDIF
            ENDIF
            SELECT riworkfile
         ENDIF
      ENDSCAN
      IF sllCancelRICheck
         * SCAN must be exited before the table being SCANned can be closed.
         = siRICancel()
         RETURN 2
      ENDIF
   ENDIF
ENDIF										&& IF slcMode <> 'NEW'

* Close the data dictionary tables.
IF USED('ridd1')
   USE IN ridd1
ENDIF
IF USED('ridd2')
   USE IN ridd2
ENDIF

SELECT (sxnCurArea)
WAIT CLEAR
RETURN 0

********************
FUNCTION siTableDesc
********************
* Find the record in the DD1 table for the table specified in the
* parameter.

PARAM stcFindTbl

IF USED('ridd1') AND SEEK(PADR(LEFT(stcFindTbl,112),112),'ridd1')
   RETURN ' ('+ALLTRIM(ridd1.desc)+')'
ELSE
   RETURN ''
ENDIF

*********************
FUNCTION siFindDD2Rec
*********************
* Find the record in the DD2 table for the table and field specified in the
* parameters so that the field properties can be accessed.

PARAM stcFindTbl,stcFindFld

LOCAL slnCurArea

slnCurArea = SELECT()
SELECT ridd2
IF SEEK(PADR(LEFT(stcFindTbl,112),112)+PADR(stcFindFld,128))
   sxlDD2Found = .T.
   IF sxlNoAuditTrail
      sxlAudit = .F.
   ELSE
      sxlAudit = ridd2.audit_trl
   ENDIF
ELSE
   sxlDD2Found = .F.
   sxlAudit = .F.
ENDIF
SELECT (slnCurArea)

*******************
FUNCTION siRICancel
*******************
* Close the RI work file and cancel any changes made to "related" tables.

LOCAL slnFor

* Close the data dictionary tables.
IF USED('ridd1')
   USE IN ridd1
ENDIF
IF USED('ridd2')
   USE IN ridd2
ENDIF

* Close and delete the RI work file.
IF USED('riworkfile')
   USE IN riworkfile
ENDIF

* Cancel changes made to "related" tables.  Note: For "related" tables,
* table buffering is enabled if a transaction is not in progress.
IF sgnRIUpd > 0
   FOR slnFor = 1 TO sgnRIUpd
      IF USED(sgaRIUpd(slnFor,2))
         SELECT (sgaRIUpd(slnFor,2))
         IF TXNLEVEL() = 0
            = TABLEREVERT(.T.)
         ENDIF
         USE
      ENDIF
   ENDFOR
ENDIF

RELEASE sgnRIUpd,sgaRIUpd
SELECT (sxnCurArea)
WAIT CLEAR
