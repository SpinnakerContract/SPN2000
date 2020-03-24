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

***************
* S<Prefix>VFld - Validation routine called from the valid method of form
***************   controls.

#INCLUDE 'MAIN.H'

LPARAM stcDDFile,stcDDField,stcControlSource,stCurValue,stcAlias

* Parameters:
* stcDDFile        - File name for data dictionary lookup (not alias).
* stcDDField       - Field name for data dictionary lookup.
* stcControlSource - ControlSource for the current control.
* stCurValue       - Value being validated.
* stcAlias         - Table alias of field

* Private variables are used in functions within this program, the Code
* validation function, or the Cleanup Code function.
PRIVATE sllRetVal,slcRetMess,slCurValue,sxcAlias,sxlAutoList,sxlTestForm,;
   sxlDD1SQLView,sxcDDToUse,sxcDD2ValData,sxlF2Cancel,sxcDD2CodeFunc,;
   sxcDD1Alias,sxcEditForm

LOCAL sloForm,slcDDFile,slcDDField,slcCtrlSrce,slnDot,slnCurRec,slnCurSel,;
   slcDD1PrimaryTag,slcDD1PrimaryFld,slcDD1,slcDD2,slnBS,;
   slcDD2Alias,slnDD2FieldOrder,slcDD2CleanupFunc,slcDD2ErrExpr,sllDD2Unique,;
   sllValCont,sllMoreSA,slnQuote1,slnQuote2,slnQuote3,slnQuote4,slcSAFile,;
   slcSAField,sllUniqueOK,slcFailMess,slcSrcTbl,slcSrcFld,slcViewPKTag,;
   slcSrcPKTag,slcViewPKExpr,slcSrcPKExpr,slcViewPKValue,sllOver,sllNoMess,;
   slcAutoMess,sllOverride,sllDoOver,slcOverMess,sllUpdateChild,slcKey,;
   slnComma,slnComma1,slnComma2,slcKeyFld,slcUpdateAliases,slnNumUA,;
   slcChildAlias

IF TYPE('oApp') = 'O'
   oApp.lValidPassed = .T.
ENDIF

IF LASTKEY() = -1 OR EMPTY(ON('KEY','F2')) OR ON('KEY','F2') = '*'
   RETURN
ENDIF

* On an On-Form Toolbar, disabling the New or Copy button while they have
* focus causes the focus to move off the control that was selected in the
* form's AddNew method to have focus.  The following code will prevent the
* focus from moving by returning a value of zero.  Note that the focus is
* attempted to be moved twice.
IF TYPE('sgnValidRet0') = 'N'
   IF sgnValidRet0 < 2 AND EMPTY(stCurValue)
      sgnValidRet0 = sgnValidRet0+1
      RETURN 0
   ELSE
      RELEASE sgnValidRet0
   ENDIF
ENDIF

IF TYPE('sglNoValid') = 'L' AND sglNoValid
   * Created in the WriteBuffer method of the Form_Toolbar class to prevent
   * validation when cancelling on an editable grid.
   RETURN
ENDIF

IF TYPE('_SCREEN.ActiveForm') <> 'O'
   RETURN
ENDIF
sloForm = _SCREEN.ActiveForm

IF EMPTY(stcDDFile)
   RETURN
ENDIF
slcDDFile = ALLTRIM(UPPER(stcDDFile))

IF EMPTY(stcDDField)
   RETURN
ENDIF
slcDDField = ALLTRIM(UPPER(stcDDField))

IF EMPTY(stcAlias)
   slcCtrlSrce = ALLTRIM(stcControlSource)
   IF EMPTY(slcCtrlSrce)
      sxcAlias = ALIAS()
   ELSE
      IF EOF()
         RETURN
      ENDIF
      slnDot = RAT('.',slcCtrlSrce)
      IF slnDot = 0
         sxcAlias = ALIAS()
      ELSE
         IF slnDot = 1 OR slnDot = LEN(slcCtrlSrce)
            RETURN
         ENDIF
         sxcAlias = LEFT(slcCtrlSrce,slnDot-1)
         IF NOT USED(sxcAlias)
            sxcAlias = ALIAS()
         ENDIF
      ENDIF
   ENDIF
ELSE
   sxcAlias = ALLTRIM(stcAlias)
ENDIF
IF EMPTY(sxcAlias)
   IF EOF()
      slnCurRec = 0
   ELSE
      slnCurRec = RECNO()
   ENDIF
ELSE
   IF EOF(sxcAlias)
      slnCurRec = 0
   ELSE
      slnCurRec = RECNO(sxcAlias)
   ENDIF
ENDIF

sllRetVal = .T.          			&& Pass validation by default
slcRetMess = ''
slCurValue = stCurValue

PUSH KEY CLEAR

IF TYPE('sloForm.ActiveControl.lAddLookupCommandButton') <> 'U' ;
   AND TYPE('sloForm.ActiveControl.lAddLookupOnFocus') <> 'U' ;
   AND (sloForm.ActiveControl.lAddLookupCommandButton OR ;
   sloForm.ActiveControl.lAddLookupOnFocus)
   * If the current control has a corresponding Lookup button, an invalid
   * value in the control prevents the usage of the Lookup button unless
   * the Auto List property for the field is set to true in the Data
   * Builder.  This code forces the setting of the Auto List property to 
   * true if a Lookup button exists for the current control.
   sxlAutoList = .T.
ELSE
   sxlAutoList = .F.
ENDIF
sxcEditForm = ''

* Determine the currently selected work area.
slnCurSel = SELECT()

* Open the application's DD1 and DD2 tables.
sxlTestForm = IIF(TYPE('sglTestForm') = 'L',sglTestForm,.F.)
IF sgcAppPre == 'APP'
   IF sxlTestForm
      slcDD1 = sgcPDD1
      slcDD2 = sgcPDD2
      sxcDDToUse = 'PDD'
   ELSE
      slcDD1 = 'SAPPDD1'
      slcDD2 = 'SAPPDD2'
      sxcDDToUse = ''
   ENDIF
ELSE
   slcDD1 = sgcPDD1
   slcDD2 = sgcPDD2
   sxcDDToUse = ''
ENDIF
*
sxcDD1Alias = slcDD1
slnDot = oApp.ExtDotPos(sxcDD1Alias)
IF slnDot > 0
   sxcDD1Alias = LEFT(sxcDD1Alias,slnDot-1)
ENDIF
slnBS = RAT('\',sxcDD1Alias)
IF slnBS > 0
   sxcDD1Alias = SUBSTR(sxcDD1Alias,slnBS+1)
ENDIF
IF USED(sxcDD1Alias)
   SELECT (sxcDD1Alias)
ELSE
   IF NOT EVAL(sgcUUse+"(slcDD1,'','','','',sxcDDToUse)")
      POP KEY
      RETURN
   ENDIF
ENDIF
* Find the DD1 record using parameters passed to this subroutine.
* If found, proceed with validation.  Otherwise, the validation test
* is considered to have been passed since there is no way to validate
* the current field.
oApp.SetOrder(ALIAS(),SET('DATASESSION'),'file_name')
IF NOT SEEK(PADR(LEFT(slcDDFile,112),112))
   SELECT (slnCurSel)
   POP KEY
   RETURN
ENDIF
slcDD1PrimaryFld = ALLTRIM(primaryfld)
slcDD1PrimaryTag = ALLTRIM(primarytag)
sxlDD1SQLView = sqlview
*
slcDD2Alias = slcDD2
slnDot = oApp.ExtDotPos(slcDD2Alias)
IF slnDot > 0
   slcDD2Alias = LEFT(slcDD2Alias,slnDot-1)
ENDIF
slnBS = RAT('\',slcDD2Alias)
IF slnBS > 0
   slcDD2Alias = SUBSTR(slcDD2Alias,slnBS+1)
ENDIF
IF USED(slcDD2Alias)
   SELECT (slcDD2Alias)
ELSE
   IF NOT EVAL(sgcUUse+"(slcDD2,'','','','',sxcDDToUse)")
      POP KEY
      RETURN
   ENDIF
ENDIF
* Find the DD2 record using parameters passed to this subroutine.
* If found, proceed with validation.  Otherwise, the validation test
* is considered to have been passed since there is no way to validate
* the current field.
oApp.SetOrder(ALIAS(),SET('DATASESSION'),'file_name')
IF NOT SEEK(PADR(LEFT(slcDDFile,112),112)+PADR(slcDDField,128))
   SELECT (slnCurSel)
   POP KEY
   RETURN
ENDIF
sxcDD2ValData = ALLTRIM(val_data)
slnDD2FieldOrder = order
slcDD2CleanupFunc = ALLTRIM(clean_func)
sxcDD2CodeFunc = ALLTRIM(code_func)
slcDD2ErrExpr = ALLTRIM(error)
sllDD2Unique = unique

* Continue if the current field is to be validated and the field does not
* contain a null/blank value when a null/blank value is allowed.
IF validate > 1 AND NOT ((ISNULL(slCurValue) OR EMPTY(slCurValue)) AND ;
   (validate = 3 OR validate = 5))
   IF val_type = 6 OR NOT EMPTY(sxcDD2ValData)
      sllValCont = .T.
      * If validation type is "Same As", find that record.
      IF val_type = 4
         sllMoreSA = .T.
         DO WHILE sllMoreSA
            slnQuote1 = AT('"',sxcDD2ValData,1)
            slnQuote2 = AT('"',sxcDD2ValData,2)
            slnQuote3 = AT('"',sxcDD2ValData,3)
            slnQuote4 = AT('"',sxcDD2ValData,4)
            IF slnQuote1 > 0 AND slnQuote2 > 0 AND slnQuote3 > 0 AND slnQuote4 > 0
               slcSAFile = ALLTRIM(UPPER(SUBSTR(sxcDD2ValData,slnQuote1+1,;
                  slnQuote2-slnQuote1-1)))
               slcSAField = LTRIM(UPPER(SUBSTR(sxcDD2ValData,slnQuote3+1,;
                  slnQuote4-slnQuote3-1)))
               IF SEEK(PADR(LEFT(slcSAFile,112),112)+PADR(slcSAField,128))
                  IF val_type <> 4    		&& Doesn't point to another rec
                     sllMoreSA = .F.
                     sxcDD2ValData = ALLTRIM(val_data)
                     IF EMPTY(slcDD2CleanupFunc)
                        slcDD2CleanupFunc = ALLTRIM(clean_func)
                     ENDIF
                     IF EMPTY(slcDD2ErrExpr)
                        slcDD2ErrExpr = ALLTRIM(error)
                     ENDIF
                  ENDIF
               ELSE
                  sllValCont = .F.
                  sllMoreSA = .F.
               ENDIF
            ELSE
               sllValCont = .F.
               sllMoreSA = .F.
            ENDIF
         ENDDO
      ENDIF
      IF sllValCont
         DO CASE
            CASE val_type = 5
               * Code
               = siCode()
            CASE val_type = 6
               * Not Null or Blank
               IF ISNULL(slCurValue) OR EMPTY(slCurValue)
                  sllRetVal = .F.
               ENDIF
            CASE ISNULL(slCurValue)
               * A null value would fail Range, Set and Referential
               * validation so there is no need to perform those
               * validations.
               sllRetVal = .F.
            CASE val_type = 1
               * Range
               = siRange()
            CASE val_type = 2
               * Set
               = siSet()
            CASE val_type = 3
               * Referential
               = siRef()
         ENDCASE
      ENDIF
   ENDIF
ENDIF

sllUniqueOK = .T.
IF sllRetVal AND sllDD2Unique
   WAIT WINDOW NOWAIT 'Checking for uniqueness...'
   slcFailMess = 'The value in this field is not unique.'
   IF NOT sxlDD1SQLView			&& Table
      IF EVAL(sgcUUse+"(slcDDFile,'uniq','again','','',sxcDDToUse)")
         LOCATE FOR &slcDDField == slCurValue
         IF FOUND()
            IF RECNO() = slnCurRec
               CONTINUE
               IF FOUND()
                  sllUniqueOK = .F.
               ENDIF
            ELSE
               sllUniqueOK = .F.
            ENDIF
         ENDIF
         USE
      ELSE
         sllUniqueOK = .F.
         slcFailMess = 'The table '+slcDDFile+' could not be opened.  '+;
            'The uniqueness check cannot be performed.'
      ENDIF
   ELSE							&& View
      SELECT (slcDD2Alias)
      slcSrcTbl = ALLTRIM(UPPER(viewsrctbl))
      slcSrcFld = ALLTRIM(viewsrcfld)
      SELECT (sxcDD1Alias)
      IF NOT EMPTY(slcDD1PrimaryTag)
         slcViewPKTag = ALLTRIM(UPPER(slcDD1PrimaryTag))
         IF SEEK(PADR(LEFT(slcSrcTbl,112),112)) AND NOT EMPTY(primarytag)
            slcSrcPKTag = ALLTRIM(UPPER(primarytag))
         ELSE
            sllUniqueOK = .F.
            slcFailMess = 'A primary key tag has not been specified for '+;
               'the table '+slcSrcTbl+'.  The uniqueness check cannot be performed.'
         ENDIF
      ELSE
         sllUniqueOK = .F.
         slcFailMess = 'A primary key tag has not been specified for '+;
            'the view '+slcDDFile+'.  The uniqueness check cannot be performed.'
      ENDIF
      IF sllUniqueOK
         IF oApp.GetIndexInfo(SET('DATASESSION'),slcDDFile,slcViewPKTag)
            slcViewPKExpr = ALLTRIM(oapp_dind.index_expr)
            IF oApp.GetIndexInfo(SET('DATASESSION'),slcSrcTbl,slcSrcPKTag)
               slcSrcPKExpr = ALLTRIM(oapp_dind.index_expr)
            ELSE
               sllUniqueOK = .F.
               slcFailMess = 'The primary key expression could not be determined for '+;
                  'the table '+slcSrcTbl+'.  The uniqueness check cannot be performed.'
            ENDIF
         ELSE
            sllUniqueOK = .F.
            slcFailMess = 'The primary key expression could not be determined for '+;
               'the view '+slcDDFile+'.  The uniqueness check cannot be performed.'
         ENDIF
         IF sllUniqueOK
            SELECT (slnCurSel)
            slcViewPKValue = OLDVAL(slcViewPKExpr)
            IF EVAL(sgcUUse+"(slcSrcTbl,'uniq','again','','',sxcDDToUse)")
               LOCATE FOR &slcSrcFld == slCurValue
               IF FOUND()
                  IF slcViewPKValue == EVAL(slcSrcPKExpr)
                     CONTINUE
                     IF FOUND()
                        sllUniqueOK = .F.
                     ENDIF
                  ELSE
                     sllUniqueOK = .F.
                  ENDIF
               ENDIF
               USE
            ELSE
               sllUniqueOK = .F.
               slcFailMess = 'The table '+slcSrcTbl+' could not be opened.  '+;
                  'The uniqueness check cannot be performed.'
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   IF sllUniqueOK
      WAIT CLEAR
   ELSE
      * Failed uniqueness check.
      sllRetVal = 0
      ?? CHR(7)
      WAIT WINDOW NOWAIT slcFailMess
   ENDIF
ENDIF

SELECT (slcDD2Alias)
IF validate = 4 OR validate = 5		&& Override OK
   sllOver = .T.
ELSE
   sllOver = .F.
ENDIF

* ON KEY LABELs should be restored before F2 dialog is brought up.
POP KEY

IF sllUniqueOK
   sllNoMess = .F.
   IF NOT sllRetVal AND sxlAutoList AND (EMPTY(sxcEditForm) OR ATC('.T.',ON('KEY','F3')) = 0)
      sllNoMess = .T.
      slcAutoMess = ''
      IF NOT EMPTY(slcDD2ErrExpr)
         SELECT (slnCurSel)
         slcAutoMess = EVAL(slcDD2ErrExpr)
      ENDIF
      IF EMPTY(slcAutoMess)
         slcAutoMess = 'Invalid Input'
      ENDIF
      ?? CHR(7)
      WAIT WINDOW NOWAIT slcAutoMess
      SELECT (slnCurSel)
      sxlF2Cancel = .T.				&& Changed in S<Prefix>HF2
      = EVAL(sgcFCT+"('HELP_F2')")
      IF NOT sxlF2Cancel
         sllRetVal = .T.
      ENDIF
   ENDIF

   IF sllRetVal AND NOT EMPTY(slcDD2CleanupFunc)
      SELECT (slnCurSel)
      sllOverride = .F.
      IF sgcAppPre == 'APP'
         IF sxlTestForm
            DO (slcDD2CleanupFunc) IN ('S'+sgcCurPre+'VC')
         ELSE
            DO (slcDD2CleanupFunc) IN SAppVC
         ENDIF
      ELSE
         DO (slcDD2CleanupFunc) IN ('S'+sgcAppPre+'VC')
      ENDIF
   ENDIF

   IF sllOver
      sllDoOver = .F.
      IF TYPE('sllRetVal') = 'L'
         IF NOT sllRetVal
            sllDoOver = .T.
         ENDIF
      ELSE
         IF TYPE('sllRetVal') = 'N'
            IF sllRetVal = 0
               sllDoOver = .T.
            ENDIF
         ENDIF
      ENDIF
      IF sllDoOver
         IF EMPTY(slcRetMess)
            slcOverMess = 'Invalid Input.'
         ELSE
            slcOverMess = ALLTRIM(slcRetMess)
         ENDIF
         IF RIGHT(slcOverMess,1) <> '.'
            slcOverMess = slcOverMess+'.'
         ENDIF
         slcOverMess = slcOverMess+'  Do you want to override?'
         ?? CHR(7)
         IF MESSAGEBOX(slcOverMess,MB_ICONQUESTION+MB_YESNO,;
            sgcAppTitle) = IDYES
            sllRetVal = .T.
            IF NOT EMPTY(slcDD2CleanupFunc)
               IF USED('f2reftable')
                  USE IN f2reftable
               ENDIF
               SELECT (slnCurSel)
               sllOverride = .T.
               IF sgcAppPre == 'APP'
                  IF sxlTestForm
                     DO (slcDD2CleanupFunc) IN ('S'+sgcCurPre+'VC')
                  ELSE
                     DO (slcDD2CleanupFunc) IN SAppVC
                  ENDIF
               ELSE
                  DO (slcDD2CleanupFunc) IN ('S'+sgcAppPre+'VC')
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF TYPE('sllRetVal') = 'L'
      IF NOT sllRetVal
         sllRetVal = 0
      ENDIF
   ENDIF

   IF TYPE('sllRetVal') = 'N' AND NOT sllNoMess
      IF sllRetVal = 0
         IF NOT EMPTY(slcDD2ErrExpr)
            SELECT (slnCurSel)
            slcRetMess = EVAL(slcDD2ErrExpr)
         ENDIF
         IF EMPTY(slcRetMess)
            slcRetMess = 'Invalid Input'
         ENDIF
         ?? CHR(7)
         WAIT WINDOW NOWAIT slcRetMess
      ENDIF
   ENDIF
ENDIF

IF USED('f2reftable')
   USE IN f2reftable
ENDIF
SELECT (slnCurSel)

IF TYPE('oApp') = 'O' AND ((TYPE('sllRetVal') = 'L' AND NOT sllRetVal) OR ;
   (TYPE('sllRetVal') = 'N' AND sllRetVal = 0))
   oApp.lValidPassed = .F.
ELSE
   * Validation passed.  See if the field that was just validated has been
   * changed and is in the primary key of the Initial Selected Alias table.
   * If so, and the form is editing multiple tables, the foreign key field
   * in the child tables that are being edited must be updated.
   IF TYPE('sloForm.cUpdateAliases') <> 'U' AND ;
      NOT EMPTY(sloForm.cUpdateAliases) AND ;
      TYPE('sloForm.DataEnvironment.InitialSelectedAlias') <> 'U' AND ;
      NOT EMPTY(sxcAlias) AND ALLTRIM(UPPER(sxcAlias)) == ;
      ALLTRIM(UPPER(sloForm.DataEnvironment.InitialSelectedAlias)) AND ;
      slnDD2FieldOrder > 0 AND ;
      SUBSTR(GETFLDSTATE(-1,sxcAlias),slnDD2FieldOrder+1,1) $ '24'
      * Open the application's DD1 table.  See if field is in primary key.
      sllUpdateChild = .F.
      IF NOT EMPTY(slcDD1PrimaryFld)
         slcKey = ','+ALLTRIM(UPPER(slcDD1PrimaryFld))+','
         slnComma = 0
         DO WHILE .T.
            slnComma = slnComma+1
            slnComma1 = AT(',',slcKey,slnComma)
            slnComma2 = AT(',',slcKey,slnComma+1)
            IF slnComma1 = 0 OR slnComma2 = 0
               EXIT
            ENDIF
            slcKeyFld = ALLTRIM(SUBSTR(slcKey,slnComma1+1,;
               slnComma2-slnComma1-1))
            IF slcDDField == slcKeyFld
               IF TYPE('sloForm.lEditableGrid') <> 'U' AND ;
                  sloForm.lEditableGrid AND ;
                  TYPE('sloForm.lEditMultTable_PKChanged') <> 'U'
                  * After this program is exited, the AfterRowColChange
                  * method of the editable grid will be executed.  It
                  * will detect that the above property has been set to
                  * true and prompt the user to save or cancel the change
                  * to the PK field.  Upon saving, the RI routine will
                  * update the foreign key fields in the related child 
                  * tables.
                  sloForm.lEditMultTable_PKChanged = .T.
               ELSE
                  sllUpdateChild = .T.
               ENDIF
               EXIT
            ENDIF
         ENDDO
      ENDIF
      SELECT (slnCurSel)
      IF sllUpdateChild AND sloForm.IsNew()
         * Update foreign key fields in child records if in add mode.  If
         * not adding, let RI update these fields.
         slcUpdateAliases = ALLTRIM(sloForm.cUpdateAliases)
         IF LEFT(slcUpdateAliases,1) <> ','
            slcUpdateAliases = ','+slcUpdateAliases
         ENDIF
         IF RIGHT(slcUpdateAliases,1) <> ','
            slcUpdateAliases = slcUpdateAliases+','
         ENDIF
         slnNumUA = 0
         slnComma = 0
         DO WHILE .T.
            slnComma = slnComma+1
            slnComma1 = AT(',',slcUpdateAliases,slnComma)
            slnComma2 = AT(',',slcUpdateAliases,slnComma+1)
            IF slnComma1 = 0 OR slnComma2 = 0
               EXIT
            ENDIF
            slcChildAlias = ALLTRIM(UPPER(SUBSTR(slcUpdateAliases,;
               slnComma1+1,slnComma2-slnComma1-1)))
            IF NOT EMPTY(slcChildAlias) AND USED(slcChildAlias)
               slnNumUA = slnNumUA+1
               SELECT (slcChildAlias)
               sloForm.UA_Link(slnNumUA)   
            ENDIF
         ENDDO
         SELECT (slnCurSel)
      ENDIF
   ENDIF
ENDIF

RETURN sllRetVal

****************
FUNCTION siRange
****************
* Range validation routine.

LOCAL slnQuote1,slnQuote2,sllRFrom,slcRngFrom,sllRTo,slcRngTo,sllNotInR

slnQuote1 = AT('"',sxcDD2ValData,1)
slnQuote2 = AT('"',sxcDD2ValData,2)
sllRFrom = .F.
IF slnQuote1 > 0 AND slnQuote2 > 0
   sllRFrom = .T.
   slcRngFrom = SUBSTR(sxcDD2ValData,slnQuote1+1,slnQuote2-slnQuote1-1)
ENDIF
slnQuote1 = AT('"',sxcDD2ValData,3)
slnQuote2 = AT('"',sxcDD2ValData,4)
sllRTo = .F.
IF slnQuote1 > 0 AND slnQuote2 > 0
   sllRTo = .T.
   slcRngTo = SUBSTR(sxcDD2ValData,slnQuote1+1,slnQuote2-slnQuote1-1)
ENDIF
IF sllRFrom AND sllRTo
   sllNotInR = .F.
   DO CASE
      CASE type = 'C'
         IF LEN(slcRngFrom) > width
            slcRngFrom = LEFT(slcRngFrom,width)
         ELSE
            slcRngFrom = slcRngFrom+SPACE(width-LEN(slcRngFrom))
         ENDIF
         IF LEN(slcRngTo) > width
            slcRngTo = LEFT(slcRngTo,width)
         ELSE
            slcRngTo = slcRngTo+SPACE(width-LEN(slcRngTo))
         ENDIF
         IF UPPER(slCurValue) < UPPER(slcRngFrom) OR ;
            UPPER(slCurValue) > UPPER(slcRngTo)
            sllNotInR = .T.
         ENDIF
      CASE type = 'N' OR type = 'F' OR type = 'I' OR type = 'Y'
         IF slCurValue < VAL(slcRngFrom) OR ;
            slCurValue > VAL(slcRngTo)
            sllNotInR = .T.
         ENDIF
      CASE type = 'D'
         IF slCurValue < CTOD(slcRngFrom) OR ;
            slCurValue > CTOD(slcRngTo)
            sllNotInR = .T.
         ENDIF
      CASE type = 'T'
         IF slCurValue < CTOT(slcRngFrom) OR ;
            slCurValue > CTOT(slcRngTo)
            sllNotInR = .T.
         ENDIF
   ENDCASE
   IF sllNotInR
      sllRetVal = .F.
   ENDIF
ENDIF

**************
FUNCTION siSet
**************
* Set validation routine.

LOCAL slnSetElem,sllNoMatch,sllMoreItems,slnQuote1,slnQuote2,slcSetVal

slnSetElem = 0
sllNoMatch = .T.
sllMoreItems = .T.
DO WHILE sllMoreItems
   slnSetElem = slnSetElem+1
   slnQuote1 = AT('"',sxcDD2ValData,slnSetElem*2-1)
   slnQuote2 = AT('"',sxcDD2ValData,slnSetElem*2)
   IF slnQuote1 > 0 AND slnQuote2 > 0
      slcSetVal = ALLTRIM(LEFT(SUBSTR(sxcDD2ValData,slnQuote1+1,;
         slnQuote2-slnQuote1-1),width))
      DO CASE
         CASE type = 'C'
            IF UPPER(slcSetVal) == UPPER(ALLTRIM(slCurValue))
               sllNoMatch = .F.
               sllMoreItems = .F.
            ENDIF
         CASE type = 'N' OR type = 'F' OR type = 'I' OR type = 'Y'
            IF VAL(slcSetVal) = slCurValue
               sllNoMatch = .F.
               sllMoreItems = .F.
            ENDIF
         CASE type = 'D'
            IF CTOD(slcSetVal) = slCurValue
               sllNoMatch = .F.
               sllMoreItems = .F.
            ENDIF
         CASE type = 'T'
            IF CTOT(slcSetVal) = slCurValue
               sllNoMatch = .F.
               sllMoreItems = .F.
            ENDIF
         OTHERWISE
            sllNoMatch = .F.
            sllMoreItems = .F.
      ENDCASE
   ELSE
      sllMoreItems = .F.
   ENDIF
ENDDO
IF sllNoMatch
   sllRetVal = .F.
ENDIF
IF 'AUTOLIST' $ sxcDD2ValData
   sxlAutoList = .T.
ENDIF

**************
FUNCTION siRef
**************
* Referential validation routine.

LOCAL slnQuote1,slnQuote2,slcRefTableOrView,slcOrigDBC,sllOK,slcFiltCd,;
   slcForFlds,slnFor,slcFK,slcPriFlds,slcPK,slcFor,slnNumFlds,slcForFld,;
   slcPriFld,slcDatabase,slcLoc,slcDD0

slnQuote1 = AT('"',sxcDD2ValData,1)
slnQuote2 = AT('"',sxcDD2ValData,2)
IF slnQuote1 > 0 AND slnQuote2 > 0
   slcRefTableOrView = UPPER(ALLTRIM(SUBSTR(sxcDD2ValData,slnQuote1+1,slnQuote2-;
      slnQuote1-1)))
   IF LEFT(slcRefTableOrView,2) == 'M.' AND NOT slcRefTableOrView == 'M.DBF'
      slcRefTableOrView = &slcRefTableOrView
      slcRefTableOrView = ALLTRIM(UPPER(slcRefTableOrView))
   ENDIF
   IF USED('f2reftable')
      USE IN f2reftable
   ENDIF
   SELECT (sxcDD1Alias)
   sllOK = .F.
   IF SEEK(PADR(LEFT(slcRefTableOrView,112),112))
      sllOK = .T.
      IF sqlview
         * Open the database or make it the current database if already open.
         slcDatabase = ALLTRIM(dbc_name)
         slcOrigDBC = ALLTRIM(DBC())
         IF NOT EMPTY(slcDataBase) AND sgcAppPre == 'APP' AND sglTestForm
            slcDD0 = sgcPDD0
            IF EVAL(sgcUUse+"(slcDD0,'dd0lookup','AGAIN','','',sxcDDToUse)")
               IF SEEK(PADR(LEFT(slcDataBase,240),240),'dd0lookup','dbc_name')
                  slcLoc = ALLTRIM(&sgcFDS3(dd0lookup.location,'SUBSTITUTE'))
                  IF NOT EMPTY(slcLoc)
                     IF RIGHT(slcLoc,1) <> '\'
                        slcLoc = slcLoc+'\'
                     ENDIF
                     slcDatabase = slcLoc+slcDatabase
                  ENDIF
               ENDIF
               USE
            ENDIF
         ENDIF
         IF DBUSED(slcDatabase)
            SET DATABASE TO (slcDatabase)
         ELSE
            OPEN DATABASE (slcDatabase)
         ENDIF
         * Create the view cursor.
         SELECT 0
         USE (slcRefTableOrView) ALIAS f2reftable
         * Switch back to the previous database.
         IF EMPTY(slcOrigDBC)
            SET DATABASE TO
         ELSE
            SET DATABASE TO (slcOrigDBC)
         ENDIF
      ELSE
         * Open the table.
         IF NOT EVAL(sgcUUse+"(slcRefTableOrView,'f2reftable','AGAIN','','',sxcDDToUse)")
            sllOK = .F.
         ENDIF
      ENDIF
   ENDIF
   IF sllOK
      slnQuote1 = AT('"',sxcDD2ValData,29)
      slnQuote2 = AT('"',sxcDD2ValData,30)
      IF slnQuote1 > 0 AND slnQuote2 > 0
         IF SUBSTR(sxcDD2ValData,slnQuote1+1,slnQuote2-slnQuote1-1) = 'T'
            sxlAutoList = .T.
         ENDIF
      ENDIF
      slnQuote1 = AT('"',sxcDD2ValData,31)
      slnQuote2 = AT('"',sxcDD2ValData,32)
      IF slnQuote1 > 0 AND slnQuote2 > 0
         sxcEditForm = ALLTRIM(SUBSTR(sxcDD2ValData,slnQuote1+1,;
            slnQuote2-slnQuote1-1))
         IF sxcEditForm == 'T' OR sxcEditForm == 'F'
            * Handles prior version where entry was 'T' or 'F'.
            sxcEditForm = ''
         ENDIF
      ENDIF
      slnQuote1 = AT('"',sxcDD2ValData,19)
      slnQuote2 = AT('"',sxcDD2ValData,20)
      IF slnQuote1 > 0 AND slnQuote2 > 0
         slcFiltCd = SUBSTR(sxcDD2ValData,slnQuote1+1,slnQuote2-slnQuote1-1)
         IF NOT EMPTY(slcFiltCd)
            SET FILTER TO &slcFiltCd
            IF TYPE('oApp') = 'U'
               LOCATE
            ELSE
               oApp.First(SET('DATASESSION'))
            ENDIF
         ENDIF
      ENDIF
      slcForFlds = ''
      FOR slnFor = 1 TO 4
         slnQuote1 = AT('"',sxcDD2ValData,slnFor*2+1)
         slnQuote2 = AT('"',sxcDD2ValData,slnFor*2+2)
         IF slnQuote1 > 0 AND slnQuote2 > 0
            slcFK = SUBSTR(sxcDD2ValData,slnQuote1+1,slnQuote2-slnQuote1-1)
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
         slnQuote1 = AT('"',sxcDD2ValData,slnFor*2+9)
         slnQuote2 = AT('"',sxcDD2ValData,slnFor*2+10)
         IF slnQuote1 > 0 AND slnQuote2 > 0
            slcPK = SUBSTR(sxcDD2ValData,slnQuote1+1,slnQuote2-slnQuote1-1)
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
      slcFor = ''
      slnNumFlds = 0
      DO WHILE .T.
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
            IF AT('M.',UPPER(slcForFld)) = 0
               * If the lookup field does not contain a memory variable,
               * add the alias to the field name.
               slcForFld = sxcAlias+'.'+slcForFld
            ENDIF
            IF TYPE(slcForFld) $ 'CM'
               * If the values being compared are character type and of 
               * different length, trim them before the comparison.
               IF LEN(&slcPriFld) = LEN(&slcForFld)
                  IF EMPTY(slcFor)
                     slcFor = slcPriFld+' = '+slcForFld
                  ELSE
                     slcFor = slcFor+' AND '+slcPriFld+' = '+slcForFld
                  ENDIF
               ELSE
                  IF EMPTY(slcFor)
                     slcFor = 'ALLTRIM('+slcPriFld+') == ALLTRIM('+;
                        slcForFld+')'
                  ELSE
                     slcFor = slcFor+' AND ALLTRIM('+slcPriFld+;
                        ') == ALLTRIM('+slcForFld+')'
                  ENDIF
               ENDIF
            ELSE
               IF EMPTY(slcFor)
                  slcFor = slcPriFld+' = '+slcForFld
               ELSE
                  slcFor = slcFor+' AND '+slcPriFld+' = '+slcForFld
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF NOT EMPTY(slcFor)
         LOCATE FOR &slcFor
         IF NOT FOUND()
            sllRetVal = .F.
         ENDIF
      ENDIF
   ENDIF
ENDIF

***************
FUNCTION siCode
***************
* Code validation routine.
*
* In the validation code, the following rules apply:
* 
* The memvar sllRetVal must be set to .F. if the field fails validation.
* If the field passes validation it does not need to be set to .T..
*
* The memvar slcRetMess can contain any error message.  It will default
* to 'Invalid Input' if it is not set to something else.
*
* The value of the object being validated is stored in the memvar
* slCurValue.  Therefore, in the validation code the value in slCurValue
* is what should validated.

IF NOT EMPTY(sxcDD2CodeFunc)
   IF sgcAppPre == 'APP'
      IF sxlTestForm
         DO (sxcDD2CodeFunc) IN ('S'+sgcCurPre+'VC')
      ELSE
         DO (sxcDD2CodeFunc) IN SAppVC
      ENDIF
   ELSE
      DO (sxcDD2CodeFunc) IN ('S'+sgcAppPre+'VC')
   ENDIF
ENDIF
