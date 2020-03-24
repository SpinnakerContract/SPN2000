******************************************************************
*      	Lawson-Smith Corporation                                
*                                                             
*      	Copyright © 1995-96 Lawson-Smith Corporation    
*         
*      	5225 Ehrlich Road, Suite C                                
*      	Tampa, FL  33624                                        
*      	U.S.A.   
*
*	This computer program is protected by copyright law and 
*	international treaties.  No part of this program may be 
*	reproduced or transmitted in any form or by any means, 
*	electronic or mechanical, for any purpose, without the 
*	express written permission of Lawson-Smith Corporation.  
*	Unauthorized reproduction or distribution of this program, 
*	or any portion of it, may result in severe civil and 
*	criminal penalties, and will be prosecuted to the maximum 
*	extent possible under the law.  
*                                       
******************************************************************

***************
* S<Prefix>CDef - Place default values into fields in current record.
*                 Default values are determined by evaluating the Default
*                 expression stored for each field in the DD2 table.  If
*                 this program is run for a copied record, only evaluate
***************   default expression for primary key fields.

LPARAMETERS stnParentRow,stcChildFKFlds,stlCopy,stcParentPKFlds,;
   stcParentAlias,stlBlankOutPK

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

LOCAL slnCurArea,slcDD1,slcDD2,slcDDToUse,slcAlias,sllView,slcCurDBF,;
   slnNumFlds,slcPKFields,sllOK,sllFirst,slnFld,slcVFields,slnComma,;
   sllMore,slnComma1,slnComma2,slcVField,slnSpace,slnExcl,slnDot,slnFor,;
   slcDDTable,slcDDField,slcDefExpr,slRetVal,slcFld,slnCurDSID,;
   slcParentPKFlds,slcChildFKFlds,slnPComma1,slnPComma2,slnCComma1,;
   slnCComma2,slcPFld,slcCFld,slnPArea,slPValue,sllReturn

LOCAL ARRAY slaFlds(1,4)

#INCLUDE 'MAIN.H'

* Save the number of the currently selected work area.
slnCurArea = SELECT()

* Open the DD1 and DD2 table.
IF sgcAppPre == 'APP'
   IF sglTestForm
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
IF stlCopy
   IF EVAL(sgcUUse+"(slcDD1,'dd1default','AGAIN','','',slcDDToUse)")
      SET ORDER TO TAG file_name
   ELSE
      stlCopy = .F.
   ENDIF
ENDIF
IF NOT EVAL(sgcUUse+"(slcDD2,'dd2default','AGAIN','','',slcDDToUse)")
   SELECT (slnCurArea)
   RETURN
ENDIF
SET ORDER TO TAG file_name

* Select the previous work area.
SELECT (slnCurArea)

slcAlias = ALLTRIM(UPPER(ALIAS()))
IF CURSORGETPROP('SourceType') = 3
   * Current table is a free table or a table in a database.  Create an
   * array of fields in the current record.
   sllView = .F.
   slcCurDBF = SUBSTR(DBF(),RAT('\',DBF())+1)
   IF NOT SEEK(PADR(LEFT(slcCurDBF,112),112),'dd2default')
      * If the current table is not in DD2, the DD2 table for each field
      * must be specified in the control's cDDTable property.
      slcCurDBF = ''
   ENDIF
   slnNumFlds = FCOUNT()
   DIMENSION slaFlds(slnNumFlds,4)
   FOR slnFld = 1 TO slnNumFlds
      * Columns in array slaFlds:
      * 1 - ControlSource field name
      * 2 - DD2 field name
      * 3 - DD2 table name
      * 4 - Alias of field's table in view (could be more than one
      *     table in view)
      slaFlds(slnFld,1) = FIELD(slnFld)
      slaFlds(slnFld,2) = FIELD(slnFld)
      slaFlds(slnFld,3) = slcCurDBF
      slaFlds(slnFld,4) = ''
   ENDFOR
ELSE
   * Current table is a view.  Create an array of fields in the current
   * record.
   sllView = .T.
   slnNumFlds = 0
   slcVFields = ','+UPPER(CURSORGETPROP('UpdateNameList'))+','
   slnComma = 0
   sllMore = .T.
   DO WHILE sllMore
      slnComma = slnComma+1
      slnComma1 = AT(',',slcVFields,slnComma)
      slnComma2 = AT(',',slcVFields,slnComma+1)
      IF slnComma1 = 0 OR slnComma2 = 0
         EXIT
      ENDIF
      slcVField = ALLTRIM(SUBSTR(slcVFields,slnComma1+1,;
         slnComma2-slnComma1-1))
      slnSpace = AT(' ',slcVField)
      slnExcl = AT('!',slcVField)
      slnDot = AT('.',slcVField)
      IF slnSpace = 0 OR slnExcl = 0 OR slnDot = 0
         LOOP
      ENDIF
      slnNumFlds = slnNumFlds+1
      DIMENSION slaFlds(slnNumFlds,4)
      * Columns in array slaFlds:
      * 1 - ControlSource field name
      * 2 - DD2 field name
      * 3 - DD2 table name
      * 4 - Alias of field's table in view (could be more than one
      *     table in view)
      slaFlds(slnNumFlds,1) = LEFT(slcVField,slnSpace-1)
      slaFlds(slnNumFlds,2) = SUBSTR(slcVField,slnDot+1)
      slaFlds(slnNumFlds,3) = ''
      slaFlds(slnNumFlds,4) = SUBSTR(slcVField,slnExcl+1,slnDot-slnExcl-1)
   ENDDO
ENDIF
sllReturn = .T.
IF slnNumFlds > 0
   IF sllView
      * For each view field, check to see if there is a control for it on
      * the form.  If so, place the entries in the control's cDDTable and
      * cDDField properties into the fields array slaFlds.
      FOR slnFld = 1 TO slnNumFlds
         FOR slnFor = 1 TO _SCREEN.ActiveForm.ControlCount
            IF TYPE('_SCREEN.ActiveForm.Controls(slnFor).cDDTable') = 'U' OR ;
               TYPE('_SCREEN.ActiveForm.Controls(slnFor).cDDField') = 'U' OR ;
               TYPE('_SCREEN.ActiveForm.Controls(slnFor).ControlSource') = 'U'
               LOOP
            ENDIF
            IF slcAlias+'.'+slaFlds(slnFld,1) == ALLTRIM(UPPER(;
               _SCREEN.ActiveForm.Controls(slnFor).ControlSource))
               slcDDTable = ALLTRIM(UPPER(;
                   _SCREEN.ActiveForm.Controls(slnFor).cDDTable))
               IF NOT EMPTY(slcDDTable)
                  slnDot = oApp.ExtDotPos(slcDDTable)
                  IF slnDot = 0
                     slcDDTable = slcDDTable+'.DBF'
                  ENDIF
                  slaFlds(slnFld,3) = slcDDTable
               ENDIF
               slcDDField = ALLTRIM(UPPER(;
                  _SCREEN.ActiveForm.Controls(slnFor).cDDField))
               IF NOT EMPTY(slcDDField)
                  slaFlds(slnFld,2) = slcDDField
               ENDIF
               EXIT
            ENDIF
         ENDFOR
      ENDFOR
      * If the DD2 table name is not yet filled in, find another field that
      * has the DD2 table name filled in and use it.
      FOR slnFld = 1 TO slnNumFlds
         IF EMPTY(slaFlds(slnFld,3))
            FOR slnFor = 1 TO slnNumFlds
               IF slaFlds(slnFld,4) == slaFlds(slnFor,4) AND ;
                  NOT EMPTY(slaFlds(slnFor,3))
                  slaFlds(slnFld,3) = slaFlds(slnFor,3)
                  EXIT
               ENDIF
            ENDFOR
         ENDIF
      ENDFOR
   ENDIF
   * For each field, see if there is a Default expression in the DD2
   * table.  If so, evaluate the expression and place the results into
   * the field.
   sllFirst = .T.
   FOR slnFld = 1 TO slnNumFlds
      IF NOT EMPTY(slaFlds(slnFld,2)) AND NOT EMPTY(slaFlds(slnFld,3))
         IF SEEK(PADR(LEFT(slaFlds(slnFld,3),112),112)+;
            PADR(slaFlds(slnFld,2),128),'dd2default')
            IF siLockDD2()
               slcDefExpr = dd2default.default
               sllOK = .T.
               IF stlCopy
                  IF sllView OR sllFirst
                     IF SEEK(PADR(LEFT(slaFlds(slnFld,3),112),112),;
                        'dd1default') AND NOT EMPTY(dd1default.primarytag) ;
                        AND NOT EMPTY(dd1default.primaryfld)
                        slcPKFields = UPPER(STRTRAN(dd1default.primaryfld,;
                           ' '))
                        IF LEFT(slcPKFields,1) <> ','
                           slcPKFields = ','+slcPKFields
                        ENDIF
                        IF RIGHT(slcPKFields,1) <> ','
                           slcPKFields = slcPKFields+','
                        ENDIF
                         sllFirst = .F.
                     ELSE
                        sllOK = .F.
                     ENDIF
                  ENDIF
                  IF sllOK
                     IF ','+ALLTRIM(slaFlds(slnFld,2))+',' $ slcPKFields
                        * Blank-out primary key fields in copied records.
                        IF EMPTY(slcDefExpr) AND stlBlankOutPK
                           BLANK FIELDS (slaFlds(slnFld,1))
                        ENDIF
                     ELSE
                        * Only evaluate default expression for primary key
                        * fields.
                        sllOK = .F.
                     ENDIF
                  ENDIF
               ENDIF
               IF sllOK AND NOT EMPTY(slcDefExpr)
                  slRetVal = EVALUATE(slcDefExpr)
                  IF dd2default.type <> 'L' AND TYPE('slRetVal') = 'L'
                     * The calculation failed and therefore the add/copy
                     * should be cancelled.
                     sllReturn = .F.
                     EXIT
                  ELSE
                     slcFld = slaFlds(slnFld,1)
                     REPLACE &slcFld WITH slRetVal
                  ENDIF
               ENDIF
               UNLOCK IN dd2default
            ELSE
               * The DD2 record was not locked and therefore the add/copy
               * should be cancelled.
               sllReturn = .F.
               EXIT
            ENDIF
         ENDIF
      ENDIF
   ENDFOR
ENDIF

IF USED('dd1default')
   USE IN dd1default
ENDIF

IF USED('dd2default')
   USE IN dd2default
ENDIF

IF NOT sllReturn
   RETURN .F.
ENDIF

* If the current table is the child in a parent/child relationship,
* fill in the foreign key fields in the new child record with values
* from the primary key fields in the parent record.
IF NOT EMPTY(stcChildFKFlds) AND NOT EMPTY(stcParentPKFlds) AND ;
   NOT EMPTY(stcParentAlias)
   IF USED(stcParentAlias) AND NOT EOF(stcParentAlias)
      slcParentPKFlds = ALLTRIM(stcParentPKFlds)
      IF LEFT(slcParentPKFlds,1) <> ','
         slcParentPKFlds = ','+slcParentPKFlds
      ENDIF
      IF RIGHT(slcParentPKFlds,1) <> ','
         slcParentPKFlds = slcParentPKFlds+','
      ENDIF
      slcChildFKFlds = ALLTRIM(stcChildFKFlds)
      IF LEFT(slcChildFKFlds,1) <> ','
         slcChildFKFlds = ','+slcChildFKFlds
      ENDIF
      IF RIGHT(slcChildFKFlds,1) <> ','
         slcChildFKFlds = slcChildFKFlds+','
      ENDIF
      sllMore = .T.
      DO WHILE sllMore
         slnPComma1 = AT(',',slcParentPKFlds,1)
         slnPComma2 = AT(',',slcParentPKFlds,2)
         slnCComma1 = AT(',',slcChildFKFlds,1)
         slnCComma2 = AT(',',slcChildFKFlds,2)
         IF slnPComma1 = 0 OR slnPComma2 = 0 OR slnCComma1 = 0 OR ;
            slnCComma2 = 0
            EXIT
         ENDIF
         slcPFld = SUBSTR(slcParentPKFlds,slnPComma1+1,;
            slnPComma2-slnPComma1-1)
         slcCFld = SUBSTR(slcChildFKFlds,slnCComma1+1,;
            slnCComma2-slnCComma1-1)
         IF EMPTY(slcPFld) OR EMPTY(slcCFld)
            EXIT
         ENDIF
         slcParentPKFlds = SUBSTR(slcParentPKFlds,slnPComma2)
         slcChildFKFlds = SUBSTR(slcChildFKFlds,slnCComma2)
         slnPArea = SELECT()
         SELECT (stcParentAlias)
         slPValue = EVALUATE(slcPFld)
         SELECT (slnPArea)
         * Move the contents of the parent field into the child field.
         REPLACE &slcCFld WITH slPValue
      ENDDO
   ENDIF
ELSE
   IF NOT EMPTY(stcChildFKFlds) AND stnParentRow > 0 AND ;
      NOT EMPTY(oApp.aActiveForms(stnParentRow,9))
      slnCurDSID = VAL(SYS(2001,'DATASESSION'))
      SET DATASESSION TO (oApp.aActiveForms(stnParentRow,6))
      IF USED(oApp.aActiveForms(stnParentRow,4)) AND ;
         NOT EOF(oApp.aActiveForms(stnParentRow,4))
         SET DATASESSION TO (slnCurDSID)
         slcParentPKFlds = ALLTRIM(oApp.aActiveForms(stnParentRow,9))
         IF LEFT(slcParentPKFlds,1) <> ','
            slcParentPKFlds = ','+slcParentPKFlds
         ENDIF
         IF RIGHT(slcParentPKFlds,1) <> ','
            slcParentPKFlds = slcParentPKFlds+','
         ENDIF
         slcChildFKFlds = ALLTRIM(stcChildFKFlds)
         IF LEFT(slcChildFKFlds,1) <> ','
            slcChildFKFlds = ','+slcChildFKFlds
         ENDIF
         IF RIGHT(slcChildFKFlds,1) <> ','
            slcChildFKFlds = slcChildFKFlds+','
         ENDIF
         sllMore = .T.
         DO WHILE sllMore
            slnPComma1 = AT(',',slcParentPKFlds,1)
            slnPComma2 = AT(',',slcParentPKFlds,2)
            slnCComma1 = AT(',',slcChildFKFlds,1)
            slnCComma2 = AT(',',slcChildFKFlds,2)
            IF slnPComma1 = 0 OR slnPComma2 = 0 OR slnCComma1 = 0 OR ;
               slnCComma2 = 0
               EXIT
            ENDIF
            slcPFld = SUBSTR(slcParentPKFlds,slnPComma1+1,;
               slnPComma2-slnPComma1-1)
            slcCFld = SUBSTR(slcChildFKFlds,slnCComma1+1,;
               slnCComma2-slnCComma1-1)
            IF EMPTY(slcPFld) OR EMPTY(slcCFld)
               EXIT
            ENDIF
            slcParentPKFlds = SUBSTR(slcParentPKFlds,slnPComma2)
            slcChildFKFlds = SUBSTR(slcChildFKFlds,slnCComma2)
            SET DATASESSION TO (oApp.aActiveForms(stnParentRow,6))
            slnPArea = SELECT()
            SELECT (oApp.aActiveForms(stnParentRow,4))
            slPValue = EVALUATE(slcPFld)
            SELECT (slnPArea)
            SET DATASESSION TO (slnCurDSID)
            * Move the contents of the parent field into the child field.
            REPLACE &slcCFld WITH slPValue
         ENDDO
      ELSE
         SET DATASESSION TO (slnCurDSID)
      ENDIF
   ENDIF
ENDIF

******************
FUNCTION siLockDD2
******************
* Attempt to lock the current DD2 record.  FoxPro handles the retry message
* in certain situations.

LOCAL sllOurMess,sllLocked

sllOurMess = .T.
IF EMPTY(ON('ERROR'))
   IF SET('REPROCESS') <= 0
      sllOurMess = .F.
   ENDIF
ENDIF

sllLocked = .F.
DO WHILE NOT sllLocked
   IF RLOCK('dd2default')
      RETURN .T.
   ENDIF
   IF sllOurMess
      IF MESSAGEBOX('In attempting to calculate the default value '+;
         'for the '+IIF(EMPTY(dd2default.label),LOWER(ALLTRIM(;
         dd2default.field_name)),ALLTRIM(dd2default.label))+;
         ' field, the data dictionary record needed for this calculation '+;
         'is already being used by someone else.  The record may be '+;
         'available if you try to use it again.  Would you like to '+;
         'attempt to use the record again?',MB_ICONQUESTION+MB_YESNO,;
         sgcAppTitle) = IDNO
         RETURN .F.
      ENDIF
   ELSE
      RETURN .F.
   ENDIF
ENDDO
