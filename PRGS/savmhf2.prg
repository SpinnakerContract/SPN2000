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
* S<Prefix>HF2 - Bring up the Help List window for the active control.
**************

#INCLUDE 'MAIN.H'

IF TYPE('_SCREEN.ActiveForm') <> 'O' OR ;
   TYPE('_SCREEN.ActiveForm.ActiveControl') <> 'O'
   RETURN
ENDIF

PRIVATE sxnF2Mess,sxcF2Mess,sxcValData,sxlReferential,sxcDType,sxnDWidth,;
   sxnForWA,sxaF2Set,sxcDLabel,sxnF2Width,sxcDDTable,sxcDDField,;
   sxnCtrlLeft,sxnCtrlTop,sxnCtrlHeight,sxnFormLeft,sxnFormTop,sxnPriWA,;
   sxnSelected,sxlFromVal,sxcDDToUse,sxcForFld1,sxcForFld2,sxcForFld3,;
   sxcForFld4,sxcForFld5,sxcForFld6,sxcForFld7,sxcForFld8,sxcForFld9,;
   sxcPriFld1,sxcPriFld2,sxcPriFld3,sxcPriFld4,sxcPriFld5,sxcPriFld6,;
   sxcPriFld7,sxcPriFld8,sxcPriFld9,sxcListFields,sxcListCaptions,;
   sxcListTags,sxnListRows,sxnListWidth,sxlRefFound,sxcPriEdit,sxcOnF3,;
   sxlAdd,sxlEdit,sxlDMV,sxcForObj1,sxcForObj2,sxcForObj3,sxcForObj4,;
   sxcForObj5,sxcForObj6,sxcForObj7,sxcForObj8,sxcForObj9,sxcCurObj,;
   sxcAlias,sxnSaveDSID,sxnCurArea,sxoActForm,sxoActCtrl

LOCAL sllContinue,sllMoreSA,slcSATable,slcSAField,slnQuote1,slnQuote2,;
   slnDot,slcDD2,slcOnAltF3,slnFor,slcFor,slcForFld,slcPriFld,slValue,;
   sllCtrlFound,slcCurCtrl,slnActiveCol,sllSkipAct,slcOnRM,sloObject,;
   slcCtrlSrce,slcLinkMaster,sllOK,slnAP,slnPFor,slnCFor,sllMoreObj,;
   sloForm,slcDD1

CLEAR TYPEAHEAD
*slcOnRM = ON('KEY','RIGHTMOUSE')
*ON KEY LABEL RIGHTMOUSE *
ON KEY LABEL F2 *
sxcOnF3 = ON('KEY','F3')
ON KEY LABEL F3 *
slcOnAltF3 = ON('KEY','ALT+F3')
ON KEY LABEL Alt+F3 *

IF EMPTY(WONTOP())
*   ON KEY LABEL RIGHTMOUSE &slcOnRM
   ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
   ON KEY LABEL F3 &sxcOnF3
   ON KEY LABEL Alt+F3 &slcOnAltF3
   ?? CHR(7)
   WAIT F2NOTAVAILABLE WINDOW NOWAIT
   RETURN
ENDIF

sxoActForm = _SCREEN.ActiveForm
sxoActCtrl = _SCREEN.ActiveForm.ActiveControl
sllCtrlFound = .F.
DO CASE
   CASE UPPER(sxoActCtrl.BaseClass) == 'GRID'
      slnActiveCol = sxoActCtrl.ActiveColumn
      * The column could have been moved from its original position.
      * Find its original position.
      sllOK = .F.
      FOR slnFor = 1 TO sxoActCtrl.ColumnCount
         IF sxoActCtrl.Columns(slnFor).ColumnOrder = slnActiveCol
            slnActiveCol = slnFor
            sllOK = .T.
            EXIT
         ENDIF
      ENDFOR
      IF sllOK
         * Find the current control in the active column.
         slcCurCtrl = ALLTRIM(UPPER(sxoActCtrl.Columns(slnActiveCol).;
            CurrentControl))
         FOR slnFor = 1 TO sxoActCtrl.Columns(slnActiveCol).ControlCount
            IF slcCurCtrl == ALLTRIM(UPPER(sxoActCtrl.;
               Columns(slnActiveCol).Controls(slnFor).Name))
               sxcCurObj = sxoActCtrl.Columns(slnActiveCol).Controls(slnFor)
               IF TYPE('sxcCurObj.cDDTable') <> 'U' AND ;
                  TYPE('sxcCurObj.cDDField') <> 'U'
                  sllCtrlFound = .T.
               ENDIF
               EXIT
            ENDIF
         ENDFOR
      ENDIF
   CASE UPPER(sxoActCtrl.Parent.BaseClass) == 'PAGE' OR ;
      UPPER(sxoActCtrl.Parent.BaseClass) == 'CONTAINER'
      slcCurCtrl = ALLTRIM(UPPER(sxoActCtrl.Name))
      FOR slnFor = 1 TO sxoActCtrl.Parent.ControlCount
         IF slcCurCtrl == ALLTRIM(UPPER(sxoActCtrl.Parent.Controls(slnFor).;
            Name))
            sxcCurObj = sxoActCtrl.Parent.Controls(slnFor)
            IF TYPE('sxcCurObj.cDDTable') <> 'U' AND ;
               TYPE('sxcCurObj.cDDField') <> 'U'
               sllCtrlFound = .T.
            ENDIF
            EXIT
         ENDIF
      ENDFOR
   OTHERWISE
      slcCurCtrl = ALLTRIM(UPPER(sxoActCtrl.Name))
      FOR slnFor = 1 TO sxoActForm.ControlCount
         IF slcCurCtrl == ALLTRIM(UPPER(sxoActForm.;
            Controls(slnFor).Name))
            sxcCurObj = sxoActForm.Controls(slnFor)
            IF TYPE('sxcCurObj.cDDTable') <> 'U' AND ;
               TYPE('sxcCurObj.cDDField') <> 'U'
               sllCtrlFound = .T.
            ENDIF
            EXIT
         ENDIF
      ENDFOR
ENDCASE
IF NOT sllCtrlFound OR TYPE('sxcCurObj.ControlSource') = 'U'
*   ON KEY LABEL RIGHTMOUSE &slcOnRM
   ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
   ON KEY LABEL F3 &sxcOnF3
   ON KEY LABEL Alt+F3 &slcOnAltF3
   ?? CHR(7)
   WAIT F2NOTAVAILABLE WINDOW NOWAIT
   RETURN
ENDIF
IF (TYPE('sxcCurObj.ReadOnly') = 'L' AND sxcCurObj.ReadOnly AND ;
   (TYPE('sxcCurObj.lLookupDescription') <> 'L' OR ;
   NOT sxcCurObj.lLookupDescription)) OR ;
   (TYPE('sxcCurObj.Enabled') = 'L' AND NOT sxcCurObj.Enabled) OR ;
   (TYPE('sxcCurObj.lOnlyEditWhenNew') = 'L' AND ;
   sxcCurObj.lOnlyEditWhenNew AND NOT sxoActForm.IsNew()) OR ;
   (ALLTRIM(UPPER(sxcCurObj.Parent.BaseClass)) == 'COLUMN' AND ;
   ((sxcCurObj.Parent.ReadOnly  AND ;
   (TYPE('sxcCurObj.lLookupDescription') <> 'L' OR ;
   NOT sxcCurObj.lLookupDescription)) OR NOT sxcCurObj.Parent.Enabled))
   * If the active control cannot be edited, don't allow the F2
   * dialog to be brought up.
   ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
   ON KEY LABEL F3 &sxcOnF3
   ON KEY LABEL Alt+F3 &slcOnAltF3
   ?? CHR(7)
   WAIT F2NOTAVAILABLE WINDOW NOWAIT
   RETURN
ENDIF
IF TYPE('sxcCurObj.lPicklist') = 'L' AND NOT sxcCurObj.lPicklist
   * If the active control's lPicklist property is set to .F.,
   * don't allow the F2 dialog to be brought up.
   ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
   ON KEY LABEL F3 &sxcOnF3
   ON KEY LABEL Alt+F3 &slcOnAltF3
   ?? CHR(7)
   WAIT F2NOTAVAILABLE WINDOW NOWAIT
   RETURN
ENDIF
sxcDDTable = sxcCurObj.cDDTable
sxcDDField = sxcCurObj.cDDField
sxnCtrlLeft = sxcCurObj.Left
sxnCtrlTop = sxcCurObj.Top
sloObject = sxcCurObj
sllMoreObj = .T.
DO WHILE sllMoreObj
   sloObject = sloObject.Parent
   DO CASE
      CASE ALLTRIM(UPPER(sloObject.BaseClass)) == 'CONTAINER'
         sxnCtrlTop = sxnCtrlTop+sloObject.Top
         sxnCtrlLeft = sxnCtrlLeft+sloObject.Left
      CASE ALLTRIM(UPPER(sloObject.BaseClass)) == 'PAGEFRAME'
         sxnCtrlTop = sxnCtrlTop+sloObject.Top+28
         sxnCtrlLeft = sxnCtrlLeft+sloObject.Left+1
      CASE ALLTRIM(UPPER(sloObject.BaseClass)) == 'FORM'
         sllMoreObj = .F.
   ENDCASE
ENDDO
sxnCtrlHeight = sxcCurObj.Height
sxnFormLeft = sxoActForm.Left
sxnFormTop = sxoActForm.Top

sxnSaveDSID = SET('DATASESSION')
SET DATASESSION TO (sxoActForm.DataSessionID)
sxnCurArea = SELECT()

* Handle fields not in the active table
IF TYPE('sxcCurObj.cAlias') <> 'U' AND NOT EMPTY(sxcCurObj.cAlias)
   sxcAlias = ALLTRIM(sxcCurObj.cAlias)
ELSE
   slcCtrlSrce = ALLTRIM(sxcCurObj.ControlSource)
   IF EMPTY(slcCtrlSrce) AND TYPE('sxcCurObj.cSaveControlSource') <> 'U'
      slcCtrlSrce = ALLTRIM(sxcCurObj.cSaveControlSource)
   ENDIF
   IF EMPTY(slcCtrlSrce)
      sxcAlias = ''
   ELSE
      slnDot = RAT('.',slcCtrlSrce)
      IF slnDot = 0
         sxcAlias = ''
      ELSE
         IF slnDot = 1 OR slnDot = LEN(slcCtrlSrce)
            sxcAlias = ''
         ELSE
            sxcAlias = LEFT(slcCtrlSrce,slnDot-1)
         ENDIF
      ENDIF
   ENDIF
ENDIF

IF EMPTY(sxcDDTable)
*   ON KEY LABEL RIGHTMOUSE &slcOnRM
   ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
   ON KEY LABEL F3 &sxcOnF3
   ON KEY LABEL Alt+F3 &slcOnAltF3
   ?? CHR(7)
   WAIT F2NOTAVAILABLE WINDOW NOWAIT
   = siPrevDS()
   RETURN
ENDIF
sxcDDTable = ALLTRIM(UPPER(sxcDDTable))
sxlDMV = .F.
IF LEN(sxcDDTable) > 10
   IF SUBSTR(sxcDDTable,5,3) = 'DMV'
      sxlDMV = .T.
   ENDIF
ENDIF
IF EMPTY(sxcDDField)
*   ON KEY LABEL RIGHTMOUSE &slcOnRM
   ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
   ON KEY LABEL F3 &sxcOnF3
   ON KEY LABEL Alt+F3 &slcOnAltF3
   ?? CHR(7)
   WAIT F2NOTAVAILABLE WINDOW NOWAIT
   = siPrevDS()
   RETURN
ENDIF
sxcDDField = ALLTRIM(UPPER(sxcDDField))

* Determine the currently selected work area (contains foreign key field(s)).
sxnForWA = SELECT()

* Open the appropriate DD2 table.
IF sgcAppPre == 'APP'
   IF UPPER(WONTOP()) = 'FRMDATABUILDER' OR sglTestForm
      * Running the Data Builder or testing a form in ProMatrix
      slcDD2 = sgcPDD2
      sxcDDToUse = 'PDD'
   ELSE
      * Running a ProMatrix form
      slcDD2 = 'SAPPDD2.DBF'
      sxcDDToUse = ''
   ENDIF
ELSE
   * Running an application form
   slcDD2 = sgcPDD2
   sxcDDToUse = ''
ENDIF

IF NOT EVAL(sgcUUse+"(slcDD2,'dd2lookup','AGAIN','','',sxcDDToUse)")
   SELECT (sxnForWA)
*   ON KEY LABEL RIGHTMOUSE &slcOnRM
   ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
   ON KEY LABEL F3 &sxcOnF3
   ON KEY LABEL Alt+F3 &slcOnAltF3
   ?? CHR(7)
   WAIT F2NOTAVAILABLE WINDOW NOWAIT
   = siPrevDS()
   RETURN
ENDIF
* Find the DD2 record for the current control.
oApp.SetOrder(ALIAS(),SET('DATASESSION'),'file_name')
IF NOT SEEK(PADR(LEFT(sxcDDTable,112),112)+PADR(sxcDDField,128))
   USE
   SELECT (sxnForWA)
*   ON KEY LABEL RIGHTMOUSE &slcOnRM
   ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
   ON KEY LABEL F3 &sxcOnF3
   ON KEY LABEL Alt+F3 &slcOnAltF3
   ?? CHR(7)
   WAIT F2NOTAVAILABLE WINDOW NOWAIT
   = siPrevDS()
   RETURN
ENDIF

DIMENSION sxaF2Set(1)
STORE .F. TO sxlReferential,sxlAdd,sxlEdit,sxlRefFound
STORE '' TO sxcForFld1,sxcForFld2,sxcForFld3,sxcForFld4,sxcForFld5,;
   sxcForFld6,sxcForFld7,sxcForFld8,sxcForFld9,sxcPriFld1,sxcPriFld2,;
   sxcPriFld3,sxcPriFld4,sxcPriFld5,sxcPriFld6,sxcPriFld7,sxcPriFld8,;
   sxcPriFld9,sxnDescLen,sxcF2Mess,sxcPriEdit,sxcForObj1,;
   sxcForObj2,sxcForObj3,sxcForObj4,sxcForObj5,sxcForObj6,sxcForObj7,;
   sxcForObj8,sxcForObj9,sxcListFields,sxcListCaptions,sxcListTags
STORE 0 TO sxnPriLen1,sxnPriLen2,sxnPriLen3,sxnPriLen4,sxnPriLen5,;
   sxnPriLen6,sxnPriLen7,sxnPriLen8,sxnPriLen9,sxnF2Mess,sxnF2Width,;
   sxnSelected,sxnPriWA,sxnListRows,sxnListWidth

IF TYPE('sxlF2Cancel') = 'L'		&& Initialized in S<Prefix>VFld
   sxlFromVal = .T.
ELSE
   sxlFromVal = .F.
ENDIF
sxlF2Cancel = .T.

sxcDType = dd2lookup.type
sxnDWidth = dd2lookup.width
IF EMPTY(dd2lookup.label)
   sxcDLabel = ALLTRIM(dd2lookup.field_name)
ELSE
   sxcDLabel = ALLTRIM(dd2lookup.label)
ENDIF
sxcValData = ALLTRIM(dd2lookup.val_data)

IF NOT EMPTY(sxcValData)

   sllContinue = .T.
   IF dd2lookup.val_type = 4
      * If validation type is "Same As", find that record.
      sllMoreSA = .T.
      DO WHILE sllMoreSA
         slcSATable = ''
         slcSAField = ''
         sxcValData = ALLTRIM(dd2lookup.val_data)
         slnQuote1 = AT('"',sxcValData,1)
         slnQuote2 = AT('"',sxcValData,2)
         IF slnQuote1 > 0 AND slnQuote2 > 0
            slcSATable = ALLTRIM(UPPER(SUBSTR(sxcValData,slnQuote1+1,;
               slnQuote2-slnQuote1-1)))
            IF NOT EMPTY(slcSATable)
               slnDot = oApp.ExtDotPos(slcSATable)
               IF slnDot = 0
                  slcSATable = slcSATable+'.DBF'
               ENDIF
            ENDIF
         ENDIF
         slnQuote1 = AT('"',sxcValData,3)
         slnQuote2 = AT('"',sxcValData,4)
         IF slnQuote1 > 0 AND slnQuote2 > 0
            slcSAField = ALLTRIM(UPPER(SUBSTR(sxcValData,slnQuote1+1,;
               slnQuote2-slnQuote1-1)))
         ENDIF
         SEEK PADR(LEFT(slcSATable,112),112)+PADR(slcSAField,128)
         IF FOUND()
            IF dd2lookup.val_type <> 4    	&& Doesn't point to another rec
               sllMoreSA = .F.
               sxcValData = ALLTRIM(dd2lookup.val_data)
            ENDIF
         ELSE
            sllMoreSA = .F.
            sllContinue = .F.
         ENDIF
      ENDDO
   ENDIF
   IF sllContinue
      DO CASE
         CASE dd2lookup.val_type = 1			&& Range
            = siF2Range()
         CASE dd2lookup.val_type = 2			&& Set
            = siF2Set()
         CASE dd2lookup.val_type = 3			&& Referential
            = siF2Ref()
      ENDCASE
   ENDIF
ENDIF

SELECT dd2lookup
USE
SELECT (sxnForWA)

sllSkipAct = .F.
DO CASE
   CASE sxnF2Mess = 0				&& Generic message
      ?? CHR(7)
      WAIT F2NOTAVAILABLE WINDOW NOWAIT
   CASE sxnF2Mess = 1   			&& Specific message
      ?? CHR(7)
      WAIT sxcF2Mess WINDOW NOWAIT
   CASE sxnF2Mess = 2				&& OK to make selection
      * Don't run the Activate method of the active form when coming back
      * from the F2 selection dialog.
      IF TYPE('sxoActForm.lSkipActivate') <> 'U'
         sxoActForm.lSkipActivate = .T.
         sllSkipAct = .T.
      ENDIF
      * Bring up the F2 selection dialog.
      CLEAR TYPEAHEAD
      IF sxlReferential
         SELECT (sxnPriWA)
         DO FORM (sgcHF2R) WITH sxlRefFound
      ELSE
         DO FORM (sgcHF2S)
      ENDIF
      SELECT (sxnForWA)
      CLEAR TYPEAHEAD

      * If a selection was made, update the current record.
      IF sxnSelected > 0 AND sxcDType <> 'L'
         IF sxlReferential				&& Referential validation
            sxlF2Cancel = .F.			&& Initialized in S<Prefix>VFld
            SELECT (sxnPriWA)
            IF RECNO() <> sxnSelected
               GOTO sxnSelected
            ENDIF
            FOR slnFor = 1 TO 9
               slcFor = STR(slnFor,1)
               slcForFld = sxcForFld&slcFor
               slcPriFld = sxcPriFld&slcFor
               IF NOT EMPTY(slcPriFld) AND NOT EMPTY(slcForFld)
                  SELECT (sxnPriWA)
                  slValue = &slcPriFld
                  SELECT (sxnForWA)
                  IF sxlDMV
                     sxcForObj&slcFor..Value = slValue
                  ELSE
                     IF NOT EMPTY(sxcAlias)
                        * Handle fields not in the active table
                        SELECT (sxcAlias)
                     ENDIF
                     REPLACE &slcForFld WITH slValue
                     SELECT (sxnForWA)
                  ENDIF
               ENDIF
            ENDFOR
         ELSE
            sxlF2Cancel = .F.			&& Initialized in S<Prefix>VFld
            slValue = LEFT(sxaF2Set(sxnSelected),sxnDWidth)
            DO CASE
               CASE sxcDType $ 'INFY'
                  slValue = VAL(slValue)
               CASE sxcDType = 'D'
                  slValue = CTOD(slValue)
               CASE sxcDType = 'T'
                  slValue = CTOT(slValue)
            ENDCASE
            IF sxlDMV
               sxcCurObj.Value = slValue
            ELSE
               IF EMPTY(sxcCurObj.ControlSource)
                  IF TYPE('sxcCurObj.cSaveControlSource') <> 'U' AND NOT ;
                     EMPTY(sxcCurObj.cSaveControlSource)
                     REPLACE (sxcCurObj.cSaveControlSource) WITH slValue
                  ENDIF
               ELSE
                  REPLACE (sxcCurObj.ControlSource) WITH slValue
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      SELECT (sxnForWA)

      IF sxnSelected > 0
         * Control value(s) have been changed so refresh the form.
         IF TYPE('sxoActForm.oOnFormContainer') = 'O'
            * If the form is using on-form controls instead of a toolbar,
            * refreshing the form at this point commits the added record.
            * Instead, we will refresh each control individually.
            FOR slnFor = 1 TO sxoActForm.ControlCount
               DO CASE
                  CASE TYPE('sxoActForm.Controls(slnFor).TabIndex') = 'U'
                     LOOP
                  CASE INLIST(UPPER(sxoActForm.Controls(slnFor).BaseClass),;
                     'TEXTBOX','COMMANDBUTTON','CHECKBOX','COMBOBOX',;
                     'EDITBOX','GRID','LISTBOX','OPTIONBUTTON','SPINNER') ;
                     AND sxoActForm.Controls(slnFor).Enabled
                     sxoActForm.Controls(slnFor).Refresh()
                  CASE UPPER(sxoActForm.Controls(slnFor).BaseClass) == ;
                     'PAGEFRAME'
                     slnAP = sxoActForm.Controls(slnFor).ActivePage
                     FOR slnPFor = 1 TO sxoActForm.Controls(slnFor).;
                        Pages(slnAP).ControlCount
                        IF TYPE('sxoActForm.Controls(slnFor).;
                           Pages(slnAP).Controls(slnPFor).TabIndex') = 'U'
                           LOOP
                        ENDIF
                        IF INLIST(UPPER(sxoActForm.Controls(slnFor).;
                           Pages(slnAP).Controls(slnPFor).BaseClass),;
                           'TEXTBOX','COMMANDBUTTON','CHECKBOX','COMBOBOX',;
                           'GRID','EDITBOX','LISTBOX','OPTIONBUTTON',;
                           'SPINNER') AND sxoActForm.Controls(slnFor).;
                           Pages(slnAP).Controls(slnPFor).Enabled
                           sxoActForm.Controls(slnFor).Pages(slnAP).;
                              Controls(slnPFor).Refresh()
                        ENDIF
                     ENDFOR
                  CASE UPPER(sxoActForm.Controls(slnFor).BaseClass) == ;
                     'CONTAINER'
                     FOR slnCFor = 1 TO sxoActForm.Controls(slnFor).;
                        ControlCount
                        IF TYPE('sxoActForm.Controls(slnFor).;
                           Controls(slnCFor).TabIndex') = 'U'
                           LOOP
                        ENDIF
                        IF INLIST(UPPER(sxoActForm.Controls(slnFor).;
                           Controls(slnCFor).BaseClass),'TEXTBOX',;
                           'COMMANDBUTTON','CHECKBOX','COMBOBOX','GRID',;
                           'EDITBOX','LISTBOX','OPTIONBUTTON','SPINNER');
                           AND sxoActForm.Controls(slnFor).;
                           Controls(slnCFor).Enabled
                           sxoActForm.Controls(slnFor).Controls(slnCFor).;
                              Refresh()
                        ENDIF
                     ENDFOR
               ENDCASE
            ENDFOR
         ELSE
            sxoActForm.RefreshForm()
         ENDIF

         * Put the focus on the next control.
         IF NOT sxlF2Cancel
            IF sxlFromVal
               _CUROBJ = _CUROBJ+1
            ELSE
               IF TYPE('sxcCurObj.Parent.Parent.BaseClass') = 'U' OR NOT ;
                  ALLTRIM(UPPER(sxcCurObj.Parent.Parent.BaseClass)) == 'GRID'
                  sxcCurObj.SetFocus()
               ENDIF
               KEYBOARD '{TAB}'
            ENDIF
         ENDIF
      ELSE
         IF sxlReferential
            USE IN (sxnPriWA)
         ENDIF
         * Place the focus on the control that had focus when F2 was
         * selected.
         IF NOT sxlFromVal
            sxcCurObj.SetFocus()
            IF TYPE('sxcCurObj.lLostFocusForLookup') <> 'U' AND ;
               NOT ((sxlAdd OR sxlEdit) AND NOT EMPTY(sxcPriEdit))
               sxcCurObj.lLostFocusForLookup = .T.
            ENDIF
         ENDIF
      ENDIF
ENDCASE

*ON KEY LABEL RIGHTMOUSE &slcOnRM
ON KEY LABEL F2 do &sgcFCT with 'HELP_F2'
ON KEY LABEL F3 &sxcOnF3
ON KEY LABEL Alt+F3 &slcOnAltF3

= siPrevDS()

* On a one-to-many form, bringing up the F2 dialog and returning more than
* once can cause the relation between the parent and child tables to be
* broken.  Selecting the parent table avoids this problem.
IF ALLTRIM(UPPER(sxoActForm.Class)) = 'FORM_TOOLBAR_ONETOMANY' AND ;
   TYPE('sxcCurObj.Parent.Parent.BaseClass') <> 'U' AND ;
   ALLTRIM(UPPER(sxcCurObj.Parent.Parent.BaseClass)) == 'GRID'
   slcLinkMaster = sxcCurObj.Parent.Parent.LinkMaster
   IF NOT EMPTY(slcLinkMaster) AND USED(slcLinkMaster)
      SELECT (slcLinkMaster)
   ENDIF
ENDIF

* Run the lookup table editing form.
IF (sxlAdd OR sxlEdit) AND NOT EMPTY(sxcPriEdit)
   IF sllSkipAct
      sxoActForm.lSkipActivate = .F.
   ENDIF

   * When returning to the calling form, prevent the message
   * from displaying that notifies the user that before leaving
   * the form, changes were made but not saved.
   sxoActForm.lMessageOnReactivation_NotThisTime = .T.
   IF sxlAdd
      sxoActForm.oFKUpdate = sxcCurObj		&& Used in FKUpdate method of form
   ENDIF

   * Run the form without showing/activating it.
   DO FORM (sxcPriEdit) NAME sloForm NOSHOW

   IF sxlAdd
      * Set the form's properties that will cause it to behave as
      * described above.
      sloForm.lStartWithAdd = .T.
      sloForm.lStartWithAddMessage = .F.
      sloForm.lFKUpdate = .T.
      IF TYPE('sxcCurObj.lFKUpdateOneAdd') <> 'U'
         sloForm.lFKUpdateOneAdd = sxcCurObj.lFKUpdateOneAdd
      ENDIF
   ENDIF
   
   * Show/activate the form and make it modal.
   sloForm.Show(1)
ENDIF

******************
FUNCTION siF2Range
******************
* If the current control is validated using Range validation, create the
* WAIT WINDOW message.

LOCAL slnQuote1,slnQuote2,sllRangeFrom,slcRangeFrom,sllRangeTo,slcRangeTo

slnQuote1 = AT('"',sxcValData,1)
slnQuote2 = AT('"',sxcValData,2)
sllRangeFrom = .F.
IF slnQuote1 > 0 AND slnQuote2 > 0
   sllRangeFrom = .T.
   slcRangeFrom = ALLTRIM(SUBSTR(sxcValData,slnQuote1+1,;
      slnQuote2-slnQuote1-1))
   IF LEN(slcRangeFrom) > dd2lookup.width
      slcRangeFrom = LEFT(slcRangeFrom,dd2lookup.width)
   ENDIF
   IF EMPTY(slcRangeFrom)
      slcRangeFrom = '" "'
   ENDIF
ENDIF
slnQuote1 = AT('"',sxcValData,3)
slnQuote2 = AT('"',sxcValData,4)
sllRangeTo = .F.
IF slnQuote1 > 0 AND slnQuote2 > 0
   sllRangeTo = .T.
   slcRangeTo = ALLTRIM(SUBSTR(sxcValData,slnQuote1+1,slnQuote2-slnQuote1-1))
   IF LEN(slcRangeTo) > dd2lookup.width
      slcRangeTo = LEFT(slcRangeTo,dd2lookup.width)
   ENDIF
   IF EMPTY(slcRangeTo)
      slcRangeTo = '" "'
   ENDIF
ENDIF
IF sllRangeFrom AND sllRangeTo
   sxnF2Mess = 1
   sxcF2Mess = 'Range: '+slcRangeFrom+' to '+slcRangeTo
ENDIF

****************
FUNCTION siF2Set
****************
* If the current control is validated using Set validation, create the
* array that holds the set of valid values.

LOCAL slnF2Elems,sllMoreItem,slnQuote1,slnQuote2,slcSetValue,slValue

slnF2Elems = 0
sllMoreItem = .T.
DO WHILE sllMoreItem
   slnF2Elems = slnF2Elems+1
   slnQuote1 = AT('"',sxcValData,slnF2Elems*2-1)
   slnQuote2 = AT('"',sxcValData,slnF2Elems*2)
   IF slnQuote1 > 0 AND slnQuote2 > 0
      slcSetValue = ALLTRIM(SUBSTR(sxcValData,slnQuote1+1,;
         slnQuote2-slnQuote1-1))
      slcSetValue = LEFT(slcSetValue,sxnDWidth)+'   '+;
         SUBSTR(slcSetValue,sxnDWidth+1)
      IF LEN(slcSetValue) > sxnF2Width
         sxnF2Width = LEN(slcSetValue)
      ENDIF
      DIMENSION sxaF2Set(slnF2Elems)
      sxaF2Set(slnF2Elems) = slcSetValue
      IF sxnSelected = 0
         slValue = LEFT(slcSetValue,sxnDWidth)
         DO CASE
            CASE sxcDType $ 'INF'
               slValue = VAL(slValue)
            CASE sxcDType = 'D'
               slValue = CTOD(slValue)
            CASE sxcDType = 'T'
               slValue = CTOT(slValue)
         ENDCASE
         IF TYPE('sxcCurObj.Value') = TYPE('slValue') AND ;
            sxcCurObj.Value = slValue
            sxnSelected = slnF2Elems
         ENDIF
      ENDIF
   ELSE
      slnF2Elems = slnF2Elems-1
      sllMoreItem = .F.
   ENDIF
ENDDO
IF slnF2Elems > 0
   sxnF2Mess = 2
ENDIF

****************
FUNCTION siF2Ref
****************
* If the current control is validated using Referential Integrity, prepare
* to bring up the selection dialog.

LOCAL slnQuote1,slnQuote2,slcRefTableOrView,slcForFlds,slcFK,slcPriFlds,;
   sllFiltErr,slcFCode,slcFileT,slcOnError,slnFor,slcPriFld,slcForFld,;
   slcFor,slcForExpr,slcForExp2,sllForExp2,slValue1,slcPK,slnNumflds,;
   sllMore,slValue2,slValue3,slValue4,slValue5,slValue6,slValue7,slValue8,;
   slValue9,slnFor1,slVal,slnActiveCol,slcListRows,slcListWidth,sllOK,;
   slcOrigDBC,slcDD1,sllSQLView,slcDatabase,slcDD0

slnQuote1 = AT('"',sxcValData,1)
slnQuote2 = AT('"',sxcValData,2)
IF slnQuote1 > 0 AND slnQuote2 > 0
   slcRefTableOrView = UPPER(ALLTRIM(SUBSTR(sxcValData,slnQuote1+1,slnQuote2-;
      slnQuote1-1)))
   IF LEFT(slcRefTableOrView,2) == 'M.' AND NOT slcRefTableOrView == 'M.DBF'
      slcRefTableOrView = UPPER(ALLTRIM(&slcRefTableOrView))
   ENDIF
   IF USED('f2reftable')
      USE IN f2reftable
   ENDIF
   IF sgcAppPre == 'APP'
      IF UPPER(WONTOP()) = 'FRMDATABUILDER' OR sglTestForm
         * Running the Data Builder or testing a form in ProMatrix
         slcDD1 = sgcPDD1
      ELSE
         * Running a ProMatrix form
         slcDD1 = 'SAPPDD1.DBF'
      ENDIF
   ELSE
      * Running an application form
      slcDD1 = sgcPDD1
   ENDIF
   sllOK = .F.
   IF EVAL(sgcUUse+"(slcDD1,'dd1lookup','AGAIN','','',sxcDDToUse)")
      IF SEEK(PADR(LEFT(slcRefTableOrView,112),112),'dd1lookup','file_name')
         sllSQLView = dd1lookup.sqlview
         slcDatabase = ALLTRIM(dd1lookup.dbc_name)
         sllOK = .T.
      ENDIF
      USE
   ENDIF
   IF sllOK
      IF sllSQLView
         * Open the database or make it the current database if already open.
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
      sxnPriWA = SELECT()
      sllFiltErr = .F.
      slnQuote1 = AT('"',sxcValData,19)
      slnQuote2 = AT('"',sxcValData,20)
      IF slnQuote1 > 0 AND slnQuote2 > 0
         slcFCode = SUBSTR(sxcValData,slnQuote1+1,slnQuote2-slnQuote1-1)
         IF NOT EMPTY(slcFCode)
            * Remove alias for the reference table from fields in the
            * filter code since it is opened here with a different alias.
            slcFileT = LEFT(slcRefTableOrView,RAT('.',slcRefTableOrView))
            slcFCode = STRTRAN(slcFCode,slcFileT)
            slcFCode = STRTRAN(slcFCode,LOWER(slcFileT))
            slcOnError = ON('ERROR')
            ON ERROR sllFiltErr = .T.
            SET FILTER TO &slcFCode
            ON ERROR &slcOnError
            oApp.First(SET('DATASESSION'))
         ENDIF
      ENDIF
      IF sllFiltErr
         = MESSAGEBOX(F2ERRORSETTINGFILTER,MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ELSE
         slcForFlds = ''
         FOR slnFor = 1 TO 4
            slnQuote1 = AT('"',sxcValData,slnFor*2+1)
            slnQuote2 = AT('"',sxcValData,slnFor*2+2)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               slcFK = SUBSTR(sxcValData,slnQuote1+1,slnQuote2-slnQuote1-1)
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
            slnQuote1 = AT('"',sxcValData,slnFor*2+9)
            slnQuote2 = AT('"',sxcValData,slnFor*2+10)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               slcPK = SUBSTR(sxcValData,slnQuote1+1,slnQuote2-slnQuote1-1)
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
               sxcPriFld&slcFor = slcPriFld
               sxcForFld&slcFor = slcForFld
               sxlReferential = .T.
               sxnF2Mess = 2
               IF sxlDMV
                  IF UPPER(sxoActCtrl.Parent.BaseClass) == 'PAGE' OR ;
                     UPPER(sxoActCtrl.Parent.BaseClass) == 'CONTAINER'
                     FOR slnFor1 = 1 TO sxoActCtrl.Parent.ControlCount
                        IF slcForFld == ALLTRIM(UPPER(sxoActCtrl.Parent.;
                           Controls(slnFor1).Name))
                           sxcForObj&slcFor = sxoActCtrl.Parent.;
                              Controls(slnFor1)
                           EXIT
                        ENDIF
                     ENDFOR
                  ELSE
                     FOR slnFor1 = 1 TO sxoActForm.ControlCount
                        IF slcForFld == ALLTRIM(UPPER(sxoActForm.;
                           Controls(slnFor1).Name))
                           sxcForObj&slcFor = sxoActForm.Controls(slnFor1)
                           EXIT
                        ENDIF
                     ENDFOR
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF sxlReferential
            slnQuote1 = AT('"',sxcValData,21)
            slnQuote2 = AT('"',sxcValData,22)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               sxcListFields = SUBSTR(sxcValData,slnQuote1+1,;
                  slnQuote2-slnQuote1-1)
            ENDIF
            slnQuote1 = AT('"',sxcValData,27)
            slnQuote2 = AT('"',sxcValData,28)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               sxcListCaptions = SUBSTR(sxcValData,slnQuote1+1,;
                  slnQuote2-slnQuote1-1)
            ENDIF
            slnQuote1 = AT('"',sxcValData,31)
            slnQuote2 = AT('"',sxcValData,32)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               sxcPriEdit = ALLTRIM(SUBSTR(sxcValData,slnQuote1+1,;
                  slnQuote2-slnQuote1-1))
               IF sxcPriEdit == 'T' OR sxcPriEdit == 'F'
                  * Handles prior version where entry was 'T' or 'F'.
                  sxcPriEdit = ''
               ENDIF
            ENDIF
            slnQuote1 = AT('"',sxcValData,35)
            slnQuote2 = AT('"',sxcValData,36)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               sxcListTags = SUBSTR(sxcValData,slnQuote1+1,;
                  slnQuote2-slnQuote1-1)
            ENDIF
            slnQuote1 = AT('"',sxcValData,37)
            slnQuote2 = AT('"',sxcValData,38)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               slcListRows = SUBSTR(sxcValData,slnQuote1+1,;
                  slnQuote2-slnQuote1-1)
               IF EMPTY(slcListRows)
                  sxnListRows = 0
               ELSE
                  sxnListRows = INT(VAL(slcListRows))
               ENDIF
            ENDIF
            slnQuote1 = AT('"',sxcValData,39)
            slnQuote2 = AT('"',sxcValData,40)
            IF slnQuote1 > 0 AND slnQuote2 > 0
               slcListWidth = SUBSTR(sxcValData,slnQuote1+1,;
                  slnQuote2-slnQuote1-1)
               IF EMPTY(slcListWidth)
                  sxnListWidth = 0
               ELSE
                  sxnListWidth = INT(VAL(slcListWidth))
               ENDIF
            ENDIF

            * Try to have the selection dialog come up on the record that
            * matches the current entry.
            SELECT (sxnForWA)
            slcForExpr = ''
            slcForExp2 = ''
            sllForExp2 = .F.
            FOR slnFor = 1 TO 9
               slcFor = STR(slnFor,1)
               slcForFld = sxcForFld&slcFor
               slcPriFld = sxcPriFld&slcFor
               IF NOT EMPTY(slcPriFld) AND NOT EMPTY(slcForFld)
                  IF sxlDMV
                     slValue&slcFor = sxcForObj&slcFor..Value
                  ELSE
                     IF NOT EMPTY(sxcAlias)
                        * Handle fields not in the active table
                        SELECT (sxcAlias)
                     ENDIF
                     slValue&slcFor = &slcForFld
                     SELECT (sxnForWA)
                  ENDIF
                  IF EMPTY(slcForExpr)
                     slcForExpr = slcPriFld+' = slValue'+slcFor
                  ELSE
                     slcForExpr = slcForExpr+' AND '+slcPriFld+;
                        ' = slValue'+slcFor
                  ENDIF
                  * For partial value match
                  slVal = slValue&slcFor
                  slcFor = slcFor+'2'
                  IF TYPE('slVal') = 'C'
                     IF EMPTY(slVal)
                        LOOP
                     ENDIF
                     sllForExp2 = .T.
                     slValue&slcFor = TRIM(slVal)
                  ELSE
                     slValue&slcFor = slVal
                  ENDIF
                  IF EMPTY(slcForExp2)
                     slcForExp2 = slcPriFld+' = slValue'+slcFor
                  ELSE
                     slcForExp2 = slcForExp2+' AND '+slcPriFld+;
                        ' = slValue'+slcFor
                  ENDIF
               ENDIF
            ENDFOR
            IF NOT EMPTY(slcForExpr)
               SELECT (sxnPriWA)
               LOCATE FOR &slcForExpr
               IF FOUND()
                  sxlRefFound = .T.
               ELSE
                  IF sllForExp2
                     * Try to find a match using partial value(s).
                     LOCATE FOR &slcForExp2
                     IF FOUND()
                        sxlRefFound = .T.
                     ELSE
                        oApp.First(SET('DATASESSION'))
                     ENDIF
                  ELSE
                     oApp.First(SET('DATASESSION'))
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   SELECT dd2lookup
ENDIF

*****************
FUNCTION siPrevDS
*****************
SELECT (sxnCurArea)
SET DATASESSION TO (sxnSaveDSID)
