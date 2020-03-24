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
* S<Prefix>HF3 - Bring up the Textual Help window for viewing/modifying
**************   field textual help.

LPARAM stcKey,stlAltF3

* Parameters:
* stcKey     - The key that caused this program to be executed.
*              Values are 'F3' or 'AF3' (Alt+F3).
* stlAltF3   - F3 editing status.  Values are .T. or .F..

PRIVATE sxnCtrlLeft,sxnCtrlTop,sxnCtrlHeight,sxnFormLeft,sxnFormTop,;
   sxcF3Text,sxcLabel,sxlCanEdit,sxlOK

LOCAL slcOnF2,slcOnRM,slcDDTable,slcDDField,sllCtrlFound,slcCurCtrl,;
   slnActiveCol,slnFor,slcCurObj,slnCurArea,slcDD2,slcDDToUse,;
   sloObject,sllMoreObj,sllOK,sloActForm,sloActCtrl,sloPC,slcDF3

IF PARAMETERS() < 2
   RETURN
ENDIF

CLEAR TYPEAHEAD
slcOnF2 = ON('KEY','F2')
ON KEY LABEL F2 *
slcOnRM = ON('KEY','RIGHTMOUSE')
*ON KEY LABEL RIGHTMOUSE *
ON KEY LABEL F3 *
IF stlAltF3
   ON KEY LABEL Alt+F3 *
ENDIF

IF EMPTY(WONTOP())
   = ResetF2F3(stlAltF3,slcOnF2,slcOnRM)
   ?? CHR(7)
   WAIT 'Field description not available' WINDOW NOWAIT
   RETURN
ENDIF

sloActForm = _SCREEN.ActiveForm
sloActCtrl = _SCREEN.ActiveForm.ActiveControl
sllCtrlFound = .F.
DO CASE
   CASE UPPER(sloActCtrl.BaseClass) == 'GRID'
      slnActiveCol = sloActCtrl.ActiveColumn
      * The column could have been moved from its original position.
      * Find its original position.
      sllOK = .F.
      FOR slnFor = 1 TO sloActCtrl.ColumnCount
         IF sloActCtrl.Columns(slnFor).ColumnOrder = slnActiveCol
            slnActiveCol = slnFor
            sllOK = .T.
            EXIT
         ENDIF
      ENDFOR
      IF sllOK
         slcCurCtrl = ALLTRIM(UPPER(sloActCtrl.Columns(slnActiveCol).;
            CurrentControl))
         FOR slnFor = 1 TO sloActCtrl.Columns(slnActiveCol).ControlCount
            IF slcCurCtrl == ALLTRIM(UPPER(sloActCtrl.;
               Columns(slnActiveCol).Controls(slnFor).Name))
               slcCurObj = sloActCtrl.Columns(slnActiveCol).Controls(slnFor)
               IF TYPE('slcCurObj.cDDTable') <> 'U' AND ;
                  TYPE('slcCurObj.cDDField') <> 'U'
                  sllCtrlFound = .T.
               ENDIF
               EXIT
            ENDIF
         ENDFOR
      ENDIF
   CASE UPPER(sloActCtrl.Parent.BaseClass) == 'PAGE' OR ;
      UPPER(sloActCtrl.Parent.BaseClass) == 'CONTAINER' OR ;
      (UPPER(sloActCtrl.BaseClass) == 'OPTIONBUTTON' AND ;
      (UPPER(sloActCtrl.Parent.Parent.BaseClass) == 'PAGE' OR ;
      UPPER(sloActCtrl.Parent.Parent.BaseClass) == 'CONTAINER'))
      IF UPPER(sloActCtrl.BaseClass) == 'OPTIONBUTTON'
         slcCurCtrl = ALLTRIM(UPPER(sloActCtrl.Parent.Name))
         sloPC = sloActCtrl.Parent.Parent
      ELSE
         slcCurCtrl = ALLTRIM(UPPER(sloActCtrl.Name))
         sloPC = sloActCtrl.Parent
      ENDIF
      FOR slnFor = 1 TO sloPC.ControlCount
         IF slcCurCtrl == ALLTRIM(UPPER(sloPC.Controls(slnFor).Name))
            slcCurObj = sloPC.Controls(slnFor)
            IF TYPE('slcCurObj.cDDTable') <> 'U' AND ;
               TYPE('slcCurObj.cDDField') <> 'U'
               sllCtrlFound = .T.
            ENDIF
            EXIT
         ENDIF
      ENDFOR
   OTHERWISE
      IF UPPER(sloActCtrl.BaseClass) == 'OPTIONBUTTON'
         slcCurCtrl = ALLTRIM(UPPER(sloActCtrl.Parent.Name))
      ELSE
         slcCurCtrl = ALLTRIM(UPPER(sloActCtrl.Name))
      ENDIF
      FOR slnFor = 1 TO sloActForm.ControlCount
         IF slcCurCtrl == ALLTRIM(UPPER(sloActForm.;
            Controls(slnFor).Name))
            slcCurObj = sloActForm.Controls(slnFor)
            IF TYPE('slcCurObj.cDDTable') <> 'U' AND ;
               TYPE('slcCurObj.cDDField') <> 'U'
               sllCtrlFound = .T.
            ENDIF
            EXIT
         ENDIF
      ENDFOR
ENDCASE
IF NOT sllCtrlFound
   = ResetF2F3(stlAltF3,slcOnF2,slcOnRM)
   ?? CHR(7)
   WAIT 'Field description not available' WINDOW NOWAIT
   RETURN
ENDIF
slcDDTable = slcCurObj.cDDTable
slcDDField = slcCurObj.cDDField
sxnCtrlLeft = slcCurObj.Left
sxnCtrlTop = slcCurObj.Top
sloObject = slcCurObj
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
sxnCtrlHeight = slcCurObj.Height
sxnFormLeft = sloActForm.Left
sxnFormTop = sloActForm.Top

IF EMPTY(slcDDTable)
   = ResetF2F3(stlAltF3,slcOnF2,slcOnRM)
   ?? CHR(7)
   WAIT 'Field description not available' WINDOW NOWAIT
   RETURN
ENDIF
slcDDTable = ALLTRIM(UPPER(slcDDTable))

IF EMPTY(slcDDField)
   = ResetF2F3(stlAltF3,slcOnF2,slcOnRM)
   ?? CHR(7)
   WAIT 'Field description not available' WINDOW NOWAIT
   RETURN
ENDIF
slcDDField = ALLTRIM(UPPER(slcDDField))

* Determine the currently selected work area.
slnCurArea = SELECT()

IF sgcAppPre == 'APP'
   IF UPPER(WONTOP()) = 'FRMDATABUILDER' OR sglTestForm
      * Running the Data Builder or testing a form in ProMatrix
      slcDD2 = sgcPDD2
      slcDF3 = sgcPDF3
      slcDDToUse = 'PDD'
   ELSE
      * Running a ProMatrix form
      slcDD2 = 'SAPPDD2.DBF'
      slcDF3 = 'SAPPDF3.DBF'
      slcDDToUse = ''
   ENDIF
ELSE
   * Running an application form
   slcDD2 = sgcPDD2
   slcDF3 = sgcPDF3
   slcDDToUse = ''
ENDIF
* Open the appropriate DD2 table.
IF NOT EVAL(sgcUUse+"(slcDD2,'f3_dd2','AGAIN','','',slcDDToUse)")
   SELECT (slnCurArea)
   = ResetF2F3(stlAltF3,slcOnF2,slcOnRM)
   ?? CHR(7)
   WAIT 'Field description not available' WINDOW NOWAIT
   RETURN
ENDIF
* Find the DD2 record for the current control.
IF NOT SEEK(PADR(LEFT(slcDDTable,112),112)+PADR(slcDDField,128),'f3_dd2','file_name')
   USE
   SELECT (slnCurArea)
   = ResetF2F3(stlAltF3,slcOnF2,slcOnRM)
   ?? CHR(7)
   WAIT 'Field description not available' WINDOW NOWAIT
   RETURN
ENDIF
IF EMPTY(f3_dd2.label)
   sxcLabel = PROPER(ALLTRIM(f3_dd2.field_name))
ELSE
   sxcLabel = ALLTRIM(f3_dd2.label)
ENDIF
* Open the appropriate F3 Help table.
IF NOT EVAL(sgcUUse+"(slcDF3,'f3_df3','AGAIN','','',slcDDToUse)")
   USE IN f3_dd2
   SELECT (slnCurArea)
   = ResetF2F3(stlAltF3,slcOnF2,slcOnRM)
   ?? CHR(7)
   WAIT 'Field description not available' WINDOW NOWAIT
   RETURN
ENDIF
IF SEEK(PADR(LEFT(slcDDTable,112),112)+PADR(slcDDField,128),'f3_df3','file_name')
   * There is a record in the F3 Help table for the field.  Bring up the F3
   * form, displaying the text in the f3_text field.
   sxcF3Text = f3_df3.f3_text
ELSE
   * There isn't a record in the F3 Help table for the field.  See if the field
   * is a view field.
   IF EMPTY(f3_dd2.viewsrctbl) OR EMPTY(f3_dd2.viewsrcfld)
      * The field is not a view field, so bring up the F3 form only if the user
      * pressed Alt+F3.
      sxcF3Text = ''
   ELSE
      * The field is a view field.
      slcDDTable = f3_dd2.viewsrctbl
      slcDDField = f3_dd2.viewsrcfld
      IF SEEK(PADR(LEFT(slcDDTable,112),112)+PADR(slcDDField,128),'f3_df3','file_name')
         * There is a record in the F3 Help table for the view field.  Bring up
         * the F3 form, displaying the text in the f3_text field.
         sxcF3Text = f3_df3.f3_text
      ELSE
         * There isn't a record in the F3 Help table for the view field.  Bring up
         * the F3 form only if the user pressed Alt+F3.
         sxcF3Text = ''
      ENDIF
   ENDIF
ENDIF
USE IN f3_dd2

IF EMPTY(sxcF3Text) AND stcKey == 'F3'
   USE IN f3_df3
   SELECT (slnCurArea)
   = ResetF2F3(stlAltF3,slcOnF2,slcOnRM)
   ?? CHR(7)
   WAIT 'Field description not available' WINDOW NOWAIT
   RETURN
ENDIF

IF stcKey == 'AF3' AND stlAltF3
   * Can edit F3 text.
   sxlCanEdit = .T.
ELSE
   * Cannot edit F3 text.
   sxlCanEdit = .F.
ENDIF
sxlOK = .F.

IF TYPE('sloActForm.lSkipActivate') <> 'U'
   sloActForm.lSkipActivate = .T.
ENDIF

DO FORM (sgcHF3F)

SELECT f3_df3
IF sxlCanEdit AND sxlOK
   * If the user selects the OK push button, save any changes.
   IF EOF()
      IF NOT EMPTY(sxcF3Text)
         IF oApp.AddRecord(ALIAS(),SET('DATASESSION'))
            REPLACE f3_df3.file_name WITH slcDDTable,;
                    f3_df3.field_name WITH slcDDField,;
                    f3_df3.f3_text WITH ALLTRIM(sxcF3Text)
         ELSE
            ?? CHR(7)
            WAIT 'Field description could not be saved' WINDOW NOWAIT
         ENDIF
      ENDIF
   ELSE
      IF EMPTY(sxcF3Text)
         DELETE
      ELSE
         REPLACE f3_df3.f3_text WITH ALLTRIM(sxcF3Text)
      ENDIF
   ENDIF
ENDIF
USE
SELECT (slnCurArea)

= ResetF2F3(stlAltF3,slcOnF2,slcOnRM)

* Put the focus back on the correct control.
sloActCtrl.SetFocus()

******************
FUNCTION ResetF2F3
******************
LPARAM stlAltF3,stcOnF2,stcOnRM

ON KEY LABEL F2 &stcOnF2
*ON KEY LABEL RIGHTMOUSE &stcOnRM
ON KEY LABEL F3 KEYB "{Ctrl+F4}"  && ENABLE for CTI
IF stlAltF3
   *ON KEY LABEL F3 do &sgcFCT with 'HELP_F3','F3',.T.  && disable for CTI
   ON KEY LABEL Alt+F3 do &sgcFCT with 'HELP_F3','AF3',.T.
ELSE
   *ON KEY LABEL F3 do &sgcFCT with 'HELP_F3','F3',.F.  && disable for CTI
ENDIF
