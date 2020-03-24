******************************************************************
*      	Lawson-Smith Corporation                                
*                                                             
*      	Copyright © 1995-1996 Lawson-Smith Corporation    
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

**************
* S<Prefix>UMv - Bring up the Mover dialog.  Return .T. if OK is selected
**************   on the dialog.

LPARAM stcSArrayName,stcDArrayName,stlMoverBars,stlNoSelectionOK,;
   stcFormCaption,stcSourceLabel,stcDestLabel,stnListBoxWidth,stcMoverClass

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

PRIVATE sxlOKSelected

LOCAL sloMover

* Parameters:
*
* stcSArrayName    - Name of source array (must be public or private).
* stcDArrayName    - Name of destination array (must be public or private).
* stlMoverBars     - Causes the destination ListBox to have MoverBars and
*                    not be sorted.  (Default: .F.)
* stlNoSelectionOK - Allows the user to have an empty destination ListBox
*                    and select OK.  Otherwise, the OK CommandButton is
*                    disabled until an entry is in the destination ListBox.
*                    (Default: .F.)
* stcFormCaption   - The form's caption.  (Default: no caption)
* stcSourceLabel   - Label to appear above the source ListBox.  (Default:
*                    label is made invisible)
* stcDestLabel     - Label to appear above the destination ListBox.
*                    (Default: label is made invisible)
* stnListBoxWidth  - The width of the source and destination ListBoxes.
*                    (Default: 120)
* stcMoverClass    - The class that the mover dialog is created from.
*                    (Default: "form_mover" class is used)

sxlOKSelected = .F.

IF PARAMETERS() < 2
   RETURN sxlOKSelected
ENDIF

IF EMPTY(stcSArrayName)
   RETURN sxlOKSelected
ENDIF
IF EMPTY(stcDArrayName)
   RETURN sxlOKSelected
ENDIF
IF EMPTY(stlMoverBars)
   stlMoverBars = .F.
ENDIF
IF EMPTY(stlNoSelectionOK)
   stlNoSelectionOK = .F.
ENDIF
IF EMPTY(stcFormCaption)
   stcFormCaption = ''
ENDIF
IF EMPTY(stcSourceLabel)
   stcSourceLabel = ''
ENDIF
IF EMPTY(stcDestLabel)
   stcDestLabel = ''
ENDIF
IF EMPTY(stnListBoxWidth)
   stnListBoxWidth = 0
ENDIF
IF EMPTY(stcMoverClass)
   stcMoverClass = 'form_mover'
ENDIF

sloMover = CREATEOBJECT(stcMoverClass,stcSArrayName,stcDArrayName,;
   stlMoverBars,stlNoSelectionOK,stcFormCaption,stcSourceLabel,;
   stcDestLabel,stnListBoxWidth)
sloMover.Show()

RETURN sxlOKSelected				&& Modified in mover dialog
