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
* S<Prefix>SMCF - Menu, control and field security check.
***************

LPARAM stcParam1,stcParam2,stcParam3

* Parameters
*
* Menu security:
*    stcParam1 - 'M'
*    stcParam2 - Option number
*    stcParam3 - Menu number
*
* Control security:
*    stcParam1 - 'C'
*    stcParam2 - Control ID
*    stcParam3 - Form Name
*
* Field security (No Display):
*    stcParam1 - 'F'
*    stcParam2 - Field ID Number
*    stcParam3 - Not used
*
* Field security (Display Only):
*    stcParam1 - 'FD'
*    stcParam2 - Field ID Number
*    stcParam3 - Not used

IF EMPTY(oSec.cCurrentUserID)
   * If Access Security is turned off, there is no need to perform the 
   * security check.
   RETURN .T.
ENDIF

LOCAL slcPar1,slcPar2,slcPar3,slnCurArea,sllReturn,slcIDGCode,slcUG,;
   slcSecNum,slnDot,slcDD2,slcDD2Alias,slnDot,slnBS

DO CASE
   CASE PARAMETERS() = 2
      slcPar1 = ALLTRIM(UPPER(stcParam1))
      slcPar2 = ALLTRIM(UPPER(stcParam2))
      slcPar3 = ''
   CASE PARAMETERS() = 3
      slcPar1 = ALLTRIM(UPPER(stcParam1))
      slcPar2 = ALLTRIM(UPPER(stcParam2))
      slcPar3 = ALLTRIM(UPPER(stcParam3))
   OTHERWISE
      RETURN .F.
ENDCASE

slnCurArea = SELECT()

* Perform security check.
sllReturn = .F.
DO CASE
   CASE slcPar1 == 'M'
      * The menu security check is run from the menu program.  It is run
      * for each menu item that has the GENMENUX directive (*:IF) in its 
      * comment snippet.  Using the values passed to this program and
      * security object properties, the security table is checked to see
      * if there is a record for the menu item for which this program was
      * called for the current user or the current user's menu permissions
      * group code if one has been assigned.  If a record exists, access
      * to this menu item is granted.
      IF siOpenSec()
         oApp.SetOrder(ALIAS(),SET('DATASESSION'),'menu')
         IF EMPTY(SUBSTR(oSec.cGroups,7,3))
            * Find security record for the current user.
            slcIDGCode = oSec.cCurrentUserID
            slcUG = 'U'
         ELSE
            * Find security record for the current user's menu group.
            slcIDGCode = SUBSTR(oSec.cGroups,7,3)
            slcUG = 'G'
         ENDIF
         IF SEEK(LEFT(oSec.StringEncrypt('M'+slcUG+PADR(slcIDGCode,10)+;
            PADR(slcPar3,3)+PADR(slcPar2,3)+SPACE(8)),18))
            * Make sure data has not been changed by validating the check 
            * sum value.
            IF LEFT(mcfsec.encrypted,4) = oSec.StringEncrypt(;
               oSec.FieldCheckSum(encrypted))
               sllReturn = .T.
            ENDIF
         ENDIF
      ENDIF
   CASE slcPar1 == 'C'
      * The controls security check is run from the control's Init method.
      * Using the values passed to this program and security object
      * properties, the security table is checked to see if there is a
      * record for the current user or the current user's control group
      * code if one has been assigned.  If a record exists, access to this
      * control is granted.
      IF siOpenSec()
         oApp.SetOrder(ALIAS(),SET('DATASESSION'),'control')
         IF EMPTY(LEFT(oSec.cGroups,3))
            * Find security record for the current user.
            slcIDGCode = oSec.cCurrentUserID
            slcUG = 'U'
         ELSE
            * Find security record for the current user's menu group.
            slcIDGCode = LEFT(oSec.cGroups,3)
            slcUG = 'G'
         ENDIF
         IF SEEK(oSec.StringEncrypt('C'+slcUG+PADR(slcIDGCode,10)+;
            LEFT(PADR(slcPar3,12),12)+LEFT(PADR(slcPar2,2),2)))
            * Make sure data has not been changed by validating the check 
            * sum value.
            IF LEFT(mcfsec.encrypted,4) = oSec.StringEncrypt(;
               oSec.FieldCheckSum(encrypted))
               sllReturn = .T.
            ENDIF
         ENDIF
      ENDIF
   CASE slcPar1 == 'F'
      * The field (no display) security check is run from the control Init
      * method.  Using the values passed to this program and security object
      * properties, the security table is checked to see if there is a
      * record for the control for which this program was called for the
      * current user's field permissions group code.  If a record exists
      * and is marked with an "n", this control will not be displayed.
      sllReturn = .T.
      IF NOT EMPTY(SUBSTR(oSec.cGroups,4,3))
         IF siOpenSec()
            oApp.SetOrder(ALIAS(),SET('DATASESSION'),'field')
            IF SEEK(LEFT(oSec.StringEncrypt('F'+PADR(slcPar2,5)+;
               SUBSTR(oSec.cGroups,4,3)+SPACE(17)),9))
               * Make sure data has not been changed by validating the
               * check sum value.
               IF LEFT(mcfsec.encrypted,4) = oSec.StringEncrypt(;
                  oSec.FieldCheckSum(encrypted))
                  IF UPPER(SUBSTR(oSec.StringDecrypt(;
                     SUBSTR(mcfsec.encrypted,5,26)),10,1)) = 'N'
                     sllReturn = .F.
                  ENDIF
               ELSE
                  * If data has been changed, do not display field.
                  sllReturn = .F.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   CASE slcPar1 == 'FD'
      * The field (display only) security check is run from the When
      * program (sgcCWhn) which is run from control When method.  It is
      * run for each control that has the call to the When program in its
      * When method.  Using the values passed to this program and security
      * object properties, the security table is checked to see if there is
      * a record for the control for which this program was called for the
      * current user's field permission group code.  If a record exists and
      * is marked with a "d", this control will not be editable.
      sllReturn = .T.
      IF NOT EMPTY(SUBSTR(oSec.cGroups,4,3))
         IF siOpenSec()
            oApp.SetOrder(ALIAS(),SET('DATASESSION'),'field')
            IF SEEK(LEFT(oSec.StringEncrypt('F'+PADR(slcPar2,5)+;
               SUBSTR(oSec.cGroups,4,3)+SPACE(17)),9))
               * Make sure data has not been changed by validating the
               * check sum value.
               IF LEFT(mcfsec.encrypted,4) = oSec.StringEncrypt(;
                  oSec.FieldCheckSum(encrypted))
                  IF UPPER(SUBSTR(oSec.StringDecrypt(;
                     SUBSTR(mcfsec.encrypted,5,26)),10,1)) = 'D'
                     sllReturn = .F.
                  ENDIF
               ELSE
                  * If data has been changed, make field display only.
                  sllReturn = .F.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
ENDCASE

IF sgcAppPre = 'APP' AND sglTestForm	&& Testing a form while in VPM
   IF USED('mcfsec')
      USE IN mcfsec
   ENDIF
ENDIF

SELECT (slnCurArea)

* Return Values
*
* Menu security:
*    .T. - Access granted
*    .F. - Access denied
*
* Control security:
*    .T. - Access granted
*    .F. - Access denied
*
* Field security (No Display):
*    .T. - Not restricted (OK to display)
*    .F. - Restricted (Do not display)
*
* Field security (Display Only):
*    .T. - Not restricted (OK to edit)
*    .F. - Restricted (Do not allow editing)

RETURN sllReturn

******************
FUNCTION siOpenSec
******************
* Open the Security table.

LOCAL sllRetVal

sllRetVal = .T.
IF sgcAppPre = 'APP' AND sglTestForm	&& Testing a form while in VPM
   IF USED('mcfsec')
      USE IN mcfsec
   ENDIF
   IF NOT EVAL(sgcUUse+"('S'+sgcCurPre+'Sec','mcfsec','AGAIN','','','PDD')")
      sllRetVal = .F.
   ENDIF
ELSE
   IF USED('mcfsec')
      SELECT mcfsec
   ELSE
      IF NOT EVAL(sgcUUse+"('S'+sgcAppPre+'Sec','mcfsec','AGAIN')")
         sllRetVal = .F.
      ENDIF
   ENDIF
ENDIF
RETURN sllRetVal
