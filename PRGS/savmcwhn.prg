******************************************************************
*      	Lawson-Smith Corporation                                
*                                                             
*      	Copyright © 1995 Lawson-Smith Corporation    
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
* S<Prefix>CWhn - This program is called from the When method of controls.
***************   It determines whether the control can receive the focus.   control.

LPARAM stcWhenExpr,stcSecNum

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

* Parameters:
* stcWhenExpr - When expression
* stcSecNum   - Field security number

LOCAL slcWhenExpr,slcSecNum,sllRetVal,slnCurArea

slcWhenExpr = ALLTRIM(stcWhenExpr)
slcSecNum = ALLTRIM(stcSecNum)
sllRetVal = .T.
slnCurArea = SELECT()

IF NOT EMPTY(slcSecNum)
   * If this field has a field security ID number specified, perform
   * the Display Only check.
   sllRetVal = EVAL(sgcFCT+"('SEC_FIELD','FD',slcSecNum)")
   SELECT (slnCurArea)
ENDIF

IF sllRetVal AND NOT EMPTY(slcWhenExpr)
   sllRetVal = EVAL(slcWhenExpr)
   SELECT (slnCurArea)
ENDIF

RETURN sllRetVal
