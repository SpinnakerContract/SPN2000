******************************************************************
*      	ProMatrix Corporation                                
*                                                             
*      	Copyright © 1995-98 ProMatrix Corporation    
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
* S<Prefix>CGPK - Calculate the incremented value to be placed in a
***************   field in a new record.

* Assumptions:
* This program is being called from the S<Prefix>CDef program.  The DD2
* table will be open and the record pointer will be positioned on the
* record for the field for which the calculation of the default value
* is being performed.

LPARAM stnLastUsed

LOCAL slnNewNumber,slcDefExpr,slnCalcPos,slnLPPos,slnRPPos,slnFor,;
   slRetVal,slcTable,slcField,sllTableUsed

LOCAL ARRAY slaLastUsed(1,1)

* If the parameter passed to this program is a negative number, the
* highest value that exists in this field must be determined.
IF stnLastUsed < 0
   slcTable = JUSTSTEM(dd2default.file_name)
   slcField = ALLTRIM(dd2default.field_name)
   sllTableUsed = USED(slcTable)
   SELECT MAX(VAL(&slcField)) ;
      FROM &slcTable ;
      WHERE NOT DELETED() ;
      INTO ARRAY slaLastUsed
   stnLastUsed = slaLastUsed(1,1)
   IF NOT sllTableUsed
      USE IN (slcTable)
   ENDIF
ENDIF

slnNewNumber = 0
slcDefExpr = ALLTRIM(dd2default.default)
slnCalcPos = ATC('CGPK(',slcDefExpr)
IF slnCalcPos > 0
   slnLPPos = slnCalcPos+4
   slnRPPos = 0
   FOR slnFor = slnLPPos+1 TO LEN(slcDefExpr)
      IF SUBSTR(slcDefExpr,slnFor,1) = ')'
         slnRPPos = slnFor
         EXIT
      ENDIF
   ENDFOR
   IF slnRPPos > 0
      IF EMPTY(stnLastUsed)
         slnNewNumber = 1
      ELSE
         slnNewNumber = stnLastUsed+1
      ENDIF
      REPLACE dd2default.default WITH STUFF(slcDefExpr,slnLPPos,;
         slnRPPos-slnLPPos+1,'('+ALLTRIM(STR(slnNewNumber))+')')
   ENDIF
ENDIF
IF slnNewNumber > 0
   IF dd2default.type $ 'CM'
      slRetVal = ALLTRIM(STR(slnNewNumber))
   ELSE
      slRetVal = slnNewNumber
   ENDIF
   RETURN slRetVal
ELSE
   * Attempt to determine new number has failed so cancel add.
   RETURN .F.
ENDIF
