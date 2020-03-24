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

***************
* S<Prefix>UAct - Add a record to the Activity Tracking table.
***************

LPARAM stcOption,stcDesc

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

LOCAL slcOption2,slnCurArea,slcDesc2,sllOK

IF TYPE('oSec') <> 'O'
   RETURN
ENDIF

slcOption2 = ALLTRIM(stcOption)
IF RIGHT(slcOption2,3) = '...'
   slcOption2 = LEFT(slcOption2,LEN(slcOption2)-3)
ENDIF

IF PARAMETERS() < 2
   slcDesc2 = ''
ELSE
   slcDesc2 = ALLTRIM(stcDesc)
ENDIF

slnCurArea = SELECT()

IF EVAL(sgcUUse+"(sgcActDBF)")
   sllOK = .T.
   IF TYPE('oApp') = 'O'
      IF NOT oApp.AddRecord(ALIAS(),SET('DATASESSION'))
         sllOK = .F.
      ENDIF
   ELSE
      APPEND BLANK
   ENDIF
   IF sllOK
      REPLACE user WITH oSec.cCurrentUserID,;
              prefix WITH sgcCurPre,;
              date WITH DATE(),;
              time WITH TIME(),;
              desc WITH slcDesc2,;
              option WITH slcOption2
   ENDIF
ENDIF

SELECT (slnCurArea)
