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
* S<Prefix>FDS3 - Replace the drive designator of the string that is passed
*                 to this program.  The first character of the string is
***************   replaced.

PARAM slcString,slcWhichWay

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

PRIVATE slcStr,slnFromCol,slnToCol,slnNumRows,sllUpper,slnFor

IF PARAMETERS() = 0
   RETURN ''
ENDIF

slcStr = slcString

IF PARAMETERS() = 1
   RETURN slcStr
ENDIF

IF LEFT(ALLTRIM(UPPER(slcWhichWay)),5) == 'STORE'
   slnFromCol = 2
   slnToCol = 1
ELSE
   slnFromCol = 1
   slnToCol = 2
ENDIF

slnNumRows = ALEN(sgaDrvSwap,1)
IF slnNumRows > 0 AND AT(':',slcStr) > 0 AND ISALPHA(slcStr)
   sllUpper = ISUPPER(slcStr)
   FOR slnFor = 1 TO slnNumRows
      IF NOT EMPTY(sgaDrvSwap(slnFor,1)) AND NOT EMPTY(sgaDrvSwap(slnFor,2))
         IF sllUpper
            IF LEFT(slcStr,1) == UPPER(sgaDrvSwap(slnFor,slnFromCol))
               slcStr = STUFF(slcStr,1,1,UPPER(sgaDrvSwap(slnFor,slnToCol)))
               EXIT
            ENDIF
         ELSE
            IF LEFT(slcStr,1) == LOWER(sgaDrvSwap(slnFor,slnFromCol))
               slcStr = STUFF(slcStr,1,1,LOWER(sgaDrvSwap(slnFor,slnToCol)))
               EXIT
            ENDIF
         ENDIF
      ENDIF
   ENDFOR
ENDIF

RETURN slcStr
