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
* S<Prefix>USP - Shorten the path passed to this program.
**************

LPARAM stcFullPath,stnLength

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

* Parameters:
* stcFullPath - File name plus path.
* stnLength   - Maximum length of string to be returned.

LOCAL slcReturn,slcPth,slnPos,slcLeft,slcRight,slnRightMax,sllMore

slcReturn = ''
IF PARAMETERS() = 2
   slcPth = ALLTRIM(stcFullPath)
   IF LEN(slcPth) <= stnLength
      slcReturn = slcPth
   ELSE
      slnPos = AT('\',slcPth)
      IF slnPos > 0
         slcLeft = LEFT(slcPth,slnPos)+'...\'
         slcRight = SUBSTR(slcPth,slnPos+1)
         sllMore = .T.
         DO WHILE sllMore
            slnPos = AT('\',slcRight)
            IF slnPos = 0
               sllMore = .F.
            ELSE
               slcRight = SUBSTR(slcRight,slnPos+1)
               IF LEN(slcRight)+LEN(slcLeft) <= stnLength
                  slcReturn = slcLeft+slcRight
                  sllMore = .F.
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      IF EMPTY(slcReturn)
         slcReturn = LEFT(slcPth,stnLength)
      ENDIF
   ENDIF
ENDIF

RETURN slcReturn
