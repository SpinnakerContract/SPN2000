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
* S<Prefix>UDir - Checks to see if the specified path (directory) exists.
***************

LPARAM stcDir

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

LOCAL sllRetVal,slcTestDir,slcOrigDef,slcOnError
   
sllRetVal = .T.
slcTestDir = ALLTRIM(stcDir)
IF NOT EMPTY(slcTestDir)
   IF NOT RIGHT(slcTestDir,1) = '\'
      * In case just a drive is passed.
      slcTestDir = slcTestDir+'\'
   ENDIF
   slcOrigDef = SYS(5)+SYS(2003)
   slcOnError = ON('ERROR')
   ON ERROR sllRetVal = .F.
   SET DEFAULT TO (slcTestDir)
   ON ERROR &slcOnError
   SET DEFAULT TO (slcOrigDef)
ENDIF
RETURN sllRetVal
