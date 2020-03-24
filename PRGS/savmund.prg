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
* S<Prefix>UND - Create a new directory as well as any higher level
**************   directories that don't exist.

LPARAM stcNewDir

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

* Parameter:
* stcNewDir - New directory to be created.

LOCAL sllOK,slcNewDir,sllMore,slnNumBS,slcDefault,slcOnError,slnBS,sllErr

sllOK = .F.
IF NOT EMPTY(stcNewDir)
   slcNewDir = ALLTRIM(stcNewDir)
   IF RIGHT(slcNewDir,1) <> '\'
      slcNewDir = slcNewDir+'\'
   ENDIF
   sllOK = .T.
   sllMore = .T.
   slnNumBS = 1
   slcDefault = SYS(5)+SYS(2003)
   slcOnError = ON('ERROR')
   ON ERROR sllErr = .T.
   * Create the entire directory structure, one level at a time.
   DO WHILE sllMore
      slnNumBS = slnNumBS+1
      slnBS = AT('\',slcNewDir,slnNumBS)
      IF slnBS > 0
         sllErr = .F.
         SET DEFAULT TO (LEFT(slcNewDir,slnBS-1))
         IF sllErr			&& Directory does not exist so create it.
            MD (LEFT(slcNewDir,slnBS-1))
            sllErr = .F.
            SET DEFAULT TO (LEFT(slcNewDir,slnBS-1))
            IF sllErr		&& Directory was not created.
               sllOK = .F.
               sllMore = .F.
            ENDIF
         ENDIF
      ELSE
         sllMore = .F.
      ENDIF
   ENDDO
   ON ERROR &slcOnError
   SET DEFAULT TO (slcDefault)
ENDIF
RETURN sllOK
