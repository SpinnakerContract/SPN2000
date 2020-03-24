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
* S<Prefix>FCTA - Load the Admin Tools array sgaTools.  Issue the 
***************   ON ERROR command.

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

PRIVATE slnCurSel,slnNumCTs,slcCallMV,slcCallPrg,sllErrSet,sllHelpInc

sllErrSet = .F.
sllHelpInc = .F.

PUBLIC ARRAY sgaTools(1,3)
DIMENSION sgaTools(1,3)
sgaTools = ''

slnCurSel = SELECT()

IF NOT EVAL(sgcUUse+"('S'+sgcAppPre+'FCT2.DBF','ct2')")
   ?? CHR(7)
   WAIT WINDOW NOWAIT 'Unable to open S'+sgcAppPre+'FCT2.DBF'
   ON SHUTDOWN
   QUIT
ENDIF

IF NOT EVAL(sgcUUse+"('S'+sgcAppPre+'FCT1.DBF','ct1')")
   ?? CHR(7)
   WAIT WINDOW NOWAIT 'Unable to open S'+sgcAppPre+'FCT1.DBF'
   ON SHUTDOWN
   QUIT
ENDIF

SET HELP OFF
slnNumCTs = 0
SCAN FOR ct1.include
   slnNumCTs = slnNumCTs+1
   DIMENSION sgaTools(slnNumCTs,3)
   sgaTools(slnNumCTs,1) = ct1.id
   sgaTools(slnNumCTs,2) = ct1.execute_mv
   SELECT ct2
   LOCATE FOR ct2.id == ct1.id
   IF FOUND()
      sgaTools(slnNumCTs,3) = ct2.control_on
      IF ALLTRIM(ct1.id) == 'ERROR' AND ct2.control_on
         slcCallMV = ct1.execute_mv
         IF NOT EMPTY(slcCallMV)
            slcCallPrg = &slcCallMV
            ON ERROR DO &slcCallPrg WITH ERROR(),MESSAGE(),SYS(2018),;
               MESSAGE(1),SYS(16),PROGRAM(),LINENO(),LASTKEY(),VARREAD(),;
               WONTOP()
            sllErrSet = .T.
         ENDIF
      ENDIF
      IF ALLTRIM(ct1.id) == 'HELP_F1'
         sllHelpInc = .T.
         IF ct2.control_on
            SET HELP ON
         ENDIF
      ENDIF
   ELSE
      sgaTools(slnNumCTs,3) = .F.
   ENDIF
   SELECT ct1
ENDSCAN
IF NOT sllHelpInc
   sgcHelpDBF = ''
ENDIF

USE

SELECT ct2
USE

SELECT (slnCurSel)

RETURN sllErrSet
