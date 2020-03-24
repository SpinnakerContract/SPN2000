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
* S<Prefix>URL - Attempt to lock the current record using the RLOCK command.
*                If the record cannot be locked, handle the retry of the
*                lock.  Return .T. if the lock was placed successfully or
**************   .F. if not.

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

#INCLUDE 'MAIN.H'

LOCAL sllOurMess,sllLocked,slnAlert

* FoxPro handles the retry message in certain situations.
sllOurMess = .T.
IF EMPTY(ON('ERROR'))
   IF SET('REPROCESS') <= 0
      sllOurMess = .F.
   ENDIF
ENDIF

sllLocked = .F.
DO WHILE NOT sllLocked
   IF RLOCK()
      RETURN .T.
   ENDIF
   IF sllOurMess
      IF MESSAGEBOX('The record you are attempting to use is '+;
         'already being used by someone else.  The record may be '+;
         'available if you try to use it again.  Would you like to '+;
         'attempt to use the record again?',MB_ICONQUESTION+MB_YESNO,;
         sgcAppTitle) = IDNO
         RETURN .F.
      ENDIF
   ELSE
      RETURN .F.
   ENDIF
ENDDO
