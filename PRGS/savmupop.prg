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
* S<Prefix>UPop - Restore the status of SET commands whose status can be set
***************   to either ON or OFF.

PARAM slcSetObj

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

* Parameter:
* slcSetObj - The name of the SET command.
PRIVATE slnObjLoc,slnObjLen,slcObjSet

* Find the command in the memory variable string sgcSetList.  The command's
* status was stored when seSetPsh was called.  sgcSetList is a string that
* contains a list of statuses that are stored until they are needed.
slnObjLoc = RAT(slcSetObj,sgcSetList)
IF slnObjLoc = 0
   RETURN
ENDIF

* Once the command is found, extract the status stored for this command.
slnObjLen = AT('=',SUBSTR(sgcSetList,slnObjLoc))
slcObjSet = SUBSTR(sgcSetList,slnObjLoc+slnObjLen,3)

* Set the status of the SET command.
SET &slcSetObj &slcObjSet

* Remove this command from sgcSetList.
IF slnObjLoc+slnObjLen+4 > LEN(sgcSetList)
   sgcSetList = SUBSTR(sgcSetList,1,slnObjLoc-1)
ELSE
   sgcSetList = SUBSTR(sgcSetList,1,slnObjLoc-1)+;
      SUBSTR(sgcSetList,slnObjLoc+slnObjLen+4)
ENDIF
