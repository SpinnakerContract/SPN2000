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
* S<Prefix>UPsh - Change the status of SET commands whose status can be set
*                 to either ON or OFF.  The status prior to the change is 
*                 stored so that it can be restored when (sgcUPop) is called
***************   at a later time.

PARAM slcSetObj,slcSetStat

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

* Parameters:
* slcSetObj  - The name of the SET command.
* slcSetStat - The status to which this command is to be set (ON or OFF).

PRIVATE slcNewStat

slcNewStat = ALLTRIM(UPPER(slcSetStat))
IF slcNewStat <> 'ON' AND slcNewStat <> 'OFF'
   RETURN
ENDIF

IF EMPTY(SET(slcSetObj))
   RETURN
ENDIF

* Store the current status of the specified SET command to the memory
* variable sgcSetList.  sgcSetList is a string that contains a list of 
* statuses that are stored until they are needed.  sgcSetList is
* initialized in the main program.
sgcSetList = sgcSetList+slcSetObj+'='+;
   IIF(SET(slcSetObj)='ON','ON ','OFF')+','

* Set the status of the SET command.
SET &slcSetObj &slcNewStat
