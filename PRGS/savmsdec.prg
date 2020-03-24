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
* S<Prefix>SDec - This program is called from the Users and Passwords
*                 reports to decrypt information printed from the Users
***************   table.

LPARAM stcString

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

LOCAL slcReturn,slnPrefix,slnStrLen,slnStrLen7,slnFor,slnPos7

slcReturn = ''
slnPrefix = INT(ASC(RIGHT(sgcAppPre,1))/10)
slnStrLen = LEN(stcString)
slnStrLen7 = slnStrLen%7+1
* Decrypt string character by character.
FOR slnFor = 1 TO slnStrLen
   slnPos7 = slnFor%7+1
   slcReturn = slcReturn+CHR(ASC(SUBSTR(stcString,slnFor,1));
      -slnPos7-slnStrLen7+slnPrefix)
ENDFOR

* Return a string that is the same length as the string that was passed
* to this method.
RETURN slcReturn
