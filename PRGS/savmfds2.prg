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
* S<Prefix>FDS2 - Load the drive swapping array sgaDrvSwap. The array
***************   sgaDrvSwap is initialized in the main program.

LPARAM stnPass

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

#INCLUDE 'MAIN.H'

LOCAL slcDrvSwap,slnNumSwap,slnCurArea

IF stnPass = 1

   * 1st Pass - This program is first run at the beginning of the main
   * program.  Some system tables are opened before the user logs on, so
   * load the array here if opening system tables requires drive swapping
   * or the loading of the array does not require the knowledge of which
   * user is logging on.

   * The following is an example of building the drive swapping array from
   * a table.  The table has two fields, "stored" and "substitute".  The
   * first field contains the drive letter as stored in VPM tables.  The
   * second field contains the drive letter for the same drive as designated
   * on the current computer.  To use this method, assign the name of the
   * drive swapping table to the memvar in the following line.
   slcDrvSwap = ''
   IF NOT EMPTY(slcDrvSwap)
      IF FILE(slcDrvSwap)
         slnCurArea = SELECT()
         IF EVAL(sgcUUse+"(slcDrvSwap,'drvswap')")
            slnNumSwap = 0
            SCAN FOR NOT EMPTY(drvswap.stored) AND ;
               NOT EMPTY(drvswap.substitute)
               slnNumSwap = slnNumSwap+1
               DIMENSION sgaDrvSwap(slnNumSwap,2)
               sgaDrvSwap(slnNumSwap,1) = drvswap.stored
               sgaDrvSwap(slnNumSwap,2) = drvswap.substitute
            ENDSCAN
            USE
            SELECT (slnCurArea)
         ELSE
            = MESSAGEBOX('Unable to open the Drive Swapping table.  Drive '+;
               'swapping will not take place.',MB_ICONEXCLAMATION+MB_OK,;
               sgcAppTitle)
         ENDIF
      ELSE
         = MESSAGEBOX('The Drive Swapping table does not exist.  Drive '+;
            'swapping will not take place.',MB_ICONEXCLAMATION+MB_OK,;
            sgcAppTitle)
      ENDIF
   ENDIF

ELSE

   * 2nd Pass - This program is run the second time from the main program
   * after the user has logged-on.  Load the array here if opening 
   * application tables requires drive swapping and knowledge of which
   * user has logged-on is required to load the array.

ENDIF
