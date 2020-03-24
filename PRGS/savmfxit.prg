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
* S<Prefix>FXit - Called from the Exit , Exit to FoxPro and Run Application
***************   menu options.

LPARAM stcAction

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

#INCLUDE 'MAIN.H'

LOCAL slcChild,slcMain

IF sgcAppPre = 'APP'					&& Program run from ProMatrix
   slcChild = WCHILD('',0)
   DO WHILE NOT EMPTY(slcChild)
      IF ALLTRIM(UPPER(slcChild)) = SYSTEMWINDOW_PM
         = MESSAGEBOX('Cannot exit Visual ProMatrix while the Project '+;
            'Manager is active.  Close the Project Manager before exiting.',;
            MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
         RETURN
      ENDIF
      slcChild = WCHILD('',1)
   ENDDO
   IF ALLTRIM(UPPER(stcAction)) == 'QUIT' AND sglQuitConf
      IF MESSAGEBOX('Exit Visual ProMatrix?',MB_ICONQUESTION+MB_YESNO,;
         sgcAppTitle) = IDNO
         RETURN
      ENDIF
   ENDIF
ELSE									&& Program run from End Application
   IF TYPE('slcMemPre') = 'U'			&& Runtime mode
      IF ALLTRIM(UPPER(stcAction)) == 'QUIT' AND sglQuitConf
         IF MESSAGEBOX('Exit this application?',MB_ICONQUESTION+MB_YESNO,;
            sgcAppTitle) = IDNO
            RETURN
         ENDIF
      ENDIF
   ELSE									&& Test mode
      IF sglQuitConf
         IF MESSAGEBOX('Return to Visual ProMatrix?',MB_ICONQUESTION+;
            MB_YESNO,sgcAppTitle) = IDNO
            RETURN
         ENDIF
      ENDIF
   ENDIF
ENDIF

* sgcMenu is used in the application's main program to decide what action
* should take place next.
sgcMenu = stcAction

IF oApp.Cleanup()
   RELEASE oApp
   slcMain = 'S'+sgcAppPre+'FMAN'
   RETURN TO &slcMain
ENDIF
