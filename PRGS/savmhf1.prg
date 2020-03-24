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
* S<Prefix>HF1 - Bring up the Help window.
**************

#INCLUDE 'MAIN.H'

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

LOCAL slcWOnTop,slcSubject

IF SET('HELP') = 'OFF'
   = MESSAGEBOX('Help not available.',MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
   RETURN
ENDIF

IF NOT 'FOXHELP' $ SET('HELP',1)
   * Identify the Subject lookup value.
   slcWOnTop = ALLTRIM(WONTOP())
   slcSubject = ''
   IF NOT EMPTY(slcWOnTop)
      DO CASE
         CASE slcWOnTop = 'P0SSYSODO1'
            slcSubject = 'Object Builder'
         CASE NOT EMPTY(WTITLE())
            slcSubject = ALLTRIM(WTITLE())
      ENDCASE
   ENDIF
   IF NOT EMPTY(slcSubject)
      DO CASE
         CASE UPPER(SUBSTR(slcSubject,1,9)) = 'PROMATRIX'
            DO CASE
               CASE UPPER(SUBSTR(slcSubject,11,6)) = 'DATA M'
                  slcSubject = 'Building Data'
               CASE UPPER(SUBSTR(slcSubject,11,6)) = 'REPORT'
                  slcSubject = 'Report Manager'
            ENDCASE
         CASE UPPER(slcSubject) = 'DATA BUILDER'
            slcSubject = 'Building Data'
         CASE UPPER(slcSubject) = 'DATA MANAGER'
            slcSubject = 'Building Data'
         CASE UPPER(slcSubject) = 'REPORT BUILDER'
            slcSubject = 'Report Manager'
      ENDCASE
   ENDIF

   * Bring up the Help window with the specified topic
   SET TOPIC TO slcSubject
ENDIF

HELP
