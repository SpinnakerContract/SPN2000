******************************************************************
*      	Lawson-Smith Corporation                                
*                                                             
*      	Copyright © 1994 Lawson-Smith Corporation    
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
* S<Prefix>FCT - This program executes the Admin Tool that is identified
*                in the first parameter.  The Admin Tool is executed if
*                the developer of the application has chosen to include it
*                in the application and the end user has not chosen to turn
**************   it off.

LPARAM slcID,slP1,slP2,slP3,slP4,slP5,slP6,slP7,slP8,slP9,slP10

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

LOCAL slRetVal,slnNumPar,slnElem,slcCallMV,slcCallPrg,sllDo,sllForm,slnDot

slRetVal = .T.					&& Default return value
slnNumPar = PARAMETERS()
IF slnNumPar > 0
   = EVAL(sgcUPsh+"('EXACT','OFF')")
   slnElem = ASCAN(sgaTools,ALLTRIM(UPPER(slcID)))
   = EVAL(sgcUPop+"('EXACT')")
   IF slnElem > 0
      * If an ID was found in the array sgaTools, this means that the
      * admin tool was chosen to be "included" in the application by the
      * developer.
      IF sgaTools(slnElem+2)
         * Tool is set "on" by user.
         slcCallMV = sgaTools(slnElem+1)
         IF NOT EMPTY(slcCallMV)
            slcCallPrg = ALLTRIM(&slcCallMV)
            IF NOT EMPTY(slcCallPrg)
               sllForm = .F.
               slnDot = AT('.',slcCallPrg)
               IF slnDot > 0
                  IF UPPER(SUBSTR(slcCallPrg,slnDot+1)) == 'SPR'
                     slcCallPrg = LEFT(slcCallPrg,slnDot-1)
                     sllForm = .T.
                  ENDIF
                  sllDo = .T.				&& Call as procedure
               ELSE
                  sllDo = .F.				&& Call as function
               ENDIF
               DO CASE
                  CASE slnNumPar = 1
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg)
                        ELSE
                           DO (slcCallPrg)
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg()
                     ENDIF
                  CASE slnNumPar = 2
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1
                        ELSE
                           DO (slcCallPrg) WITH slP1
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1)
                     ENDIF
                  CASE slnNumPar = 3
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2)
                     ENDIF
                  CASE slnNumPar = 4
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2,slP3
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2,slP3
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2,slP3)
                     ENDIF
                  CASE slnNumPar = 5
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2,slP3,slP4
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2,slP3,slP4
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2,slP3,slP4)
                     ENDIF
                  CASE slnNumPar = 6
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2,slP3,slP4,;
                              slP5
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2,slP3,slP4,slP5
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2,slP3,slP4,slP5)
                     ENDIF
                  CASE slnNumPar = 7
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2,slP3,slP4,;
                              slP5,slP6
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2,slP3,slP4,slP5,;
                              slP6
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2,slP3,slP4,slP5,;
                           slP6)
                     ENDIF
                  CASE slnNumPar = 8
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2,slP3,slP4,;
                              slP5,slP6,slP7
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2,slP3,slP4,slP5,;
                              slP6,slP7
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2,slP3,slP4,slP5,;
                           slP6,slP7)
                     ENDIF
                  CASE slnNumPar = 9
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2,slP3,slP4,;
                              slP5,slP6,slP7,slP8
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2,slP3,slP4,slP5,;
                              slP6,slP7,slP8
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2,slP3,slP4,slP5,;
                           slP6,slP7,slP8)
                     ENDIF
                  CASE slnNumPar = 10
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2,slP3,slP4,;
                              slP5,slP6,slP7,slP8,slP9
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2,slP3,slP4,slP5,;
                              slP6,slP7,slP8,slP9
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2,slP3,slP4,slP5,;
                           slP6,slP7,slP8,slP9)
                     ENDIF
                  OTHERWISE
                     IF sllDo
                        IF sllForm
                           DO FORM (slcCallPrg) WITH slP1,slP2,slP3,slP4,;
                              slP5,slP6,slP7,slP8,slP9,slP10
                        ELSE
                           DO (slcCallPrg) WITH slP1,slP2,slP3,slP4,slP5,;
                              slP6,slP7,slP8,slP9,slP10
                        ENDIF
                     ELSE
                        slRetVal = &slcCallPrg(slP1,slP2,slP3,slP4,slP5,;
                           slP6,slP7,slP8,slP9,slP10)
                     ENDIF
               ENDCASE
            ENDIF
         ENDIF
      ENDIF
   ENDIF
ENDIF

RETURN slRetVal
