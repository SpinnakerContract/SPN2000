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
* S<Prefix>ULgo - Display logo and execute additional program if one is
***************   specified.

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

IF FILE("C:\NOWATLWP.PRR") && 21Sep1998 stops logo from showing at lwp
  RETURN
ENDIF

PRIVATE slnCurArea,slcDo,slcWOnTop,slnSpaceH,slnSpaceW,slnLogoRow,;
   slnLogoCol,slcOpen

slnCurArea = SELECT()

slcWOnTop = WONTOP()
ACTIVATE SCREEN
CLEAR
IF sgcAppPre == 'APP'
   slcOpen = 'SSYSFPM.DBF'
ELSE
   slcOpen = 'S'+sgcAppPre+'FApD.APD'
ENDIF
IF EVAL(sgcUUse+"(slcOpen,'logodbf')")
   IF NOT EOF()
      IF TYPE('logodbf.logo_bmp') <> 'U' AND NOT EMPTY(logodbf.logo_bmp)
         * Display logo from BMP or ICO file.
         IF TYPE('_SCREEN.imgLogo') <> 'O'
            _SCREEN.AddObject('imgLogo','image')
         ENDIF
         _SCREEN.imgLogo.Picture = logodbf.logo_bmp
         _SCREEN.imgLogo.BackStyle = 0
         IF logodbf.logo_pos = 1
            * Center logo.
            _SCREEN.imgLogo.Top = (_SCREEN.Height-_SCREEN.imgLogo.Height)/2
            _SCREEN.imgLogo.Left = (_SCREEN.Width-_SCREEN.imgLogo.Width)/2
         ELSE
            * Display logo at user-specified coordinates.
            _SCREEN.imgLogo.Top = logodbf.logo_row
            _SCREEN.imgLogo.Left = logodbf.logo_col
         ENDIF
         _SCREEN.imgLogo.Visible = .T.
      ELSE
         * Display logo from general field.
         IF logodbf.logo_pos = 1
            * Center logo.
            @ 0,0 SAY logodbf.logo_pic ;
               CENTER ;
               STYLE 'T'
         ELSE
            * Display logo at user-specified coordinates.
            @ logodbf.logo_row,logodbf.logo_col SAY logodbf.logo_pic ;
               STYLE 'T'
         ENDIF
      ENDIF
   ENDIF
   USE
ENDIF

IF sgcAppPre == 'APP'
   slcDo = 'SSysULg2'
   DO (slcDo)
ELSE
   DO ('S'+sgcAppPre+'ULg2')
ENDIF

SELECT (slnCurArea)
IF NOT EMPTY(slcWOnTop)
   ACTIVATE WINDOW (slcWOnTop) SAME
ENDIF
