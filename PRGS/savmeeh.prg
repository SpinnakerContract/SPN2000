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
* S<Prefix>EEH - Error handling routine (called by ON ERROR command).
**************

PARAM slnErrNum,slcMess,slcMessPar,slcCode,slcProg16,slcProg,slnCurLine,;
   slnLastKey,slcVarRead,slcWOnTop

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

#INCLUDE 'MAIN.H'

* Parameters:
* slnErrNum  - ERROR()
* slcMess    - MESSAGE()
* slcMessPar - SYS(2018)
* slcCode    - MESSAGE(1)
* slcProg16  - SYS(16)
* slcProg    - PROGRAM()
* slnCurLine - LINENO()
* slnLastKey - LASTKEY()
* slcVarRead - VARREAD()
* slcWOnTop  - WONTOP()

* Out-of-memory errors must be handled before any more memory is used.
DO CASE
   CASE INLIST(slnErrNum,21,22)
      = siFreeMem()
      CLEAR TYPEAHEAD
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      SET CURSOR ON
      ?? CHR(7)
      WAIT WINDOW 'Too many memory variables.  Press any key.'
      = siDispErr()
      IF TYPE('oApp') <> 'O' OR TYPE('sglErrQuit') = 'U' OR sglErrQuit
         * Make sure the user is recorded as being logged-off.
         IF TYPE('oSec') = 'O'
            oSec.LogOn_LogOff('OFF')
         ENDIF
         ON SHUTDOWN
         QUIT
      ELSE
         IF oApp.Cleanup()
            sgcMenu = 'QUIT'
            RELEASE oApp,sglNoValid,sglGridFocus
            slcMain = 'S'+sgcAppPre+'FMAN'
            RETURN TO &slcMain
         ENDIF
      ENDIF
   CASE INLIST(slnErrNum,43,1149,1150,1151,1250,1600,1809,1845)
      = siFreeMem()
      CLEAR TYPEAHEAD
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      SET CURSOR ON
      ?? CHR(7)
      WAIT WINDOW 'Not enough memory available for the current process.  '+;
         'Press any key.'
      = siDispErr()
      IF TYPE('oApp') <> 'O' OR TYPE('sglErrQuit') = 'U' OR sglErrQuit
         * Make sure the user is recorded as being logged-off.
         IF TYPE('oSec') = 'O'
            oSec.LogOn_LogOff('OFF')
         ENDIF
         ON SHUTDOWN
         QUIT
      ELSE
         IF oApp.Cleanup()
            sgcMenu = 'QUIT'
            RELEASE oApp,sglNoValid,sglGridFocus
            slcMain = 'S'+sgcAppPre+'FMAN'
            RETURN TO &slcMain
         ENDIF
      ENDIF
ENDCASE

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

PRIVATE slcConsole,slcAction,sllNoLog,slcSetDev,slcSetPrt,slcCursor,;
   sllUseWait,slcAlert,slcOnError,slcAddInfo,sllUndefQuit,slcMain

* Turn off the ON ERROR call to this program during the execution of this
* program.
slcOnError = ON('ERROR')
ON ERROR
slcConsole = SYS(100)				&& Console setting prior to ON ERROR

* Some errors should be ignored.
IF INLIST(slnErrNum,104,181,182,279,1164,1726,1539,1582,1583,1884) OR ;
   (slnErrNum = 91 AND 'RELEASE' $ slcCode)
   SET CONSOLE &slcConsole
   ON ERROR &slcOnError
   RETURN
ENDIF

* Set the environment for the remaining errors.
sllNoLog = .F.
sllUndefQuit = .F.
sllUseWait = .F.
slcAddInfo = ''
slcSetDev = SYS(101)
slcSetPrt = SYS(102)
slcCursor = SET('CURSOR')
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CURSOR ON
CLEAR TYPEAHEAD

* If any of the global memory variables used by this program are undefined,
* exit the application without logging the error.
IF TYPE('oSec') <> 'O' OR TYPE('sgcCurPre') = 'U' OR ;
   TYPE('sgcErrDBF') = 'U' OR TYPE('sgcUPsh') = 'U' OR ;
   TYPE('sgcUPop') = 'U' OR TYPE('sgcAppPre') = 'U' OR ;
   TYPE('sgcUUse') = 'U'
   sllNoLog = .T.
   sllUndefQuit = .T.
ENDIF

IF TYPE('oApp') = 'O' AND NOT oApp.lValidPassed
   oApp.lValidPassed = .T.
ENDIF

* If the global memory variable sgcAppTitle is missing, display the error
* information in a wait window before exiting the application.
IF TYPE('sgcAppTitle') = 'U'
   sllUseWait = .T.
   sllUndefQuit = .T.
ENDIF

IF (TYPE('sgcAppPre') <> 'U' AND sgcAppPre = 'APP') OR ;
   TYPE('slcMemPre') <> 'U'
   * In ProMatrix and while testing an application, display additional
   * error information.
   slcAddInfo = CHR(13)+CHR(13)+;
      'ERROR #: '+ALLTRIM(STR(slnErrNum))+CHR(13)+;
      'MESSAGE: '+slcMess+CHR(13)+;
      'PROGRAM: '+slcProg16+CHR(13)+;
      'LINE #: '+ALLTRIM(STR(slnCurLine))+CHR(13)+;
      'CODE: '+slcCode
ENDIF

* Handle the remaining errors.
DO CASE
   CASE INLIST(slnErrNum,1,1162,1802)
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'The file '+ALLTRIM(UPPER(slcMessPar))+' cannot be '+;
            'found.  Press any key.'+slcAddInfo
      ELSE
         = MESSAGEBOX('The file '+ALLTRIM(UPPER(slcMessPar))+' cannot be '+;
            'found.'+slcAddInfo,MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'MAIN MENU'
   CASE INLIST(slnErrNum,3,108,1106,1503)
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'File is being used by someone else.  Try again? '+;
            '(Y/N)'+slcAddInfo TO slcAlert
         IF UPPER(slcAlert) == 'Y'
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ELSE
         IF MESSAGEBOX('A file needed by this program is being used '+;
            'by someone else.  You may try to use the file again.  Try '+;
            'again?'+slcAddInfo,MB_ICONQUESTION+MB_YESNO,sgcAppTitle) =;
            IDYES
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ENDIF
   CASE slnErrNum = 6
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'Too many files open.  Press any key.'+slcAddInfo
      ELSE
         = MESSAGEBOX('Too many files open.  This may be caused by the '+;
            'config.sys FILES setting not being high enough.'+slcAddInfo,;
            MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'MAIN MENU'
   CASE INLIST(slnErrNum,19,20,114,1141,1683)
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'There is a problem with an index.  Press any key.'+;
            slcAddInfo
      ELSE
         = MESSAGEBOX('There is a problem with an index you are attempting '+;
            'to use.  Try recreating the indexes used in the current '+;
            'process.'+slcAddInfo,MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'MAIN MENU'
   CASE slnErrNum = 56
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'Out of disk space.  Press any key.'+slcAddInfo
      ELSE
         = MESSAGEBOX('Out of disk space.  This application must be '+;
            'terminated.'+slcAddInfo,MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'QUIT'
      sllNoLog = .T.
   CASE INLIST(slnErrNum,109,130,1502)
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'Record is being used by someone else.  Try '+;
            'again? (Y/N)'+slcAddInfo TO slcAlert
         IF UPPER(slcAlert) == 'Y'
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ELSE
         IF MESSAGEBOX('The record you need is already being used '+;
            'by someone else.  You may try to use the record again.  '+;
            'Try again?'+slcAddInfo,MB_ICONQUESTION+MB_YESNO,sgcAppTitle) =;
            IDYES
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ENDIF
   CASE slnErrNum = 124
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'Problem printing to the designated printer.  Press '+;
            'any key.'
      ELSE
         = MESSAGEBOX('Problem printing to the designated printer.  Either '+;
            'a path to the printer has not been established or the '+;
            'printer cannot be shared.',MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'RETURN'
   CASE slnErrNum = 125
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'Printer not ready.  Try again? (Y/N)' TO slcAlert
         IF UPPER(slcAlert) == 'Y'
            slcAction = 'RETRY'
         ELSE
            slcAction = 'RETURN'
         ENDIF
      ELSE
         IF MESSAGEBOX('Printer not ready.  You may ready the printer and '+;
            'try again.  Try again?',MB_ICONQUESTION+MB_YESNO,sgcAppTitle) ;
            = IDYES
            slcAction = 'RETRY'
         ELSE
            slcAction = 'RETURN'
         ENDIF
      ENDIF
   CASE slnErrNum = 1104
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'An error occurred while trying to use a file.  '+;
            'Try again? (Y/N)'+slcAddInfo TO slcAlert
         IF UPPER(slcAlert) == 'Y'
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ELSE
         IF MESSAGEBOX('An error was returned by the operating '+;
            'system while trying to use a file.  This might be a '+;
            'temporary problem, and the error may not reoccur if you try '+;
            'to use the file again.  If the error does reoccur you may '+;
            'have a serious problem.  You may try to use the file again.'+;
            '  Try again?'+slcAddInfo,MB_ICONQUESTION+MB_YESNO,;
            sgcAppTitle) = IDYES
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ENDIF
   CASE slnErrNum = 1105
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'An error occurred while trying to write to a file.'+;
            '  Try again? (Y/N)'+slcAddInfo TO slcAlert
         IF UPPER(slcAlert) == 'Y'
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
            sllNoLog = .T.
         ENDIF
      ELSE
         IF MESSAGEBOX('An error was returned by the operating '+;
            'system while trying to write to a file.  This might be a '+;
            'temporary problem, and the error may not reoccur if you '+;
            'make another try to write to the file.  If the error does '+;
            'reoccur, you may have a serious problem or you may simply '+;
            'be attempting to write to a write-protected diskette or to '+;
            'a LAN directory for which you do not have proper '+;
            'authorization.  You may try to write to the file again.  '+;
            'Try again?'+slcAddInfo,MB_ICONQUESTION+MB_YESNO,;
            sgcAppTitle) = IDYES
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
            sllNoLog = .T.
         ENDIF
      ENDIF
   CASE slnErrNum = 1112
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'An error occurred while trying to close a file.  '+;
            'Try again? (Y/N)'+slcAddInfo TO slcAlert
         IF UPPER(slcAlert) == 'Y'
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ELSE
         IF MESSAGEBOX('An error was returned by the operating '+;
            'system while trying to close a file.  This might be a '+;
            'temporary problem, and the error may not reoccur if you '+;
            'make another attempt to close the file.  If the error does '+;
            'reoccur you may have a serious problem.  You may try to '+;
            'close the file again.  Try again?'+slcAddInfo,;
            MB_ICONQUESTION+MB_YESNO,sgcAppTitle) = IDYES
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ENDIF
   CASE slnErrNum = 1705
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'File is not currently available.  Try again? (Y/N)'+;
            slcAddInfo TO slcAlert
         IF UPPER(slcAlert) == 'Y'
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ELSE
         IF MESSAGEBOX('A file you are trying to use is currently '+;
            'not available.  The file may be available if you try to '+;
            'use it again.  Try again?'+slcAddInfo,MB_ICONQUESTION+;
            MB_YESNO,sgcAppTitle) = IDYES
            slcAction = 'RETRY'
         ELSE
            slcAction = 'MAIN MENU'
         ENDIF
      ENDIF
   CASE slnErrNum = 1707
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'Structural index file is missing.  Press any key.'+;
            slcAddInfo
      ELSE
         = MESSAGEBOX('The structural index for a file you are attempting '+;
            'to use is missing.  This process cannot be run without this '+;
            'index.'+slcAddInfo,MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'MAIN MENU'
   CASE slnErrNum = 1831
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'Error building a temporary index.  Press any key.'+;
            slcAddInfo
      ELSE
         = MESSAGEBOX('An error has occurred while building a temporary '+;
            'index for a query.  The cause may be insufficient disk '+;
            'space.  This application must be terminated.'+slcAddInfo,;
            MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'QUIT'
      sllNoLog = .T.
   CASE slnErrNum = 1839
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'Are you sure you want to cancel the query? (Y/N)';
            TO slcAlert
         IF UPPER(slcAlert) == 'Y'
            slcAction = 'MAIN MENU'
         ELSE
            slcAction = 'RETRY'
         ENDIF
      ELSE
         IF MESSAGEBOX('You have attempted to cancel the current '+;
            'query.  You may cancel the query and return to the main '+;
            'menu or continue with the query.  Cancel the query?',;
            MB_ICONQUESTION+MB_YESNO,sgcAppTitle) = IDYES
            slcAction = 'MAIN MENU'
         ELSE
            slcAction = 'RETRY'
         ENDIF
      ENDIF
   CASE INLIST(slnErrNum,55,102,1002,1102,1157,1294,1296,1410,1462,1800)
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'A nonrecoverable error has occurred.  Press any '+;
            'key.'+slcAddInfo
      ELSE
         = MESSAGEBOX('A nonrecoverable program or data error has '+;
            'occurred.  Because of the nature of the error, this '+;
            'application must be terminated.'+slcAddInfo,;
            MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'QUIT'
   OTHERWISE
      IF sllUseWait
         ?? CHR(7)
         WAIT WINDOW 'A nonrecoverable error has occurred.  Press any '+;
            'key.'+slcAddInfo
      ELSE
         = MESSAGEBOX('A nonrecoverable program or data error has '+;
            'occurred.  Because of the nature of the error, you will be '+;
            'returned to the main menu.'+slcAddInfo,;
            MB_ICONEXCLAMATION+MB_OK,sgcAppTitle)
      ENDIF
      slcAction = 'MAIN MENU'
ENDCASE

IF slcAction = 'MAIN MENU' AND SUBSTR(slcProg,5,4) == 'UUSE'
   * If the error occurred in the file opening routine S<Prefix>UUse, don't
   * return to the main menu.  Instead, return to S<Prefix>UUse after 
   * setting sglOpenErr to .T..
   slcAction = 'RETURN'
   sglOpenErr = .T.
   = siLogErr()
ELSE
   IF TYPE('sglErrQuit') = 'U' OR (sglErrQuit AND slcAction = 'MAIN MENU')
      * If the error occurred prior to the user gaining access to the 
      * application, they must be exited from the application.
      slcAction = 'QUIT'
   ENDIF
ENDIF

DO CASE
   CASE slcAction = 'QUIT' OR sllUndefQuit
      IF NOT sllNoLog
         = siLogErr()
      ENDIF
      * Make sure the user is recorded as being logged-off.
      IF NOT sllUndefQuit
         oSec.LogOn_LogOff('OFF')
      ENDIF
      IF TYPE('oApp') <> 'O' OR (TYPE('sglErrQuit') <> 'U' AND sglErrQuit)
         * Error occurred prior to instantiation of application object
         * oApp.
         ON SHUTDOWN
         QUIT
      ELSE
         * sgcMenu is used in the application's main program to decide
         * what action should take place next.
         IF oApp.Cleanup()
            sgcMenu = 'QUIT'
            RELEASE oApp,sglNoValid,sglGridFocus
            IF TYPE('sgcAppPre') <> 'U'
               slcMain = 'S'+sgcAppPre+'FMAN'
               RETURN TO &slcMain
            ELSE
               RETURN TO MASTER
            ENDIF
         ENDIF
      ENDIF
   CASE slcAction = 'RETRY'
      SET DEVICE TO &slcSetDev
      SET PRINTER &slcSetPrt
      SET CONSOLE &slcConsole
      SET CURSOR &slcCursor
      ON ERROR &slcOnError
      RETRY
   CASE slcAction = 'RETURN'
      SET DEVICE TO &slcSetDev
      SET PRINTER &slcSetPrt
      SET CONSOLE &slcConsole
      SET CURSOR &slcCursor
      ON ERROR &slcOnError
      RETURN
   CASE slcAction = 'MAIN MENU'
      IF NOT sllNoLog
         = siLogErr()
      ENDIF
      ON ERROR &slcOnError
      IF TYPE('oApp') <> 'O'
         ON SHUTDOWN
         QUIT
      ELSE
         IF oApp.Cleanup()
            RELEASE oApp,sglNoValid,sglGridFocus
            slcMain = 'S'+sgcAppPre+'FMAN'
            RETURN TO &slcMain
         ENDIF
      ENDIF
ENDCASE

******************
FUNCTION siFreeMem
******************
* Free up some memory.
CLEAR WINDOWS
CLEAR POPUPS
CLEAR MENUS
CLOSE ALL

*****************
FUNCTION siLogErr
*****************
LOCAL slcTemp1,slcTemp2,slnCurArea,sllOK

slcTemp1 = SYS(3)+'.TMP'
LIST MEMORY TO (slcTemp1) NOCONSOLE
slcTemp2 = SYS(3)+'.TMP'
LIST STATUS TO (slcTemp2) NOCONSOLE
slnCurArea = SELECT()
IF EVAL(sgcUUse+"(sgcErrDBF)")
   sllOK = .T.
   IF TYPE('oApp') = 'O'
      IF NOT oApp.AddRecord(ALIAS(),SET('DATASESSION'))
         sllOK = .F.
      ENDIF
   ELSE
      = EVAL(sgcUPsh+"('DELETED','OFF')")
      LOCATE FOR DELETED()
      IF FOUND()
         RECALL
      ELSE
         APPEND BLANK
      ENDIF
      = EVAL(sgcUPop+"('DELETED')")
   ENDIF
   IF sllOK
      REPLACE user WITH oSec.cCurrentUserID,;
              prefix WITH sgcCurPre,;
              err_date WITH DATE(),;
              err_time WITH TIME(),;
              err_no WITH slnErrNum,;
              err_mess WITH slcMess,;
              cur_prog WITH slcProg16,;
              line_no WITH slnCurLine,;
              line_code WITH slcCode,;
              top_window WITH slcWOnTop,;
              get_field WITH slcVarRead,;
              last_key WITH slnLastKey
      APPEND MEMO cur_memory FROM (slcTemp1)
      APPEND MEMO cur_status FROM (slcTemp2)
   ENDIF
   USE
   SELECT (slnCurArea)
ENDIF
ERASE (slcTemp1)
ERASE (slcTemp2)

******************
FUNCTION siDispErr
******************
CLEAR TYPEAHEAD
?? CHR(7)
WAIT WINDOW 'Error '+ALLTRIM(STR(slnErrNum))+' has occurred in program '+;
   ALLTRIM(slcProg)+', line '+ALLTRIM(STR(slnCurLine))+'.  Press any key.'
