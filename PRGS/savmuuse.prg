******************************************************************
*      	ProMatrix Corporation                                
*                                                             
*      	Copyright © 1995-1998 ProMatrix Corporation    
*         
*      	5225 Ehrlich Road, Suite C                                
*      	Tampa, FL  33624                                        
*      	U.S.A.   
*
*	This computer program is protected by copyright law and 
*	international treaties.  No part of this program may be 
*	reproduced or transmitted in any form or by any means, 
*	electronic or mechanical, for any purpose, without the 
*	express written permission of ProMatrix Corporation.  
*	Unauthorized reproduction or distribution of this program, 
*	or any portion of it, may result in severe civil and 
*	criminal penalties, and will be prosecuted to the maximum 
*	extent possible under the law.  
*                                       
******************************************************************

***************
* S<Prefix>UUse - Open a table and assign an alias.  The table can be
***************   opened exclusively and made read-only.

PARAM slcFile,slcAlias,slcAgain,slcExcl,slcNoUpd,slcDD

* Acceptable values of parameters:
* slcFile
*    - Null, blank or no parameter (same as performing SELECT 0)
*    - Table name without path (can include extension)
*    - Table name with path (allows the opening of two tables with
*      the same name or the opening of a table outside the FoxPro path)
* slcAlias
*    - Null, blank or no parameter (alias will be the same as table name)
*    - Alias name (up to 254 characters)
* slcAgain
*    - Null, blank or no parameter (AGAIN not performed)
*    - Anything but above (will open already open table AGAIN in another
*      work area)
* slcExcl
*    - Null, blank or no parameter (EXCLUSIVE not performed)
*    - Anything but above (will open table exclusively)
* slcNoUpd
*    - Null, blank or no parameter (NOUPDATE not performed)
*    - Anything but above (will open table as read-only)
* slcDD
*    - 'PDD' (use the project's DD)
*    - 'APP' (use the ProMatrix DD)
*    - No parameter or anything but above (use the DD of the current
*      application)

PRIVATE sllAlready,sllAgain,slnDD1Rec,sllHasPath,slnDot,slnPrevWrk,sllExcl,;
   sllNoUpd,slcDD1,slnDD1Area,slcLoc,sllCloseDD1,slcDD1E,slcAltAlias,;
   slcAlready,slcAltAlready,sllSelect0

slnPrevWrk = SELECT()

* Initialize the memvar sglOpenErr.  The value can be changed by the error
* handler.  When the error handler changes sglOpenErr to .T., this means
* that control was returned to this program by means of a RETURN and not
* a RETRY.
IF TYPE('sglOpenErr') = 'U'
   PUBLIC sglOpenErr
ENDIF
sglOpenErr = .F.

* Make sure the table name has an extension.  If the FILE parameter is
* empty, this function will return after selecting an unused work area.
sllHasPath = .F.
IF EMPTY(slcFile)
   sllSelect0 = .T.
   slcFile = ''
ELSE
   sllSelect0 = .F.
   slcFile = ALLTRIM(UPPER(slcFile))
   IF siExtDotPos(slcFile) = 0
      slcFile = slcFile+'.DBF'
   ENDIF
   IF AT('\',slcFile) > 0
      sllHasPath = .T.
   ENDIF
ENDIF

* If the ALIAS parameter is empty, make the alias the same as the file name.
slcAltAlias = ''
IF NOT sllSelect0
   slcAltAlias = slcFile
   slnDot = siExtDotPos(slcAltAlias)
   IF slnDot > 0
      slcAltAlias = LEFT(slcAltAlias,slnDot-1)
   ENDIF
   slnBS = RAT('\',slcAltAlias)
   IF slnBS > 0
      slcAltAlias = SUBSTR(slcAltAlias,slnBS+1)
   ENDIF
   slcAltAlias = STRTRAN(slcAltAlias,' ','_')	&& Handle long file names
ENDIF
IF EMPTY(slcAlias)
   slcAlias = slcAltAlias
   slcAltAlias = ''
ELSE
   slcAlias = STRTRAN(slcAlias,' ','_')
   IF NOT EMPTY(slcAltAlias)
      IF slcAltAlias == ALLTRIM(UPPER(slcAlias))
         slcAltAlias = ''
      ENDIF
   ENDIF
ENDIF

* If the AGAIN parameter is not empty, the user wants to open a table
* in more than one work area.
IF EMPTY(slcAgain)
   sllAgain = .F.
ELSE
   sllAgain = .T.
   slcAltAlias = ''
ENDIF

* If the EXCLUSIVE parameter is not empty, the user wants to open the
* table exclusively.
IF EMPTY(slcExcl)
   sllExcl = .F.
ELSE
   sllExcl = .T.
ENDIF

* If the NOUPDATE parameter is not empty, the user wants to open the
* table as read-only.
IF EMPTY(slcNoUpd)
   sllNoUpd = .F.
ELSE
   sllNoUpd = .T.
ENDIF

* Determine which DD to use.
IF EMPTY(slcDD)
   IF sgcAppPre == 'APP'
      slcDD = 'APP'
   ELSE
      slcDD = 'PDD'
   ENDIF
ELSE
   IF ALLTRIM(UPPER(slcDD)) == 'APP' OR ALLTRIM(UPPER(slcDD)) == 'PDD'
      slcDD = ALLTRIM(UPPER(slcDD))
   ELSE
      IF sgcAppPre == 'APP'
         slcDD = 'APP'
      ELSE
         slcDD = 'PDD'
      ENDIF
   ENDIF
ENDIF

* Make sure the DD1 table is open.
sllCloseDD1 = .F.
slnDD1Area = 0
IF slcDD == 'APP'						&& Use the ProMatrix DD1 table
   slcDD1E = 'SAPPDD1.DBF'
ELSE									&& Use the project DD1 table
   slcDD1E = sgcPDD1
ENDIF
IF NOT EMPTY(slcDD1E) AND FILE(slcDD1E)
   slcDD1 = ALLTRIM(SUBSTR(slcDD1E,RAT('\',slcDD1E)+1))
   slnDot = siExtDotPos(slcDD1)
   IF slnDot > 0
      slcDD1 = LEFT(slcDD1,slnDot-1)
   ENDIF
   IF USED(slcDD1)
      SELECT (slcDD1)
   ELSE
      SELECT 0
      IF sglOpenErr
         = siExit('','PREVWRK')
         RETURN .F.
      ENDIF
      USE (slcDD1E) AGAIN
      IF sglOpenErr
         = siExit('','PREVWRK')
         RETURN .F.
      ENDIF
      IF NOT slcDD1 $ slcFile
         * There are times when the table to be opened (slcFile) must
         * be the only table that is opened.  This means that the DD1
         * table must not be left open if it was opened for use by 
         * this program.  It is not closed if it was already open when
         * this program was called or if it is the table to be opened
         * (slcFile).
         sllCloseDD1 = .T.
      ENDIF
   ENDIF
   slnDD1Area = SELECT()
ENDIF

* If the file has a path entered in the location field of it's DD1 
* record, add the path to the file name as long as the file name
* passed to this program did not contain a path.
IF slnDD1Area > 0
   IF EOF()
      slnDD1Rec = 0
   ELSE
      slnDD1Rec = RECNO()
   ENDIF
   IF NOT EMPTY(slcFile)
      IF NOT sllHasPath
         LOCATE FOR ALLTRIM(UPPER(file_name)) == slcFile
         IF FOUND()
            slcLoc = EVAL(sgcFDS3+"(ALLTRIM(UPPER(location)),'SUBSTITUTE')")
            IF NOT EMPTY(slcLoc)
               sllHasPath = .T.
               IF RIGHT(slcLoc,1) <> '\'
                  slcLoc = slcLoc+'\'
               ENDIF
               slcFile = slcLoc+slcFile
            ENDIF
         ENDIF
      ENDIF
   ENDIF
ENDIF

* Check to see if the table is already open.  If so, and the table is not
* going to be opened exclusively or read-only, select its work area. If 
* the table is going to be opened exclusively or read-only, close it so
* that it can be opened later.
IF NOT sllSelect0
   sllAlready = .F.
   slcAlready = ''
   slcAltAlready = ''
   IF NOT EMPTY(slcAlias)
      IF USED(slcAlias)
         sllAlready = .T.
         slcAlready = slcAlias
      ENDIF
   ENDIF 
   IF NOT EMPTY(slcAltAlias)
      IF USED(slcAltAlias)
         sllAlready = .T.
         slcAltAlready = slcAltAlias
      ENDIF
   ENDIF
   IF sllAlready
      IF (sllNoUpd AND NOT sllAgain) OR sllExcl OR EMPTY(slcAlready)
         * Close the table.
         IF NOT EMPTY(slcAlready)
            SELECT (slcAlready)
            USE
         ENDIF
         IF NOT EMPTY(slcAltAlready)
            SELECT (slcAltAlready)
            IF slnDD1Area <> SELECT()
               * Avoid closing the DD1 opened above if this program was
               * called to open DD1 with a different alias.
               USE
            ENDIF
         ENDIF
      ELSE
         IF NOT sllNoUpd
            SELECT (slcAlias)
            SET RELATION TO
            SET FILTER TO
            IF TYPE('oApp') = 'U'
               SET ORDER TO 0
               LOCATE
            ELSE
               oApp.SetOrder(ALIAS(),SET('DATASESSION'),'')
               oApp.First(SET('DATASESSION'))
            ENDIF
            = siExit('DD1POS','')
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF
ENDIF

* Select an unused work area. If so, open the table there.
SELECT 0
IF sglOpenErr
   = siExit('DD1POS','PREVWRK')
   RETURN .F.
ENDIF
IF NOT sllSelect0
   DO CASE
      CASE NOT sllExcl AND NOT sllNoUpd
         USE (slcFile) ALIAS (slcAlias) AGAIN
         IF sglOpenErr
            = siExit('DD1POS','PREVWRK')
            RETURN .F.
         ENDIF
      CASE sllExcl AND NOT sllNoUpd
         USE (slcFile) ALIAS (slcAlias) EXCLUSIVE
         IF sglOpenErr
            = siExit('DD1POS','PREVWRK')
            RETURN .F.
         ENDIF
      CASE NOT sllExcl AND sllNoUpd
         USE (slcFile) ALIAS (slcAlias) NOUPDATE AGAIN
         IF sglOpenErr
            = siExit('DD1POS','PREVWRK')
            RETURN .F.
         ENDIF
      CASE sllExcl AND sllNoUpd
         USE (slcFile) ALIAS (slcAlias) EXCLUSIVE NOUPDATE
         IF sglOpenErr
            = siExit('DD1POS','PREVWRK')
            RETURN .F.
         ENDIF
   ENDCASE
ENDIF
= siExit('DD1POS','')
RETURN .T.

***************
FUNCTION siExit
***************
LPARAM slcDD1Pos,slcPrevWrk

IF NOT EMPTY(slcDD1Pos)
   * Reposition the DD1 record pointer.
   IF slnDD1Area > 0
      IF sllCloseDD1
         USE IN (slnDD1Area)
      ELSE
         IF slnDD1Rec = 0
            GO BOTTOM IN (slnDD1Area)
            SKIP IN (slnDD1Area)
         ELSE
            GOTO slnDD1Rec IN (slnDD1Area)
         ENDIF
      ENDIF
   ENDIF
ENDIF
IF NOT EMPTY(slcPrevWrk)
   * Select the work area that was the current work area when this routine
   * was called.
   SELECT (slnPrevWrk)
ENDIF

********************
FUNCTION siExtDotPos
********************
LPARAM stcFileName

LOCAL slnDot,slcFileName

slnDot = 0
IF NOT EMPTY(stcFileName)
   slcFileName = TRIM(stcFileName)
   slnDot = RAT('.',slcFileName)
   IF slnDot > 0
      IF slnDot < LEN(slcFileName)-3
         slnDot = 0
      ENDIF
   ENDIF
ENDIF
RETURN slnDot
