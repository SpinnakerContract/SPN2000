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
* S<Prefix>UAl - Displays alert window with user-defined text and push
**************   buttons.

PARAM slcText,slcHeading,slcPB,sllBell,slcTitle,slnPBInit,slcQEI,slnClose

_REFOX_ = (9876543210)
_REFOX_ = (9876543210)

* Parameters:
*
* slcText    - Message to be displayed.
* slcHeading - Heading to be displayed.
* slcPB      - Push button definition.  Defaults to Yes and No.
* sllBell    - .T. means sound bell.  Defaults to .F.
* slcTitle   - Title of alert window.
* slnPBInit  - Number of the push button to be initially selected. 
*              Defaults to 1.
* slcQEI     - 'Q' means display question mark. 'E' means display
*              exclamation point.  'I' means display information
*              picture.  Defaults to question mark.
* slnClose   - Number to be returned when user selects Close from the
*              Control Box.  The number relates to one of the push buttons.
*              If this parameter is not passed, a Close option is not added
*              to the Control Box.

PRIVATE slcPBStr,slnPBLen,slnPBNum,slnComma,sllMore,slnPos1,slnPos2,;
   slcStr,slnStrLen,slnRealLen,slnFor,slnPBLine,slnHeading,slnText,;
   slnMaxLen,slnMaxLine,slnNumElem,slcSubText,slnPosSp,slaAlert,slnTotLine,;
   slnAddLine,slnPBSel,slcTitle2,slnPipe,slnLongest,slnLineLen,slnAddMLine,;
   slcPBLabel,slnLast,slcPassText,slcDispQEI
   
CLEAR TYPEAHEAD
PUSH KEY CLEAR

* Set up default values if all parameters are not passed.
DO CASE
   CASE PARAMETERS() = 0
      slcText = ''
      slcHeading = ''
      slcPB = '\!\<Yes,\?\<No'
      sllBell = .F.
      slcTitle2 = ''
      slnPBInit = 1
      slcDispQEI = 'Q'
      slnClose = 0
   CASE PARAMETERS() = 1
      slcHeading = ''
      slcPB = '\!\<Yes,\?\<No'
      sllBell = .F.
      slcTitle2 = ''
      slnPBInit = 1
      slcDispQEI = 'Q'
      slnClose = 0
   CASE PARAMETERS() = 2
      slcPB = '\!\<Yes,\?\<No'
      sllBell = .F.
      slcTitle2 = ''
      slnPBInit = 1
      slcDispQEI = 'Q'
      slnClose = 0
   CASE PARAMETERS() = 3
      sllBell = .F.
      slcTitle2 = ''
      slnPBInit = 1
      slcDispQEI = 'Q'
      slnClose = 0
   CASE PARAMETERS() = 4
      slcTitle2 = ''
      slnPBInit = 1
      slcDispQEI = 'Q'
      slnClose = 0
   CASE PARAMETERS() = 5
      slcTitle2 = slcTitle
      slnPBInit = 1
      slcDispQEI = 'Q'
      slnClose = 0
   CASE PARAMETERS() = 6
      slcTitle2 = slcTitle
      slcDispQEI = 'Q'
      slnClose = 0
   CASE PARAMETERS() = 7
      slcTitle2 = slcTitle
      IF NOT slcQEI == 'Q' AND NOT slcQEI == 'E' AND NOT slcQEI == 'I'
         slcDispQEI = 'Q'
      ELSE
         slcDispQEI = slcQEI
      ENDIF
      slnClose = 0
   CASE PARAMETERS() = 8
      slcTitle2 = slcTitle
      IF NOT slcQEI == 'Q' AND NOT slcQEI == 'E' AND NOT slcQEI == 'I'
         slcDispQEI = 'Q'
      ELSE
         slcDispQEI = slcQEI
      ENDIF
ENDCASE
IF EMPTY(slcTitle2)
   slcTitle2 = sgcAppTitle
ENDIF

* Create push button definition string.
slcPB = ALLTRIM(slcPB)
IF EMPTY(slcPB)
   slcPB = '\!\<Yes,\?\<No'
ENDIF
slcPB = ','+slcPB+','
slcPBStr = '@*HT '
slnPBLen = 0
slnPBNum = 0
slnComma = 0
sllMore = .T.
DO WHILE sllMore
   slnComma = slnComma+1
   slnPos1 = AT(',',slcPB,slnComma)
   slnPos2 = AT(',',slcPB,slnComma+1)
   IF slnPos2 = 0
      EXIT
   ENDIF
   slcStr = ALLTRIM(SUBSTR(slcPB,slnPos1+1,slnPos2-slnPos1-1))
   IF NOT EMPTY(slcStr)
      slnStrLen = LEN(slcStr)
      slcPBLabel = ''
      FOR slnFor = 1 TO slnStrLen
         IF NOT SUBSTR(slcStr,slnFor,1) $ '!?\<'
            slcPBLabel = slcPBLabel+SUBSTR(slcStr,slnFor,1)
         ENDIF
      ENDFOR
      IF NOT EMPTY(slcPBLabel)
         slnPBNum = slnPBNum+1
         IF slnPBNum = 1
            slcPBStr = slcPBStr+slcStr
         ELSE
            slcPBStr = slcPBStr+';'+slcStr
         ENDIF
         slnRealLen = TXTWIDTH(slcPBLabel,'Arial',9,'B')
         IF slnRealLen > slnPBLen
            slnPBLen = slnRealLen
         ENDIF
      ENDIF
   ENDIF
ENDDO
IF slnPBNum = 0
   slcPBStr = '@*HT \!\<Yes;\?\<No'
   slnPBLen = 8
   slnPBNum = 2
ENDIF

slnPBLen = slnPBLen+6			&& Add blank space on each side of PB label
IF slnPBNum = 1
   slnPBSpace = 1
ELSE
   slnPBSpace = (70-slnPBNum*slnPBLen)/(slnPBNum-1)
   IF slnPBSpace < 0
      slnPBSpace = 1
   ELSE
      IF slnPBSpace > 6
         slnPBSpace = 6
      ENDIF
   ENDIF
ENDIF
slnPBLine = slnPBNum*slnPBLen+(slnPBNum-1)*slnPBSpace
IF slnPBLine > 70
   slnPBLine = 70
ENDIF

slcHeading = ALLTRIM(slcHeading)
slnHeading = TXTWIDTH(slcHeading,'Arial',9,'B')
IF slnHeading > 70							&& Max heading length is 70
   slnHeading = 70
ENDIF

slcText = ALLTRIM(slcText)
slnText = TXTWIDTH(slcText,'Arial',9,'B')
IF slnText > 59								&& Max text length is 59
   slnText = 59
ENDIF

* Determine the length of the lines to be displayed in the alert window
* and the maximum number of lines.  The maximum line length is 70 characters
* and the maximum number of lines is 12 (14 if no heading).  The line length
* is determined by taking the longest of the heading, text (plus question
* mark picture (11 char)) and push button lines with a minimum length of
* 40 characters.
IF slnHeading > slnText+11
   slnMaxLen = slnHeading
ELSE
   slnMaxLen = slnText+11
ENDIF
IF slnPBLine > slnMaxLen
   slnMaxLen = slnPBLine
ENDIF
IF slnMaxLen < 40
   slnMaxLen = 40
ENDIF
IF slnHeading = 0
   slnMaxLine = 14
ELSE
   slnMaxLine = 12
ENDIF

* Create an array that contains the lines of the message text.  The 
* character '|' in the text will act as a line feed.
slnNumElem = 0
slnLongest = 0
DO WHILE LEN(slcText) > 0
   slnPipe = AT('|',slcText)
   IF slnPipe > 0
      slcPassText = LEFT(slcText,slnPipe-1)
   ELSE
      slcPassText = slcText
   ENDIF
   slnLast = siReturn(slcPassText,slnMaxLen-11)
   DO CASE
      CASE slnLast < 0
         slcSubText = LEFT(slcPassText,ABS(slnLast))
         slcText = SUBSTR(slcText,ABS(slnLast)+1)
      CASE slnLast = 0
         slcSubText = slcPassText
         IF slnPipe = 0
            slcText = ''
         ELSE
            slcText = ALLTRIM(SUBSTR(slcText,slnPipe+1))
         ENDIF
      OTHERWISE
         slcSubText = LEFT(slcPassText,slnLast)
         slcText = ALLTRIM(SUBSTR(slcText,slnLast+1))
   ENDCASE
   slnNumElem = slnNumElem+1
   DIMENSION slaAlert(slnNumElem)
   slaAlert(slnNumElem) = ALLTRIM(slcSubText)
   slnLineLen = TXTWIDTH(slaAlert(slnNumElem),'Arial',9,'B')
   IF slnLineLen > slnLongest
      slnLongest = slnLineLen
   ENDIF
   IF slnNumElem = slnMaxLine
      EXIT
   ENDIF
ENDDO

* Determine the number of text and heading lines.
DO CASE
   CASE slnHeading = 0 AND slnNumElem <= 3
      slnTotLine = 3
   CASE slnHeading = 0 AND slnNumElem > 3
      slnTotLine = slnNumElem
   CASE slnHeading > 0 AND slnNumElem <= 3
      slnTotLine = 5
   CASE slnHeading > 0 AND slnNumElem > 3
      slnTotLine = slnNumElem+2
ENDCASE

IF slnLongest > 0
   * The length of the lines to be displayed in the alert window must be
   * calculated again because of the possibility of a '|' being in the text.
   IF slnHeading > slnLongest+11
      slnMaxLen = slnHeading
   ELSE
      slnMaxLen = slnLongest+11
   ENDIF
   IF slnPBLine > slnMaxLen
      slnMaxLen = slnPBLine
   ENDIF
   IF slnMaxLen < 40
      slnMaxLen = 40
   ENDIF
ENDIF

* Sound the bell.
IF sllBell
   ?? CHR(7)
ENDIF

* Create a window whose size is determined by the length of heading line,
* message lines and push buttons.
IF slnClose > 0
   DEFINE WINDOW m0SAlert ;
      AT 0,0 ;
      SIZE slnTotLine+5,slnMaxLen+8 ;
      TITLE slcTitle2 ;
      FONT "Arial", 9 ;
      STYLE "B" ;
      FLOAT ;
      CLOSE ;
      NOMINIMIZE ;
      DOUBLE ;
      COLOR RGB(,,,192,192,192)
ELSE
   DEFINE WINDOW m0SAlert ;
      AT 0,0 ;
      SIZE slnTotLine+5,slnMaxLen+8 ;
      TITLE slcTitle2 ;
      FONT "Arial", 9 ;
      STYLE "B" ;
      FLOAT ;
      NOCLOSE ;
      NOMINIMIZE ;
      DOUBLE ;
      COLOR RGB(,,,192,192,192)
ENDIF
MOVE WINDOW m0SAlert CENTER

ACTIVATE WINDOW m0SAlert NOSHOW

* Display SAY and GET objects.
IF slnHeading = 0
   slnAddLine = -2
ELSE
   slnAddLine = 0
   @ 1,4 SAY slcHeading;
      SIZE 1,slnMaxLen;
      PICTURE '@I'
ENDIF
slnAddMLine = 0
DO CASE
   CASE slnNumElem = 1
      @ 4+slnAddLine,14.5 SAY slaAlert(1)
   CASE slnNumElem = 2
      @ 3.5+slnAddLine,14.5 SAY slaAlert(1)
      @ 4.5+slnAddLine,14.5 SAY slaAlert(2)
   CASE slnNumElem > 2
      slnAddMline = slnNumElem-3
      FOR slnFor = 1 TO slnNumElem
         @ 2+slnFor+slnAddLine,14.5 SAY slaAlert(slnFor)
      NEXT
ENDCASE
DO CASE
   CASE slcDispQEI = 'E'
      @ 3.2+slnAddLine+slnAddMLine/2,4.2 SAY 'sEMark.bmp' BITMAP ;
         SIZE 3.267,8.167 ;
         STYLE 'T'
   CASE slcDispQEI = 'I'
      @ 3.2+slnAddLine+slnAddMLine/2,4.2 SAY 'sIMark.bmp' BITMAP ;
         SIZE 3.267,8.167
   OTHERWISE
      @ 3.2+slnAddLine+slnAddMLine/2,4.2 SAY 'sQMark.bmp' BITMAP ;
         SIZE 3.267,8.167
ENDCASE
@ 7.2+slnAddLine+slnAddMLine,4+(slnMaxLen-slnPBLine)/2 GET slnPBSel ;
	PICTURE slcPBStr ;
	SIZE 1.53,slnPBLen,slnPBSpace ;
	DEFAULT 1 ;
	FONT "Arial", 9 ;
	STYLE "B"

ACTIVATE WINDOW m0SAlert

CLEAR TYPEAHEAD

READ ;
   CYCLE ;
   MODAL ;
   ACTIVATE siAct() ;
   DEACTIVATE siDeact()

RELEASE WINDOW m0SAlert
POP KEY

* Return the number corresponding to the push button selected by the user.
RETURN slnPBSel

**************
FUNCTION siAct
**************
* READ activate routine.  Place the focus initially on the push button
* specified by the slnPBInit parameter.

PRIVATE slnFor

IF slnPBInit > 1
   FOR slnFor = 1 TO slnPBInit-1
      KEYBOARD '{DNARROW}'
   ENDFOR
ENDIF

****************
FUNCTION siDeact
****************
* READ deactivate routine.

IF NOT WVISIBLE('m0SAlert')
   * If window was closed using the close box (i.e. no longer
   * visible), sglExitWin must be set to .T. in order to exit 
   * the window program.
   IF slnClose > 0
      slnPBSel = slnClose
   ENDIF
ENDIF

*****************
FUNCTION siReturn
*****************
* Return string of specified length taking the font into consideration.

PARAM slcRStr,slnRLen

PRIVATE slcRStr2,slnSpace,slnLastChar,sllMore,slnSpacePos,sllMore2,slnCt

IF TXTWIDTH(slcRStr,'Arial',9,'B') <= slnRLen
   RETURN 0
ENDIF
slcRStr2 = slcRStr+' '					&& Make sure there is a space
slnSpace = 0
slnLastChar = 0
sllMore = .T.
DO WHILE sllMore
   slnSpace = slnSpace+1
   slnSpacePos = AT(' ',slcRStr2,slnSpace)
   IF TXTWIDTH(LEFT(slcRStr2,slnSpacePos-1),'Arial',9,'B') > slnRLen
      IF slnLastChar = 0
         sllMore2 = .T.
         slnCt = 0
         DO WHILE sllMore2
            slnCt = slnCt+1
            IF TXTWIDTH(LEFT(slcRStr2,slnCt),'Arial',9,'B') > slnRLen
               RETURN (slnCt-1)*-1
            ENDIF
         ENDDO
      ELSE
         RETURN slnLastChar
      ENDIF
   ELSE
      slnLastChar = slnSpacePos-1
   ENDIF
ENDDO

