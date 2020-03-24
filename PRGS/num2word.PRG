** to get the decimal value. (Elvis)
***? mod( 15265.25,1)

* Program...........: No2WordE
* Author............: Paul Vlad Tatavu
* Description.......: Transforms a number into its equivalent in words in
*                   : English. It drops any fractional part.
*                   :
*                   : It works with negative numbers.
*                   :
* Calling Samples...: ? No2WordE( 1234.23)
*                   :
* Parameters........: 1. The number to be transformed

PARAMETER pnNumber
PRIVATE lcNumber, lnLen, lcReturn, lnI, lcHundred, lcTens, lcSign
PRIVATE lcDecimals,lnHaveDecimals	&& Vars added by Edhy to handle decimals.

*-- Get the sign
IF pnNumber < 0
    lcSign	 = TRANSTR("Minus ")
    pnNumber = -pnNumber
ELSE
    lcSign = ""
ENDIF
IF INT( pnNumber ) = 0
    RETURN TRANSTR("Zero")
ENDIF

** -- This part is added by Edhy to add decimals to the translation.
lcDecimals = ""
lnHaveDecimals = MOD( pnNumber, 1 )
IF lnHaveDecimals > 0
	lcDecimals = TRANSTR(" and ") + RIGHT( STR( lnHaveDecimals, 3, 2 ), 2 ) + "/100"
ELSE
	lcDecimals = TRANSTR(" and ") + "00/100"
ENDIF
*? lcdecimals

*-- Transform the number to string and pad it to the left with zeros up to
*-- the first length multiple of 3
lcNumber = LTRIM( STR( INT( pnNumber), 18))
lnLen    = LEN( lcNumber)
lnLen    = INT( lnLen/3+1)*3
lcNumber = PADL( lcNumber, lnLen, "0")

lcReturn = ""

*-- Parse the string and transform each group of 3 digits
*-- It starts from the end of the string, so lnI=1 is the right most group
FOR lnI = 1 TO lnLen/3

    *-- The string for hundreds
    lcHundred = Digit( SUBSTR( lcNumber, lnLen-lnI*3+1, 1))
    IF NOT EMPTY( lcHundred)
        lcHundred = lcHundred + TRANSTR(" Hundred")
    ENDIF

    *-- The string for tens
    lcTens = Tens( SUBSTR( lcNumber, lnLen-lnI*3+2, 2))

    *-- Add tens string to hundreds string
    IF NOT EMPTY( lcTens)
        IF NOT EMPTY( lcHundred)
            lcHundred = lcHundred + " " + lcTens
        ELSE
            lcHundred = lcTens
        ENDIF
    ENDIF

    *-- Add the group type string: Thousand, Million, Billion or Trillion
    IF NOT EMPTY( lcHundred)
        DO CASE
        CASE lnI = 2
            lcReturn = lcHundred + TRANSTR(" Thousand") + ;
                IIF( EMPTY( lcReturn), "", " ")+lcReturn
        CASE lnI = 3
            lcReturn = lcHundred + TRANSTR(" Million") + ;
                IIF( EMPTY( lcReturn), "", " ")+lcReturn
        CASE lnI = 4
            lcReturn = lcHundred + TRANSTR(" Billion") + ;
                IIF( EMPTY( lcReturn), "", " ")+lcReturn
        CASE lnI = 5
            lcReturn = lcHundred + TRANSTR(" Trillion") + ;
                IIF( EMPTY( lcReturn), "", " ")+lcReturn
        OTHE
            lcReturn = lcHundred
        ENDCASE
    ENDIF
ENDFOR
RETURN ALLT(lcReturn+lcdecimals)


*======================================================================
FUNCTION Tens
PARAMETER pcNumber
PRIVATE lcTens, lcUnits, lcReturn

lcTens  = LEFT( pcNumber, 1)
lcUnits = RIGHT( pcNumber, 1)
DO CASE
CASE lcTens = "0"  && >= 0  and <= 9
    lcReturn = Digit( lcUnits)
CASE lcTens = "1"  && >= 10 and <= 19
    DO CASE
    CASE lcUnits = "0"
        lcReturn = TRANSTR("Ten")
    CASE lcUnits = "1"
        lcReturn = TRANSTR("Eleven")
    CASE lcUnits = "2"
        lcReturn = TRANSTR("Twelve")
    CASE lcUnits = "3"
        lcReturn = TRANSTR("Thirteen")
    CASE lcUnits = "4"
        lcReturn = TRANSTR("Fourteen")
    CASE lcUnits = "5"
        lcReturn = TRANSTR("Fifteen")
    CASE lcUnits = "6"
        lcReturn = TRANSTR("Sixteen")
    CASE lcUnits = "7"
        lcReturn = TRANSTR("Seventeen")
    CASE lcUnits = "8"
        lcReturn = TRANSTR("Eighteen")
    CASE lcUnits = "9"
        lcReturn = TRANSTR("Nineteen")
    ENDCASE
OTHER  && >= 20 and <= 99
    DO CASE
    CASE lcTens  = "2"
        lcReturn = TRANSTR("Twenty")
    CASE lcTens  = "3"
        lcReturn = TRANSTR("Thirty")
    CASE lcTens  = "4"
        lcReturn = TRANSTR("Forty")
    CASE lcTens  = "5"
        lcReturn = TRANSTR("Fifty")
    CASE lcTens  = "6"
        lcReturn = TRANSTR("Sixty")
    CASE lcTens  = "7"
        lcReturn = TRANSTR("Seventy")
    CASE lcTens  = "8"
        lcReturn = TRANSTR("Eighty")
    CASE lcTens  = "9"
        lcReturn = TRANSTR("Ninety")
    ENDCASE
    lcUnits = Digit( lcUnits)
    IF NOT EMPTY( lcUnits)
        lcReturn = lcReturn + "-" + lcUnits
    ENDIF
ENDCASE
RETURN lcReturn && Tens

*======================================================================
FUNCTION Digit
PARAMETER pcDigit
PRIVATE lcReturn
DO CASE
CASE pcDigit = "0"
    lcReturn = ""
CASE pcDigit = "1"
    lcReturn = TRANSTR("One")
CASE pcDigit = "2"
    lcReturn = TRANSTR("Two")
CASE pcDigit = "3"
    lcReturn = TRANSTR("Three")
CASE pcDigit = "4"
    lcReturn = TRANSTR("Four")
CASE pcDigit = "5"
    lcReturn = TRANSTR("Five")
CASE pcDigit = "6"
    lcReturn = TRANSTR("Six")
CASE pcDigit = "7"
    lcReturn = TRANSTR("Seven")
CASE pcDigit = "8"
    lcReturn = TRANSTR("Eight")
CASE pcDigit = "9"
    lcReturn = TRANSTR("Nine")
ENDCASE
RETURN lcReturn && Digit


** -- If using Visual ProMatrix Multilingual Toolkit, comment out this function 
** -- and use the one that comes with VPM
FUNCTION TRANSTR
LPARAM stcString,stcUse_ID
IF TYPE('_SCREEN.oApp.oInt') = 'O'
   RETURN _SCREEN.oApp.oInt.TranslateString(stcString,stcUse_ID)
ELSE
   RETURN stcString
ENDIF
