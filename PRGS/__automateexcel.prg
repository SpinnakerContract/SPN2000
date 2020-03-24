*!*	' Declare an object variable to hold the object 
*!*	' reference. Dim as Object causes late binding. 
*!*	Dim ExcelSheet As Object
*!*	Set ExcelSheet = CreateObject("Excel.Sheet")

*!*	This code starts the application creating the object, in this case, a Microsoft Excel spreadsheet. Once an object is created, you reference it in code using the object variable you defined. In the following example, you access properties and methods of the new object using the object variable, ExcelSheet, and other Microsoft Excel objects, including the Application object and the Cells collection.

*!*	' Make Excel visible through the Application object.
*!*	ExcelSheet.Application.Visible = True
*!*	' Place some text in the first cell of the sheet.
*!*	ExcelSheet.Application.Cells(1, 1).Value = "This is column A, row 1"
*!*	' Save the sheet to C:\test.xls directory.
*!*	ExcelSheet.SaveAs "C:\TEST.XLS"
*!*	' Close Excel with the Quit method on the Application object.
*!*	ExcelSheet.Application.Quit
*!*	' Release the object variable.
*!*	Set ExcelSheet = Nothing



* Simple automation with Excel.  Just copy this faq into prg file and run it.

* include an EXCEL header file and reference values by name.
* #INCLUDE C:\MyProject\INCLUDE\xl5en32.h
* If you do not have a header file and need to create one.  Refer to FAQ:
* How to create office header files in VFP FAQ184-2749

* creates random numbers for quarterly data.
* adds some detail records

CREATE CURSOR curCompany (Company C(20), Qtr1 N(10,2), qtr2 N(10,2), qtr3 N(10,2), qtr4 N(10,2))
FOR lni = 1 TO 10
    APPEND BLANK 
    REPLACE curCompany.company WITH SYS(2015)
    REPLACE curCompany.qtr1 WITH 1 + 1000 * RAND( )
    REPLACE curCompany.qtr2 WITH 1 + 1000 * RAND( )
    REPLACE curCompany.qtr3 WITH 1 + 1000 * RAND( )
    REPLACE curCompany.qtr4 WITH 1 + 1000 * RAND( )
ENDFOR
    
    
* Excel: HorizontalAlignment 
* 2 = Left
* 3 = Center
* 4 = Right
    
local oExcel, oSheet
oExcel = CreateObject([Excel.Application])
oExcel.Visible = .F.
oExcel.Workbooks.Add()
oExcel.SheetsInNewWorkbook = 1
*OExcel.WorkBooks.Open("C:\test.xls")
oSheet = oExcel.ActiveSheet


lnRow = 0
SELECT curCompany
GO TOP
DO WHILE NOT EOF()
    lnRow = lnRow + 1
    IF lnRow = 1
        oSheet.Cells(lnRow,1).Value = [RFQ 27  FoxPro Rocks!]
        oSheet.Cells(lnRow,1).Locked = .F.

        lnRow = 3
        lnCol = 3
        oSheet.Range([C3]).Select
        oSheet.Cells(lnRow,lnCol).Value = [Qtr 1]
        oSheet.Cells(lnRow,lnCol).Font.Bold = .T.
        
        *oSheet.Cells(lnRow,lnCol).HorizontalAlignment = xlCenter
        oSheet.Cells(lnRow,lnCol).HorizontalAlignment = 3
        
        lnCol = lnCol + 1
        oSheet.Range([D3]).Select
        oSheet.Cells(lnRow,lnCol).Value = [Qtr 2]
        oSheet.Cells(lnRow,lnCol).Font.Bold = .T.
        *oSheet.Cells(lnRow,lnCol).HorizontalAlignment = xlCenter
        oSheet.Cells(lnRow,lnCol).HorizontalAlignment = 3
        
        lnCol = lnCol + 1
        oSheet.Range([E3]).Select
        oSheet.Cells(lnRow,lnCol).Value = [Qtr 3]
        oSheet.Cells(lnRow,lnCol).Font.Bold = .T.
        *oSheet.Cells(lnRow,lnCol).HorizontalAlignment = xlCenter
        oSheet.Cells(lnRow,lnCol).HorizontalAlignment = 3

        lnCol = lnCol + 1
        oSheet.Range([F3]).Select
        oSheet.Cells(lnRow,lnCol).Value = [Qtr 4]
        oSheet.Cells(lnRow,lnCol).Font.Bold = .T.
        *oSheet.Cells(lnRow,lnCol).HorizontalAlignment = xlCenter
        oSheet.Cells(lnRow,lnCol).HorizontalAlignment = 3
        
        lnRow = 4
        lnBeginRange = lnRow
    ENDIF
    
    oSheet.Cells(lnRow,1).Value = curCompany.Company 
    oSheet.Cells(lnRow,3).Value = curCompany.qtr1 
    oSheet.Cells(lnRow,4).Value = curCompany.qtr2 
    oSheet.Cells(lnRow,5).Value = curCompany.qtr3 
    oSheet.Cells(lnRow,6).Value = curCompany.qtr4 

    SKIP
ENDDO        

* Create the formula rather than hardcoding total so the user can 
* change the spreadsheet and it will reflect new totals.
* Example:  =SUM(D5:D10)
FOR lni = 1 TO 4
lcFormula = [=SUM(] + CHR(64 + lni) + ALLTRIM(STR(m.lnBeginRange)) + [:] +;
                CHR(64 + 3 + lni) + ALLTRIM(STR(m.lnRow)) + [)]
                

oSheet.Cells(lnRow+1,2+lni).Formula = [&lcFormula]
ENDFOR 
oSheet.Protect("3119")

osheet.SaveAs("C:\TEST.XLS")
*!*	' Close Excel with the Quit method on the Application object.
osheet.Application.Quit

RETURN

*****************************************************
*!*	Late Edition.
*!*	These miscellaneous Excel automation command are compliments of jrbbldr 
*!*	JRB-Bldr
*!*	VisionQuest Consulting
*!*	Business Analyst & CIO Consulting Services
*!*	CIOServices@yahoo.com
*!*	*****************************************************

*!*	tmpsheet = CREATEOBJECT('excel.application')
*!*	oExcel = tmpsheet.APPLICATION

*!*	* --- Set Excel to only have one worksheet ---
*!*	oExcel.SheetsInNewWorkbook = 1

*!*	* --- Delete the Default Workbook that has 3 worksheets ---
*!*	oExcel.Workbooks.CLOSE

*!*	* --- Now Add a new book with only 1 worksheet ---
*!*	oExcel.Workbooks.ADD
*!*	xlBook = oExcel.ActiveWorkbook.FULLNAME
*!*	xlSheet = oExcel.activesheet

*!*	* --- Name Worksheet ---
*!*	xlSheet.NAME = "Sheet Name"

*!*	* --- Make Excel Worksheet Visible To User ---
*!*	oExcel.VISIBLE = .T. && Set .F. if you want to print only

*!*	   <do whatever>

*!*	oExcel.WINDOWS(xlBook).ACTIVATE
*!*	xlSheet.RANGE([A2]).SELECT

*!*	* --- Save Excel Results ---
*!*	oExcel.CutCopyMode = .F. && Clear the clipboard from previous Excel Paste
*!*	oExcel.DisplayAlerts = .F.

*!*	* --- Save Results ---
*!*	xlSheet.SAVEAS(mcExclFName)

*!*	* --- Close the Worksheet ---
*!*	oExcel.workbooks.CLOSE

*!*	* --- Quit Excel ---
*!*	oExcel.QUIT
*!*	RELEASE oExcel

*!*	tmpsheet = CREATEOBJECT('excel.application')
*!*	oExcel = tmpsheet.APPLICATION
*!*	oExcel.ReferenceStyle = 1  && Ensure Columns in A-B Format instead of 1-2 Format

*!*	mcStrtColRow = 'A1'
*!*	mcEndColRow = 'AB5'
*!*	mcLastCol = 'AZ:'

*!*	* --- Time Masquerading As Text Format Cells ---
*!*	xlSheet.RANGE[mcStrtColRow,mcEndColRow].EntireColumn.NumberFormat = "h:mm:ss"

*!*	* --- Standard Text Format Cells ---
*!*	xlSheet.RANGE[mcStrtColRow,mcEndColRow].EntireColumn.NumberFormat = "@"

*!*	* --- Date Format Cells ---
*!*	xlSheet.RANGE[mcStrtColRow,mcEndColRow].EntireColumn.NumberFormat = "mm/dd/yyyy"

*!*	* --- Auto-Fit All Columns ---
*!*	xlSheet.COLUMNS("A:" + mcLastCol).EntireColumn.AutoFit
 
