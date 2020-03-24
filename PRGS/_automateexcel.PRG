loExcel = CREATEOBJECT("Excel.Application")
IF TYPE("loExcel") # "O"  && Problem, Excel not instantiated...
         WAIT WINDOW CHR(13) + CHR(13) + "Error Instantiating Excel Application" + CHR(13) + CHR(13)  TIMEOUT 15
Else 
         loExcel.DisplayAlerts = .T.
         loExcel.Visible = .T.
         loExcel.WorkBooks.add()
              lcSheetName = [PRR]
              USE C:\SVM\data\empclock
              GO top
              _VFP.DataToClip("empclock", , 3)  && Copy to buffer
            
              i=1
              WITH loExcel
                       IF .Sheets.count < i && By Default, loExcel.WorkBooks.add() will add 3 sheet in there alredy.
                                .Worksheets.add(, .Sheets(i))
                       ENDIF
                       .Sheets(i).Activate  && Activate the sheet
                       .Sheets(i).Name = lcSheetName && Change SheetName
                       .ActiveSheet.Paste()  && Paste the Data
                       .Rows("1:1").Select  && Select First Row, Then Delete it
                       .Selection.Delete()  
                       .Rows("1:1").Select  && Select the new first row and Bold it
                       .Selection.Font.Bold = .T.
                       .Columns("C:C").Select    
                       .Selection.EntireColumn.Hidden = .T. && Hide third column, the "Score" Column
             ENDWITH
         *loExcel.Workbooks(1).SaveAs()
         *loExcel.Quit
         RELEASE loExcel
ENDIF
