   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    �Left = 31
Top = 19
Width = 101
Height = 90
Alias = "tmpschd41"
CursorSource = ..\..\nacvfp\temp\tmpschd41.dbf
Name = "Cursor1"
                             cursor                         �Left = 636
Top = 187
Width = 101
Height = 90
Alias = "configlc"
CursorSource = ..\..\nacvfp\configlc.dbf
Name = "Cursor4"
                                  cursor                         �Left = 638
Top = 36
Width = 101
Height = 90
Alias = "configav"
CursorSource = f:\nacfox\configav.dbf
Name = "Cursor3"
                                                                                                                                         cursor                        ���   �   �                         �   %   �       �      �           �  U  N  %�C� Report Designer���6 � �,� Report Designer�� � G+(� davmprcs� U  DAVMPRCS Init,     ��1 ��A 2                       n       )   �                                                      dataenvironment                Lucida Console                 ORIENTATION=1ol
DEVICE=H      yPROCEDURE Init
IF WEXIST("Report Designer")
  ZOOM WIND "Report Designer" MAX
ENDIF
SET PROC TO davmprcs

ENDPROC
         Verdana                        
"Assembly"                                                    Arial                          '"Spinnaker Contract Manufacturing Inc."                         part                                                          Arial                          job                                                           Arial                          Verdana                        "Days"                         Verdana                        "Lot #"                                                       "..\bitmaps\a2k-logo.bmp"      'DateExpand(DATE())+[ @ ]+LEFT(TIME(),5)                                                        Arial                          _pageno                                                       Arial                          Arial                          "Page:"                        Arial                          
"Printed:"                     "@J"                           Lucida Console                 FILE("C:\THISFILEONLYON.CTI")                                                                  "schdchng01.frx"               Verdana                        "Cust"                         cdays                                                         Arial                          configlc.rptline02                                            Verdana                        LEFT(code,6)+[ ]+customer                                     Lucida Console                 Verdana                        	"Rev Qty"                      Arial                          "Schedule  Changes  Report"                                     nqty                                                          Arial                          "@Z 999,999"                   odate                                                         Arial                          p_status                                                      Arial                          Verdana                        "Status"                       ndate                                                         Arial                          Verdana                        "Orig Date"                    Verdana                        
"Rev Date"                     Courier New                    Verdana                        Arial                          Arial                          Arial                          Arial                          Lucida Console                 Verdana                        �Left = 73
Top = 392
Width = 788
Height = 383
AutoOpenTables = .T.
AutoCloseTables = .T.
InitialSelectedAlias = "tmpschd41"
Name = "Dataenvironment"
 