   a   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    �Left = 25
Top = 15
Width = 101
Height = 215
Alias = "tmp_qlog1"
CursorSource = ..\..\nacvfp\temp\tmp_qlog1.dbf
Name = "Cursor3"
                            cursor                         �Left = 150
Top = 20
Width = 101
Height = 90
Alias = "configlc"
CursorSource = ..\..\nacvfp\configlc.dbf
Name = "Cursor2"
                                                                                                                                      cursor                        ���   �   �                         �   %   �       �      �           �  U  N  %�C� Report Designer���6 � �,� Report Designer�� � G+(� davmprcs� U  DAVMPRCS Init,     ��1 ��A 2                       n       )   �                                                      yPROCEDURE Init
IF WEXIST("Report Designer")
  ZOOM WIND "Report Designer" MAX
ENDIF
SET PROC TO davmprcs

ENDPROC
         �Left = 79
Top = 390
Width = 788
Height = 343
AutoCloseTables = .F.
InitialSelectedAlias = "tmp_qlog1"
Name = "Dataenvironment"
                             dataenvironment                Arial                          Lucida Console                 Arial                          Arial                          "Quality Report"                                              Arial                          "SCMI"                                                        "..\bitmaps\a2k-logo.bmp"      'DateExpand(DATE())+[ @ ]+LEFT(TIME(),5)                                                        Arial                          _pageno                                                       Arial                          Arial                          "Page:"                        Arial                          
"Printed:"                     "@J"                           Lucida Console                 FILE("C:\THISFILEONLYON.CTI")                                                                  "qualrpt02.frx"                tmp_qlog1.cpassdet                                            Lucida Console                 "![built]$lower(configlc.rptline01)                              tmp_qlog1.cfaildet                                            Lucida Console                 "![built]$lower(configlc.rptline01)                              tmp_qlog1.csummtxt                                            Lucida Console                 ALLT(configlc.rptline01)                                      Arial                          Courier New                    Arial                          Arial                    