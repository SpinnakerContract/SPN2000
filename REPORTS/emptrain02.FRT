   _   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    �Left = 15
Top = 16
Width = 101
Height = 90
Alias = "tmpetrain_20"
CursorSource = ..\..\nacvfp\temp\tmpetrain_20.dbf
Name = "Cursor4"
                       cursor                         �Left = 650
Top = 32
Width = 101
Height = 90
Alias = "configlc"
CursorSource = ..\..\nacvfp\configlc.dbf
Name = "Cursor2"
                                                                                                                                      cursor                        ���   �   �                         �   %   �       �      �           �  U  N  %�C� Report Designer���6 � �,� Report Designer�� � G+(� davmprcs� U  DAVMPRCS Init,     ��1 ��A 1                       l       )   �                                                      wPROCEDURE Init
IF WEXIST("Report Designer")
  ZOOM WIND "Report Designer" MAX
ENDIF
SET PROC TO davmprcs
ENDPROC
           rLeft = 28
Top = 363
Width = 817
Height = 238
InitialSelectedAlias = "tmpetrain_20"
Name = "Dataenvironment"
                dataenvironment                Lucida Console                 Monotype Sorts                 Arial                          Lucida Console                 Arial                          "Employee Training Report"                                      'DateExpand(DATE())+[ @ ]+LEFT(TIME(),5)                                                        Arial                          _pageno                                                       Arial                          Arial                          "Page:"                        Arial                          
"Printed:"                     ctext                                                         Lucida Console                 "@J"                           Lucida Console                 FILE("C:\THISFILEONLYON.CTI")                                                                  "emptrain01.frx"               Arial                          '"Spinnaker Contract Manufacturing Inc."                         configlc.rptline01                                            Arial                          IIF(tmpetrain_20.ldone,[4],[ ])                                                                Monotype Sorts                 cmemo                                                         Lucida Console                 Courier New                    Arial                          Arial                          Arial                          Lucida Console           