   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    �Left = 85
Top = 18
Width = 101
Height = 309
Alias = "tmptshorts"
CursorSource = ..\..\nacvfp\temp\tmptshorts.dbf
Filter = "trbl_qty>0"
Name = "Cursor1"
                                    cursor                         �Left = 652
Top = 38
Width = 101
Height = 90
Alias = "configlc"
CursorSource = ..\..\nacvfp\configlc.dbf
Name = "Cursor4"
                                   cursor                         �Left = 533
Top = 37
Width = 101
Height = 90
Alias = "configav"
CursorSource = f:\nacfox\configav.dbf
Name = "Cursor3"
                                                                                                                                         cursor                        ���   �   �                         �   %   �       �      �           �  U  N  %�C� Report Designer���6 � �,� Report Designer�� � G+(� davmprcs� U  DAVMPRCS Init,     ��1 ��A 2                       n       )   �                                                      dataenvironment                ORIENTATION=1ol
DEVICE=H      yPROCEDURE Init
IF WEXIST("Report Designer")
  ZOOM WIND "Report Designer" MAX
ENDIF
SET PROC TO davmprcs

ENDPROC
         Lucida Console                 Arial                          "TROUBLE  ORDER  REPORT"                                      Arial                                                         "Mfg Part#"                                                                                  Arial                          '"Spinnaker Contract Manufacturing Inc."                         IIF(!EMPT(cae),[ .. ]+cae,mfg)                                                                 Arial                          customer                                                      Arial                          Arial                                                         "Cust "                        Arial                                                         
"This Lot"                                                    "..\bitmaps\a2k-logo.bmp"      'DateExpand(DATE())+[ @ ]+LEFT(TIME(),5)                                                        Arial                          _pageno                                                       Arial                          Arial                          "Page:"                        Arial                          
"Printed:"                     "@J"                           Lucida Console                 FILE("C:\THISFILEONLYON.CTI")                                                                  "trblord1.frx"                 totneed                                                       Arial                          "@JZ 999,999.99"               	need_date                                                     Arial                          Arial                                                         	"Need By"                      tot_oo                                                        Arial                          "@JZ 999,999.999"              Arial                                                         "PO_OO"                        lot                                                           Arial                          Arial                                                         "Lot "                         trbl_qty                                                      Arial                          "@JZ 999,999.999"              Arial                                                         "Trouble Qty"                  part                                                          Arial                          Arial                                                         "Part "                        tmptshorts.troubles                                           Lucida Console                 need_all                                                      Arial                          "@JZ 999,999.999"              Arial                                                         
"All Lots"                     Courier New                    Arial                          Arial                          Arial                          Arial                          Arial                          Lucida Console                 pLeft = 22
Top = 310
Width = 788
Height = 378
InitialSelectedAlias = "tmptshorts"
Name = "Dataenvironment"
            