   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    �Left = 102
Top = 33
Width = 91
Height = 260
Alias = "tmphcml3"
CursorSource = ..\..\nacvfp\temp\tmphcml3.dbf
Name = "Cursor1"
                              cursor                         �Left = 652
Top = 38
Width = 101
Height = 90
Alias = "configlc"
CursorSource = ..\..\nacvfp\configlc.dbf
Name = "Cursor4"
                                   cursor                        Left = 533
Top = 37
Width = 101
Height = 90
Alias = "configav"
CursorSource = f:\nacfox\configav.dbf
Name = "Cursor3"
                                                                                                                                                                          cursor                        ���   �   �                         u�   %   �       �      �           �  U  U  %�C� Report Designer���6 � �,� Report Designer�� � G+(� davmprcs� F� � U  DAVMPRCS TMPHCML3 Init,     ��1 ��A q 2                       }       )   �                                   �PROCEDURE Init
IF WEXIST("Report Designer")
  ZOOM WIND "Report Designer" MAX
ENDIF
SET PROC TO davmprcs
SELE tmphcml3

ENDPROC
                           nLeft = 73
Top = 392
Width = 823
Height = 388
InitialSelectedAlias = "tmphcml3"
Name = "Dataenvironment"
                    dataenvironment                Verdana                        Lucida Console                 Arial                          Arial                          Verdana                        Verdana                        Arial                          Courier New                    "PO #"                         Verdana                        "Docs"                         Verdana                        "@J 999"                       Verdana                                                       tmphcml3.ndoccnt               !EMPT(tmphcml1.cdetails)       Verdana                                                        [SKUs: ]+ALLT(tmphcml3.cdetails)                                "----------- MSDS -----------"                                  Verdana                        Verdana                                                       tmphcml3.po                    Verdana                                                       tmphcml3.msds_prot             ORIENTATION=1ol
DEVICE=\      Verdana                                                       tmphcml3.msds_heal             Verdana                                                       tmphcml3.msds_reac             Arial                          "NSI   MSDS  ITEMS"            Verdana                        "Mfg Part#"                                                   Verdana                        	"Expires"                      Arial                          '"Spinnaker Contract Manufacturing Inc."                         tmphcml3.mfg                                                  Verdana                        tmphcml3.loc                                                  Verdana                        Verdana                        "SKU #"                        Verdana                        "Flame React Health Protect"                                    tmphcml3.qoh                                                  Verdana                        "@JZ 999,999.9"                DTOC(tmphcml3.dexpire)                                        Verdana                        !EMPT(tmphcml3.dexpire)        Verdana                        "LOC"                          Verdana                        "Qty OH"                       n_s(tmphcml3.sku)                                             Verdana                        !EMPT(tmphcml3.sku)                                           "..\bitmaps\a2k-logo.bmp"      'DateExpand(DATE())+[ @ ]+LEFT(TIME(),5)                                                        Arial                          _pageno                                                       Arial                          Arial                          "Page:"                        Arial                          
"Printed:"                     "@J"                           Lucida Console                 FILE("C:\THISFILEONLYON.CTI")                                                                  "hcml1.frx"                    tmphcml3.msds_flam                                            Verdana                        ALLT(tmphcml3.cwhen)                                          Verdana                        !EMPT(tmphcml1.cwhen)          Verdana                        	"Expires"                      Verdana                        "Time until"             