   j   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        �6i��&������:�      Arial��~ ��~`��w,q��8��      zLeft = 290
Top = 20
Width = 95
Height = 90
Alias = "configlc"
CursorSource = ..\data\configlc.dbf
Name = "Cursor3"
        cursor                         zLeft = 150
Top = 20
Width = 95
Height = 90
Alias = "configav"
CursorSource = ..\data\configav.dbf
Name = "Cursor2"
        cursor                         �Left = 10
Top = 20
Width = 95
Height = 90
Alias = "av337s"
CursorSource = ..\data\av337s.dbf
Name = "Cursor1"
                                                                                                                                                 cursor                         wPROCEDURE Init
SET PROC TO davmprcs
IF WEXIST("Report Designer")
  ZOOM WIND "Report Designer" MAX
ENDIF
ENDPROC
           dataenvironment                av337s.backtext                                               Arial                          [PROPOR]$UPPE(av337s.backfont)                                  Arial                          "NOTICE"                       Arial                          �"Weight and balance or operating limitation changes shall be entered in the appropriate aircraft record.An alteration must be compatible with all previous alterations to assure continued conformity with the applicable airworthiness requirements."              Arial                          [PAST EVAL]$configav.prodstat                                                                  "EVALUATION COPY"              Arial                           "ADDITIONAL SHEETS ARE ATTACHED"                                Arial                          �"8. DESCRIPTION OF WORK ACCOMPLISHED  (If more space is required, attach additional sheets.  Identify with aircraft nationality and registration mark and date work completed.)"                  configlc.rptline01             Arial                          av337s.backtext                                               Lucida Console                 [MONO]$UPPE(av337s.backfont)                                    Arial                          1=2                                                           L"CTI Developers   There are two back text boxes above on top of each other "                     Arial                          Arial                          Arial                          Arial                          Arial                          Arial                          Lucida Console                 Arial                          mLeft = 128
Top = 326
Width = 520
Height = 200
InitialSelectedAlias = "av337s"
Name = "Dataenvironment"
                    ���   �   �                         �   %   �       �      �           �  U  N  G+(� davmprcs� %�C� Report Designer���G � �,� Report Designer�� � U  DAVMPRCS Init,     ��1 ��A 1                       l       )   �                                                