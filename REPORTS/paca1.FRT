   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Courier New                    �Left = 33
Top = 29
Width = 101
Height = 90
Alias = "tmprma"
CursorSource = ..\..\nacvfp\temp\tmprma.dbf
Name = "Cursor2"
                                   cursor                        Left = 374
Top = 19
Width = 101
Height = 90
Alias = "configlc"
CursorSource = ..\..\nacvfp\configlc.dbf
Name = "Cursor1"
                                                                                                                                                                       cursor                        ���   �   �                         Zp   %   �       �      �           �  U  N  %�C� Report Designer���6 � �,� Report Designer�� � G+(� davmprcs� U  DAVMPRCS BeforeOpenTables,     ��1 ��A 2                       z       )   �                                          �PROCEDURE BeforeOpenTables
IF WEXIST("Report Designer")
  ZOOM WIND "Report Designer" MAX
ENDIF
SET PROC TO davmprcs

ENDPROC
                              mLeft = 425
Top = 536
Width = 520
Height = 200
InitialSelectedAlias = "tmprma"
Name = "Dataenvironment"
                     dataenvironment                Verdana                        Arial                          Times New Roman                Times New Roman                8"Spinnaker Contract Mfg. Inc. PREVENTIVE Action Request"        Times New Roman                >"THIS SECTION TO BE COMPLETED BY SPINNAKER CONTRACT MFG. INC."                                   Times New Roman                h"Return Completed form to        Quality Manager       Materials Manager, Spinnaker Contract Mfg. Inc.,"                          Times New Roman                <"PO Box 276 Tilton, NH  03276-0276 or fax to (603) 286-7857"                                     Times New Roman                ."Controlled Form F054 rev. B (Quality Record)"                  Times New Roman                "Originator"                   Times New Roman                "Date:"                        Times New Roman                "Part number affected:"        Times New Roman                "SCMI Lot#"                    Times New Roman                "Process affected:"            Times New Roman                "PAR Number:"                  Times New Roman                "Proposed Preventive Action:"                                   Times New Roman                6"This section to be completed by SCMI Quality Manager"          Times New Roman                "RESPONSE DUE DATE:"           Times New Roman                V"RESPONSE SECTION TO BE COMPLETED BY:  ______________________________________________"           Times New Roman                8"This section to be completed by RESPONDENT named above"        Times New Roman                 "Benefits of Preventive Action:"                                Times New Roman                ."Evaluation of Inventory and Work In Process:"                  Times New Roman                "Preventive Action:"           Times New Roman                "Planned Implementation Date:"                                  Times New Roman                M"This section to be completed by SCMI Quality Manager and person named below"                    Times New Roman                :"Method of Monitoring Effectiveness of Preventive Action:"      Times New Roman                @"Individual Responsible for Implementingthe Preventive Action:"                                 Times New Roman                O"Individual Responsible for Monitoring theeffectiveness of Preventive Action:"                  Times New Roman                "Scheduled Audit Date:"        Times New Roman                +"Follow-up Date: (1 month post-audit date)"                     Times New Roman                %"Audit Completed / Report Closed By:"                           Times New Roman                "Date:"                        Times New Roman                "Originator Sign-Off:"         Times New Roman                "Date:"                        Arial                          "Corrective Action #"          ALLT(STR(tmprma.idnum))                                       Verdana                                                       "..\bitmaps\a2k-logo.bmp"      Courier New                    Times New Roman                Times New Roman                Times New Roman                Times New Roman                Times New Roman          