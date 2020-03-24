zzStartdir = [C:\SVM]
*!*	IF FILE(zzstartdir+[\PKZ25.EXE]).and.!FILE(zzstartdir+[\DATA\PKZ25.EXE])
*!*	  COPY FILE zzstartdir+[\PKZ25.EXE] TO zzstartdir+[\DATA\PKZ25.EXE]
*!*	ENDIF
*!*	IF FILE(zzstartdir+[\PKZIP.EXE]).and.!FILE(zzstartdir+[\DATA\PKZIP.EXE])
*!*	  COPY FILE zzstartdir+[\PKZIP.EXE] TO zzstartdir+[\DATA\PKZIP.EXE]
*!*	ENDIF
IF FILE(zzstartdir+[\FOXPRO.ESO]).and.!FILE(zzstartdir+[\DATA\FOXPRO.ESO])
  COPY FILE zzstartdir+[\FOXPRO.ESO] TO zzstartdir+[\DATA\FOXPRO.ESO]
ENDIF
IF FILE(zzstartdir+[\FOXPRO.ESL]).and.!FILE(zzstartdir+[\DATA\FOXPRO.ESL])
  COPY FILE zzstartdir+[\FOXPRO.ESL] TO zzstartdir+[\DATA\FOXPRO.ESL]
ENDIF
IF FILE(zzstartdir+[\FOXFIX.EXE]).and.!FILE(zzstartdir+[\DATA\FOXFIX.EXE])
  COPY FILE zzstartdir+[\FOXFIX.EXE] TO zzstartdir+[\DATA\FOXFIX.EXE]
ENDIF
IF FILE(zzstartdir+[\FOXR.EXE]).and.!FILE(zzstartdir+[\DATA\FOXR.EXE])
  COPY FILE zzstartdir+[\FOXR.EXE] TO zzstartdir+[\DATA\FOXR.EXE]
ENDIF
IF FILE(zzstartdir+[\FOXMAINT.FXP]).and.!FILE(zzstartdir+[\DATA\FOXMAINT.FXP])
  COPY FILE zzstartdir+[\FOXMAINT.FXP] TO zzstartdir+[\DATA\FOXMAINT.FXP]
ENDIF