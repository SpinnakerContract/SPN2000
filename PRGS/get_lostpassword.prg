** Copyright Avian Management Services 2003
** Author David Le Mesurier
** Created 07/03/2003 10:30:35 AM
** StringDecrypt function modified from VPM sec, Copyright Promatrix
** Get_decrypted function modified from PSP libary, Copyright ProSysPlus

#IF .F.
TEXT
********************************
*   HELP BUILDER COMMENT BLOCK *
********************************
*:Help Documentation
*:Purpose:
Developer / System Admin Manager tool to get lost passwords and related data
*:Keywords:
Lost passwords, Security
*:Parameters:

*:Returns:

*:Remarks:
IF the system administrator has forgotten his / her password this will retrieve it for them.
BUILD as exe file AND place in main directory of application, then execute it.

ALTER the value stored in lcPassword to suit your system.

*:Example:

*:EndHelp
ENDTEXT
#ENDIF

**** Get the application prefix.
** The main application directory contains a file named S<AppPre>.fapd.apd
** If we look for this file and parse it's name we can get the application prefix.

RELEASE gcAppPre
PUBLIC gcAppPre


LOCAL lnNumberOfFiles,lcUserTable,lcPassword,lcPassword1,lcMessage

STORE '' TO lcUserTable,lcPassword,lcPassword,lcMessage
STORE 0 TO lnNumberOfFiles

** Add some basic security so that we can leave it on a system.
** Developer can alter this to suite their own system if they want to leave the tool on the users machine.
STORE 'mypassword' TO lcPassword

lcPassword1 = INPUTBOX('Please enter you password','Password retreival','',10000)

IF lcPassword1 <> lcPassword
MESSAGEBOX('Invalid Password')
RETURN 
endif

lnNumberOfFiles = ADIR(laFAPDFILE, '*.APD')  && Create array

IF lnNumberOfFiles = 1
gcAppPre = SUBSTR(laFAPDFILE(1,1),2,3)
ELSE
MESSAGEBOX('Unable to locate APD file',0,'Password retreival')
RETURN 
endif

lcUserTable = 'S'+ gcAppPre+ 'SUSR'

CD data
IF USED(lcUserTable)
USE IN &lcUserTable
endif

LOCAL lcUserName, lcPassword
USE (lcUserTable) IN 0 EXCLUSIVE
SCAN

lcText = encrypted

lcMessage = 'User Number ' + ALLTRIM(get_decrypted(lctext,5,10))
lcMessage =  lcMessage + CHR(10) +  'User ID ' + ALLTRIM(get_decrypted(lctext,15,10))
lcMessage =  lcMessage + CHR(10) +   'First Name ' + ALLTRIM(get_decrypted(lctext,25,20))
lcMessage =  lcMessage + CHR(10) +   'Last Name ' + ALLTRIM(get_decrypted(lctext,65,20))
lcMessage =  lcMessage + CHR(10) +   'Password ' + ALLTRIM(get_decrypted(lctext,85,10))
lcMessage =  lcMessage + CHR(10) +   'Password Date ' + ALLTRIM(get_decrypted(lctext,95,10))
lcMessage =  lcMessage + CHR(10) +   'Temporary Password ' + ALLTRIM(get_decrypted(lctext,108,1))
lcMessage =  lcMessage + CHR(10) +   'Password Form Aceess ' + ALLTRIM(get_decrypted(lctext,109,1))
lcMessage =  lcMessage + CHR(10) +   'F3 Edit rights ' + ALLTRIM(get_decrypted(lctext,110,1))
lcMessage =  lcMessage + CHR(10) +   'Logged on ' + ALLTRIM(get_decrypted(lctext,111,1))
lcMessage =  lcMessage + CHR(10) +   'Multiple Log-on ' + ALLTRIM(get_decrypted(lctext,112,1))
lcMessage =  lcMessage + CHR(10) +   'Status ' + ALLTRIM(get_decrypted(lctext,113,10))
lcMessage =  lcMessage + CHR(10) +   'Control Permisson Group ' + ALLTRIM(get_decrypted(lctext,123,3))

lcMessage =  lcMessage + CHR(10) +   'Field Permisson Group ' + ALLTRIM(get_decrypted(lctext,126,3))
lcMessage =  lcMessage + CHR(10) +   'Menu Permisson Group ' + ALLTRIM(get_decrypted(lctext,129,3))
lcMessage =  lcMessage + CHR(10) +   'Repoert Manager Type ' + ALLTRIM(get_decrypted(lctext,132,1))

MESSAGEBOX(lcMessage,0,'Password Retrieval')

ENDSCAN
USE IN (lcUserTable)

RELEASE gcAppPre

******************************************
************ FUNCTIONS *******************


*** Code for get_decrypted modified from PSP Library get_decrypted.prg
FUNCTION get_decrypted
LPARAMETERS tcEncrypted,tnStart,tnLen

LOCAL lcRetString
		lcRetString= StringDecrypt(SUBSTR(tcEncrypted,tnStart,tnLen))
RETURN lcRetString

** Modified from VPM oSec, Copyright Promatrix
FUNCTION stringdecrypt
* Decrypt the string passed to this method.

LPARAM tcString

LOCAL lcReturn,lcPrefix,lnPrefix,lnStrLen,lnStrLen7,lnFor,lnPos7
lcReturn = ''
lcPrefix = gcAppPre
lnPrefix = INT(ASC(RIGHT(lcPrefix,1))/10)
lnStrLen = LEN(tcString)
lnStrLen7 = lnStrLen%7+1
* Decrypt string character by character.
FOR lnFor = 1 TO lnStrLen
   lnPos7 = lnFor%7+1
   lcReturn = lcReturn+CHR(ASC(SUBSTR(tcString,lnFor,1))-lnPos7-lnStrLen7+lnPrefix)
ENDFOR

* Return a string that is the same length as the string that was passed to this method.
RETURN lcReturn
