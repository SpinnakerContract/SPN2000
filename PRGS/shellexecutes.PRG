DECLARE INTEGER ShellExecute IN shell32.dll ; 
  INTEGER hndWin, STRING cAction, STRING cFileName, ; 
  STRING cParams, STRING cDir, INTEGER nShowWin
You can put this code anywhere in your application, provided it is executed before you create the email message. (Note that ShellExecute is case-sensitive in the above command.)

So what does it do?
Essentially, ShellExecute() lets you "execute" any pogram, document or shortcut. At its simplest, it can be used to launch an external application. For example, this command will launch Notepad:

ShellExecute(0,"open","Notepad.Exe","","",1)
Alternatively, you could use the following code to launch Notepad and have it open a file called Readme.txt:

ShellExecute(0,"open","Readme.txt","","",1)
And this will launch Word and open Disclaimer.RTF:

ShellExecute(0,"open","Disclaimer.RTF","","",1)
The beauty of this is that you don't have to know which application is registered to open a specific document type on the user's system. If the user didn't have Word installed, the above code would launch Wordpad instead, as this can also process RTF files. Of course, if the user had no application at all associated with RTFs, the command would fail.

The second parameter to ShellExecute() lets you specify the action that you want to execute. By changing it to "print", for example, you can print the document rather than open it for editing. Similarly, the following code will play a sound file (using either the Windows Media Player or whatever other application is associated with the WAV extension):

ShellExecute(0,"play","ringin.wav","","",1)
Another possibility:

ShellExecute(0,"open","www.microsoft.com","","",1)
This will launch the user's default web browser and navigate to the site specified.

What about email?
So, back to our original requirement. To create an email message, all you have to do is ShellExecute a mailto link. At its simplest, the link looks something like this:

mailto:john@mycompany.com
So the following code will open a message composing window in the user's default mail client, with the To line already filled in: 

lcMail = "mailto:john@mycompany.com" 
ShellExecute(0,"open",lcMail,"","",1)
Going further, you can specify the following parameters to the mailto link:

CC=    Carbon copy 
BCC= Blind carbon copy 
SUBJECT= Subject text  
BODY= Body text 

Place a question mark before the first of these parameters (right after the email address). Use ampersands to separate further parameters, as in this example:

lcMail = "mailto:john@mycompany.com"+ ;
  "?CC= boss@mycompany.com&Subject= Meet for lunch"+ ;
   "&Body= Please join me for a sandwich at noon." 
ShellExecute(0,"open",lcMail,"","",1)
