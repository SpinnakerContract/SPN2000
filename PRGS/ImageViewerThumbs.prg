I created a form with 60 image controls named something like image01, image02, etc. I use macro substitution to loop through them as in:

CLEAR RESOURCES 
FOR mycount=1 TO ThisForm.numthumbs
    CurrentThumbnail="Thisform."+ALLTRIM(Thisform.thumbclassname)+ALLTRIM(STR(mycount))
    oCurrentThumb=&CurrentThumbnail
    WITH oCurrentThumb
        .PICTURE=""
        .picturepath=""
        .photoid=""
        .selected=.f.
        .recordnumber=0
        .bordercolor=_SCREEN.oApp.ClrNotImported
        .tooltiptext=""
    ENDWITH 
ENDFOR

I use similar code when I have to resize the controls (in the form's resize method), and other things. It's best to blank them out (thisform.control.picture="") first, which is what the above code does. It just looks cleaner.

-- Martin

PS You can call me at 407-767-9278 if you have any questions.

