LPARAM xtype
nStart=SECO()
xtype = IIF(PARA()=0,1,xtype)
_screen.fontname = [Lucida Console]
_screen.fontsize = 14
CLEAR
j= rand(-1)
PUBLIC zstr,zset
zStr = []
zset = "abcdefghi01234567890jklmnopqrstuvwxyz01234567890()][0123456789"+UPPE([abcdefghijklmnopqrstuvwxyz])
? zSet
? LEN(ALLT(zset))
** Creates a 30-40 byte randon string and stuffs it into the WINDOWS clipboard 
qstr=RIGHT(SYS(2015),4)
FOR jjj = 1 TO 10000
  zStr = qstr
  zMax = 47+VAL( LEFT(RIGHT(STR(SECO(),8,2),2),1) )
  STORE [] TO zchar,xxlast
  DO WHILE LEN(zStr) < zMax
    xx = CEIL(rand() * 100)
    IF !BETW(xx,1,LEN(ALLT(zset)))
      LOOP
    ENDIF
    zchar = SUBS(zset,xx,1)
    IF ISAL(zchar) .and. ISAL(xxlast)
      LOOP
    ENDIF
    xxlast = zchar
    zStr   = zStr + zchar
  ENDDO
  jRemain = (SECO()-nStart)
  STORE 0  TO jDays, jHours, jMinutes, jSeconds
  STORE [] TO cdays, cHours, cMinutes, cSeconds
  IF jRemain > 86359 && days
    jDays   = INT(jRemain / 86400)
    jRemain = jRemain - (jDays * 86400)
    cdays   = IIF(jDays>0,ALLT(STR(jDays))+[d ],[])
  ENDIF
  IF jRemain > 3559 && hrs
    jHours   = INT(jRemain / 3600)
    jRemain  = jRemain - (jHours * 3600)
    cHours   = IIF(jHours>0,ALLT(STR(jHours))+[h ],[])
  ENDIF
  IF jRemain > 59 && min
    jMinutes = INT(jRemain / 60)
    jRemain  = jRemain - (jMinutes * 60)
    cMinutes = IIF(jMinutes>0,ALLT(STR(jMinutes))+[m ],[])
  ENDIF
  cSeconds = ALLT(STR(jRemain))+[s ] && always show seconds even if zero
  cdur = (cdays+cHours+cMinutes+cSeconds)

  ? STR(jjj,5)+[    ]+PADR(zStr,50)
  IF jjj > 2
    FOR zzz = 1 TO 5
      DO CASE
        CASE _Screen.windowstate(1) # 1 .and. jjj <= 10 && maximized and less than 10 operations
          yy=INKEY(.7)
        CASE _Screen.windowstate(1) # 1 .and. jjj > 10 && maximized and more than 10 operations
          yy=INKEY(10)
        OTHERWISE
          yy=INKEY(.7)
      ENDCASE      
      ?? STR(zzz,3)
      IF jjj = 3 .and. zzz = 1
        _Screen.windowstate=1 && minimized
      ENDIF
    ENDFOR
  ELSE
    yy = 0
  ENDIF
  ?? [        ]+cdur
  IF yy = 27
    EXIT
  ENDIF
  _ClipText = zstr
ENDFOR
*? zStr