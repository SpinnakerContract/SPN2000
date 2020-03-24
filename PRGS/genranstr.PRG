j= rand(-1)
PUBLIC zstr,zset
zStr = []
zset = "abcdefghi01234567890jklmnopqrstuvwxyz01234567890()][0123456789"+UPPE([abcdefghijklmnopqrstuvwxyz])
** Creates a 30-40 byte randon string and stuffs it into the WINDOWS clipboard
FOR jjj = 1 TO 3000
  ? jjj
  zStr = []
  zMax = 30+VAL( LEFT(RIGHT(STR(SECO(),8,2),2),1) )
  DO WHILE LEN(zStr) < zMax
    xx = CEIL(rand() * 100)
    IF !BETW(xx,1,86)
      LOOP
    ENDIF
    zStr = zStr + SUBS(zset,xx,1)
  ENDDO
  yy=INKEY(2)
  IF yy = 27
    EXIT
  ENDIF
  _ClipText = zstr
ENDFOR
*? zStr