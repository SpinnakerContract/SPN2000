LPARAM ddate
SET CENT ON
dfdoy    = CTOD("01/01/"+SUBS(DTOS(ddate),1,4))
dnextsat = ddate
DO WHILE DOW(dnextsat)<7
  dnextsat = dnextsat + 1
ENDDO
IF YEAR(dnextsat)>YEAR(ddate) && prod week is in next year
  RETURN(SUBS(DTOS(dnextsat),3,2))+[01]
ELSE
  nweeks = CEIL((dnextsat-dfdoy)/7)
  RETURN(SUBS(DTOS(dnextsat),3,2))+STRTR(STR(nweeks,2),[ ],[0])
ENDIF
