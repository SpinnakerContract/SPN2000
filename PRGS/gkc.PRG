*FUNCTION GenOKeyCodes &&-----------------------------------------------------------------------
*// for all modules other than level 1
PARA zs,zn,zlevel
_REFOX_ = (9876543210)
_REFOX_ = (9876543210)
PRIV zs,zn,zc,zl,zp,zz,zo,z1,z2,z3,z4,z5,ml,s1,s2,s3,s4,s5,s6,n1,n2,n3,n4,n5
zlevel = ALLT(UPPE(zlevel))
zs = ALLT(zs)
zn = ALLT(zn)
IF LEN(zs)#4 .OR. LEN(zn)<3
  RETURN([FALSE])
ENDIF
STORE 0 TO zc,zl,zp,zz
FOR ttt = 1 TO LEN(zn)  && analyze all characters in NAME
  ml = SUBS(zn,ttt,1)   && get current character
  zc = zc + IIF(BETW(ASC(ml),65,90),1,0)  && count  CAPS   A-Z 
  zl = zl + IIF(BETW(ASC(ml),97,122),1,0) && count  low    a-z
  zp = zp + IIF(BETW(ASC(ml),33,47),1,0)  && next 3 count ALL PUNC
  zp = zp + IIF(BETW(ASC(ml),58,64),1,0) 
  zp = zp + IIF(BETW(ASC(ml),91,96),1,0) 
  zz = zz + 93-ASC(ml)                    && arbitrary # offset from 93
ENDFOR
zo = ALLT(STR(zz))                        && char str of arbitrary #
*WAIT zo WIND TIME 10
STORE [] TO z1,z2,z3,z4,z5
FOR ttt = 1 TO LEN(zo)    && cycle thru char str
  ml = SUBS(zo,ttt,1)     && get char
  z1 = IIF(ttt=1,ml,z1)   && if 1  then first number
  z2 = IIF(ttt=2,ml,z2)   && etc....
  z3 = IIF(ttt=3,ml,z3)
  z4 = IIF(ttt=4,ml,z4)
  z5 = IIF(ttt=5,ml,z5)
ENDFOR
*WAIT [Z's   ]+z1+z2+z3+z4+z5 WIND TIME 10
s1=VAL(SUBS(zs,4,1))
s2=VAL(SUBS(zs,3,1))
s3=VAL(SUBS(zs,2,1))
s4=VAL(SUBS(zs,1,1))
s5=CEIL((s3+s4)+IIF(MOD(s3,2)=0,1,2)+IIF(MOD(s4,2)=0,2,5))
s6=ALLT(STR((s4+s2+s3+s1)*s5))
N1=66+CEILING(LEN(zn)/5)
N2=79-OCCU(CHR(32),zn)
N3=71+CEIL(zc/2)
N4=85-CEIL(zl/2)
N5=68+zp
DO CASE
  CASE zlevel=[LEVEL 2]
    f1 = [2]+s6+ALLT(STR(zc))+zo+ALLT(STR(N3))
  CASE zlevel=[LEVEL 3]
    f1 = [3]+zo+ALLT(STR(zl))+s6+ALLT(STR(N5))
  CASE zlevel=[MULTI-USER] && multi-user
    f1 = [M]+ALLT(STR(zc))+zo+ALLT(STR(N4))+s6
  CASE zlevel=[SECURITY] && security
    f1 = [S]+ALLT(STR(zl))+s6+ALLT(STR(N3))+ALLT(STR(N5))
  OTHERWISE  && old quotes
    f1=z3+CHR(N5)+CHR(89-(s4+4))+CHR(N4)+CHR(s3+3+65)+CHR(N3);
    +CHR(89-(s2+2))+CHR(N2)+CHR(s1+1+65)+CHR(N1)+CHR(65+(s5))+;
    z1+z5+[Q]+z4+z2
ENDCASE
RETURN(ALLT(f1))

