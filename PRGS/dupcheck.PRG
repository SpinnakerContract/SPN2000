*// Ex: DO dupcheck IN LWWPROCS WITH [pieces],[p_pcno]
CLOSE DATA ALL
CLEAR
SET DELE ON
zTable = [                     ]
zKey   = [                     ]
zOrder = [                     ]
@ 2,2 SAY [Table ] GET zTable   PICT [@!]
READ
SELE 0
IF !EMPT(zTable)
  USE &zTable
  zfc = FCOUNT()
  FOR jjj = 1 TO MIN(45,zfc)
    @ 1+jjj,40 SAY FIELD(jjj)
  ENDFOR
ENDIF
@ 4,2 SAY [Field ] GET zKey     PICT [@!]
@ 6,2 SAY [Order ] GET zOrder   PICT [@!]
READ
*PARA ztable,zkey,zorder
*PRIV ztable,zkey,zorder
IF EMPT(zOrder)
  zorder = zkey
ENDIF
SELE &zkey AS _key, COUNT(*) AS _count ;
  FROM (ztable) GROUP BY _key HAVING _count > 1;
  ORDER BY &zorder
