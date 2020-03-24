zref  = avprthst.ref_misc
zfr   = AT([ref_],zref)
zfv   = IIF([line_]$zref,AT([ line_],zref),AT([ vend_],zref))
zitm  = SUBS(zref,zfr,zfv-zfr+1)
zitm  = STRTR(zitm,[ref_],[])
zvend = SUBS(zref,AT([vend_],zref)+5,6)

? STRTR(zitm+[_]+zvend,[ ],[]) &&+[_PE_]+STRTR(STRTR(avprthst.ref_num,[P],[]),[ ],[])