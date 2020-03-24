close data all
use avinvoic
zap
append from n:\foxpro2\cti2\avinvoic
REPLA all dcleared WITH IIF([CLEARED]$UPPE(iequip),DATE(),{})

use avcustom
zap
append from n:\foxpro2\cti2\avcustom

use avcahist
zap
append from n:\foxpro2\cti2\avcahist

use avmonthl
zap
append from n:\foxpro2\cti2\avmonthl

CLOSE DATA