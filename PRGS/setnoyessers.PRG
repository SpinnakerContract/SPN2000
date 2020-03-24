USE F:\NACFOX\serlog
DISPLAY STATUS
set order to lotnum DESC
SET KEY TO [ 61564]
brow
SET KEY TO [ 61513]
brow
SET KEY TO [ 61558]
brow
SET KEY TO [ 61554]
brow
SET KEY TO [ 61089]
brow
SET KEY TO [ 61758]
brow
SET KEY TO [ 61758]
brow
SET KEY TO [ 61775]
brow
SET KEY TO [ 61564]
brow
DO FORM c:\spn\forms\test_data.scx
USE F:\NACFOX\serlog
set order to lotnum DESC
SET KEY TO [ 61564]
brow
repl full_ser WITH ALLT(prefix)+[B06]+ALLT(suffix)+STRTR(STR(sernum2,3),[ ],[)])
brow
repl full_ser WITH ALLT(prefix)+[B06]+ALLT(suffix)+STRTR(STR(sernum2,3),[ ],[)])
? STRTR(STR(sernum2,3),[ ],[)])
repl full_ser WITH ALLT(prefix)+[B06]+ALLT(suffix)+STRTR(STR(sernum2,3),[ ],[0])
skip