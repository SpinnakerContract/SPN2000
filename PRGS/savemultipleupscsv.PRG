cnt=ADIR(upscsvs,"F:\UPS\*.CSV")
? cnt
? UpsCsvs(1,1)
? UpsCsvs(1,2)
? UpsCsvs(1,3)
? UpsCsvs(1,3)+1
? UpsCsvs(1,4)
? UpsCsvs(1,1)
filename= UpsCsvs(1,1)
filedate = UpsCsvs(1,3)
filetime = UpsCsvs(1,4)
filedate = DTOS(UpsCsvs(1,3))
filetime = STRTR(UpsCsvs(1,4),[:],[])
? STRTR(filename,[.CSV],filedate+filetime+[.CSV])
SELE 0
fn2= STRTR(filename,[.CSV],filedate+filetime+[.CSV])
COPY ("F:\UPS\"+filename) TO ("F:\UPS\"+fns)
COPY FILE ("F:\UPS\"+filename) TO ("F:\UPS\"+fns)
COPY FILE ("F:\UPS\"+filename) TO ("F:\UPS\"+fn2)
cnt=ADIR(upscsvs,"F:\UPS\UPS_CSV_EXPORT_DAILY.CSV")
? cnt
? UpsCsvs(1,1)
