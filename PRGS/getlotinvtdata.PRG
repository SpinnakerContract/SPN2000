* 24Feb16
CLOSE DATA
SET EXCL OFF
SET SAFETY OFF
SET TALK ON
SET ODOM TO 5000
SELE 0
USE V:\nacfox\incinslog
COPY TO F:\nacfox\incinslog WITH CDX
USE V:\nacfox\serlog
COPY TO F:\nacfox\serlog WITH CDX
USE V:\nacfox\qgen
COPY TO F:\nacfox\qgen WITH CDX TYPE FOX2X
USE V:\nacfox\qmat
COPY TO F:\nacfox\qmat WITH CDX TYPE FOX2X
USE V:\nacfox\qmat_s
COPY TO F:\nacfox\qmat_s WITH CDX
USE V:\nacfox\inv
COPY TO F:\nacfox\inv WITH CDX TYPE FOX2X
USE V:\nacfox\batchprt
COPY TO F:\nacfox\batchprt WITH CDX TYPE FOX2X
USE V:\nacfox\inocart
COPY TO F:\nacfox\inocart WITH CDX
USE V:\nacfox\inohist
COPY TO F:\nacfox\inohist WITH CDX
USE V:\nacfox\pickhist
COPY TO F:\nacfox\pickhist WITH CDX
CLOSE DATA