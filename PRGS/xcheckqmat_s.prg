CLOSE DATA
CREATE DBF C:qmat_s_tst (job C(6),mfg C(25),need N(14,2), used N(12,2), qvused N(12,2), s_used N(12,2), s_qvused N(12,2), s_cnt N(3),s_caecnt N(3), details M(10), cissues C(25))
SELE 0
USE F:\nacfox\qgen
SELE 0
USE F:\nacfox\batchprt
SELE 0
USE F:\nacfox\qmat
SET ORDER TO job_mfg
SELE 0
USE F:\nacfox\qmat_s
SET ORDER TO qlink
zCrLf = CHR(13)+CHR(10)
SELE qgen
SCAN FOR !bal>0 .AND. job > [ 46070]
  SELE qmat
  SET KEY TO qgen.job
  SEEK qgen.job
  SCAN WHILE qmat.job = qgen.job
    APPEN BLANK IN qmat_s_tst
    REPL job WITH qgen.job,mfg WITH qmat.mfg,need WITH qgen.qty*qmat.perunit IN qmat_s_tst
    REPL used WITH used+qmat.used, qvused WITH qvused+qmat.qvused IN qmat_s_tst
    SELE qmat_s
    SEEK qmat.qlink
    SCAN WHILE qmat_s.qlink = qmat.qLINK
      DO CASE
        CASE !EMPT(qmat_s.cae) .and. qmat_s.lnew &&.or. (!qmat_s.lnew.and.SEEK(qmat_s.sku,[batchprt],[sku])
          REPL s_qvused WITH s_qvused + qmat_s.qty, s_caecnt WITH s_caecnt + 1 IN qmat_s_tst
          REPL details WITH details + [CAE  ]+qmat_s.cae+STR(qmat_s.sku,8)+STR(qmat_s.qty,14,2)+[  lNew=.T.]  +zCrLf IN qmat_s_tst
        CASE EMPT(qmat_s.cae).and. (!qmat_s.lnew.and.SEEK(qmat_s.sku,[batchprt],[sku]).and.batchprt.mfg#qmat_s.mfg)
          REPL s_qvused WITH s_qvused + qmat_s.qty, s_caecnt WITH s_caecnt + 1 IN qmat_s_tst && must be a cae
          REPL details WITH details + [CAEb ]+batchprt.mfg+STR(qmat_s.sku,8)++STR(qmat_s.qty,14,2)+[  lNew=.F.]  +zCrLf IN qmat_s_tst
        CASE EMPT(qmat_s.cae).and. (qmat_s.lnew.and.SEEK(qmat_s.sku,[batchprt],[sku]).and.batchprt.mfg==qmat_s.mfg)
          REPL s_used WITH s_used + qmat_s.qty, s_cnt WITH s_cnt + 1 IN qmat_s_tst
          REPL details WITH details + [ORIG ]+batchprt.mfg+STR(qmat_s.sku,8)++STR(qmat_s.qty,14,2)+[  lNew=.T.]  +zCrLf IN qmat_s_tst
        OTHERWISE
          REPL s_used WITH s_used + qmat_s.qty, s_cnt WITH s_cnt + 1 IN qmat_s_tst        
          REPL details WITH details + [ORIG ]+qmat.mfg+STR(qmat_s.sku,8)+STR(qmat_s.qty,14,2)+[  lNew=X]  +zCrLf IN qmat_s_tst
      ENDCASE
    ENDSCAN
  ENDSCAN
ENDSCAN
SELE qmat_s_tst
GO TOP
BROW
