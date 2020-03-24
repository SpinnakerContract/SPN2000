**PROCEDURE po_hist &&----------------------
*  compiled FOR VFP version
PARA po1,po2,po3,po4,po5,po6
PRIV po1,po2,po3,po4,po5,po6,m_alias,jusen
*---- example
*=po_hist(po,mfg,qty,[REASON.....],sku,cost)
* called to automatically make an history entry

IF !USED('poedhist')
  =open_dbf("poedhist")
ENDIF
IF !USED('confloc')
  jusen = LEFT(SYS(0),14)
ELSE
  jusen = confloc.username
ENDIF
  m_alias = ALIAS()
  SELE poedhist
  APPE BLANK
  REPLACE po WITH po1,mfg WITH po2, date WITH DATE();
    ,time WITH TIME(), qty WITH po3,ref_misc WITH po4;
    ,cost WITH po6,sku WITH po5, userid WITH jusen
  IF !EMPT(m_alias)
    SELE &m_alias
  ENDIF
RETURN

