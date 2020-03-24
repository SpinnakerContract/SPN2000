* FUNCTION: Is_Run32.prg
* AUTHOR: George Tasker
* DATE: January 13, 1998 - 8:26 AM
* PURPOSE: Determines if a Windows
* application is running and returns
* the handle of the window if it is,
* otherwise returns 0. 32 bit version
* for Windows 95/NT 3.51-4

LPARAMETER pctitle

* Parameter list description
*
* pctitle - The title bar of the Window
*   Note: The title does not have to be
*   the complete title that appears
*
*
* API Declarations
DECLARE INTEGER GetActiveWindow IN Win32API
DECLARE INTEGER GetWindow IN Win32API;
  INTEGER hwnd, INTEGER dflag
DECLARE INTEGER GetWindowText IN Win32API ;
  INTEGER hwnd, STRING @lptstr, INTEGER cbmax

LOCAL lnhwnd, lnnext, lldone, lctitle_bar, lcsearchfor,;
  lntext_len
lcsearchfor = UPPER(ALLTRIM(pctitle))
lnhwnd = GetActiveWindow()
lnnext = 2
lldone = .F.
lctitle_bar = ""
DO WHILE NOT lldone
  IF NOT EMPTY(lnhwnd)
    lctitle_bar = SPACE(200) + CHR(0)
    lntext_len = GetWindowText(lnhwnd, @lctitle_bar, 200)
    lctitle_bar = UPPER(LEFT(lctitle_bar, lntext_len))
    lldone = (lcsearchfor $ lctitle_bar)
    IF NOT lldone
      lnhwnd = GetWindow(lnhwnd, lnnext)
    ENDIF
  ELSE
    lldone = .T.
  ENDIF
ENDDO
RETURN lnhwnd