LPARA jSec
CLEAR
PRIVATE     jDays, jHours, jMinutes, jSeconds, jSec, jRemain
STORE 0  TO jDays, jHours, jMinutes, jSeconds
STORE [] TO cDays, cHours, cMinutes, cSeconds
STORE jSec TO jRemain
IF jRemain > 86359 && days
  jDays   = INT(jRemain / 86400)
  jRemain = jRemain - (jDays * 86400)
  cDays   = IIF(jDays>0,ALLT(STR(jDays))+[d ],[])
ENDIF
IF jRemain > 3559 && hrs
  jHours   = INT(jRemain / 3600)
  jRemain  = jRemain - (jHours * 3600)
  cHours   = IIF(jHours>0,ALLT(STR(jHours))+[h ],[])
ENDIF
IF jRemain > 59 && min
  jMinutes = INT(jRemain / 60)
  jRemain  = jRemain - (jMinutes * 60)
  cMinutes = IIF(jMinutes>0,ALLT(STR(jMinutes))+[m ],[])
ENDIF
jSeconds = jRemain
cSeconds = ALLT(STR(jSeconds))+[s ]
? cDays+cHours+cMinutes+cSeconds
RETURN

*  from other users stuff  on net

*!*	jDays = jSec / 86400
*!*	jHours = (jSec / 3600) - (jDays * 24)
*!*	jMinutes = (jSec / 60) - (jDays * 1440) - (jHours * 60)
*!*	jSeconds = MOD(jSec,60)



*!*	' Just add new cases, and define the strings accordingly
*!*	'(remember to allow spaces too.

*!*	Public Function Duration(TotalSeconds As Long, UpFormat As _
*!*	    Integer) As String
*!*
*!*	  ' Format = 0, 1, 2
*!*	  ' This determines the format of the time to be returned
*!*	  ' Type 0: 1d 4h 15m 47s
*!*	  ' Type 1: 1 day, 4:15:47
*!*	  ' Type 2: 1 day 4hrs 15mins 47secs
*!*	  ' Type else: Defaults to type 0
*!*
*!*	  Dim Seconds
*!*	  Dim Minutes
*!*	  Dim Hours
*!*	  Dim Days
*!*	  Dim DayString As String
*!*	  Dim HourString As String
*!*	  Dim MinuteString As String
*!*	  Dim SecondString As String
*!*
*!*	  Seconds = Int(TotalSeconds Mod 60)
*!*	  Minutes = Int(TotalSeconds \ 60 Mod 60)
*!*	  Hours = Int(TotalSeconds \ 3600 Mod 24)
*!*	  Days = Int(TotalSeconds \ 3600 \ 24)

*!*	  Select Case UpFormat
*!*	    Case 0
*!*	      DayString = "d "
*!*	      HourString = "h "
*!*	      MinuteString = "m "
*!*	      SecondString = "s"
*!*	    Case 1
*!*	      If Days = 1 Then DayString = " day, " _
*!*	      Else: DayString = " days, "
*!*	      HourString = ":"
*!*	      MinuteString = ":"
*!*	      SecondString = ""
*!*	    Case 2
*!*	      If Days = 1 Then DayString = " day " _
*!*	      Else: DayString = " days, "
*!*	      If Hours = 1 Then HourString = "hr " _
*!*	      Else: HourString = "hrs "
*!*	      If Minutes = 1 Then MinuteString = "min " _
*!*	      Else: MinuteString = "mins "
*!*	      If Seconds = 1 Then SecondString = "sec " _
*!*	      Else: SecondString = "secs"
*!*	    Case Else
*!*	      DayString = "d "
*!*	      HourString = "h "
*!*	      MinuteString = "m "
*!*	      SecondString = "s"
*!*	  End Select
*!*
*!*	  Select Case Days
*!*	    Case 0
*!*	      Duration = Format(Hours, "0") & HourString & _
*!*	            Format(Minutes, "00") & MinuteString & _
*!*	             Format(Seconds, "00") & SecondString
*!*	    Case Else
*!*	      Duration = Days & DayString & _
*!*	          Format(Hours, "0") & HourString & Format _
*!*	          (Minutes, "00") & MinuteString & _
*!*	           Format(Seconds, "00") & SecondString
*!*	    End Select
*!*
*!*	End Function
