win_flex --wincompat ourxref.l
IF  %ERRORLEVEL% NEQ 0 (
	ECHO "win_flex failed"
	pause
	EXIT 1
	)
win_bison -d ourxref.y
IF  %ERRORLEVEL% NEQ 0 (
	ECHO "win_bison failed"
	pause
	EXIT 2
	)
pause
EXIT 0