win_flex --wincompat ada.l
IF  %ERRORLEVEL% NEQ 0 (
	ECHO "win_flex failed"
	EXIT 1
	)
win_bison -d ada.y
IF  %ERRORLEVEL% NEQ 0 (
	ECHO "win_bison failed"
	EXIT 2
	)
EXIT 0