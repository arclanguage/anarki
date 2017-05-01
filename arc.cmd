@ECHO OFF
SETLOCAL ENABLEEXTENSIONS
REM SET me=%~n0
SET arc_dir=%~dp0

SET optionkey=%1
IF "%optionkey%"=="/h" (
  ECHO arc [/h] [^<files^>]
  ECHO.
  ECHO OPTIONS
  ECHO   /h
  ECHO     Print help and exit
  ECHO.
  ECHO   ^<files^>
  ECHO     Don't start up a REPL; instead, execute the files one by one. When the last file finishes executing, exit Arc.
  ECHO.
  ECHO EXAMPLES
  ECHO   Start the Arc REPL:
  ECHO     arc
  ECHO   Run the files "first-file-to-run.arc" and "second-file-to-run.arc":
  ECHO     arc first-file-to-run.arc second-file-to-run.arc
  EXIT /B 0
)

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

racket -f "%arc_dir%/as.scm" %*

ENDLOCAL
