@ECHO OFF
SETLOCAL ENABLEEXTENSIONS
REM SET me=%~n0
SET arc_dir=%~dp0

SET optionkey=%1
IF "%optionkey%"=="/h" (
  ECHO arc [/h] [^<file^> [^<file_args^>]]
  ECHO.
  ECHO OPTIONS
  ECHO   /h
  ECHO     Print help and exit
  ECHO.
  ECHO   ^<file^> [^<file_args^>]
  ECHO     Don't start up a REPL; instead, execute the file, passing to it any file_args. When the file finishes executing, exit Arc.
  ECHO.
  ECHO EXAMPLES
  ECHO   Start the Arc REPL:
  ECHO     arc
  ECHO   Run the file "file-to-run.arc", passing to it the argument 3
  ECHO     arc file-to-run.arc 3
  EXIT /B 0
)

SET arglen=0
FOR %%X IN (%*) DO SET /A arglen+=1

IF %arglen% EQU 0 (
  racket -t %arc_dir%boot.scm -e "(tl)"
) ELSE (
  racket -t %arc_dir%boot.scm -e "(aload (vector-ref (current-command-line-arguments) 0))" %*
)

ENDLOCAL
