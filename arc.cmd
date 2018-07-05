@ECHO OFF
SETLOCAL ENABLEEXTENSIONS
REM SET me=%~n0
SET arc_dir=%~dp0

racket -t "%arc_dir%boot.rkt" -e "(anarki-windows-cli)" -- %*

ENDLOCAL
