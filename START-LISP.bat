@echo off
setlocal
cd /d "%~dp0"
rem Launch REPL directly without interactive pauses
powershell -NoLogo -ExecutionPolicy Bypass -File "%~dp0start-lisp.ps1" -NoPrompt -Action repl
endlocal

