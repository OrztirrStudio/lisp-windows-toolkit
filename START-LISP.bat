@echo off
echo Starting Lisp Development Environment...
echo.
echo This will open PowerShell and run your Lisp setup script.
echo.
pause
powershell -ExecutionPolicy Bypass -File "%~dp0start-lisp.ps1"
pause
