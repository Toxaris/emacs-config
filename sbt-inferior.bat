@echo off

setlocal

set SBT_OPTS=%SBT_OPTS% -Djline.WindowsTerminal.directConsole=false
sbt %*
