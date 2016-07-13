@echo off

setlocal

set SBT_OPTS=%SBT_OPTS% -Djline.WindowsTerminal.directConsole=false -Dfile.encoding=UTF8
sbt %*
