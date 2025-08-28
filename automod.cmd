@echo off
setlocal
set SERVER=--server=true
if not "%~1"=="-s" goto :run
set SERVER=--server=false
:run
scala-cli --suppress-outdated-dependency-warning %SERVER% %~dp0project.scala -- %*