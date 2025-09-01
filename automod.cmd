@echo off
setlocal
if exist "%~dp0lib\luaj-jse-1.0.0.jar" goto :server
md "%~dp0lib" 2> nul
curl -JLso "%~dp0lib\luaj-jse-1.0.0.jar" https://github.com/jpabscale/luaj/releases/download/1.0.0/luaj-jse-1.0.0.jar
:server
set SERVER=--server=true
if not "%~1"=="-s" goto :run
set SERVER=--server=false
:run
scala-cli --suppress-outdated-dependency-warning %SERVER% "%~dp0project.scala" -- %*