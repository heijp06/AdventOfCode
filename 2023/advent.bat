@echo off

if "%~1" == "" goto Usage

set "day=0%1"
set "day=%day:~-2%"

stack new day%day% --resolver https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/21/21.yaml .\advent.hsfiles

goto :eof

:Usage
    echo "advent.bat <nr of day>"
