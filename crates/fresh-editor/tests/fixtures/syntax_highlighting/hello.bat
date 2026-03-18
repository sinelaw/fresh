@echo off
REM Batch file syntax highlighting test
SET NAME=World
SET COUNT=0

IF "%NAME%"=="" (
    SET NAME=Default
)

:LOOP
IF %COUNT% GEQ 5 GOTO END
ECHO Hello, %NAME%! Count: %COUNT%
SET /A COUNT+=1
GOTO LOOP

:END
ECHO Done.
PAUSE
