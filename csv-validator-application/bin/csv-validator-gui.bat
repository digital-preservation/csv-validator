@REM
@REM Copyright (c) 2013, The National Archives <digitalpreservation@nationalarchives.gov.uk>
@REM https://www.nationalarchives.gov.uk
@REM
@REM This Source Code Form is subject to the terms of the Mozilla Public
@REM License, v. 2.0. If a copy of the MPL was not distributed with this
@REM file, You can obtain one at http://mozilla.org/MPL/2.0/.
@REM

@ECHO OFF

REM Maximum memory:
REM ---------------
REM This is the maximum memory CSV-Validator can use in megabytes.
REM You can edit this script and set the maximum memory on the line below to set the maximum memory after the "=".
REM If you do not want to make a permanent change but wish to set max memory each time, in the command prompt, before
REM launching the batch file, set the max memory variable (e.g. SET csvValidatorMemory=1024)

REM Edit the following line, remove 'REM' from beginning and set a specific maximum memory to change it permanently.
REM SET csvValidatorMemory=512

REM Location of this script at runtime
SET SCRIPT_HOME=%~dp0

REM Create param for runtime option and set default max memory:
SET RUNTIME_OPTIONS="-Xmx512m"

IF "%csvValidatorMemory%"=="" GOTO LaunchProgram
SET RUNTIME_OPTIONS="-Xmx%csvValidatorMemory%m"

:LaunchProgram
REM For GUI, we are launching the java process using `start` so it launches a separate command prompt.
start "" "${JRE_BIN_PATH}javaw" %RUNTIME_OPTIONS% -jar "%SCRIPT_HOME%/csv-validator-ui-${project.version}.jar"