@echo off
REM  The contents of this file are subject to the Mozilla Public License
REM  Version 1.1 (the "License"); you may not use this file except in
REM  compliance with the License. You may obtain a copy of the License
REM  at http://www.mozilla.org/MPL/
REM
REM  Software distributed under the License is distributed on an "AS IS"
REM  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
REM  the License for the specific language governing rights and
REM  limitations under the License.
REM
REM  The Original Code is RabbitMQ.
REM
REM  The Initial Developer of the Original Code is GoPivotal, Inc.
REM  Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
REM

setlocal

rem Preserve values that might contain exclamation marks before
rem enabling delayed expansion
set TDP0=%~dp0
set STAR=%*
setlocal enabledelayedexpansion

if "!RABBITMQ_BASE!"=="" (
    set RABBITMQ_BASE=!APPDATA!\RabbitMQ
)

if "!RABBITMQ_USE_LONGNAME!"=="" (
    set RABBITMQ_NAME_TYPE="-sname"
)

if "!RABBITMQ_USE_LONGNAME!"=="true" (
    set RABBITMQ_NAME_TYPE="-name"
)

if "!COMPUTERNAME!"=="" (
    set COMPUTERNAME=localhost
)

if "!RABBITMQ_NODENAME!"=="" (
    set RABBITMQ_NODENAME=rabbit@!COMPUTERNAME!
)

if "!RABBITMQ_MNESIA_BASE!"=="" (
    set RABBITMQ_MNESIA_BASE=!RABBITMQ_BASE!/db
)

if "!RABBITMQ_MNESIA_DIR!"=="" (
    set RABBITMQ_MNESIA_DIR=!RABBITMQ_MNESIA_BASE!/!RABBITMQ_NODENAME!-mnesia
)

if not exist "!ERLANG_HOME!\bin\erl.exe" (
    echo.
    echo ******************************
    echo ERLANG_HOME not set correctly.
    echo ******************************
    echo.
    echo Please either set ERLANG_HOME to point to your Erlang installation or place the
    echo RabbitMQ server distribution in the Erlang lib folder.
    echo.
    exit /B
)

rem rabbitmqctl starts distribution itself, so we need to make sure epmd
rem is running.
"!ERLANG_HOME!\bin\erl.exe" !RABBITMQ_NAME_TYPE! rabbitmqctl-prelaunch-!RANDOM!!TIME:~9! -noinput -eval "erlang:halt()."

"!ERLANG_HOME!\bin\erl.exe" ^
-pa "!TDP0!..\ebin" ^
-noinput ^
-hidden ^
!RABBITMQ_CTL_ERL_ARGS! ^
-sasl errlog_type error ^
-mnesia dir \""!RABBITMQ_MNESIA_DIR:\=/!"\" ^
-s rabbit_control_main ^
-nodename !RABBITMQ_NODENAME! ^
-extra !STAR!

endlocal
endlocal
