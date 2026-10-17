@ECHO OFF
:: Builds OtlpSmoke.exe with Delphi 13 into .\Bin (run from any folder).
:: Then: docker compose up, and Bin\OtlpSmoke.exe [endpoint]

SETLOCAL
SET BDS=C:\Program Files (x86)\Embarcadero\Studio\37.0
SET HERE=%~dp0
SET ROOT=%HERE%..\..\..

IF NOT EXIST "%HERE%Bin\dcu" MKDIR "%HERE%Bin\dcu"

"%BDS%\bin\dcc32.exe" -Q -B "-NS:System;Winapi;System.Win" ^
  "-U%ROOT%\Source;%ROOT%\Libs\Neon\Source" ^
  "-I%ROOT%\Source" ^
  "-NU%HERE%Bin\dcu" "-E%HERE%Bin" ^
  "%HERE%OtlpSmoke.dpr"

EXIT /B %ERRORLEVEL%
