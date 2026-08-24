@ECHO OFF

:: Delphi 13.0 Florence
@SET BDS=C:\Program Files (x86)\Embarcadero\Studio\37.0
@SET BDSINCLUDE=%BDS%\include
@SET BDSCOMMONDIR=C:\Users\Public\Documents\Embarcadero\Studio\37.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v4.0.30319
@SET FrameworkVersion=v4.5
@SET FrameworkSDKDir=
@SET PATH=%FrameworkDir%;%FrameworkSDKDir%;%BDS%\bin;%BDS%\bin64;%PATH%
@SET LANGDIR=EN
@SET PLATFORM=
@SET PlatformSDK=
::::::::::::::::::::::::::::::::

call MCPConnect.Tests.Build.Common.bat