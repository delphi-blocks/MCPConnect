SET _TARGET=%1
IF [%1] == [] (SET _TARGET="Make")

SET _CONFIG=%2
IF [%2] == [] (SET _CONFIG="Release")

SET _PLATFORM=%3
IF [%3] == [] (SET _PLATFORM="Win32")

SET BUILDTARGET="/t:%_TARGET%"
SET BUILDCONFIG="/p:config=%_CONFIG%"
SET BUILDPLATFORM="/p:platform=%_PLATFORM%"

ECHO Compile...
@ECHO OFF
msbuild MCPConnect.Tests.Framework.dproj %BUILDTARGET% %BUILDCONFIG% %BUILDPLATFORM% /property:DCC_Define=CI 

IF %ERRORLEVEL% NEQ 0 (
  
  ECHO =================================================
  ECHO ===    MCPConnect Tests Failed to Compile     ===
  ECHO =================================================
  EXIT /B 1
  
) ELSE (
  :: The project's DCC_ExeOutput is ..\Bin - %_PLATFORM%\%_CONFIG% holds only
  :: the .dcu files, so running from there silently re-runs a stale binary.
  ..\Bin\MCPConnect.Tests.Framework.exe
)    
