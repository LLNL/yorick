# Microsoft Developer Studio Project File - Name="bench" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=bench - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "bench.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "bench.mak" CFG="bench - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "bench - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "bench - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "bench - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\Release"
# PROP BASE Intermediate_Dir "..\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\Release"
# PROP Intermediate_Dir "..\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\play" /I "..\play\win" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "NO_EXP10" /D "_AFXDLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 /nologo /subsystem:windows /machine:I386 /nodefaultlib:"libcd" /libpath:"Release"

!ELSEIF  "$(CFG)" == "bench - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\Debug"
# PROP BASE Intermediate_Dir "..\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\Debug"
# PROP Intermediate_Dir "..\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\play" /I "..\play\win" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "NO_EXP10" /D "_AFXDLL" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 /nologo /subsystem:windows /map /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "bench - Win32 Release"
# Name "bench - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\play\all\alarms.c
# End Source File
# Begin Source File

SOURCE=..\gist\bench.c
# End Source File
# Begin Source File

SOURCE=..\play\all\bitlrot.c
# End Source File
# Begin Source File

SOURCE=..\play\all\bitmrot.c
# End Source File
# Begin Source File

SOURCE=..\play\all\bitrev.c
# End Source File
# Begin Source File

SOURCE=..\gist\cgm.c
# End Source File
# Begin Source File

SOURCE=..\gist\clip.c
# End Source File
# Begin Source File

SOURCE=..\play\win\clips.c
# End Source File
# Begin Source File

SOURCE=..\play\win\conterm.c
# End Source File
# Begin Source File

SOURCE=..\play\win\cursors.c
# End Source File
# Begin Source File

SOURCE=..\play\win\dir.c
# End Source File
# Begin Source File

SOURCE=..\gist\draw.c
# End Source File
# Begin Source File

SOURCE=..\gist\draw0.c
# End Source File
# Begin Source File

SOURCE=..\play\win\ellipse.c
# End Source File
# Begin Source File

SOURCE=..\gist\engine.c
# End Source File
# Begin Source File

SOURCE=..\play\win\feep.c
# End Source File
# Begin Source File

SOURCE=..\play\win\files.c
# End Source File
# Begin Source File

SOURCE=..\gist\gcntr.c
# End Source File
# Begin Source File

SOURCE=..\play\win\getdc.c
# End Source File
# Begin Source File

SOURCE=..\gist\gist.c
# End Source File
# Begin Source File

SOURCE=..\gist\gread.c
# End Source File
# Begin Source File

SOURCE=..\gist\gtext.c
# End Source File
# Begin Source File

SOURCE=..\play\win\handler.c
# End Source File
# Begin Source File

SOURCE=..\play\all\hash.c
# End Source File
# Begin Source File

SOURCE=..\play\all\hash0.c
# End Source File
# Begin Source File

SOURCE=..\play\all\hashctx.c
# End Source File
# Begin Source File

SOURCE=..\play\all\hashid.c
# End Source File
# Begin Source File

SOURCE=..\gist\hlevel.c
# End Source File
# Begin Source File

SOURCE=..\play\win\mfcapp.cpp
# End Source File
# Begin Source File

SOURCE=..\play\win\mfcmain.cpp
# End Source File
# Begin Source File

SOURCE=..\play\win\mfcterm.cpp
# End Source File
# Begin Source File

SOURCE=..\play\all\mm.c
# End Source File
# Begin Source File

SOURCE=..\play\all\mminit.c
# End Source File
# Begin Source File

SOURCE=..\play\all\mmtest.c
# End Source File
# Begin Source File

SOURCE=..\play\all\p595.c
# End Source File
# Begin Source File

SOURCE=..\play\win\pals.c
# End Source File
# Begin Source File

SOURCE=..\play\win\pathnm.c
# End Source File
# Begin Source File

SOURCE=..\play\win\pcell.c
# End Source File
# Begin Source File

SOURCE=..\play\win\pfill.c
# End Source File
# Begin Source File

SOURCE=..\play\win\plines.c
# End Source File
# Begin Source File

SOURCE=..\play\all\pmemcpy.c
# End Source File
# Begin Source File

SOURCE=..\play\win\points.c
# End Source File
# Begin Source File

SOURCE=..\play\win\prect.c
# End Source File
# Begin Source File

SOURCE=..\gist\ps.c
# End Source File
# Begin Source File

SOURCE=..\play\win\pscr.c
# End Source File
# Begin Source File

SOURCE=..\play\all\pstrcpy.c
# End Source File
# Begin Source File

SOURCE=..\play\all\pstrncat.c
# End Source File
# Begin Source File

SOURCE=..\play\win\ptext.c
# End Source File
# Begin Source File

SOURCE=..\play\win\pwin.c
# End Source File
# Begin Source File

SOURCE=..\play\win\sigseh.c
# End Source File
# Begin Source File

SOURCE=..\gist\tick.c
# End Source File
# Begin Source File

SOURCE=..\gist\tick60.c
# End Source File
# Begin Source File

SOURCE=..\play\win\timeu.c
# End Source File
# Begin Source File

SOURCE=..\play\win\timew.c
# End Source File
# Begin Source File

SOURCE=..\play\win\usernm.c
# End Source File
# Begin Source File

SOURCE=..\play\win\wpoll.c
# End Source File
# Begin Source File

SOURCE=..\play\win\wstdio.c
# End Source File
# Begin Source File

SOURCE=..\gist\xbasic.c
# End Source File
# Begin Source File

SOURCE=..\gist\xfancy.c
# End Source File
# Begin Source File

SOURCE=.\yorick.rc
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /i "..\play\win"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\gist\cgm.h
# End Source File
# Begin Source File

SOURCE=..\gist\clip.h
# End Source File
# Begin Source File

SOURCE=..\play\win\config.h
# End Source File
# Begin Source File

SOURCE=..\gist\draw.h
# End Source File
# Begin Source File

SOURCE=..\gist\engine.h
# End Source File
# Begin Source File

SOURCE=..\play\plugin.h
# End Source File
# Begin Source File

SOURCE=..\gist\gist.h
# End Source File
# Begin Source File

SOURCE=..\gist\gtext.h
# End Source File
# Begin Source File

SOURCE=..\gist\hlevel.h
# End Source File
# Begin Source File

SOURCE=..\play\win\mfcapp.h
# End Source File
# Begin Source File

SOURCE=..\play\win\mfcres.h
# End Source File
# Begin Source File

SOURCE=..\play\win\mfcterm.h
# End Source File
# Begin Source File

SOURCE=..\play\play.h
# End Source File
# Begin Source File

SOURCE=..\play\win\playw.h
# End Source File
# Begin Source File

SOURCE=..\gist\ps.h
# End Source File
# Begin Source File

SOURCE=..\play\pstdio.h
# End Source File
# Begin Source File

SOURCE=..\play\pstdlib.h
# End Source File
# Begin Source File

SOURCE=..\gist\xbasic.h
# End Source File
# Begin Source File

SOURCE=..\gist\xfancy.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\yorick.ico
# End Source File
# End Group
# End Target
# End Project
