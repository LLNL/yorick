# Microsoft Developer Studio Project File - Name="play" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=play - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "play.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "play.mak" CFG="play - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "play - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "play - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "play - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\Release"
# PROP BASE Intermediate_Dir "..\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\Release"
# PROP Intermediate_Dir "..\Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\play" /I "..\play\win" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "play - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\Debug"
# PROP BASE Intermediate_Dir "..\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\Debug"
# PROP Intermediate_Dir "..\Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\play" /I "..\play\win" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "play - Win32 Release"
# Name "play - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\play\any\alarms.c
# End Source File
# Begin Source File

SOURCE=..\play\any\bitlrot.c
# End Source File
# Begin Source File

SOURCE=..\play\any\bitmrot.c
# End Source File
# Begin Source File

SOURCE=..\play\any\bitrev.c
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

SOURCE=..\play\win\ellipse.c
# End Source File
# Begin Source File

SOURCE=..\play\win\feep.c
# End Source File
# Begin Source File

SOURCE=..\play\win\files.c
# End Source File
# Begin Source File

SOURCE=..\play\win\getdc.c
# End Source File
# Begin Source File

SOURCE=..\play\win\handler.c
# End Source File
# Begin Source File

SOURCE=..\play\any\hash.c
# End Source File
# Begin Source File

SOURCE=..\play\any\hash0.c
# End Source File
# Begin Source File

SOURCE=..\play\any\hashctx.c
# End Source File
# Begin Source File

SOURCE=..\play\any\hashid.c
# End Source File
# Begin Source File

SOURCE=..\play\any\mm.c
# End Source File
# Begin Source File

SOURCE=..\play\any\mminit.c
# End Source File
# Begin Source File

SOURCE=..\play\any\p595.c
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

SOURCE=..\play\any\pmemcpy.c
# End Source File
# Begin Source File

SOURCE=..\play\win\points.c
# End Source File
# Begin Source File

SOURCE=..\play\win\prect.c
# End Source File
# Begin Source File

SOURCE=..\play\win\pscr.c
# End Source File
# Begin Source File

SOURCE=..\play\any\pstrcpy.c
# End Source File
# Begin Source File

SOURCE=..\play\any\pstrncat.c
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

SOURCE=..\play\win\timeu.c
# End Source File
# Begin Source File

SOURCE=..\play\win\timew.c
# End Source File
# Begin Source File

SOURCE=..\play\win\usernm.c
# End Source File
# Begin Source File

SOURCE=..\play\win\wdl.c
# End Source File
# Begin Source File

SOURCE=..\play\win\wpoll.c
# End Source File
# Begin Source File

SOURCE=..\play\win\wspawn.c
# End Source File
# Begin Source File

SOURCE=..\play\win\wstdio.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\play\win\config.h
# End Source File
# Begin Source File

SOURCE=..\play\plugin.h
# End Source File
# Begin Source File

SOURCE=..\play\phash.h
# End Source File
# Begin Source File

SOURCE=..\play\play.h
# End Source File
# Begin Source File

SOURCE=..\play\win\playw.h
# End Source File
# Begin Source File

SOURCE=..\play\pstdio.h
# End Source File
# Begin Source File

SOURCE=..\play\pstdlib.h
# End Source File
# End Group
# End Target
# End Project
