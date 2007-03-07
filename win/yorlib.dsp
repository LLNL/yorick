# Microsoft Developer Studio Project File - Name="yorlib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=yorlib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "yorlib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "yorlib.mak" CFG="yorlib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "yorlib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "yorlib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "yorlib - Win32 Release"

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
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\play" /I "..\yorick" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_WINDOWS" /D "_AFXDLL" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "yorlib - Win32 Debug"

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
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\play" /I "..\yorick" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_WINDOWS" /D "_AFXDLL" /YX /FD /GZ /c
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

# Name "yorlib - Win32 Release"
# Name "yorlib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\yorick\array.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ascio.c
# End Source File
# Begin Source File

SOURCE=..\yorick\autold.c
# End Source File
# Begin Source File

SOURCE=..\yorick\bcast.c
# End Source File
# Begin Source File

SOURCE=..\yorick\binio.c
# End Source File
# Begin Source File

SOURCE=..\yorick\binobj.c
# End Source File
# Begin Source File

SOURCE=..\yorick\binpdb.c
# End Source File
# Begin Source File

SOURCE=..\yorick\binstd.c
# End Source File
# Begin Source File

SOURCE=..\yorick\cache.c
# End Source File
# Begin Source File

SOURCE=..\matrix\cblasy.c
# End Source File
# Begin Source File

SOURCE=..\fft\cfft2.c

!IF  "$(CFG)" == "yorlib - Win32 Release"

# SUBTRACT CPP /O<none>

!ELSEIF  "$(CFG)" == "yorlib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\fft\cfftb.c
# End Source File
# Begin Source File

SOURCE=..\fft\cfftf.c
# End Source File
# Begin Source File

SOURCE=..\fft\cffti.c
# End Source File
# Begin Source File

SOURCE=..\yorick\clog.c
# End Source File
# Begin Source File

SOURCE=..\yorick\convrt.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dbdsqr.c
# End Source File
# Begin Source File

SOURCE=..\yorick\debug.c
# End Source File
# Begin Source File

SOURCE=..\yorick\defmem.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dgecon.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dgels.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dgelss.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dgesv.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dgesv2.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dgesvd.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dgtsv.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dgyor.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dlamc3.c
# End Source File
# Begin Source File

SOURCE=..\matrix\dlasr.c
# End Source File
# Begin Source File

SOURCE=..\yorick\dlsym.c
# End Source File
# Begin Source File

SOURCE=..\yorick\fnctn.c
# End Source File
# Begin Source File

SOURCE=..\yorick\fortrn.c
# ADD CPP /D "f_linkage_"
# End Source File
# Begin Source File

SOURCE=..\yorick\funcdef.c
# End Source File
# Begin Source File

SOURCE=..\yorick\graph.c
# ADD CPP /I "..\gist"
# End Source File
# Begin Source File

SOURCE=..\yorick\graph0.c
# End Source File
# Begin Source File

SOURCE=..\yorick\list.c
# End Source File
# Begin Source File

SOURCE=..\yorick\nonc.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ops.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ops0.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ops1.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ops2.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ops3.c
# End Source File
# Begin Source File

SOURCE=..\yorick\opsv.c
# End Source File
# Begin Source File

SOURCE=..\yorick\parse.c
# End Source File
# Begin Source File

SOURCE=..\yorick\pathfun.c
# End Source File
# Begin Source File

SOURCE=..\yorick\range.c
# End Source File
# Begin Source File

SOURCE=..\fft\roll2.c
# End Source File
# Begin Source File

SOURCE=..\yorick\spawn.c
# End Source File
# Begin Source File

SOURCE=..\yorick\std0.c
# End Source File
# Begin Source File

SOURCE=..\yorick\std1.c
# End Source File
# Begin Source File

SOURCE=..\yorick\std2.c
# End Source File
# Begin Source File

SOURCE=..\yorick\style.c
# ADD CPP /I "..\gist"
# End Source File
# Begin Source File

SOURCE=..\yorick\task.c
# End Source File
# Begin Source File

SOURCE=..\yorick\yapi.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ydata.c
# End Source File
# Begin Source File

SOURCE=..\regexp\yfnmatch.c
# End Source File
# Begin Source File

SOURCE=..\yorick\yhash.c
# End Source File
# Begin Source File

SOURCE=.\yinit.c
# End Source File
# Begin Source File

SOURCE=..\yorick\yinput.c
# End Source File
# Begin Source File

SOURCE=..\yorick\yio.c
# End Source File
# Begin Source File

SOURCE=..\yorick\yorick.c
# End Source File
# Begin Source File

SOURCE=..\yorick\yrdwr.c
# End Source File
# Begin Source File

SOURCE=..\regexp\yregexp.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ystr.c
# ADD CPP /I "..\regexp"
# End Source File
# Begin Source File

SOURCE=.\ywrap.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\yorick\bcast.h
# End Source File
# Begin Source File

SOURCE=..\yorick\binio.h

!IF  "$(CFG)" == "yorlib - Win32 Release"

# Begin Custom Build
InputDir=..\yorick
OutDir=..\Release
InputPath=..\yorick\binio.h

"$(InputDir)\prmtyp.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(OutDir)\numfmt numfmt.h 
	move/y numfmt.h $(InputDir)\prmtyp.h 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "yorlib - Win32 Debug"

# Begin Custom Build
InputDir=..\yorick
OutDir=..\Debug
InputPath=..\yorick\binio.h

"$(InputDir)\prmtyp.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(OutDir)\numfmt numfmt.h 
	move/y numfmt.h $(InputDir)\prmtyp.h 
	
# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\matrix\cblasy.h
# End Source File
# Begin Source File

SOURCE=..\yorick\defmem.h
# End Source File
# Begin Source File

SOURCE=..\matrix\dg.h
# End Source File
# Begin Source File

SOURCE=..\yorick\hash.h
# End Source File
# Begin Source File

SOURCE=..\yorick\parse.h
# End Source File
# Begin Source File

SOURCE=..\yorick\prmtyp.h
# End Source File
# Begin Source File

SOURCE=..\yorick\yapi.h
# End Source File
# Begin Source File

SOURCE=..\yorick\yasync.h
# End Source File
# Begin Source File

SOURCE=..\yorick\ydata.h
# End Source File
# Begin Source File

SOURCE=..\regexp\yfnmatch.h
# End Source File
# Begin Source File

SOURCE=..\yorick\yio.h
# End Source File
# Begin Source File

SOURCE=..\regexp\yregexp.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\makeinit.bat

!IF  "$(CFG)" == "yorlib - Win32 Release"

USERDEP__MAKEI="..\i0\std.i"	"..\i0\graph.i"	"..\i0\matrix.i"	"..\i0\fft.i"	"..\drat\drat.i"	"..\hex\hex.i"	
# Begin Custom Build
OutDir=.\..\Release
ProjDir=.
InputPath=.\makeinit.bat

BuildCmds= \
	$(ProjDir)\makeinit.bat $(OutDir)

"$(ProjDir)\yinit.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(ProjDir)\ywrap.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ELSEIF  "$(CFG)" == "yorlib - Win32 Debug"

USERDEP__MAKEI="..\i0\std.i"	"..\i0\graph.i"	"..\i0\matrix.i"	"..\i0\fft.i"	"..\drat\drat.i"	"..\hex\hex.i"	
# Begin Custom Build
OutDir=.\..\Debug
ProjDir=.
InputPath=.\makeinit.bat

BuildCmds= \
	$(ProjDir)\makeinit.bat $(OutDir)

"$(ProjDir)\yinit.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)

"$(ProjDir)\ywrap.c" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
   $(BuildCmds)
# End Custom Build

!ENDIF 

# End Source File
# End Target
# End Project
