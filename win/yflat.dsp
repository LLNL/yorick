# Microsoft Developer Studio Project File - Name="yflat" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=yflat - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "yflat.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "yflat.mak" CFG="yflat - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "yflat - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "yflat - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "yflat - Win32 Release"

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
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\play" /I "..\play\win" /I "..\gist" /I "..\yorick" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_AFXDLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /machine:I386
# ADD LINK32 /nologo /subsystem:windows /machine:I386

!ELSEIF  "$(CFG)" == "yflat - Win32 Debug"

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
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "..\play" /I "..\play\win" /I "..\gist" /I "..\yorick" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_AFXDLL" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 /nologo /subsystem:windows /map /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "yflat - Win32 Release"
# Name "yflat - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\play\all\alarms.c
# End Source File
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

SOURCE=..\play\all\bitlrot.c
# End Source File
# Begin Source File

SOURCE=..\play\all\bitmrot.c
# End Source File
# Begin Source File

SOURCE=..\play\all\bitrev.c
# End Source File
# Begin Source File

SOURCE=..\drat\bound.c
# End Source File
# Begin Source File

SOURCE=..\yorick\cache.c
# End Source File
# Begin Source File

SOURCE=..\matrix\cblasy.c
# End Source File
# Begin Source File

SOURCE=..\fft\cfft2.c

!IF  "$(CFG)" == "yflat - Win32 Release"

# SUBTRACT CPP /O<none>

!ELSEIF  "$(CFG)" == "yflat - Win32 Debug"

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

SOURCE=..\gist\cgm.c
# End Source File
# Begin Source File

SOURCE=..\gist\clip.c
# End Source File
# Begin Source File

SOURCE=..\play\win\clips.c
# End Source File
# Begin Source File

SOURCE=..\yorick\clog.c
# End Source File
# Begin Source File

SOURCE=..\play\win\conterm.c
# End Source File
# Begin Source File

SOURCE=..\yorick\convrt.c
# End Source File
# Begin Source File

SOURCE=..\play\win\cursors.c
# End Source File
# Begin Source File

SOURCE=..\matrix\cxerbla.c
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

SOURCE=..\play\win\dir.c
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

SOURCE=..\drat\drat.c
# End Source File
# Begin Source File

SOURCE=..\gist\draw.c
# ADD CPP /D "NO_EXP10"
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

SOURCE=..\yorick\fnctn.c
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

SOURCE=..\yorick\graph.c
# End Source File
# Begin Source File

SOURCE=..\yorick\graph0.c
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

SOURCE=..\hex\hex.c
# End Source File
# Begin Source File

SOURCE=..\hex\hex24.c
# End Source File
# Begin Source File

SOURCE=..\hex\hex5.c
# End Source File
# Begin Source File

SOURCE=..\gist\hlevel.c
# End Source File
# Begin Source File

SOURCE=..\hex\hydram.c
# End Source File
# Begin Source File

SOURCE=..\yorick\list.c
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

SOURCE=..\play\all\p595.c
# End Source File
# Begin Source File

SOURCE=..\play\win\pals.c
# End Source File
# Begin Source File

SOURCE=..\yorick\parse.c
# End Source File
# Begin Source File

SOURCE=..\yorick\pathfun.c
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

SOURCE=..\yorick\range.c
# End Source File
# Begin Source File

SOURCE=..\hex\regul.c
# End Source File
# Begin Source File

SOURCE=..\fft\roll2.c
# End Source File
# Begin Source File

SOURCE=..\play\win\sigseh.c
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

SOURCE=..\hex\store.c
# End Source File
# Begin Source File

SOURCE=..\yorick\style.c
# End Source File
# Begin Source File

SOURCE=..\yorick\task.c
# End Source File
# Begin Source File

SOURCE=..\gist\tick.c
# ADD CPP /D "NO_EXP10"
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

SOURCE=..\hex\tools.c
# End Source File
# Begin Source File

SOURCE=..\drat\track.c
# End Source File
# Begin Source File

SOURCE=..\drat\trans.c
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
# ADD CPP /D "NO_EXP10"
# End Source File
# Begin Source File

SOURCE=..\yorick\ydata.c
# End Source File
# Begin Source File

SOURCE=..\drat\ydrat.c
# End Source File
# Begin Source File

SOURCE=..\regexp\yfnmatch.c
# End Source File
# Begin Source File

SOURCE=..\yorick\yhash.c
# End Source File
# Begin Source File

SOURCE=..\hex\yhex.c
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

SOURCE=.\yorick.rc
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409 /i "..\play\win"
# End Source File
# Begin Source File

SOURCE=..\yorick\yrdwr.c
# End Source File
# Begin Source File

SOURCE=..\regexp\yregexp.c
# End Source File
# Begin Source File

SOURCE=..\yorick\ystr.c
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

!IF  "$(CFG)" == "yflat - Win32 Release"

# Begin Custom Build
InputDir=..\yorick
OutDir=..\Release
InputPath=..\yorick\binio.h

"$(InputDir)\prmtyp.h" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	$(OutDir)\numfmt numfmt.h 
	move/y numfmt.h $(InputDir)\prmtyp.h 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "yflat - Win32 Debug"

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

SOURCE=..\drat\bound.h
# End Source File
# Begin Source File

SOURCE=..\matrix\cblasy.h
# End Source File
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

SOURCE=..\yorick\defmem.h
# End Source File
# Begin Source File

SOURCE=..\matrix\dg.h
# End Source File
# Begin Source File

SOURCE=..\drat\drat.h
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

SOURCE=..\yorick\hash.h
# End Source File
# Begin Source File

SOURCE=..\hex\hex.h
# End Source File
# Begin Source File

SOURCE=..\gist\hlevel.h
# End Source File
# Begin Source File

SOURCE=..\hex\hydram.h
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

SOURCE=..\yorick\parse.h
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

SOURCE=..\play\pmin.h
# End Source File
# Begin Source File

SOURCE=..\yorick\prmtyp.h
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

SOURCE=..\hex\regul.h
# End Source File
# Begin Source File

SOURCE=..\hex\tools.h
# End Source File
# Begin Source File

SOURCE=..\drat\track.h
# End Source File
# Begin Source File

SOURCE=..\drat\trans.h
# End Source File
# Begin Source File

SOURCE=..\gist\xbasic.h
# End Source File
# Begin Source File

SOURCE=..\gist\xfancy.h
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
# Begin Source File

SOURCE=..\play\yversion.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\yorick.ico
# End Source File
# End Group
# Begin Source File

SOURCE=.\makeinit.bat

!IF  "$(CFG)" == "yflat - Win32 Release"

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

!ELSEIF  "$(CFG)" == "yflat - Win32 Debug"

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
# Begin Source File

SOURCE=.\README
# End Source File
# End Target
# End Project
