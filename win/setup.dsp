# Microsoft Developer Studio Project File - Name="setup" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Generic Project" 0x010a

CFG=setup - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "setup.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "setup.mak" CFG="setup - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "setup - Win32 Release" (based on "Win32 (x86) Generic Project")
!MESSAGE "setup - Win32 Debug" (based on "Win32 (x86) Generic Project")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
MTL=midl.exe

!IF  "$(CFG)" == "setup - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "..\Release"
# PROP BASE Intermediate_Dir "..\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\Release"
# PROP Intermediate_Dir "..\Release"
# PROP Target_Dir ""

!ELSEIF  "$(CFG)" == "setup - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "..\Debug"
# PROP BASE Intermediate_Dir "..\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\Debug"
# PROP Intermediate_Dir "..\Debug"
# PROP Target_Dir ""

!ENDIF 

# Begin Target

# Name "setup - Win32 Release"
# Name "setup - Win32 Debug"
# Begin Group "SRC-files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\drat\drat.i

!IF  "$(CFG)" == "setup - Win32 Release"

# Begin Custom Build
InputPath=..\drat\drat.i
InputName=drat

"..\i0\$(InputName)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy/y $(InputPath) ..\i0

# End Custom Build

!ELSEIF  "$(CFG)" == "setup - Win32 Debug"

# Begin Custom Build
InputPath=..\drat\drat.i
InputName=drat

"..\i0\$(InputName)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy/y $(InputPath) ..\i0

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\hex\hex.i

!IF  "$(CFG)" == "setup - Win32 Release"

# Begin Custom Build
InputPath=..\hex\hex.i
InputName=hex

"..\i0\$(InputName)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy/y $(InputPath) ..\i0

# End Custom Build

!ELSEIF  "$(CFG)" == "setup - Win32 Debug"

# Begin Custom Build
InputPath=..\hex\hex.i
InputName=hex

"..\i0\$(InputName)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy/y $(InputPath) ..\i0

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\hex\ylmdec.i

!IF  "$(CFG)" == "setup - Win32 Release"

# Begin Custom Build
InputPath=..\hex\ylmdec.i
InputName=ylmdec

"..\i\$(InputName)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy/y $(InputPath) ..\i

# End Custom Build

!ELSEIF  "$(CFG)" == "setup - Win32 Debug"

# Begin Custom Build
InputPath=..\hex\ylmdec.i
InputName=ylmdec

"..\i\$(InputName)" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	copy/y $(InputPath) ..\i

# End Custom Build

!ENDIF 

# End Source File
# End Group
# End Target
# End Project
