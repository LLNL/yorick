# Fortran configuations (not all verified)

# Solaris
FC=f77
FORTRAN_LIBS=-lF77
FORTRAN_LINKAGE=-Df_linkage_

# SunOS 4
FC=f77
FORTRAN_LIBS=-lM77 -lF77
FORTRAN_LINKAGE=-Df_linkage_

# HPUX
FC=f77
FORTRAN_LIBS=-lcl
FORTRAN_LINKAGE=-Df_linkage

# HPUX + libvec
FC=f77
FORTRAN_LIBS=-lvec -lcl
FORTRAN_LINKAGE=-Df_linkage

# Compaq alpha
FC=f77
FORTRAN_LIBS=-lots
FORTRAN_LINKAGE=-Df_linkage_

# AIX
FC=xlf
FORTRAN_LIBS=-lxlf
FORTRAN_LINKAGE=-Df_linkage_

# SGI
FC=f77
FORTRAN_LIBS=-lf
FORTRAN_LINKAGE=-Df_linkage_

# Linux
FC=g77
FORTRAN_LIBS=-lf2c
FORTRAN_LINKAGE=-Df_linkage_
