/*
 * $Id: mpy.h,v 1.1 2010-02-28 21:49:21 dhmunro Exp $
 * Public mpy compiled interface.
 */
/* Copyright (c) 2010, David H. Munro
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef MPY_H
#define MPY_H

/* yapi.h depends on plugin.h */
#include "yapi.h"

BEGIN_EXTERN_C

/* for use in mpyfile.c on_include function */
PLUG_API void mpy_initialize(int *pargc, char **pargv[]);
PLUG_API int mpy_size, mpy_rank, mpy_nfan, mpy_parallel;

PLUG_API void mpy_bcast(int nmsgs);
/* nmsgs>0 --> broadcast top nmsgs stack elements (arrays) on rank 0
 *             to all ranks (stack unchanged on rank 0, else nmsgs longer)
 * nmsgs<=0 --> send acknowledge from leaf ranks to rank 0
 */

#ifdef USE_MYPROBE
/* work around catamount bug/feature broken MPI_Probe */
#undef MPI_Probe
#define MPI_Probe my_mpi_Probe
PLUG_API int my_mpi_Probe(int source, int tag,
			  MPI_Comm comm, MPI_Status *status);
#endif

END_EXTERN_C
#endif
