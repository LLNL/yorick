/*
 * $Id: mpy.h,v 1.2 2010-03-20 16:41:17 dhmunro Exp $
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

/* mpy_get_next(0) checks for incoming mpy messages without blocking
 *   will not return if mpy error message arrives (calls y_error)
 *   return value not meaningful, non-zero arguments only useful in mpy.c
 */
PLUG_API int mpy_get_next(int block);

#ifdef MPY_MPI_H
/* mpy_control_irecv(request, 0);
 *   sets up mpy control IRecv for use with Waitsome or Testsome
 *   if it ever triggers, you MUST call mpy_errquiet:
 * if (request[0] == MPI_REQUEST_NULL) mpy_errquiet();
 *   mpy_errquiet does not return (calls y_error)
 * mpy_control_irecv(request, 1);
 *   cancels mpy control IRecv, setting request[0]=MPI_REQUEST_NULL
 *   this will not return if an mpy error message arrives
 */
PLUG_API void mpy_control_irecv(MPI_Request *request, int cancel);
PLUG_API void mpy_errquiet(void);
#endif

#ifdef USE_MYPROBE
/* work around catamount bug/feature broken MPI_Probe */
#undef MPI_Probe
#define MPI_Probe my_mpi_Probe
PLUG_API int my_mpi_Probe(int source, int tag,
			  MPI_Comm comm, MPI_Status *status);
#endif

END_EXTERN_C
#endif
