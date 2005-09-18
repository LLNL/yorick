/*
 * $Id: bcast.h,v 1.1 2005-09-18 22:03:48 dhmunro Exp $
 * Declare generic broadcast, scatter, and gather functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef BCAST_H
#define BCAST_H

/*--------------------------------------------------------------------------*/

#ifndef NOT_YORICK
#include "ydata.h"

/* Broadcast(dst, ddims, src, sdims, base)
   broadcasts the src array into the dst array (where both src
   and dst array elements are size bytes long), returning 0 on
   success, 1 if sdims is not L-conformable with ddims. */

PLUG_API int Broadcast(void *dst, const Dimension *ddims,
                       void *src, const Dimension *sdims, StructDef *base);

#else
/* Only Scatter and Gather functions are relevant to standalone
   binary file package.  */
#include "binio.h"
#endif

/*--------------------------------------------------------------------------*/

/* Scatter(src, dst, base, number, strider)
   scatters the src array into the dst array, given the strider for
   the dst array and the base StructDef for the objects to be scattered.
   If the strider is 0, the given number of contiguous objects is
   copied.
      x(index list)= expression      */

PLUG_API void Scatter(void *src, void *dst, StructDef *base, long number,
                      const Strider *strider);

/* Gather(dst, src, base, number, strider)
   gathers into the dst array from the src array, given the strider for
   the src array and the base StructDef for the objects to be gathered.
   If the strider is 0, the given number of contiguous objects is
   copied.
      ... evaluate x(index list) ...  */

PLUG_API void Gather(void *dst, void *src, StructDef *base, long number,
                     const Strider *strider);

/* Scatter and Gather have analogues YWrite and YRead when the data
   resides on an IOStream instead of in memory.  CastRead and CastWrite
   are low-level workers for YRead and YWrite which do not perform
   format conversions.  */
PLUG_API void CastWrite(void *src, long dst, StructDef *base, long number,
                        const Strider *strider);
PLUG_API void CastRead(void *dst, long src, StructDef *base, long number,
                       const Strider *strider);

/*--------------------------------------------------------------------------*/
#endif
