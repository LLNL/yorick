/*
 * $Id: yasync.h,v 1.1 2005-09-18 22:04:15 dhmunro Exp $
 * Headers describing a simple memory management package for
 * Yorick-derived programs, which is "safe" against asynchronous
 * interrupts.  That is, memory allocated with this package will
 * be tracked and freed, even if the subroutine which allocates
 * the memory never finishes due to an asynchronous interrupt.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef YASYNC_H
#define YASYNC_H

extern void *YAsyncBytes(long n);    /* use instead of malloc */
extern void YAsyncFree(void *ptr);   /* use instead of free */

extern void YAsyncWork(void);
/* Preface a series of calls to YAsyncBytes, which are allocating
   temporary working space, with a call to YAsyncWork.
   If anything allocated with YAsyncBytes after the previous call
   to YAsyncWork has not been freed by a call to YAsyncFree, this
   call to YAsyncWork will free those arrays.  (You can therefore
   place a second call to YAsyncWork at the end of the routine that
   needed the temporary space in order to clean up, instead of
   matching calls to YAsyncFree.  I personally don't care for this
   programming style -- I like to see the matching frees.)
   Therefore, a routine which calls YAsyncWork, and thereafter uses
   YAsyncBytes to allocate workspace, need not worry about recording
   the whereabouts of its arrays to protect against interrupts.
 */

extern void YAsyncError(const char *msg);
/* This routine cleans up all memory allocated since the most recent
   YAsyncWork, then prints the given error message (or a standard
   message if msg==0).
   YAsyncError NEVER RETURNS TO THE CALLER.
 */

extern void YWarning(const char *msg);
/* Prints the warning message, but takes no other action.
   (Duplicates declaration in binio.h.)
   Use this very sparingly, if at all.
 */

#endif
