/*
 * $Id: pmin.h,v 1.1 2005-09-18 22:05:31 dhmunro Exp $
 * minimally intrusive play event handling (no stdio)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "plugin.h"

BEGIN_EXTERN_C

/* call p_pending_events() to handle all pending
 *   events and expired alarms without blocking
 * call p_wait_while to set a flag and wait for play graphics events
 *   until flag is reset to zero
 * call p_xhandler to set abort_hook and on_exception routines
 *   a less obtrusive alternative to p_handler */
PLUG_API void p_pending_events(void);
PLUG_API void p_wait_while(int *flag);
PLUG_API void p_xhandler(void (*abort_hook)(void),
                         void (*on_exception)(int signal, char *errmsg));
PLUG_API int p_wait_stdin(void);

END_EXTERN_C
