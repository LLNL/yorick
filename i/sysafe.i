/*
 * $Id: sysafe.i,v 1.1 2005-09-18 22:06:10 dhmunro Exp $
 * Replacement for system function safe for use when Yorick gets huge.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func sysafe(command)
/* DOCUMENT sysafe, "command line"
         or system, "command line"
     pass the command line to a UNIX sh (Bourne) shell for execution.
     This requires a fork() system call, which in turn makes a copy of
     the yorick executable in virtual memory before replacing that copy
     with the sh shell.  If yorick has grown to enormous size, the copy
     can bring your machine to its knees or kill it.  If you include
     sysafe.i before yorick grows (before you start the calculation that
     requires the large data arrays), a pipe is opened to an sh which
     remains running, and the original system command is replaced by
     sysafe.  Future system commands will be piped to the already
     running sh, so no dangerous copy operation is required.

     There are four problems with this approach:
       (1) You can't run interactive programs with sysafe, because the
           stdin is from the pipe (sysafe_pipe) instead of the keyboard.
           Attempting to do so may lock up yorick.
       (2) Since the command runs asynchronously now, yorick can't wait
           until it completes, and yorick's prompt will often precede
           the output from the command, unlike using the default system
           function.
       (3) Some typographical errors in commands may kill the sh; since
           you don't start a new one each time, the system command will
           stop working.
       (4) The shorthand $ syntax still uses the dangerous system call;
           you need to call system as an ordinary function for sysafe
           to protect you.

   SEE ALSO: system_orig
 */
{
  write, sysafe_pipe, format="%s\n", command;
  fflush, sysafe_pipe;
}

if (is_void(system_orig)) {
  sysafe_pipe= popen("sh", 1);
  system_orig= system;
  system= sysafe;
}
