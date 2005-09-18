/*
 * $Id: movie.i,v 1.1 2005-09-18 22:06:01 dhmunro Exp $
 * Support functions for making animated sequences.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func movie(draw_frame, time_limit, min_interframe, bracket_time)
/* DOCUMENT movie, draw_frame
         or movie, draw_frame, time_limit
         or movie, draw_frame, time_limit, min_interframe
     runs a movie based on the given DRAW_FRAME function.  The movie
     stops after a total elapsed time of TIME_LIMIT seconds, which
     defaults to 60 (one minute), or when the DRAW_FRAME function
     returns zero.

     func draw_frame(i)
     {
       // Input argument i is the frame number.
       // draw_frame should return non-zero if there are more
       // frames in this movie.  A zero return will stop the
       // movie.
       // draw_frame must NOT include any fma command if the
       // making_movie variable is set (movie sets this variable
       // before calling draw_frame)
     }

     If MIN_INTERFRAME is specified, a pauses will be added as
     necessary to slow down the movie.  MIN_INTERFRAME is a time
     in seconds (default 0).

     The keyword bracket_time= (again a time in seconds) can be
     used to adjust the duration of the pauses after the first
     and last frames.  It may also be a two element array [beg, end].
     If the pause at the end is greater than five seconds, you will
     be prompted to explain that hitting <RETURN> will abort the final
     pause.

     If every frame of your movie has the same limits, use the
     limits command to fix the limits before you call movie.

   BUG:  If you hit <RETURN> to start a movie early, it will not
         pause at the end of the movie at all.  You probably should
         not use long initial pauses.

   SEE ALSO: movie_stats
 */
{
  if (is_void(time_limit)) time_limit= 60.0;
  if (is_void(min_interframe)) min_interframe= 0.0;
  if (is_void(bracket_time)) bracket_time= [2.,2.];
  else if (numberof(bracket_time)<2) bracket_time= array(bracket_time,2);

  elapsed= this_frame= array(0.0, 3);

  window, wait=1;  /* make sure window is ready to draw */
  fma;             /* clear out any existing picture */
  animate, 1;
  making_movie= 1;

  i= 0;
  timer, elapsed;
  elapsed0= elapsed;
  more= draw_frame(++i);
  fma;
  timer, elapsed, this_frame;
  wait= bracket_time(1)-this_frame(3);
  waited= waited0= 0.0;
  if (wait>0) {
    if (wait>5)
      write,
        format="Movie starts in %.0f secs, or when you hit <RETURN>\n", wait;
    pause, long(1000.*wait);
    waited0+= wait;
  } else {
    wait= min_interframe-this_frame(3);
    if (wait>0) {
      pause, long(1000.*wait);
      waited+= wait;
    }
  }

  while (more) {
    this_frame()= 0.0;
    more= draw_frame(++i);
    fma;
    timer, elapsed, this_frame;
    if (!more || (elapsed(3)-elapsed0(3))>time_limit) break;
    wait= min_interframe-this_frame(3);
    if (wait>0) {
      pause, long(1000.*wait);
      waited+= wait;
    }
  }

  wait= bracket_time(2)-this_frame(3);
  if (wait>0) {
    if (wait>5) {
      write,
        format="Holding last frame for %.0f secs, or hit <RETURN>\n", wait;
      pause, 100;  /* huh? */
    }
    pause, long(1000.*wait);
    waited0+= wait;
  }
  timer, elapsed;

  animate, 0;

  extern movie_timing;
  movie_timing= grow(elapsed-elapsed0, i, waited, waited0);
}

func movie_stats(timing)
/* DOCUMENT movie_stats
         or movie_stats, timing
     prints statistics from the last movie command, or from the
     command which produced TIMING.  TIMING is the contents of the
     movie_timing external variable after the movie command completes.

   SEE ALSO: movie
 */
{
  if (is_void(timing)) timing= movie_timing;
  cpu= timing(1)+timing(2);
  wall= timing(3);
  nframes= long(timing(4));
  waited= timing(5)+timing(6);
  wait= timing(5);

  write, format="  Wall(sec)  Wait(sec)  CPU(sec)%s\n", "";
  write, format="  %9.3f  %9.3f  %8.3f   %ld frames\n",
         wall, waited, cpu, nframes;
  write, format="  %9.3f  %9.3f  %8.3f   per frame\n",
         (wall-waited)/nframes, wait/(nframes>1?nframes-1:1), cpu/nframes;
}
