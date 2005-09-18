/*
 * $Id: maked.i,v 1.1 2005-09-18 22:05:08 dhmunro Exp $
 * Yorick script to genrate alphabetized listings of all help
 * command documentation.
 */

#include "mkdoc.i"

/* assume current working directory is top level of distribution */
mkdoc, "../i0/std.i", "std.doc";
mkdoc, "../i0/graph.i", "graph.doc";
mkdoc, ["../i0/fft.i","../i0/matrix.i"], "math.doc";
mkdoc, "../drat/drat.i", "drat.doc";
mkdoc, "../hex/hex.i", "hex.doc";

/* go ahead and copy ../i0/include/README as a .doc also */
f= open("../i/README");
g= create("library.doc");
do {
  lines= rdline(f, 1000);
  n= sum(lines!=string(0));
  if (n) write, g, lines(1:n), format="%s\n";
} while (n==1000);
f= g= [];

quit;
