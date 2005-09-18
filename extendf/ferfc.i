/*
 * ferfc.i
 * simple example of adding a compiled extension to yorick in fortran
 * This is not very portable; the yorick configuration scripts do not
 * figure out how to call Fortran routines from C, so you will need to
 * do that on every platform before you can make this work.
 * Read extend/cerfc.i for a summary of the process.  The only difference
 * here is that you will need to edit the automatically generated Makefile
 * by hand in order to add the PKG_DEPLIBS required to link a Fortran
 * program on your platform.
 */

plug_in, "ferfc";

func ferfc(x)
/* DOCUMENT ferfc(x)
     returns the complementary error function 1-erf with fractional
     error less than 1.2e-7 everywhere.
   SEE ALSO: erfc
 */
{
  /* A simple interpreted wrapper can generate additional arguments
     necessary for the compiled function.
     In this case, the result of the calculation is an array, which
     is an input parameter to the compiled function.
     The length and dimensions of the result array are the same as the
     input x array.
   */
  y= array(double, dimsof(x));
  raw_ferfc, y, x, numberof(x);
  return y;
}

extern raw_ferfc;
/* PROTOTYPE FORTRAN
   void ferfc(double array y, double array x, int array n)
 */

/* The PROTOTYPE FORTRAN comment:
   (1) attaches the compiled function ferfc to the interpreted function
       raw_ferfc; the name is adjusted according to the FORTRAN_LINKAGE
       convention specified in Y_HOME/Maketmpl
   (2) generates wrapper code for ferfc that converts the data types
       to those shown in the comment -- for output variables such as
       y in this case, it is the responsibility of the interpreted caller
       (func ferf above) to ensure that no conversion is necessary
       (otherwise the result will go to a temporary array and be discarded)
   (3) note that all arguments to Fortran functions need the word "array"
       ("*" in the corresponding ANSI C prototype)
   (4) don't bother to try passing strings to Fortran; write yourself a
       wrapper that doesn't use string arguments
 */
