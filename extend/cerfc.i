/*
 * cerfc.i
 * simple example of adding a compiled package to yorick
 *
 * See drat/ and hex/ directories for more complete examples
 * (although those are slightly more complicated than usual in order
 * to be able to build them before yorick itself is installed).
 *
 * The scenario is that you are dissatisfied with the performance
 * of the interpreted complementary error function erfc (defined in
 * i/dawson.i); you find it too slow for your needs.  Therefore, you
 * need to write a compiled complementary error function.  The easiest
 * choice of language is C; since yorick itself is written in C.
 *
 * <If you know how to make C call Fortran on your platform, yorick
 * also has support for calling Fortran-77 routines.  In particular,
 * you are responsible for knowing what libraries your Fortran
 * compiler requires you to link against, since the link will be
 * performed by the C compiler.  You are in essence trying to write
 * a library in Fortran that is to be called by C programs; that is
 * a much different proposition than writing a complete Fortran program.
 * The documentation for your Fortran compiler may or may not explain
 * how to do this.  Note that for the externally called routines, you
 * must stick to the Fortran-77 calling sequence or the task is hopeless;
 * there is no (portable) way for a C program to insert all the hidden
 * arguments required to support a Fortran-90 function call.>
 *
 * Always devote a separate directory to a yorick compiled package.
 * it should contain all the relevant source code (compiled and
 * interpreted), plus any test data and documentation.  Once the
 * package is built, you can install it in a permanent location
 * alongside other packages.
 *
 * The first job is to design the interpreted interface for your
 * compiled package.  The name of the package will be "cerfc", and
 * your package include file will be this file, cerfc.i.  The first
 * executable line of a package that loads a package include file is
 *
 *   plug_in, "pkgname";
 *
 * If you built your package as a plugin (dynamically loaded at runtime),
 * this command opens the pkgname.so dynamic library containing your
 * compiled code.  If you statically load your package into a custom
 * version of yorick, the plug_in command is a no-op, but it is necessary
 * to mark this file as a yorick interface to a compiled package called
 * "pkgname".
 *
 * The natural form of a compiled version of erfc would be
 *   double cerfc(double input)
 * but this would be worthless for yorick, because it operates only on
 * a single number, returning a single result at a time.  This will
 * cripple the performance.  The minimum useful compiled function is
 *   void cerfc(double *input, long n, double *output)
 * which computes n results "at once" from the point of view of its
 * caller.  You will also need an interpreted "wrapper" to hide this
 * non-yorick-look-and-feel interface from the users, so they sees
 *   output = cerfc(input)
 * where input can be an arbitrary numeric array.  The required wrapper
 * and declaration of the underlying compiled routine are given below.
 *
 * After writing cerfc.i, it remains only to write cerfc.c, the actual
 * implementation of the function whose C prototype is given above.
 * When that is done, you need only run existing programs in order to
 * build your compiled package.
 *
 * The first job is to create a Makefile, which you do by running yorick
 * like this:
 *   yorick -batch make.i
 * For this simple (but not necessarily atypical) example, the resulting
 * Makefile is complete.  If your package depended on some third party
 * library, you would need to edit the resulting Makefile to add, for
 * example, whatever special flags (-L and -l) are required to link your
 * code to the PKG_DEPLIBS line of the Makefile.
 *
 * Note that you would never distribute a package without the Makefile.
 * Although make.i creates Makefile automatically, you are only supposed
 * to do that once!  After the Makefile is created, it becomes as much
 * a part of your source code as cerfc.i or cerfc.c -- remember that you
 * may have had to edit it by hand!
 *
 * To debug your package, type:
 *   make TGT=exe COPT=-g
 * This creates a custom yorick executable in this directory (called
 * yorick), which you can use to debug the code in cerfc.c.  Of course,
 * I haven't left any bugs for you to find, so you can imagine that
 * debugging has already been done for the cerfc package and move on.
 *
 * To build the final version of your compiled package, type:
 *   make
 * This will usually create cerfc.so (or cerfc.dll on cygwin).  If
 * you now start yorick in this directory and include cerfc.i, you
 * will have interpreted access to your compiled cerfc function.
 * You can compare its performance to the interpreted erfc, to see
 * whether the exercise has been worthwhile.
 *
 * If you are on a platform which does not allow plugins, or where yorick
 * simply does not know how to do dynamic loading, then the "make" command
 * will build a custom yorick executable with the compiled cerfc built in
 * (statically loaded).  In this case, you need not include cerfc.i
 * explicitly, you can just run cerfc.
 */

plug_in, "cerfc";

func cerfc(x)
/* DOCUMENT cerfc(x)
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
  raw_cerfc, y, x, numberof(x);
  return y;
}

extern raw_cerfc;
/* PROTOTYPE
   void cerfc(double array y, double array x, long n)
 */

/* The PROTOTYPE comment:
   (1) attaches the compiled function cerfc to the interpreted function
       raw_cerfc
   (2) generates wrapper code for cerfc that converts the data types
       to those shown in the comment -- for output variables such as
       y in this case, it is the responsibility of the interpreted caller
       (func cerf above) to ensure that no conversion is necessary
       (otherwise the result will go to a temporary array and be discarded)
   (3) note that the word "array" replaces the symbol "*" in the
       corresponding ANSI C prototype
       complicated data types should use the interpreted data type
       "pointer" and pass their arguments as "&arg".
 */
