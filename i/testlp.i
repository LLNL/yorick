/*
 * $Id: testlp.i,v 1.1 2005-09-18 22:06:12 dhmunro Exp $
 * Yorick LAPACK benchmark very similar to the Linpack benchmark.
 * Contributed by Steve Langer.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func testlp(n)
/* DOCUMENT testlp
     Run a benchmark of Yorick's LUsolve routine similar to the
     Linpack benchmark.
 */
{
  elapsed= array(0.0, 3);
  old= second(0.0);

  /* set the size of the matrix */
  if (is_void(n)) n= 100;
  n= min(500, max(20, n));
  write," using size ",n;
  cray= .056;
  ops= (2.0*n^3)/3.0 + 2.0*n^2;
  /* WARNING WARNING!!! will not allocate extra storage,
     AND will not test case where there is extra storage in matrix a */
  msiz= n;

  /* allocate arrays */
  a= array(0.0, msiz, msiz);
  aa= array(0.0, msiz, msiz);
  b= array(0.0, msiz);
  bb= array(0.0, msiz);
  x= array(0.0, msiz);
  time= array(0.0, 8, 6);
  lda= msiz;
  norma= 0.0;

  /* set the number of passes to make through the solver loop
     so that time can be measured accurately
     */
  ntimes= 50;

  /* generate matrix a and rhs b once and for all */
  matgen,aa,lda,n,bb,norma;

  /* copy matrix into place and run first test */
  b= bb;
  a= aa;
  t1= second(old);
  b= LUsolve(a,b);
  total= second(old) - t1;

  /* compute a residual to verify results. */

  x= b;
  a= aa;
  b= -bb;
  dmxpy,n,b,n,lda,x,a;
  resid= max(abs(b));
  normx= max(abs(x));
  eps= epslon(1.0);
  residn= resid/( n*norma*normx*eps );
  write,"  norm. resid      resid           machep",
  "        x(1)          x(n)";
  write,format="%15.8e%15.8e%15.8e%15.8e%15.8e\n", residn,resid,eps,x(1),x(n);
  if (resid>1.e-10) write,"****WARNING**** resid too large";
  write,format="\n    times are reported for matrices of order %6d\n",n;

  time(1,3)= total;
  time(1,4)= ops/(1.0e6*total + !total);
  time(1,5)= 2.0/time(1,4);
  time(1,6)= total/cray;
  write,format=" times for array with leading dimension of%6d\n", lda;
  write,"   dgesv     mflops       unit      ratio";
  write,format="%11.3e%11.3e%11.3e%11.3e\n", time(1,3),time(1,4),
  time(1,5),time(1,6);

  b= bb;
  a= aa;
  t1= second(old);
  b= LUsolve(a,b);
  total= second(old) - t1;
  time(2,3)= total;
  time(2,4)= ops/(1.0e6*total + !total);
  time(2,5)= 2.0/time(2,4);
  time(2,6)= total/cray;

  b= bb;
  a= aa;
  t1= second(old);
  b= LUsolve(a,b);
  total= second(old) - t1;
  time(3,3)= total;
  time(3,4)= ops/(1.0e6*total + !total);
  time(3,5)= 2.0/time(3,4);
  time(3,6)= total/cray;

  tm2= 0;
  t1= second(old);
  for(i= 1; i <= ntimes; i++) {
    tm= second(old);
    b= bb;
    a= aa;
    tm2 += second(old) - tm;
    b= LUsolve(a,b);
  }
  total= (second(old) - t1 - tm2)/ntimes;
  time(4,3)= total;
  time(4,4)= ops/(1.0e6*total + !total);
  time(4,5)= 2.0/time(4,4);
  time(4,6)= total/cray;

  write,format="%11.3e%11.3e%11.3e%11.3e\n", time(2,3),time(2,4),
        time(2,5),time(2,6);
  write,format="%11.3e%11.3e%11.3e%11.3e\n", time(3,3),time(3,4),
        time(3,5),time(3,6);
  write,format=" result of %4d passes through solver\n", ntimes;
  write,"   dgesv     mflops       unit      ratio";
  write,format="%11.3e%11.3e%11.3e%11.3e\n", time(4,3),time(4,4),
        time(4,5),time(4,6);
}

func second(old)
{
  /* return the total CPU plus system time for this job,
     less the input argument. */
  timer, elapsed;
  return elapsed(1)+elapsed(2)-old;
}

func dmxpy (n1, y, n2, ldm, x, m)
{
/*   purpose:
     multiply matrix m times vector x and add the result to vector y.

   parameters:

     n1 integer, number of elements in vector y, and number of rows in
         matrix m

     y double precision(n1), vector of length n1 to which is added
         the product m*x

     n2 integer, number of elements in vector x, and number of columns
         in matrix m

     ldm integer, leading dimension of array m

     x double precision(n2), vector of length n2

     m double precision(ldm,n2), matrix of n1 rows and n2 columns

 ----------------------------------------------------------------------*/
  /*   cleanup odd vector */
  j= n2%2;
  if (j >= 1) {
    y(1:n1)+= x(j)*m(1:n1,j);
  }
  /*   cleanup odd group of two vectors */
  j= n2%4;
  if (j >= 2) {
    y(1:n1)+= x(j-1)*m(1:n1,j-1) + x(j)*m(1:n1,j);
  }
  /*   cleanup odd group of four vectors */
  j= n2%8;
  if (j >= 4) {
    y(1:n1)+= x(j-3)*m(1:n1,j-3) + x(j-2)*m(1:n1,j-2) +
              x(j-1)*m(1:n1,j-1) + x(j)*m(1:n1,j);
  }
  /*   cleanup odd group of eight vectors */
  j= n2%16;
  if (j >= 8) {
    y(1:n1)+= x(j-7)*m(1:n1,j-7) + x(j-6)*m(1:n1,j-6) +
              x(j-5)*m(1:n1,j-5) + x(j-4)*m(1:n1,j-4) +
              x(j-3)*m(1:n1,j-3) + x(j-2)*m(1:n1,j-2) +
              x(j-1)*m(1:n1,j-1) + x(j)  *m(1:n1,j);
  }
  /*   main loop - groups of sixteen vectors */
  jmin= j+16;
  for(j=jmin; j<=n2; j+=16) {
    y(1:n1)+= x(j-15)*m(i,j-15) + x(j-14)*m(1:n1,j-14) +
              x(j-13)*m(1:n1,j-13) + x(j-12)*m(1:n1,j-12) +
              x(j-11)*m(1:n1,j-11) + x(j-10)*m(1:n1,j-10) +
              x(j- 9)*m(1:n1,j- 9) + x(j- 8)*m(1:n1,j- 8) +
              x(j- 7)*m(1:n1,j- 7) + x(j- 6)*m(1:n1,j- 6) +
              x(j- 5)*m(1:n1,j- 5) + x(j- 4)*m(1:n1,j- 4) +
              x(j- 3)*m(1:n1,j- 3) + x(j- 2)*m(1:n1,j- 2) +
              x(j- 1)*m(1:n1,j- 1) + x(j)   *m(1:n1,j);
  }
}

func matgen(a,lda,n,b,&norma)
{
  init= 1325;
  for(j= 1; j <= n; j++) {
    for(i= 1; i <= n; i++) {
      init= 3125*init % 65536;
      a(i,j)= (init - 32768.0)/16384.0;
    }
  }
  norma= max(a(1:n,1:n));
  b(1:n)= a(1:n,sum:1:n);
}

func epslon (x)
{
/*
     estimate unit roundoff in quantities of size x.

      double precision a,b,c,eps

     this program should function properly on all systems
     satisfying the following two assumptions,
        1.  the base used in representing dfloating point
            numbers is not a power of three.
        2.  the quantity  a  in statement 10 is represented to
            the accuracy used in dfloating point variables
            that are stored in memory.
     the statement number 10 and the go to 10 are intended to
     force optimizing compilers to generate code satisfying
     assumption 2.
     under these assumptions, it should be true that,
            a  is not exactly equal to four-thirds,
            b  has a zero for its last bit or digit,
            c  is not exactly equal to one,
            eps  measures the separation of 1.0 from
                 the next larger dfloating point number.
     the developers of eispack would appreciate being informed
     about any systems where these assumptions do not hold.

     *****************************************************************
     this routine is one of the auxiliary routines used by eispack iii
     to avoid machine dependencies.
     *****************************************************************

     this version dated 4/6/83.
*/
  a= 4.0/3.0;
  do {
    b= a - 1.0;
    c= b + b + b;
    eps= abs(c-1.0);
  } while(eps== 0.0) ;
  return eps*abs(x);
}
