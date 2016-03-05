/*
 * $Id: gcd.i,v 1.1 2005-09-18 22:06:00 dhmunro Exp $
 * GCD, LCM, and prime factorization routines.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func gcd(a, b)
/* DOCUMENT gcd(a,b)
     returns the GCD (greatest common divisor) of A and B, which must
     be one of the integer data types.  A and B may be conformable
     arrays; the semantics of the gcd call are the same as any other
     binary operation.  Uses Euclid's celebrated algorithm.
     The absolute values of A and B are taken before the operation
     commences; if either A or B is 0, the return value will be 0.
   SEE ALSO: lcm, is_prime, factorize
 */
{
  a= abs(a);
  b= abs(b);
  c= min(a, b);
  a= max(a, b);
  b= c;          /* simplifies c=0 case */

  if (dimsof(a)(1)) {
    /* array case */
    for (list=where(c) ; numberof(list) ; list=where(c)) {
      b(list)= bl= c(list);
      c(list)= a(list) % bl;
      a(list)= bl;
    }

  } else {
    /* scalar case can be less baroque */
    while (c) {
      b= c;
      c= a % b;
      a= b;
    }
  }

  return b;
}

func lcm(a, b)
/* DOCUMENT lcm(a,b)
     returns the LCM (least common multiple) of A and B, which must
     be one of the integer data types.  A and B may be conformable
     arrays; the semantics of the lcm call are the same as any other
     binary operation.
     The absolute values of A and B are taken before the operation
     commences; if either A or B is 0, the return value will be 0.
   SEE ALSO: gcd, is_prime, factorize
 */
{
  d= gcd(a, b);
  /* two potential problems: zero divide and overflow - handle the
     first but not the second */
  return abs(a*b)/(d+!d);
}

func is_prime(x)
/* DOCUMENT is_prime(x)
     return non-zero if and only if X (which must be a scalar integer)
     is prime.  May return a false positive if X is greater than about
     3e9, since at most 20000 candidate factors are checked.
     The absolute value of X is taken first; zero is not prime, but 1 is.
   SEE ALSO: gcd, lcm, factorize
 */
{
  x= long(abs(x));
  if (x<4) return x>0;
  /* make a list of factors which includes 2, 3, and all larger
     odd numbers not divisible by 3 less or equal to sqrt(x) */
  top= min(long((sqrt(x)+8.)/3.+0.5), 20000);
  factors= ((3*indgen(2:top))/2)*2 - 7;
  factors(1:2)= [2,3];
  return allof(x%factors);
}

func factorize(x)
/* DOCUMENT factorize(x)
     return list of prime factors of X and their powers as an n-by-2
     array.  May include a large non-prime factor if X exceeds 3e9.
     In any event, product(result(,1)^result(,2)) will equal abs(X).
     X must be a scalar integer type.
   SEE ALSO: gcd, lcm, is_prime
 */
{
  x= long(abs(x));
  if (x<2) return [[x],[1]];

  /* first get rid of any prime factors less than 102 */
  primes= [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,
           71,73,79,83,89,97,101];
  primes= primes(where(!(x%primes)));
  powers= _fact_extract(primes, x);   /* returns "deflated" x */

  if (x>1) {
    /* large prime factors require a less direct approach */
    top= min(long((sqrt(x)+2.)/3.+0.5), 20000);
    if (top>=35) {
      /* trial divisors are all odd numbers 103 or greater which are
         not divisible by three and do not exceed sqrt(x) */
      trial= ((3*indgen(35:top))/2)*2 - 1;
      /* discard all trial divisors which do not divide x */
      trial= trial(where(!(x%trial)));
      /* the smallest remaining divisor must be prime - remove it and
         all its subsequent multiples to find the next prime divisor
         and so on until the list contains only primes */
      list= trial;
      for (n=0 ; numberof(trial) ; ++n) {
        list(n+1)= trial(1);
        trial= trial(where(trial%trial(1)));
      }
      if (n) {
        trial= list(1:n);
        grow, primes, trial;
        grow, powers, _fact_extract(trial,x);
      }
    }
    if (x>1) {
      grow, primes, [x];
      grow, powers, [1];
    }
  }

  return [primes, powers];
}

func _fact_extract(primes, &x)
{
  if (is_void(primes)) return [];
  /* first get largest power of each prime less than or equal x */
  powers= long(log(x)/log(primes)+1.e-6);
  factors= gcd(primes^powers, x);
  x/= long(exp(sum(log(factors)))+0.5);
  return long(log(factors)/log(primes)+0.5);
}
