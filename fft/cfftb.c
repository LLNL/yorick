/*
 * $Id: cfftb.c,v 1.1 2005-09-18 22:04:52 dhmunro Exp $
 * Swarztrauber complex FFT routines (backward direction).
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "cfft.h"

static void passb (long *nac,long ido,long ip,long l1,long idl1,
                   double cc[],double c1[],double c2[],double ch[],
                   double ch2[],double wa[]);
static void passb5 (long ido,long l1,double cc[],double ch[],
                    double wa1[],double wa2[],double wa3[],double wa4[]);
static void passb3 (long ido,long l1,double cc[],double ch[],
                    double wa1[],double wa2[]);
static void passb2 (long ido,long l1,double cc[],double ch[],double wa1[]);
static void passb4 (long ido,long l1,double cc[],double ch[],
                    double wa1[],double wa2[],double wa3[]);



void cfftb (long n,double c[],double wsave[])
{
  /*      implicit double  (a-h,o-z);*/
#undef wsave_1
#define wsave_1(a1) wsave[a1-1]
#undef c_1
#define c_1(a1) c[a1-1]
  /*-----implicit-declarations-----*/
  long iw2;
  long iw1;
  /*-----end-of-declarations-----*/
  if (n == 1) return;
  iw1 = n+n+1;
  iw2 = iw1+n+n;
  cfftb1 (n,c,wsave,&wsave_1(iw1),(long *)&wsave_1(iw2));
  return;
}



void cfftb1 (long n,double c[],double ch[],double wa[],long ifac[])
{
  /*      implicit double  (a-h,o-z);*/
#undef wa_1
#define wa_1(a1) wa[a1-1]
#undef c_1
#define c_1(a1) c[a1-1]
#undef ch_1
#define ch_1(a1) ch[a1-1]
#undef ifac_1
#define ifac_1(a1) ifac[a1-1]
  /*-----implicit-declarations-----*/
  long i;
  long n2;
  long ix4;
  long ix3;
  long ix2;
  long idl1;
  long idot;
  long ido;
  long l2;
  long ip;
  long k1;
  long iw;
  long l1;
  long na;
  long nf;
  long nac;
  /*-----end-of-declarations-----*/
  nf = ifac_1(2);
  na = 0;
  l1 = 1;
  iw = 1;
  for (k1=1 ; k1<=nf ; k1+=1) {
    ip = ifac_1(k1+2);
    l2 = ip*l1;
    ido = n/l2;
    idot = ido+ido;
    idl1 = idot*l1;
    if (ip != 4) goto L_103;
    ix2 = iw+idot;
    ix3 = ix2+idot;
    if (na != 0) goto L_101;
    passb4 (idot,l1,c,ch,&wa_1(iw),&wa_1(ix2),&wa_1(ix3));
    goto L_102;
  L_101:     passb4 (idot,l1,ch,c,&wa_1(iw),&wa_1(ix2),&wa_1(ix3));
  L_102:    na = 1-na;
    goto L_115;
  L_103:    if (ip != 2) goto L_106;
    if (na != 0) goto L_104;
    passb2 (idot,l1,c,ch,&wa_1(iw));
    goto L_105;
  L_104:     passb2 (idot,l1,ch,c,&wa_1(iw));
  L_105:    na = 1-na;
    goto L_115;
  L_106:    if (ip != 3) goto L_109;
    ix2 = iw+idot;
    if (na != 0) goto L_107;
    passb3 (idot,l1,c,ch,&wa_1(iw),&wa_1(ix2));
    goto L_108;
  L_107:     passb3 (idot,l1,ch,c,&wa_1(iw),&wa_1(ix2));
  L_108:    na = 1-na;
    goto L_115;
  L_109:    if (ip != 5) goto L_112;
    ix2 = iw+idot;
    ix3 = ix2+idot;
    ix4 = ix3+idot;
    if (na != 0) goto L_110;
    passb5 (idot,l1,c,ch,&wa_1(iw),&wa_1(ix2),&wa_1(ix3),&wa_1(ix4));
    goto L_111;
  L_110:     passb5 (idot,l1,ch,c,&wa_1(iw),&wa_1(ix2),&wa_1(ix3),&wa_1(ix4));
  L_111:    na = 1-na;
    goto L_115;
  L_112:    if (na != 0) goto L_113;
    passb (&nac,idot,ip,l1,idl1,c,c,c,ch,ch,&wa_1(iw));
    goto L_114;
  L_113:     passb (&nac,idot,ip,l1,idl1,ch,ch,ch,c,c,&wa_1(iw));
  L_114:    if (nac != 0) na = 1-na;
  L_115:    l1 = l2;
    iw = iw+(ip-1)*idot;
  }
  if (na == 0) return;
  n2 = n+n;
  for (i=1 ; i<=n2 ; i+=1) {
    c_1(i) = ch_1(i);
  }
  return;
}



static void passb (long *nac,long ido,long ip,long l1,long idl1,
                   double cc[],double c1[],double c2[],double ch[],
                   double ch2[],double wa[])
{
  /*      implicit double  (a-h,o-z);*/
#undef ch2_2
#define ch2_2(a1,a2) ch2[a1-1+idl1*(a2-1)]
#undef c2_2
#define c2_2(a1,a2) c2[a1-1+idl1*(a2-1)]
#undef wa_1
#define wa_1(a1) wa[a1-1]
#undef c1_3
#define c1_3(a1,a2,a3) c1[a1-1+ido*(a2-1+l1*(a3-1))]
#undef cc_3
#define cc_3(a1,a2,a3) cc[a1-1+ido*(a2-1+ip*(a3-1))]
#undef ch_3
#define ch_3(a1,a2,a3) ch[a1-1+ido*(a2-1+l1*(a3-1))]
  /*-----implicit-declarations-----*/
  long idj;
  long idij;
  double wai;
  double war;
  long idlj;
  long ik;
  long lc;
  long l;
  long inc;
  long idl;
  long i;
  long k;
  long jc;
  long j;
  long idp;
  long ipph;
  long ipp2;
  long idot;
  /*-----end-of-declarations-----*/
  idot = ido/2;
  ipp2 = ip+2;
  ipph = (ip+1)/2;
  idp = ip*ido;

  if (ido >= l1) {
    for (j=2 ; j<=ipph ; j+=1) {
      jc = ipp2-j;
      for (k=1 ; k<=l1 ; k+=1) {
        for (i=1 ; i<=ido ; i+=1) {
          ch_3(i,k,j) = cc_3(i,j,k)+cc_3(i,jc,k);
          ch_3(i,k,jc) = cc_3(i,j,k)-cc_3(i,jc,k);
        }
      }
    }
    for (k=1 ; k<=l1 ; k+=1) {
      for (i=1 ; i<=ido ; i+=1) {
        ch_3(i,k,1) = cc_3(i,1,k);
      }
    }
  } else {
    for (j=2 ; j<=ipph ; j+=1) {
      jc = ipp2-j;
      for (i=1 ; i<=ido ; i+=1) {
        for (k=1 ; k<=l1 ; k+=1) {
          ch_3(i,k,j) = cc_3(i,j,k)+cc_3(i,jc,k);
          ch_3(i,k,jc) = cc_3(i,j,k)-cc_3(i,jc,k);
        }
      }
    }
    for (i=1 ; i<=ido ; i+=1) {
      for (k=1 ; k<=l1 ; k+=1) {
        ch_3(i,k,1) = cc_3(i,1,k);
      }
    }
  }
  idl = 2-ido;
  inc = 0;
  for (l=2 ; l<=ipph ; l+=1) {
    lc = ipp2-l;
    idl = idl+ido;
    for (ik=1 ; ik<=idl1 ; ik+=1) {
      c2_2(ik,l) = ch2_2(ik,1)+wa_1(idl-1)*ch2_2(ik,2);
      c2_2(ik,lc) = wa_1(idl)*ch2_2(ik,ip);
    }
    idlj = idl;
    inc = inc+ido;
    for (j=3 ; j<=ipph ; j+=1) {
      jc = ipp2-j;
      idlj = idlj+inc;
      if (idlj > idp) idlj = idlj-idp;
      war = wa_1(idlj-1);
      wai = wa_1(idlj);
      for (ik=1 ; ik<=idl1 ; ik+=1) {
        c2_2(ik,l) = c2_2(ik,l)+war*ch2_2(ik,j);
        c2_2(ik,lc) = c2_2(ik,lc)+wai*ch2_2(ik,jc);
      }
    }
  }
  for (j=2 ; j<=ipph ; j+=1) {
    for (ik=1 ; ik<=idl1 ; ik+=1) {
      ch2_2(ik,1) = ch2_2(ik,1)+ch2_2(ik,j);
    }
  }
  for (j=2 ; j<=ipph ; j+=1) {
    jc = ipp2-j;
    for (ik=2 ; ik<=idl1 ; ik+=2) {
      ch2_2(ik-1,j) = c2_2(ik-1,j)-c2_2(ik,jc);
      ch2_2(ik-1,jc) = c2_2(ik-1,j)+c2_2(ik,jc);
      ch2_2(ik,j) = c2_2(ik,j)+c2_2(ik-1,jc);
      ch2_2(ik,jc) = c2_2(ik,j)-c2_2(ik-1,jc);
    }
  }
  *nac = 1;
  if (ido == 2) return;
  *nac = 0;
  for (ik=1 ; ik<=idl1 ; ik+=1) {
    c2_2(ik,1) = ch2_2(ik,1);
  }
  for (j=2 ; j<=ip ; j+=1) {
    for (k=1 ; k<=l1 ; k+=1) {
      c1_3(1,k,j) = ch_3(1,k,j);
      c1_3(2,k,j) = ch_3(2,k,j);
    }
  }
  if (idot <= l1) {
    idij = 0;
    for (j=2 ; j<=ip ; j+=1) {
      idij = idij+2;
      for (i=4 ; i<=ido ; i+=2) {
        idij = idij+2;
        for (k=1 ; k<=l1 ; k+=1) {
          c1_3(i-1,k,j) = wa_1(idij-1)*ch_3(i-1,k,j)-wa_1(idij)*ch_3(i,k,j);
          c1_3(i,k,j) = wa_1(idij-1)*ch_3(i,k,j)+wa_1(idij)*ch_3(i-1,k,j);
        }
      }
    }
  } else {
    idj = 2-ido;
    for (j=2 ; j<=ip ; j+=1) {
      idj = idj+ido;
      for (k=1 ; k<=l1 ; k+=1) {
        idij = idj;
        for (i=4 ; i<=ido ; i+=2) {
          idij = idij+2;
          c1_3(i-1,k,j) = wa_1(idij-1)*ch_3(i-1,k,j)-wa_1(idij)*ch_3(i,k,j);
          c1_3(i,k,j) = wa_1(idij-1)*ch_3(i,k,j)+wa_1(idij)*ch_3(i-1,k,j);
        }
      }
    }
  }
}



static void passb5 (long ido,long l1,double cc[],double ch[],
                    double wa1[],double wa2[],double wa3[],double wa4[])
{
  /*      implicit double  (a-h,o-z);*/
#undef wa4_1
#define wa4_1(a1) wa4[a1-1]
#undef wa3_1
#define wa3_1(a1) wa3[a1-1]
#undef wa2_1
#define wa2_1(a1) wa2[a1-1]
#undef wa1_1
#define wa1_1(a1) wa1[a1-1]
#undef ch_3
#define ch_3(a1,a2,a3) ch[a1-1+ido*(a2-1+l1*(a3-1))]
#undef cc_3
#define cc_3(a1,a2,a3) cc[a1-1+ido*(a2-1+5*(a3-1))]
  /* Original Swarztrauber constants rather imprecise
     -- thanks to Eric Thiebaut for better values
  static double tr11= .309016994374947;
  static double ti11= .951056516295154;
  static double tr12= -.809016994374947;
  static double ti12= .587785252292473;
  */
  static double tr11= 0.309016994374947424102293417182819059; /*cos(+2*pi/5)*/
  static double ti11= 0.951056516295153572116439333379382143; /*sin(+2*pi/5)*/
  static double tr12=-0.809016994374947424102293417182819059; /*cos(+4*pi/5)*/
  static double ti12= 0.587785252292473129168705954639072769; /*sin(+4*pi/5)*/
  /*-----implicit-declarations-----*/
  double di2;
  double di5;
  double dr2;
  double dr5;
  double di4;
  double di3;
  double dr4;
  double dr3;
  long i;
  double ci4;
  double cr4;
  double ci5;
  double cr5;
  double ci3;
  double cr3;
  double ci2;
  double cr2;
  double tr3;
  double tr4;
  double tr2;
  double tr5;
  double ti3;
  double ti4;
  double ti2;
  double ti5;
  long k;
  /*-----end-of-declarations-----*/
  if (ido != 2) goto L_102;
  for (k=1 ; k<=l1 ; k+=1) {
    ti5 = cc_3(2,2,k)-cc_3(2,5,k);
    ti2 = cc_3(2,2,k)+cc_3(2,5,k);
    ti4 = cc_3(2,3,k)-cc_3(2,4,k);
    ti3 = cc_3(2,3,k)+cc_3(2,4,k);
    tr5 = cc_3(1,2,k)-cc_3(1,5,k);
    tr2 = cc_3(1,2,k)+cc_3(1,5,k);
    tr4 = cc_3(1,3,k)-cc_3(1,4,k);
    tr3 = cc_3(1,3,k)+cc_3(1,4,k);
    ch_3(1,k,1) = cc_3(1,1,k)+tr2+tr3;
    ch_3(2,k,1) = cc_3(2,1,k)+ti2+ti3;
    cr2 = cc_3(1,1,k)+tr11*tr2+tr12*tr3;
    ci2 = cc_3(2,1,k)+tr11*ti2+tr12*ti3;
    cr3 = cc_3(1,1,k)+tr12*tr2+tr11*tr3;
    ci3 = cc_3(2,1,k)+tr12*ti2+tr11*ti3;
    cr5 = ti11*tr5+ti12*tr4;
    ci5 = ti11*ti5+ti12*ti4;
    cr4 = ti12*tr5-ti11*tr4;
    ci4 = ti12*ti5-ti11*ti4;
    ch_3(1,k,2) = cr2-ci5;
    ch_3(1,k,5) = cr2+ci5;
    ch_3(2,k,2) = ci2+cr5;
    ch_3(2,k,3) = ci3+cr4;
    ch_3(1,k,3) = cr3-ci4;
    ch_3(1,k,4) = cr3+ci4;
    ch_3(2,k,4) = ci3-cr4;
    ch_3(2,k,5) = ci2-cr5;
  }
  return;
 L_102: for (k=1 ; k<=l1 ; k+=1) {
   for (i=2 ; i<=ido ; i+=2) {
     ti5 = cc_3(i,2,k)-cc_3(i,5,k);
     ti2 = cc_3(i,2,k)+cc_3(i,5,k);
     ti4 = cc_3(i,3,k)-cc_3(i,4,k);
     ti3 = cc_3(i,3,k)+cc_3(i,4,k);
     tr5 = cc_3(i-1,2,k)-cc_3(i-1,5,k);
     tr2 = cc_3(i-1,2,k)+cc_3(i-1,5,k);
     tr4 = cc_3(i-1,3,k)-cc_3(i-1,4,k);
     tr3 = cc_3(i-1,3,k)+cc_3(i-1,4,k);
     ch_3(i-1,k,1) = cc_3(i-1,1,k)+tr2+tr3;
     ch_3(i,k,1) = cc_3(i,1,k)+ti2+ti3;
     cr2 = cc_3(i-1,1,k)+tr11*tr2+tr12*tr3;
     ci2 = cc_3(i,1,k)+tr11*ti2+tr12*ti3;
     cr3 = cc_3(i-1,1,k)+tr12*tr2+tr11*tr3;
     ci3 = cc_3(i,1,k)+tr12*ti2+tr11*ti3;
     cr5 = ti11*tr5+ti12*tr4;
     ci5 = ti11*ti5+ti12*ti4;
     cr4 = ti12*tr5-ti11*tr4;
     ci4 = ti12*ti5-ti11*ti4;
     dr3 = cr3-ci4;
     dr4 = cr3+ci4;
     di3 = ci3+cr4;
     di4 = ci3-cr4;
     dr5 = cr2+ci5;
     dr2 = cr2-ci5;
     di5 = ci2-cr5;
     di2 = ci2+cr5;
     ch_3(i-1,k,2) = wa1_1(i-1)*dr2-wa1_1(i)*di2;
     ch_3(i,k,2) = wa1_1(i-1)*di2+wa1_1(i)*dr2;
     ch_3(i-1,k,3) = wa2_1(i-1)*dr3-wa2_1(i)*di3;
     ch_3(i,k,3) = wa2_1(i-1)*di3+wa2_1(i)*dr3;
     ch_3(i-1,k,4) = wa3_1(i-1)*dr4-wa3_1(i)*di4;
     ch_3(i,k,4) = wa3_1(i-1)*di4+wa3_1(i)*dr4;
     ch_3(i-1,k,5) = wa4_1(i-1)*dr5-wa4_1(i)*di5;
     ch_3(i,k,5) = wa4_1(i-1)*di5+wa4_1(i)*dr5;
   }
 }
  return;
}



static void passb3 (long ido,long l1,double cc[],double ch[],
                    double wa1[],double wa2[])
{
  /*      implicit double  (a-h,o-z);*/
#undef wa2_1
#define wa2_1(a1) wa2[a1-1]
#undef wa1_1
#define wa1_1(a1) wa1[a1-1]
#undef ch_3
#define ch_3(a1,a2,a3) ch[a1-1+ido*(a2-1+l1*(a3-1))]
#undef cc_3
#define cc_3(a1,a2,a3) cc[a1-1+ido*(a2-1+3*(a3-1))]
  /* Original Swarztrauber constants rather imprecise
     -- thanks to Eric Thiebaut for better values
  static double taur= -.5;
  static double taui= .866025403784439;
  */
  static double taur=-0.500000000000000000000000000000000000; /*cos(+2*pi/3)*/
  static double taui= 0.866025403784438646763723170752936183; /*sin(+2*pi/3)*/
  /*-----implicit-declarations-----*/
  double di3;
  double di2;
  double dr3;
  double dr2;
  long i;
  double ci3;
  double cr3;
  double ci2;
  double ti2;
  double cr2;
  double tr2;
  long k;
  /*-----end-of-declarations-----*/
  if (ido != 2) goto L_102;
  for (k=1 ; k<=l1 ; k+=1) {
    tr2 = cc_3(1,2,k)+cc_3(1,3,k);
    cr2 = cc_3(1,1,k)+taur*tr2;
    ch_3(1,k,1) = cc_3(1,1,k)+tr2;
    ti2 = cc_3(2,2,k)+cc_3(2,3,k);
    ci2 = cc_3(2,1,k)+taur*ti2;
    ch_3(2,k,1) = cc_3(2,1,k)+ti2;
    cr3 = taui*(cc_3(1,2,k)-cc_3(1,3,k));
    ci3 = taui*(cc_3(2,2,k)-cc_3(2,3,k));
    ch_3(1,k,2) = cr2-ci3;
    ch_3(1,k,3) = cr2+ci3;
    ch_3(2,k,2) = ci2+cr3;
    ch_3(2,k,3) = ci2-cr3;
  }
  return;
 L_102: for (k=1 ; k<=l1 ; k+=1) {
   for (i=2 ; i<=ido ; i+=2) {
     tr2 = cc_3(i-1,2,k)+cc_3(i-1,3,k);
     cr2 = cc_3(i-1,1,k)+taur*tr2;
     ch_3(i-1,k,1) = cc_3(i-1,1,k)+tr2;
     ti2 = cc_3(i,2,k)+cc_3(i,3,k);
     ci2 = cc_3(i,1,k)+taur*ti2;
     ch_3(i,k,1) = cc_3(i,1,k)+ti2;
     cr3 = taui*(cc_3(i-1,2,k)-cc_3(i-1,3,k));
     ci3 = taui*(cc_3(i,2,k)-cc_3(i,3,k));
     dr2 = cr2-ci3;
     dr3 = cr2+ci3;
     di2 = ci2+cr3;
     di3 = ci2-cr3;
     ch_3(i,k,2) = wa1_1(i-1)*di2+wa1_1(i)*dr2;
     ch_3(i-1,k,2) = wa1_1(i-1)*dr2-wa1_1(i)*di2;
     ch_3(i,k,3) = wa2_1(i-1)*di3+wa2_1(i)*dr3;
     ch_3(i-1,k,3) = wa2_1(i-1)*dr3-wa2_1(i)*di3;
   }
 }
  return;
}



static void passb2 (long ido,long l1,double cc[],double ch[],double wa1[])
{
  /*      implicit double  (a-h,o-z);*/
#undef wa1_1
#define wa1_1(a1) wa1[a1-1]
#undef ch_3
#define ch_3(a1,a2,a3) ch[a1-1+ido*(a2-1+l1*(a3-1))]
#undef cc_3
#define cc_3(a1,a2,a3) cc[a1-1+ido*(a2-1+2*(a3-1))]
  /*-----implicit-declarations-----*/
  double ti2;
  double tr2;
  long i;
  long k;
  /*-----end-of-declarations-----*/
  if (ido > 2) goto L_102;
  for (k=1 ; k<=l1 ; k+=1) {
    ch_3(1,k,1) = cc_3(1,1,k)+cc_3(1,2,k);
    ch_3(1,k,2) = cc_3(1,1,k)-cc_3(1,2,k);
    ch_3(2,k,1) = cc_3(2,1,k)+cc_3(2,2,k);
    ch_3(2,k,2) = cc_3(2,1,k)-cc_3(2,2,k);
  }
  return;
 L_102: for (k=1 ; k<=l1 ; k+=1) {
   for (i=2 ; i<=ido ; i+=2) {
     ch_3(i-1,k,1) = cc_3(i-1,1,k)+cc_3(i-1,2,k);
     tr2 = cc_3(i-1,1,k)-cc_3(i-1,2,k);
     ch_3(i,k,1) = cc_3(i,1,k)+cc_3(i,2,k);
     ti2 = cc_3(i,1,k)-cc_3(i,2,k);
     ch_3(i,k,2) = wa1_1(i-1)*ti2+wa1_1(i)*tr2;
     ch_3(i-1,k,2) = wa1_1(i-1)*tr2-wa1_1(i)*ti2;
   }
 }
  return;
}



static void passb4 (long ido,long l1,double cc[],double ch[],
                    double wa1[],double wa2[],double wa3[])
{
  /*      implicit double  (a-h,o-z);*/
#undef wa3_1
#define wa3_1(a1) wa3[a1-1]
#undef wa2_1
#define wa2_1(a1) wa2[a1-1]
#undef wa1_1
#define wa1_1(a1) wa1[a1-1]
#undef ch_3
#define ch_3(a1,a2,a3) ch[a1-1+ido*(a2-1+l1*(a3-1))]
#undef cc_3
#define cc_3(a1,a2,a3) cc[a1-1+ido*(a2-1+4*(a3-1))]
  /*-----implicit-declarations-----*/
  double ci4;
  double ci2;
  double cr4;
  double cr2;
  double ci3;
  double cr3;
  long i;
  double tr3;
  double ti4;
  double tr2;
  double tr1;
  double ti3;
  double tr4;
  double ti2;
  double ti1;
  long k;
  /*-----end-of-declarations-----*/
  if (ido != 2) goto L_102;
  for (k=1 ; k<=l1 ; k+=1) {
    ti1 = cc_3(2,1,k)-cc_3(2,3,k);
    ti2 = cc_3(2,1,k)+cc_3(2,3,k);
    tr4 = cc_3(2,4,k)-cc_3(2,2,k);
    ti3 = cc_3(2,2,k)+cc_3(2,4,k);
    tr1 = cc_3(1,1,k)-cc_3(1,3,k);
    tr2 = cc_3(1,1,k)+cc_3(1,3,k);
    ti4 = cc_3(1,2,k)-cc_3(1,4,k);
    tr3 = cc_3(1,2,k)+cc_3(1,4,k);
    ch_3(1,k,1) = tr2+tr3;
    ch_3(1,k,3) = tr2-tr3;
    ch_3(2,k,1) = ti2+ti3;
    ch_3(2,k,3) = ti2-ti3;
    ch_3(1,k,2) = tr1+tr4;
    ch_3(1,k,4) = tr1-tr4;
    ch_3(2,k,2) = ti1+ti4;
    ch_3(2,k,4) = ti1-ti4;
  }
  return;
 L_102: for (k=1 ; k<=l1 ; k+=1) {
   for (i=2 ; i<=ido ; i+=2) {
     ti1 = cc_3(i,1,k)-cc_3(i,3,k);
     ti2 = cc_3(i,1,k)+cc_3(i,3,k);
     ti3 = cc_3(i,2,k)+cc_3(i,4,k);
     tr4 = cc_3(i,4,k)-cc_3(i,2,k);
     tr1 = cc_3(i-1,1,k)-cc_3(i-1,3,k);
     tr2 = cc_3(i-1,1,k)+cc_3(i-1,3,k);
     ti4 = cc_3(i-1,2,k)-cc_3(i-1,4,k);
     tr3 = cc_3(i-1,2,k)+cc_3(i-1,4,k);
     ch_3(i-1,k,1) = tr2+tr3;
     cr3 = tr2-tr3;
     ch_3(i,k,1) = ti2+ti3;
     ci3 = ti2-ti3;
     cr2 = tr1+tr4;
     cr4 = tr1-tr4;
     ci2 = ti1+ti4;
     ci4 = ti1-ti4;
     ch_3(i-1,k,2) = wa1_1(i-1)*cr2-wa1_1(i)*ci2;
     ch_3(i,k,2) = wa1_1(i-1)*ci2+wa1_1(i)*cr2;
     ch_3(i-1,k,3) = wa2_1(i-1)*cr3-wa2_1(i)*ci3;
     ch_3(i,k,3) = wa2_1(i-1)*ci3+wa2_1(i)*cr3;
     ch_3(i-1,k,4) = wa3_1(i-1)*cr4-wa3_1(i)*ci4;
     ch_3(i,k,4) = wa3_1(i-1)*ci4+wa3_1(i)*cr4;
   }
 }
  return;
}
