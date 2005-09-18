/*
 * $Id: md5.i,v 1.1 2005-09-18 22:06:16 dhmunro Exp $
 * routines to compute MD5 checksums
 * entirely in interpreted code, so they are slow,
 *   on the order of a few seconds per megabyte
 * original C source from L. Peter Deutsch ghost@aladdin.com
 *   adapted by David H. Munro
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func md5sum(file, hex=)
/* DOCUMENT digest = md5sum(filename)
 *          digest = md5sum(filename, hex=1)
 *   compute MD5 digest of a file, an array of 16 char
 *   with the hex=1 keyword, returned digest is a string (in hex)
 *
 * SEE ALSO: md5
 */
{
  if (!is_stream(file)) file = open(file, "rb");
  nbyte = sizeof(file);
  if (!nbyte) return md5(md5());
  nchunk = 1000000;  /* process file in megabyte chunks */
  buf = array(char, min(nbyte,nchunk));
  for (state=[],n=0 ; n<nbyte ; n+=nchunk) {
    if (n+nchunk > nbyte) {
      nchunk = nbyte - n;
      if (sizeof(buf) > nchunk) buf = array(char, nchunk);
    }
    if (_read(file,n,buf) < nchunk)
      error, "i/o error reading file";
    state = md5(buf, state);
  }
  return md5(state, hex=hex);
}

func md5(data, state, hex=)
/* DOCUMENT state = md5(data)
 *          state = md5(data, state)
 *          digest = md5(state)
 *          digest = md5(state, hex=1)
 *   compute MD5 digest of data, an array of 16 char
 *   with the hex=1 keyword, returned digest is a string (in hex)
 *
 * SEE ALSO: md5sum
 */
{
  local count, digest;
  type = structof(data);
  if (is_void(state)) {
    if (type == pointer) {
      /* no more data, return MD5 digest */
      count = *data(1);                /* makes a copy of count */
      _md5_increment, count, sizeof(*data(3));  /* add pending */
      /* number of additional bytes to reach 56 mod 64 */
      nbyte = ((55-(count(1)>>3))&63) + 1;
      pad = grow(array(char, nbyte), _md5_number(count));
      pad(1) = '\x80';
      state = md5(pad, data);
      return _md5_hex(_md5_number(*state(2)), hex=hex);
    }

    /* create a new state, this is first call for this digest */
    state = [&[0, 0],
             &[0x67452301, ~0x10325476, ~0x67452301, 0x10325476],
             &[]];
    if (is_void(data)) return state;
  }

  if (type != char) error, "md5 requires char input data";

  data = grow(*state(3), data(*));   /* prepend any leftover data */
  nbyte = sizeof(data);
  next = nbyte & 63;
  state(3) = next? &data(1-next:0) : &[];
  nbyte -= next;

  if (nbyte) {
    eq_nocopy, count, *state(1);
    _md5_increment, count, nbyte;

    nword = nbyte>>6;
    tmp = array(char, 4, 16, nword);
    tmp(*) = data(1:-next);
    tmp = long(tmp);
    data = tmp(1,,) | (tmp(2,,)<<8) | (tmp(3,,)<<16) | (tmp(4,,)<<24);
    tmp = [];

    eq_nocopy, digest, *state(2);
    a0 = digest(1);
    b0 = digest(2);
    c0 = digest(3);
    d0 = digest(4);

    for (i=1 ; i<=nword ; ++i) {
      x = data(_md5_order,i) + _md5_t;
      a = a0;
      b = b0;
      c = c0;
      d = d0;
      /* round 1 */
      a = a + ((b & c) | ((~b) & d)) + x(1);
      a = ((a << 7) | ((a >> 25)&0x0000007f)) + b;
      d = d + ((a & b) | ((~a) & c)) + x(2);
      d = ((d << 12) | ((d >> 20)&0x00000fff)) + a;
      c = c + ((d & a) | ((~d) & b)) + x(3);
      c = ((c << 17) | ((c >> 15)&0x0001ffff)) + d;
      b = b + ((c & d) | ((~c) & a)) + x(4);
      b = ((b << 22) | ((b >> 10)&0x003fffff)) + c;
      a = a + ((b & c) | ((~b) & d)) + x(5);
      a = ((a << 7) | ((a >> 25)&0x0000007f)) + b;
      d = d + ((a & b) | ((~a) & c)) + x(6);
      d = ((d << 12) | ((d >> 20)&0x00000fff)) + a;
      c = c + ((d & a) | ((~d) & b)) + x(7);
      c = ((c << 17) | ((c >> 15)&0x0001ffff)) + d;
      b = b + ((c & d) | ((~c) & a)) + x(8);
      b = ((b << 22) | ((b >> 10)&0x003fffff)) + c;
      a = a + ((b & c) | ((~b) & d)) + x(9);
      a = ((a << 7) | ((a >> 25)&0x0000007f)) + b;
      d = d + ((a & b) | ((~a) & c)) + x(10);
      d = ((d << 12) | ((d >> 20)&0x00000fff)) + a;
      c = c + ((d & a) | ((~d) & b)) + x(11);
      c = ((c << 17) | ((c >> 15)&0x0001ffff)) + d;
      b = b + ((c & d) | ((~c) & a)) + x(12);
      b = ((b << 22) | ((b >> 10)&0x003fffff)) + c;
      a = a + ((b & c) | ((~b) & d)) + x(13);
      a = ((a << 7) | ((a >> 25)&0x0000007f)) + b;
      d = d + ((a & b) | ((~a) & c)) + x(14);
      d = ((d << 12) | ((d >> 20)&0x00000fff)) + a;
      c = c + ((d & a) | ((~d) & b)) + x(15);
      c = ((c << 17) | ((c >> 15)&0x0001ffff)) + d;
      b = b + ((c & d) | ((~c) & a)) + x(16);
      b = ((b << 22) | ((b >> 10)&0x003fffff)) + c;
      /* round 2 */
      a = a + ((b & d) | ((~d) & c)) + x(17);
      a = ((a << 5) | ((a >> 27)&0x0000001f)) + b;
      d = d + ((a & c) | ((~c) & b)) + x(18);
      d = ((d << 9) | ((d >> 23)&0x000001ff)) + a;
      c = c + ((d & b) | ((~b) & a)) + x(19);
      c = ((c << 14) | ((c >> 18)&0x00003fff)) + d;
      b = b + ((c & a) | ((~a) & d)) + x(20);
      b = ((b << 20) | ((b >> 12)&0x000fffff)) + c;
      a = a + ((b & d) | ((~d) & c)) + x(21);
      a = ((a << 5) | ((a >> 27)&0x0000001f)) + b;
      d = d + ((a & c) | ((~c) & b)) + x(22);
      d = ((d << 9) | ((d >> 23)&0x000001ff)) + a;
      c = c + ((d & b) | ((~b) & a)) + x(23);
      c = ((c << 14) | ((c >> 18)&0x00003fff)) + d;
      b = b + ((c & a) | ((~a) & d)) + x(24);
      b = ((b << 20) | ((b >> 12)&0x000fffff)) + c;
      a = a + ((b & d) | ((~d) & c)) + x(25);
      a = ((a << 5) | ((a >> 27)&0x0000001f)) + b;
      d = d + ((a & c) | ((~c) & b)) + x(26);
      d = ((d << 9) | ((d >> 23)&0x000001ff)) + a;
      c = c + ((d & b) | ((~b) & a)) + x(27);
      c = ((c << 14) | ((c >> 18)&0x00003fff)) + d;
      b = b + ((c & a) | ((~a) & d)) + x(28);
      b = ((b << 20) | ((b >> 12)&0x000fffff)) + c;
      a = a + ((b & d) | ((~d) & c)) + x(29);
      a = ((a << 5) | ((a >> 27)&0x0000001f)) + b;
      d = d + ((a & c) | ((~c) & b)) + x(30);
      d = ((d << 9) | ((d >> 23)&0x000001ff)) + a;
      c = c + ((d & b) | ((~b) & a)) + x(31);
      c = ((c << 14) | ((c >> 18)&0x00003fff)) + d;
      b = b + ((c & a) | ((~a) & d)) + x(32);
      b = ((b << 20) | ((b >> 12)&0x000fffff)) + c;
      /* round 3 */
      a = a + (b ~ c ~ d) + x(33);
      a = ((a << 4) | ((a >> 28)&0x0000000f)) + b;
      d = d + (a ~ b ~ c) + x(34);
      d = ((d << 11) | ((d >> 21)&0x000007ff)) + a;
      c = c + (d ~ a ~ b) + x(35);
      c = ((c << 16) | ((c >> 16)&0x0000ffff)) + d;
      b = b + (c ~ d ~ a) + x(36);
      b = ((b << 23) | ((b >> 9)&0x007fffff)) + c;
      a = a + (b ~ c ~ d) + x(37);
      a = ((a << 4) | ((a >> 28)&0x0000000f)) + b;
      d = d + (a ~ b ~ c) + x(38);
      d = ((d << 11) | ((d >> 21)&0x000007ff)) + a;
      c = c + (d ~ a ~ b) + x(39);
      c = ((c << 16) | ((c >> 16)&0x0000ffff)) + d;
      b = b + (c ~ d ~ a) + x(40);
      b = ((b << 23) | ((b >> 9)&0x007fffff)) + c;
      a = a + (b ~ c ~ d) + x(41);
      a = ((a << 4) | ((a >> 28)&0x0000000f)) + b;
      d = d + (a ~ b ~ c) + x(42);
      d = ((d << 11) | ((d >> 21)&0x000007ff)) + a;
      c = c + (d ~ a ~ b) + x(43);
      c = ((c << 16) | ((c >> 16)&0x0000ffff)) + d;
      b = b + (c ~ d ~ a) + x(44);
      b = ((b << 23) | ((b >> 9)&0x007fffff)) + c;
      a = a + (b ~ c ~ d) + x(45);
      a = ((a << 4) | ((a >> 28)&0x0000000f)) + b;
      d = d + (a ~ b ~ c) + x(46);
      d = ((d << 11) | ((d >> 21)&0x000007ff)) + a;
      c = c + (d ~ a ~ b) + x(47);
      c = ((c << 16) | ((c >> 16)&0x0000ffff)) + d;
      b = b + (c ~ d ~ a) + x(48);
      b = ((b << 23) | ((b >> 9)&0x007fffff)) + c;
      /* round 4 */
      a = a + (c ~ ((~d) | b)) + x(49);
      a = ((a << 6) | ((a >> 26)&0x0000003f)) + b;
      d = d + (b ~ ((~c) | a)) + x(50);
      d = ((d << 10) | ((d >> 22)&0x000003ff)) + a;
      c = c + (a ~ ((~b) | d)) + x(51);
      c = ((c << 15) | ((c >> 17)&0x00007fff)) + d;
      b = b + (d ~ ((~a) | c)) + x(52);
      b = ((b << 21) | ((b >> 11)&0x001fffff)) + c;
      a = a + (c ~ ((~d) | b)) + x(53);
      a = ((a << 6) | ((a >> 26)&0x0000003f)) + b;
      d = d + (b ~ ((~c) | a)) + x(54);
      d = ((d << 10) | ((d >> 22)&0x000003ff)) + a;
      c = c + (a ~ ((~b) | d)) + x(55);
      c = ((c << 15) | ((c >> 17)&0x00007fff)) + d;
      b = b + (d ~ ((~a) | c)) + x(56);
      b = ((b << 21) | ((b >> 11)&0x001fffff)) + c;
      a = a + (c ~ ((~d) | b)) + x(57);
      a = ((a << 6) | ((a >> 26)&0x0000003f)) + b;
      d = d + (b ~ ((~c) | a)) + x(58);
      d = ((d << 10) | ((d >> 22)&0x000003ff)) + a;
      c = c + (a ~ ((~b) | d)) + x(59);
      c = ((c << 15) | ((c >> 17)&0x00007fff)) + d;
      b = b + (d ~ ((~a) | c)) + x(60);
      b = ((b << 21) | ((b >> 11)&0x001fffff)) + c;
      a = a + (c ~ ((~d) | b)) + x(61);
      a = ((a << 6) | ((a >> 26)&0x0000003f)) + b;
      d = d + (b ~ ((~c) | a)) + x(62);
      d = ((d << 10) | ((d >> 22)&0x000003ff)) + a;
      c = c + (a ~ ((~b) | d)) + x(63);
      c = ((c << 15) | ((c >> 17)&0x00007fff)) + d;
      b = b + (d ~ ((~a) | c)) + x(64);
      b = ((b << 21) | ((b >> 11)&0x001fffff)) + c;
      a0 += a;
      b0 += b;
      c0 += c;
      d0 += d;
    }

    digest(1) = a0;
    digest(2) = b0;
    digest(3) = c0;
    digest(4) = d0;
  }

  return state;
}

func _md5_increment(count, nbyte)
{
  /* add number of bits to count, with carry if overflows 32 bits */
  count(2) += nbyte >> 29;
  old = count(1);
  nlo = (nbyte << 3) & 0xffffffff;
  count(1) = new = (old + nlo) & 0xffffffff;
  if ((new&0x80000000)? (old & nlo & 0x80000000) : ((old | nlo)&0x80000000))
    count(1) += 1;
}

func _md5_number(w)
{
  return char( w(-,) >> [0,8,16,24] )(*);
}

_md5_order = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
              2,7,12,1,6,11,16,5,10,15,4,9,14,3,8,13,
              6,9,12,15,2,5,8,11,14,1,4,7,10,13,16,3,
              1,8,15,6,13,4,11,2,9,16,7,14,5,12,3,10];

_md5_t = [~0x28955b87, ~0x173848a9, 0x242070db, ~0x3e423111, ~0x0a83f050,
          0x4787c62a, ~0x57cfb9ec, ~0x02b96afe, 0x698098d8, ~0x74bb0850,
          ~0x0000a44e, ~0x76a32841, 0x6b901122, ~0x02678e6c, ~0x5986bc71,
          0x49b40821, ~0x09e1da9d, ~0x3fbf4cbf, 0x265e5a51, ~0x16493855,
          ~0x29d0efa2, 0x02441453, ~0x275e197e, ~0x182c0437, 0x21e1cde6,
          ~0x3cc8f829, ~0x0b2af278, 0x455a14ed, ~0x561c16fa, ~0x03105c07,
          0x676f02d9, ~0x72d5b375, ~0x0005c6bd, ~0x788e097e, 0x6d9d6122,
          ~0x021ac7f3, ~0x5b4115bb, 0x4bdecfa9, ~0x0944b49f, ~0x4140438f,
          0x289b7ec6, ~0x155ed805, ~0x2b10cf7a, 0x04881d05, ~0x262b2fc6,
          ~0x1924661a, 0x1fa27cf8, ~0x3b53a99a, ~0x0bd6ddbb, 0x432aff97,
          ~0x546bdc58, ~0x036c5fc6, 0x655b59c3, ~0x70f3336d, ~0x00100b82,
          ~0x7a7ba22e, 0x6fa87e4f, ~0x01d3191f, ~0x5cfebceb, 0x4e0811a1,
          ~0x08ac817d, ~0x42c50dca, 0x2ad7d2bb, ~0x14792c6e];

func md5_test
{
  test = ["", "a", "abc", "message digest", "abcdefghijklmnopqrstuvwxyz",
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
          "12345678901234567890123456789012345678901234567890123456789"+
          "012345678901234567890"];
  answ = ["d41d8cd98f00b204e9800998ecf8427e",
          "0cc175b9c0f1b6a831c399e269772661",
          "900150983cd24fb0d6963f7d28e17f72",
          "f96b697d7cb7938d525a2f31aaf161d0",
          "c3fcd3d76192e4007dfb496cca67e13b",
          "d174ab98d277d9f5a5611c2c9f419d9f",
          "57edf4a22be3c955ac49da2e2107b67a"];
  correct = [[0xd4,0x1d,0x8c,0xd9,0x8f,0x00,0xb2,0x04,
              0xe9,0x80,0x09,0x98,0xec,0xf8,0x42,0x7e],
             [0x0c,0xc1,0x75,0xb9,0xc0,0xf1,0xb6,0xa8,
              0x31,0xc3,0x99,0xe2,0x69,0x77,0x26,0x61],
             [0x90,0x01,0x50,0x98,0x3c,0xd2,0x4f,0xb0,
              0xd6,0x96,0x3f,0x7d,0x28,0xe1,0x7f,0x72],
             [0xf9,0x6b,0x69,0x7d,0x7c,0xb7,0x93,0x8d,
              0x52,0x5a,0x2f,0x31,0xaa,0xf1,0x61,0xd0],
             [0xc3,0xfc,0xd3,0xd7,0x61,0x92,0xe4,0x00,
              0x7d,0xfb,0x49,0x6c,0xca,0x67,0xe1,0x3b],
             [0xd1,0x74,0xab,0x98,0xd2,0x77,0xd9,0xf5,
              0xa5,0x61,0x1c,0x2c,0x9f,0x41,0x9d,0x9f],
             [0x57,0xed,0xf4,0xa2,0x2b,0xe3,0xc9,0x55,
              0xac,0x49,0xda,0x2e,0x21,0x07,0xb6,0x7a]];
  actual = array(char, 16, numberof(answ));
  for (i=1 ; i<=numberof(answ) ; ++i) {
    data = *pointer(test(i));
    actual(,i) = md5(md5(((numberof(data)>1)?data(1:-1):[])));
  }
  if (anyof(actual!=correct)) error, "md5 function flunked test";
}

func _md5_hex(digest, hex=)
{
  if (!hex) return digest;
  s = swrite(format="%02x",digest);
  s = s(1:16:2) + s(2:16:2);
  s = s(1:8:2) + s(2:8:2);
  s = s(1:4:2) + s(2:4:2);
  return s(1) + s(2);
}
