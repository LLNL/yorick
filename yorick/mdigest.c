/* mdigest.c
 * md5 and sha1 message digest functions from public domain starting points:
 * md5 - Alexander Peslyak 
 * http://openwall.info/wiki/people/solar/software/public-domain-source-code/md5
 * sha1 - Steve Reid
 * ftp://ftp.funet.fi/pub/crypt/hash/sha/sha1.c
 * Modifications by David H. Munro (github.com/dhmunro) are hereby placed
 * in the public domain.
 */
/* The code in this file is a blend of the Peslyak and Reid code,
 * extracting what I considered to be the strongest points of each.
 * I chose portability and conciseness over speed.  It is futile to
 * expect portable C code to run as fast as hand coded assembler
 * optimized to run on a specific platform, which is far more difficult
 * to maintain as well as to write, so I haven't tried.
 */

#include "mdigest.h"
#include <string.h>

/* ------------------------------------------------------------------------ */

/* MD5 functions */
#define F(x, y, z) ((z) ^ ((x) & ((y) ^ (z))))
#define G(x, y, z) ((y) ^ ((z) & ((x) ^ (y))))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | ~(z)))
/* SHA-1 uses F and H, plus the following function */
#define J(x, y, z) ((((x) | (y)) & (z)) | ((x) & (y)))

#define ROL(v, b) (((v) << (b)) | (((v)&0xffffffff) >> (32 - (b))))

/* SET reads 4 input bytes in little (md5) or big (sha1) endian byte
 * order and stores them in a properly aligned word in host byte order
 */
#define SETLE(n) (wkspc[(n)] = \
 (md_uint32)ptr[(n) * 4] | ((md_uint32)ptr[(n) * 4 + 1] << 8) | \
 ((md_uint32)ptr[(n) * 4 + 2] << 16) | ((md_uint32)ptr[(n) * 4 + 3] << 24))
#define SETBE(n) (wkspc[(n)] = \
 ((md_uint32)ptr[(n) * 4] << 24) | ((md_uint32)ptr[(n) * 4 + 1] << 16) | \
 ((md_uint32)ptr[(n) * 4 + 2] << 8) | (md_uint32)ptr[(n) * 4 + 3])
#define GET(n) (wkspc[(n)])

/* md5_block and sha1_block are the workhorses
 * they process 64 byte (512 bit) blocks; nbytes>0 must be a multiple of 64
 * the state variables ctx->a-d for md5 or ctx->a-e for sha1 are updated
 * the bit counter ctx->(lo,hi) is not incremented here
 * the return value is the input data address plus nbytes bytes
 */
static void *md5_block(md_state *ctx, void *data, unsigned long nbytes);
static void *sha1_block(md_state *ctx, void *data, unsigned long nbytes);

#define MD5STEP(f, a, b, c, d, x, t, s) \
 (a)+=f((b),(c),(d))+(x)+(t); (a)=ROL((a),(s)); (a)+=(b)

static void *
md5_block(md_state *ctx, void *data, unsigned long nbytes)
{
  unsigned char *ptr = data;
  md_uint32 a, b, c, d, a0, b0, c0, d0;
  md_uint32 wkspc[16];  /* set by SETLE macro in first round */
  if (!nbytes) return ptr;
  a = ctx->a;
  b = ctx->b;
  c = ctx->c;
  d = ctx->d;
  do {
    a0=a; b0=b; c0=c; d0=d;
    /* round 1 */
    MD5STEP(F, a, b, c, d, SETLE(0), 0xd76aa478, 7);
    MD5STEP(F, d, a, b, c, SETLE(1), 0xe8c7b756, 12);
    MD5STEP(F, c, d, a, b, SETLE(2), 0x242070db, 17);
    MD5STEP(F, b, c, d, a, SETLE(3), 0xc1bdceee, 22);
    MD5STEP(F, a, b, c, d, SETLE(4), 0xf57c0faf, 7);
    MD5STEP(F, d, a, b, c, SETLE(5), 0x4787c62a, 12);
    MD5STEP(F, c, d, a, b, SETLE(6), 0xa8304613, 17);
    MD5STEP(F, b, c, d, a, SETLE(7), 0xfd469501, 22);
    MD5STEP(F, a, b, c, d, SETLE(8), 0x698098d8, 7);
    MD5STEP(F, d, a, b, c, SETLE(9), 0x8b44f7af, 12);
    MD5STEP(F, c, d, a, b, SETLE(10), 0xffff5bb1, 17);
    MD5STEP(F, b, c, d, a, SETLE(11), 0x895cd7be, 22);
    MD5STEP(F, a, b, c, d, SETLE(12), 0x6b901122, 7);
    MD5STEP(F, d, a, b, c, SETLE(13), 0xfd987193, 12);
    MD5STEP(F, c, d, a, b, SETLE(14), 0xa679438e, 17);
    MD5STEP(F, b, c, d, a, SETLE(15), 0x49b40821, 22);
    /* round 2 */
    MD5STEP(G, a, b, c, d, GET(1), 0xf61e2562, 5);
    MD5STEP(G, d, a, b, c, GET(6), 0xc040b340, 9);
    MD5STEP(G, c, d, a, b, GET(11), 0x265e5a51, 14);
    MD5STEP(G, b, c, d, a, GET(0), 0xe9b6c7aa, 20);
    MD5STEP(G, a, b, c, d, GET(5), 0xd62f105d, 5);
    MD5STEP(G, d, a, b, c, GET(10), 0x02441453, 9);
    MD5STEP(G, c, d, a, b, GET(15), 0xd8a1e681, 14);
    MD5STEP(G, b, c, d, a, GET(4), 0xe7d3fbc8, 20);
    MD5STEP(G, a, b, c, d, GET(9), 0x21e1cde6, 5);
    MD5STEP(G, d, a, b, c, GET(14), 0xc33707d6, 9);
    MD5STEP(G, c, d, a, b, GET(3), 0xf4d50d87, 14);
    MD5STEP(G, b, c, d, a, GET(8), 0x455a14ed, 20);
    MD5STEP(G, a, b, c, d, GET(13), 0xa9e3e905, 5);
    MD5STEP(G, d, a, b, c, GET(2), 0xfcefa3f8, 9);
    MD5STEP(G, c, d, a, b, GET(7), 0x676f02d9, 14);
    MD5STEP(G, b, c, d, a, GET(12), 0x8d2a4c8a, 20);
    /* round 3 */
    MD5STEP(H, a, b, c, d, GET(5), 0xfffa3942, 4);
    MD5STEP(H, d, a, b, c, GET(8), 0x8771f681, 11);
    MD5STEP(H, c, d, a, b, GET(11), 0x6d9d6122, 16);
    MD5STEP(H, b, c, d, a, GET(14), 0xfde5380c, 23);
    MD5STEP(H, a, b, c, d, GET(1), 0xa4beea44, 4);
    MD5STEP(H, d, a, b, c, GET(4), 0x4bdecfa9, 11);
    MD5STEP(H, c, d, a, b, GET(7), 0xf6bb4b60, 16);
    MD5STEP(H, b, c, d, a, GET(10), 0xbebfbc70, 23);
    MD5STEP(H, a, b, c, d, GET(13), 0x289b7ec6, 4);
    MD5STEP(H, d, a, b, c, GET(0), 0xeaa127fa, 11);
    MD5STEP(H, c, d, a, b, GET(3), 0xd4ef3085, 16);
    MD5STEP(H, b, c, d, a, GET(6), 0x04881d05, 23);
    MD5STEP(H, a, b, c, d, GET(9), 0xd9d4d039, 4);
    MD5STEP(H, d, a, b, c, GET(12), 0xe6db99e5, 11);
    MD5STEP(H, c, d, a, b, GET(15), 0x1fa27cf8, 16);
    MD5STEP(H, b, c, d, a, GET(2), 0xc4ac5665, 23);
    /* round 4 */
    MD5STEP(I, a, b, c, d, GET(0), 0xf4292244, 6);
    MD5STEP(I, d, a, b, c, GET(7), 0x432aff97, 10);
    MD5STEP(I, c, d, a, b, GET(14), 0xab9423a7, 15);
    MD5STEP(I, b, c, d, a, GET(5), 0xfc93a039, 21);
    MD5STEP(I, a, b, c, d, GET(12), 0x655b59c3, 6);
    MD5STEP(I, d, a, b, c, GET(3), 0x8f0ccc92, 10);
    MD5STEP(I, c, d, a, b, GET(10), 0xffeff47d, 15);
    MD5STEP(I, b, c, d, a, GET(1), 0x85845dd1, 21);
    MD5STEP(I, a, b, c, d, GET(8), 0x6fa87e4f, 6);
    MD5STEP(I, d, a, b, c, GET(15), 0xfe2ce6e0, 10);
    MD5STEP(I, c, d, a, b, GET(6), 0xa3014314, 15);
    MD5STEP(I, b, c, d, a, GET(13), 0x4e0811a1, 21);
    MD5STEP(I, a, b, c, d, GET(4), 0xf7537e82, 6);
    MD5STEP(I, d, a, b, c, GET(11), 0xbd3af235, 10);
    MD5STEP(I, c, d, a, b, GET(2), 0x2ad7d2bb, 15);
    MD5STEP(I, b, c, d, a, GET(9), 0xeb86d391, 21);
    /* accumulate state */
    a+=a0; b+=b0; c+=c0; d+=d0;
    ptr += 64;
  } while (nbytes -= 64);
  ctx->a = a;
  ctx->b = b;
  ctx->c = c;
  ctx->d = d;
  return ptr;
}

#define SHA1GET(n) (wkspc[(n)&15]=ROL(wkspc[(n+13)&15] ^ wkspc[(n+8)&15] ^ \
                                      wkspc[(n+2)&15] ^ wkspc[(n)&15], 1))
#define SHA1STEP(f,v,w,x,y,z,b,c) \
  (z)+=f((w),(x),(y))+(b)+(c)+ROL((v),5); (w)=ROL((w),30)
#define R0(v,w,x,y,z,i) SHA1STEP(F,v,w,x,y,z, SETBE(i),  0x5a827999)
#define R1(v,w,x,y,z,i) SHA1STEP(F,v,w,x,y,z, SHA1GET(i), 0x5a827999)
#define R2(v,w,x,y,z,i) SHA1STEP(H,v,w,x,y,z, SHA1GET(i), 0x6ed9eba1)
#define R3(v,w,x,y,z,i) SHA1STEP(J,v,w,x,y,z, SHA1GET(i), 0x8f1bbcdc)
#define R4(v,w,x,y,z,i) SHA1STEP(H,v,w,x,y,z, SHA1GET(i), 0xca62c1d6)

static void *
sha1_block(md_state *ctx, void *data, unsigned long nbytes)
{
  unsigned char *ptr = data;
  md_uint32 a, b, c, d, e, a0, b0, c0, d0, e0;
  md_uint32 wkspc[16];  /* set in first round by R0 macro */
  if (!nbytes) return ptr;
  a = ctx->a;
  b = ctx->b;
  c = ctx->c;
  d = ctx->d;
  e = ctx->e;
  do {
    a0=a; b0=b; c0=c; d0=d; e0=e;
    /* 4 rounds of 20 operations each, unrolled */
    R0(a,b,c,d,e, 0); R0(e,a,b,c,d, 1); R0(d,e,a,b,c, 2); R0(c,d,e,a,b, 3);
    R0(b,c,d,e,a, 4); R0(a,b,c,d,e, 5); R0(e,a,b,c,d, 6); R0(d,e,a,b,c, 7);
    R0(c,d,e,a,b, 8); R0(b,c,d,e,a, 9); R0(a,b,c,d,e,10); R0(e,a,b,c,d,11);
    R0(d,e,a,b,c,12); R0(c,d,e,a,b,13); R0(b,c,d,e,a,14); R0(a,b,c,d,e,15);
    R1(e,a,b,c,d,16); R1(d,e,a,b,c,17); R1(c,d,e,a,b,18); R1(b,c,d,e,a,19);
    R2(a,b,c,d,e,20); R2(e,a,b,c,d,21); R2(d,e,a,b,c,22); R2(c,d,e,a,b,23);
    R2(b,c,d,e,a,24); R2(a,b,c,d,e,25); R2(e,a,b,c,d,26); R2(d,e,a,b,c,27);
    R2(c,d,e,a,b,28); R2(b,c,d,e,a,29); R2(a,b,c,d,e,30); R2(e,a,b,c,d,31);
    R2(d,e,a,b,c,32); R2(c,d,e,a,b,33); R2(b,c,d,e,a,34); R2(a,b,c,d,e,35);
    R2(e,a,b,c,d,36); R2(d,e,a,b,c,37); R2(c,d,e,a,b,38); R2(b,c,d,e,a,39);
    R3(a,b,c,d,e,40); R3(e,a,b,c,d,41); R3(d,e,a,b,c,42); R3(c,d,e,a,b,43);
    R3(b,c,d,e,a,44); R3(a,b,c,d,e,45); R3(e,a,b,c,d,46); R3(d,e,a,b,c,47);
    R3(c,d,e,a,b,48); R3(b,c,d,e,a,49); R3(a,b,c,d,e,50); R3(e,a,b,c,d,51);
    R3(d,e,a,b,c,52); R3(c,d,e,a,b,53); R3(b,c,d,e,a,54); R3(a,b,c,d,e,55);
    R3(e,a,b,c,d,56); R3(d,e,a,b,c,57); R3(c,d,e,a,b,58); R3(b,c,d,e,a,59);
    R4(a,b,c,d,e,60); R4(e,a,b,c,d,61); R4(d,e,a,b,c,62); R4(c,d,e,a,b,63);
    R4(b,c,d,e,a,64); R4(a,b,c,d,e,65); R4(e,a,b,c,d,66); R4(d,e,a,b,c,67);
    R4(c,d,e,a,b,68); R4(b,c,d,e,a,69); R4(a,b,c,d,e,70); R4(e,a,b,c,d,71);
    R4(d,e,a,b,c,72); R4(c,d,e,a,b,73); R4(b,c,d,e,a,74); R4(a,b,c,d,e,75);
    R4(e,a,b,c,d,76); R4(d,e,a,b,c,77); R4(c,d,e,a,b,78); R4(b,c,d,e,a,79);
    /* accumulate state */
    a+=a0; b+=b0; c+=c0; d+=d0; e+=e0;
    ptr += 64;
  } while (nbytes -= 64);
  ctx->a = a;
  ctx->b = b;
  ctx->c = c;
  ctx->d = d;
  ctx->e = e;
  return ptr;
}

/* md5 and sha1 initialize state vector to identical values!? */
void
md_init(md_state *ctx)
{
  ctx->lo = ctx->hi = 0;  /* message bit count */
  ctx->a = 0x67452301;
  ctx->b = 0xefcdab89;
  ctx->c = 0x98badcfe;
  ctx->d = 0x10325476;
  ctx->e = 0xc3d2e1f0;  /* unused for md5 */
}

/* both md5 and sha1 operate on 64 byte (512 bit) blocks */
static void md_update(md_state *ctx, void *data, unsigned long nbytes,
                      void *(*block)(md_state *ctx, void *data,
                                     unsigned long nbytes));
static void
md_update(md_state *ctx, void *data, unsigned long nbytes,
          void *(*block)(md_state *ctx, void *data, unsigned long nbytes))
{
  unsigned long i, j;
  j = (ctx->lo >> 3) & 63;  /* number of bytes waiting in ctx->buffer */
  /* ctx->(lo,hi) = number of bits that will have been processed */
  if ((ctx->lo += (nbytes << 3)) < (nbytes << 3)) ctx->hi++;
  ctx->hi += (nbytes >> 29);
  if ((j + nbytes) > 63) {
    if ((i = 64 - j)) {  /* flush remainder from previous call */
      memcpy(&ctx->buffer[j], data, i);
      block(ctx, ctx->buffer, 64);
      data = (unsigned char *)data + i;
      nbytes -= i;
    }
    if (nbytes) {
      data = block(ctx, data, nbytes & ~0x3fUL);
      nbytes &= 0x3fUL;
    }
    j = 0;
  }
  /* store remainder in ctx buffer for next time */
  if (nbytes) memcpy(&ctx->buffer[j], data, nbytes);
}

void
md5_update(md_state *ctx, void *data, unsigned long nbytes)
{
  md_update(ctx, data, nbytes, md5_block);
}

void
sha1_update(md_state *ctx, void *data, unsigned long nbytes)
{
  md_update(ctx, data, nbytes, sha1_block);
}

#define PUTLE(a,i,v) (a)[i]=(v); (a)[i+1]=(v)>>8; \
  (a)[i+2]=(v)>>16; (a)[i+3]=(v)>>24;
#define PUTBE(a,i,v) (a)[i]=(v)>>24; (a)[i+1]=(v)>>16; \
  (a)[i+2]=(v)>>8; (a)[i+3]=(v) 

void
md5_final(void *rslt, md_state *ctx)
{
  unsigned char *result = rslt;
  unsigned long i, j;
  i = (ctx->lo>>3) & 0x3f;
  ctx->buffer[i++] = 0x80;  /* one bit marker after data */
  j = 64 - i;
  if (j < 8) {  /* not enough space for bit count, flush */
    memset(&ctx->buffer[i], 0, j);
    md5_block(ctx, ctx->buffer, 64);
    i = 0;
    j = 64;
  }
  memset(&ctx->buffer[i], 0, j - 8);  /* zero pad */
  /* append message bit count (in little-endian order) */
  PUTLE(ctx->buffer, 56, ctx->lo);
  PUTLE(ctx->buffer, 60, ctx->hi);
  /* flush final 64 byte (512 bit) block */
  md5_block(ctx, ctx->buffer, 64);
  /* return 16 byte (128 bit) md5 message digest (little-endian) */
  PUTLE(result, 0, ctx->a);
  PUTLE(result, 4, ctx->b);
  PUTLE(result, 8, ctx->c);
  PUTLE(result, 12, ctx->d);
  /* clear input state (why bother? have to call md_init anyway) */
  memset(ctx, 0, sizeof(md_state));
}

void
sha1_final(void *rslt, md_state *ctx)
{
  unsigned char *result = rslt;
  unsigned long i, j;
  i = (ctx->lo>>3) & 0x3f;
  ctx->buffer[i++] = 0x80;  /* one bit marker after data */
  j = 64 - i;
  if (j < 8) {  /* not enough space for bit count, flush */
    memset(&ctx->buffer[i], 0, j);
    sha1_block(ctx, ctx->buffer, 64);
    i = 0;
    j = 64;
  }
  memset(&ctx->buffer[i], 0, j - 8);  /* zero pad */
  /* append message bit count (in big-endian order) */
  PUTBE(ctx->buffer, 56, ctx->hi);
  PUTBE(ctx->buffer, 60, ctx->lo);
  /* flush final 64 byte (512 bit) block */
  sha1_block(ctx, ctx->buffer, 64);
  /* return 20 byte (160 bit) sha1 message digest (big-endian) */
  PUTBE(result, 0, ctx->a);
  PUTBE(result, 4, ctx->b);
  PUTBE(result, 8, ctx->c);
  PUTBE(result, 12, ctx->d);
  PUTBE(result, 16, ctx->e);
  /* clear input state (why bother? have to call md_init anyway) */
  memset(ctx, 0, sizeof(md_state));
}
