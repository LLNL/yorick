/* mdigest.h
 * md5 and sha1 message digest functions from public domain starting points:
 * md5 - Alexander Peslyak 
 * http://openwall.info/wiki/people/solar/software/public-domain-source-code/md5
 * sha1 - Steve Reid
 * ftp://ftp.funet.fi/pub/crypt/hash/sha/sha1.c
 * Modifications by David H. Munro (github.com/dhmunro) are hereby placed
 * in the public domain.
 */
#ifndef MDIGEST_H
#define MDIGEST_H

/* any 32 bit or wider integer data type will do
 * int is 32 bits on all major server, desktop, or laptop platforms
 * long may be necessary on platforms with 16 bit words
 */
#ifndef MD_INT32
#define MD_INT32 int
#endif
typedef unsigned MD_INT32 md_uint32;

/* md5 and sha1 share state struct and initialization function */
typedef struct md_state md_state;
struct md_state {
  md_uint32 lo, hi, a, b, c, d, e;
  unsigned char buffer[64];
};
extern void md_init(md_state *ctx);

extern void md5_update(md_state *ctx, void *data, unsigned long nbytes);
extern void md5_final(void *result, md_state *ctx);
extern void sha1_update(md_state *ctx, void *data, unsigned long nbytes);
extern void sha1_final(void *result, md_state *ctx);

#endif
