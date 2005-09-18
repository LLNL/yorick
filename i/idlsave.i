/*
 * $Id: idlsave.i,v 1.1 2005-09-18 22:06:15 dhmunro Exp $
 * read IDL save files
 * IDL is a trademark of Reasearch Systems Incorporated (RSI)
 * code based on work of Craig Markwardt
 *          http://cow.physics.wisc.edu/~craigm/idl/
 */
/*
;  ============== STATEMENT OF RESEARCH SYSTEMS INCORPORATED ==============
;---------------------------------------------------------------------------
; IDL is a product of Research Systems, Inc (RSI). Use of IDL is governed
; by the IDL End User License Agreement (EULA). All IDL users are
; required to read and agree to the terms of the IDL EULA at the time
; that they install IDL.
; 
; The CMSVLIB software, written by Craig Markwardt, embodies
; unpublished proprietary information about the IDL Save file
; format. Research Systems grants to the author of this software, and
; to all IDL users, a license to use and redistribute this software in
; source or binary form, subject to the following conditions:
; 
; 1. The author, and any users of this software must be in full
;    compliance with the IDL End User License Agreement (EULA).
; 2. Redistributions of source code must retain the complete and
;    unaltered text of this notice.
; 3. Redistributions in binary form must reproduce the complete and
;    unaltered text of this notice in the documentation and/or other
;    materials provided with the distribution.
; 4. The name of Research Systems Inc. may not be used to endorse or
;    promote this software or products derived from it without specific
;    prior written permission from Research Systems, Inc.
; 5. Allowed use of this software is limited to reading and writing
;    IDL variable related portions of IDL Save files. It may not be
;    used as a basis for reverse engineering, or otherwise
;    accessing any other portions of an IDL save file, including but
;    not limited to, those portions that encode executable IDL programs.
;    Such use is in violation of the IDL EULA, and will be prosecuted
;    to the fullest extent possible by Research Systems, Inc. It is
;    permissible to read such sections of an IDL save file for the
;    sole purpose of transferring it without examination or interpretation
;    to another save file.
; 6. Research Systems disclaims any responsibility for compatibility
;    with this software, and reserves the right to change the IDL save
;    file format in any way, at any time, including changes that would
;    render this software incomplete or inoperable.
; 7. This software is not a product of Research Systems Inc. Research
;    Systems Inc disclaims any responsibility for its development or
;    maintenance.
; 
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHOR OR RESEARCH SYSTEMS INC BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
; SUCH DAMAGE.
;---------------------------------------------------------------------------
*/

func idl_open(name, &commons, loud=)
/* DOCUMENT f = idl_open(filename)
 *       or f = idl_open(filename, commons)
 *   openb for an IDL save file
 *   optional COMMONS is returned as an array of pointers to
 *     arrays of strings; the first string in each array is the name
 *     of an IDL common block; the others are the names of the
 *     variables in that common block
 *   all variable names have been converted to lower case
 *   loud=1 keyword reports on timestamp and other information
 *     about the user, host, etc., stored in the save file
 *
 *   floating complex data becomes an array of float with leading
 *     dimension of 2, use f2z to recover complex
 *   64 bit integers become an array of long with leading dimension
 *     of 2, use l2ll to recover single long (if sizeof(long)=8)
 *
 * SEE ALSO: openb, f2z, l2ll
 */
{
  f = open(name, "rb");
  sign = array(char, 4);
  _read, f, 0, sign;
  if (anyof(sign != ['S','R','\0','\4']))
    error, name+" signature not that of IDL save file";
  /* Markwardt doesn't say that save file is XDR format, but seems to be */
  xdr_primitives, f;  /* ?? save files always big-endian, 4 byte longs ?? */
  len = sizeof(f);

  commons = [];
  ncommon = 0;
  a64 = 0;
  for (addr=4 ; addr<len ;) {
    addr0 = addr;
    type = _idl_record(f, a64, addr);
    if (type == 6) break;
    addr0 += 16+a64;
    if (type == 10) {
      addr0 += 1024;
      date = _idl_string(f, addr0);
      user = _idl_string(f, addr0);
      host = _idl_string(f, addr0);
      if (loud) {
        write, format="Date: %s\n", date;
        write, format="User: %s\n", user;
        write, format="Host: %s\n", host;
      }
    } else if (type == 14) {
      sfmt = 0;
      _read, f, addr0, sfmt;
      addr0 += 4;
      arch = _idl_string(f, addr0);
      osys = _idl_string(f, addr0);
      ridl = _idl_string(f, addr0);
      if (loud) {
        write, format="Save: %ld\n", sfmt;
        write, format="Arch: %s\n", arch;
        write, format="OS:   %s\n", osys;
        write, format="IDL:  %s\n", ridl;
      }
    } else if (type == 13) {
      author = _idl_string(f, addr0);
      title = _idl_string(f, addr0);
      other = _idl_string(f, addr0);
      if (loud) {
        write, format="Author: %s\n", author;
        write, format="Title: %s\n", title;
        write, format="Other: %s\n", other;
      }
    } else if (type == 15) {
      write, "WARNING: "+name+" has IDL pointers";
    } else if (type == 17) {
      if (loud) write, "64 bit addresses present";
    } else if (type == 1) {
      nvars = 0;
      _read, f, addr0, nvars;
      addr0 += 4;
      if (ncommon >= numberof(commons))
        grow, commons, array(pointer, max(numberof(commons), 4));
      com = array(string, nvars+1);
      for (i=1 ; i<=nvars+1 ; i++) com(i) = _idl_string(f, addr0, 1);
      commons(++ncommon) = &com;
      com = [];
    } else if (type==2 || type==3) {
      vname = _idl_string(f, addr0, 1);
      if (_idl_type(f, addr0, vtype, vdims))
        add_variable, f, addr0, vname, vtype, vdims;
    }
  }
  if (ncommon && loud) write, format="Common blocks: %ld\n", ncommon;
  return f;
}

/* record types:
 * 0  start_marker -- start of save file
 * 1  common -- common block
 * 2  variable
 * 3  system_variable
 * 6  end_marker -- end of save file (no more records)
 * 10 timestamp
 * 12 compiled -- IDL byte code
 * 13 identification -- of author
 * 14 version -- of IDL
 * 15 heap_header -- index info for heap
 * 16 heap_data -- heaps used for pointer data
 * 17 promote64 -- begin 64 bit record addresses
 */

func _idl_record(f, &a64, &addr)
{
  head = array(long, 3);
  _read, f, addr, head;
  type = head(1);
  addr = head(2);
  addrlo = head(3);
  /* Markwardt doesn't say if promote64 record itself has 8 byte addr! */
  if (!addr && addrlo) addr = addrlo;
  else if (a64) addr = addrlo | (addr<<32);
  if (type == 17) a64 = 4;
  return type;
}

func _idl_string(f, &addr, lc)
{
  len = 0;
  _read, f, addr, len;
  addr += 4;
  if (len > 0) {
    c = array(char, len);
    _read, f, addr, c;
    addr += len;
    len &= 3;
    if (len) addr += 4-len;
    if (lc) {
      list = where((c>='A') & (c<='Z'));
      if (numberof(list)) c(list) |= ('A'~'a');
    }
  } else if (len < 0) {
    c = [];
  }
  return string(&c);
}

/* data types (Sun XDR format):
 * 1  char
 * 2  short
 * 3  long
 * 4  float
 * 5  double
 * 6  fcomplex
 * 7  (string)
 * 8  (struct)
 * 9  complex
 * 10 (pointer)
 * 11 (object reference)
 * 12 ushort
 * 13 ulong
 * 14 llong (64 bit)
 * 15 ullong (64 bit)
 */

func _idl_type(f, &addr, &vtype, &vdims)
{
  vtype = 0;
  _read, f, addr, vtype;
  addr += 4;
  flag = 0;
  _read, f, addr, flag;
  addr += 4;
  vdims = [];
  /* flag bit 0x10 may indicate membership in a common block */
  if (flag & 0x24) {
    ad = array(0, 4);
    _read, f, addr, ad;
    /* ad(2) = 0x02 normally, 0x04, 0x08 observed in common block vars
     * 0x36 in system vars with broken dimension lists (IDL 6.0)
     */
    if (ad(1)!=8 || (ad(2)&0x20)) {
      if (loud) write, "WARNING: unknown type, skipping "+vname;
      return 0;
    }
    ndims = 0;
    addr += 16;
    _read, f, addr, ndims;
    addr += 12;
    if (ndims<1 || ndims>10) {
      if (loud) write, "WARNING: bad dims, skipping "+vname;
      return 0;
    }
    vdims = array(0, 1+ndims);
    _read, f, addr, vdims;
    addr += 4*(vdims(1)+1);
    vdims(1) = ndims;
    if (flag & 0x20) {
      /* don't bother with structs for now */
      if (loud) write, "WARNING: struct type, skipping "+vname;
      return 0;
    }
  }
  addr += 4;
  if (vtype == 1) {
    vtype = char;
    return 1;
  } else if (vtype==2 || vtype==12) {
    vtype = short;
    return 1;
  } else if (vtype==3 || vtype==13) {
    vtype = long;
    return 1;
  } else if (vtype == 4) {
    vtype = float;
    return 1;
  } else if (vtype == 5) {
    vtype = double;
    return 1;
  } else if (vtype == 9) {
    vtype = complex;
    return 1;
  } else if (vtype == 6) {
    /* see f2z below */
    vtype = float;
    if (!numberof(vdims)) {
      vdims = [1,2];
    } else {
      vdims = grow([vdims(1)+1],vdims);
      vdims(2) = 2;
    }
    return 1;
  } else if (vtype==14 || vtype==15) {
    /* see l2ll below */
    vtype = long;
    if (!numberof(vdims)) {
      vdims = [1,2];
    } else {
      vdims = grow([vdims(1)+1],vdims);
      vdims(2) = 2;
    }
    return 1;
  }
  return 0;
}

func f2z(x)
/* DOCUMENT z = f2z(x)
 *   convert 2-by-dims float or double X to complex.
 */
{
  z = x(1,..)+0.0i;
  z.im = x(2,..);
  return z;
}

func l2ll(x)
/* DOCUMENT z = l2ll(x)
 *   convert 2-by-dims 32 bit integer X to 64 bit integer
 *   (only works if sizeof(long)=8)
 */
{
  return long(x(2,..)) | (long(x(1,..))<<32);
}
