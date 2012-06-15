/*
 * $Id: pnm.i,v 1.1 2005-09-18 22:06:04 dhmunro Exp $
 *
 * Modern reference: http://netpbm.sourceforge.net
 *
 * Read and write PBM, PGM, or PPM image files defined by the PBMPLUS
 * package by Jef Poskanzer.  The most recent version of this package
 * (as of 16 Apr 1996) is called NetPBM and is available from:
 * * wuarchive.wustl.edu (128.252.135.4),
 *   directory /graphics/graphics/packages/NetPBM
 * * ikaros.fysik4.kth.se (130.237.35.2), directory /pub/netpbm.
 * * ftp.informatik.uni-oldenburg.de (134.106.1.9). This site also carries
 *   binaries for the Amiga.
 * * peipa.essex.ac.uk (155.245.115.161), directory ipa/src/manip
 * * ftp.rahul.net (192.160.13.1), directory /pub/davidsen/source
 * * ftp.cs.ubc.ca, directory /ftp/archive/netpbm
 * Among the converters that come with this package are GIF, FITS,
 * PICT, and TIFF.  Many, many other formats are supported.
 *
 * See also the Independent JPEG group software for conversion to and
 * from JFIF (JPEG) files.  The home archive site is:
 *    ftp.uu.net:/graphics/jpeg/
 * E-mail jpeg-info@uunet.uu.net for more information.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

local pnm;
/* DOCUMENT #include "pnm.i"
     makes pnm_read and pnm_write functions available, which see.
 */

/* ------------------------------------------------------------------------ */

func pnm_read(filename, noflip)
/* DOCUMENT image= pnm_read(filename)
         or image= pnm_read(filename, noflip)

     read a PBM, PGM, or PPM image from FILENAME.  PBM and PGM files
     result in width-by-height arrays; PPM files result in 3-by-width-
     by-height arrays.  If NOFLIP is present and non-zero, the returned
     image is in exactly the order stored in FILENAME.  Otherwise, the
     height dimension is reversed, since images in the files read top
     to bottom, while the Yorick image plotting commands go bottom to
     top.

   SEE ALSO: pnm_display, pnm_write
 */
{
  f= open(filename, "rb");
  magic= array('\0', 2);
  if (_read(f,0,magic)<2 || magic(1)!='P' ||
      magic(2)<'1' || magic(2)>'6')
    error, "not a PBM, PGM, or PPM file: "+filename;
  magic= long(magic(2)-'0');
  raw= magic>3;
  magic%= 3;
  if (raw) {
    image= _pnm_rawrd(f, magic);
  } else {
    /* reopen f as a text file for text PNM magic numbers */
    f=open(filename);
    image= _pnm_txtrd(f, magic);
  }
  if (!noflip) image= image(..,::-1);
  return image;
}

func _pnm_txtrd(f, magic)
{
  /* read text PNM */
  line= rdline(f);
  tok= strtok(line);
  line= tok(2);
  tok= tok(1);
  if (strlen(tok)!=2)
    error, "garbled PNM header in "+filename;

  width= height= maxcol= 0;
  if (magic==1) maxcol= 1;  /* maxcol not present in PBM */
  while (!width || !height || !maxcol) {
    tok= strtok(line);
    line= tok(2);
    tok= tok(1);
    if (tok && strpart(tok,1:1)!="#") {
      n= 0;
      if (sread(tok,format="%ld",n)!=1 || n<=0)
        error, "bad PNM header in "+filename;
      if (!width) width= n;
      else if (!height) height= n;
      else if (!maxcol) maxcol= n;
    } else {
      line= rdline(f);
    }
    if (!strlen(line)) line= rdline(f);
  }

  /* construct result, treating PBM as PGM since text PBM is
     one character per pixel */
  image= _pnm_image(width, height, maxcol, magic+(magic==1));

  /* PGM and PPM files may be read as ordinary numbers
   * PBM files may have no whitespace between 0s and 1s */
  if (magic!=1) format= "%d";
  else format= "%1d";

  /* allow image to start on same line as maxcol - yuck */
  n= sread(line, format=format, image);
  img= array(structof(image), numberof(image)-n);
  while (numberof(image)>n) {
    m= read(f, format=format, img);
    if (!m) {
      /* don't distinguish between legal comments with # and
       * illegal garbage characters that halt read */
      if (!rdline(f)) error, "early end-of-file in "+filename;
      backup, f;
      continue;
    }
    image(n+1:n+m)= img(1:m);
    n+= m;
  }

  return image;
}

func _pnm_rawrd(f, magic)
{
  /* read binary PNM -- much faster than text */
  addr= height= width= maxcol= 0;
  if (magic==1) maxcol= 1;
  _pnm_token, f, addr;
  if (addr!=3 ||
      sread(_pnm_token(f,addr), width)!=1 || width<=0 ||
      sread(_pnm_token(f,addr), height)!=1 || height<=0 ||
      (!maxcol && sread(_pnm_token(f,addr), maxcol)!=1) || maxcol<=0)
    error, "bad PNM header in "+filename;
  if (maxcol > 65535)
    error, "max color > 65535 in raw PNM file "+filename;
  image= _pnm_image(width, height, maxcol, magic);
  col = 0;
  if (structof(image) != char) {
    col = (dimsof(image)(1) == 3);
    if (col) image = char(image)(,-:1:2,,);
    else image = char(image)(-:1:2,,);
    ++col;
  }
  _read, f, addr, image;
  return _pnm_image(image, width, height, col);
}

func _pnm_token(f, &addr)
{
  /* binary PNM token finder must be extremely robust, since file
     may have come from a different architecture and must have been
     transmitted in binary mode */
  line= array(char, 80);  /* lines guaranteed less than 70 chars */
  for (;;) {
    n= _read(f, addr, line);
    if (!n) error, "end-of-file in PNM header in "+filename;
    if (n<80) line= line(1:n);
    i= 1;
    c= line(i);
    while ((c==' '||c=='\t') && i<n) c= line(++i);
    if (i==n) error, "PNM header line too long in "+filename;
    if (c=='\012' || c=='\015') {
      addr+= 1;
    } else {
      addr+= i-1;
      if (i>1) line= line(i:n);
      if (c=='#') {
        list= where((line=='\012') | (line=='\015'));
        if (!numberof(list))
          error, "PNM header line too long in "+filename;
        addr+= list(1);
      } else {
        list= where(line<=' ');
        if (!numberof(list))
          error, "PNM header line too long in "+filename;
        list= list(1);
        addr+= list;
        return string(&line(1:list));
      }
    }
  }
}

func _pnm_image(width, height, maxcol, magic)
{
  task = dimsof(width)(1);
  if (task) {
    if (task>1) {   /* called from _pnm_rawrd */
      /* here, magic = 1 is 2D 2 byte, 2 is 3D 2 byte */
      if (magic) {  /* first byte is msb */
        if (magic > 1)
          width = (long(width(,2,,)) & 0xff) | ((long(width(,1,,)) & 0xff)<<8);
        else
          width = (long(width(2,,)) & 0xff) | ((long(width(1,,)) & 0xff)<<8);
        maxcol = max(width);
        if (maxcol < 256) width = char(width);
        else if (maxcol < 32768) width = short(width);
      }
      return width;
    }
    /* reconstruct binary PBM from packed bits */
    image = array(char, ((height+7)/8)*8, maxcol);
    n = numberof(image);
    image(1:n:8) = width>>7;
    image(2:n:8) = width>>6;
    image(3:n:8) = width>>5;
    image(4:n:8) = width>>4;
    image(5:n:8) = width>>3;
    image(6:n:8) = width>>2;
    image(7:n:8) = width>>1;
    image(8:n:8) = width;
    width = [];
    return image(1:height,) & '\1';
  }

  /* construct result array - binary PBM stored as packed bits */
  if (magic==1) return array(char, (width+7)/8*height);
  if (maxcol<256) type = char;
  else if (maxcol<32768) type = short;
  else type = long;
  if (magic==2) return array(type, width, height);
  return array(type, 3, width, height);
}

/* ------------------------------------------------------------------------ */

func pnm_display(image, size=, flip=)
/* DOCUMENT pnm_display, image

     Attempt to display the IMAGE with the pli command and a pseudo-color
     palette.  The IMAGE must be a 3-by-width-by-height array of RGB
     pixel values.  If called as a function, the return value is the
     width-by-height array of pixels which index into the new palette.
     The palette is returned as the external variables red, green, and
     blue.  The new palette is both coarse and slow to compute.

     The size= keyword can be used to set the palette size.  The default
     is 200 colors, the size of all the distribution palettes.

     The flip=1 keyword can be used to flip the image bottom for top.

     The square=1 limits flag is set; use
        limits,square=0
     to return to the default non-square plot limits.

   SEE ALSO: pnm_read
 */
{
  if (is_void(size)) size= 200;
  image= bytscl(image,top=255);

  r= image(1,,);
  g= image(2,,);
  b= image(3,,);

  /* count cells in 16x16x16 subcubes of full RGB color cube */
  rgb= (long(r>>4) | (g&0xf0) | ((b&0xf0)<<4)) + 1;
  hist= histogram(rgb,top=4096);

  /* the size available colors must be shared among occupied subcubes
     -- allocate them in order of occupation */
  order= map= sort(-hist);
  map(order)= indgen(0:4095);
  c= order-1;
  cr= char(c<<4)+'\10';
  cg= char(c&0xf0)+'\10';
  cb= char((c>>4)&0xf0)+'\10';

  extern red, green, blue;
  red= cr(1:size);
  green= cg(1:size);
  blue= cb(1:size);

  map= char(map);
  pal= long([red,green,blue])(,-,);
  pic= long([cr,cg,cb])(-,,);
  for (i=size+1 ; i<=4096 && hist(order(i)) ; i=k+1) {
    k= min(i+7, 4096);
    map(order(i:k))= ((pal-pic(,i:k,))^2)(,,sum)(mnx,)-1;
  }

  rgb= map(rgb);
  if (flip) rgb= rgb(,::-1);

  fma;
  limits, square=1;
  palette, red, green, blue;
  pli, rgb;

  return rgb;
}

/* ------------------------------------------------------------------------ */

func pnm_write(image, filename, noflip, text=, noscale=, bits=)
/* DOCUMENT pnm_write, image, filename)
         or pnm_write, image, filename, noflip)

     write IMAGE to a PBM, PGM, or PPM file called FILENAME.  If
     NOFLIP is present and non-zero, the image in FILENAME is stored
     in exactly the order of IMAGE in memory.  Otherwise, the height
     dimension is reversed, since images in the files read top to
     bottom, while the Yorick image plotting commands go bottom to
     top.

     If IMAGE is a 3-by-width-by-height array, a PPM will be written.
     Otherwise, IMAGE must be a width-by-height array.  If IMAGE
     contains at most two distinct values, a PBM will be written,
     otherwise a PGM.  You can force a PGM using the bits=8 keyword.

     If IMAGE is of type char, it will be used as is, otherwise it
     will be scaled to the range 0 to 255.  (Note that for a PPM, this
     means the largest single rgb component value sets the scale.)

     If the text= keyword is present and non-zero, a text PNM file
     will be written; the default is to write a binary or raw PNM.

     If the noscale=1 keyword is supplied, the IMAGE will not be
     scaled to the range 0 to 255.  In this case, the IMAGE must
     have an integer data type with minimum value >=0.
     If bits= is not specified, then noscale=1 forces pnm_write to
     guess the value you intended the brightest component value.
     If IMAGE is not color, the guess is bits=1 if there are only
     1 or 2 values, bits=8 if max(IMAGE)<256, and bits=16 otherwise.
     For color IMAGE, the guess is either bits=8 or bits=16.

     Normally, PGM and PPM files have a pixel or color component
     size of 8 bits, so they run from 0 (darkest) to 255 (brightest).
     With bits= you can force a different maximum value.  Use bits=8
     to force a binary IMAGE to be written as a PGM.  The largest
     legal value of bits is 16, the smallest is 1.

   SEE ALSO: pnm_colorize, pnm_read
 */
{
  /* condition input image */
  dims= dimsof(image);
  if (dims(1)==2) {
    width= dims(2);
    height= dims(3);
  } else if (dims(1)==3 && dims(2)==3) {
    width= dims(3);
    height= dims(4);
    color= 1;
  } else {
    error, "image must be w-by-h (bw) or 3-by-w-by-h (color) array";
  }
  guess_bits = is_void(bits);
  if (guess_bits) bits = 8;
  else bits = long(bits);
  if (bits<1 || bits>16) error, "bits= between 1 and 16 inclusive";
  top = 2^bits-1;
  if (noscale) {
    if (structof(image(1)+0)!=long)
      error, "noscale= image must have integer data type";
    else if (min(image)<0)
      error, "noscale= image must be positive everywhere";
  } else {
    if (top > 255)
      error, "autoscaled image must have fewer than 8 bits";
  }
  if (structof(image)!=char) {
    if (!noscale) image = bytscl(image, top=top);
  } else {
    if (top > 255)
      error, "must write char image with no more than than 8 bits";
  }
  maxcol = long(max(image));
  if (!noscale && !color && (guess_bits || top==1)) {
    itest = (image!='\0');
    /* be careful! char is unsigned! */
    if (max(char(int(maxcol)*itest)-image)==0) {
      maxcol = top = 1;
      image = char(itest);
    }
  }
  if (maxcol>top) {
    if (!guess_bits)
      error, "maximum image value exceeds specified number of bits";
    top = 65535;  /* since guess_bits==1, top was 255 here */
    if (maxcol > top)
      error, "maximum legal scaled image value is 65535 in a PNM";
  }
  if (!noflip) image= image(..,::-1);

  if (color) magic= 3;
  else if (top==1) magic= 1;
  else magic= 2;
  if (!text) magic+= 3;
  header= swrite(format="P%ld\n%ld %ld\n%s", magic, width, height,
                 (top>1? print(top)(1)+"\n" : ""));

  if (text) {
    f= create(filename);
    write, f, format="%s", header;
    if (top==1) format= "%1d";
    else format= " %d";
    write, f, linesize=70, format=format, image;
    write, f, "";  /* be sure there is a newline at end */

  } else {
    if (!color && top==1) {
      /* pad width to multiple of 8 pixels */
      img = array(structof(image), ((width+7)/8)*8, height);
      img(1:width,) = image;  /* padding bits are all zero */
      /* pack raw PBM images 8 bits per char */
      img = (img(*) != 0);
      image = char((img(1::8)<<7) | (img(2::8)<<6) | (img(3::8)<<5) |
                   (img(4::8)<<4) | (img(5::8)<<3) | (img(6::8)<<2) |
                   (img(7::8)<<1) | img(8::8));
      img = [];
    } else if (top > 255) {
      if (color) {
        img = char(image)(,-:1:2,,);
        img(,1,,) = char(image>>8);
      } else {
        img = char(image)(-:1:2,,);
        img(1,,) = char(image>>8);
      }
      image = img;   img = [];
    } else if (structof(image) != char) {
      image = char(image);
    }
    f= open(filename, "wb");
    _write, f, 0, (*pointer(header))(1:-1);
    addr= strlen(header);
    _write, f, addr, image;
    remove, filename+"L";  /* get rid of contents log file */
  }
}

/* ------------------------------------------------------------------------ */

func pnm_colorize(image, cmin=, cmax=, top=)
/* DOCUMENT color_image= pnm_colorize(image)

     colorize the width-by-height array IMAGE, returning a 3-by-
     width-by-height array, suitable for use with pnm_write.
     The current graphics palette is used to perform the colorization.
     The keywords cmin=, cmax=, and top= are recognized and have the
     same meaning as for the plf, pli, and bytscl functions.

   SEE ALSO: pnm_write, bytscl
 */
{
  if (structof(image)!=char)
    image= bytscl(image, cmin=cmin, cmax=cmax, top=top);

  local r, g, b;
  palette, query=1, r,g,b;
  rgb= transpose([r,g,b]);

  return rgb(,image+1);
}

/* ------------------------------------------------------------------------ */
