/*
 * $Id: color.i,v 1.1 2005-09-18 22:05:54 dhmunro Exp $
 * Hue-Saturation-Value color representation routines and other
 * color and palette construction tools.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

local palette_directory;
/* DOCUMENT palette_directory= "~/Gist/"
     holds the default directory for the dump_palette command.
     The directory name *must* end with "/"; the default is shown.
   SEE ALSO: dump_palette
 */
palette_directory= "~/Gist/";

func dump_palette(name)
/* DOCUMENT dump_palette, name
     dump the current palette under the NAME.  If NAME contains no
     slash characters, the palette_directory will be prepended to the
     name.  The name can be fed back to the palette command in order
     to reload the cumped palette.
   SEE ALSO: brighten, palette, palette_directory
 */
{
  if (!strmatch(name,"/")) name= palette_directory+name;
  local r, g, b;
  palette, query=1, r, g, b;
  f= create(name);
  write,f, format="%s\n", "ncolors= "+pr1(numberof(r));
  write,f, format="%s\n", "ntsc= 1";
  write,f, format="%s\n", "#  r   g   b";
  write,f, format="%4d%4d%4d\n", r, g, b;
}

func brighten(factor)
/* DOCUMENT brighten, factor
         or brighten
     brighten the current palette by the specified FACTOR.
     The FACTOR is the slope of the transfer function for the color value
     (see to_hsv for a description of the hsv color system); a value of
     1.0 always remains 1.0, but values near 0.0 change by FACTOR.
     FACTOR= 1.0 is a no-op.  The default factor is 4.0.
   SEE ALSO: dump_palette
 */
{
  if (is_void(factor)) factor= 4.0;
  local r, g, b;
  palette, query=1, r, g, b;
  hsv= to_hsv([r,g,b]);
  v= hsv(,3);
  /* this function is a symmetric parabolic mapping from [0,1] to [0,1] */
  fv= 0.5*(factor-1.0)*v;
  n= (2.*factor-fv)*v;
  d= 1.+fv+sqrt(max(1.+(factor^2-1.)*v,0.));
  hsv(,3)= n/d;
  /* here is an alternative which has the property that applying the
     function twice is the same as applying with the product of the
     two factors - however, this nice property is spoiled by the
     quantization of the byte scaling of the rgb values */
  /* hsv(,3)= 1.-(1.-v)^factor; */
  rgb= to_rgb(hsv);
  palette, rgb(,1),rgb(,2),rgb(,3);
}

func to_rgb(hsv)
/* DOCUMENT rgb= to_rgb(hsv)
         or rgb= to_rgb([h,s,v])
     return the RGB representation of the n-by-3 array of HSV colors
     rgb: red, green, blue from 0 to 255
     hsv: h= hue in degrees, red=0, green=120, blue=240
          s= saturation from 0 (gray) to 1 (full hue)
          v= value from 0 (black) to 1 (full intensity)
          s= 1 - min(r,g,b)/max(r,g,b)
          v= max(r,g,b)/255
   SEE ALSO: to_hsv
 */
{
  hsv= double(hsv);
  h= hsv(*,1);
  s= hsv(*,2);
  v= hsv(*,3);

  /* normalize hue to lie in 0<=h<360 */
  h= h % 360.0;
  h+= (h<0.0)*360.0;

  /* divide hue into 60 degree sectors */
  i= long(h/60.0);
  f= h/60.0 - i;

  p= 1.0 - s;
  q= 1.0 - s*f;
  t= 1.0 - s*(1.-f);
  /* each hue sector will be represented by rgb values taken
   * from one of v, p, q, or t */
  r= ((i==0|i==5) + (i==2|i==3)*p + (i==1)*q + (i==4)*t) * v;
  g= ((i==1|i==2) + (i==4|i==5)*p + (i==3)*q + (i==0)*t) * v;
  b= ((i==3|i==4) + (i==0|i==1)*p + (i==5)*q + (i==2)*t) * v;

  /* return array same shape as input */
  rgb= hsv;
  rgb(*,1)= r;
  rgb(*,2)= g;
  rgb(*,3)= b;
  return bytscl(rgb,top=255,cmin=0.0,cmax=1.0);
}

func to_hsv(rgb)
/* DOCUMENT hsv= to_hsv(rgb)
         or hsv= to_hsv([r,g,b])
     return the HSV representation of the n-by-3 array of RGB colors
     rgb: red, green, blue from 0 to 255
     hsv: h= hue in degrees, red=0, green=120, blue=240
          s= saturation from 0 (gray) to 1 (full hue)
          v= value from 0 (black) to 1 (full intensity)
          s= 1 - min(r,g,b)/max(r,g,b)
          v= max(r,g,b)
   SEE ALSO: to_rgb
 */
{
  rgb/= 255.0;
  hsv= rgb;
  rgb= rgb(*,);
  r= rgb(,1);
  g= rgb(,2);
  b= rgb(,3);

  /* compute and normalize hue angle */
  h= atan((g-b)*sqrt(0.75), r-0.5*(g+b)+1.e-30)/pi * 180.;
  h+= (h<0.0)*360.;

  /* any given hue is adjacent to one primary, opposite a second primary,
   * and neutral for the third
   * value is adjacent, which is always maximum
   * the ratio of opposite to adjacent is 1-saturation */
  v= max(r,g,b);
  s= 1.0 - (min(r,g,b)+1.e-30)/(v+1.e-30);

  hsv(*,1)= h;
  hsv(*,2)= s;
  hsv(*,3)= v;
  return hsv;
}
