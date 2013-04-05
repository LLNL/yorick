/* torgb.i
 * W3C HTML and X11 color names.
 */
/* Copyright (c) 2013, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func torgb(name, x11=, hex=)
/* DOCUMENT rgb = torgb(name)
 *          name = torgb(rgb)
 *   returns [r,g,b] corresponding to color NAME.  If NAME is an array of
 *   strings, return value will be 3-by-dimsof(NAME).  The color names are
 *   the colors defined in the W3C standard for SVG and CSS level 3, which
 *   are supported by nearly all Web browsers.  With four exceptions, they
 *   match the X11 colors contained in rgb.txt: Gray, Green, Maroon, and
 *   Purple are substantially darker in W3C than in X11.  W3C also defines
 *   the colors Lime and Silver, which are not in the X11 rgb.txt (although
 *   Lime in W3C is the same color as Green in X11).  You can specify the
 *   x11=1 keyword to get the X11 values for the four disputed defintions.
 *   You can also specify hex=1 to return rgb packed as 0xrrggbb into a
 *   single value of type long.  There are 140 color names (but only 138
 *   unique colors, see quirk 2 below).
 *
 *   The name lookup is case insensitive, and you may spell any color
 *   containing "gray" as "grey".
 *
 *   The torgb function also performs approximate "inverse lookup" if you
 *   specify rgb (either as [r,g,b] or as 0xrrggbb), returning the nearest
 *   named color in CIE LUV space.
 *
 *   See http://en.wikipedia.org/wiki/X11_color_names for color patches;
 *   http://www.centerkey.com/colors/ is a nice selection tool.
 *
 *   Quirks:
 *   1. W3C's DarkGray is significantly lighter than it's Gray, because
 *      DarkGray matches the X11 color while Gray does not.
 *   2. Aqua is identical to Cyan and Fuchsia is identical to Magenta.
 *   3. The following 16 colors are the original pre-HTML 4 colors,
 *      also the oldest VGA/CGA colors: White Silver Gray Black Red Maroon
 *      Yellow Olive Lime Green Aqua Teal Blue Navy Fuchsia Purple
 *
 * SEE ALSO: color, cmap
 */
{
  rgb = w3c_rgb;
  if (x11) rgb((w3c_name == x11_name(-,))(mxx,)) = x11_rgb;
  rgb = rgb(-:1:3,);
  rgb = (rgb >> [16,8,0]) & 0xff;
  d = dimsof(name);
  if (structof(name) == string) {
    /* ordinary forward lookup */
    name = strcase(0, name);
    name = streplace(name, strfind("grey", name), "gray");
    mask = (strcase(0,w3c_name) == name(-,..));
    if (anyof(!mask(max,..)))
      error, "unknown color name "+name(where(!mask(max,..))(1));
    rgb = rgb(,mask(mxx,..));
    return hex? (rgb(1,..)<<16)|(rgb(2,..)<<8)|rgb(3,..) : char(rgb);

  } else {
    /* inverse lookup, find closest named color */
    if (!d(1) || d(2)!=3) {
      name = (name(-:1:3,..) >> [16,8,0]) & 0xff;
      d = dimsof(name);
    }
    luv = transpose(rgb2luv(transpose(rgb)));
    if (d(1) > 1) name = transpose(rgb2luv(transpose(name, 2)), 0)(,-,..);
    else name = rgb2luv(name);
    return w3c_name(((name - luv)^2)(sum,..)(mnx,..));
  }
}

w3c_name =
  ["AliceBlue","AntiqueWhite","Aqua","Aquamarine","Azure","Beige","Bisque",
   "Black","BlanchedAlmond","Blue","BlueViolet","Brown","Burlywood","CadetBlue",
   "Chartreuse","Chocolate","Coral","Cornflower","Cornsilk","Crimson","Cyan",
   "DarkBlue","DarkCyan","DarkGoldenrod","DarkGray","DarkGreen","DarkKhaki",
   "DarkMagenta","DarkOliveGreen","DarkOrange","DarkOrchid","DarkRed",
   "DarkSalmon","DarkSeaGreen","DarkSlateBlue","DarkSlateGray","DarkTurquoise",
   "DarkViolet","DeepPink","DeepSkyBlue","DimGray","DodgerBlue","Firebrick",
   "FloralWhite","ForestGreen","Fuchsia","Gainsboro","GhostWhite","Gold",
   "Goldenrod","Gray","Green","GreenYellow","Honeydew","HotPink","IndianRed",
   "Indigo","Ivory","Khaki","Lavender","LavenderBlush","LawnGreen",
   "LemonChiffon","LightBlue","LightCoral","LightCyan","LightGoldenrod",
   "LightGray","LightGreen","LightPink","LightSalmon","LightSeaGreen",
   "LightSkyBlue","LightSlateGray","LightSteelBlue","LightYellow","Lime",
   "LimeGreen","Linen","Magenta","Maroon","MediumAquamarine","MediumBlue",
   "MediumOrchid","MediumPurple","MediumSeaGreen","MediumSlateBlue",
   "MediumSpringGreen","MediumTurquoise","MediumVioletRed","MidnightBlue",
   "MintCream","MistyRose","Moccasin","NavajoWhite","Navy","OldLace","Olive",
   "OliveDrab","Orange","OrangeRed","Orchid","PaleGoldenrod","PaleGreen",
   "PaleTurquoise","PaleVioletRed","PapayaWhip","PeachPuff","Peru","Pink",
   "Plum","PowderBlue","Purple","Red","RosyBrown","RoyalBlue","SaddleBrown",
   "Salmon","SandyBrown","SeaGreen","Seashell","Sienna","Silver","SkyBlue",
   "SlateBlue","SlateGray","Snow","SpringGreen","SteelBlue","Tan","Teal",
   "Thistle","Tomato","Turquoise","Violet","Wheat","White","WhiteSmoke",
   "Yellow","YellowGreen"];
w3c_rgb =
  [0xf0f8ff,0xfaebd7,0x00ffff,0x7fffd4,0xf0ffff,0xf5f5dc,0xffe4c4,0x000000,
   0xffebcd,0x0000ff,0x8a2be2,0xa52a2a,0xdeb887,0x5f9ea0,0x7fff00,0xd2691e,
   0xff7f50,0x6495ed,0xfff8dc,0xdc143c,0x00ffff,0x00008b,0x008b8b,0xb8860b,
   0xa9a9a9,0x006400,0xbdb76b,0x8b008b,0x556b2f,0xff8c00,0x9932cc,0x8b0000,
   0xe9967a,0x8fbc8f,0x483d8b,0x2f4f4f,0x00ced1,0x9400d3,0xff1493,0x00bfff,
   0x696969,0x1e90ff,0xb22222,0xfffaf0,0x228b22,0xff00ff,0xdcdcdc,0xf8f8ff,
   0xffd700,0xdaa520,0x808080,0x008000,0xadff2f,0xf0fff0,0xff69b4,0xcd5c5c,
   0x4b0082,0xfffff0,0xf0e68c,0xe6e6fa,0xfff0f5,0x7cfc00,0xfffacd,0xadd8e6,
   0xf08080,0xe0ffff,0xfafad2,0xd3d3d3,0x90ee90,0xffb6c1,0xffa07a,0x20b2aa,
   0x87cefa,0x778899,0xb0c4de,0xffffe0,0x00ff00,0x32cd32,0xfaf0e6,0xff00ff,
   0x7f0000,0x66cdaa,0x0000cd,0xba55d3,0x9370db,0x3cb371,0x7b68ee,0x00fa9a,
   0x48d1cc,0xc71585,0x191970,0xf5fffa,0xffe4e1,0xffe4b5,0xffdead,0x000080,
   0xfdf5e6,0x808000,0x6b8e23,0xffa500,0xff4500,0xda70d6,0xeee8aa,0x98fb98,
   0xafeeee,0xdb7093,0xffefd5,0xffdab9,0xcd853f,0xffc0cb,0xdda0dd,0xb0e0e6,
   0x7f007f,0xff0000,0xbc8f8f,0x4169e1,0x8b4513,0xfa8072,0xf4a460,0x2e8b57,
   0xfff5ee,0xa0522d,0xc0c0c0,0x87ceeb,0x6a5acd,0x708090,0xfffafa,0x00ff7f,
   0x4682b4,0xd2b48c,0x008080,0xd8bfd8,0xff6347,0x40e0d0,0xee82ee,0xf5deb3,
   0xffffff,0xf5f5f5,0xffff00,0x9acd32];

x11_name = ["Gray", "Green", "Maroon", "Purple"];
x11_rgb = [0xbebebe, 0x00ff00, 0xb03060, 0xa020f0];
