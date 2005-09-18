/*
 * $Id: style.i,v 1.1 2005-09-18 22:06:10 dhmunro Exp $
 * Get and set graphics styles from within the Yorick interpreter.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

func get_style(&landscape, &systems, &legends, &clegends)
/* DOCUMENT get_style, landscape, systems, legends, clegends

     get the detailed style of the current drawing.  The arguments
     are all outputs:

     landscape: 1 if drawing is landscape orientation, 0 if portrait
     system:    an array of GfakeSystem struct instances, one per
                coordinate system in this drawing (ordinarily just one).
     legends:   a GeLegendBox structure instance describing the layout
                of the plot legends
     clegends:  a GeLegendBox structure instance describing the layout
                of the contour legends

     See the help for the GeLegendBox and GpTextAttribs structs for
     the details of the legends and clegends arguments.  Basically,
     you can adjust the location of the legends on the page, the
     font and height of the characters used to render legends, and
     whether the legends are split into two columns.

     The coordinate systems are the systems accessible via the
     plsys command.  The index of the system in the system array is
     the index you use to switch to it in the plsys command.  Simple
     styles have only one coordinate system, and you should carefully
     consider whether you should design a graphic style with multiple
     coordinate systems -- most likely, you can do a better job by
     combining several separate Yorick pictures with some sort of
     page layout program, rather than trying to do this work within
     Yorick itself.

     See the help for the GfakeSystem struct for complete details of
     what you can adjust.  The most interesting features you can
     control are the location and aspect ratio of the viewport, and
     the details of the axis ticks and labels.  The gridxy function
     provides a simpler interface for fiddling with ticks and labels
     if that is all you need.  The system.viewport member is the
     [xmin,xmax,ymin,ymax] of the rectangle on the page where your
     plots will appear, expressed in NDC coordinates (0.0013 NDC units
     equals one point, and there are 72.27 points per inch, and 2.54
     cm per inch; the NDC origin is always at the lower left hand
     corner of the paper, with x increasing leftward and y increasing
     upward).  If you change the size of the viewport, you will also
     need to change the parameters of the tick-generating model; like
     other problems in typography and page layout, this is harder
     than you might think.

   SEE ALSO: set_style, read_style, write_style
 */
{
  landscape= [0n];
  n= raw_style(0, landscape, &[], &[]);
  if (!n) error, "no current drawing";
  systems= array(GfakeSystem, n);
  legends= array(GeLegendBox, 2);
  raw_style, 0, landscape, &systems, &legends;
  landscape= landscape(1);
  clegends= legends(2);
  legends= legends(1);
}

func set_style(landscape, systems, legends, clegends)
/* DOCUMENT set_style, landscape, systems, legends, clegends

     set the detailed style of the current drawing.  The arguments
     are all inputs, having the same meanings as for get_style (which
     see).  All arguments are required, so you may need to call
     get_style as a starting point, if you only want to make a few
     changes.  See the Y_SITE/g/work.gs and the other .gs files for
     examples of reasonable values to choose.

     Calling set_style destroys anything that was plotted in the
     window, like the style= keyword of the window command.

   SEE ALSO: get_style, read_style, write_style
 */
{
  if (structof(systems)!=GfakeSystem || structof(legends)!=GeLegendBox ||
      structof(clegends)!=GeLegendBox || numberof(legends)!=1 ||
      numberof(clegends)!=1 || numberof(landscape)!=1 ||
      structof(landscape+0)!=long)
    error, "illegal input data types or sizes";
  landscape= [int(landscape(1))];
  raw_style, numberof(systems),  landscape, &systems, &[legends,clegends];
}

/* ------------------------------------------------------------------------ */

func write_style(file, landscape, systems, legends, clegends)
/* DOCUMENT write_style, file, landscape, systems, legends, clegends

     write a Gist style sheet (.gs file), using the data structures
     as described in the get_style function.  The FILE can be a
     filename or a text file stream.

   SEE ALSO: get_style, set_style, read_style
 */
{
  if (structof(file)==string) file= create(file);

  write,file,format="# %s\n",
    "Gist style sheet made by Yorick write_style function";
  write,file,format="# Created: %s\n", timestamp();
  write,file,format="# (%ld coordinate systems)\n\n", numberof(systems);

  write,file,format="landscape= %d\n\n", (landscape!=0);

  for (i=1 ; i<=numberof(systems) ; ++i) {
    sys= systems(i);
    if (i==1) {
      default= sys;
      which= "default";
    } else {
      which= "system";
    }
    legend= sys.legend;
    if (!strlen(legend)) legend= "0";
    else legend= "\""+legend+"\"";
    write,file,format="%s= { legend= %s", which, (i>1? legend : "0");
    final= " }";
    vp= sys.viewport;
    if (i==1 || anyof(vp!=default.viewport)) {
      final= "}";
      write,file,format=",\n  viewport= { %f, %f, %f, %f }",
        vp(1),vp(2),vp(3),vp(4);
    }
    ticks= sys.ticks;
    if (i==1 || ticks!=default.ticks) {
      final= "}";
      write,file,format=",\n%s\n", "  ticks= {"
      axes= [ticks.horiz, ticks.vert];
      daxes= [default.ticks.horiz, default.ticks.vert];
      prefix= "\n    ";
      for (j=1 ; j<=2 ; ++j) {
        axis= axes(j);
        daxis= daxes(j);
        if (i==1 || axis!=daxis) {
          write,file,format="%s%s= {\n", prefix, (j==1? "horiz" : "vert");
          nitems= 0;
          prefix= "      ";  suffix= "";
          if (i==1 || axis.nMajor!=daxis.nMajor) {
            write,file,format="%snMajor= %f", prefix, axis.nMajor;
            prefix= ",  ";  suffix= " ";
            ++nitems;
          }
          if (i==1 || axis.nMinor!=daxis.nMinor) {
            write,file,format="%snMinor= %f", prefix, axis.nMinor;
            prefix= ",  ";  suffix= " ";
            ++nitems;
          }
          if (i==1 || axis.logAdjMajor!=daxis.logAdjMajor) {
            write,file,format="%slogAdjMajor= %f", prefix, axis.logAdjMajor;
            prefix= ",  ";  suffix= " ";
            if ((++nitems)==3) {
              nitems= 0;
              prefix= ",\n      ";
            }
          }
          if (i==1 || axis.logAdjMinor!=daxis.logAdjMinor) {
            write,file,format="%slogAdjMinor= %f", prefix, axis.logAdjMinor;
            prefix= ",  ";  suffix= " ";
            if ((++nitems)==3) {
              nitems= 0;
              prefix= ",\n      ";
            }
          }
          if (i==1 || axis.nDigits!=daxis.nDigits) {
            write,file,format="%snDigits= %d", prefix, axis.nDigits;
            prefix= ",  ";  suffix= " ";
            if ((++nitems)==3) {
              nitems= 0;
              prefix= ",\n      ";
            }
          }
          if (i==1 || axis.gridLevel!=daxis.gridLevel) {
            write,file,format="%sgridLevel= %d", prefix, axis.gridLevel;
            prefix= ",  ";  suffix= " ";
            if ((++nitems)==3) {
              nitems= 0;
              prefix= ",\n      ";
            }
          }
          if (i==1 || axis.flags!=daxis.flags) {
            write,file,format="%sflags= 0x%03x", prefix, axis.flags;
            prefix= ",  ";  suffix= " ";
            if ((++nitems)==3) {
              nitems= 0;
              prefix= ",\n      ";
            }
          }
          if (i==1 || axis.tickOff!=daxis.tickOff) {
            write,file,format="%stickOff= %f", prefix, axis.tickOff;
            prefix= ",  ";  suffix= " ";
            if ((++nitems)==3) {
              nitems= 0;
              prefix= ",\n      ";
            }
          }
          if (i==1 || axis.labelOff!=daxis.labelOff) {
            write,file,format="%slabelOff= %f", prefix, axis.labelOff;
            prefix= ",  ";  suffix= " ";
            if ((++nitems)==3) {
              nitems= 0;
              prefix= ",\n      ";
            }
          }
          if (i==1 || anyof(axis.tickLen!=daxis.tickLen)) {
            if (prefix!="      ") prefix= ",\n      ";
            _style_wvect, file,  prefix, "tickLen", axis.tickLen;
            prefix= ",\n      ";  suffix= "";  nitems= 0;
          }
          if (i==1 || axis.tickStyle!=daxis.tickStyle) {
            if (prefix!="      ") prefix= ",\n      ";
            _style_wline, file,  prefix, "tickStyle", axis.tickStyle;
            prefix= ",\n      ";  suffix= "";  nitems= 0;
          }
          if (i==1 || axis.gridStyle!=daxis.gridStyle) {
            if (prefix!="      ") prefix= ",\n      ";
            _style_wline, file,  prefix, "gridStyle", axis.gridStyle;
            prefix= ",\n      ";  suffix= "";  nitems= 0;
          }
          if (i==1 || axis.textStyle!=daxis.textStyle) {
            if (prefix!="      ") prefix= ",\n      ";
            _style_wtext, file,  prefix, "textStyle", axis.textStyle;
            prefix= ",\n      ";  suffix= "";  nitems= 0;
          }
          if (i==1 || axis.xOver!=daxis.xOver) {
            write,file,format="%sxOver= %f", prefix, axis.xOver;
            prefix= ",  ";  suffix= " ";
            if ((++nitems)==3) prefix= ",\n      ";
          }
          if (i==1 || axis.yOver!=daxis.yOver) {
            write,file,format="%syOver= %f", prefix, axis.yOver;
            suffix= " ";
          }
          write,file,format="%s}", suffix;
          prefix= ",\n\n    ";  suffix= "";
        }
      }
      if (i==1 || ticks.frame!=default.ticks.frame) {
        write,file,format="%sframe= %d", prefix, ticks.frame;
        prefix= ",\n    ";  suffix= " ";
      }
      if (i==1 || ticks.frameStyle!=default.ticks.frameStyle) {
        _style_wline, file,  prefix, "frameStyle", ticks.frameStyle;
        suffix= "";
      }
      write,file,format="%s}", suffix;
    }
    write,file,format="%s\n", final;

    if (i==1) {
      write,file,format="\nsystem= { legend= %s }\n", legend;
    }
  }

  legs= [legends,clegends];
  for (i=1 ; i<=2 ; ++i) {
    leg= legs(i);
    if (leg.nlines) {
      write,file,format="\n%slegends= {\n", (i==1? "" : "c");
      write,file,format="  x= %f, y= %f, dx= %f, dy= %f",
        leg.x, leg.y, leg.dx, leg.dy;
      _style_wtext, file, ",\n  ", "textStyle", leg.textStyle;
      write,file,format=",\n  nchars= %d, nlines= %d, nwrap= %d }\n",
        leg.nchars, leg.nlines, leg.nwrap;
    } else {
      write,file,format="\n%slegends= { nlines= 0 }\n", (i==1? "" : "c");
    }
  }

  return file;
}

func _style_wvect(file, prefix, member, value)
{
  write,file,format="%s%s= {", prefix, member;
  prefix= " ";
  for (i=1 ; i<=numberof(value) ; ++i) {
    write,file,format="%s%f", prefix, value(i);
    prefix= ", ";
  }
  write,file,format="%s}", " ";
}

func _style_wline(file, prefix, member, style)
{
  write,file,format="%s%s= { color= %d, type= %d, width= %f }",
    prefix, member, style.color, style.type, style.width;
}

func _style_wtext(file, prefix, member, style)
{
  write,file,format="%s%s= { color= %d, font= 0x%02x, height= %f",
    prefix, member, style.color, style.font, style.height;
  if (strpart(prefix,1:1)!=",") {
    if (strpart(prefix,1:1)!="\n") prefix= ",\n"+prefix;
    else prefix= ","+prefix;
  }
  write,file,format="%s  orient= %d, alignH= %d, alignV= %d, opaque= %d }",
    prefix, style.orient, style.alignH, style.alignV, style.opaque;
}

func read_style(file, &landscape, &systems, &legends, &clegends)
/* DOCUMENT read_style, file, landscape, systems, legends, clegends

     read a Gist style sheet (.gs file), and return the data
     structures as described in the get_style function.  The FILE
     can be a filename or a text file stream.

   SEE ALSO: get_style, set_style, write_style
 */
{
  if (structof(file)==string) {
    f= open(file, "r", 1);
    if (!f) {
      /* maybe the file is in one of the standard locations */
      f= open("~/gist/"+file, "r", 1);
      if (!f) {
        f= open("~/Gist/"+file, "r", 1);
        if (!f) {
          f= open(Y_SITE+"g/"+file, "r", 1);
          if (!f) error, "missing style file: "+file;
        }
      }
    }
  } else {
    f= file;
  }

  /* set up default values (same as work.gs) */
  landscape= 0n;
  default_line= GpLineAttribs(color= 254,  type= 1,  width= 1.0);
  default_text= GpTextAttribs(
    color= 254,  font= 0x08,  height= 0.0182,
    orient= 0,  alignH= 0,  alignV= 0,  opaque= 0);
  default_ltxt= GpTextAttribs(
    color= 254,  font= 0x00,  height= 0.0156,
    orient= 0,  alignH= 1,  alignV= 1,  opaque= 0);
  default= GfakeSystem(viewport=[0.19, 0.60, 0.44, 0.85],
    ticks= GaTickStyle(
      horiz= GaAxisStyle(
        nMajor=7.5, nMinor=50.0, logAdjMajor=1.2, logAdjMinor=1.2,
        nDigits=3, gridLevel=1, flags=0x033, tickOff=0.0007, labelOff=0.0182,
        tickLen= [0.0143, 0.0091, 0.0052, 0.0026, 0.0013],
        tickStyle= default_line,
        gridStyle= GpLineAttribs(color= 254,  type= 3,  width= 1.0),
        textStyle= default_text,
        xOver= 0.395,  yOver= 0.370),
      vert= GaAxisStyle(
        nMajor=7.5, nMinor=50.0, logAdjMajor=1.2, logAdjMinor=1.2,
        nDigits=3, gridLevel=1, flags=0x033, tickOff=0.0007, labelOff=0.0182,
        tickLen= [0.0143, 0.0091, 0.0052, 0.0026, 0.0013],
        tickStyle= default_line,
        gridStyle= GpLineAttribs(color= 254,  type= 3,  width= 1.0),
        textStyle= default_text,
        xOver= 0.150,  yOver= 0.370),
      frame= 0,
      frameStyle= GpLineAttribs(color= 254,  type= 1,  width= 1.0)));
  default_legb= GeLegendBox(
    x= 0.04698,  y= 0.360,  dx= 0.3758,  dy= 0.0,
    textStyle= default_ltxt, nchars= 36,  nlines= 20,  nwrap= 2);
  default_clegb= GeLegendBox(
    x= 0.6182,  y= 0.8643,  dx= 0.0,  dy= 0.0,
    textStyle= default_ltxt, nchars= 14,  nlines= 28,  nwrap= 1);
  legends= default_legb;
  clegends= default_clegb;

  /* parse the file */
  systems= [];
  type= [];
  line= "";
  for (;;) {
    s= _style_token(f, line, type);
    if (!line) break;
    if (type!=3) _style_goof, f, line, s;
    if (s=="landscape") {
      landscape= _style_token(f, line, type);
      if (type!=2 || structof(landscape)!=long)
        _style_goof, f, line, "landscape= ????";
      landscape= (landscape!=0);
    } else if (s=="default") {
      default= _style_system(f, line, default);
    } else if (s=="system") {
      grow, systems, [_style_system(f, line, default)];
    } else if (s=="legends") {
      legends= _style_legends(f, line, default_legb);
    } else if (s=="clegends") {
      clegends= _style_legends(f, line, default_clegb);
    } else {
      _style_goof, f, line, s;
    }
  }

  if (is_void(systems)) systems= [default];
}

func _style_system(f, &line, default)
{
  system= default;
  type= [];
  s= _style_token(f, line, type);
  if (s!="{") _style_goof, f, line, s;
  for (;;) {
    s= _style_token(f, line, type);
    if (type!=3) _style_goof, f, line, s;
    if (s=="legend") {
      s= _style_token(f, line, type);
      if (type!=1) {
        if (type==2 && s==0) s= string(0);
        else _style_goof, f, line, s;
      }
      system.legend= s;
    } else if (s=="viewport") {
      system.viewport= _style_vector(f,line);
    } else if (s=="ticks") {
      system.ticks= _style_ticks(f,line,system.ticks);
    } else {
      _style_goof, f, line, s;
    }
    s= _style_token(f, line, type);
    if (s=="}") break;
    if (s!=",") _style_goof, f, line, s;
  }
  return system;
}

func _style_legends(f, &line, default)
{
  legend= default;
  type= [];
  s= _style_token(f, line, type);
  if (s!="{") _style_goof, f, line, s;
  for (;;) {
    s= _style_token(f, line, type);
    if (type!=3) _style_goof, f, line, s;
    if (s=="textStyle")
      get_member(legend,s)= _style_text(f,line,default.textStyle);
    else
      get_member(legend,s)= _style_token(f,line,type);
    s= _style_token(f, line, type);
    if (s=="}") break;
    if (s!=",") _style_goof, f, line, s;
  }
  return legend;
}

func _style_ticks(f, &line, default)
{
  ticks= default;
  type= [];
  s= _style_token(f, line, type);
  if (s!="{") _style_goof, f, line, s;
  for (;;) {
    s= _style_token(f, line, type);
    if (type!=3) _style_goof, f, line, s;
    if (s=="horiz" || s=="vert") {
      get_member(ticks,s)= _style_axis(f,line,get_member(ticks,s));
    } else if (s=="frame") {
      ticks.frame= _style_token(f,line,type);
    } else if (s=="frameStyle") {
      ticks.frameStyle= _style_line(f,line,ticks.frameStyle);
    } else {
      _style_goof, f, line, s;
    }
    s= _style_token(f, line, type);
    if (s=="}") break;
    if (s!=",") _style_goof, f, line, s;
  }
  return ticks;
}

func _style_axis(f, &line, default)
{
  axis= default;
  type= [];
  s= _style_token(f, line, type);
  if (s!="{") _style_goof, f, line, s;
  for (;;) {
    s= _style_token(f, line, type);
    if (type!=3) _style_goof, f, line, s;
    if (s=="tickLen") {
      value= _style_vector(f, line);
      axis.tickLen(1:numberof(value))= value;
    } else if (s=="tickStyle" || s=="gridStyle") {
      get_member(axis,s)= _style_line(f,line,get_member(axis,s));
    } else if (s=="textStyle") {
      axis.textStyle= _style_text(f,line,axis.textStyle);
    } else {
      get_member(axis,s)= _style_token(f,line,type);
    }
    s= _style_token(f, line, type);
    if (s=="}") break;
    if (s!=",") _style_goof, f, line, s;
  }
  return axis;
}

func _style_vector(f, &line)
{
  vector= type= [];
  s= _style_token(f, line, type);
  if (s!="{") _style_goof, f, line, s;
  for (;;) {
    s= _style_token(f, line, type);
    if (type!=2) _style_goof, f, line, s;
    grow, vector, [double(s)];
    s= _style_token(f, line, type);
    if (s=="}") break;
    if (s!=",") _style_goof, f, line, s;
  }
  return vector;
}

func _style_text(f, &line, default)
{
  junk= [0.0];
  text= default;
  type= [];
  s= _style_token(f, line, type);
  if (s!="{") _style_goof, f, line, s;
  for (;;) {
    s= _style_token(f, line, type);
    if (type!=3) _style_goof, f, line, s;
    if (text=="path") text= "orient";
    if (noneof(text==["expand","spacing","upX","upY"]))
      get_member(text,s)= _style_token(f,line,type);
    else
      junk(1)= _style_token(f,line,type);
    s= _style_token(f, line, type);
    if (s=="}") break;
    if (s!=",") _style_goof, f, line, s;
  }
  if (text.color < 0) text.color += 256;
  return text;
}

func _style_line(f, &line, default)
{
  style= default;
  type= [];
  s= _style_token(f, line, type);
  if (s!="{") _style_goof, f, line, s;
  for (;;) {
    s= _style_token(f, line, type);
    if (type!=3) _style_goof, f, line, s;
    get_member(style,s)= _style_token(f,line,type);
    s= _style_token(f, line, type);
    if (s=="}") break;
    if (s!=",") _style_goof, f, line, s;
  }
  if (style.color < 0) style.color += 256;
  return style;
}

/* retrieve next token from file, updating current line
   -- set line to "" initially
   -- on output, type= 0 is delimiter, 1 is quoted, 2 is number, 3 keyword
 */
func _style_token(f, &line, &type, norecurse)
{
  s= "";
  for (;;) {
    /* give up if no more lines in file */
    if (!line) return line;
    /* remove leading whitespace */
    sread, line, s, format="%[ \t]";
    line= strpart(line, 1+strlen(s):0);
    /* stop if line has more chars, and is not a comment */
    if (strlen(line)) {
      s= strpart(line,1:1);
      if (s!="#") break;
    }
    /* otherwise get the next line and try again */
    line= rdline(f);
  }

  /* look at first character to see if this is a delimiter */
  cs= (*pointer(s))(1);
  if (anyof(['{','}',',','=']==cs)) {
    /* token is one of the four valid delimiters */
    line= strpart(line,2:0);
    type= 0;
    return string(&[cs,'\0']);
  } else if (cs=='"') {
    /* token is a quoted string */
    s= strtok(line, "\"");
    type= 1;
    line= s(2);
    return s(1);
  }

  /* token must be either a number or a keyword */
  s= strtok(line, " \t{},=")(1);
  line= strpart(line, strlen(s)+1:0);
  if ((cs>='0' && cs<='9') || cs=='.' || cs=='-') {
    /* token is a number */
    if (strmatch(s,".") || strmatch(s,"e",1)) {
      value= 0.0;
      if (!sread(s,value)) _style_goof, f, line, s;
    } else {
      value= 0;
      if (!sread(s,value,format="%i")) _style_goof, f, line, s;
    }
    type= 2;
    return value;
  } else {
    /* if next token is =, this token is a keyword */
    if (!norecurse) next= _style_token(f, line, type, 1);
    if (type==0 && next=="=") {
      type= 3;
      return s;
    } else {
      _style_goof, f, line, s;
    }
  }
}

func _style_goof(f, line, s)
{
  write, "graphics style file format error at or just before:";
  write, print(f)(2);
  error, "unrecognized token: "+pr1(s);
}

/* ------------------------------------------------------------------------ */
/* The following structure definitions must match those in
   Gist/gist.h and Gist/draw.h */

/* Note: NDC units 0.0013 equals one point equals 1/72.27 inch */
one_point= 0.0013;
one_inch= 72.27*one_point;

struct GpLineAttribs {
  long color;     /* 255=bg 254=fg 253=b 252=w ...=rgb ...=cmy */
  int type;       /* line types: 0=none 1=solid 2=- 3=. 4=-. 5=-..  */
  double width;   /* 1.0 is normal width of a line (1/2 point) */
}

struct GpTextAttribs {
  long color;       /* 255=bg 254=fg 253=b 252=w ...=rgb ...=cmy */
  int font;         /* text font
                       fonts: 0=courier 4=times 8=helvetica 12=symbol
                             16=newcentury
                              or with 1 for bold, 2 for italic */
  double height;    /* character height in NDC, default 0.0156 (12pt)
                       UNLIKE GKS, GIST font sizes are always specified
                       in NDC.  This drastically simplifies coding for
                       devices like X windows, which must load a font
                       at each size.  It also conforms better with
                       a Mac-like user interface in which font size
                       in points is selected by the user.  */
  int orient;          /* text orientation: 0=right 1=left 2=up 3=down  */
  int alignH, alignV;  /* text alignments:
                          alignH: 0=normal 1=left 2=center 3=right
                          alignV: 0=normal 1=top 2=cap 3=half 4=base 5=bot */

  int opaque;
}

struct GaAxisStyle {
  double nMajor, nMinor, logAdjMajor, logAdjMinor;
  int nDigits, gridLevel;
  int flags;    /* 0x001 ticks on lower edge
                   0x002 ticks on upper edge
                   0x004 ticks in center
                   0x008 inward ticks
                   0x010 outward ticks
                   0x020 labels on lower edge
                   0x040 labels on upper edge
                   0x080 full grid lines
                   0x100 origin grid line   */

  double tickOff, labelOff;  /* offsets in NDC from the edge of the
                                viewport to the ticks or labels */
  double tickLen(5);         /* tick lengths in NDC */

  GpLineAttribs tickStyle, gridStyle;
  GpTextAttribs textStyle;   /* alignment ignored, set correctly */
  double xOver, yOver;       /* position for overflow label */
}

struct GaTickStyle {
  GaAxisStyle horiz, vert;
  int frame;
  GpLineAttribs frameStyle;
}

struct GeLegendBox {
  double x, y;              /* NDC location of this legend box */
  double dx, dy;            /* if non-zero, offset to 2nd column */
  GpTextAttribs textStyle;  /* font, size, etc. of these legends */
  int nchars, nlines;       /* max number of characters per line, lines */
  int nwrap;                /* max number of lines to wrap long legends */
}

struct GfakeSystem {
  double viewport(4);    /* [xmin,xmax,ymin,ymax] in NDC coordinates */
  GaTickStyle ticks;     /* tick style for this coordinate system */
  string legend;         /* e.g.- "System 0" or "System 1" */
}

/* ------------------------------------------------------------------------ */
