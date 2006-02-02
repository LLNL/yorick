#!/bin/sh
# $Id: yorapi.sh,v 1.3 2006-02-02 09:17:06 thiebaut Exp $
# MSWindows and AIX both require lists of all symbols declared as PLUG_API
# in order to properly link plugins.

# MSWindows - when plugin built, need API list to resolve symbols
#             defined in the yorick executable
# AIX - when yorick built, need API list to mark symbols which the
#       yorick executable exports

PLAY_DIRS=`grep '^PLAY_DIRS=' Make.cfg | sed -e 's/^PLAY_DIRS=//'`
if test "$PLAY_DIRS" = "win"; then
  WIN=win
else
  WIN=x11
fi

rm -f cfg.* yorapi.def
# header files containing PLUG_API declarations
grep -h PLUG_API >cfg.00 \
 play/play.h play/phash.h play/pstdlib.h play/pstdio.h play/$WIN/playwin.h \
 yorick/ydata.h yorick/binio.h yorick/yio.h yorick/defmem.h yorick/hash.h \
 yorick/bcast.h yorick/parse.h yorick/yapi.h regexp/yfnmatch.h regexp/yregexp.h \
 matrix/cblasy.h matrix/dg.h fft/cfft.h gist/gist.h gist/hlevel.h gist/draw.h \
 gist/engine.h gist/cgm.h gist/ps.h gist/gtext.h gist/xbasic.h gist/xfancy.h

# splitcmd=`echo one,two,three|sed -e 'y/,/\n/'|wc -l`
# in AIX sed, y recognizes \n but s does not
# splitcmd='y/,/\n/'
# in Linux sed, s recognizes \n but y does not
# splitcmd='s/,/\n/g'
# but both recognize escaped newline in 4th line from bottom in this script:

cat >cfg.01 <<EOF
s/;.*/;/
s/[ 	]*([^*].*)/:/
s/([^*][^)]*/:/
s/)://
s/\[.*\]//
s/;//
s/: *,/:/
s/PLUG_API //
s/volatile //
s/const //
s/unsigned //
s/struct //
s/(\*//
s/^[ 	]*[a-zA-Z0-9_]*[ 	]*//
s/\**//g
s/[ 	]*,[ 	]*/,/g
s/,/\\
/g
s/://g
s/[ 	]*//g
EOF

sed -f cfg.01 cfg.00 >yorapi.def
rm -f cfg.*

# original MSWindows script
# dlltool -z preyor.def libyor.a
# sed -e 's/ @ .*//' -e 's/	//' <preyor.def | tail -n +3 >preyor.def1
# sed -e 's/.*/\0 = yorick.exe.\0/' <preyor.def1 >libyor.def
