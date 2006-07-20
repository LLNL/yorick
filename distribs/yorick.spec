Name:       yorick
Version:    2.1
Release:    02
Summary:    interpreted language and scientific graphics
Packager:   David H. Munro <dhmunro@users.sourceforge.net>

Group:      Development/Languages
License:    BSD
URL:        http://yorick.sourceforge.net
Source0:    yorick-%{version}.%{release}.tgz
BuildRoot:  %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
BuildRequires: info

Icon:       yicon48.xpm

# emacs site-lisp directories for fedora core, site-start.d is subdir
%define emacs_site_d  %{_datadir}/emacs/site-lisp
%define xemacs_site_d %{_datadir}/xemacs/site-packages/lisp
%define yordir %{_libdir}/yorick/%{version}

%description
Yorick is an interpreted programming language for:
 * scientific simulations or calculations
 * postprocessing or steering large simulation codes
 * interactive scientific graphics
 * reading, writing, and translating large files of numbers

The language features a compact syntax for many common array
operations, so it processes large arrays of numbers very quickly and
efficiently.  Superficially, yorick code resembles C code, but yorick
variables are never explicitly declared and have a dynamic scoping
similar to many Lisp dialects.  The yorick language is designed to be
typed interactively at a keyboard, as well as stored in files for
later use.

This package includes an emacs-based development environment, which
you can launch by typing M-x yorick in emacs.


%prep
%setup -q

%build
make reloc
CFLAGS="$RPM_OPT_FLAGS" make config
make
make docs
cd doc; make yorick.info; cd ..

%install
rm -rf $RPM_BUILD_ROOT
make install

install -dm 755 $RPM_BUILD_ROOT%{yordir}
mv relocate/* $RPM_BUILD_ROOT%{yordir}/
rmdir relocate
install -dm 755 $RPM_BUILD_ROOT%{_bindir}
ln -s ../..%{yordir}/bin/yorick $RPM_BUILD_ROOT%{_bindir}/yorick
ln -s ../..%{yordir}/bin/gist $RPM_BUILD_ROOT%{_bindir}/gist

install -dm 755 $RPM_BUILD_ROOT%{_datadir}/info
install -pm 644 doc/yorick.info* $RPM_BUILD_ROOT%{_datadir}/info
gzip -9qnf $RPM_BUILD_ROOT%{_datadir}/info/yorick.info*
install -dm 755 $RPM_BUILD_ROOT%{_mandir}/man1
install -pm 644 doc/yorick.1 doc/gist.1 $RPM_BUILD_ROOT%{_mandir}/man1
gzip -9qnf $RPM_BUILD_ROOT%{_mandir}/man1/yorick.1
gzip -9qnf $RPM_BUILD_ROOT%{_mandir}/man1/gist.1
install -dm 755 $RPM_BUILD_ROOT%{yordir}/emacs
install -pm 644 emacs/yorick.el $RPM_BUILD_ROOT%{yordir}/emacs
install -pm 644 emacs/yorick-auto.el $RPM_BUILD_ROOT%{yordir}/emacs
for dir in %{emacs_site_d} %{xemacs_site_d} ; do
  install -dm 755 $RPM_BUILD_ROOT$dir $RPM_BUILD_ROOT$dir/site-start.d
  ln -s %{yordir}/emacs/yorick.el $RPM_BUILD_ROOT$dir
  ln -s %{yordir}/emacs/yorick-auto.el $RPM_BUILD_ROOT$dir/site-start.d
done

%clean
rm -rf $RPM_BUILD_ROOT


%post
if [ "$1" = "1" ] ; then  # first install
 if [ -x /sbin/install-info ]; then
   /sbin/install-info %{_datadir}/info/yorick.info.gz %{_datadir}/info/dir
 fi
fi

%preun
if [ "$1" = "0" ] ; then # last uninstall
 if [ -x /sbin/install-info ]; then
   /sbin/install-info --delete yorick %{_datadir}/info/dir
 fi
fi


%triggerin -- emacs-common
if [ -d %{emacs_site_d} ] ; then
  ln -sf %{yordir}/emacs/yorick.el %{emacs_site_d}
  ln -sf %{yordir}/emacs/yorick-auto.el %{emacs_site_d}/site-start.d
fi

%triggerin -- xemacs-common
if [ -d %{xemacs_site_d} ] ; then
  ln -sf %{yordir}/emacs/yorick.el %{xemacs_site_d}
  ln -sf %{yordir}/emacs/yorick-auto.el %{xemacs_site_d}/site-start.d
fi

%triggerun -- emacs-common
if [ "$2" = "0" ] ; then
  rm -f %{emacs_site_d}/yorick.el*
  rm -f %{emacs_site_d}/site-start.d/yorick-auto.el*
fi

%triggerun -- xemacs-common
if [ "$2" = "0" ] ; then
  rm -f %{xemacs_site_d}/yorick.el*
  rm -f %{xemacs_site_d}/site-start.d/yorick-auto.el*
fi


%files
%defattr(-,root,root,-)
%doc LICENSE README
%docdir %{yordir}/doc
%doc %{_mandir}/man1/yorick.1.gz
%doc %{_mandir}/man1/gist.1.gz
%doc %{_datadir}/info/yorick*
%{yordir}/
%{_bindir}/yorick
%{_bindir}/gist
%ghost %{_datadir}/*emacs

%changelog
* Wed Jul 19 2006 David H. Munro <dave@dogberry.localdomain> - 2.1-02
- update to fedora core style

* Sat Mar 12 2005  <munro1@llnl.gov> 1.6-02
- 1.6.02 changed plugin support, package Makefile system, see NEWS

* Fri Jul 30 2004  <munro1@llnl.gov> 1.6-01
- 1.6.01 added plugin support, fixed bugs, see NEWS

* Wed Oct  1 2003  <munro1@llnl.gov> 1.5-14
- 1.5.14 numerous bug fixes, see NEWS

* Thu Oct 31 2002  <munro1@llnl.gov>
- 1.5.12 fixes double alignment for big speedup on i86

* Mon Jul  1 2002  <munro@star.llnl.gov>
- 1.5.10 has numerous bug fixes since 08, see NEWS

* Sat Feb  2 2002  <munro@star.llnl.gov>
- 1.5.08 is 07 infant mortality replacement, see NEWS

* Tue Jan 22 2002  <munro@star.llnl.gov>
- numerous 1.5.07 bug fixes, see NEWS file

* Wed Nov 14 2001  <munro@star.llnl.gov>
- 1.5-06 bug fixes, see NEWS file

* Fri Oct 19 2001  David H. Munro <munro1@llnl.gov>
- numerous bug fixes relative to 1.5-01, 1.5-02 releases, see NEWS file

