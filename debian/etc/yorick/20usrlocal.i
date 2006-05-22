/*
  /etc/yorick/20usrlocal.i

  The purpose of this file is to add a local yorick tree under /usr/local for
  e.g. add-on packages installed using the pkg_mngr and locally maintained
  packages. All paths (.i, .so, .gp/.gs) are taken into account.

 */
require, "pathfun.i";
add_y_home,"/usr/local/lib/yorick/";
