/*
 * $Id: mfcapp.h,v 1.1 2005-09-18 22:05:35 dhmunro Exp $
 * MFC implementation of play, boss and worker thread declarations
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#define VC_EXTRALEAN

#include <afxwin.h>
#include <afxcmn.h>

class mfc_boss : public CWinApp {
public:
  mfc_boss(int (*on_launch)(int, char **));
  virtual BOOL InitInstance();
  virtual int ExitInstance();

  void on_sigint();
  void on_view_term();
  void on_view_hist();
  void on_update_view_term(CCmdUI *ui);
  void on_update_view_hist(CCmdUI *ui);

  afx_msg void on_about();
  DECLARE_MESSAGE_MAP()
};

extern mfc_boss the_boss;
