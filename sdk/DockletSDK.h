//Header file for ObjectDock DockletSDK
#pragma once

#include <Gdiplus.h>
using namespace Gdiplus;
#pragma comment(lib, "gdiplus.lib")

BOOL DockletIsVisible(HWND hwndDocklet);
BOOL DockletGetRect(HWND hwndDocklet, RECT *rcDocklet);
int DockletGetLabel(HWND hwndDocklet, char *szLabel);
void DockletSetLabel(HWND hwndDocklet, char *szLabel);


Bitmap *DockletLoadGDIPlusImage(char *szImage);
void DockletSetImage(HWND hwndDocklet, Image *lpImageNew, BOOL bAutomaticallyDeleteImage = TRUE);
void DockletSetImageFile(HWND hwndDocklet, char *szImage);
void DockletSetImageOverlay(HWND hwndDocklet, Image *imageOverlay, BOOL bAutomaticallyDeleteImage = TRUE);

BOOL DockletBrowseForImage(HWND hwndParent, char *szImage, char *szAlternateRelativeRoot = NULL);

void DockletLockMouseEffect(HWND hwndDocklet, BOOL bLock);
void DockletDoAttentionAnimation(HWND hwndDocklet);

int WritePrivateProfileInt(LPCTSTR lpAppName, LPCTSTR lpKeyName, int iValue, LPCTSTR lpFileName);

void DockletGetRootFolder(HWND hwndDocklet, char *szFolder);
void DockletGetRelativeFolder(HWND hwndDocklet, char *szFolder);

void DockletDefaultConfigDialog(HWND hwndDocklet);

int DockletQueryDockEdge(HWND hwndDocklet);
int DockletQueryDockAlign(HWND hwndDocklet);
BOOL DockletSetDockEdge(HWND hwndDocklet, int iNewEdge);
BOOL DockletSetDockAlign(HWND hwndDocklet, int iNewAlign);