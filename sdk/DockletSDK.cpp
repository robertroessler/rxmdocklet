#include <windows.h>
#include "DockletSDK.h"


BOOL DockletIsVisible(HWND hwndDocklet)
{
	typedef BOOL(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet);
	DUMMY_TYPEDEF HostDockletIsVisible = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletIsVisible");
	if(!HostDockletIsVisible)
		return FALSE;

	return HostDockletIsVisible(hwndDocklet);
}

int DockletGetLabel(HWND hwndDocklet, char *szLabel)
{
	typedef int(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, char *szLabel);
	DUMMY_TYPEDEF HostDockletGetLabel = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletGetLabel");
	if(!HostDockletGetLabel)
		return 0;

	return HostDockletGetLabel(hwndDocklet, szLabel);
}


void DockletSetLabel(HWND hwndDocklet, char *szLabel)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, char *szLabel);
	DUMMY_TYPEDEF HostDockletSetLabel = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletSetLabel");
	if(!HostDockletSetLabel)
		return;

	HostDockletSetLabel(hwndDocklet, szLabel);
}


void DockletSetImageOverlay(HWND hwndDocklet, Image *imageOverlay, BOOL bAutomaticallyDeleteImage)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, Image *imageOverlay, BOOL bAutomaticallyDeleteImage);
	DUMMY_TYPEDEF HostDockletSetImageOverlay = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletSetImageOverlay");
	if(!HostDockletSetImageOverlay)
		return;

	HostDockletSetImageOverlay(hwndDocklet, imageOverlay, bAutomaticallyDeleteImage);
}

void DockletSetImageFile(HWND hwndDocklet, char *szImage)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, char *szImage);
	DUMMY_TYPEDEF HostDockletSetImageFile = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletSetImageFile");
	if(!HostDockletSetImageFile)
		return;

	HostDockletSetImageFile(hwndDocklet, szImage);
}

BOOL DockletBrowseForImage(HWND hwndParent, char *szImage, char *szAlternateRelativeRoot)
{
	typedef BOOL(__stdcall *DUMMY_TYPEDEF)(HWND hwndParent, char *szImage, char *szAlternateRelativeRoot);
	DUMMY_TYPEDEF HostDockletBrowseForImage = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletBrowseForImage");
	if(!HostDockletBrowseForImage)
		return FALSE;

	return HostDockletBrowseForImage(hwndParent, szImage, szAlternateRelativeRoot);
}

Bitmap *DockletLoadGDIPlusImage(char *szImage)
{
	typedef Bitmap *(__stdcall *DUMMY_TYPEDEF)(char *szImage);
	DUMMY_TYPEDEF HostDockletLoadGDIPlusImage = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletLoadGDIPlusImage");
	if(!HostDockletLoadGDIPlusImage)
		return NULL;

	return HostDockletLoadGDIPlusImage(szImage);
}

void DockletSetImage(HWND hwndDocklet, Image *lpImageNew, BOOL bAutomaticallyDeleteImage)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, Image *lpImageNew, BOOL bAutomaticallyDeleteImage);
	DUMMY_TYPEDEF HostDockletSetImage = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletSetImage");
	if(!HostDockletSetImage)
		return;

	HostDockletSetImage(hwndDocklet, lpImageNew, bAutomaticallyDeleteImage);
}

void DockletLockMouseEffect(HWND hwndDocklet, BOOL bLock)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, BOOL bLock);
	DUMMY_TYPEDEF HostDockletLockMouseEffect = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletLockMouseEffect");
	if(!HostDockletLockMouseEffect)
		return;

	HostDockletLockMouseEffect(hwndDocklet, bLock);
}

void DockletDoAttentionAnimation(HWND hwndDocklet)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet);
	DUMMY_TYPEDEF HostDockletDoAttentionAnimation = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletDoAttentionAnimation");
	if(!HostDockletDoAttentionAnimation)
		return;

	HostDockletDoAttentionAnimation(hwndDocklet);
}


BOOL DockletGetRect(HWND hwndDocklet, RECT *rcDocklet)
{
	typedef BOOL(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, RECT *rcDocklet);
	DUMMY_TYPEDEF HostDockletGetRect = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletGetRect");
	if(!HostDockletGetRect)
		return FALSE;

	return HostDockletGetRect(hwndDocklet, rcDocklet);
}

void DockletGetRootFolder(HWND hwndDocklet, char *szRootFolder)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, char *szRootFolder);
	DUMMY_TYPEDEF HostDockletGetRootFolder = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletGetRootFolder");
	if(!HostDockletGetRootFolder)
		return;

	HostDockletGetRootFolder(hwndDocklet, szRootFolder);
}

void DockletGetRelativeFolder(HWND hwndDocklet, char *szFolder)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, char *szFolder);
	DUMMY_TYPEDEF HostDockletGetRelativeFolder = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletGetRelativeFolder");
	if(!HostDockletGetRelativeFolder)
		return;

	HostDockletGetRelativeFolder(hwndDocklet, szFolder);
}

void DockletDefaultConfigDialog(HWND hwndDocklet)
{
	typedef void(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet);
	DUMMY_TYPEDEF HostDockletDefaultConfigDialog = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletDefaultConfigDialog");
	if(!HostDockletDefaultConfigDialog)
		return;

	HostDockletDefaultConfigDialog(hwndDocklet);
}


int DockletQueryDockEdge(HWND hwndDocklet)
{
	typedef int(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet);
	DUMMY_TYPEDEF HostDockletQueryDockEdge = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletQueryDockEdge");
	if(!HostDockletQueryDockEdge)
		return -1;

	return HostDockletQueryDockEdge(hwndDocklet);
}

int DockletQueryDockAlign(HWND hwndDocklet)
{
	typedef int(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet);
	DUMMY_TYPEDEF HostDockletQueryDockAlign = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletQueryDockAlign");
	if(!HostDockletQueryDockAlign)
		return -1;

	return HostDockletQueryDockAlign(hwndDocklet);
}

BOOL DockletSetDockEdge(HWND hwndDocklet, int iNewEdge)
{
	typedef BOOL(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, int iNewEdge);
	DUMMY_TYPEDEF HostDockletSetDockEdge = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletSetDockEdge");
	if(!HostDockletSetDockEdge)
		return FALSE;

	return HostDockletSetDockEdge(hwndDocklet, iNewEdge);
}

BOOL DockletSetDockAlign(HWND hwndDocklet, int iNewAlign)
{
	typedef BOOL(__stdcall *DUMMY_TYPEDEF)(HWND hwndDocklet, int iNewAlign);
	DUMMY_TYPEDEF HostDockletSetDockAlign = (DUMMY_TYPEDEF) GetProcAddress(GetModuleHandle(NULL), "DockletSetDockAlign");
	if(!HostDockletSetDockAlign)
		return FALSE;

	return HostDockletSetDockAlign(hwndDocklet, iNewAlign);
}

static inline LPSTR  itoX(int i, LPSTR  sz, int r) { return _itoa(i, sz, r); }

static inline LPWSTR itoX(int i, LPWSTR sz, int r) { return _itow(i, sz, r); }

int WritePrivateProfileInt(LPCTSTR lpAppName, LPCTSTR lpKeyName, int iValue, LPCTSTR lpFileName)
{
   //////////////////////////////////////////////////////////////////////////
   ///Helper function included to quickly & easily save integers to an Ini
	TCHAR szNumber[24];
	return WritePrivateProfileString(lpAppName, lpKeyName, itoX(iValue, szNumber, 10), lpFileName);
}

