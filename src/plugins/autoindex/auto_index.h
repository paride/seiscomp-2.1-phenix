/*
//----------------------------------------------------------------------------
//  Include :   AUTO_INDEX.H
//  Update  :   12-01-2001
//  By      :   Arie van Wettum, Universiteit Utrecht, Seismologie.
//----------------------------------------------------------------------------
*/

#define		NARS_DIR		"/home/sleeman/capeverde/LIVE/data/CV_logger/"
#define		INDEX_DIR		"/home/sleeman/capeverde/LIVE/data/CV_index/"
#define		DEBUGFILE		"/home/sleeman/capeverde/LIVE/data/CV_index/auto_index.debug"
#define		INDEXFILE		"/home/sleeman/capeverde/LIVE/data/CV_index/indexfile"
#define		SORTFILE		"/home/sleeman/capeverde/LIVE/data/CV_index/sortfile"

#define		DAYSEC			86400
#define		HRSEC			3600
#define		MINSEC			60

struct _indx
{
	int  fnr;
	char fname[30];
	double start;
};

