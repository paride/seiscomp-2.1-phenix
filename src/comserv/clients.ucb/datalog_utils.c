/**************************************************************************/
/*  datalog_utils - utility functions for datalog program.                */
/*  Douglas Neuhauser, UC Berkeley Seismographic Station                  */
/*      Copyright (c) 1995 The Regents of the University of California.   */
/*                                                                        */
/*  modified 2000/02/21 by Ronny Kopischke, ronny@kopischke.de            */
/*                      - every file will be opened once for writing now  */
/*                      - active files with false header will be closed   */
/*                        and renamed after starting the datalog          */
/*                                                                        */
/*  modified 2000/05/17 by andres.heinloo@acm.org                         */
/*                      - closing and renaming of files merged into       */
/*                        store_seed()                                    */
/*                                                                        */
/**************************************************************************/

#ifndef lint
static char sccsid[] = "%W% %G% %U%";
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <termio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/stat.h>
#include <math.h>
#include <fnmatch.h>

off_t lseek (int fd, off_t offset, int whence);

#include "qlib2.h"

#include "dpstruc.h"
#include "seedstrc.h"
#include "stuff.h"
#include "timeutil.h"
#include "service.h"
#include "cfgutil.h"

#include "datalog.h"

#define MAX_BLKSIZE	4096
#define	MIN_SEED_BLKSIZE 256
#define	DEFAULT_BLKSIZE	512
#define VOLLEN		(8 + 3+4+4+2+5+2+3+23+23+1+1+2)
#define BEGTIME_OFFSET	(8 + 3+4+4+2+5+2+3)
#define VOL_TIMELEN	23
#define ENDTIME_OFFSET (BEGTIME_OFFSET+VOL_TIMELEN)
#define NULL_STR	"(null)"
#define TIME_FMT	1
#define	boolean_value(str)  ((str[0] == 'y' || str[0] == 'Y') ? 1 : 0)

#ifdef __cplusplus
#define this _this
#endif

typedef struct _sel {		/* Structure for storing selectors.	*/
    int nselectors;		/* Number of selectors.			*/
    seltype *selectors;		/* Ptr to selector array.		*/
} SEL;

typedef struct _filter {
    int flag;			/* 0 = ignore all, 1 = use selector list*/
    SEL sel;			/* Ptr to selector list.		*/
} FILTER;

static FILTER save [NUMQ];	/* save filtering info for each type.	*/

/************************************************************************/
/*  Function declarations.						*/
/************************************************************************/
void append_durlist (DURLIST *head, char *channel, char *duration);
void boolean_mask (short *ps, short bit_mask, char *str);
char *build_filename(FINFO *fip);
char *channel_duration (DURLIST *head, char *channel);
int close_file(FINFO *fip);
char *datadir_path (FINFO *fip);
char *datafile_path (FINFO *fip);
char *date_string(INT_TIME time, char *dflt_str);
int decode_val (char *str, int len);
void dump_durlist (DURLIST *head);
void dump_data_hdr(DATA_HDR *hdr);
void dump_seed(unsigned char *str);
EXT_TIME file_time_limit(EXT_TIME file_time, char *duration);
FINFO *find_finfo (DATA_HDR *hdr);
int make_dir(char *dirname, int mode);
int open_file(FINFO *fip);
void parse_cfg (char *str1, char *str2);
int read_vol(FINFO *fip);
int sel_match (char *location, char *channel, SEL *psel);
int selector_match (char *str1, char *str2);
void set_selectors (pclient_struc me);
int start_new_file (FINFO *fip, EXT_TIME ext_begtime);
int store_seed (seed_record_header *pseed);
void store_selectors(int type, SEL *psel, char *str);
void update_durlist (char *str, int nhead);
void verify_cfg();
int write_vol(FINFO *fip, int blksize);
int reduce_reclen (FINFO *fip, DATA_HDR *hdr, seed_record_header *pseed);
int terminate_program (int error);

/************************************************************************/
/*  Externals required in multiple files.				*/
/************************************************************************/
extern DURLIST durhead[5];	/* data, detection, calib, timing, log	*/
extern char *extension[5];	/* data, detection, calib, timing, log	*/
extern char pidfile[1024];	/* pid file.				*/
extern short data_mask;		/* data mask for cs_setup.		*/
extern char lockfile[160];	/* Name of optional lock file.		*/
extern int verbosity;		/* verbosity setting.			*/

static int trimreclen;		/* flag to reduce record length.	*/
static FINFO *fhead;
static char station_dir[256];
static char filename_fmt[256];
static seed_record_header *pseedblock;
static SEL sel[NUMQ];

/************************************************************************/
/*  store_seed:								*/
/*	Store SEED packet in the appropriate file.			*/
/************************************************************************/
int store_seed (seed_record_header *pseed)
{
    FINFO *fip;
    DATA_HDR *hdr;
    EXT_TIME begtime, endtime;
    int blksize = DEFAULT_BLKSIZE;
    char vol[MAX_BLKSIZE+1];
    char vol_str[MAX_BLKSIZE+1];
    char vol_fmt[24];
    char span_str[48];
    int status;
    int lrl;
    off_t pos;
    char filename[1024];
    int loop;

    pseedblock = pseed;
    if (verbosity & 256) dump_seed((unsigned char *)pseedblock);

    hdr = decode_hdr_sdr((SDR_HDR *)pseed, MAX_BLKSIZE);
    /* For some reason, decode_hdr_sdr does not trim all of the strings	*/
    /* in the hdr.  We will do it here, so that comparisons are always	*/
    /* done on trimmed strings.						*/
    trim (hdr->station_id);
    trim (hdr->channel_id);
    trim (hdr->location_id);
    trim (hdr->network_id);
    if (hdr == NULL) {
	fprintf (stderr, "Unable to decode hdr\n");
	terminate_program (1);
    }

    status = 0;
    for (loop=0;loop<1;loop++) {
	begtime = int_to_ext(hdr->begtime);
	if ((fip = find_finfo(hdr)) == NULL) break;
	if (trimreclen) reduce_reclen (fip, hdr, pseed);

	/* Open current file if no file is open.  Read volume hdr (if any).	*/
	if (fip->fd < 0) {
	    if (!open_file(fip)) {
		terminate_program (1);
	    }
	    fip->begtime = begtime;
	    if ( ! (read_vol(fip) || write_vol(fip,hdr->blksize))) {
		fprintf (stderr, "Error initializing VOL hdr in %s\n", 
			 datafile_path(fip));
	    }
	    /* Compute file limit based on file begtime and duration.		*/
	    fip->limit = file_time_limit(fip->begtime, fip->duration);
	}

	/* Ensure blksize of file and blksize of data match.  */
	if (fip->blksize == 0) fip->blksize = hdr->blksize;
	if (fip->blksize != hdr->blksize) {
	    fprintf (stderr, "Error - data blksize = %d, file blksize = %d\n",
		     hdr->blksize, fip->blksize);
	    terminate_program (1);
	}

	/* Close and rename file if it is time for a new file. */
	if (start_new_file(fip,begtime) ||
		strcmp(fip->station,hdr->station_id)!=0 ||
		strcmp(fip->network,hdr->network_id)!=0 ||
		strcmp(fip->location,hdr->location_id)!=0) {
	    if (! close_file(fip)) {
		fprintf (stderr, "Error closing/renaming file for %s %s %s %s\n",
			 fip->station, fip->location, fip->channel, fip->network);
		terminate_program (1);
	    }
	    strcpy(fip->station, hdr->station_id);
	    strcpy(fip->network, hdr->network_id);
	    strcpy(fip->location, hdr->location_id);
	}

	/* Open current file if no file is open.  Read volume hdr (if any).	*/
	if (fip->fd < 0) {
	    if (!open_file(fip)) {
		terminate_program (1);
	    }
	    fip->begtime = begtime;
	    if ( ! (read_vol(fip) || write_vol(fip,hdr->blksize))) {
		fprintf (stderr, "Error initializing VOL hdr in %s\n", 
			 datafile_path(fip));
	    }
	    /* Compute file limit based on file begtime and duration.		*/
	    fip->limit = file_time_limit(fip->begtime, fip->duration);
	}

	/* Append new block to the end of the file. */
	while ((pos = lseek(fip->fd,0,SEEK_END)) < 0 && errno == EINTR) ;
	if (pos < 0) {
	    fprintf (stderr, "Error seeking EOF in %s\n", datafile_path(fip));
	    break;
	}
	if (xwrite(fip->fd,(char *)pseed,hdr->blksize) != hdr->blksize) {
	    fprintf (stderr, "Error appending to %s\n", datafile_path(fip));
	    break;
	}

	/* Update volume header with new endtime. */
	fip->endtime = int_to_ext(hdr->endtime);
	sprintf(span_str,"%04d,%03d,%02d:%02d:%02d.%04d~%04d,%03d,%02d:%02d:%02d.%04d~",
		fip->begtime.year, fip->begtime.doy, fip->begtime.hour,
		fip->begtime.minute, fip->begtime.second, 
		fip->begtime.usec/USECS_PER_TICK,
		fip->endtime.year, fip->endtime.doy, fip->endtime.hour,
		fip->endtime.minute, fip->endtime.second, 
		fip->endtime.usec/USECS_PER_TICK);
	while ((pos = lseek(fip->fd,BEGTIME_OFFSET,SEEK_SET)) < 0 && 
	       errno == EINTR) ;
	if (pos < 0) {
	    fprintf (stderr, "Error seeking BOF for %s\n", datafile_path(fip));
	    break;
	}
	if (xwrite(fip->fd,span_str,strlen(span_str)) != strlen(span_str)) {
	    fprintf (stderr, "Error updating volhdr in %s\n", datafile_path(fip));
	    break;
	}
	status = 1;
    }
    if (hdr) free_data_hdr(hdr);
    return(status);
}

/************************************************************************/
/* find_finfo -								*/
/*	Return FINFO structure appropriate for this record.		*/
/************************************************************************/
FINFO *find_finfo (DATA_HDR *hdr) 
{
    FINFO *fip;
    int status;
    BS *bs = hdr->pblockettes;
    int itype = DAT_INDEX;
    int b1000 = 0;
    int b2000 = 0;
    int n = -1;

    /* Determine the type of packet based on the blockette types.   */
    /* Assume it is a data packet unless proven otherwise.	    */
    /* Check packets in this order:				    */
    /* 2xx => events		*/
    /* 3xx => calibration	*/
    /* 5xx => timing		*/
    /* sample rate = 0 => log	*/
    while (bs != (BS *)NULL) {
	n = bs->type;
	if (n >= 200 && n < 300) {
	    itype = DET_INDEX;
	    break;
	}
	else if (n >= 300 && n <= 400) {
	    itype = CAL_INDEX;
	    break;
	}
	else if (n >= 500 && n < 600) {
	    itype = CLK_INDEX;
	    break;
	}
	else if (n == 1000) {
	    /* Note that we found a blockette 1000, but keep scanning.	*/
	    b1000 = 1;
	}
	else if (n == 2000) {
	    /* Note that we found a blockette 2000, but keep scanning.	*/
	    b2000 = 1;
	}
	else {
	    /* Unknown or unimportant blockette - skip it */
	}
	bs = bs->next;
    }
    /* LOG channel is any channel still identified as a data channel	*/
    /* but with a sample rate of 0 and non-zero sample count.		*/
    if (itype == DAT_INDEX && hdr->sample_rate == 0 && hdr->num_samples != 0) {
	itype = LOG_INDEX;
    }
    if (itype == DAT_INDEX && hdr->sample_rate == 0 && hdr->num_samples == 0 && b2000) {
	itype = BLK_INDEX;
    }
    /* EVERY data packet should have a blockette 1000.			*/
    if (itype == DAT_INDEX && b1000 == 0) {
	fprintf (stderr, "Unknown blockette/packet type %d for %s %s %s %s\n", n,
		 hdr->station_id, hdr->location_id, 
		 hdr->channel_id, hdr->network_id);
	/*::
	dump_data_hdr(hdr);
	::*/
	return(NULL);
    }
    /* Ignore empty end-of-detection packets.	    */
    if (itype == DAT_INDEX && hdr->sample_rate == 0 && hdr->num_samples == 0)
	return (NULL);

    /* Check the filter to see if this channel and type should be saved	*/
    /* or discarded.							*/
    if (save[itype].flag == FALSE) return (NULL);
    if (itype <= CAL_INDEX && 
	! sel_match (hdr->location_id, hdr->channel_id, &save[itype].sel))
	return (NULL);

    /* Find FINFO ptr for this channel and type. */
    for (fip = fhead; fip != NULL; fip = fip->next) {
    /*								*/
    /* because the datalog opens more and more files if the	*/
    /* station id is changed and files with the old station	*/
    /* id exist, here are a version without the checking	*/
    /* of the station id 					*/
    /*  							*/ 
    /* changed by Ronny Kopischke			    	*/
    /*								*/
    /* 2000/05/17 [AH] closing and renaming of files merged	*/
    /*                 into store_seed()			*/
    /*								*/
    /*	status = (strcmp(fip->station,hdr->station_id)==0 &&	*/ 
    /*		  strcmp(fip->channel,hdr->channel_id)==0 &&	*/
    /*		  strcmp(fip->network,hdr->network_id)==0 &&	*/
    /*		  strcmp(fip->location,hdr->location_id)==0 &&	*/
    /*		  fip->itype == itype);				*/
	status = (strcmp(fip->channel,hdr->channel_id)==0 &&
		  strcmp(fip->location,hdr->location_id)==0 &&
		  fip->itype == itype);
	if (status) break;
    }

    /* If none found, create one. */
    if (fip == NULL) {
	if ((fip = (FINFO *)malloc(sizeof(FINFO))) == NULL) {
	    fprintf (stderr, "Unable to malloc FINFO for %s %s %s %s\n",
		     hdr->station_id, hdr->location_id,
		     hdr->channel_id, hdr->network_id);
	    terminate_program (1);
	}
	memset ((void *)fip, 0, sizeof(FINFO));
	strcpy(fip->station, hdr->station_id);
	strcpy(fip->channel, hdr->channel_id);
	strcpy(fip->network, hdr->network_id);
	strcpy(fip->location, hdr->location_id);
	memset((void *)&fip->begtime, 0, sizeof(fip->begtime));
	memset((void *)&fip->endtime, 0, sizeof(fip->endtime));
	strcpy(fip->filename, "active");
	strcpy(fip->duration, channel_duration(&durhead[itype],fip->channel));
	fip->itype = itype;
	fip->fd = -1;
	sprintf(fip->ch_dir, "%s%s%s.%s", hdr->location_id, *hdr->location_id ? ".": "", hdr->channel_id, extension[fip->itype]);
	fip->next = fhead;
	fhead = fip;
    }
    return (fip);
}

/************************************************************************/
/*  make_dir:								*/
/*	Create the specified directory.	Return status from mkdir.	*/
/************************************************************************/
int make_dir(char *dirname, int mode)
{
    int status;
    while ((status=mkdir(dirname,mode))==-1 && errno == EINTR) ;
    return (status);
}

/************************************************************************/
/*  file_time_limit:							*/
/*	Compute the closing time for the file from begtime and duration.*/
/************************************************************************/
EXT_TIME file_time_limit(EXT_TIME file_time, char *duration)
{
    INT_TIME t0, t1;
    t1 = ext_to_int(file_time);
    /* Round down to the beginning of the day. */
    file_time.usec = file_time.second = file_time.minute = file_time.hour = 0;
    t0 = ext_to_int(file_time);
    while (tdiff(t1,t0)>0.) {
	t0 = end_of_span(t0,duration);
    }
    return (int_to_ext(t0));
}

/************************************************************************/
/*  start_new_file:							*/
/*	Decide whether it is time to start a new file.			*/
/*	Return 1 if it is time, 0 if not.				*/
/************************************************************************/
int start_new_file (FINFO *fip, EXT_TIME ext_begtime)
{
    int status;
    INT_TIME begtime, fbegtime, limittime;
    begtime = ext_to_int(ext_begtime);
    fbegtime = ext_to_int(fip->begtime);
    limittime = ext_to_int(fip->limit);
    status =  (tdiff(begtime,limittime) >= 0) || (tdiff(fbegtime,begtime) > 0);
    return(status);
}

/************************************************************************/
/*  datadir_path:							*/
/*	Return ptr to static pathname of directory for data file.	*/
/************************************************************************/
char *datadir_path (FINFO *fip)
{
    static char filename[1024];
    sprintf (filename,"%s/%s",station_dir,fip->ch_dir);
    return (filename);
}

/************************************************************************/
/*  datafile_path:							*/
/*	Return ptr to static pathname of data file.			*/
/************************************************************************/
char *datafile_path (FINFO *fip)
{
    static char filename[1024];
    sprintf (filename,"%s/%s",datadir_path(fip),fip->filename);
    return (filename);
}

/************************************************************************/
/*  open_file:								*/
/*	Create channel directory (if necessary) and open current file	*/
/*	for specified channel.	Return 0 on error, 1 on success.	*/
/************************************************************************/
int open_file(FINFO *fip)
{
    char *dirname, *filename;
    /* Make channel directory in case it doesn't exist. */
    dirname = datadir_path(fip);
    if (make_dir(dirname,0777) < 0 && errno != EEXIST) {
	fprintf (stderr, "Error %d creating channel directory %s\n", 
		 errno, dirname);
	return(0);
    }
    /* Open the file. */
    filename = datafile_path(fip);
    while ((fip->fd = open(filename, O_RDWR | O_CREAT, 0666)) == -1
	   && errno == EINTR) ;
    if (fip->fd < 0) {
	fprintf (stderr, "Unable to open file %s for %s %s %s %s\n",
		 filename, fip->station, fip->location, 
		 fip->channel, fip->network);
	return(0);
    }
    return(1);
}

/************************************************************************/
/*  read_vol:								*/
/*	Read and parse volume header for current file, and fill in	*/
/*	FINFO structure.  Return 0 on error, 1 on success.		*/
/************************************************************************/
int read_vol(FINFO *fip)
{
    char vol[512], tmpstr[512];
    off_t pos;
    int n, nscan;
    int lrl;
    char *p;
    INT_TIME *p_it;
    
    if (fip->fd < 0) return(0);
    /* Look for an existing volume header.  If none, create one. */
    while ((pos = lseek(fip->fd,0,SEEK_SET)) < 0 && errno == EINTR) ;
    if (pos < 0) {
	fprintf (stderr, "Error seeking BOF for %s\n", datafile_path(fip));
	return(0);
    }
    while ((n = read(fip->fd,vol,VOLLEN)) < 0 && errno == EINTR) ;
    switch (n) {
      case VOLLEN:
	vol[VOLLEN] = '\0';
	nscan = sscanf (&vol[19],"%02d",&lrl);
	strncpy (fip->station,&vol[21],5);
	strncpy (fip->location,&vol[26],2);
	strncpy (fip->channel,&vol[28],3);

	/* Parse volume start time.					*/
	p = &vol[31];
	charvncpy (tmpstr, p, 22, 0);
	p += strlen(tmpstr) + 1;
	if ((p_it = parse_date (tmpstr)) == NULL) {
	    fprintf (stderr, "Error reading start time on volume hdr in file %s\n",
		     datafile_path(fip));
	    return(0);
	}
	fip->begtime = int_to_ext(*p_it);
	/* Parse volume end time.					*/
	charvncpy (tmpstr, p, 22, 0);
	p += strlen(tmpstr) + 1;
	if ((p_it = parse_date (tmpstr)) == NULL) {
	    fprintf (stderr, "Error reading endtime on volume hdr in file %s\n",
		     datafile_path(fip));
	    return(0);
	}
	fip->endtime = int_to_ext(*p_it);
	/* Skip over next 2 variable fields.				*/
	charvncpy (tmpstr, p, 22, 0);
	p += strlen(tmpstr) + 1;
	charvncpy (tmpstr, p, 22, 0);
	p += strlen(tmpstr) + 1;
	/* Pick up network id.						*/
	strncpy (fip->network,p,2);
	/* Trim the strings for future comparision with parsed hdrs.	*/
	trim(fip->station);
	trim(fip->channel);
	trim(fip->location);
	trim(fip->network);
	return(1);
	break;
      default:	break;
    }
    return (0);
}

/************************************************************************/
/*  write_vol:								*/
/*	Construct and write volume header to the specified file.	*/
/*	Return 0 on error, 1 on success.				*/
/************************************************************************/
int write_vol(FINFO *fip, int blksize)
{
    int lrl;
    char vol[MAX_BLKSIZE+1];

    lrl = (int) log2((double)blksize);
    memset (vol,' ',blksize);
    sprintf(vol,"%06d%c%c%03d%04d%4.1f%02d%-5.5s%-2.2s%-3.3s%04d,%03d,%02d:%02d:%02d.%04d~%04d,%03d,%02d:%02d:%02d.%04d~~~%-2.2s",
	    1, 'V', ' ', 8, VOLLEN-8, 2.3, lrl,
	    fip->station, fip->location, fip->channel, 
	    fip->begtime.year, fip->begtime.doy, fip->begtime.hour,
	    fip->begtime.minute, fip->begtime.second, 
	    fip->begtime.usec/USECS_PER_TICK,
	    fip->endtime.year, fip->endtime.doy, fip->endtime.hour,
	    fip->endtime.minute, fip->endtime.second, 
	    fip->endtime.usec/USECS_PER_TICK,
	    fip->network);
    vol[strlen(vol)] = ' ';
    if (xwrite(fip->fd,vol,blksize) != blksize) {
	fprintf (stderr, "Error writing volhdr in %s\n", 
		 datafile_path(fip));
	return(0);
    }
    return(1);
}

/************************************************************************/
/*  close_file:								*/
/*	Close current file and rename it based on naming template.	*/
/*	Return 0 on error, 1 on success.				*/
/************************************************************************/
int close_file(FINFO *fip) 
{
    char *oldname, newname[1024];
    int status;
    struct stat sb;

    close(fip->fd);
    fip->fd = -1;
    oldname = datafile_path(fip);
    strcpy(newname,build_filename(fip));
    status = stat(newname,&sb);
    if (status == 0) {
	char basename[1024];
	int seq = 1;
	strcpy(basename,newname);
	while (sprintf(newname,"%s.%d",basename,seq), 
	       (status=stat(newname,&sb)) == 0) ++seq;
    }
    if (errno != ENOENT) {
	fprintf (stderr, "Error %d stating file %s\n", errno, newname);
	return(0);
    }
    status = rename(oldname,newname);
    if (status!=0) {
	fprintf (stderr, "Error %d renaming %s to %s\n", errno,
		 oldname, newname);
	return(0);
    }
    return(1);
}

/************************************************************************/
/*  channel_duration:							*/
/*	Return duration for specified channel.				*/
/************************************************************************/
char *channel_duration (DURLIST *head, char *channel)
{
    DURLIST *prev = head->next;

    /* Traverse the durlist looking for a match or EOL.	*/
    while (prev != NULL) {
	if (fnmatch(prev->channel,channel,0)==0) return (prev->duration);
	prev = prev->next;
    }
    return (head->duration);
}

/************************************************************************/
/*  append_durlist:							*/
/*	Append a durlist to the end of the list.			*/
/************************************************************************/
void append_durlist (DURLIST *head, char *channel, char *duration)
{
    DURLIST *sp;
    DURLIST *prev;

    prev = head->next;
    if ((int)strlen(channel) > 3) {
	fprintf (info, "Invalid channel: %s\n", channel);
	terminate_program (1);
    }
    if ((int)strlen(duration) > 7) {
	fprintf (info, "Invalid channel: %s\n", channel);
	terminate_program (1);
    }
    /* Default value goes in the head element.			*/
    if (strcasecmp(channel,"???")==0) {
	strcpy(head->channel,channel);
	strcpy(head->duration, duration);
	return;
    }
    /* Traverse the durlist looking for a match or EOL.	*/
    while (prev && prev->next != NULL) {
	if (strcasecmp(channel,prev->channel)==0) break;
	prev = prev->next;
    }
    /* Update durlist entry if match is found.			*/
    if (prev && strcasecmp(channel,prev->channel)==0) {
	strcpy(prev->duration, duration);
	return;
    }
    if ((sp = (DURLIST *)malloc(sizeof(DURLIST))) == NULL) {
	fprintf (stderr, "unable to malloc durlist\n");
	terminate_program (1);
    }
    /* Append after "prev" entry at end of the list.		*/
    strcpy(sp->channel, channel);
    strcpy(sp->duration, duration);
    sp->next = (prev != NULL) ? prev->next : NULL;
    if (prev == NULL) head->next = sp;
    else prev->next = sp;
}

/************************************************************************/
/*  update_durlist:							*/
/*	Update durlist for the specified channel and type		*/
/************************************************************************/
void update_durlist (char *str, int nhead)
{
    char *duration;
    char *channel;
    int nchannels = 0;
    int error = 0;
    int i;
    char *p;

    /* Split comma-delimted string into individual tokens	*/
    /* First token is the duration.				*/
    /* Verify that it is a valid duration.			*/
    /* Currently duration must be one of the following:		*/
    /* a.   integer days:   nD					*/
    /* b.   integer hours:  nH					*/
    /*	    where 0<n<=24 && 24%n == 0				*/
    /*	    so that files end roughly on day boundaries.	*/
    duration = strtok(str,",");
    i = strtol(duration,&p,10);
    if (strcmp(p,"H") == 0) {
	if (i <= 0 || i > 24 || 24%i != 0) ++error;
    }
    else if (strcmp(p,"d") == 0) {
	if (i <= 0) ++error;
    }
    else ++error;
    if (error) {
	fprintf (info, "Invalid duration: %s\n", duration);
	terminate_program (1);
    }

    while (channel = strtok(NULL,",")) {
	append_durlist(&durhead[nhead],channel,duration);
	++nchannels;
    }
    if (nchannels == 0) append_durlist (&durhead[nhead],"???",duration);
}

/************************************************************************/
/*  build_filename:							*/
/*	Build a filename from the template format.			*/
/************************************************************************/
char *build_filename(FINFO *fip)
{
    static char name[1024];
    char *ip, *op;
    ip = filename_fmt;
    sprintf (name, "%s/%s/", station_dir, fip->ch_dir);
    op = name + strlen(name);

    while (*ip != '\0') {
	switch (*ip) {
	  case ('%'):
	    switch (*++ip) {
	    case ('S'):
		sprintf (op, "%s", fip->station);
		uppercase(op);
		op += strlen(fip->station); break;
	    case ('s'):
		sprintf (op, "%s", fip->station);
		lowercase(op);
		op += strlen(fip->station); break;
	    case ('N'):
		sprintf (op, "%s", fip->network);
		uppercase(op);
		op += strlen(fip->network); break;
	    case ('n'):
		sprintf (op, "%s", fip->network);
		lowercase(op);
		op += strlen(fip->network); break;
	    case ('L'):
		sprintf (op, "%s", fip->location);
		uppercase(op);
		op += strlen(fip->location); break;
	    case ('l'):
		sprintf (op, "%s", fip->location);
		lowercase(op);
		op += strlen(fip->location); break;
	    case ('C'):
		sprintf (op, "%s", fip->channel);
		uppercase(op);
		op += strlen(fip->channel); break;
	    case ('c'):
		sprintf (op, "%s", fip->channel);
		lowercase(op);
		op += strlen(fip->channel); break;
	    case ('X'):
		sprintf (op, "%s", extension[fip->itype]);
		uppercase(op);
		op += strlen(extension[fip->itype]); break;
	    case ('x'):
		sprintf (op, "%s", extension[fip->itype]);
		lowercase(op);
		op += strlen(extension[fip->itype]); break;
	    case ('Y'):
		sprintf (op, "%04d", fip->begtime.year);
		op += 4; break;
	    case ('y'):
		sprintf (op, "%02d", fip->begtime.year % 100);
		op += 2; break;
	    case ('m'):
		sprintf (op, "%02d", fip->begtime.month);
		op += 2; break;
	    case ('d'):
		sprintf (op, "%02d", fip->begtime.day);
		op += 2; break;
	    case ('j'):
		sprintf (op, "%03d", fip->begtime.doy);
		op += 3; break;
	    case ('H'):
		sprintf (op, "%02d", fip->begtime.hour);
		op += 2; break;
	    case ('M'):
		sprintf (op, "%02d", fip->begtime.minute);
		op += 2; break;
	    default:
		fprintf (info, "invalid format\n");
		sprintf (op, "%%%c", *ip);
		break;
	    }
	    ++ip; break;
	default:    *op++ = *ip++; break;
	}
    }
    *op = '\0';
    return(name);
}

/************************************************************************/
/*  parse_cfg:								*/
/*	Parse config directives in section for for this program.	*/
/************************************************************************/
void parse_cfg (char *str1, char *str2)
{
    int i;
    char *p;

    if (strcmp(str1, "DIR") == 0) {		/* major data directory	*/
	charncpy(station_dir, str2, 255);
    }
    else if (strcmp(str1, "FILENAME_FORMAT") == 0) {	/* major data directory	*/
	charncpy(filename_fmt, str2, 255);
    }
    else if (strcmp(str1, "PROGRAM") == 0) {	/* program pathname	*/
    }
    else if (strcmp(str1, "PIDFILE") == 0) {	/* pid file for client	*/
	FILE *fp = NULL;
	charncpy(pidfile,str2,1024-1);
	if ((fp = fopen(pidfile,"w")) == NULL) {
	    fprintf (stderr, "Unable to open pid file: %s\n", pidfile);
	}
	else {
	    fprintf (fp, "%d\n",(int)getpid());
	    fclose(fp);
	}
    }
    else if (strcmp(str1,"LOCKFILE")==0) {
	strcpy(lockfile,str2);
    }
    else if (strcmp(str1,"TRIMRECLEN")==0) {
	trimreclen = boolean_value(str2);
    }

    /* Set selector and/or data mask for each type of info.		*/
    /* Global selector sets default for data, detection, and cal.	*/
    else if (strcmp(str1, "SELECTOR") == 0) {	/* global selectors 	*/
	store_selectors(DATAQ, &sel[DATAQ],str2);
	store_selectors(DETQ, &sel[DETQ],str2);
	store_selectors(CALQ, &sel[CALQ],str2);
	store_selectors(TIMQ, &sel[TIMQ],str2);
	store_selectors(MSGQ, &sel[MSGQ],str2);
	store_selectors(BLKQ, &sel[BLKQ],str2);
    }
    /* Set data_mask and optional selectors for each type of info.	*/
    else if (strcmp(str1, "DATA_SELECTOR") == 0) {	/* data mask, selectors.*/
	boolean_mask (&data_mask, CSIM_DATA, str2);
	if ((p=strchr(str2,','))) store_selectors(DATAQ, &sel[DATAQ],++p);
    }
    else if (strcmp(str1, "DETECTION_SELECTOR") == 0) {	/* det mask, selectors.	*/
	boolean_mask (&data_mask, CSIM_EVENT, str2);
	if ((p=strchr(str2,','))) store_selectors(DETQ, &sel[DETQ],++p);
    }
    else if (strcmp(str1, "CALIBRATION_SELECTOR") == 0) {	/* cal mask, selectors.	*/
	boolean_mask (&data_mask, CSIM_CAL, str2);
	if ((p=strchr(str2,','))) store_selectors(CALQ, &sel[CALQ],++p);
    }
    else if (strcmp(str1, "TIMING_SELECTOR") == 0) {	/* clock (timing) mask.	*/
	boolean_mask (&data_mask, CSIM_TIMING, str2);
	if ((p=strchr(str2,','))) store_selectors(TIMQ, &sel[TIMQ],++p);
    }
    else if (strcmp(str1, "LOG_SELECTOR") == 0) {	/* log (msgs) mask.	*/
	boolean_mask (&data_mask, CSIM_MSG, str2);
	if ((p=strchr(str2,','))) store_selectors(MSGQ, &sel[MSGQ],++p);
    }
    else if (strcmp(str1, "BLK_SELECTOR") == 0) {	/* opaque blockettes mask.	*/
	boolean_mask (&data_mask, CSIM_BLK, str2);
	if ((p=strchr(str2,','))) store_selectors(BLKQ, &sel[BLKQ],++p);
    }

    /* Set filter selector and/or data flag for each type of info.	*/
    /* Global selector sets default for data, detection, and cal.	*/
    else if (strcmp(str1, "SAVE") == 0) {	/* global selectors 	*/
	save[DAT_INDEX].flag = TRUE;
	store_selectors(DATAQ, &save[DAT_INDEX].sel,str2);
	save[DET_INDEX].flag = TRUE;
	store_selectors(DETQ, &save[DET_INDEX].sel,str2);
	save[CAL_INDEX].flag = TRUE;
	store_selectors(CALQ, &save[CAL_INDEX].sel,str2);
	save[CLK_INDEX].flag = TRUE;
	store_selectors(TIMQ, &save[CLK_INDEX].sel,str2);
	save[LOG_INDEX].flag = TRUE;
	store_selectors(MSGQ, &save[LOG_INDEX].sel,str2);
	save[BLK_INDEX].flag = TRUE;
	store_selectors(BLKQ, &save[BLK_INDEX].sel,str2);
    }
    /* Set data_flag and optional selectors for each type of info.	*/
    else if (strcmp(str1, "DATA_SAVE") == 0) {	/* data flag, selectors.*/
	save[DAT_INDEX].flag = boolean_value(str2);
	if ((p=strchr(str2,','))) store_selectors(DATAQ, &save[DAT_INDEX].sel,++p);
    }
    else if (strcmp(str1, "DETECTION_SAVE") == 0) {	/* det flag, selectors.	*/
	save[DET_INDEX].flag = boolean_value(str2);
	if ((p=strchr(str2,','))) store_selectors(DETQ, &save[DET_INDEX].sel,++p);
    }
    else if (strcmp(str1, "CALIBRATION_SAVE") == 0) {	/* cal flag, selectors.	*/
	save[CAL_INDEX].flag = boolean_value(str2);
	if ((p=strchr(str2,','))) store_selectors(CALQ, &save[CAL_INDEX].sel,++p);
    }
    else if (strcmp(str1, "TIMING_SAVE") == 0) {	/* clock (timing) flag.	*/
	save[CLK_INDEX].flag = boolean_value(str2);
	if ((p=strchr(str2,','))) store_selectors(TIMQ, &save[CLK_INDEX].sel,++p);
    }
    else if (strcmp(str1, "LOG_SAVE") == 0) {		/* log (msgs) flag.	*/
	save[LOG_INDEX].flag = boolean_value(str2);
	if ((p=strchr(str2,','))) store_selectors(MSGQ, &save[LOG_INDEX].sel,++p);
    }
    else if (strcmp(str1, "BLK_SAVE") == 0) {		/* opaque blockette flag.*/
	save[BLK_INDEX].flag = boolean_value(str2);
	if ((p=strchr(str2,','))) store_selectors(BLKQ, &save[BLK_INDEX].sel,++p);
    }

    /* Set time limit for the max duration of files.			*/
    /* Global time limit applies to any type of file or channel that	*/
    /* does not have a specific limit.					*/
    else if (strcmp(str1, "LIMIT") == 0) {	/* time limit for files	*/
	char str3[160];
	for (i=0; i<6; i++) {
	    strcpy(str3,str2);
	    update_durlist (str3, i);
	}
    }
    else if (strcmp(str1, "DATA_LIMIT") == 0) {	/* limit for data files	*/
	update_durlist (str2, DAT_INDEX);
    }
    else if (strcmp(str1, "DETECTION_LIMIT") == 0) {	/* limit for det files	*/
	update_durlist (str2, DET_INDEX);
    }
    else if (strcmp(str1, "CALIBRATION_LIMIT") == 0) {	/* limit for cal files	*/
	update_durlist (str2, CAL_INDEX);
    }
    else if (strcmp(str1, "TIMING_LIMIT") == 0) {	/* limit for clock files*/
	update_durlist (str2, CLK_INDEX);
    }
    else if (strcmp(str1, "LOG_LIMIT") == 0) {	/* limit for log files	*/
	update_durlist (str2, LOG_INDEX);
    }
    else if (strcmp(str1, "BLK_LIMIT") == 0) {	/* limit for log files	*/
	update_durlist (str2, BLK_INDEX);
    }

    /* Identifier character or string to be associated with each type	*/
    /* of file.  
    /* Channel directories are named:	channel.extension		*/
    /* and the extension can be used when constructing filenames.	*/
    else if (strcmp(str1, "DATA_EXT") == 0) {	/* dat file extension	*/
	if (extension[DAT_INDEX]) free(extension[DAT_INDEX]);
	extension[DAT_INDEX] = (char *)malloc(MAXLEN_X+1);
	charncpy(extension[DAT_INDEX],str2,MAXLEN_X);
    }
    else if (strcmp(str1, "DETECTION_EXT") == 0) {	/* det file extension	*/
	if (extension[DET_INDEX]) free(extension[DET_INDEX]);
	extension[DET_INDEX] = (char *)malloc(MAXLEN_X+1);
	charncpy(extension[DET_INDEX],str2,MAXLEN_X);
    }
    else if (strcmp(str1, "CALIBRATION_EXT") == 0) {	/* cal file extension	*/
	if (extension[CAL_INDEX]) free(extension[CAL_INDEX]);
	extension[CAL_INDEX] = (char *)malloc(MAXLEN_X+1);
	charncpy(extension[CAL_INDEX],str2,MAXLEN_X);
    }
    else if (strcmp(str1, "TIMING_EXT") == 0) {	/* clock file extension	*/
	if (extension[CLK_INDEX]) free(extension[CLK_INDEX]);
	extension[CLK_INDEX] = (char *)malloc(MAXLEN_X+1);
	charncpy(extension[CLK_INDEX],str2,MAXLEN_X);
    }
    else if (strcmp(str1, "LOG_EXT") == 0) {	/* log file extension	*/
	if (extension[LOG_INDEX]) free(extension[LOG_INDEX]);
	extension[LOG_INDEX] = (char *)malloc(MAXLEN_X+1);
	charncpy(extension[LOG_INDEX],str2,MAXLEN_X);
    }
    else if (strcmp(str1, "BLK_EXT") == 0) {	/* blockette file extension	*/
	if (extension[BLK_INDEX]) free(extension[BLK_INDEX]);
	extension[BLK_INDEX] = (char *)malloc(MAXLEN_X+1);
	charncpy(extension[BLK_INDEX],str2,MAXLEN_X);
    }
    else {
	fprintf (stderr, "Unknown datalog directive: %s\n", str1);
	terminate_program (1);
    }
}

/************************************************************************/
/*  verify_cfg:								*/
/*	Verify necessary information from config directives.		*/
/************************************************************************/
void verify_cfg()
{
    int errors = 0;
    if (strlen(station_dir)==0) {
	fprintf (stderr,"Missing DIR entry\n");
	++errors;
    }
    if (errors) terminate_program (1);
}

/************************************************************************/
/*  boolean_mask:							*/
/*	Set or clear the bit_mask values in *ps based on Y/N string.	*/
/************************************************************************/
void boolean_mask (short *ps, short bit_mask, char *str)
{
    switch (str[0]) {
      case 'Y':
      case 'y':
	*ps |= bit_mask;
	break;
      case 'N':
      case 'n':
	*ps &= (! bit_mask);
    }
}
   
const char *selector_type_str[] = {"DAT", "DET", "CAL", "TIM", "MSG", "BLK"};
/************************************************************************/
/*  store_selectors:							*/
/*	Parse and store selectors for specific types of info.		*/
/************************************************************************/
void store_selectors(int type, SEL *psel, char *str)
{
    char *token;
    int i, l;
    seltype *selectors = NULL;
    char *p = str;
    int n = 0;

    if ((int)strlen(str) <= 0) return;
    psel->nselectors = 0;
    if (psel->selectors) free (psel->selectors);
    while (token = strtok(p,",")) {
	if ((int)strlen(token) > 5) {
	    fprintf (info, "Error in selector list for %s : %s\n",
		     selector_type_str[type], token);
	    if (selectors) free (selectors);
	    return;
	}
	for (i=1,l=strlen(token); i<l; i++) {
	    if (islower(token[i])) token[i] = toupper(token[i]);
	}
	selectors = (selectors == NULL) ? (seltype *)malloc(sizeof(seltype)) : 
	    (seltype *)realloc (selectors, (n+1)*sizeof(seltype));
	if (selectors == NULL) {
	    fprintf (info, "Error allocating selector space for %s\n",
		     selector_type_str[type]);
	    return;
	}
	strcpy(selectors[n++],lead(5,'?',token));
	p = NULL;
    }
    psel->selectors = selectors;
    psel->nselectors = n;
    return;
}

/************************************************************************/
/*  set_selectors:							*/
/*	Set selector values for the single station.			*/
/*	Assume sufficient selectors have been reserved.			*/
/************************************************************************/
void set_selectors (pclient_struc me)
{
    pclient_station this ;
    pselarray psa ;
    int nsel = 1;
    int type, n;
    int i, j;

    this = (pclient_station) ((long) me + me->offsets[0]) ;
    psa = (pselarray) ((long) me + this->seloffset) ;
	
    for (type=0; type<CHAN; type++) {
	n = sel[type].nselectors;
	if (n+nsel > this->maxsel) {
	    fprintf (info, "Error: Require %d selectors, allocated %d\n",
		     n+nsel, this->maxsel);
	    return;
	}
	if (n > 0) {
	    this->sels[type].first = nsel;
	    this->sels[type].last = nsel + n - 1;
	    memcpy ((void *)&((*psa)[nsel]), (void *)sel[type].selectors, n*sizeof(seltype));
	}
	nsel += n;
    }
    if (! (verbosity & 4)) return;
    for (type=0; type<CHAN; type++) {
	n = sel[type].nselectors;
	if (n > 0) {
	    for (i=this->sels[type].first; i<=this->sels[type].last; i++) {
		fprintf (info, "%s selectors[%d] = %-5.5s\n", selector_type_str[type], i,
			 (*psa)[i]);
	    }
	}
    }
    return;
}

/************************************************************************/
/*  decode_val:								*/
/*	Decode decimal value (apparent bug in strtol that decodes	*/
/*	non-digits).							*/
/************************************************************************/
int decode_val (char *str, int len)
{
    int i;
    char tmpstr[80];
    for (i=0; i<len; i++) {
	if (! ((str[i] >= '0' && str[i] <= '9') || str[i] == ' ')) break;
    }
    if (i<len) return (-1);
    strncpy(tmpstr,str,len);
    tmpstr[len] = '\0';    
    i = strtol(tmpstr, NULL, 10);
    return (i);
}

/************************************************************************/
/*  sel_match:								*/
/*	Attempt to match location and channel to a selector list.	*/
/*	Return TRUE if a match is found, FALSE otherwise.		*/
/************************************************************************/
int sel_match (char *location, char *channel, SEL *psel)
{
    int i;
    char str[6];
    memcpy (str, location, 2);
    memcpy (str+2, channel, 3);
    str[5] = '\0';
    for (i=0; i<psel->nselectors; i++) {
	if (selector_match(str,psel->selectors[i])) return (TRUE);
    }
    return (FALSE);
}

/************************************************************************/
/*  selector_match:							*/
/*	Match a string to a selector.					*/
/*	Return TRUE if match, FALSE otherwise.				*/
/************************************************************************/
int selector_match (char *str1, char *str2)
{
    int i;
    char a, b;
    for (i=0; i<5; i++) {
	if (str1[i] == '?' || str2[i] == '?') continue;
	a = (islower(str1[i])) ? toupper(str1[i]) : str1[i];
	b = (islower(str2[i])) ? toupper(str2[i]) : str2[i];
	if (a == b) continue;
	return (FALSE);
    }
    return (TRUE);
}

/************************************************************************/
/*  date_string:							*/
/*	Return the string for a specified date.				*/
/************************************************************************/
char *date_string(INT_TIME time, char *dflt_str) 
{
    char    *p;
    if (missing_time(time)) return (dflt_str);
    p = time_to_str(time,TIME_FMT);
    return ((p!=NULL) ? p : dflt_str);
}

/************************************************************************/
/*  dump_seed:								*/
/*	Dump miniSEED block in decimal (for debugging).			*/
/************************************************************************/
void dump_seed(unsigned char *str)
{
    int i, k;
    for (i=0; i<512; i+=16) {
	printf ("%08d  ", i);
	for (k=i; k<512 && k<i+16; k++)
	    printf (" %03d", str[k]);
	printf ("\n");
    }
}

/************************************************************************/
/*  dump_data_hdr:							*/
/*	Dump the specified block header information.			*/
/************************************************************************/
void
dump_data_hdr (DATA_HDR *hdr)
{
    int		bl;
    fprintf (info, "seqno = %d\n", hdr->seq_no);
    fprintf (info, "station = %s\n", hdr->station_id);
    fprintf (info, "location = %s\n", hdr->location_id);
    fprintf (info, "channel = %s\n", hdr->channel_id);
    fprintf (info, "network = %s\n", hdr->network_id);
    fprintf (info, "hdrtime = %s\n", date_string(hdr->hdrtime, NULL_STR));
    fprintf (info, "begtime = %s\n", date_string(hdr->begtime, NULL_STR));
    fprintf (info, "endtime = %s\n", date_string(hdr->endtime, NULL_STR));
    fprintf (info, "sample_rate = %d\n", hdr->sample_rate);
    fprintf (info, "activity_flags = 0x%02x\n", hdr->activity_flags);
    fprintf (info, "io_flags = 0x%02x\n", hdr->io_flags);
    fprintf (info, "data_quality_flags = 0x%02x\n", hdr->data_quality_flags);
    fprintf (info, "num_ticks_correction = %d\n", hdr->num_ticks_correction);
    fprintf (info, "num_blockettes = %d\n", hdr->num_blockettes);
    fprintf (info, "first_blockette = %d\n", hdr->first_blockette);
    fprintf (info, "num_samples = %d\n", hdr->num_samples);
    fprintf (info, "num_data_frames = %d\n", hdr->num_data_frames);
    fprintf (info, "x0 = %d\n", hdr->x0);
    fprintf (info, "xn = %d\n", hdr->xn);
    /* dump out blockettes */
    if (hdr->pblockettes != NULL) {
	int next;
	int l;
	int type;
	BS *bs = hdr->pblockettes;
	do {
	    BLOCKETTE_HDR *bh;
	    if (bs->wordorder != my_wordorder) {
	    	if (swab_blockette(bs->type, bs->pb, bs->len) < 0) {
		    fprintf (info, "blockette %d\n",bs->type);
		    continue;
		}
	    	bs->wordorder = my_wordorder;
	    }
	    bh = (BLOCKETTE_HDR *)(bs->pb);
	    switch (type=bh->type) {
	      case 100:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 200:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 201:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 300:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 310:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 320:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 390:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 400:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 405:	
		fprintf (info, "blockette %d\n",type);
		break;
	      case 1000: 
		fprintf (info, "blockette %d, fmt = %d (%s), order = %d (%s), len = %d (%d)\n",
			 type, 
			 ((BLOCKETTE_1000 *)bh)->format, 
			 (((BLOCKETTE_1000 *)bh)->format == STEIM1) ? "STEIM1" :
			 (((BLOCKETTE_1000 *)bh)->format == STEIM2) ? "STEIM2" : 
			 "unknown",
			 ((BLOCKETTE_1000 *)bh)->word_order, 
			 (((BLOCKETTE_1000 *)bh)->word_order == SEED_BIG_ENDIAN) ? "BIG_ENDIAN" :
			 (((BLOCKETTE_1000 *)bh)->word_order == SEED_LITTLE_ENDIAN) ? "LITTLE_ENDIAN" : 
			 "unknown",
			 ((BLOCKETTE_1000 *)bh)->data_rec_len, 
			 roundoff ( ldexp(1.,((BLOCKETTE_1000 *)bh)->data_rec_len) ) );
		break;
	      case 1001: 
		fprintf (info, "blockette %d, clock_quality = %d, usec99 = %d, frame_count=%d\n",
			 type, 
			 ((BLOCKETTE_1001 *)bh)->clock_quality, 
			 ((BLOCKETTE_1001 *)bh)->usec99, 
			 ((BLOCKETTE_1001 *)bh)->frame_count );
		break;
	      default:
		fprintf (info,   "unknown blockette %d, skipping\n", bh->type);
		fprintf (stderr, "unknown blockette %d, skipping\n", bh->type);
	    }
	    bs = bs->next;
	} while (bs != (BS *)NULL);
    }
    dump_seed((unsigned char *)pseedblock);

    fprintf (info, "\n");
    fflush(info);
}

/************************************************************************/
/*  dump_durlist:							*/
/*	Dump a durlist (for diagnostics).				*/
/************************************************************************/
void dump_durlist (DURLIST *head)
{
    DURLIST *sp = head;
    while (sp != NULL) {
	fprintf (info, "channel = %s, duration = %s\n", sp->channel, sp->duration);
	sp = sp->next;
    }
}

/************************************************************************/
/*  reduce_reclen:							*/
/*	Change the blocksize of the record if it appears that all	*/
/*	records for this channel and type can fit into a smaller	*/
/*	MiniSEED record.						*/
/*	Currently this will be done ONLY for records of type DAT_INDEX.	*/
/*  Return:								*/
/*	1 if reblocked; 0 if not reblocked.				*/
/*	If reblocked,							*/
/*	a.  Change blksize in DATA_HDR.					*/
/*	b.  Change blksize in blockette 1000 of Mini_SEED record.	*/
/************************************************************************/
int reduce_reclen (FINFO *fip, DATA_HDR *hdr, seed_record_header *pseed)
{
    int status = 0;
    int min_blksize, min_data_rec_len;

    if (fip->itype != DAT_INDEX) return (status);
    if (! IS_STEIM_COMP(hdr->data_type)) return (status);

    /* Determine the new minimum blksize and data_record_len.		*/
    /* Limit the minimum size by the MIN_SEED_BLKSIZE.			*/
    if (my_wordorder < 0) get_my_wordorder();
    min_blksize = ((int)((hdr->first_data+sizeof(FRAME)-1)/sizeof(FRAME)) +
		   hdr->num_data_frames) * sizeof(FRAME);
    if (min_blksize < MIN_SEED_BLKSIZE) min_blksize = MIN_SEED_BLKSIZE;
    min_data_rec_len = (int) log2(min_blksize);
    while (exp2(min_data_rec_len) < min_blksize) ++min_data_rec_len;
    min_blksize = (int) exp2(min_data_rec_len);

    /* If this record is a candidate for a smaller blksize, find and	*/
    /* alter the data_rec_len in the blockette 1000.			*/
    if (min_blksize < hdr->blksize) {
	/* Rewrite MiniSEED record to reflect new minimum blksize.	*/
	/* Change the blockette 1000 that contains the blocksize.	*/
	BLOCKETTE_1000 *b1000;		/* ptr to blockette 1000.	*/
	char *p = (char *)pseed;	/* ptr in SEED data record.	*/
	short int bl_type, bl_next;	/* blockette header fields.	*/
	int i = hdr->first_blockette;	/* index of blockette in record.*/
	int swapflag;			/* flag for byteswapping.	*/

	swapflag = (my_wordorder != hdr->hdr_wordorder);
	bl_next = hdr->first_blockette;
	while (bl_next > 0) {
	    b1000 = (BLOCKETTE_1000 *)(p+i);
	    bl_type = b1000->hdr.type;
	    bl_next = b1000->hdr.next;
	    if (swapflag) {
		swab2(&bl_type);
		swab2(&bl_next);
	    }
	    if (bl_type == 1000) {
		b1000->data_rec_len = min_data_rec_len;
		hdr->blksize = min_blksize;
		status = 1;
		break;
	    }
	}
    }
    return (status);
}
