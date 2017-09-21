#pragma ident "$Id: purge.c,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $"
/* --------------------------------------------------------------------
 Program  : Any
 Task     : Archive Library API functions.
 File     : purge.c
 Purpose  : Purge functions.
 Host     : CC, GCC, Microsoft Visual C++ 5.x, MCC68K 3.1
 Target   : Solaris (Sparc and x86), Linux, DOS, Win32, and RTOS
 Author	  : Robert Banfill (r.banfill@reftek.com)
 Company  : Refraction Technology, Inc.
            2626 Lombardy Lane, Suite 105
            Dallas, Texas  75220  USA
            (214) 353-0609 Voice, (214) 353-9659 Fax, info@reftek.com
 Copyright: (c) 1997 Refraction Technology, Inc. - All Rights Reserved.
 Notes    :
 $Revision: 1.1.1.1 $
 $Logfile : R:/cpu68000/rt422/struct/version.h_v  $
 Revised  :
  17 Aug 1998  ---- (RLB) First effort.

-----------------------------------------------------------------------*/

#define _PURGE_C
#include "archive.h"

/* Local prototypes ---------------------------------------------------*/
static BOOL Purge(H_ARCHIVE harchive);
static BOOL FindOldestEvent(H_ARCHIVE harchive, EVENT * event);
static BOOL RemoveEventFile(CHAR *filespec);
static BOOL PathEmpty(CHAR *path);

/*---------------------------------------------------------------------*/
VOID InitPurge(PURGE *purge)
{
    ASSERT(purge != NULL);

    purge->active = FALSE;
    purge->stop = FALSE;
    MUTEX_INIT(&purge->mutex);
    SEM_INIT(&purge->semaphore, 0, 1);
    purge->thread_id = 0;

    return;
}

/*---------------------------------------------------------------------*/
VOID DestroyPurge(PURGE *purge)
{
    ASSERT(purge != NULL);

    MUTEX_DESTROY(&purge->mutex);
    SEM_DESTROY(&purge->semaphore);

    return;
}

/*---------------------------------------------------------------------*/
BOOL PurgeOldestEvent(H_ARCHIVE harchive)
{
    ARCHIVE *archive; 

    if (!ValidateHandle(harchive)) {
        ArchiveLog(ARC_LOG_ERRORS, "%s", "PurgeOldestEvent: ValidateHandle failed");
        return FALSE;
    }

    archive = &_archive[harchive];

    MUTEX_LOCK(&archive->purge.mutex);
    if(archive->purge.active) {
        MUTEX_UNLOCK(&archive->purge.mutex);
        SEM_POST(&archive->purge.semaphore);
    }
    else
        MUTEX_UNLOCK(&archive->purge.mutex);

    return TRUE;
}

/*---------------------------------------------------------------------*/
THREAD_FUNC PurgeThread(VOID *argument)
{
    PURGE *purge; 

    ASSERT(argument != NULL);

    purge = (PURGE *)argument;

    MUTEX_LOCK(&purge->mutex);
    purge->active = TRUE;
    purge->stop = FALSE;
    MUTEX_UNLOCK(&purge->mutex);

    ArchiveLog(ARC_LOG_VERBOSE, "PurgeThread start, tid: %u, pid %u", THREAD_SELF(), getpid());

    while(TRUE) {
        SEM_WAIT(&purge->semaphore);
        if(purge->stop)
            break;
        
        ArchiveLog( ARC_LOG_VERBOSE, "%s", "Archive: purge initiated");

        if(!Purge(purge->handle)) 
            break;
    }

    ArchiveLog(ARC_LOG_VERBOSE, "PurgeThread exit");
    MUTEX_LOCK(&purge->mutex);
    purge->active = FALSE;
    MUTEX_UNLOCK(&purge->mutex);
    THREAD_EXIT((VOID *)0);
}

/*---------------------------------------------------------------------*/
static BOOL Purge(H_ARCHIVE harchive)
{
    VOID *ptr;
    CHAR string[32];
    REAL64 earliest;
    STREAM criteria;
    STREAM *stream;
    EVENT event;

    if (!ValidateHandle(harchive)) {
        ArchiveLog(ARC_LOG_ERRORS, "Purge: ValidateHandle failed");
        return (FALSE);
    }

    _archive[harchive].last_error = ARC_NO_ERROR;
    _archive_error = ARC_NO_ERROR;

    /* Find the oldest event in the archive */
    if (!FindOldestEvent(harchive, &event)) {
        ArchiveLog(ARC_LOG_ERRORS, "Purge: FindOldestEvent failed");
        return (FALSE);
    }

    ArchiveLog(ARC_LOG_VERBOSE, "Purge event: %s", event.filespec);

    MUTEX_LOCK(&_archive[harchive].mutex);

    /* Find the stream record */
    if ((stream = LookupStream(harchive, event.unit, event.stream)) == NULL) {
        ArchiveLog(ARC_LOG_ERRORS, "%s", "LookupStream failed");
        return (FALSE);
    }
    /* Adjust size */
    if (stream->bytes > event.bytes)
        stream->bytes -= event.bytes;
    else
        stream->bytes = 0;
    if (_archive[harchive].state.bytes > event.bytes)
        _archive[harchive].state.bytes -= event.bytes;
    else
        _archive[harchive].state.bytes = 0;

    ArchiveLog(ARC_LOG_MAXIMUM, "Archive: stream %04X:%u size: %s bytes", event.unit, event.stream,
        FormatAsSIBinaryUnit(string, stream->bytes, TRUE));
    ArchiveLog(ARC_LOG_MAXIMUM, "Archive size: %s bytes", FormatAsSIBinaryUnit(string,
            _archive[harchive].state.bytes, TRUE));

    MUTEX_UNLOCK(&_archive[harchive].mutex);

    /* Delete the file */
    RemoveEventFile(event.filespec);

    /* Find the *new* earliest event for this stream */
    InitStream(&criteria);
    criteria.unit = event.unit;
    criteria.stream = event.stream;
    InitEvent(&event);
    if (!ArchiveFindFirstEvent(harchive, &criteria, &event)) {
        MUTEX_LOCK(&_archive[harchive].mutex);
        stream->time.earliest = VOID_TIME;
        stream->time.latest = VOID_TIME;
    }
    /* This is now the earliest time in the stream record */
    else {
        MUTEX_LOCK(&_archive[harchive].mutex);
        stream->time.earliest = event.time.earliest;
        ArchiveLog(ARC_LOG_MAXIMUM, "Purge: stream earliest data: %s", FormatMSTime(string,
                stream->time.earliest, 10));
    }

    /* Find new archive earliest time */
    earliest = VOID_TIME;
    if ((stream = ArchiveFirstStream(harchive, &ptr)) != NULL) {
        do {
            if (UndefinedTime(earliest) || stream->time.earliest < earliest)
                earliest = stream->time.earliest;
        } while ((stream = ArchiveNextStream(&ptr)) != NULL);
    }
    if (UndefinedTime(earliest)) {
        _archive[harchive].state.time.earliest = VOID_TIME;
        _archive[harchive].state.time.latest = VOID_TIME;
    }
    else {
        _archive[harchive].state.time.earliest = earliest;
        ArchiveLog(ARC_LOG_MAXIMUM, "Purge: archive earliest data: %s", FormatMSTime(string,
                _archive[harchive].state.time.earliest, 10));
    }
    MUTEX_UNLOCK(&_archive[harchive].mutex);

    return (TRUE);
}

/*---------------------------------------------------------------------*/
static BOOL FindOldestEvent(H_ARCHIVE harchive, EVENT * event)
{
    STREAM criteria;
    EVENT info;
    MSTIME_COMP time;

    ASSERT(event != NULL);

    if (!ValidateHandle(harchive)) {
        ArchiveLog(ARC_LOG_ERRORS, "Purge: ValidateHandle failed");
        return (FALSE);
    }

    InitEvent(event);

    /* Find first file in archive */
    InitEvent(&info);
    InitStream(&criteria);
#if 0
    if (ArchiveFindFirstEvent(harchive, &criteria, &info)) {
        do {
            /* Ignore all state-of-health */
            if (info.stream != 0)
                break;
        } while (ArchiveFindNextEvent(harchive, &criteria, &info));
    }
    else {
        ArchiveLog(ARC_LOG_VERBOSE, "Purge: ArchiveFindFirstEvent failed");
    }
#else
    if (!ArchiveFindFirstEvent(harchive, &criteria, &info)) {
        ArchiveLog(ARC_LOG_VERBOSE, "Purge: ArchiveFindFirstEvent failed");
    }
#endif
    if (info.unit == VOID_UNIT) {
        ArchiveLog(ARC_LOG_VERBOSE, "Purge: void unit!");
        return (FALSE);
    }

    /* Set criteria time range to cover this day */
    DecomposeMSTime(info.time.earliest, &time);
    criteria.time.earliest = EncodeMSTimeDOY(time.year, time.doy, 0, 0, 0.0);
    criteria.time.latest = criteria.time.earliest + DAY;

    /* Find oldest event file in this day */
    InitEvent(&info);
    if (ArchiveFindFirstEvent(harchive, &criteria, &info)) {
        do {
#if 0
            /* Ignore all state-of-health */
            if (info.stream == 0)
                continue;
#endif
            if (event->unit == VOID_UNIT || info.time.earliest < event->time.earliest)
                memcpy(event, &info, sizeof(STREAM));
        } while (ArchiveFindNextEvent(harchive, &criteria, &info));
    }

    if (event->unit == VOID_UNIT) {
        ArchiveLog(ARC_LOG_VERBOSE, "Purge: void event unit");
        return (FALSE);
    }

    return (TRUE);
}

/*---------------------------------------------------------------------*/
static BOOL RemoveEventFile(CHAR *path)
{

    ASSERT(path != NULL);

    /* Delete the file... */
    if(remove(path) < 0) {
        ArchiveLog(ARC_LOG_ERRORS, "Purge: unable to remove file: %s", path);
        return FALSE;
    }

    /* Remove emtpy stream directory */
    if(!TrimPath(path, 1)) 
        return FALSE;
    if(!PathEmpty(path))
        return TRUE;
    ArchiveLog(ARC_LOG_VERBOSE, "Purge: removing %s", path);
    if(!DestroyPath(path)) {
        ArchiveLog(ARC_LOG_ERRORS, "Purge: unable to remove directory: %s - %s", path,
            FileErrorString(FileLastError()));
        return FALSE;
    }

    /* Remove empty unit directory */
    if(!TrimPath(path, 1)) 
        return FALSE;
    if(!PathEmpty(path))
        return TRUE;
    ArchiveLog(ARC_LOG_VERBOSE, "Purge: removing %s", path);
    if(!DestroyPath(path)) {
        ArchiveLog(ARC_LOG_ERRORS, "Purge: unable to remove directory: %s - %s", path,
            FileErrorString(FileLastError()));
        return FALSE;
    }

    /* Remove empty day directory */
    if(!TrimPath(path, 1)) 
        return FALSE;
    if(!PathEmpty(path))
        return TRUE;
    ArchiveLog(ARC_LOG_VERBOSE, "Purge: removing %s", path);
    if(!DestroyPath(path)) {
        ArchiveLog(ARC_LOG_ERRORS, "Purge: unable to remove directory: %s - %s", path,
            FileErrorString(FileLastError()));
        return FALSE;
    }

    return TRUE;
}

/*---------------------------------------------------------------------*/
static BOOL PathEmpty(CHAR *path)
{
    BOOL result;
    EVENT info;

    ASSERT(path != NULL);

    /* Look for any files... */
    result = TRUE;
    sprintf(info.filespec, "%s/*", path);
    info.attribute = FIND_FILE;
    if(FileFindFirst(&info)) 
        result = FALSE;
    FileFindClose(&info);

    if(!result)
        return result;

    /* or subdirectories... */
    result = TRUE;
    sprintf(info.filespec, "%s/*", path);
    info.attribute = FIND_SUBDIR; 
    if(FileFindFirst(&info)) {
        do {
            if(info.filespec[0] != '.') {
                result = FALSE;
                break;
            }
        } while (FileFindNext(&info));
    }
    FileFindClose(&info);

    return result;
}

/* Revision History
 *
 * $Log: purge.c,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.4  2002/01/18 17:53:21  nobody
 * changed interpretation of unit ID from BCD to binary
 *
 * Revision 1.3  2001/10/04 20:42:19  nobody
 * Fixed Linux build issues
 *
 * Revision 1.2  2001/07/23 18:48:25  nobody
 * Added purge thread
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
