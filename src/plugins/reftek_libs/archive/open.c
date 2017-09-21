#pragma ident "$Id: open.c,v 1.1.1.1 2005/07/26 19:28:53 andres Exp $"
/* --------------------------------------------------------------------
 Program  : Any
 Task     : Archive Library API functions.
 File     : open.c
 Purpose  : Open and close routines.
 Host     : CC, GCC, Microsoft Visual C++ 5.x
 Target   : Solaris (Sparc and x86), Linux, Win32
 Author   : Robert Banfill (r.banfill@reftek.com)
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

-------------------------------------------------------------------- */

#include "archive.h"

/* Local prototypes ---------------------------------------------------*/
BOOL ValidatePath(CHAR * path, H_ARCHIVE harchive);

/*---------------------------------------------------------------------*/
H_ARCHIVE ArchiveOpenForWrite(CHAR * path)
{
    H_ARCHIVE harchive;
    ARCHIVE *archive;

    ArchiveLog(ARC_LOG_VERBOSE, "Open archive for write: %s", path);

    _archive_error = ARC_NO_ERROR;

    /* Get next available handle */
    if ((harchive = GetNextHandle()) == VOID_H_ARCHIVE)
        return (VOID_H_ARCHIVE);

    _archive[harchive].last_error = ARC_NO_ERROR;

    /* Check out the path */
    if (!ValidatePath(path, harchive))
        return (VOID_H_ARCHIVE);

    /* Create stream list */
    if (!CreateList(&_archive[harchive].streams)) {
        _archive_error = ARC_NO_MEMORY;
        return (VOID_H_ARCHIVE);
    }

    /* Get state and lock for write access */
    if (!OpenStateForWrite(harchive)) {
        _archive_error = _archive[harchive].last_error;
        return (VOID_H_ARCHIVE);
    }

    _n_archives++;

    /* Start the purge thread... */
    archive = &_archive[harchive];
    if (archive->state.thres_bytes != VOID_UINT64) {
        MUTEX_LOCK(&archive->purge.mutex);
        if(!archive->purge.active ) {
            ArchiveLog(ARC_LOG_VERBOSE, "Starting purge thread");
            if(!THREAD_CREATE(&archive->purge.thread_id, PurgeThread, &archive->purge)) {
                MUTEX_UNLOCK(&archive->purge.mutex);
                _archive_error = ARC_PURGE_THREAD;
                return (VOID_H_ARCHIVE);
            }
        }
        MUTEX_UNLOCK(&archive->purge.mutex);
    }

    return (harchive);
}

/*---------------------------------------------------------------------*/
H_ARCHIVE ArchiveOpenForRead(CHAR * path)
{
    H_ARCHIVE harchive;

    ArchiveLog(ARC_LOG_VERBOSE, "Open archive for read: %s", path);

    _archive_error = ARC_NO_ERROR;

    /* Get next available handle */
    if ((harchive = GetNextHandle()) == VOID_H_ARCHIVE)
        return (VOID_H_ARCHIVE);

    _archive[harchive].last_error = ARC_NO_ERROR;

    /* Check out the path */
    if (!ValidatePath(path, harchive))
        return (VOID_H_ARCHIVE);

    /* Create stream list */
    if (!CreateList(&_archive[harchive].streams)) {
        _archive_error = ARC_NO_MEMORY;
        return (VOID_H_ARCHIVE);
    }

    /* Get state and open for read access */
    if (!OpenStateForRead(harchive)) {
        _archive_error = _archive[harchive].last_error;
        return (VOID_H_ARCHIVE);
    }

    _n_archives++;

    return (harchive);
}

/*---------------------------------------------------------------------*/
BOOL ArchiveClose(H_ARCHIVE harchive)
{
    NODE *node;
    STREAM *stream;

    if (!ValidateHandle(harchive))
        return (FALSE);

    _archive[harchive].last_error = ARC_NO_ERROR;
    _archive_error = ARC_NO_ERROR;

    ArchiveLog(ARC_LOG_VERBOSE, "Close archive: %s", _archive[harchive].path);

    /* Close all open event files on archive... */
    if ((node = FirstNode(&_archive[harchive].streams))) {
        do {
            stream = (STREAM *) node->data;
            if (_archive[harchive].access == ARC_WRITE)
                CloseEventFileAndRename(harchive, stream);
            else
                CloseEventFile(stream);
        } while ((node = NextNode(node)) != NULL);
    }

    /* If open for write... */
    if (_archive[harchive].access == ARC_WRITE) {
        MUTEX_LOCK(&_archive[harchive].purge.mutex);
        if(_archive[harchive].purge.active ) {
            _archive[harchive].purge.stop = TRUE;
            MUTEX_UNLOCK(&_archive[harchive].purge.mutex);
            ArchiveLog(ARC_LOG_VERBOSE, "Stopping purge thread");
            SEM_POST(&_archive[harchive].purge.semaphore);
            THREAD_JOIN(&_archive[harchive].purge.thread_id);
        }
        else 
            MUTEX_UNLOCK(&_archive[harchive].purge.mutex);

        /* Mark as closed and write state to disk */
        _archive[harchive].state.write = FALSE;
        if (!WriteState(harchive))
            return (FALSE);
    }

    /* Close the state file */
    if (!FileClose(_archive[harchive].file)) {
        _archive[harchive].last_error = ARC_FILE_IO_ERROR;
        return (FALSE);
    }

    _n_archives--;

    /* Clear state */
    MUTEX_DESTROY(&_archive[harchive].mutex);
    DestroyList(&_archive[harchive].streams);
    DestroyPurge(&_archive[harchive].purge);

    InitArchive(harchive);

    return (TRUE);
}

/*---------------------------------------------------------------------*/
BOOL ValidatePath(CHAR * path, H_ARCHIVE harchive)
{

    _archive[harchive].last_error = ARC_NO_ERROR;
    _archive_error = ARC_NO_ERROR;

    /* Check out the path */
    if (path == NULL)
        _archive_error = ARC_BAD_PATH;

    if (strlen(path) > MAX_PATH_LEN - ARC_PATH_LEN)
        _archive_error = ARC_BAD_PATH;

    if (!IsDirectory(path))
        _archive_error = ARC_BAD_PATH;

    if (_archive_error != ARC_NO_ERROR)
        return (FALSE);

    /* Fully qualify the path */
    QualifyPath(_archive[harchive].path, path);

    return (TRUE);
}

/* Revision History
 *
 * $Log: open.c,v $
 * Revision 1.1.1.1  2005/07/26 19:28:53  andres
 * Imported sources
 *
 * Revision 1.2  2001/07/23 18:48:25  nobody
 * Added purge thread
 *
 * Revision 1.1.1.1  2000/06/22 19:13:09  nobody
 * Import existing sources into CVS
 *
 */
