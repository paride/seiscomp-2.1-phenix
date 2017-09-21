/****************************************************************************
BM         
M* Module      : FileFind.c 
 * 
D* Description : File find utilitie 
 * 
I* Import      :
 *
E* Export      : int	FileFind( char*, char*, char* )
E*              
E*              
 *
P* Author      : W. Ruesing
 *
U* Updates     : 8.04.93 - iFound is now set by iCount, because of an
U*                         counting error, if an error flag is returned. 
EM 
 ****************************************************************************/
#include <stdio.h>
#include <memory.h>
#include <ctype.h>
#include <dirent.h>
#include <search.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

/* ======================================================================== */
/*                               Prototypes                                 */
/* ======================================================================== */
extern int   TestIfDir     ( char*, char*, char[] );
extern int   IsAPatternFile( char*, char* );
extern int   FileFind      ( char*, char*, char* );
extern int   WildCardStrCmp( char*, char* );	
extern char  *OStrTok      ( char*, char* );

/* ======================================================================== */
/* Switch for testing the functions                                         */ 
/* ======================================================================== */
/*#define TEST_FILE_FIND_MAIN	(1)*/ 
/*#define TEST_FILE_FIND	(1)*/  
/*#define TEST_WILDCARD_FIND	(1)*/ 

/* ======================================================================== */
/* Structure of the AVL tree leaf items (external library)                  */
/* ======================================================================== */
#define BUFLEN	(256)
typedef struct
	{
		long lKey;
		char cpFileName[BUFLEN];
		char cpPathName[BUFLEN];
	}LEAF;

#include "avl/avltree.h"

static TREE *root     = NULL,
	    *rootSort = NULL;
LEAF        *LLeaf1   = NULL,
            *LLeaf2   = NULL;
static long lKeyPos   = 0L;
static iCount         = 0;
	 
	 
/* ======================================================================== */
/*                                 macros                                   */
/* ======================================================================== */
char	caErrBuf[BUFLEN/2];
#define PERROR( x, y )	strcpy( caErrBuf, x);\
			strcat( caErrBuf, " [" );\
			strcat( caErrBuf, y );\
			strcat( caErrBuf, "] " );\
			perror( caErrBuf );\
			

/* ======================================================================== */
/*                      Module global variables                             */
/* ======================================================================== */
#define	TRUE		(1)
#define	FALSE		(0)
#define	NOT_FOUND	(0)
#define	FOUND		(1)
#define OPEN_ERROR	(-1)
#define CLOSE_ERROR	(-2)
#define STATUS_ERROR	(-3)
#define MALLOC_ERROR	(-4)
#define FLAG_ERROR	(-5)
#define DOUBLE_NODE	(-6)
#define EMPTY_TREE	(-7)
#define BUFFER_ERROR 	(-8)


static int	iRecursiveFlag 	= FALSE,	/* recursive search down    */
		iBufferStrFlag 	= FALSE,	/* save the file names      */
		iNameDownFlag	= FALSE,	/* skip downward            */
		iNameFreeFlag	= FALSE,	/* free all                 */
		iNameUpFlag	= FALSE,	/* ship upward              */
		iNameJoinFlag	= FALSE,	/* join path and filename   */
		iSkipUpFlag	= FALSE,	/* skip up one record       */
		iSkipDownFlag	= FALSE,	/* skip down one record     */
		iVerboseFlag	= FALSE,	/* show actions             */
		iSkipWildFlag   = FALSE,        /* skip for wildcard        */
		iSNewWildFlag	= FALSE,	/* skip with other wildcard */
		iPreviewFlag	= FALSE;	/* preview next entry       */

static int      iNewRun         = TRUE;         /* a new run after FileFind */
                                                /* returned 0               */
/****************************************************************************
BF
F * Function    : FileFind
  *
D * Description : Search for a filename. Search starts at a given directory
D *               and searches all subdirectorys for a file to find.
D *               The * wildcard can be passed to this subroutine.
D *               The last found Pattern is buffert in cpStartDir and
D *               cpSearchPattern. Where cpStartDir is the directory where
D *               the cpSearchPattern is found in.        
  * 
C * Call        : FileFind( cpStartDir, cpSearchPattern, cpWhatSearchFlag )
  *
P * Parameter   : char *cpStartDir         - Start directory
P *               char *cpSearchPattern    - File wildcard string
P *		  char *cpWhatSearchFlag   - kind of search flag
P *					     <r|R> recursive search down the
P *						   directory tree.
P *					     <b|B> store the found files in a
P *				 	           AVL tree.
P *					     <u|U> step upward while reading
P *						   the next entry in tree.
P *                                          <d|D> step downward while reading
P *                                                the tree.
P *                                          <f|F> free the search tree and 
P *       	                                   init the FileFind function.
P *					     <j|J> join path and filename in
P *                                                one string.
P *                                          <+>   skip up one record
P *                                          <->   skip down one record
P *                                          <#>   skip for wildcard
P *                                          parameters : NULL,NULL,NULL    	
P *                                                a rewind will happen. 
P *                                          <&>   skip from last positon with
P *                                                a new wildcard
P *                                          <p|P> preview with wildcard  
  *
E * Error Code  : OPEN_ERROR	(-1)
E *		  CLOSE_ERROR	(-2)
E *               STATUS_ERROR	(-3) 
E * 		  MALLOC_ERROR	(-4)
E *               FLAG_ERROR	(-5)
E *               DOUBLE_NODE	(-6)
E *               EMPTY_TREE	(-7)
E *               BUFFER_ERROR 	(-8)
  *
R * Return      : int FOUND     (1)
R *                   NOT_FOUND (0)
R *		      or Error Codes	  
EF
 ****************************************************************************/
/* ======================================================================== */
int	FileFind( char *cpStartDir, 
                  char *cpSearchPattern, 
                  char *cpWhatSearchFlag )
/* ======================================================================== */
{
int	FileFindEntrys( char*, char*, char* ),
        iFindCode = -1;



	/* ================================= */
	/*  rewind the tree                  */
	/* ================================= */
	if( !cpStartDir && !cpSearchPattern && !cpWhatSearchFlag )
	{
		lKeyPos   = 0L;                 /* rewind file tree         */
		iNewRun   = TRUE;
		return( FOUND );
	}
	/* ================================= */
	/*  test if flags are correct        */
	/* ================================= */
	if( !SetSearchFlags(cpWhatSearchFlag) )
	{
		PERROR( NULL, "FLAG ERROR" );
		return( FLAG_ERROR );
	}
	/* ============================================= */
	/* act on path and file buffer with +|-|f flags  */
	/* ============================================= */
	if( iSkipDownFlag || iSkipUpFlag || iNameFreeFlag )
	{
		return( BufferFilePath(cpSearchPattern,cpStartDir) );	
	}
	/* ============================================= */
	/* search in the tree for more times with # flag */
        /* ============================================= */
	else if( iSkipWildFlag || iSNewWildFlag || iPreviewFlag ) 
	{

	int	          iBufReturn = 1;         /* > 0 if found             */
	static char       caSearchPatOld[BUFLEN]; /* buffer search pattern    */
                                                  /* until a new run with a   */
                                                  /* new pattern comes        */
	static long lKeyPosSave;                  /* save old position        */
                                                
		if( iNewRun )                     /* start first search or    */
                {                                 /* the last return was zero */           
			lKeyPos   = 0L;           /* rewind file tree         */
			strcpy( caSearchPatOld, cpSearchPattern );
		}
		if( iSNewWildFlag )               /* continue the search with */
                {                                 /* a new wildcard           */           
			strcpy( caSearchPatOld, cpSearchPattern );
		}
		if( iPreviewFlag )                /* preview for net entry    */
		{
			lKeyPosSave = lKeyPos;
		}
		
		if( iVerboseFlag ) 
			fprintf( stderr, "\n#\tSearching for Pattern [%s]", 
					                          caSearchPatOld );
		iSkipUpFlag = TRUE;               /* skip flag for stepping   */
                /* ===================================================== */
                /* skip down the tree while end of tree or found pattern */ 
                /* ===================================================== */
		while( (iBufReturn = BufferFilePath(cpSearchPattern,
                                                    cpStartDir))     > 0 )
		{
			if( IsAPatternFile(cpSearchPattern, caSearchPatOld) )
			{
				iBufReturn = FOUND;
				break;
			}	
		}

		if( iBufReturn > 0 )
			iNewRun = FALSE;       /* a pattern was found         */ 
		else                           /* continue the search         */ 
			iNewRun = TRUE;        /* force a rewind and new run  */

		iSkipUpFlag = FALSE;           /* disable skip flag for other */
                                               /* operationes                 */
		if( iPreviewFlag )             /* preview for net entry       */
		{
			lKeyPos = lKeyPosSave;
		}		
		return( iBufReturn ); 
	}
	else   	/* build a new file and path tree */
	{
		/* ========================================= */
		/* search the given path and wildcard        */
		/* ========================================= */
		iFindCode = FileFindEntrys( cpStartDir, 
                                    cpSearchPattern, 
                                    cpWhatSearchFlag );
		if( iFindCode > 0 )
			iFindCode = iCount;
		/* ============================ */
		/* act on path and file buffer  */
        	/* with d|u flags               */
		/* ============================ */			
		if( iNameDownFlag || iNameUpFlag )
		{
			iNameJoinFlag  = 
			iBufferStrFlag = FALSE;
			if( BufferFilePath(cpSearchPattern,
                                         cpStartDir) < 0 )
				return( BUFFER_ERROR );

		}
		return( iFindCode );
	}
}

/* ======================================================================== */
int	FileFindEntrys( char *cpStartDir, 
                        char *cpSearchPattern, 
                        char *cpWhatSearchFlag )
/* ======================================================================== */
{
int		iFound = NOT_FOUND,
		iIsDirReturn,
		iBufferReturn,
		iFileCount,
		BufferFilePath( char*, char* ),
		SetSearchFlags( char* );
DIR		*dirPtr;          
struct dirent	*dirDirEntry;
char		cpNewDirFound[253];
		
	if( dirPtr = opendir( cpStartDir ) )
	{
               for( dirDirEntry = readdir( dirPtr );
		    dirDirEntry != NULL; 
		    dirDirEntry = readdir( dirPtr ) )
		{
			if( ( iIsDirReturn = TestIfDir( dirDirEntry->d_name,
							cpStartDir,
						        cpNewDirFound )) > 0 )
			{
                                /* ========================= */
                                /* step down next directory  */
                                /* ========================= */
				if( iRecursiveFlag )  
				{
					if( iFileCount = 
						   FileFindEntrys(cpNewDirFound,
				                                  cpSearchPattern,
							          cpWhatSearchFlag) )
						if( iFileCount >= 0 )
							iFound += iFileCount;
						else
							return( iFileCount );
				}
			}			
			else if( iIsDirReturn == 0 )
			{ 
				/* =================================== */
				/*  test if wildcard in filename       */
				/* =================================== */
				if( IsAPatternFile(dirDirEntry->d_name, 
						   cpSearchPattern)    )
				{
					iFound++;
					iBufferReturn = BufferFilePath(dirDirEntry->d_name,
                                                                       cpStartDir          );
					if( iBufferReturn < 0 )
						return( BUFFER_ERROR );
				}
			}
			else
				return( iIsDirReturn );
		}
               	if( closedir(dirPtr) )
		{
			PERROR( NULL, "CLOSEDIR ERROR" );
			return( CLOSE_ERROR );
		}
		else
		{
			return( iFound );
		}
	}
	else
	{
		PERROR( cpStartDir, "OPENDIR ERROR");
		return( OPEN_ERROR );
	}
}

/* ======================================================================== */
/* initialization of find file flags                                        */
/* ======================================================================== */
int	SetSearchFlags( char *cpFlagStr )                /* sub of FileFind */
/* ======================================================================== */
{
	if( !cpFlagStr || strlen(cpFlagStr) == 0 )
		return( FALSE);
	else
	{ 
		iRecursiveFlag 	= 
		iBufferStrFlag 	= 
		iNameDownFlag 	=	 
		iNameFreeFlag 	=
		iNameUpFlag	=
		iNameJoinFlag	= 
		iSkipUpFlag	=
		iSkipDownFlag	=
		iSNewWildFlag	=  
		iSkipWildFlag   = 
		iPreviewFlag	= FALSE; 

		while( *cpFlagStr )
		{
			switch( *cpFlagStr )
			{
				case ' ' :  	/* skip blanks              */  
					break;
				case 'p' :	/* preview next entry       */
				case 'P' : 	
					iPreviewFlag 	= TRUE;
					break;
   
				case 'r' :	/* recursive search down    */
				case 'R' : 	
					iRecursiveFlag 	= TRUE;
					break;
				case 'b' :	/* save the file names      */
				case 'B' :	
					iBufferStrFlag 	= TRUE;
					break;
				case 'u' :	/* skip downward            */
				case 'U' :	
					iNameDownFlag	= TRUE;
					break;
				case 'd' :	/* ship upward              */
				case 'D' :	
					iNameUpFlag	= TRUE;
					break;
				case 'f' :	/* free the tree            */
				case 'F' :	
					iNameFreeFlag	= TRUE;
					break;
				case 'j' :	/* join path and filename   */
				case 'J' :	
					iNameJoinFlag	= TRUE;
					break;
				case '-' :	/* skip down flag           */
					iSkipDownFlag	= TRUE;
					break;
				case '+' :	/* skip up flag             */
					iSkipUpFlag	= TRUE;
					break;
				case 'v' :	/* verbose flag             */
				case 'V' :
					iVerboseFlag    = TRUE;
					break;
				case '#' :	/* skip up flag             */
					iSkipWildFlag   = TRUE;
					break;
				case '&' :	/* continue with a new      */
                                                /* wildcard                 */          
					iSNewWildFlag	= TRUE;
					break;
				default :
					PERROR( "", "NO SUCH FLAG ERROR" );
					exit( 1 );
			}
			cpFlagStr++;
		}
		return( TRUE );			
	}
}
/* ======================================================================== */
/* Compare function for inserting new file and path in the avl tree         */
/* ======================================================================== */
int MemStrCmp( LEAF* LpLeafK, LEAF* LpLeafNok )            /* sub of FileFind */
/* ======================================================================== */
{
#ifdef COMSERV
extern int ComservCmp( char*, char* );
	
	return( ComservCmp((char*)LpLeafK->cpFileName,
	                   (char*)LpLeafNok->cpFileName) );
#else
	return( strcmp((char*)LpLeafK->cpFileName,
                       (char*)LpLeafNok->cpFileName) );
#endif
}
/* ======================================================================== */
/* Compare function for inserting new key in the avl tree after buffering   */
/* all names and pathes in the avl tree                                     */
/* ======================================================================== */
int MemKeyCmp( LEAF* LpLeafKin, LEAF* LpLeafK )            /* sub of FileFind */
/* ======================================================================== */
{
	return( LpLeafKin->lKey - LpLeafK->lKey );
}

/* ======================================================================== */
/* insert a member in the avl tree                                          */ 
/* ======================================================================== */
int InsertAvlMember( int (*pfFkt)(),                     /* sub of FileFind */                      
                     char *cpPath,
                     char *cpFile,
		     long lKeyPos )
/* ======================================================================== */
{
char	    caHelpBuf[BUFLEN];

		if( !(LLeaf1 = AvlTreeAlloc(sizeof(LEAF))) )
		{
			PERROR( "", "MALLOC_ERROR" );
			return( MALLOC_ERROR );
		}
		else
		{
			if( cpPath && cpFile )
			{
				if( iNameJoinFlag )
				{
					strcpy( caHelpBuf, cpPath );
					strcat( caHelpBuf, "/"); 				
					strcat( caHelpBuf, cpFile ); 
					strcpy( LLeaf1->cpFileName, caHelpBuf );
				}
				else
					strcpy( LLeaf1->cpFileName, cpFile );
				strcpy( LLeaf1->cpPathName, cpPath );
				LLeaf1->lKey = lKeyPos;
			}
			 				
			if( iBufferStrFlag )
				LLeaf2 = AvlInsert(&root, LLeaf1, pfFkt);
			else
				LLeaf2 = AvlInsert(&rootSort, LLeaf1, pfFkt);

			if( LLeaf2 )
			{
				PERROR( (char*)LLeaf1->cpFileName, "DOUBLE NODE" );
				AvlTreeFree( LLeaf1 );
				return( DOUBLE_NODE );
			}
		}
		return( FOUND );
}

/* ======================================================================== */
/* handle the flags set for working with the avl tree                       */
/* ======================================================================== */
int BufferFilePath( char *cpFile, char *cpPath )         /* sub of FileFind */
/* ======================================================================== */
{
int         MemStrCmp      ( LEAF*, LEAF*),
            MemKeyCmp      ( LEAF*, LEAF*),
            InsertAvlMember( int(*)(), char*, char*, long ),
            iCode     = 0; 
char	    caFile[BUFLEN],
            caPath[BUFLEN];
LEAF        LpTemp; 

	if( iBufferStrFlag )                /* buffer the found filenames   */  
	{
		lKeyPos = 0L;
		if( InsertAvlMember(MemStrCmp,
                                    cpPath,
                                    cpFile,
                                    lKeyPos) == DOUBLE_NODE )
			return( DOUBLE_NODE ); 
		if( iVerboseFlag ) 
			fprintf( stderr, "\n#\t[%s] buffert !",cpFile  );
	}
	else if( iNameFreeFlag )          /* free all malloc memory of root */  
	{
		AvlFreeAll( &rootSort );
	}
	else if( iSkipUpFlag || iSkipDownFlag )            
	{                                 /* skip up or down                */
		if( iSkipDownFlag )       /* skip down one record           */
			lKeyPos--;			
		if( iSkipUpFlag )         /* skip up one record             */
			lKeyPos++;
		LpTemp.lKey = lKeyPos;
		if( LLeaf1 = AvlFind( (TREE*)rootSort,
                                      (LEAF*)&LpTemp,
                                      MemKeyCmp) )
		{
			strcpy( cpPath, LLeaf1->cpPathName );
			strcpy( cpFile, LLeaf1->cpFileName );
		}
		else
		{
			strcpy( cpFile, "" );
			strcpy( cpPath, "" );
			return( NOT_FOUND );
		}
		
	}
	else if( iNameDownFlag || iNameUpFlag )
        {                                 /* 
					  /* sort upwards or downwards      */        
		if( !root )
		{
			PERROR( "", "EMPTY TREE" );
			return( EMPTY_TREE );
		}
		lKeyPos = 0L;
	        while( root )
		{ 
			if( iNameDownFlag )
				LLeaf1 = AvlMax( (TREE*)root );	
		
			if( iNameUpFlag )
				LLeaf1 = AvlMin( (TREE*)root );

			if( LLeaf1 )
			{
				lKeyPos++;
				strcpy( caPath, LLeaf1->cpPathName );
				strcpy( caFile, LLeaf1->cpFileName );
				if( iCode = AvlDelete(&root, 
                                                        LLeaf1,
                                                        MemStrCmp) )
				{
				/*	AvlFreeAll( &root );*/

				}
				if( InsertAvlMember(MemKeyCmp,
                                    		    caPath,
                                    		    caFile,
                                                    lKeyPos) == DOUBLE_NODE )
					return( DOUBLE_NODE ); 
 			}
		}
		lKeyPos = 0;
	}
	else
	{
		PERROR( "", "NO FLAG IN COMMAND" );
		exit(1);
	}
	return( FOUND );
}  

/****************************************************************************
BF
F * Function    : TestIfDir
  *
D * Description : Test if a given directory entry is a subdirectory.
  * 
C * Call        :TestIfDir( cpDirNameToTest,
C *                         cpStartDir,
C *                         cpNewPathName )  
  *
P * Parameter   : char	*cpDirNameToTest	- Direntry to test
P *               char	*cpStartDir		- Current directory
P *               char  cpNewPathName[]		- New path, testDir+currentDir    
  *
E * Error Code  : STATUS_ERROR	(-3) 
E * 		  MALLOC_ERROR	(-4)	
  *
R * Return      : int FOUND     (1)
R *                   NOT_FOUND (0)
R *		      or Error Codes	
EF
 ****************************************************************************/
/* ======================================================================== */
int 	TestIfDir( char *cpDirNameToTest,
                   char *cpStartDir,
                   char cpNewPathName[] )
/* ======================================================================== */
{
struct stat     stStatusBuffer;

	if( *cpDirNameToTest == '.' )
		return( NOT_FOUND );
	
	strcpy( cpNewPathName, cpStartDir );
	strcat( cpNewPathName, "/"); 
	strcat( cpNewPathName, cpDirNameToTest);
	
	if( !stat( cpNewPathName, &stStatusBuffer ) )
	{
		if( S_ISDIR( (mode_t)stStatusBuffer.st_mode ) )
		{
			if( iVerboseFlag ) 
				fprintf( stderr, "\n Directory [%s] found !",
								cpNewPathName );
			return( FOUND );
	}
		else
			return( NOT_FOUND );
	}		
	else
	{
		PERROR( cpNewPathName, "STATUS ERROR" );
		return( STATUS_ERROR );
	}

}

/****************************************************************************
BF
F * Function    : IsAPatternFile 
  *
D * Description : Compare two name string with the * wildcard.
  * 
C * Call        : IsAPatternFile( cpFileNameRead , cpSearchPattern )  
  *
P * Parameter   : char	*cpFileNameRead		- A given name string
P *		  char	*cpSearchPattern	- Wildcard pattern string
  *
E * Error Code  : none
  *
R * Return      : int FOUND     (1)
R *                   NOT_FOUND (0)
EF
 ****************************************************************************/
/* ======================================================================== */
int	IsAPatternFile( char *cpFileNameRead , char *cpSearchPattern )
/* ======================================================================== */
{
extern int	StrMatch( char*, char* );

	/* dot files are not forced */
	if( !strcmp(cpFileNameRead, ".") || !strcmp(cpFileNameRead, "..") )
		return( NOT_FOUND );

		if( iVerboseFlag ) 
			fprintf( stderr, "\n##\t\tFile        [%s] compare\n##\t\tfor Pattern [%s]", 
					       		           cpFileNameRead, cpSearchPattern );
	if( StrMatch( cpFileNameRead, cpSearchPattern )
	    /*WildCardStrCmp( cpFileNameRead, cpSearchPattern ) 
								*/ )
	{
		if( iVerboseFlag ) 
			fprintf( stderr, "\n###\t\t\tFile        [%s] found\n###\t\t\tfor Pattern [%s]", 
					                             cpFileNameRead, cpSearchPattern );
		iCount++;
		return( FOUND ); 
	}
	else
		return( NOT_FOUND );
}

/****************************************************************************
BF
F * Function    : WildCardStrCmp 
  *
D * Description : Wildcard pattern matching. Compares two strings against.
D *               * wildcard is allowed. 
  * 
C * Call        : WildCardStrCmp( cpPatternStr, cpMatchStr ) 
  *
P * Parameter   : char	*cpPatternStr
P *		  char	*cpMatchStr 
  *
E * Error Code  : none
  *
R * Return      : int FOUND     (1)
R *                   NOT_FOUND (0)
EF
 ****************************************************************************/
/* ======================================================================== */
int	WildCardStrCmp( char *cpPatternStr, char *cpMatchStr )	
/* ======================================================================== */
{
#define	STRLEN		(64)
#define	DELIMSTR	"*"
#define	DELIMCHR	'*'
char		*cpToken = NULL,
		*cpSubPattern = NULL,
		cpHelpStr[STRLEN],
		cpHelpMatch[STRLEN],
		cpHelpToken[STRLEN];
int		LastSubString ( char*, char* ),
		FirstSubString( char*, char* );

	strcpy( cpHelpStr, cpPatternStr );
	cpToken = OStrTok( cpMatchStr, DELIMSTR );
			
	if( !cpToken )
		return( NOT_FOUND );
	else
	{
		if( (*cpToken == DELIMCHR ) &&		/*  Match "*"       */
		    ( strlen(cpToken) == 1 ) )
		{
			return( FOUND );
		} 
		else if( (*cpToken == DELIMCHR ) &&	/*  Match "*-"      */
			 ( *(cpToken+strlen(cpToken)-1) != DELIMCHR ) )
		     {
			if( LastSubString( cpToken+1, cpHelpStr ) )
				return( FOUND );
			else
				return( NOT_FOUND ); 
		     }
		else if( (*cpToken != DELIMCHR ) && 	  /* Match "-"      */
			 ( *(cpToken+strlen(cpToken)-1) != DELIMCHR ) )
		     {
			if( FirstSubString( cpToken, cpHelpStr ) )
				return( FOUND );
			else
				return( NOT_FOUND );
		     } 
		else if( (*cpToken != DELIMCHR ) && 	  /* Match "-*"     */
			 ( *(cpToken+strlen(cpToken)-1) == DELIMCHR ) )
		     {
			if( FirstSubString( cpToken, cpHelpStr ) )
			{
				strcpy( cpHelpMatch,
			 	        cpMatchStr + strlen(cpToken)-1 );
				return( WildCardStrCmp( 
						&cpHelpStr[strlen(cpToken)-2],
						cpHelpMatch) );
			}
			else
				return( NOT_FOUND );
		     } 	
		else if( (*cpToken == DELIMCHR ) && 	  /* Match "*-*"    */
			 ( *(cpToken+strlen(cpToken)-1) == DELIMCHR ) )
		     {
			strncpy( cpHelpToken, cpToken+1, strlen(cpToken)-2 );
			*(cpHelpToken+strlen(cpToken)-2) = '\0'; 
		      	if( cpSubPattern = strstr( cpHelpStr, cpHelpToken ) )
			{
				strcpy( cpHelpStr,
			 	        cpSubPattern + strlen(cpHelpToken) );
				strcpy( cpHelpMatch,
			 	        cpMatchStr + strlen(cpToken)-1 );
				return( WildCardStrCmp( cpHelpStr, cpHelpMatch) );
			}
			else 
				return( NOT_FOUND );
		     }  	 		 
	}

}
/* ======================================================================== */
int	LastSubString( char *cpTokenSub, char *cpHelpSub )
/* ======================================================================== */
{
	if( strlen(cpTokenSub) <= strlen(cpHelpSub) )
	{
		if( strcmp(&cpHelpSub[strlen(cpHelpSub)-strlen(cpTokenSub)],
			   cpTokenSub) )
			return( NOT_FOUND );
		else
			return( FOUND );
	}
	else
		return( NOT_FOUND );
}
/* ======================================================================== */
int	FirstSubString( char *cpToken, char *cpHelpStr )
/* ======================================================================== */
{
	if( (strlen(cpToken)-1) <= strlen(cpHelpStr) )
	{
		if( strncmp( cpHelpStr, cpToken, strlen(cpToken)-1 ) )
			return( NOT_FOUND );
		else
			return( FOUND );
	}
	else
		return( NOT_FOUND );
}

/****************************************************************************
BF
F * Function    : OStrTok 
  *
D * Description : extract a token from a scanstring. The token are separated
D *		  with characters in the separater string. The funktion 
D *               returns the token with a leadind and trailing separator
D *               character, if possible. 
  * 
C * Call        : OStrTok( cpScanString, cpSeperator )  
  *
P * Parameter   : char	*cpScanString	- String to scan
P *		  char	*cpSeperator	- Chars wich separate the string token 
  *
E * Error Code  : none 
  *
R * Return      : char*	- the extracted token
R *		  NULL	- no token found
EF
 ****************************************************************************/
/* ======================================================================== */
char 	*OStrTok( char *cpScanString, char *cpSeperator )
/* ======================================================================== */
{
int	        iPosition, iHelp = 0;
int             iNotFirst = 0; 
static char	*caHoldScanString;
char		caTempBuffer[STRLEN];

	for( iPosition = 0 ; iPosition < STRLEN ; iPosition++ )
		caTempBuffer[iPosition] = '\0';
   
	if( cpScanString )
		caHoldScanString = cpScanString;

	while( *caHoldScanString != '\0' )
	{
		for( iPosition = strlen( cpSeperator )-1 ;
                     iPosition >= 0 ;
                     iPosition-- )
		{			

			if( *caHoldScanString == *(cpSeperator+iPosition) )
			{
				if( (strlen(caTempBuffer) && iNotFirst ) ||
				    (strlen(caTempBuffer) &&
			            caTempBuffer[0] != *(cpSeperator+iPosition)))

				{
					caTempBuffer[iHelp++] = 
						*(cpSeperator+iPosition);
					caTempBuffer[iHelp] = '\0';
					return( strdup((char*)caTempBuffer) );
				}
				iNotFirst++;
			}
		}
		caTempBuffer[iHelp++] = *caHoldScanString;
		caTempBuffer[iHelp] = '\0';
		caHoldScanString++;
	}
	if( strlen(caTempBuffer) )
		return( strdup((char*)caTempBuffer) );
	else
		return( NULL );
}
					
/* ######################################################################## */
/* various tests
/* ######################################################################## */
/*
#define TEST_FILE_FIND_MAIN
*/

#ifdef TEST_FILE_FIND_MAIN
int	main( int argc, char *argv[] )
{
char	caDir[256],
	caFile[256];
int	iRet;

	iRet = FileFind( argv[1], argv[2], argv[3] );
	fprintf( stderr, "\n\nFileFind returns code [%d]\n\n", iRet );

	strcpy( caDir, argv[1] );
	strcpy( caFile, argv[2] );
 	while( FileFind(caDir, caFile, argv[4]) > 0 ) 
	{
		fprintf( stderr, "\n\tP[%s]##F[%s]",caDir, caFile );
	}

	fprintf( stderr, "\nFinish returned[%d]",FileFind( NULL, NULL, "f" ));
	fprintf( stderr, "\n\n" );
	exit( 0 );	
}
#endif
/* ======================================================================== */
#ifdef TEST_WILDCARD_FIND 
int	main( int argc, char *argv[] )
{

	fprintf( stderr, "\nFileWildcard code must be 1 [%d]\n",
			     WildCardStrCmp( "testfall", "*" ) ); 

	fprintf( stderr, "\nFileWildcard code must be 1 [%d]\n",
			     WildCardStrCmp( "testfall", "*ll" ) ); 

	fprintf( stderr, "\nFileWildcard code must be 1 [%d]\n",
			     WildCardStrCmp( "testfall", "*st*al*" ) ); 

	fprintf( stderr, "\nFileWildcard code must be 1 [%d]\n",
			     WildCardStrCmp( "testfall", "t*st*al*l" ) ); 

	fprintf( stderr, "\nFileWildcard code must be 1 [%d]\n",
			     WildCardStrCmp( "testfall", "t*" ) ); 

	fprintf( stderr, "\nFileWildcard code must be 1 [%d]\n",
			     WildCardStrCmp( "testfall", "testfall" ) ); 

	fprintf( stderr, "\nFileWildcard code must be 0 [%d]\n",
			     WildCardStrCmp( "testfall", "*es*v*" ) ); 

	fprintf( stderr, "\nFileWildcard code must be 0 [%d]\n",
			     WildCardStrCmp( "testfall", "fehler" ) ); 

	fprintf( stderr, "\nFileWildcard code must be 0 [%d]\n",
			     WildCardStrCmp( "testfall", "" ) ); 


	exit( 0 );	
}
#endif
/* ######################################################################## */
