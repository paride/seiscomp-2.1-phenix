/****************************************************************************
BM         
M* Module      : LsFilesDirs.c
 * 
D* Description : List all files with its size from a start directory. 
 * 
I* Import      :
 *
E* Export      : 
 *
P* Author      : Wilhelm Ruesing
 *
U* Updates     :
EM 
 ****************************************************************************/
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>

#define	DIR_ENTRY	(0)
#define	FILE_ENTRY	(1)
#define	FAIL		(2)

void PrintEntry	( char* );
int  TestIfDir	( char*, char* );
void SelectFiles( char* );

FILE	*fpOutput;

/* ======================================================================== */
int main(int argc, char **argv )
/* ======================================================================== */
{
	if( argc < 2 || argc > 3 )
	{
		fprintf( stderr, "\nusage : FileSize <StartPath> [OutFile]\n\n" );
		exit(-1);
	}
	else if( argc == 2 )
	{
		fpOutput = stdout;
	}
	else
	{
		if( !(fpOutput = fopen(argv[2], "w")) )
		{
			perror( "fopen ERROR" );
			exit(-1);
		}
		fprintf( stderr, "\nscanning for files in %s.\n",argv[1] );
	}

	SelectFiles( argv[1] );
	fclose( fpOutput );
	exit(0); 
}

/* ======================================================================== */
void PrintEntry( char *cpFilePath )
/* ======================================================================== */
{
struct stat 	spInfoBuf;
int iStatRtn;

	if( stat(cpFilePath, &spInfoBuf) )
	{
		fprintf( fpOutput, "\nCan't get size of %s !\n", cpFilePath );
		perror( "stat ERROR" );
		exit(-1);
	}
	else
		fprintf( fpOutput, "%d#%s\n",spInfoBuf.st_size, cpFilePath );
}

/* ======================================================================== */
int TestIfDir( char *cpDirPath, char *cpDirNameToTest )
/* ======================================================================== */
{
struct stat     stStatusBuffer;

	if( *cpDirNameToTest == '.' )
		return( FAIL );
	
	if( !stat( cpDirPath, &stStatusBuffer ) )
	{
		if( S_ISDIR( (mode_t)stStatusBuffer.st_mode ) )
			return( DIR_ENTRY );
		else
			return( FILE_ENTRY );
	}		
	perror( "stat ERROR" );
	exit(-1);
}

/* ======================================================================== */
void SelectFiles( char *cpStartDir )
/* ======================================================================== */
{
int	iIsDirReturn,
	iBufferReturn,
	iFileCount;
DIR	*dirPtr;
char	caNowFileName[257];
          
struct dirent	*dirDirEntry;
		
	if( dirPtr = opendir( cpStartDir ) )
	{
		for(	dirDirEntry = readdir( dirPtr );
			dirDirEntry != NULL; 
			dirDirEntry = readdir( dirPtr ) )
		{
			sprintf( caNowFileName,"%s/%s", cpStartDir,
					   	  dirDirEntry->d_name );
			if( (iIsDirReturn = TestIfDir(caNowFileName,
					              dirDirEntry->d_name )) 
								== DIR_ENTRY )
				SelectFiles( caNowFileName );
			else if( iIsDirReturn == FILE_ENTRY )
				PrintEntry( caNowFileName );
		}
               	if( closedir(dirPtr) )
		{
			perror( "closedir ERROR" );
			exit(-1);
		}
		return;

	}
	perror("opendir ERROR");
	exit(-1);
}
	
