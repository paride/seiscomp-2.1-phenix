#include <stdio.h>
#include <unistd.h>
#include <string.h>

#define TRUE  (1)
#define FALSE (0)
extern int iJoinedPath; /* to select active files */

int SplitStr( char*, int, char, char* );

int ComservCmp( char *cpFirst, char *cpSecond ) {
extern int StrMatch( char*, char* );
char caFirstStat[5],
     caFirstDay[4],
     caSecStat[5],
     caSecDay[4];
     
     if( iJoinedPath == TRUE ) {
	return(strcmp(cpFirst,cpSecond));
     }

     if( StrMatch(cpFirst, "_D__*") )
        SplitStr( &cpFirst[4], 3, '.', caFirstStat );
     else
        SplitStr( cpFirst, 3, '.', caFirstStat );
     SplitStr( cpFirst, 6, '.', caFirstDay );
     
     if( StrMatch(cpSecond, "_D__*") )
        SplitStr( &cpSecond[4], 3, '.', caSecStat );
     else
        SplitStr( cpSecond, 3, '.', caSecStat );
     SplitStr( cpSecond, 6, '.', caSecDay );
     
     if( atoi(caFirstDay) != atoi(caSecDay) )
        return( atoi(caFirstDay) - atoi(caSecDay) );
     else 
        return( strcmp(cpFirst, cpSecond) );
}

int SplitStr( char *cpSplit, int iPos, char cTok, char *cpBack ) {

int  iCount,i;
char *cpStart, caWord[128];

    iCount = 1, i = 0;
    strcpy( caWord, cpSplit );
    cpStart = &caWord[0];
    
    
    while( caWord[i] != '\0' ) {
        if( caWord[i] == cTok ) {
            caWord[i] = '\0';
            
            if( iCount == iPos ) {
                strcpy( cpBack, cpStart );
                return( 1 );
                break;
            } 
            iCount++;
            cpStart = &caWord[i+1];           
        }
        i++;
    }
 
    return( 0 );   
}

/*
int main( int argc, char** argv ) {

    printf( "\ndiff %d\n", ComservCmp("_D__RUE.GE.VHE.D.1997.108.1420"
    ,"_D__RUE.GE.VHE.D.1997.112.0043" ) );

    printf( "\ndiff %d\n", ComservCmp("_D__RUE.GE.VHE.D.1997.108.1420"
    ,"_D__RUE.GE.VHE.D.1997.108.1420" ) );
    
    printf( "\ndiff %d\n", ComservCmp("_D__RUE.GE.VHB.D.1997.108.1420"
    ,"_D__RUE.GE.VHE.D.1997.108.1420" ) );

    printf( "\ndiff %d\n", ComservCmp("_D__RUE.GE.VHE.D.1997.109.1420"
    ,"_D__RUE.GE.VHE.D.1997.108.1420" ) );
}
*/   
 
           
     
     
