#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>


/*   ********************************************************************
   SIFIL COMPUTES THE OUTPUT OF A 2-CASCADE RECURSIVE FILTER FOR THE
   SIMULATION OF A SEISMOMETER-GALVANOMETER-SYSTEM RESP. THE OUTPUT
   OF A NBAND-CASCADE 2-POLE-BUTTERWORTH-FILTER
   BY D.SEIDL AND W.HANKA
   ********************************************************************  */

void sifil(int LSEC, float DA[], int KS,float W1[],float W2[],int K,int NREC,int NBAND)
{
static float DAM[3][4][30], DAFM[3][4][30];
float W11,W12,W13,W14,W15,W21,W22,W23,
      DAS1,DAS2,DAS3,DAS4,DAFS1,DAFS2,DAFS3,DAFS4,DAFG,DAFG1,DAFG2,DAFS;
int   J,N;

/*
fprintf( stderr, "\nw1 %f %f %f %f %f",W1[0],W1[1],W1[2],W1[3],W1[5] );
fprintf( stderr, "\nw1 %f %f %f",W2[0],W2[1],W2[2] );
fprintf( stderr, "\nK %d NREC %d NBAND %d", K,NREC,NBAND );
*/

      if(K > 30) 
      	return;
	
      if(NBAND > 3) 
      	NBAND=2;
      else
        NBAND--;
	
      if(NREC <= 1) {
      	for( J=0; J< 4; J++) {
      		for( N=0; N < 3; N++) {
      			DAM[N][J][K]=0.0;
      			DAFM[N][J][K]=0.0;
        	}
      	}
      }
          
      W11=W1[0];
      W12=W1[1];
      W13=W1[2];
      W14=W1[3];
      W15=W1[4];
      W21=W2[0];
      W22=W2[1];
      W23=W2[2];

      if(NBAND < 1) {

      	DAS1=DAM[0][0][K];
      	DAS2=DAM[0][1][K];
      	DAFS1=DAFM[0][0][K];
      	DAFS2=DAFM[0][1][K];
      	DAFG1=DAFM[1][0][K];
      	DAFG2=DAFM[1][1][K];

      	for( J=0; J < LSEC; J++ ) {
      		DAFS=W12*DAS1;
      		DAFS=DAFS+W13*DAS2;
      		DAS2=DAS1;
      		DAS1=DA[J];
      		DAFS=DAFS+W11*DAS1;
      		DAFS=DAFS-W14*DAFS1;
      		DAFS=DAFS-W15*DAFS2;
     		DAFG=DAFS+DAFS1+DAFS1+DAFS2;
       	 	DAFG=W21*DAFG;
      		DAFG=DAFG-W22*DAFG1;
     		DA[J]=DAFG-W23*DAFG2;
     		DAFS2=DAFS1;
     		DAFS1=DAFS;
     		DAFG2=DAFG1;
		DAFG1=DA[J];
      	}      
      	DAM[0][0][K]=DAS1;
      	DAM[0][1][K]=DAS2;
      	DAFM[0][0][K]=DAFS1;
      	DAFM[0][1][K]=DAFS2;
      	DAFM[1][0][K]=DAFG1;
      	DAFM[1][1][K]=DAFG2;
      	return;
     } else {
      	for( N=0; N < NBAND; N++ ) {
      		DAS1=DAM[N][0][K];
      		DAS2=DAM[N][1][K];
      		DAS3=DAM[N][2][K];
      		DAS4=DAM[N][3][K];
      		DAFS1=DAFM[N][0][K];
      		DAFS2=DAFM[N][1][K];
      		DAFS3=DAFM[N][2][K];
      		DAFS4=DAFM[N][3][K]; 
      		for( J=0; J < LSEC; J++ ) {
      			DAFS=DAS4-2.*DAS2;
      			DAS4=DAS3;
      			DAS3=DAS2;
      			DAS2=DAS1;
      			DAS1=DA[J];
      			DAFS=DAS1+DAFS;
      			DAFS=W11*DAFS;
      			DAFS=DAFS-W12*DAFS1;
      			DAFS=DAFS-W13*DAFS2;
      			DAFS=DAFS-W14*DAFS3;
      			DAFS=DAFS-W15*DAFS4;
      			DAFS4=DAFS3;
      			DAFS3=DAFS2;
      			DAFS2=DAFS1;
      			DAFS1=DAFS;
			DA[J]=DAFS;
        	}
      		DAM[N][0][K]=DAS1;
      		DAM[N][1][K]=DAS2;
      		DAM[N][2][K]=DAS3;
     		DAM[N][3][K]=DAS4;
      		DAFM[N][0][K]=DAFS1;
      		DAFM[N][1][K]=DAFS2;
      		DAFM[N][2][K]=DAFS3;
   		DAFM[N][3][K]=DAFS4;
      	}
      	return;
     }
}
