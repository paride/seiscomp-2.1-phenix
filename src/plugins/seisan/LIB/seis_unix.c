/*        update                                                       */
/* sep 98 jh    : ---------- version 7.0 check ---------------------*/
/*                no changes                                        */
/* nov 5 98 jh  : change call so no '_' in name, does not work on linux */
/* nov 12 bmt   : add clock_ function     */                         
/* feb 19 lo    : include stdlib.h and define strcmp */
/* mat 16 jh    : undefine strcmp, does not work on sun os, rm getfil */ 
/*                not usded in any programs                           */
/* mar 18 lo    : putenvsun changed to run with gnu compilers       */
/* aug 3  jh    : put in qsort in get files again, where did it go  */
/* sep 28 bmt   : add _open,_read,_eof function*/
/* sep 29 bmt : time adjusted */
/* nov 8  bmt : change read and open name functions*/
/* dec 9  bmt : add functions for bgisei*/
/* jun 20 jh  : revised bgisei routines */


#include <curses.h>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <libgen.h>

#define maxchar 100      /* Max number of characters in a string */

typedef char string[maxchar+1];   /* String type */
        char envtext[255];




/*  Put enviroment variable on sun                */
/* changed to run with gnu compilers lo mar 99    */
/* mar 30 99 by jh   ad sort_fi                   */

    void putenvsun_(text)

     char* text;
{
/*      strcpy(envtext,text);
      putenv(envtext); */
      putenv(text);
}
/******************************************************************/

/* routine to make system call in c from fortran, is faster */
/* than doing it from fortran ...                           */
/* modified to handle any length of command string bjb 2001/02/14 */
void systemc_(text,length)
int *length; 
char *text;
{
  char *c_buffer;
  c_buffer = (char *)malloc (*length +1);
  if (! c_buffer) return;
  strncpy (c_buffer, text, *length);
  *(c_buffer + *length) = '\0';
  system(c_buffer);
}

/* - - - - - - - - - - old routine - - - - - - - - - - */
/* routine to make system call in c from fortran, is faster */
/* than doing it from fortran ...                           */
/* If $SEISARCH != "SOLARIS", then add a null to string     */
/*
void systemc_(text,length)

int *length; 
char *text;
{
char arch[20];
char command_text[241];

get_arch_(arch);                Get environment variable 
if( strcmp(arch,"SOLARIS") != 0 ) text[*length]='\0'; 

strncpy(command_text,text,*length);
command_text[*length] = '\0';
printf("\n %s \n",command_text);
system(command_text);
}
*/

/*void systemcc_(text,length)
int *length; 
char *text;
{
char arch[20];
char command_text[240];
if( strcmp(arch,"SOLARIS") != 0 ) text[*length]='\0'; 
strncpy(command_text,text,*length);
command_text[*length] = '\0';
system(command_text);
}  */

void readn_(char_ptr,n)
char *char_ptr;
long *n;
{

  char input[100];
  int i;

  for (i=0;i<*n;i++) char_ptr[i]=getchar();
  fflush(stdin);
}



/* updates */
/* 25 3 97 jh : commanr out closedir */
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <time.h>
#include <unistd.h> 


    void getfiles_(sdir,dir_length,sfiles,max_sfiles,nfiles)
/* -----------------------------------------------------------ARNE-SJURSEN-- */
/* get list of all files in directory sdir */
/* sep 98 by jh ----- version 7.0 check -------------------------------------*/
/*          no change                                                        */
/* put in qsort again *  */

  char *sdir;                  /* directory name */
  int *dir_length;             /*length of sdir */
  char  *sfiles;               /* files to get   */
  int *max_sfiles;             /* maximum number of s-files */
  int *nfiles;                 /* number of files found */


{

/*  int (*strcmp)(); */
  int i;
  int length;                   /* length of string to use */
  DIR   *dirp;                  /* directory structure */
  char *base;                    /* data base used */
  struct dirent *dp;            /* directory entry */

  sdir[*dir_length]='\0';       /* assume no null char from fortran */
  length=80;
  *nfiles=0;
  dirp = opendir(sdir);
  if (!dirp) {
/*    closedir(dirp);  */
    nfiles=0;
    return;                      /* return since no files */
  } 
/*   get files  */
  for (dp=readdir(dirp);dp!=NULL;dp=readdir(dirp)) {
    if ((*nfiles)<(*max_sfiles)){ 
        strncpy((sfiles+((*nfiles)++)*(length)),dp->d_name,length);
}
    else {
      printf("--%s-- ERROR: Out of variable space, \'%s\' will not be listed\n"
  	,"",dp->d_name);
    }
  }
/*    printf(" %d \n",*nfiles); */
 

  qsort (sfiles,*nfiles,length,strcmp); 

  closedir(dirp); 

}

clock_t clock_() {
 return clock();
}

/* updates */
/* sep 98 by jh -------------- version 7.0 check ----------------------*/
/*              no change                                              */
/* mar 19 99 lo change name to sortfi                                  */

#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <string.h>
#include <fcntl.h>      /* Needed only for _O_RDWR definition */
#include <unistd.h>
#define _O_BINARY 0

    void sortfi_(files,nfiles)
/* sort character strings 80 chars long */

  char  *files;               /* file names to sort   */
  int *nfiles;                 /* number of names */


{



  qsort (files,*nfiles,80,strcmp);


}

/* only used in linux */

int  new_open(name,attr)
char *name;    /*file name*/
int attr;     /* attribute of the file*/
{
 return open(name,attr);
}

int new_read(f, p, c) 
int f;   /* file descriptor */
void *p;  /*buffer for receiver*/
int c;   /* # of byte to read */
{
  return read(f,p,c);
}


/*convert chinees format to ascii*/

/*b. moreno   sep 28 1999*/
/*update*/
/* sep 29 bmt : time adjusted */
/* nov 8  bmt : change read and open name function*/
/* dec 9  bmt : add functions for bgisei*/
/* Jan 2003 bmt : fix bug in number of channel to be converted*/

#include <errno.h>
#include <time.h>

/*   response.h     Feb 17,1997.  */

 #ifndef _response_h
 #define _response_h
 
 #define  PAZTYPE    0
 #define  FAPTYPE    1
 #define  FIRTYPE    2
 
 typedef  struct {
         float  real,image;
         } complex;
      
 typedef  struct {
         float   freq,amp,phase;
         }  FAP;
         
 /*  response is given in poles and zeros    */
 typedef struct {
         long              type;
         long              nop;      /*  number of poles   */
         complex          *pole;
         long              noz;      /*  number of zeroes  */
         complex          *zero;
         }  RESPAZ;
         
 /*  response is given using FIR filter conefficients format  */
 typedef struct {
         long              type;
         char              name[20];  /*  the filter's identity        */
         long              noc;       /*  number of coefficients       */
         float            *coe;       /*  coefficients of the  filter  */
         } RESFIR;
         
 /*  response is given in frequency - amplitude - phase   */
 typedef struct {
         long              type;
         long              ntrip;
         FAP              *fap;
         } RESFAP;
  
 typedef union  {
         long              type;
         RESPAZ            ResPaz;
         RESFAP            ResFap;
         RESFIR            ResFir;
         }  RESPONSE;

#endif         
/*  Any system's response can be seperated to serial setps of filter,each filter can
    be given in FIR coefficients,poles and zeroes ,even frequencies,amplitude,phase 
    groups,each step multiplex together to form system's responese.
    
    In this file,serial structures is defined to describe the above process. for
    example: a sytem's response is seperated into three steps of filter,thefirst is 
    a filter given in poles and zeroes,and second is given in a FIR filter,the last is
    also given in a FIR filter.It can be described using defined strutures as below:
 
         RESPONSE    *lpResp;
       
         ...
         lpResp -> type = FAPTYPE;
         ...
         lpResp -> type = FIRTYPE;
         ...
         lpResp -> type = FIRTYPE;               
         ....         
         */

/***************************************************************************/
#ifndef _evtfile_h
#define _evtfile_h

#define UD	0
#define EW	1
#define NS	2

#define DISPLACEMENT   0     /*  three constant defined to indicate data type   */
#define VELOCITY       1
#define ACCELERATION   2

#define RADIO          0
#define LINE           1
#define RAL            2      /*  Radio and line is both used   */
#define DIAL           10     /*  Dial to get data              */
#define MAIL           11     /*  DATA MAIL                     */

#define DISP           0x00000001
#define VELO           0x00000002
#define ACCE           0x00000004
#define ROTAT          0x00000008
#define FILTR          0x00000010
#define BIASC          0x00000020
#define LINER          0x00000040

#define GPS            0
#define BPM            1    

typedef struct {
 	short	year,mon,day;
	short	hour,min,sec;
} WD_TIME;	/* struct of date & time */

typedef struct {
	char	NetName[80];
	long	stn_sum;
	float	centlat;
	float	centlon;
	float	centalt;
	} NETPAR;

typedef struct {
        double    time;         /*  YYYYMMDDhhmmss.ss   */
        float     latitude;
        float     longitude;
        float     depth;
        short     Ml,Md,Ms,Mb;   /* value is 10 times of magnitude  */
        char      LocName[12];
        } QUAKEPAR;

typedef struct {
        char      EvtFlag[16];    /* "digital event" always */ 
        char      hostype[16];    /* "PC" or "Work Station" */
        int       rectype;        /* displacement,velocity,accelaration,original data  */
        int       process;        /* precess this file underwent    */
	NETPAR    netpar;
	QUAKEPAR  quakepar[3];
	long 	  data_begin;	/* data beginning time , unit: second */
	WD_TIME   data_start;	/* data beginning time */
	long	  rec_length;	/* unit: second	*/
} EVT_HEAD;  	/*  header of event file */

typedef struct {
        char   name[12];
        int    no;
        float  factor;
        int    respstep;
        char   resp[12288];      /* reserved for storage of response data */
        } CHA_PAR;
        	
typedef struct {
	long		no;
	char		name[20];
	char            DASType[10];            /* data acqusituion system type      */
	short           wlen;                   /* word length of DAS                */
	float           VLtd;                   /* input amplitude limited voltage   */
	long		samp;			/* sampling                          */
	long		comp;			/* 1 or 3 component                  */
	short           TranMode;               /* Data tansition method.            */
	char            TranIns[12];            /* Tansition instruments type        */
	short           TimerMode;              /* Time synconizing method           */
	char            ClkType[12];            /* Clock type                        */
	float           ClkErr;                 /* clock error in ms                 */
	float		latitude;
	float		longitude;
	float		altitude;
	float           azimuth,incident;       /* if rotated,otherwise is zeroes    */
	short		weight;
	short		veloc;
	int             oritype;                /* Original data type  */
	char		seismometer[12];
} STN_PAR;	/* parameter of seismic station */

/*  event_file struct  */
/* struct{
		EVT_HEAD	evt_head;
		STN_PAR		stn_par;    Station 1's parameters
		CHA_PAR         cha_par;    channel 1 of station 1's parameters
		...
		CHA_PAR         cha_par;    channel comp of station 1's parameters
		...
		STN_PAR		stn_par;    Station sum's parameters
		CHA_PAR         cha_par;    channel 1 of station sum's parameters
		...
		CHA_PAR         cha_par;    channel comp of station sum's parameters
		DATA		
	        {
		  No.1(sec)    {long  flag;  void data[SecSize1];	 Station 1
				long  flag;  void data[SecSize2];	 Station 2
				......			......
				long  flag;  void data[SecSizeN];	 Station N}

		  No.2(sec)    {long  flag;  void data[SecSize1];	 Station 1
				long  flag;  void data[SecSize2];	 Station 2
				......			......
				long  flag;  void data[SecSizeN];	 Station N}
		  ...
		  ...
		  ...

		  No.n(sec)    {long  flag;  void data[SecSize1];	 Station 1
				long  flag;  void data[SecSize2];	 Station 2
				......			......
				long  flag;  void data[SecSizeN];	 Station N}
		}
	}	
*/

/*  Instruments response segment structure    
  
  A system's response can be devided into a few subsegments,each 
segment define a grade of response,this grade of resopnse can be
described as followings:(all the data described here is in binary
format)
  int    type;  
    if(type == PAZTYPE)               if this grade of response described
                                      with zero and ploe points.
      {
        float    factor;              A ratio factor to indicate the amplitude change 
        int      nop;                 number of poles.
        complex  pole[nop];           the poles.
        int      noz;                 number of zeroes.
        complext zero[noz];           the zeros.
      }        
    else if(type == FAPTYPE)          if this grade of response described
                                      with amplitude and phase response
      {
        float    factor;              A ratio factor to indicate the amplitude change.
        int      ntrip;               number of data groups.
        FAP      fap[ntrip];          each frequency's frequency,amplitude and phase.
      }
    else if(type == FIRTYPE)          if this grade of response described
                                      with fir filter's coefficients.
      {
        float    factor;              A ratio factor to indicate the amplitude change.
        char     name[20];            the name of the filter.
        int      noc;                 number of coefficients.
        float   *coe;                 the coefficients.
      }
      
 complex and FAP is structures defined in response.h      */
 
#endif

/*   datacore.h    Feb 17,1997.  */

#ifndef  _data_core_h
#define  _data_core_h


#define  DXMAXPIXEL   1600     /*  This constant have to be 4's times   */
#define  PXMAXPIXEL   10240    /*  printed area x max pixels.    */

#define  WDATATYPE    float
#define  WDOTSIZE     sizeof(WDATATYPE)

typedef  struct  DATABLK  {
             struct   DATABLK    *lpNext;
             struct   DATABLK    *lpLast;
             long                 flag;
             long                 epsec;
             float               *data;
          } DATABLK;
          
typedef  struct {
         char       att;
         WDATATYPE  data;
         }  DSPDATA;
         
typedef  struct DSPINFO {
                      DSPDATA    *DspD;
                      long        process;
                      int         OoC;      /*  Order of component           */
                      int         OoT;      /*  Order of display Track       */
                      float       GainF;    /*  Gain Factor                  */
                      short       NoD;      /*  Sum of points to be plotted  */
                }  DSPINFO;
                
typedef  struct PRTINFO {
                      char        Label[20];
                      DSPDATA    *PrtD;
                      int         OoC;      /*  Order of component           */
                      int         OoT;      /*  Order of display Track       */
                      int         GainF;    /*  Gain Factor                  */
                      short       NoD;      /*  Sum of points to be plotted  */
                 }  PRTINFO;
                
typedef  struct  CHANNEL  {
             struct   CHANNEL    *lpNext;
             struct   CHANNEL    *lpLast;
             struct   CHANNEL    *OriCha;
                      FILE       *fp;
                      void       *HomeStn;
                      CHA_PAR     Para;
                      void       *lpAppInfo;
                      int         flag;     /* processes underwent  */
                      DATABLK    *hData;
                      DATABLK    *lpBlkFst;
                      DATABLK    *lpBlkEnd;
                      DATABLK    *lpBlk;
                      DATABLK    *tmpBlk;
                      int         BlkSize;
                   }  CHANNEL;

typedef  struct  {
                     STN_PAR      Para;
                     CHANNEL     *hCha;
                     int          Selected; 
                     int          state;
                     int          SecSize;
                     int          flag;
                     char         data[4800];
                  }  STNBLK;

typedef  struct  STATION  {
             struct  STATION     *lpNext;
             struct  STATION     *lpLast;
                     STNBLK      *lpStnBlk;
                  } STATION;
#endif


struct tm *newtime;

STATION   *hStn;

EVT_HEAD   EvtHead;

int      FEvent;

char  rtype[3][32]  = {"Displacement","Velocity","Acceleration"};


typedef union  GENDATA {

          char   *data8;

          short  *data16;

          long   *data32;

          float  *dataf;

          double *datad;

} UDATA;

GetSampleData(i,wlen,data)
int i;
int wlen;
UDATA data;
{     

switch(wlen) {

     case   8: return data.data8[i];
     case  16: return data.data16[i];
     case  32: return data.data32[i];
     case -32: return data.dataf[i];   
     case -64: return data.datad[i];
	 default:  return data.data16[i]; 	 
     };

   }

void LoadEvtHead()

{

 STATION *lpStn;

 CHANNEL *lpCha;

 int      i,j,k,cal_sum,code;

 EVT_HEAD eh;

 int      pf[18];

 char     stn_name[20];

 FILE    *fp;



/***  Load Event Head   ***/

   new_read(FEvent,(char *)&EvtHead,276);  

   if(EvtHead.rec_length <=0)

    EvtHead.rec_length = 30;

  
   for(i=0;i<EvtHead.netpar.stn_sum;i++)

     {

     if(i==0)  {

       lpStn = (STATION *)malloc(sizeof(STATION));

       hStn  = lpStn;

       lpStn -> lpLast = NULL;

       lpStn -> lpNext = NULL;   }

     else  {

       lpStn -> lpNext = (STATION *)malloc(sizeof(STATION));

       lpStn -> lpNext -> lpLast = lpStn;

       lpStn = lpStn -> lpNext;   

       lpStn -> lpNext = NULL;    };

     lpStn -> lpStnBlk = (STNBLK *)malloc(sizeof(STNBLK));

     /*fread((char *)&lpStn->lpStnBlk->Para,sizeof(STN_PAR),1,FEvent);*/
     new_read(FEvent,(char *)&lpStn->lpStnBlk->Para,120);

     lpStn->lpStnBlk->SecSize   = 0;

     for(j=0;j<lpStn->lpStnBlk->Para.comp;j++)

       {

       if(j==0) {

         lpStn -> lpStnBlk->hCha   =  (CHANNEL *)malloc(sizeof(CHANNEL));

         lpCha =  lpStn  -> lpStnBlk->hCha;

         lpCha -> lpLast =  NULL;

         lpCha -> lpNext =  NULL;  } 

       else {

         lpCha -> lpNext = (CHANNEL *)malloc(sizeof(CHANNEL));

         lpCha -> lpNext -> lpLast = lpCha;

         lpCha =  lpCha -> lpNext;

         lpCha -> lpNext = NULL;   };

       lpCha -> HomeStn = (void *)lpStn;

       lpCha -> flag = 2;

       lpCha->BlkSize = lpStn->lpStnBlk->Para.samp*lpStn->lpStnBlk->Para.wlen/8;

   /*    fread((char *)&lpCha->Para,sizeof(CHA_PAR),1,FEvent);*/
       new_read(FEvent,(char *)&lpCha->Para,12312);
       }

     }

}




void bgiasc_(argv,cc)
char *argv; 
int *cc;
{

  STATION   *lpStn;

  CHANNEL   *lpCha;

  FILE      *fpar;

  int        i,n,flag,doy,j,num_sample,k,val,ichanel;

  float      window_time;

  char       tmp[64],fname[64],data[800];  

  RESFAP    *lpRsp;

  FAP       *fap;

  char      *sg;   /* century control */

  


   argv[*cc]=0;
     
   if( (FEvent = new_open( argv, O_RDONLY | _O_BINARY )) == -1 ){
  
    printf("Fail to open %s, %s\n",argv,strerror(errno));

    return ;          }

    

  LoadEvtHead();

  newtime = gmtime(&EvtHead.data_begin);

  EvtHead.data_start.mon=newtime->tm_mon+1;

  EvtHead.data_start.day=newtime->tm_mday;

  EvtHead.data_start.hour=newtime->tm_hour;

  EvtHead.data_start.min=newtime->tm_min;

  EvtHead.data_start.sec=newtime->tm_sec;

  fpar  =  fopen("bgiasc.evt","w");

  if(fpar == NULL) {

    printf("Fail to open %s, %s\n",fname,strerror(errno));

    return ;    }

  

  fprintf(fpar,"%30s","SSSN CUBA");

  fprintf(fpar,"%3d",EvtHead.netpar.stn_sum*3);

  lpStn = hStn;

  lpCha = lpStn->lpStnBlk->hCha;

  doy= newtime->tm_yday;

  window_time=(float)(EvtHead.rec_length*lpStn->lpStnBlk->Para.samp/100.0);

  fprintf(fpar,"%2d %3d %2d %2d %2d %2d %6.3f",EvtHead.data_start.year,

                doy,EvtHead.data_start.mon,EvtHead.data_start.day,EvtHead.data_start.hour,

                EvtHead.data_start.min,(float)EvtHead.data_start.sec);

  fprintf(fpar," %9.3f%11s\n",window_time," ");

  /*fprintf(fpar,"%80s\n"," ");*/

  j=0;  

/*  while(lpStn != NULL) {

    lpCha = lpStn->lpStnBlk->hCha; 

    while(lpCha != NULL) {

    fprintf(fpar,"%-5s%-4s %7.2f",

                lpStn->lpStnBlk->Para.name,lpCha->Para.name,

                0);

    window_time=(float)(EvtHead.rec_length*lpStn->lpStnBlk->Para.samp/100.0);

    fprintf(fpar," %8.2f",window_time);

      lpCha = lpCha -> lpNext;

      }

	fprintf(fpar,"  \n");

	++j;

	lpStn = lpStn -> lpNext;

    }

  for(i=j; i<10; ++i)

    fprintf(fpar,"%80s\n"," ");*/

  

  lpStn = hStn;

  ichanel=0;

  while(lpStn != NULL) {

    lpCha  = lpStn -> lpStnBlk->hCha;

    while(lpCha != NULL) {

       ++ichanel;
	   /*sprintf(fname,"%s.%02d%d","chan",lpStn->lpStnBlk->Para.no,lpCha->Para.no);*/
       sprintf(fname,"%s.%d","chan",ichanel);

       lpCha -> fp = fopen(fname,"w");

       if(lpCha -> fp == NULL) {

         printf("Fail to open %s, %s\n",fname,strerror(errno));

         return ;      }

       lpCha = lpCha -> lpNext;

       }

    lpStn = lpStn -> lpNext;

    }

  

  setbuf(stdout,NULL);

  printf("%d\n",EvtHead.rec_length);

  for(n=0;n<EvtHead.rec_length;n++) {

 /*   printf(".");*/

    lpStn = hStn;

/*   if(_eof(FEvent)) break;*/

    while(lpStn != NULL) {

      new_read(FEvent,(char *)&flag,4);

      lpCha  = lpStn -> lpStnBlk->hCha;

      while(lpCha != NULL) {

        new_read(FEvent,(char *)data,lpCha->BlkSize);
		
        for(i=0;i<lpStn->lpStnBlk->Para.samp;i++) 

          fprintf(lpCha->fp,"%d\n",GetSampleData(i,lpStn->lpStnBlk->Para.wlen,data));

        lpCha = lpCha -> lpNext;

        }

      lpStn = lpStn -> lpNext;

      }

    }

  lpStn = hStn;

  while(lpStn != NULL) {

    lpCha = lpStn -> lpStnBlk->hCha;

    while(lpCha != NULL) {

      fclose(lpCha->fp);

      lpCha = lpCha -> lpNext;   }

    lpStn = lpStn -> lpNext;

    }

  

  lpStn = hStn;

  while(lpStn != NULL) {

    lpCha = lpStn->lpStnBlk->hCha;

    while(lpCha != NULL) {

      if(EvtHead.data_start.year<60) sg="1";

      else sg=" ";

      fprintf(fpar,"%-5s%-4s%3d %3d %2d %2d",lpStn->lpStnBlk->Para.name,lpCha->Para.name,EvtHead.data_start.year,doy,EvtHead.data_start.mon,EvtHead.data_start.day);

      fprintf(fpar," %2d %2d %6.3f",EvtHead.data_start.hour,EvtHead.data_start.min,(float)EvtHead.data_start.sec);

      fprintf(fpar," %7.2f",lpStn->lpStnBlk->Para.samp*1.0);

           num_sample=(int)(window_time*lpStn->lpStnBlk->Para.samp);

      fprintf(fpar," %6d %8.4f %9.4f %5.0f 4   \n",num_sample,lpStn->lpStnBlk->Para.latitude,lpStn->lpStnBlk->Para.longitude,lpStn->lpStnBlk->Para.altitude);      

      /*fprintf(fpar,"%80s\n",rtype[EvtHead.rectype]);

      fprintf(fpar,"%8.3f%8.3f%8.3f%8.3f%8.3f",1.0,0.7,133.0,0,2048.0);

      fprintf(fpar,"%8.3f%8.3f%8.3f%8.3f%8.3f\n",lpCha->Para.factor,0,0,0,0);

      fprintf(fpar,"%8.3f%8.3f%8.3f%8.3f%8.3f",0,0,0,0,0);

      fprintf(fpar,"%8.3f%8.3f%8.3f%8.3f%8.3f\n",0,0,0,0,0);

      lpRsp = (RESFAP *)lpCha -> Para.resp;

      fap = (FAP *)&lpRsp->fap;

      for(j=0;j<3;++j) {

      i=10*j; 

      for(k=0;k<10;++k) {

        if (i+k<lpRsp->ntrip)

        fprintf(fpar,"%8.3f",fap[i+k].freq);

        else

        fprintf(fpar,"%8.3f",0); 

      }

      fprintf(fpar,"\n");

      for(k=0;k<10;++k) {

        if (i+k<lpRsp->ntrip)

        fprintf(fpar,"%8.3f",fap[i+k].amp);

        else

        fprintf(fpar,"%8.3f",0); 

      }

      fprintf(fpar,"\n");

      for(k=0;k<10;++k) {

        if (i+k<lpRsp->ntrip)

        fprintf(fpar,"%8.3f",fap[i+k].phase);

        else

        fprintf(fpar,"%8.3f",0); 

      }

      fprintf(fpar,"\n");

     }

       sprintf(fname,"%s.%02d%d","chan",lpStn->lpStnBlk->Para.no,lpCha->Para.no);


       lpCha -> fp = fopen(fname,"r");

       k=0; 

       i=0;

       while((!feof(lpCha->fp)) && (i<num_sample)) {

        fscanf(lpCha->fp,"%d",&val);

        fprintf(fpar,"%11d",val);

        k=k+1;

        if(k>6) {

         fprintf(fpar,"\n");

         k=0;

        }

        ++i;

       }
	   
       if(k!=0)

       fprintf(fpar,"\n");

       fclose(lpCha->fp);

       remove(fname);*/

      lpCha = lpCha -> lpNext;

      }

    lpStn = lpStn -> lpNext;

    }

  fclose(fpar);
  
  return ;
}

