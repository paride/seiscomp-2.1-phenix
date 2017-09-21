/*----------------------------------------------------------------------------
//  Program :   AUTO_INDEX.C
//  Update  :   20-12-2000
//  By      :   Arie van Wettum, Universiteit Utrecht, Seismologie.
//
//  May 2001:   Modified by Reinoud Sleeman, ORFEUS Data Center 
//              to feed SeedLink with data from diskfiles; this plugin
//              checks the data directory regularly for new datafiles;
//              these files are supposed to be in mini-SEED, otherwise
//              intermediate format conversions may be applied
//----------------------------------------------------------------------------*/

#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <strings.h>
#include <signal.h>
#include "auto_index.h"

/*
//---------------- Forward declarations ---------------------------------------*/

void 	err_outp(char *err);
void 	transfer_mseed(char *src_dir);
void 	sort_file(int n);
double 	get_starttime(char *fn);
time_t 	read_time_status_change(char * filename);
int 	check_dir_change(char *src_dir);

/*
//---------------- Globals ------------------------------------------------*/

char    fname[256], lbuf[256], syscom[140];
long    interval = 5000000;                         /* Time in microseconds*/
FILE    *fd1, *fd2, *fd3;
int     debug = 0;
time_t  data_dir_time;

/*
//-------------------------------------------------------------------------*/

int main(int argc, char *argv[]) 
{       
	int n, cnt;
	
/*
// --- Set the option flags ---	*/

	cnt = 0;
	
	for(n = 0; n < argc; n++) 
	{		
		if(strncmp(argv[n], "-h", 2) == 0)
		{
    		printf("\n\n\n                ---------------- AUTO_INDEX.C --------------- \n\n"); 
    	 	printf("By      :    Arie van Wettum, Universiteit Utrecht, Seismology. \n");
    	 	printf("Date    :    20-12-2000. \n\n\n");
    	    printf("Features:    Generate automatic index file for NARS datalogger\n");	   	     
    		printf("Use     :    auto_index [-v -h -t n]\n\n"); 
   		    printf("Where   :   -h    This help menu\n");
   		    printf("            -v    Verbose option.  Provide additional information msgs. \n");
    		printf("            -t n  Interval time 'n' in microseconds, default is 5000000 (5 sec).\n\n");
    		exit(1); 
		}

		if(strncmp(argv[n], "-v", 2) == 0)
		{
			debug = 1;			
        	if((fd1 = fopen(DEBUGFILE, "w+")) == NULL)
        	{
		      perror("Can't open DEBUGFILE\n"); 
		      exit(1);
	    	} 			
			err_outp(" ----- auto_fmv.debug ----");			
			continue;
		}
		
		if(strncmp(argv[n], "-t", 2) == 0)
		{
			n++;
			sscanf(argv[n],"%ld", &interval);
			continue;
		}
				
	    argv[cnt++] = argv[n];
	}
	argc = cnt;
		
    if(debug)
    {    
        err_outp("Verbose option is on.");
        sprintf(lbuf,"interval = %ld ", interval);
        err_outp(lbuf);        
    }	    

    while (1) 
    {            
        printf("check dir changes\n");

        if ( check_dir_change(NARS_DIR) )
        {
              transfer_mseed (NARS_DIR);
        }
		
        usleep(interval);
    }			
    exit(0);	
} 				

/*-------------------------------------------------------------------------*/

void err_outp(char *err)
{      
      if(debug)
         fprintf(fd1, "%s\n", err);
}

/*-------------------------------------------------------------------------*/

time_t read_time_status_change(char * filename)
{
   struct stat mstat;

  if(stat(filename, &mstat))
  {
      sprintf(lbuf,"Error reading stat of file %s\n", filename);
      err_outp(lbuf);      		
      return(-1);
  } 
  else 
  { 
       return(mstat.st_ctime);	 /* Time of last file status change */
/*     return(mstat.st_mtime);    Time of last data modification  */
  }
}

/*-------------------------------------------------------------------------*/


int check_dir_change (char *src_dir)
{ 	  
       static time_t latest_t = 0, new_t = 0;            

        /* --- Check for data directory ---- */

	if (access(src_dir, R_OK)) 
              err_outp("No data directory availabel !");

        /*----- Check for directory change ----*/

	strcpy(fname, src_dir);
	strcat(fname, ".");
	 
	new_t = read_time_status_change(fname);  

        data_dir_time = new_t;
	
	if(new_t != latest_t)
	{
		latest_t = new_t;		
		return(1);
	}

	return(0);	                                                                       
}


void transfer_mseed(char *src_dir)
{
    DIR *dirp;
    struct dirent *dp;
    struct _indx indx;
    int n = 0;
time_t  file_time;
char fullname[100];


    dirp = opendir(src_dir);
    printf("[%s] opened\n", src_dir);

    while ((dp = readdir(dirp)) != NULL)
    {
                if((strcmp(".", dp->d_name) != 0) && (strcmp("..", dp->d_name) != 0))
                {

                        sprintf( fullname, "%s%s", src_dir,dp->d_name);
                        file_time = read_time_status_change(fullname);
                        /*printf("dir_time  %ld  new_t  %ld\n", data_dir_time,file_time);*/

                        if ( file_time >= data_dir_time ) {

/* Non mini-SEED data-files may be converted here */ 
                             printf("process new file: %s\n", fullname);
/*

                             sprintf ( syscom, "gcfconv -F %s -f sac -l 99999", fullname);
                             
*/
                             /* Open datafile and send data using send_mseed */

                             /*
                             strcpy ( station_code, "FPPC" );

                             if ((datafp = fopen(fullname, "r")) != NULL ) {
                                  printf("File %s not opened\n", fullname);
                             }


                              while (1)
                              {
                                  if (fread(dbuf, 512, 1, datafp)) {
                                       if ( !(stat = send_mseed(station_code, dbuf, 512))) {
                                            printf("[disk_plugin] NO data sent with send_mseed!\n");
                                            break;
                                       }
                                  }
                              }

                              */

                        }

                }
    }

    closedir(dirp);

}


/*-------------------------------------------------------------------------*/

void sort_file(int n)
{
	int x, y, p, s;
	struct _indx indx;
	long youngest;
	
    if((fd3 = fopen(INDEXFILE, "w+")) == NULL)
    {
		perror("Can't open INDEXFILE!\n"); 
		exit(1);
	}
	
	s = 0;
	
	for(y = 0; y < n; y++)
	{	
		rewind(fd2);
	    youngest = 0;

		for(x = 0; x < n; x++)
		{		
			fread(&indx, sizeof(indx), 1, fd2);
		
			if(indx.fnr >= 0)
			{
				if( youngest < indx.start)  /*-- Search for most recent file ( biggest time value) --*/
				{
					youngest = indx.start;
					p = x;
				}
			}		
		}
	
		fseek(fd2, p * sizeof(indx), 0);         /* Get the struct, set index to -1 and save again.*/
		fread(&indx, sizeof(indx), 1, fd2);
		
		indx.fnr = -1; 
		fseek(fd2, p * sizeof(indx), 0);  	
		fwrite(&indx, sizeof(indx), 1, fd2);
	    		
 		indx.fnr = s++;    			/* Write with new follownr to indexfile.*/
   		fwrite(&indx, sizeof(indx), 1, fd3);
	}

	fclose(fd3);
	fclose(fd2);		
}

/*-------------------------------------------------------------------------*/

double get_starttime(char *fn)
{
	int x, y, p, year, daynr, hrs, min, sec;
	char d[10][20], bf[3];
	char strt[24];
	double start = 0.0;
	
        y = p = 0;
            
	for(x = 0; x < strlen(fn); x++)
	{
		    if(fn[x] == '.')
		    {                  
			    d[y++][p]= 0;
			    p = 0;
		    }
            else
            {
                d[y][p++]= fn[x];
            }
        } 	         

	d[y][p]= 0;

	if(strncmp(d[4],"dat", 3) != 0)
	{
		sprintf(lbuf,"Wrong extention %s\n", d[4]);
        err_outp(lbuf);        
	}
	else
	{   
		sprintf(strt,"%s%s%s.0", d[1], d[2], d[3]);
	}            
	
	return(atof(strt));	
}
