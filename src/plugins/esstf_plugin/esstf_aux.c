/*                                                           */
/* esstf_aux.c - declarations for esstf_aux.c and            */
/*               esstf_plugin.c                              */
/*                                                           */
/*               please edit esstf_config.h                  */
/*                                                           */
/* Version: 2002.119   -    Mathias Hoffmann                 */
/*                          AWI Bremerhaven                  */
/*                                                           */
/*      tested with:                                         */
/*      Solaris 2.6 (sparc) &  Linux 2.4.4 (intel)           */
/*                                                           */

#include <errno.h>
#include "esstf_aux.h"

static unsigned int bcd(unsigned int TF, unsigned int j);
extern char progname[];

/* "public" functions */

/* read datablock from input and extract header and DCF time information */
/*-----------------------------------------------------------------------*/
int read_esstf_datablock(DATABLOCK *DB, FILE *esstf_file){
  
  int read = 0;
  
  /* read DATABLOCK_SIZE number of bytes from input */ 
  if ( (read = fread(&DB->datablock, 1, DATABLOCK_SIZE, esstf_file)) == 0) 
    return 0;

  /* process the data ...*/
  extr_header(DB);
  extr_DCFtime(DB);
  set_time(DB);
  extr_data(DB);
  set_station_code(DB);

return read;
}

/* set time status information to 0 */
/*-----------------------------------------------------------------------*/
void init_datablock(DATABLOCK *DB){

DB->datablock_time_status = 0;
DB->DCF_time.status = 0;
DB->ASCII_time.status = 0;

}

/* this debug function displays the content of the ASCII header and */
/* time info */ 
/*-----------------------------------------------------------------------*/
void show_datablock_info(DATABLOCK *DB){

  /* display warning message, if ASCII and DCF time info is invalid */
  if (DB->datablock_time_status == -1){
    printf("channel: %02d  *** NO TIME INFO *** \n", DB->channel);
    return;
  }
  
  printf("# %4s_%-4s ch:%02d %02d:%02d:%02d-%02d.%02d.%02d  NOSC:%d  stat:%d (it: %d %d %d)\n",
 	  DB->station,
	  DB->component,
	  DB->channel,
 	  DB->datablock_time->hour,
 	  DB->datablock_time->minute,
 	  DB->datablock_time->second,
 	  DB->datablock_time->day,
 	  DB->datablock_time->month,
 	  DB->datablock_time->year,
 	  DB->datablock_time->nosc,
 	  DB->datablock_time_status,
	  DB->it.year,
	  DB->it.second,
	  DB->it.usec);
	 
}



/* "private" functions */

/* extracts the ASCII Header of the esstf datablock (first 48 byte) */
/*-----------------------------------------------------------------------*/
int extr_header(DATABLOCK *DB){

  unsigned char *buffer = &DB->datablock[0];
  unsigned char ascii[5];
  short pos, len;

  DB->ASCII_time.status = -1;

  /* convert ASCII data to int */
  pos = 0;  len = 3;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->channel = atoi(ascii);
  pos = 3;  len = 2;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->ASCII_time.year = atoi(ascii);
  pos = 5;  len = 2;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->ASCII_time.month = atoi(ascii);
  pos = 7;  len = 2;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->ASCII_time.day = atoi(ascii);
  pos = 9;  len = 2;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->ASCII_time.hour = atoi(ascii);
  pos = 11;  len = 2;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->ASCII_time.minute = atoi(ascii);
  pos = 13;  len = 2;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->ASCII_time.second = atoi(ascii);
  pos = 15;  len = 4;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->ASCII_time.nosc = atoi(ascii);
  pos = 19;  len = 4;
    strncpy(ascii, buffer+pos, len); ascii[len] = '\0';
    DB->nob = atoi(ascii);
    
  /* simple range check */  
  if (
       (DB->ASCII_time.year >= 0) && (DB->ASCII_time.year <= 99) &&
       (DB->ASCII_time.month >= 1) && (DB->ASCII_time.month <= 12) &&
       (DB->ASCII_time.day >= 1) && (DB->ASCII_time.day <=31) &&
       (DB->ASCII_time.hour >= 0) && (DB->ASCII_time.hour <= 24) &&
       (DB->ASCII_time.minute >= 0) && (DB->ASCII_time.minute <= 60) &&
       (DB->ASCII_time.second >= 0) && (DB->ASCII_time.second <= 60) &&
       (DB->ASCII_time.nosc <= 500 )
     )
     DB->ASCII_time.status = 1;

return  DB->ASCII_time.status;
}

/* extracts the binary 3 byte essf format data to 32 bit int data */
/*-----------------------------------------------------------------------*/
int extr_data(DATABLOCK *DB){

  unsigned char *data_v = &DB->datablock[48];
  int i;
  unsigned char dummy[2];
  short int *wert, *factor;

  #if defined (__ESSTF_INTEL__)
    char intel[2];            /* LINUX */
    dummy[1] = 0;             /* LINUX */
  #else
    dummy[0] = 0;             /* sparc */
  #endif

  for (i = 0; i < ((DATABLOCK_SIZE -48)/4); i++){
    #if defined (__ESSTF_INTEL__)
      dummy[0] = data_v[2];             /* LINUX */
      intel[0] = data_v[1];             /* byte swapping for linux */
      intel[1] = data_v[0];             /* byte swapping for linux */
      wert = (short int*)&intel[0];     /*  LINUX: 1.+ 2. byte: data */
  #else
      dummy[1] = data_v[2];             /* sparc */
      wert =   (short int*)&data_v[0];  /*  sparc: 1.+ 2. byte: data */
  #endif
    
    
    factor = (short int*)&dummy[0];     /*  3. byte: gain ranging */
    				        /*  4. byte: digital byte, nix */
    DB->si32_data[i] = (*wert << *factor);
    data_v += 4;		        /* point to next 4 bytes */
  
  }



}

/* extracts the DCF time info from the digital byte of channel 0 */
/*-----------------------------------------------------------------------*/
int extr_DCFtime(DATABLOCK *DB){

  unsigned int i, j = 0;
  unsigned int dcf_nosc;
  unsigned char *data;
  unsigned int len = DATABLOCK_SIZE - 48;
  unsigned int search = 0, search_beg = 0, start = 0;
  unsigned int sec = 0, min = 0, hour = 0;
  unsigned int day = 0, month = 0, year = 0;
  unsigned int C0, TF, byte = 8;

  if (DB->channel == 0){
    
  data = &DB->datablock[48];

  for (i=0; i < len; i+=4){
 
      C0=(data[3] & CHANNEL_0);
      TF = (data[3] & TIME_FAST);
      
      /* search for DCF starting header */
      if (!start){
    	if (TF) search++;
    	else search = 0;

    	if (search == 6) search_beg = 1;
    	if (search_beg && !search){
    	  start = i + 2*4;
    	  search_beg = 0;
    	  dcf_nosc = (start/4)-1;
    	}
      }
      else{
    	if (i >= start && j < 48){
    	  if (j < 8)		     sec   += bcd(TF, j);
    	  if ((j >= 8)  && (j < 16)) min   += bcd(TF, j-8);
    	  if ((j >= 16) && (j < 24)) hour  += bcd(TF, j-16);
    	  if ((j >= 24) && (j < 32)) day   += bcd(TF, j-24);
    	  if ((j >= 32) && (j < 40)) month += bcd(TF, j-32);
    	  if ((j >= 40) && (j < 48)) year  += bcd(TF, j-40);
    	  j++;
    	  if (j == 48){
    	}
      }
    }

    data += 4;
    
    DB->DCF_time.year = year;
    DB->DCF_time.month = month;
    DB->DCF_time.day = day;
    DB->DCF_time.hour = hour;
    DB->DCF_time.minute = min;
    DB->DCF_time.second = sec;
    DB->DCF_time.nosc = dcf_nosc;
    
    /* simple range check */
    if (
    	 (year >= 00) && (year <= 99) &&
    	 (month >= 1) && (month <= 12) &&
    	 (day >= 1) && (day <=31) &&
    	 (hour >=0) && (hour <= 24) &&
    	 (min >=0) && (min <= 60) &&
    	 (sec >=0) && (sec <= 60) &&
    	 (dcf_nosc <= 500 )
       )
       DB->DCF_time.status = 1;
    else
       DB->DCF_time.status = -1;
    
    }
  }
   
return DB->DCF_time.status;
}

/* sets the valid datablock time (ASCII or DCF) */
/* calculates the time in form: yyyy SecondsOfYear */
/*-----------------------------------------------------------------------*/
int set_time(DATABLOCK *DB){
  
  struct tm T;      /* in time.h */
  time_t t;         /* seconds since 1970 (t=0) */
  int ysec;         /* seconds of day*/

  /* valid DCF time info */
  if (DB->DCF_time.status == 1) {
    DB->datablock_time = &DB->DCF_time;
    DB->datablock_time_status = 1;
  } 
  /* if no DCF time is available choose ASCI time info */
  else if (DB->ASCII_time.status == 1) {
    DB->datablock_time = &DB->ASCII_time;
    DB->datablock_time_status = 2;
  }
  /* if both are false, mark time status as bad */
  else 
    DB->datablock_time_status = -1;
  
 
  if (DB->datablock_time_status != -1){

    /* the next lines are to obtain the day of year  */
    /* works only if the system is running at UTC !!!*/
     
    /* copy to tm-Struktur */
    T.tm_year	 = DB->datablock_time->year + 100;  /* yy years since 1900 */
    T.tm_mon	 = DB->datablock_time->month - 1;   /* mm month since january */
    T.tm_mday	 = DB->datablock_time->day;
    T.tm_hour	 = DB->datablock_time->hour;
    T.tm_min	 = DB->datablock_time->minute;
    T.tm_sec	 = DB->datablock_time->second;
 
    t = mktime(&T);    /* convert to seconds */
    gmtime_r(&t, &T);  /* and back */
 

    /* seconds into the year */
    ysec = 86400 * T.tm_yday;
    ysec += 3600 * T.tm_hour;
    ysec += 60 * T.tm_min;
    ysec += T.tm_sec;

    /* set the time for seedlink */
    DB->it.year = T.tm_year + 1900;
    DB->it.second = ysec;
    /* microsecond info from NOSC */
    DB->it.usec = DB->datablock_time->nosc / SAMPLERATE * 1000;
  }
 
return DB->datablock_time_status;
}

/* assignes the station name/componet to the current channel */
/*-----------------------------------------------------------------------*/
void set_station_code(DATABLOCK *DB){

DB->station = station_code[DB->channel];
DB->component = station_component[DB->channel];

}

/* helper function to calculate the time from DCF bits */
/*-----------------------------------------------------------------------*/
static unsigned int bcd(unsigned int TF, unsigned int j){

  if (TF){
    switch(j){
      case 0: return 1;
      case 1: return 2;
      case 2: return 4;
      case 3: return 8;
      case 4: return 10;
      case 5: return 20;
      case 6: return 40;
      case 7: return 0;
    }
  }
  else return 0;
}

/* send the datablock with time info to seedlink*/
/*-----------------------------------------------------------------------*/
int send_datablock(DATABLOCK *DB){
  
  int sent = 0;
  INT_TIME *time = &DB->it;
  char timestr[30];
  char message[80];

  /* if not a invalid channel (marked with NULL) */
  if ( strcmp(DB->station, "NULL") != 0 ){
    
    /* if no valid time info is present, let seedlink calculate the */
    /* time from the former call of send_raw */
    if (DB->datablock_time_status == -1)
      time = NULL;
      
    /* now send the data to seedlink ...*/
    sent = send_raw(DB->station, DB->component,
                    time, 0,
	            &DB->si32_data[0], 500);

    if (sent == -1)
      diag_message("send_raw");

  }
  
return sent;
}

/* try to close the data stream to the seedlink server... */
/* invoked by signal handling */
/*-----------------------------------------------------------------------*/
int send_datablock_last(void){
  
  int sent = 0;
  int i;
  char buffer[1024];
  char *code, *comp;
  
  diag_message("caught signal - trying to terminate...");

  for (i = 0; i < CHANNELS; i++){
    code = station_code[i];
    comp = station_component[i];
    
    if ( (strcmp(code, "NULL") != 0) && (strcmp(comp, "NULL") != 0) ){
      sent = send_raw(code, comp, NULL, 0, NULL, 0);
      sprintf(buffer, "flushing stream %4s_%-4s", code, comp);
      diag_message(buffer);
    }
  }

return sent;
}


/* generates diagnostic messages, ... */
/*-----------------------------------------------------------------------*/
void diag_message(char *message){

  time_t loc_time;
  char timestr[30];
  char buffer[1024];
  
  time(&loc_time); strcpy(timestr,ctime(&loc_time));
  timestr[strlen(timestr) -1] = 0;

  sprintf(buffer, "%s - [%s] %s", timestr,progname,message);
  
  if (errno != 0)
    perror(buffer);
  else
    fprintf(stderr, "%s\n", buffer);
}

/* prints out the waveform data in ascii format for the selected channel */
/*-----------------------------------------------------------------------*/
void dump_esstf_data(DATABLOCK *DB, int channel, int info){
  int i;
  
  if (DB->channel != channel) return;
  
  if (info)
    show_datablock_info(DB);
  
  for (i = 0; i < ((DATABLOCK_SIZE -48)/4); i++){
    printf("%d\n", DB->si32_data[i]);
  }

}









