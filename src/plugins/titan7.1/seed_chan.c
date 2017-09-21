/*==================================================================*
    seed_chan.c

    Generate channel name according to the type of sensor, the gain,
    the component type, the sample rate and the orientation tolerance.

    Instrument code (2nd letter) has changed n yyear 2000 for
    accelerometer. Was name[1] = 'G' now name[1] = 'N'.

 *==================================================================*/
#include "seed.h"
#include "proto.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

struct sensorType {
   char  type[10];      /* sensor type (STS1, CMG3, etc...)  */
   char  band[4];       /* LP, ACC */
   struct sensorType *next;
};

static void  read_sensor_list ();
static void  getRespDir       ();

static char  *PZdir;
static char  *RESPDBdir;
static struct sensorType *sensorListHead;
static struct sensorType *sensorListTail;


/*==================================================================*/
void seed_channel_map(sensor, gain, comp, srate, orient_tol, name)
char   *sensor;
char   gain;
char   *comp;
double srate;
double orient_tol;
char   *name;
{
char   bandCode[4];
int    found_sensor;
struct sensorType *pt;
int    dbug = 0;

if (dbug)
{
  printf("==== seed_channel_map: input: sensor=%s g='%c' ", sensor, gain);
  printf("comp=%s srate=%E\n", comp, srate);
}

    read_sensor_list();

    found_sensor = FALSE;
    for (pt=sensorListHead; pt!=NULL; pt=pt->next)
    {
       if (!strcmp(sensor, pt->type))
       {
          found_sensor = TRUE;
          sprintf(bandCode, "%s", pt->band);
          break;
       }
    }
    if (found_sensor == FALSE)
    {
        fprintf(stderr,
          "ERROR: seed_channel_map: sensor '%s' not found in list\n",sensor);
        exit(1);
    }
if (dbug) printf("====                   found %s; band_code=%s\n",
                  sensor, bandCode);


/* SEED channel naming. Band Code = 1st char
 *          srate       corner period   code
 *      --------------    ---------      -- 
 *      >= 80             <  10 sec       E
 *      <  80 to >= 10    <  10 sec       S
 *      >= 80             >= 10 sec       H
 *      <  80 to >= 10    >= 10 sec       B
 *      > 1   to <  10                    M
 *      > 0.1 to <= 1                     L
 *      > 0.01 to <= 0.1                  V
 *      <= 0.01                           U
 */

 /* Default is High Gain Seismometer */
    if (gain == 'L');
    else gain = 'H';

    name[3] = '\0';

/* First letter */

    name[0] = '\0';
  if (srate >= 80.0                 && strcmp(bandCode,"LP")) name[0] = 'E';
  if (srate >= 10.0 && srate < 80.0 && strcmp(bandCode,"LP")) name[0] = 'S';
  if (srate >= 80.0                 && !strcmp(bandCode,"LP")) name[0] = 'H';
  if (srate >= 10.0 && srate < 80.0 && !strcmp(bandCode,"LP")) name[0] = 'B';
   if (srate >  1.0  && srate < 10.0)                         name[0] = 'M';
   if (srate >  0.1  && srate <= 1.0)                         name[0] = 'L';
   if (srate >  0.01 && srate <= 0.1)                         name[0] = 'V';
   if (srate <= 0.01)                                         name[0] = 'U';
   if (name[0] == 0)
   {
     fprintf(stderr,"ERROR: channel_map: sensor band code unsupported: \n");
     fprintf(stderr,"       sensor %s srate: %.6f\n", sensor, srate);
     exit(1);
   }

/* Second letter */

    if (!strcmp(bandCode,"ACC")) name[1] = 'N';    /* Accelerometer */
    else                         name[1] = gain;

/* Third letter */

/*
 * If horizontal orientation is off by more than 5 degrees,
 * N and E should be replaced by 2 and 3 respectively.
 */

    if      (comp[0]=='0' || comp[0]=='Z' || comp[2]=='Z') name[2] = 'Z';
    else if (comp[0]=='1' || comp[0]=='N' || comp[2]=='N') name[2] = 'N';
    else if (comp[0]=='2' || comp[0]=='E' || comp[2]=='E') name[2] = 'E';
    else
    {
      fprintf(stderr,"ERROR: channel_map: unsupported component %s\n", comp);
      exit(1);
    }
    if (orient_tol > 5.0)
    {
        if      (name[2] == 'Z') name[2] = '0';
        else if (name[2] == 'N') name[2] = '1';
        else if (name[2] == 'E') name[2] = '2';
    }


if (dbug) printf("====                   %s it=%E comp=%s     -----> %s\n",
                 sensor, srate, comp, name);
}




/*==================================================================*/
static void read_sensor_list()
{
char  fname[511];
FILE  *Fp = NULL;
char  line[255];
int   lineno = 0;
char  *token[3];
int   ntokens;
struct sensorType *pt;


   getRespDir();

   if (PZdir)
   {
      sprintf(fname, "%s/%s", PZdir, SENSORLIST);
   }
   else if (RESPDBdir)
   {
      sprintf(fname, "%s/%s", RESPDBdir, SENSORLIST);
   }
   else
   {
      fprintf(stderr,"ERROR: read_sensor_list: can't find responses dir\n");
   }
/*
printf("===== opening %s\n", fname);
*/
   if (!(Fp = fopen(fname, "r")))
   {
       fprintf(stderr,"ERROR: read_sensor_list: can't open %s\n", fname);
       exit(1);
   }

   while (!getLine(Fp, line, 255, '#', &lineno))
   {
      trim(line);
      ntokens = sparse(line, token, " \t", 3);
      pt = (struct sensorType *) calloc(sizeof(struct sensorType), 1L);
      append_linklist_element(pt, sensorListHead, sensorListTail);
      sprintf(pt->type, "%s", token[0]);
      sprintf(pt->band, "%s", token[1]);
   }

/*
   for (pt=sensorListHead; pt!=NULL; pt=pt->next)
       printf("===== %s\n", pt->type);
*/

   fclose(Fp); Fp = NULL;
   return;
}



/*==================================================================*/
static void getRespDir()
{

    if ((PZdir = getenv("PZ")))
    {
       if (!isdir(PZdir))
       {
          fprintf(stderr,"\tmkseed: directory '%s' not found\n", PZdir);
          exit(1);
       }
    }
    else if ((RESPDBdir = getenv("RESPDB")))
    {
       if (!isdir(RESPDBdir))
       {
          fprintf(stderr,"\tmkseed: directory '%s' not found\n", RESPDBdir);
          exit(1);
       }
    }
    else
    {
       fprintf(stderr,"\tmkseed: Can't get environment variable. ");
       fprintf(stderr,"The variable 'PZ' or 'RESPDB'\n");
       fprintf(stderr,"\tmust be set to the ");
       fprintf(stderr,"directory containing responses informations.\n");
       fprintf(stderr,
           "\tExamples:\n");
       fprintf(stderr,"\t  setenv PZ /home/fels/respdb/PZ\n");
       fprintf(stderr,"\t  setenv RESPDB /home/fels/respdb/respdb.tgrs\n\n");
       exit(1);
    }
}

