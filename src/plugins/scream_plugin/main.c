/* 
 * scream2slink module  
 *    Supports listening for UDP gcf packets from SCREAM!
 *
 */

/* Mar 2004  - Developed by Reinoud Sleeman  (ORFEUS/KNMI)  */
/*             for the SCREAM plugin in SeedLink            */



#include "project.h"
#include "config.h"
#include "map.h"


struct config_struct config;
char                 *mapfile;
Map                  *rootmap;

int get_line (FILE *fd, char *line)
{
        char c;
        int i=0, n;

        do {

          n = fscanf ( fd, "%c", &c);
          if (n<0) return (-1);

          if ( c == '\n' ) break;

          *(line+i) = c;
          i++;
          
        } while (1);

        *(line+i) = '\0';

        return(i);
}


Map *init_map ()
{
        Map *map;
        map = malloc (sizeof(Map));
        return(map);
}

int add_map (Map *inmap)
{
   Map *newmap, *mp;

   if ( rootmap == NULL ) {
        rootmap = malloc (sizeof(Map));
        rootmap->stream  = strdup ( inmap->stream ); 
        rootmap->network = strdup ( inmap->network ); 
        rootmap->station = strdup ( inmap->station ); 
        rootmap->channel = strdup ( inmap->channel ); 
        rootmap->id      = inmap->id;
        rootmap->next    = NULL;
   }
   else {
        for ( mp = rootmap; mp != NULL; mp=mp->next ) {

              if ( strncmp ( mp->stream, inmap->stream, strlen(inmap->stream)) == 0 )
                   break;
        }

        if (mp==NULL) {
            newmap = init_map ();
            newmap->stream  = strdup ( inmap->stream ); 
            newmap->network = strdup ( inmap->network ); 
            newmap->station = strdup ( inmap->station ); 
            newmap->channel = strdup ( inmap->channel ); 
            newmap->id      =  inmap->id;
            newmap->next    = NULL;

            for ( mp = rootmap; mp != NULL; mp=mp->next ) {

              if ( mp->next == NULL ) {
                   mp->next = newmap;
                   break;
              }
            }
        }
   }
}

int parse_mapfile (char *mapfile)
{
   FILE *fd;
   Map *newmap;
   char lineid[200];
   char network[20];
   char code[20];
   char station[20];
   char channel[20];
   char line[1000];
   int  id, n;


   if ( (fd = fopen(mapfile,"r") ) == NULL ) {
         printf("Map file [%s] not opened\n", mapfile);  
         exit(0);
   }

   else {

        /* parse lines like:
              ChanInfo PMSTZ4	IP  PMST	BHZ	1
        */

        do {

           newmap = malloc (sizeof(Map));

           n = get_line (fd, line);
           if (n<0) break;

           sprintf ( lineid, " ");
           sscanf ( line, "%s", lineid );

           if ( strncmp ( lineid, "ChanInfo", 8) == 0 ) {

               n = sscanf ( line, "%s %s %s %s %s %d", 
                            lineid, code, network, station, channel, &id);

                newmap->stream = strdup ( code ); 
                newmap->network = strdup ( network ); 
                newmap->station = strdup ( station ); 
                newmap->channel = strdup ( channel ); 
                newmap->id =  id;
                //printf ( "newmap:  %s  %s %s %s %s %d\n", 
                         //lineid, newmap->stream, newmap->network, newmap->station, newmap->channel, newmap->id);

               if ( newmap->stream != NULL) {
                    add_map (newmap);
               }

               if (newmap != NULL) free(newmap);
           }

        } while (1);

   }
}


int
main (int argc, char **argv)
{
  int optind;
  Map *mp;
  
  /* Process command line argument */

  for(optind=1 ; optind < argc ; optind++)
  {

    if (strcmp(argv[optind], "-h") == 0) 
    {
         config.server = (char *) malloc (100);
         strcpy ( config.server, argv[++optind] );
    }
    else if (strcmp(argv[optind], "-p") == 0) 
    {
         config.port = atoi(argv[++optind]);
    }
    else if (strcmp(argv[optind], "-m") == 0) 
    {
         mapfile = (char *) malloc (140);
         strcpy ( mapfile, argv[++optind] );
    }
  }
  /* config.protocol = SCM_PROTO_TCP; */
  config.protocol = SCM_PROTO_UDP;


  if ( config.server == NULL || config.port == 0 ) {
       printf("No server or port defined\n");
       return(0);
  }

  printf("server: %s  port: %d  mapfile: %s\n", config.server, config.port, mapfile);

  parse_mapfile (mapfile);

  for ( mp = rootmap; mp != NULL; mp=mp->next ) {
        printf("found mapping:   %s ->  %s  %s  %s  %d\n",
        mp->stream,
        mp->network,
        mp->station,
        mp->channel,
        mp->id );   

  }

  scm_init (config.protocol, config.server, config.port);

  for (;;) 
  {
      scm_dispatch ();
  }


}
