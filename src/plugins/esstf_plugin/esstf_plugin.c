/*                                                           */
/* esstf_plugin.c - plugin for the seedlink server           */
/*                  reads an esstf data stream from a file   */
/*                  or a fifo                                */
/*                                                           */
/*          configuration: edit esstf_config.h and recompile */
/*                                                           */
/* Version: 2002.119   -    Mathias Hoffmann                 */
/*                          AWI Bremerhaven                  */
/*                                                           */
/*      tested with:                                         */
/*      Solaris 2.6 (sparc) &  Linux 2.4.4 (intel)           */
/*                                                           */

#include <signal.h>
#include "esstf_aux.h"

#if defined (__ESSTF_INTEL__)
  #include <getopt.h>
#endif

void usage();

/* global program name*/
char progname[] = "esstf_plugin";

/*-----------------------------------------------------------------------*/
void sig_handler(int sig){
  send_datablock_last();
  exit(0);
}


/*-----------------------------------------------------------------------*/
int main(int argc, char **argv){

int read = 0;
DATABLOCK DB;
char *filename;
FILE *esstf_file;
char message[1024];
int first = 1;
int loop = 1;
int info = 0;

/* option args */
char c;
int show_mode = 0;
int send_mode = 0;
int dump_mode = 0;
int file_mode = 0;
int dump_channel;

/* parsing the option args and filename */
if (argc < 2) usage(argv);

while((c = getopt(argc, argv, "a:hdsf")) != EOF){
  switch(c){
    case 'h': usage(progname); exit(0);
    case 'a': dump_mode = 1; dump_channel = atoi(optarg); 
              sprintf(message, "mode: dump ASCII data ch: %d", dump_channel); 
              diag_message(message); 
	      break;
    case 'd': show_mode = 1;
              diag_message("mode: dump header and time info"); 
	      break;
    case 's': send_mode = 1;
              diag_message("mode: don't send data to seedlink server"); 
              break;
    case 'f': file_mode = 1;
              diag_message("mode: file operation, don't loop"); 
              break;
    exit(1);
  }
}

/* enable dumping of headers only for selected channel */
if (dump_mode && show_mode) {
  show_mode = 0;
  info = 1;
}

/* if there is no filename at the end... exit! */
if (optind < argc) 
  filename = argv[optind];
else
  usage(argv);

/* catch SIGINT and SIGTERM ... */
/* when sending data to the seedlink server */
if (!send_mode){
  signal(SIGINT, sig_handler);
  signal(SIGTERM, sig_handler);
}

/* loop for ever ... */
while (loop){

  /* try to open the file */
  esstf_file = fopen(filename, "r");
  if (first){
    sprintf(message, "opening file  %s  for reading", filename);
    diag_message(message);
    first = 0;
  }
  if (esstf_file == NULL) exit (1);
  
  init_datablock(&DB);

  /* read the esstf datablock (one channel) and proceed */
  while( (read = read_esstf_datablock(&DB, esstf_file)) != 0 ){
    if (show_mode) show_datablock_info(&DB); 
    if (!send_mode) send_datablock(&DB); 
    if (dump_mode) dump_esstf_data(&DB, dump_channel, info); 

  }
  fclose(esstf_file);

  /* read a file only once */
  if (file_mode) loop = 0;

} /* end of while(1) */

}

/*-----------------------------------------------------------------------*/
void usage(){

  #if defined (__ESSTF_INTEL__)
    printf("%s version: %s (intel)\n",progname, ESSTF_PLUGIN_VERSION);
  #else
    printf("%s version: %s (sparc)\n",progname, ESSTF_PLUGIN_VERSION);
  #endif
  printf("Mathias Hoffmann, AWI Bremerhaven\n");
  printf("usage:   %s [options] <esstf file or fifo>\n", progname);
  
  printf("options: -d     dump header information\n"
         "         -a<ch> dump ascii data for channel ch\n"
         "         -s     don't send data to seedlink server\n"
         "         -f     file operation; exit at end of file, don't loop\n"
         "         -h     help message\n"
         "default: send data to seedlink server\n"
        );
  
  exit(1);
}

