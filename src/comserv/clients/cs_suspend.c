#include <stdio.h>
#include <stdlib.h>

#include "dpstruc.h"
#include "stuff.h"
#include "service.h"

#define CLIENT_NAME "SUSP"

const char *const usage = "Usage: cs_suspend <station>\n";

const char *comserv_status(unsigned int n)
  {
    static const char *const stats[13] =
      {
        "Good", "Enqueue Timeout", "Service Timeout", "Init Error",
        "Attach Refused", "No Data", "Server Busy", "Invalid Command",
        "Server Dead", "Server Changed", "Segment Error", "Command Size",
        "Privileged Command"
      };

    if(n > 12) return "Unknown";
    return stats[n];
  }
    
int main(int argc, char **argv)
  {
    tclient_struc *client = NULL;
    tclient_station *pstation = NULL;
    comstat_rec *pcomm = NULL;
    tstations_struc tstations;
    int status;
    
    if(argc != 2)
      {
        printf(usage);
        return 1;
      }

    cs_setup(&tstations, CLIENT_NAME, argv[1], TRUE, FALSE, 0, 1, CSIM_MSG, 6000);

    if(tstations.station_count == 0)
      {
        fprintf(stderr, "station not found\n");
        return 1;
      }

    if((client = cs_gen(&tstations)) == (tclient_struc *) CSCR_PRIVATE)
      {
        fprintf(stderr, "error creating shared memory segment\n");
        return 1;
      }

    pstation = (tclient_station *)((char *)client + client->offsets[0]);
    pcomm = (comstat_rec *) ((char *)client + pstation->comoutoffset);
    pstation->command = CSCM_SUSPEND;
    status = cs_svc(client, 0);
    
    if(status != CSCR_GOOD)
      {
        printf("suspend link: %s\n", comserv_status(status));
        cs_off(client);
        return 1;
      }
      
    pcomm->completion_status = CSCS_IDLE;
    cs_off(client);
    return 0;
  }

