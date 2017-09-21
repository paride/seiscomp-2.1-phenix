/*   Client command handler
     Copyright 1994-2000 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 31 Mar 94 WHO Created
    1 31 Mar 94 WHO Data and blockette buffers merged at client level.
    2  7 Apr 94 WHO Support for local commands added.
    3  9 Apr 94 WHO Remote commands added, one by one.
    4 15 Apr 94 WHO Last two commands added, yeah!
    5 16 Apr 94 WHO Allow CSCM_CAL and CSCM_CHAN commands on shear systems,
                    data will be faked based on config file entries. Set
                    seed format into linkstat.
    6 17 Apr 94 WHO Add code to handle CSQ_LAST and CSQ_TIME options.
    7 30 May 94 WHO Handle comm_mask and new parameters for CSCM_COMM_EVENT.
                    Change method of handling earliest time field for getting
                    packets.
    8  6 Jun 94 WHO Add definitions for privilege bit mask and check them.
    9  9 Jun 94 WHO Cleanup to avoid warnings.
   10 12 Jun 94 WHO PRIV_XXX constants fixed.
   11 18 Aug 94 WHO Fix = instead of == in CSCM_CMD_ACK.
   12 20 Jun 95 WHO CSCM_LINKADJ changed. CSCM_LINKSET added.
   13  3 Oct 95 WHO New definition for reconfigure added.
   14 29 May 96 WHO Start of conversion to run on OS9.
   15  9 Jun 96 WHO Transfer only valid bytes from server to client.
                    Set pshort in unblock call.
   16 15 Jul 96 WHO If there are no Comm Event names, just return a null
                    character at the beginning of the list.
   17  3 Dec 96 WHO Add support for Blockette Queue.
   18 27 Jul 97 WHO Add support for FLOOD control.
   19 17 Oct 97 WHO Add VER_COMMANDS
   20 21 Mar 00 WHO The fix in edition 26 of comlink.c actually started
                    checking the "command_tag" for command echos, which
                    was setup wrong or not at all for most commands.
*/
#include <stdio.h>
#include <errno.h>
#include <string.h>
#ifndef _OSK
#include <unistd.h>
#include <termio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#endif
#include <signal.h>
#include <limits.h>
#include "quanstrc.h"
#include "stuff.h"
#include "service.h"
#include "server.h"
#include "pascal.h"
#ifdef _OSK
#include "os9stuff.h"
#endif

short VER_COMMANDS = 20 ;

extern tuser_privilege user_privilege ;
extern boolean verbose ;
extern boolean insane ;
extern boolean detavail_ok ;
extern boolean detavail_loaded ;
extern boolean stop ;
extern boolean follow_up ;
extern boolean xfer_down_ok ;
extern boolean xfer_up_ok ;
extern byte detavail_seg[14] ;
extern byte cmd_seq ;
extern byte upphase ;
extern linkstat_rec linkstat ;
extern short combusy ;
extern short vcovalue ;
extern short highclient ;
extern short resclient ;
extern short uids ;
extern short down_count ;
extern short mappoll ;
extern short xfer_resends ;
extern short sincemap ;
extern pserver_struc base ;
extern tclients clients[MAXCLIENTS] ;
extern tring rings[NUMQ] ;
extern link_record curlink ;
extern linkstat_rec linkstat ;
extern ultra_type *pultra ;
extern long start_time ;
extern long noackmask ;
extern long blockmask ;
extern long comm_mask ;
extern long polltime ;
extern long reconfig_on_err ;
extern long netto ;
extern long netdly ;
extern long grpsize ;
extern long grptime ;
extern DP_to_DA_msg_type gmsg ;
extern seg_map_type xfer_seg ;
extern download_struc *pdownload ;
extern tupbuf *pupbuf ;
extern string59 xfer_source ;
extern string59 xfer_destination ;
extern unsigned short xfer_total ;
extern unsigned short xfer_size ;
extern unsigned short xfer_up_curseg ;
extern unsigned short seg_size ;
extern unsigned short xfer_segments ;
extern unsigned short xfer_bytes ;
extern int upmemid ;
extern char seedformat[4] ;
extern char seedext ;
extern char station_desc[] ;
extern int data_source ;

/* Define those functions that require privileged access */
#define PRIV_CLIENTS 1
#define PRIV_UNBLOCK 2
#define PRIV_RECONFIGURE 4
#define PRIV_SUSPEND_RESUME 8
#define PRIV_TERMINATE 16
#define PRIV_SHELL 32
#define PRIV_VCO 64
#define PRIV_LINKADJ 128
#define PRIV_MASS_RECENTER 256
#define PRIV_CAL 512
#define PRIV_DET 1024
#define PRIV_REC 2048
#define PRIV_COMM 4096
#define PRIV_DOWNLOAD 8192
#define PRIV_UPLOAD 16384

/* Array to lookup privilege mask given the command */
long privilege [CSCM_UPLOAD_ABORT+1] = {0, 0, 0, 0, 0, 0, 0, 0, PRIV_CLIENTS, PRIV_UNBLOCK,
    PRIV_RECONFIGURE, PRIV_SUSPEND_RESUME, PRIV_SUSPEND_RESUME, 0, PRIV_TERMINATE, PRIV_LINKADJ, 0,
    0, 0, 0, PRIV_SHELL, PRIV_VCO, PRIV_LINKADJ, PRIV_MASS_RECENTER, PRIV_CAL, PRIV_CAL, PRIV_DET, 
    PRIV_DET, PRIV_REC, PRIV_COMM, 0, PRIV_DOWNLOAD, PRIV_DOWNLOAD, PRIV_UPLOAD, PRIV_UPLOAD} ;

void unblock (short clientnum) ;
void do_abort (void) ;
void reconfigure (boolean full) ;
void send_tx_packet (byte nr, byte cmd_var, DP_to_DA_msg_type *msg) ;

  boolean checkcom (comstat_rec *pcom, short clientnum, boolean dadp)
    begin
      if (pcom->completion_status != CSCS_IDLE)
        then
          return TRUE ; /* can't start another one until done */
      if ((dadp) land (combusy != NOCLIENT))
        then
          return TRUE ; /* Can't use the server */
      if (++cmd_seq == 0)
        then
          cmd_seq = 1 ;
      return FALSE ;
    end

  byte handler (pclient_struc svc, short clientnum)
    begin
      typedef void *pvoid ;

      boolean good, goodone, found ;
      short i, j, k ;
      short lowi ;
      pring_elem bscan[NUMQ] ;
      pdata_user pdata ;
      pclient_station client ;
      last_struc *plast ;
      tclients *pt ;
      pselarray psa ;
      seltype *psel ;
      pvoid pv, pv2 ;
      pchar pc1, pc2 ;
      chan_record *pcr2 ;
      chan_struc *pcs ;
      ultra_rec *pur ;
      comstat_rec *pcom ;
      tring_elem *datatemp ;
      shell_com *pshell ;
      short *pshort ;
      long *plong ;
      linkadj_com *plac ;
      recenter_com *prc ;
      cal_start_com *pcsc ;
      det_enable_com *pdec ;
      det_change_com *pdcc ;
      client_info *pci ;
      one_client *poc ;
      rec_enable_com *prec ;
      rec_one *pro ;
      download_com *pdc ;
      download_result *pdr ;
      upload_com *puc ;
      upload_result *pupres ;
      comm_event_com *pcec ;
      linkset_com *plsc ;
      boolean *pflood ;
      long highest[NUMQ] ;
      long lowest, priv ;
      long size, msize ;
      seltype cmp ;
      static seltype any = "?????" ;
      DP_to_DA_msg_type msg ;
      
      pt = &clients[clientnum] ;
      client = (pclient_station) ((long) svc + svc->offsets[svc->curstation]) ;
      pcom = (comstat_rec *) ((long) svc + client->comoutoffset) ;
      pv = &pcom->moreinfo ;
 
/* Check privilege requirements */
      if ((client->command >= CSCM_ATTACH) land (client->command <= CSCM_UPLOAD_ABORT))
        then
          priv = privilege[client->command] ;
        else
          priv = 0 ;
      if (priv)
        then
          begin
            base->server_uid = getuid () ;
            if (base->server_uid != svc->client_uid)
              then
                begin
                  found = FALSE ;
                  for (i = 0 ; i < uids ; i++)
                    if ((user_privilege[i].user_id == svc->client_uid) land
                        ((user_privilege[i].user_mask and priv) != 0))
                      then
                        begin
                          found = TRUE ;
                          break ;
                        end
                  if (lnot found)
                    then
                      return CSCR_PRIVILEGE ;
                end
          end

/* Have permission to proceed */
      switch (client->command)
        begin
          case CSCM_ATTACH : break ;
          case CSCM_DATA_BLK :
            begin
              client->valdbuf = 0 ;
/* If starting fresh, clear pointers and counters */
              if (client->seqdbuf != CSQ_NEXT)
                then
                  begin
                    client->next_data = 0 ;
                    for (i = DATAQ ; i < NUMQ ; i++)
                      begin
                        pt->last[i].scan = NULL ;
                        pt->last[i].packet = -1 ;
                        bscan[i] = rings[i].tail ; /* start at oldest */
                        if (pt->blocking)
                          then
                            while ((bscan[i] != rings[i].head) land
                                   (lnot test_bit(bscan[i]->blockmap, clientnum)))
                              bscan[i] = (pring_elem) bscan[i]->next ;
                      end
                    if ((client->seqdbuf == CSQ_LAST) land (base->next_data > 0))
                      then
                        client->next_data = base->next_data -1 ;
                  end
/* Go through records gotten last time to unblock */
              if (client->seqdbuf != CSQ_FIRST)
                then
                  for (i = DATAQ ; i < NUMQ ; i++)
                    begin
                      plast = &pt->last[i] ;
                      bscan[i] = plast->scan ;
                      if ((bscan[i] == NULL) lor
                          (bscan[i] == rings[i].head) lor
                          (bscan[i]->packet_num != plast->packet))
                        then
                          bscan[i] = rings[i].tail ; /* not valid, start at oldest */
                      while ((bscan[i] != rings[i].head) land
                             (bscan[i]->packet_num < client->next_data))
                        begin
                          if (pt->blocking)
                            then
                              clr_bit (&bscan[i]->blockmap, clientnum) ;
                          bscan[i] = (pring_elem) bscan[i]->next ;
                          if (bscan[i] != rings[i].head)
                            then
                              begin
                                plast->scan = bscan[i] ;
                                plast->packet = bscan[i]->packet_num ;
                              end
                        end
                    end

/* 
   New records that can be transferred to client. They are transferred in the 
   order that they were received, regardless of packet type.
*/
              pdata = (pdata_user) ((long) svc + client->dbufoffset) ;
              while (client->valdbuf < client->reqdbuf)
                begin
                  lowest = LONG_MAX ;
                  for (i = DATAQ ; i < NUMQ ; i++)
                    if ((test_bit(client->datamask, i)) land
                       (bscan[i] != rings[i].head) land
                       (bscan[i]->packet_num < lowest))
                      then
                        begin
                          lowi = i ;
                          lowest = bscan[i]->packet_num ;
                        end
                  if (lowest == LONG_MAX)
                    then
                      break ; /* no more new blockettes */
                  if (test_bit(client->datamask, lowi))
                    then
                      begin
                        good = bscan[lowi]->user_data.header_time >= client->startdbuf ;
                        if (good land (lowi < NUMQ)) /* still good and selectors valid */
                          then
                            begin
                              psa = (pselarray) ((long) svc + client->seloffset) ;
                              good = FALSE ;
                              memcpy((pchar) &cmp, (pchar) &bscan[lowi]->user_data.data_bytes[13], 5) ;
                              for (k = client->sels[lowi].first ; k <= client->sels[lowi].last ; k++)
                                begin
                                  goodone = TRUE ;
#ifdef _OSK
                                  psel = (seltype *) (*psa)[k] ;
#else
                                  psel = &((*psa)[k]) ;
#endif
                                  if (memcmp((pchar) psel, (pchar)&any, 5) != 0)
                                    then
                                      begin
                                        for (j = 0 ; j < 5 ; j++)
                                          if (((*psel)[j] != '?') land ((*psel)[j] != cmp[j]))
                                            then
                                              begin
                                                goodone = FALSE ;
                                                break ;
                                              end
                                      end
                                  if (goodone)
                                    then
                                      begin
                                        good = TRUE ;
                                        break ;
                                      end
                                end
                            end
                        if (good)
                          then
                            begin
                              memset ((pchar) pdata, 0, client->dbufsize) ;
                              memcpy ((pchar) pdata, (pchar) &bscan[lowi]->user_data, rings[lowi].xfersize) ;
                              client->valdbuf++ ;
                              pdata = (pdata_user) ((long) pdata + client->dbufsize) ;
                            end
                      end
                  client->next_data = bscan[lowi]->packet_num + 1 ;
                  bscan[lowi] = (pring_elem) bscan[lowi]->next ;
                end
              client->seqdbuf = CSQ_NEXT ;
              break ;
            end
          case CSCM_LINK :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (lnot linkstat.linkrecv)
                then
                  return CSCR_NODATA ;
              if (client->comoutsize < sizeof(link_record))
                then
                  return CSCR_SIZE ;
              if (checkcom(pcom, clientnum, FALSE))
                then
                  return CSCR_BUSY ;
              memcpy(pv, (pchar) &curlink, sizeof(link_record)) ;
              pcom->command_tag = cmd_seq ;
              pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_CAL :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultrarecv)
                then
                  return CSCR_NODATA ;
              size = sizeof(cal_record) - ((MAXCAL - pultra->calcount) * sizeof(eachcal)) ;
              if (client->comoutsize < size)
                then
                  return CSCR_SIZE ;
              if (checkcom(pcom, clientnum, FALSE))
                then
                  return CSCR_BUSY ;
              pv2 = (pvoid) ((long) pultra + pultra->caloffset) ;
              memcpy(pv, pv2, size) ;
              pcom->command_tag = cmd_seq ;
              pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_DIGI :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (lnot linkstat.ultrarecv)
                then
                  return CSCR_NODATA ;
              if (client->comoutsize < sizeof(digi_record))
                then
                  return CSCR_SIZE ;
              if (checkcom(pcom, clientnum, FALSE))
                then
                  return CSCR_BUSY ;
              memcpy (pv, (pchar) pultra, sizeof(digi_record)) ;
              pcom->command_tag = cmd_seq ;
              pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_ULTRA :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (lnot linkstat.ultrarecv)
                then
                  return CSCR_NODATA ;
              size = sizeof(ultra_rec) - (CE_MAX * (COMMLENGTH + 1)) ;
              msize = 0 ;
              pc1 = (pchar) ((long) pultra + pultra->comoffset) ;
              pc2 = pc1 ;
              for (i = 0 ; i < pultra->comcount ; i++)
                begin
                  msize = msize + *pc1 + 1 ;
                  pc1 = (pchar) ((long) pc1 + *pc1 + 1) ;
                end
              size = size + msize ;
              if (client->comoutsize < size)
                then
                  return CSCR_SIZE ;
              if (checkcom(pcom, clientnum, FALSE))
                then
                  return CSCR_BUSY ;
              pur = (ultra_rec *) pv ;
              pur->vcovalue = vcovalue ;
              pur->pllon = pultra->pllon ;
              pur->umass_ok = pultra->umass_ok ;
              pur->ultra_rev = pultra->ultra_rev ;
              pur->comm_mask = comm_mask ;
              if (pultra->comcount != 0)
                then
                  memcpy ((pchar) &pur->commnames, pc2, msize) ;
                else
                  pur->commnames[0] = '\0' ;
              pcom->command_tag = cmd_seq ;
              pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_LINKSTAT :
            begin
              if (client->comoutsize < sizeof(linkstat_rec))
                then
                  return CSCR_SIZE ;
              msize = 0 ;
              for (j = DATAQ ; j < NUMQ ; j++)
                begin
                  datatemp = rings[j].head ;
                  for (i = 0 ; i < rings[j].count ; i++)
                    begin
                      if (datatemp->blockmap)
                        then
                          msize++ ;
                      datatemp = (pring_elem) datatemp->next ;
                    end
                end
              if (checkcom(pcom, clientnum, FALSE))
                then
                  return CSCR_BUSY ;
              linkstat.blocked_packets = msize ;
              linkstat.seconds_inop = (long) dtime () - start_time ;
              memcpy((pchar) &linkstat.seedformat, (pchar) &seedformat, 4) ;
              linkstat.seedext = seedext ;
              strpas(linkstat.description, station_desc) ;
              linkstat.lsr_sp1 = 0 ;
              linkstat.pollusecs = polltime ;
              linkstat.reconcnt = reconfig_on_err ;
              linkstat.net_idle_to = netto ;
              linkstat.net_conn_dly = netdly ;
              linkstat.grpsize = grpsize ;
              linkstat.grptime = grptime ;
              memcpy(pv, (pchar) &linkstat, sizeof(linkstat_rec)) ;
              pcom->command_tag = cmd_seq ;
              pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_CHAN :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultrarecv)
                then
                  return CSCR_NODATA ;
              if (checkcom(pcom, clientnum, FALSE))
                then
                  return CSCR_BUSY ;
              size = 0 ;
              pcs = (chan_struc *) pv ;
              pcs->chancount = 0 ;
              pcr2 = (chan_record *) ((long) pultra + pultra->usedoffset) ;
              for (i = 0 ; i < pultra->usedcount ; i++)
                begin
                  psa = (pselarray) ((long) svc + client->seloffset) ;
                  good = FALSE ;
                  memcpy((pchar) &cmp, (pchar) &pcr2->seedloc, 2) ;
                  memcpy(&cmp[2], (pchar) &pcr2->seedname, 3) ;
                  for (k = client->sels[CHAN].first ; k <= client->sels[CHAN].last ; k++)
                    begin
                      goodone = TRUE ;
#ifdef _OSK
                      psel = (seltype *) (*psa)[k] ;
#else
                      psel = &((*psa)[k]) ;
#endif
                      if (memcmp((pchar) psel, (pchar) &any, 5) != 0)
                        then
                          begin
                            for (j = 0 ; j < 5 ; j++)
                              if (((*psel)[j] != '?') land ((*psel)[j] != cmp[j]))
                                then
                                  begin
                                    goodone = FALSE ;
                                    break ;
                                  end
                          end
                      if (goodone)
                        then
                          begin
                            good = TRUE ;
                            break ;
                          end
                    end
                  if (good)
                    then
                      begin
                        size = size + sizeof(chan_record) ;
                        if (client->comoutsize < (size + 1))
                          then
                            return CSCR_SIZE ;
                        memcpy ((pchar) &((pcs->chans)[pcs->chancount++]), (pchar) pcr2, sizeof(chan_record)) ;
                      end
                  pcr2++ ;
                end
              pcom->command_tag = cmd_seq ;
              pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_CLIENTS :
            begin
              if (client->comoutsize < (sizeof(one_client) * highclient + 2))
                then
                  return CSCR_SIZE ;
              if (checkcom(pcom, clientnum, FALSE))
                then
                  return CSCR_BUSY ;
              pci = (client_info *) pv ;
              pci->client_count = 0 ;
              for (i = 0 ; i < highclient ; i++)
                begin
                  poc = &pci->clients[pci->client_count++] ;
                  poc->client_memid = clients[i].client_memid ;
                  poc->client_pid = clients[i].client_pid ;
                  poc->client_name.l = clients[i].client_name.l ;
                  poc->last_service = clients[i].last_service ;
                  poc->timeout = clients[i].timeout ;
                  poc->blocking = clients[i].blocking ;
                  poc->active = clients[i].active ;
                  poc->reserved = (i < resclient) ;
                  msize = 0 ;
                  for (j = DATAQ ; j < NUMQ ; j++)
                    begin
                      datatemp = rings[j].head ;
                        for (k = 0 ; k < rings[j].count ; k++)
                          begin
                            if (test_bit(datatemp->blockmap, i))
                              then
                                msize++ ;
                            datatemp = (pring_elem) datatemp->next ;
                         end
                    end
                  poc->block_count = msize ;
                end
              pcom->command_tag = cmd_seq ;
              pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_UNBLOCK :
            begin
              pshort = (short *) ((long) svc + client->cominoffset) ;
              unblock (*pshort) ;
              clr_bit (&blockmask, *pshort) ;
              break ;
            end
          case CSCM_RECONFIGURE :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              reconfigure (TRUE) ;
              break ;
            end
          case CSCM_SUSPEND :
            begin
              set_bit (&noackmask, NUMQ) ;
              linkstat.suspended = TRUE ;
              break ;
            end
          case CSCM_RESUME :
            begin
              clr_bit (&noackmask, NUMQ) ;
              linkstat.suspended = FALSE ;
              break ;
            end
          case CSCM_CMD_ACK :
            begin
              pcom->completion_status = CSCS_IDLE ;
              if (combusy == clientnum)
                then
                  combusy = NOCLIENT ;
              break ;
            end
          case CSCM_DET_REQUEST :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (lnot linkstat.ultrarecv)
                then
                  return CSCR_NODATA ;
              if (checkcom(pcom, clientnum, TRUE))
                then
                  return CSCR_BUSY ;
              psa = (pselarray) ((long) svc + client->seloffset) ;
              msg.drs.cmd_type = DET_REQ ;
              msg.drs.dp_seq = cmd_seq ;
              msg.drs.rc_sp3 = 0 ;
              k = client->sels[CHAN].first ;
#ifdef _OSK
              memcpy ((pchar) &msg.drs.dr_loc, (pchar) (*psa)[k], 2) ;
#else
              memcpy ((pchar) &msg.drs.dr_loc, (pchar) &((*psa)[k]), 2) ;
#endif
              memcpy ((pchar) &msg.drs.dr_name, &((*psa)[k][2]), 3) ;
              combusy = clientnum ;
              clients[clientnum].outbuf = (pchar) pcom ;
              clients[clientnum].outsize = client->comoutsize ;
              detavail_ok = TRUE ;
              detavail_loaded = FALSE ;
              for (i = 0 ; i < 14 ; i++)
                detavail_seg[i] = 0 ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, DET_REQ, &msg) ;
              pcom->completion_status = CSCS_INPROGRESS ;
              break ;
            end
          case CSCM_TERMINATE :
            begin
              if (checkcom(pcom, clientnum, FALSE))
                then
                  return CSCR_BUSY ;
              pcom->command_tag = cmd_seq ;
              pcom->completion_status = CSCS_FINISHED ;
              stop = TRUE ;
              break ;
            end
          case CSCM_SHELL :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              pshell = (shell_com *) ((long) svc + client->cominoffset) ;
              msg.scs.cmd_type = SHELL_CMD ;
              msg.scs.dp_seq = cmd_seq ;
              memcpy ((pchar) &msg.scs.sc, (pchar) &pshell->shell_parameter, 80) ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, SHELL_CMD, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_FLOOD_CTRL :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              pflood = (boolean *) ((long) svc + client->cominoffset) ;
              msg.fcs.cmd_type = FLOOD_CTRL ;
              msg.fcs.dp_seq = cmd_seq ;
              msg.fcs.flood_on_off = *pflood ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, FLOOD_CTRL, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end  
          case CSCM_VCO :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              pshort = (short *) ((long) svc + client->cominoffset) ;
              msg.mmc.cmd_type = AUTO_DAC_CORRECTION ;
              if (*pshort == -1)
                then
                  begin
                    msg.mmc.cmd_parms.param1 = 1 ; /* Enable PLL */
                    pultra->pllon = TRUE ;
                  end
                else
                  begin
                    msg.mmc.cmd_parms.param1 = 0 ; /* Disable PLL */
                    pultra->pllon = FALSE ;
                  end
              msg.mmc.dp_seq = cmd_seq ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, AUTO_DAC_CORRECTION, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              if (*pshort != -1)
                then
                  begin
                    vcovalue = *pshort ;
                    if (linkstat.ultrarecv)
                      then
                        pultra->vcovalue = *pshort ;
                    if (lnot curlink.rcecho)
                      then
                        begin
                          pcom->completion_status = CSCS_IDLE ;
                          if (checkcom(pcom, clientnum, FALSE))
                            then
                              return CSCR_BUSY ;
                        end
                    if (linkstat.ultraon)
                      then
                        begin
                          gmsg.mmc.cmd_type = ACCURATE_DAC_CORRECTION ;
                          gmsg.mmc.cmd_parms.param2 = *pshort >> 8 ;
                          gmsg.mmc.cmd_parms.param3 = *pshort and 255 ;
                        end
                      else
                        begin
                          gmsg.mmc.cmd_type = MANUAL_CORRECT_DAC ;
                          gmsg.mmc.cmd_parms.param2 = *pshort >> 4 ;
                        end
                    gmsg.mmc.dp_seq = cmd_seq ;
                    if (curlink.rcecho)
                      then
                        follow_up = TRUE ;
                      else
                        begin
                          pcom->command_tag = cmd_seq ;
                          send_tx_packet (0, ACCURATE_DAC_CORRECTION, &gmsg) ;
                          pcom->completion_status = CSCS_FINISHED ;
                        end
                  end
              break ;
            end
          case CSCM_LINKADJ :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              plac = (linkadj_com *) ((long) svc + client->cominoffset) ;
              msg.las.cmd_type = LINK_ADJ ;
              msg.las.dp_seq = cmd_seq ;
              memcpy ((pchar) &msg.las.la, (pchar) plac, sizeof(linkadj_com)) ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, LINK_ADJ, &msg) ;
              curlink.window_size = plac->window_size ;
              curlink.msg_prio = plac->set_msg ;
              curlink.det_prio = plac->set_det ;
              curlink.time_prio = plac->set_time ;
              curlink.cal_prio = plac->set_calp ;
              curlink.resendtime = plac->resendtime ;
              curlink.synctime = plac->synctime ;
              curlink.resendpkts = plac->resendpkts ;
              curlink.netdelay = plac->netdelay ;
              curlink.nettime = plac->nettime ;
              curlink.netmax = plac->netmax ;
              curlink.groupsize = plac->groupsize ;
              curlink.grouptime = plac->grouptime ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_MASS_RECENTER :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              prc = (recenter_com *) ((long) svc + client->cominoffset) ;
              if (linkstat.ultraon)
                then
                  begin
                    msg.ums.cmd_type = ULTRA_MASS ;
                    msg.ums.dp_seq = cmd_seq ;
                    msg.ums.mbrd = prc->board ;
                    msg.ums.mdur = prc->duration ;
                  end
                else
                  begin
                    msg.mmc.cmd_type = MASS_RECENTERING ;
                    msg.mmc.dp_seq = cmd_seq ;
                  end
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, msg.ums.cmd_type, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_CAL_START :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              pcsc = (cal_start_com *) ((long) svc + client->cominoffset) ;
              if (linkstat.ultraon)
                then
                  begin
                    msg.ucs.cmd_type = ULTRA_CAL ;
                    msg.ucs.dp_seq = cmd_seq ;
                    msg.ucs.rc_sp2 = 0 ;
                    memcpy ((pchar) &msg.ucs.xc, (pchar) pcsc, sizeof(cal_start_com)) ;
                  end
                else
                  begin
                    msg.mmc.cmd_type = START_CAL ;
                    msg.mmc.dp_seq = cmd_seq ;
                    msg.mmc.cmd_parms.param0 = 0 ;
                    msg.mmc.cmd_parms.param1 = 0x5a ;
                    j = pcsc->map << ((pcsc->calnum - 1) * 3) ;
                    switch (pcsc->calcmd)
                      begin
                        case SINE :
                          begin
                            msg.mmc.cmd_parms.param2 = 0x10 + (j and 7) ;
                            msg.mmc.cmd_parms.param4 = pcsc->sfrq - 3 ;
                            break ;
                          end
                        case STEP :
                          begin
                            msg.mmc.cmd_parms.param2 = 0x30 + (j and 7) ;
                            msg.mmc.cmd_parms.param4 = 0 ;
                            break ;
                          end
                        case RAND :
                          begin
                            msg.mmc.cmd_parms.param2 = 0x70 + (j and 7) ;
                            msg.mmc.cmd_parms.param4 = pcsc->rmult ;
                            break ;
                          end
                        case WRAND :
                          begin
                            msg.mmc.cmd_parms.param2 = 0x70 + (j and 7) ;
                            msg.mmc.cmd_parms.param4 = 0 - pcsc->rmult ;
                            break ;
                          end
                      end
                    msg.mmc.cmd_parms.param3 = (pcsc->amp div 6) + 17 ;
                    msg.mmc.cmd_parms.param6 = pcsc->duration div 256 ;
                    msg.mmc.cmd_parms.param5 = pcsc->duration and 255 ;
                    msg.mmc.cmd_parms.param7 = ((lnot pcsc->plus) << 1) +
                                          pcsc->capacitor + (j and 0x38) ;
                  end
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, msg.ucs.cmd_type, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_CAL_ABORT :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              pshort = (short *) ((long) svc + client->cominoffset) ;
              if (linkstat.ultraon)
                then
                  begin
                    msg.uss.cmd_type = ULTRA_STOP ;
                    msg.uss.dp_seq = cmd_seq ;
                    msg.uss.sbrd = *pshort ;
                  end
                else
                  begin
                    msg.mmc.cmd_type = STOP_CAL ;
                    msg.mmc.dp_seq = cmd_seq ;
                    msg.mmc.cmd_parms.param0 = 0 ;
                  end
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, msg.ums.cmd_type, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_DET_ENABLE :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              pdec = (det_enable_com *) ((long) svc + client->cominoffset) ;
              msg.des.cmd_type = DET_ENABLE ;
              msg.des.dp_seq = cmd_seq ;
              msg.des.rc_sp4 = 0 ;
              memcpy ((pchar) &msg.des.de, (pchar) pdec, sizeof(det_enable_com)) ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, DET_ENABLE, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_DET_CHANGE :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              pdcc = (det_change_com *) ((long) svc + client->cominoffset) ;
              msg.dcs.cmd_type = DET_CHANGE ;
              msg.dcs.dp_seq = cmd_seq ;
              msg.dcs.rc_sp5 = 0 ;
              memcpy ((pchar) &msg.dcs.dc, (pchar) pdcc, sizeof(det_change_com)) ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, DET_CHANGE, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_REC_ENABLE :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              prec = (rec_enable_com *) ((long) svc + client->cominoffset) ;
              msg.res.cmd_type = REC_ENABLE ;
              msg.res.dp_seq = cmd_seq ;
              msg.res.rc_sp6 = 0 ;
              memcpy ((pchar) &msg.res.re, (pchar) prec, sizeof(rec_enable_com)) ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, REC_ENABLE, &msg) ;
              for (j = 0 ; j < prec->count ; j++)
                begin
                  pro = &(prec->changes[j]) ;
                  pcr2 = (chan_record *) ((long) pultra + pultra->usedoffset) ;
                  for (i = 0 ; i < pultra->usedcount ; i++)
                    begin
                      if ((memcmp((pchar) &pro->seedname, (pchar) &pcr2->seedname, 3) == 0) land
                          (memcmp((pchar) &pro->seedloc, (pchar) &pcr2->seedloc, 2) == 0))
                        then
                          begin
                            pcr2->enabled = pro->mask ;
                            pcr2->c_prio = pro->c_prio ;
                            pcr2->e_prio = pro->e_prio ;
                            break ;
                          end
                        else
                          pcr2++ ;
                    end
                end
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_COMM_EVENT :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, curlink.rcecho))
                then
                  return CSCR_BUSY ;
              pcec = (comm_event_com *) ((long) svc + client->cominoffset) ;
              msg.ces.cmd_type = COMM_EVENT ;
              msg.ces.dp_seq = cmd_seq ;
              msg.ces.rc_sp1 = 0 ;
              comm_mask = (comm_mask and ((not pcec->remote_mask) or pcec->remote_map)) or
                           (pcec->remote_mask and pcec->remote_map) ;
              msg.ces.mask = comm_mask ;
              plong = (long *) pv ;
              *plong = comm_mask ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, COMM_EVENT, &msg) ;
              if (curlink.rcecho)
                then
                  begin
                    combusy = clientnum ;
                    clients[clientnum].outbuf = (pchar) pcom ;
                    pcom->completion_status = CSCS_INPROGRESS ;
                  end
                else
                  pcom->completion_status = CSCS_FINISHED ;
              break ;
            end
          case CSCM_DOWNLOAD :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, TRUE))
                then
                  return CSCR_BUSY ;
              /* As a precaution, send abort message first */
              msg.downs.cmd_type = DOWN_ABT ;
              msg.downs.dp_seq = cmd_seq ;
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, DOWN_ABT, &msg) ;
              pdc = (download_com *) ((long) svc + client->cominoffset) ;
              pdr = (download_result *) pv ;
              pdr->dpshmid = NOCLIENT ;
              pdr->fsize = 0 ;
              pdr->byte_count = 0 ;
              msg.downs.cmd_type = DOWN_REQ ;
              msg.downs.dp_seq = cmd_seq ;
              memcpy((pchar) &msg.downs.fname, pdc->dasource, 60) ; /* da file name */
              memcpy((pchar) &xfer_source, pdc->dasource, 60) ; /* keep copy for comparison */
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, DOWN_REQ, &msg) ;
              memset((pchar) &xfer_seg, '\0', 128) ; /* clear map */
              pdownload = NULL ;
              xfer_down_ok = TRUE ;
              down_count = 0 ;
              xfer_total = 0 ;
              xfer_size = 0 ;
              combusy = clientnum ;
              clients[clientnum].outbuf = (pchar) pcom ;
              pcom->completion_status = CSCS_INPROGRESS ;
              break ;
            end
          case CSCM_DOWNLOAD_ABORT :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (xfer_down_ok)
                then
                  do_abort () ;
              if (pcom->completion_status == CSCS_IDLE)
                then
                  begin
                    if (checkcom(pcom, clientnum, FALSE))
                      then
                        return CSCR_BUSY ;
                    pcom->completion_status = CSCS_FINISHED ;
                  end
              break ;
            end
          case CSCM_UPLOAD :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (checkcom(pcom, clientnum, TRUE))
                then
                  return CSCR_BUSY ;
              puc = (upload_com *) ((long) svc + client->cominoffset) ;
              pupres = (upload_result *) pv ;
              pupres->bytecount = 0 ;
              pupres->retries = -1 ;
              pupbuf = (tupbuf *) shmat (puc->dpshmid, NULL, 0) ;
              if ((long) pupbuf == ERROR)
                then
                  return CSCR_PRIVATE ;
              upmemid = puc->dpshmid ;
              msg.us.cmd_type = UPLOAD ;
              msg.us.dp_seq = cmd_seq ;
              msg.us.return_map = TRUE ;
              msg.us.upload_control = CREATE_UPLOAD ;
              msg.us.up_union.up_create.file_size = puc->fsize ;
              memcpy((pchar) &msg.us.up_union.up_create.file_name, puc->dadest, 60) ; /* da file name */
              memcpy((pchar) &xfer_destination, puc->dadest, 60) ; /* keep copy for comparison */
              pcom->command_tag = cmd_seq ;
              send_tx_packet (0, UPLOAD, &msg) ;
              memset((pchar) &xfer_seg, '\0', 128) ; /* clear map */
              upphase = WAIT_CREATE_OK ;
              xfer_up_curseg = 0 ;
              seg_size = DP_TO_DA_MESSAGE_LENGTH - 10 ;
              xfer_size = puc->fsize ;
              xfer_segments = (unsigned int) (xfer_size + (seg_size - 1)) div (unsigned int) seg_size ;
              xfer_up_ok = TRUE ;
              xfer_total = xfer_size ;
              xfer_bytes = 0 ;
              sincemap = 0 ;
              mappoll = 30 ;
              xfer_resends = -1 ;
              combusy = clientnum ;
              clients[clientnum].outbuf = (pchar) pcom ;
              pcom->completion_status = CSCS_INPROGRESS ;
              break ;
            end
          case CSCM_UPLOAD_ABORT :
            begin
              if (data_source != SRC_COMLINK) return CSCR_INVALID ;
              if (lnot linkstat.ultraon)
                then
                  return CSCR_INVALID ;
              if (xfer_up_ok)
                then
                  do_abort () ;
              if (pcom->completion_status == CSCS_IDLE)
                then
                  begin
                    if (checkcom(pcom, clientnum, FALSE))
                      then
                        return CSCR_BUSY ;
                    pcom->completion_status = CSCS_FINISHED ;
                  end
              break ;
            end
          case CSCM_LINKSET :
            begin
              plsc = (linkset_com *) ((long) svc + client->cominoffset) ;
              polltime = plsc->pollusecs ;
              reconfig_on_err = plsc->reconcnt ;
              netto = plsc->net_idle_to ;
              netdly = plsc->net_conn_dly ;
              grpsize = plsc->grpsize ;
              grptime = plsc->grptime ;
              break ;
            end
          default :
            begin
              return CSCR_INVALID ;
            end
        end
      return CSCR_GOOD ;
    end

