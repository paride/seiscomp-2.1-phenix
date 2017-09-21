/*   Server comlink protocol file
     Copyright 1994-1999 Quanterra, Inc.
     Written by Woodrow H. Owens

Edit History:
   Ed Date      By  Changes
   -- --------- --- ---------------------------------------------------
    0 23 Mar 94 WHO First created.
    1  9 Apr 94 WHO Command Echo processing added.
    2 15 Apr 94 WHO Commands done.
    3 30 May 94 WHO If rev 1 or higher ultra record received, set comm_mask.
    4  9 Jun 94 WHO Consider a "busy" client still active if it is a
                    foreign client, without trying to do a Kill (0).
                    Cleanup to avoid warnings.
    5 11 Jun 94 WHO Martin-Marietta support added.
    6  9 Aug 94 WHO Set last_good and last_bad fields in linkstat.
    7 11 Aug 94 WHO Add support for network.
    8 25 Sep 94 WHO Allow receiving RECORD_HEADER_3.
    9  3 Nov 94 WHO Use SOLARIS2 definition to alias socket parameter.
   10 13 Dec 94 WHO Remove record size in seedheader function, COMSERV always
                    uses 512 byte blocks.
   11 16 Jun 95 WHO Don't try to open closed network connection until
                    netdly_cnt reaches netdly. Clear netto_cnt timeout
                    counter when a packet is received.
   12 20 Jun 95 WHO Updates due to link adj and link record packets. Transmitted
                    packet circular buffer added to accomodate grouping.
   13 15 Aug 95 WHO Set frame_count for Q512 and MM256 data packets.
   14  2 Oct 95 WHO Implement new link_pkt/ultra_req handshaking protocol.
   15 28 Jan 96 DSN Update check_input to handle unexpected characters better.
                    Correctly assign state when unexpected character is found.
   16  3 Jun 96 WHO Start of conversion to OS9
   17  4 Jun 96 WHO Comparison for dbuf.seq being positive removed, seq
                    is unsigned. cli_addr, network, and station made external.
   18  7 Jun 96 WHO Check result of "kill" with ERROR, not zero.
   19 13 Jun 96 WHO Adjust seedname and location fields for COMMENTS.
   20  3 Aug 96 WHO If anystation flag is on, don't check station, and show what
                    station came from. If noultra is on, don't poll for ultra
                    packets.
   21  7 Dec 96 WHO Add support for Blockettes and UDP.
   22 11 Jun 97 WHO Clear out remainder of packets that are received with
                    less than 512 bytes. Convert equivalent of header_flag
                    in blockette packet to seed sequence number.
   23 27 Jul 97 WHO Handle FLOOD_PKT.
   24 17 Oct 97 WHO Add VER_COMLINK
   25 23 Dec 98 WHO Use link_retry instead of a fixed 10 seconds for
                    polling for link packets (VSAT).
   26  4 Nov 99 WHO Change "=" to "==" in cmd_echo processing.
*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#ifndef _OSK
#include <unistd.h>
#include <termio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#else
#include <sgstat.h>
#include <sg_codes.h>
#include "os9inet.h"
#include "os9stuff.h"
#endif
#include "quanstrc.h"
#include "stuff.h"
#include "service.h"
#include "cfgutil.h"
#include "timeutil.h"
#include "seedutil.h"
#include "server.h"
#include "pascal.h"

short VER_COMLINK = 26 ;

extern seed_net_type network ;
extern complong station ;
long reconfig_on_err = 25 ;
boolean anystation = FALSE ;

extern boolean noultra ;
extern pchar src, srcend, dest, destend, term ;
extern unsigned char sbuf[BLOB] ;
extern DA_to_DP_buffer dbuf ;
extern byte last_packet_received ;
extern byte lastchar ;
extern int path ;
extern int sockfd ;
extern struct sockaddr_in cli_addr ;
extern int upmemid ;
extern DP_to_DA_buffer temp_pkt ;
extern tcrc_table crctable ;
extern tclients clients[MAXCLIENTS] ;
extern pserver_struc base ;

extern byte inphase ;
extern byte upphase ;
extern long seq ;
extern long comm_mask ;
extern boolean verbose ;
extern boolean rambling ;
extern boolean insane ;
extern linkstat_rec linkstat ;
extern boolean detavail_ok ;
extern boolean detavail_loaded ;
extern boolean first ;
extern boolean firstpacket ;
extern boolean seq_valid ;
extern boolean xfer_down ;
extern boolean xfer_down_ok ;
extern boolean xfer_up_ok ;
extern boolean map_wait ;
extern boolean ultra_seg_empty ;
extern boolean override ;
extern boolean follow_up ;
extern boolean serial ;
extern boolean udplink ;
extern boolean notify ;
extern short ultra_percent ;
extern unsigned short lowest_seq ;
extern short linkpoll ;
extern long con_seq ;
extern long netdly_cnt ;
extern long netdly ;
extern long netto_cnt ;
extern long grpsize ;
extern long grptime ;
extern long link_retry ;
extern double last_sent ;
extern short combusy ;
extern byte cmd_seq ;
extern link_record curlink ;
extern string59 xfer_destination ;
extern string59 xfer_source ;
extern byte ultra_seg[14] ;
extern byte detavail_seg[14] ;
extern seg_map_type xfer_seg ;
extern unsigned short seg_size ;
extern unsigned short xfer_size ;
extern unsigned short xfer_up_curseg ;
extern unsigned short xfer_bytes ;
extern unsigned short xfer_total ;
extern unsigned short xfer_last ;
extern unsigned short xfer_segments ;
extern unsigned short cal_size ;
extern unsigned short used_size ;
extern unsigned short ultra_size ;
extern unsigned short detavail_size ;
extern cal_record *pcal ;
extern short sequence_mod ;
extern short vcovalue ; 
extern short xfer_resends ;
extern short mappoll ;
extern short sincemap ;
extern short down_count ;
extern short minctr ;
extern short txwin ;
extern download_struc *pdownload ;     
extern ultra_type *pultra ;
extern tupbuf *pupbuf ;
DP_to_DA_msg_type replybuf ;
extern DP_to_DA_msg_type gmsg ;
 
extern string3 seed_names[20][7] ;
extern location_type seed_locs[20][7] ;
 
static byte this_packet ;

static int maxbytes = 0 ;

long julian (time_array *gt) ;
pchar time_string (double jul) ;
tring_elem *getbuffer (short qnum) ;
boolean bufavail (short qnum) ;
boolean checkmask (short qnum) ;
signed char encode_rate (short rate) ;

static short seq_seen[DEFAULT_WINDOW_SIZE] = { 0,0,0,0,0,0,0,0};

typedef struct sockaddr *psockaddr ;

typedef struct
  begin
    byte cmd ;              /* command number */
    byte ack ;              /* acknowledgement of packet n */
    byte dpseq ;            /* sequence for DP to DA commands */
    char leadin ;           /* leadin character */
    short len ;             /* Length of message */
    DP_to_DA_msg_type buf ; /* buffer for the raw message */
  end txbuf_type ;
  
typedef union
  begin
    unsigned char b[2] ;
    signed char sb[2] ;
    be_int16_t s ;
  end be_compword ;

typedef union
  begin
    unsigned char b[4] ;
    signed char sb[4] ;
    be_int16_t s[2] ;
    be_int32_t l ;
    be_float32_t f ;
  end be_complong ;

static short nextin = 0 ;
static short nextout = 0 ;
static txbuf_type txbuf[64] ;

ssize_t writen(int fd, const void *vptr, size_t n)
  {
    ssize_t nwritten;
    size_t nleft = n;
    const char *ptr = (const char *) vptr;   
    int loop = 0;
    
    while (nleft > 0)
      {
        if ((nwritten = write(fd, ptr, nleft)) == 0)
            return 0;

        if(nwritten < 0)
          {
            if(errno == EAGAIN && (++loop < 1000))
              {
                usleep(1);
                continue;
              }

            return -1;
          }
        
        if(loop > 0 && rambling)
            printf("writen: successful after %d tries\n", loop + 1);

        loop = 0;
    
        nleft -= nwritten;
        ptr += nwritten;
      }
    
    return(n);
  }

  void gcrcinit (void)
    begin
      short count, bits ;
      long tdata, accum ;

      for (count = 0 ; count < 256 ; count++)
        begin
          tdata = ((long) count) << 24 ;
          accum = 0 ;
          for (bits = 1 ; bits <= 8 ; bits++)
            begin
              if ((tdata eor accum) < 0)
                then
                  accum = (accum << 1) eor CRC_POLYNOMIAL ;
                else
                  accum = (accum << 1) ;
              tdata = tdata << 1 ;
            end
          crctable[count] = accum ;
        end
    end

  long gcrccalc (pchar b, short len)
    begin
      be_complong crc ;

      crc.l = 0 ;
      while (len-- > 0)
        crc.l = (crc.l << 8) eor crctable[(crc.b[0] eor *b++) and 255] ;
      return crc.l ;
    end

  unsigned short checksum (pchar addr, short size)
    begin
      unsigned short ck ;

      ck = 0 ;
      while (size-- > 0)
          ck = ck + ((short) *addr++ and 255) ;
      return ck ;
    end

  void send_window (void)
    begin
      short i, len ;
      int numwrit ;
      pchar ta, tstr ;
      char transmit_buf[2 * sizeof(DP_to_DA_buffer) + 2] ;
      be_complong ltemp ;
      byte b ;
      txbuf_type *cur ;
 
      while (txwin > 0)
        begin
          cur = &txbuf[nextout] ;
          temp_pkt.c.cmd = cur->cmd ;
          temp_pkt.c.ack = cur->ack ;
          temp_pkt.msg = cur->buf ;
          temp_pkt.msg.scs.dp_seq = cur->dpseq ;
          transmit_buf[0] = cur->leadin ;
          len = cur->len ;
          ta = (pchar) ((long) &temp_pkt + 6) ;
          temp_pkt.c.chksum = checksum(ta, len - 6) ;
          ltemp.l = gcrccalc(ta, len - 6) ;
          temp_pkt.c.crc = ltemp.s[0] ;
          temp_pkt.c.crc_low = ltemp.s[1] ;
          if (udplink)
            then
              numwrit = sendto (path, (pchar) &temp_pkt, len, 0,
                        (psockaddr) &cli_addr, sizeof(cli_addr)) ;
            else
              begin
                tstr = &transmit_buf[1] ;
                ta = (pchar) &temp_pkt ;
                for (i = 0 ; i < len ; i++)
                  begin
                    b = (*ta >> 4) and 15 ;
                    if (b > 9)
                      then
                        *tstr++ = b + 55 ;
                      else
                        *tstr++ = b + 48 ;
                    b = *ta++ and 15 ;
                    if (b > 9)
                      then
                        *tstr++ = b + 55 ;
                      else
                        *tstr++ = b + 48 ;
                  end
                end
          txwin-- ;
          if (insane)
            then
              if (cur->cmd == ACK_MSG)
                then
                  printf ("Acking packet %d from slot %d, packets queued=%d\n", cur->ack, nextout, txwin) ;
                else
                  printf ("Sending command %d from slot %d, packets queued=%d\n", ord(cur->cmd), nextout, txwin) ;
           nextout = ++nextout and 63 ;
           if ((path >= 0) land lnot udplink)
            then
              begin
                numwrit = writen(path, (pchar) &transmit_buf, len * 2 + 1) ;
                if ((numwrit < 0) land (verbose))
                  then
                    perror ("Error writing to port ") ;
              end
          last_sent = dtime () ;
        end
    end

  void send_tx_packet (byte nr, byte cmd_var, DP_to_DA_msg_type *msg)
    begin
      txbuf_type *cur ;
      
      cur = &txbuf[nextin] ;
      if (cmd_var == ACK_MSG)
        then
          begin
            cur->cmd = cmd_var ;
            cur->ack = nr ;
            cur->leadin = LEADIN_ACKNAK ;
            cur->len = DP_TO_DA_LENGTH_ACKNAK ;
            cur->dpseq = 0 ;
          end
        else
          begin
            cur->cmd = cmd_var ;
            cur->ack = last_packet_received ;
            cur->buf = *msg ;
            cur->dpseq = cmd_seq++ ;
            cur->len = DP_TO_DA_LENGTH_CMD ;
            cur->leadin = LEADIN_CMD ;
          end ;
      txwin++ ;
      if (insane)
        then
          printf ("Placing outgoing packet in slot %d, total in window=%d\n", nextin, txwin) ;
      nextin = ++nextin and 63 ;
      if (txwin >= grpsize)
        then
          send_window () ;
    end
    
  void send_ack (void)
    begin
      DP_to_DA_msg_type msg ;

      last_packet_received = this_packet ;
      send_tx_packet (this_packet, ACK_MSG, &msg) ;
    end

  void request_ultra (void)
    begin
      DP_to_DA_msg_type mmsg ;

      if (path < 0)
        then
          return ;
      mmsg.scs.cmd_type = ULTRA_REQ ;
      send_tx_packet (0, ULTRA_REQ, &mmsg) ;
      if (rambling)
        then
          printf ("Requesting ultra packet\n") ;
    end

  void request_link (void)
    begin
      DP_to_DA_msg_type mmsg ;

      linkpoll = 0 ;
      if (path < 0)
        then
          return ;
      mmsg.scs.cmd_type = LINK_REQ ;
      send_tx_packet (0, LINK_REQ, &mmsg) ;
      if (rambling)
        then
          printf ("Requesting link packet\n") ;
    end

  void request_map (void)
    begin
      DP_to_DA_msg_type mmsg ;

      mmsg.us.cmd_type = UPLOAD ;
      mmsg.us.dp_seq = cmd_seq ;
      mmsg.us.return_map = TRUE ;
      if (upphase == WAIT_CREATE_OK)
        then
          begin
            mmsg.us.upload_control = CREATE_UPLOAD ;
            mmsg.us.up_union.up_create.file_size = xfer_size ;
            memcpy (mmsg.us.up_union.up_create.file_name, xfer_destination, 60) ;
          end
        else
          mmsg.us.upload_control = MAP_ONLY ;
      send_tx_packet (0, UPLOAD, &mmsg) ;
      mappoll = 30 ;
    end

  void reconfigure (boolean full)
    begin
      short i ;

      if (full)
        then
          begin
            linkstat.linkrecv = FALSE ;
            seq_valid = FALSE ;
            lowest_seq = 300 ;
            con_seq = 0 ;
            linkpoll = 0 ;
            request_link () ;
          end
       if (linkstat.ultraon)
        then
          begin
            linkstat.ultrarecv = FALSE ;
            ultra_seg_empty = TRUE ;
            for (i = 0 ; i < 14 ; i++)
              ultra_seg[i] = 0 ;
            if (ultra_size)
              then
                begin
                  free(pultra) ;
                  ultra_size = 0 ;
                end
            ultra_percent = 0 ;
            if (lnot full)
              then
                request_ultra () ;
          end
        else
          for (i = 0 ; i < DEFAULT_WINDOW_SIZE ; i++)
            seq_seen[i] = 0 ;
    end

  void clearmsg (DP_to_DA_msg_type *m)
    begin
      m->mmc.dp_seq = 0 ;
      m->mmc.cmd_parms.param0 = 0 ;
      m->mmc.cmd_parms.param1 = 0 ;
      m->mmc.cmd_parms.param2 = 0 ;
      m->mmc.cmd_parms.param3 = 0 ;
      m->mmc.cmd_parms.param4 = 0 ;
      m->mmc.cmd_parms.param5 = 0 ;
      m->mmc.cmd_parms.param6 = 0 ;
      m->mmc.cmd_parms.param7 = 0 ;
    end
    
  boolean checkbusy (void) /* returns TRUE if client is foreign or alive */
    begin
      return (combusy != NOCLIENT) land 
         ((clients[combusy].client_address->client_uid != base->server_uid) lor
#ifdef _OSK
          (kill(clients[combusy].client_pid, SIGWAKE) != ERROR)) ;
#else
          (kill(clients[combusy].client_pid, 0) != ERROR)) ;
#endif
    end

  void do_abort (void)
    begin
      DP_to_DA_msg_type mmsg ;
      comstat_rec *pcom ;
      download_result *pdr ;

      if (xfer_up_ok)
        then
          begin
            xfer_up_ok = FALSE ;
            clearmsg (&mmsg) ;
            mmsg.us.cmd_type = UPLOAD ;
            mmsg.us.dp_seq = cmd_seq ;
            mmsg.us.return_map = FALSE ;
            mmsg.us.upload_control = ABORT_UPLOAD ;
            send_tx_packet (0, UPLOAD, &mmsg) ;
            clearmsg (&mmsg) ;
            mmsg.us.cmd_type = UPLOAD ;
            mmsg.us.dp_seq = cmd_seq ;
            mmsg.us.return_map = FALSE ;
            mmsg.us.upload_control = ABORT_UPLOAD ;
            send_tx_packet (0, UPLOAD, &mmsg) ;
            shmdt((pchar)pupbuf) ;
            if (checkbusy ())
              then
                begin
                  pcom = (comstat_rec *) clients[combusy].outbuf ;
                  if (upmemid != NOCLIENT)
                    then
                      begin
                        shmctl(upmemid, IPC_RMID, NULL) ;
                        upmemid = NOCLIENT ;
                      end
                  if (pcom->completion_status == CSCS_INPROGRESS)
                    then
                      pcom->completion_status = CSCS_ABORTED ;
                  combusy = NOCLIENT ;
                end
            xfer_size = 0 ;
          end ;
      if (xfer_down_ok)
        then
          begin
            xfer_down_ok = FALSE ;
            clearmsg (&mmsg) ;
            mmsg.scs.cmd_type = DOWN_ABT ;
            send_tx_packet (0, DOWN_ABT, &mmsg) ;
            clearmsg (&mmsg) ;
            mmsg.scs.cmd_type = DOWN_ABT ;
            send_tx_packet (0, DOWN_ABT, &mmsg) ;
            if (pdownload != NULL)
              then
                shmdt((pchar)pdownload) ;
            if (checkbusy ())
              then
                begin
                  pcom = (comstat_rec *) clients[combusy].outbuf ;
                  pdr = (download_result *) &pcom->moreinfo ;
                  if (pdr->dpshmid != NOCLIENT)
                    then
                      begin
                        shmctl(pdr->dpshmid, IPC_RMID, NULL) ;
                        pdr->dpshmid = NOCLIENT ;
                      end
                  if (pcom->completion_status == CSCS_INPROGRESS)
                    then
                      pcom->completion_status = CSCS_ABORTED ;
                  combusy = NOCLIENT ;
                end
          end
    end

  void next_segment (void)
    begin
      DP_to_DA_msg_type mmsg ;
      unsigned short i ;
      unsigned short h ;
      pchar p1, p2 ;
      boolean allsent ;
      unsigned short off, cnt ; 
      comstat_rec *pcom ;
      upload_result *pupres ;

      clearmsg (&mmsg) ;
      mmsg.scs.cmd_type = UPLOAD ;
      mmsg.us.return_map = FALSE ;
      mmsg.us.upload_control = SEND_UPLOAD ;
      if (checkbusy ())
        then
          begin
            pcom = (comstat_rec *) clients[combusy].outbuf ;
            pupres = (upload_result *) &pcom->moreinfo ;
          end
        else
          begin
            do_abort () ;
            return ;
          end
      if (upphase == SENDING_UPLOAD)
        then
          begin
            i = xfer_up_curseg ;
            allsent = TRUE ;
            while (i < xfer_segments)
              begin
                if (((xfer_seg[i div 8]) and ((byte) (1 << (i mod 8)))) == 0)
                  then
                    begin
                      allsent = FALSE ;
                      off = seg_size * i ;
                      mmsg.us.up_union.up_send.byte_offset = off ;
                      xfer_up_curseg = i + 1 ;
                      mmsg.us.up_union.up_send.seg_num = xfer_up_curseg ;
                      if (((unsigned int) off + (unsigned int) seg_size) >= (unsigned int) xfer_size)
                        then
                          begin
                            cnt = xfer_size - off ;
                            xfer_up_curseg = 0 ;
                            mmsg.us.return_map = TRUE ;
                            mappoll = 30 ;
                            sincemap = 0 ;
                            upphase = WAIT_MAP ;
                            if (xfer_resends < 0)
                              then
                                xfer_resends = 0 ;
                          end
                        else
                          begin
                            cnt = seg_size ;
                            if (++sincemap >= 10)
                              then
                                begin
                                  mmsg.us.return_map = TRUE ;
                                  mappoll = 30 ;
                                  sincemap = 0 ;
                                  upphase = WAIT_MAP ;
                                end
                          end ;
                      mmsg.us.up_union.up_send.byte_count = cnt ;
                      p1 = (pchar) ((long) pupbuf + off) ;
                      p2 = (pchar) &mmsg.us.up_union.up_send.bytes ;
                      memcpy(p2, p1, cnt) ;
                      xfer_bytes = xfer_bytes + cnt ;
                      if (xfer_bytes > xfer_total)
                        then
                          xfer_bytes = xfer_total ;
                      if (xfer_resends >= 0)
                        then
                          xfer_resends++ ;
                      pupres->bytecount = xfer_bytes ;
                      pupres->retries = xfer_resends ;
                      send_tx_packet (0, UPLOAD, &mmsg) ;
                      break ;
                    end
                i++ ;
              end
            if (allsent)
              then
                for (i = 0 ; i < xfer_segments ; i++)
                  if (((xfer_seg[i div 8]) and ((byte)(1 << (i mod 8)))) == 0)
                    then
                      begin
                        xfer_up_curseg = 0 ;
                        upphase = WAIT_MAP ;
                        request_map () ;
                        return ;
                      end
            if (allsent)
              then
                begin
                  xfer_up_ok = FALSE ;
                  upphase = UP_IDLE ;
                  mmsg.us.upload_control = UPLOAD_DONE ;
                  xfer_size = 0 ;
                  shmdt((pchar)pupbuf) ;
                  pcom->completion_status = CSCS_FINISHED ;
                  combusy = NOCLIENT ;
                  send_tx_packet (0, UPLOAD, &mmsg) ;
                end
          end
    end

  void process_upmap (void)
    begin
      comstat_rec *pcom ;
      
      mappoll = 0 ;
      switch (upphase)
        begin
          case WAIT_CREATE_OK :
            begin
              if (dbuf.data_buf.cu.upload_ok)
                then
                  begin
                    upphase = SENDING_UPLOAD ;
                    memcpy ((pchar) &xfer_seg, (pchar) &dbuf.data_buf.cu.segmap, sizeof(seg_map_type)) ;
                  end
                else
                  begin
                    upphase = UP_IDLE ;
                    xfer_up_ok = FALSE ;
                    shmdt((pchar)pupbuf) ;
                    if (checkbusy ())
                      then
                        begin
                          pcom = (comstat_rec *) clients[combusy].outbuf ;
                          if (upmemid != NOCLIENT)
                            then
                              begin
                                shmctl(upmemid, IPC_RMID, NULL) ;
                                upmemid = NOCLIENT ;
                              end
                          if (pcom->completion_status == CSCS_INPROGRESS)
                            then
                              pcom->completion_status = CSCS_CANT ;
                          combusy = NOCLIENT ;
                        end
                    xfer_size = 0 ;
                  end
              break ;
            end
          case WAIT_MAP :
            begin
              upphase = SENDING_UPLOAD ;
              memcpy ((pchar) &xfer_seg, (pchar) &dbuf.data_buf.cu.segmap, sizeof(seg_map_type)) ;
            end
        end
    end

  void setseed (byte cmp, byte str, seed_name_type *seed, location_type *loc)
    begin
      memcpy((pchar) seed, (pchar) &seed_names[cmp][str], 3) ;
      memcpy((pchar) loc, (pchar) &seed_locs[cmp][str], 2) ;
    end
    
  double seed_jul (seed_time_struc *st)
    begin
      double t ;
      
      t = jconv (st->yr - 1900, st->jday) ;
      t = t + (double) st->hr * 3600.0 + (double) st->minute * 60.0 + 
          (double) st->seconds + (double) st->tenth_millisec * 0.0001 ;
      return t ;
    end

  void process (void)
    begin
      typedef char string5[6] ;
      typedef block_record *tpbr ;
      
      char rcelu[2] = {'N', 'Y'} ;
      string5 lf[2] = {"QSL", "Q512"} ;
      short da_sum, dp_sum, i, j, l ;
      long da_crc, dp_crc ;
      boolean b, full ;
      byte k ;
      pchar ta ;
      seed_name_type sn ;
      seed_net_type sn2 ;
      header_type workheader ;
      short size ;
      unsigned short bc, xfer_offset ;
      typedef error_control_packet *tecp ;
      tecp pecp ;
      pchar p1, p2 ;
      time_quality_descriptor *ptqd ;
      calibration_result *pcal ;
      clock_exception *pce ;
      squeezed_event_detection_report *ppick ;
      commo_reply *preply ;
      be_complong ltemp ;
      tring_elem *freebuf ;
      seed_fixed_data_record_header *pseed ;
      comstat_rec *pcr, *pcom ;
      download_result *pdr ;
      det_request_rec *pdetavail ;
      download_struc *pds ;
      tpbr pbr ;
      be_int32_t *pl ;
      char seedst[5] ;
      char s1[64], s2[64] ;

/*:: start debug */
      if (insane) 
        then
          printf ("process packet\n");
/*:: end debug */
      dest = (pchar) &dbuf.seq ; /* reset buffer pointer */
      linkstat.total_packets++ ;
      size = (short) ((long) term - (long) dest - 6) ;
      if ((size and 1) == 0) /* SUN gets pissed if size is odd */
        then
          begin
            pecp = (tecp) term ;
            pecp = (tecp) ((long) pecp - 6) ;
            dp_sum = checksum(dest, size) ;
            dp_crc = gcrccalc(dest, size) ;
            ltemp.s[0] = pecp->crc ;
            ltemp.s[1] = pecp->crc_low ;
          end
        else
          pecp = NULL ;
      if ((pecp != NULL) land (dp_sum == pecp->chksum) land (ltemp.l == dp_crc))
        then
          begin
            netto_cnt = 0 ; /* got a packet, reset timeout */
            linkstat.last_good = dtime () ;
            this_packet = dbuf.seq ;
            if (size < 514)
              then
                begin
                  ta = (pchar) ((long) dest + size) ;
                  memset (ta, '\0', 514 - size) ;
                end
            if (linkstat.ultraon)
              then
                begin
                  switch (dbuf.ctrl)
                    begin
                      case SEQUENCE_SPECIAL :
                        begin
                          if (dbuf.data_buf.cl.frame_type == LINK_PKT)
                            then
                              begin
                                linkstat.linkrecv = TRUE ;
                                seq_valid = FALSE ;
                                last_packet_received = dbuf.seq ;
                                sequence_mod = dbuf.data_buf.cl.seq_modulus ;
                                curlink.msg_prio = dbuf.data_buf.cl.msg_prio ;
                                curlink.det_prio = dbuf.data_buf.cl.det_prio ;
                                curlink.time_prio = dbuf.data_buf.cl.time_prio ;
                                curlink.cal_prio = dbuf.data_buf.cl.cal_prio ;
                                curlink.window_size = dbuf.data_buf.cl.window_size ;
                                curlink.total_prio = dbuf.data_buf.cl.total_prio ;
                                curlink.link_format = dbuf.data_buf.cl.link_format ;
                                linkstat.data_format = curlink.link_format ;
                                curlink.rcecho = dbuf.data_buf.cl.rcecho ;
                                curlink.resendtime = dbuf.data_buf.cl.resendtime ;
                                curlink.synctime = dbuf.data_buf.cl.synctime ;
                                curlink.resendpkts = dbuf.data_buf.cl.resendpkts ;
                                curlink.netdelay = dbuf.data_buf.cl.netdelay ;
                                curlink.nettime = dbuf.data_buf.cl.nettime ;
                                curlink.netmax = dbuf.data_buf.cl.netmax ;
                                curlink.groupsize = dbuf.data_buf.cl.groupsize ;
                                curlink.grouptime = dbuf.data_buf.cl.grouptime ;
                                if (verbose)
                                  then
                                    begin
                                      printf ("Window=%d  Modulus=%d  Start=%d  RC Echo=%c  Data Format=%s\n",
                                             curlink.window_size, sequence_mod, last_packet_received,
                                             rcelu[curlink.rcecho], lf[linkstat.data_format]) ;
                                      printf ("Total Levels=%d  Msg=%d  Det=%d  Time=%d  Cal=%d  Sync Time=%d\n",
                                              curlink.total_prio - 1, curlink.msg_prio, curlink.det_prio,
                                              curlink.time_prio, curlink.cal_prio, curlink.synctime) ;
                                      printf ("Resend Time=%d  Resend Pkts=%d  Group Pkt Size=%d  Group Timeout=%d\n",
                                              curlink.resendtime, curlink.resendpkts,
                                              curlink.groupsize, curlink.grouptime) ;
                                      printf ("Net Restart Dly=%d  Net Conn. Time=%d  Net Packet Limit=%d\n",
                                              curlink.netdelay, curlink.nettime, curlink.netmax) ;
                                    end
                                reconfigure (FALSE) ;
                              end
                          return ;
                        end
                      case SEQUENCE_INCREMENT : ;
                      case SEQUENCE_RESET :
                        begin
                          if (seq_valid)
                            then
                              begin
                                if (dbuf.seq != (byte) ((((unsigned int) last_packet_received) + 1) mod (unsigned int) sequence_mod))
                                  then
                                    begin
                                      if (checkmask (-1))
                                        then
                                          return ;
                                      linkstat.seq_errors++ ;
                                      if (verbose)
                                        then
                                          printf ("SEQ EXP=%d  GOT=%d\n",
                                            ((unsigned int) last_packet_received + 1) mod (unsigned int) sequence_mod, dbuf.seq) ;
                                      this_packet = last_packet_received ;
                                      send_ack () ;
                                      if (++con_seq > reconfig_on_err && reconfig_on_err > 0)
                                        then
                                          begin
                                            printf ("%s reconfigure due to sequence errors\n",
                                                    localtime_string(dtime())) ;
                                            reconfigure (TRUE) ;
                                          end
                                      return ;
                                    end
                                  else
                                    con_seq = 0 ;
                              end
                          else if (linkstat.linkrecv land (dbuf.seq == last_packet_received))
                            then
                              seq_valid = TRUE ;
                            else
                              return ;
                        end
                    end
                end
              else /* old system */
                begin
                  if (seq_valid)
                    then
                      begin
                        if (dbuf.seq != (byte) ((((unsigned int) last_packet_received) + 1) mod (unsigned int) sequence_mod))
                          then
                            begin
                              if (checkmask (-1))
                                then
                                  return ;
                              linkstat.seq_errors++ ;
                              if (verbose)
                                then
                                  printf ("SEQ EXP=%d  GOT=%d\n",
                                       ((unsigned int) last_packet_received + 1) mod (unsigned int) sequence_mod, dbuf.seq) ;
                              this_packet = last_packet_received ;
                              send_ack () ;
                              if (++con_seq > reconfig_on_err && reconfig_on_err > 0)
                                then
                                  begin
                                    printf ("%s reconfigure due to sequence errors\n",
                                            localtime_string(dtime())) ;
                                    reconfigure (TRUE) ;
                                  end
                              return ;
                            end
                          else
                            con_seq = 0 ;
                      end
                  else if ((unsigned short) dbuf.seq == lowest_seq)
                    then
                      begin
                        seq_valid = TRUE ;
                        if (verbose)
                          then
                            printf ("Start=%d\n", this_packet) ;
                      end
                    else
                      begin
   /*:: Mark all sequence numbers that we have seen.  */
   /*:: When we have seen a sequence number twice, */
   /*:: determine the first of the sequence that we should accept. */
                        int first = -1 ;
                        int prev = -1 ;
                        int i ;
                        
                        if (dbuf.seq < DEFAULT_WINDOW_SIZE)
                          then
                            begin
                              if (seq_seen[dbuf.seq] > 0)
                                then
                                  begin
                                    int prev_low = -1 ;
                                    int cur_low = -1 ;
                                    int looking_for_start = 1 ;
                                    for (i = 0 ; i < DEFAULT_WINDOW_SIZE ; i++)
                                      then
                                        begin
                                          if ((lnot seq_seen[i]) land (lnot looking_for_start))
                                            then
                                              begin
                                                prev_low = cur_low ;
                                                cur_low = -1 ;
                                                looking_for_start = 1 ;
                                              end
                                          else if (looking_for_start land seq_seen[i])
                                            then
                                              begin
                                                cur_low = i ;
                                                looking_for_start = 0 ;
                                              end
                                        end
                                    if (cur_low < 0) 
                                      then
                                        cur_low = prev_low ;
                                    if (cur_low >= 0) 
                                      then
                                        lowest_seq = cur_low ;
                                  end
                                else seq_seen[dbuf.seq] = 1 ;
                            end
                        return ;
                      end
                end
            switch (dbuf.data_buf.cr.h.frame_type)
              begin
                case RECORD_HEADER_1 : ;
                case RECORD_HEADER_2 : ;
                case RECORD_HEADER_3 :
                  begin
                    if (linkstat.data_format == CSF_Q512)
                      then
                        dbuf.data_buf.cr.h.frame_count = 7 ;
                    if (checkmask (DATAQ))
                      then
                        return ;
                      else
                        send_ack () ;
                    if (lnot linkstat.ultraon)
                      then
                        begin
                          setseed(dbuf.data_buf.cr.h.component, dbuf.data_buf.cr.h.stream,
                                  &dbuf.data_buf.cr.h.seedname, &dbuf.data_buf.cr.h.location) ;
                          memcpy((pchar) &dbuf.data_buf.cr.h.seednet, (pchar) &network, 2) ;
                        end
                    if (override)
                      then
                        memcpy((pchar) &dbuf.data_buf.cr.h.station, (pchar) &station, 4) ;
                    else if ((lnot anystation) land (memcmp((pchar) &dbuf.data_buf.cr.h.station, (pchar) &station, 4) != 0))
                      then
                        printf ("Station %4.4s data received instead of %4.4s\n", 
                           &dbuf.data_buf.cr.h.station, &station) ;
                    if (firstpacket)
                      then
                        begin
                          firstpacket = FALSE ;
                          if (linkstat.ultraon)
                            then
                              begin
                                if (lnot linkstat.linkrecv)
                                  then
                                    request_link () ;
                              end
                        end
              /* Unless an option is specified to override the station, an error should
                 be generated if the station does not agree. Same goes for network.
              */
              /* put into data buffer ring */
                    freebuf = getbuffer (DATAQ) ;                  /* get free buffer */
                    pseed = (seed_fixed_data_record_header *) &freebuf->user_data.data_bytes ;
                    freebuf->user_data.reception_time = dtime () ;    /* reception time */
                    freebuf->user_data.header_time = seedheader (&dbuf.data_buf.cr.h, pseed) ; /* convert header to SEED */
                    if (rambling)
                      then
                        begin
                          if (anystation)
                            then
                              printf("%4.4s.", &dbuf.data_buf.cr.h.station) ;
                          printf("%s Header time %s", seednamestring(&dbuf.data_buf.cr.h.seedname,
                             &dbuf.data_buf.cr.h.location), time_string(freebuf->user_data.header_time)) ;
                          printf(", received at %s\n", 
                              time_string(freebuf->user_data.reception_time)) ;
                        end
                    p1 = (pchar) ((long) pseed + 64) ;             /* skip header */
                    memcpy (p1, (pchar) &dbuf.data_buf.cr.frames, 448) ;   /* and copy data portion */
                    break ;
                  end
                case BLOCKETTE :
                  begin
                    if (checkmask (BLKQ))
                      then
                        return ;
                      else
                        send_ack () ;
                    pbr = (tpbr) &dbuf.data_buf ;
                    strcpy ((pchar)&seedst, "     ") ; /* initialize to spaces */
                    j = 0 ;
                    for (i = 0 ; i <= 3 ; i++)
                      if (station.b[i] != ' ')
                        then
                          seedst[j++] = station.b[i] ; /* move in non space characters */
                    if (override)
                      then
                        memcpy((pchar) &(pbr->hdr.station_ID_call_letters), (pchar) &seedst, 5) ;
                    else if ((lnot anystation) land
                         (memcmp((pchar) &(pbr->hdr.station_ID_call_letters), (pchar) &seedst, 5) != 0))
                      then
                        printf ("Station %4.4s data received instead of %4.4s\n", 
                           &(pbr->hdr.station_ID_call_letters), &station) ;
              /* Unless an option is specified to override the station, an error should
                 be generated if the station does not agree. Same goes for network.
              */
              /* put into data buffer ring */
                    freebuf = getbuffer (BLKQ) ;                  /* get free buffer */
                    pseed = (seed_fixed_data_record_header *) &freebuf->user_data.data_bytes ;
                    freebuf->user_data.reception_time = dtime () ;    /* reception time */
                    freebuf->user_data.header_time = seed_jul (&pbr->hdr.starting_time) ; /* convert SEED time to julian */
                    if (rambling)
                      then
                        begin
                          if (anystation)
                            then
                              printf("%4.4s.", (pchar) &(pbr->hdr.station_ID_call_letters)) ;
                          printf("%s Header time %s", seednamestring(&(pbr->hdr.channel_id),
                             &(pbr->hdr.location_id)), time_string(freebuf->user_data.header_time)) ;
                          printf(", received at %s\n", 
                              time_string(freebuf->user_data.reception_time)) ;
                        end
                    pl = (be_int32_t *) &pbr->hdr ;
                    seedsequence (&pbr->hdr, *pl) ;
                    memcpy ((pchar) &freebuf->user_data.data_bytes, (pchar) pbr, 512) ; /* copy record into buffer */
                    break ;
                  end
                 case EMPTY : ;
                 case FLOOD_PKT :
                  if (checkmask (-1))
                    then
                      return ;
                    else
                      begin
                        send_ack () ;
                        linkstat.sync_packets++ ;
                        break ;
                      end
                case COMMENTS :
                  begin
                    if (checkmask (MSGQ))
                      then
                        return ;
                      else
                        send_ack () ;
                    if (linkstat.ultraon)
                      then
                        begin
                          p1 = (pchar) &dbuf.data_buf.cc.ct ;
                          p1 = (pchar) ((long) p1 + (unsigned long) *p1 + 1) ;
                          p2 = (pchar) &dbuf.data_buf.cc.cc_station ;
                          memcpy(p2, p1, sizeof(long) + sizeof(seed_net_type) +
                                    sizeof(location_type) + sizeof(seed_name_type)) ;
                        end
                    if (lnot linkstat.ultraon)
                      then
                        begin
                          memcpy((pchar) &dbuf.data_buf.cc.cc_station, (pchar) &station, 4) ;
                          memcpy((pchar) &dbuf.data_buf.cc.cc_net, (pchar) &network, 2) ;
                        end
                    if (override)
                      then
                        memcpy((pchar) &dbuf.data_buf.cc.cc_station, (pchar) &station, 4) ;
                    else if ((lnot anystation) land (memcmp((pchar) &dbuf.data_buf.cc.cc_station, (pchar) &station, 4) != 0))
                      then
                        printf ("Station %4.4s message received instead of %4.4s\n", 
                           &dbuf.data_buf.cc.cc_station, &station) ;
                    if (linkstat.ultraon land (lnot linkstat.ultrarecv) land
                       (strncasecmp((pchar) &dbuf.data_buf.cc.ct, "FROM AQSAMPLE: Acquisition begun", 32) == 0))
                      then
                        request_ultra () ;
                  /* Put into blockette ring */
                    if (rambling)
                      then
                        begin
                          dbuf.data_buf.cc.ct[dbuf.data_buf.cc.ct[0]+1] = '\0' ;
                          if (anystation)
                            then
                              printf("%4.4s.", &dbuf.data_buf.cc.cc_station) ;
                          printf ("%s\n", &dbuf.data_buf.cc.ct[1]) ;
                        end ;
                    freebuf = getbuffer (MSGQ) ;
                    pseed = (seed_fixed_data_record_header *) &freebuf->user_data.data_bytes ;
                    freebuf->user_data.reception_time = dtime () ;    /* reception time */
                    freebuf->user_data.header_time = seedblocks ((seed_record_header *)pseed, &dbuf.data_buf) ;
                    break ;
                  end
                case CLOCK_CORRECTION :
                  begin
                    if (checkmask (TIMQ))
                      then
                        return ;
                      else
                        send_ack () ;
                    if (vcovalue < 0)
                      then
                        if (linkstat.ultraon)
                          then
                            vcovalue = dbuf.data_buf.ce.header_elog.clk_exc.vco ;
                          else
                            begin
                              ptqd = (time_quality_descriptor *) &dbuf.data_buf.ce.header_elog.clk_exc.correction_quality ;
                              vcovalue = (unsigned short) ptqd->time_base_VCO_correction * 16 ;
                            end
                    pce = &dbuf.data_buf.ce.header_elog.clk_exc ;
                    if (lnot linkstat.ultraon)
                      then
                        begin
                          memcpy((pchar) &pce->cl_station, (pchar) &station, 4) ;
                          memcpy((pchar) &pce->cl_net, (pchar) &network, 2) ;
                        end
                    if (override)
                      then
                        memcpy((pchar) &pce->cl_station, (pchar) &station, 4) ;
                    else if ((lnot anystation) land (memcmp((pchar) &pce->cl_station, (pchar) &station, 4) != 0))
                      then
                        printf ("Station %4.4s timing received instead of %4.4s\n", 
                           &pce->cl_station, &station) ;
                   /* put into blockette ring */
                    freebuf = getbuffer (TIMQ) ;
                    pseed = (seed_fixed_data_record_header *) &freebuf->user_data.data_bytes ;
                    freebuf->user_data.reception_time = dtime () ;    /* reception time */
                    freebuf->user_data.header_time = seedblocks ((seed_record_header *)pseed, &dbuf.data_buf) ;
                    if (rambling)
                      then
                        begin
                         if (anystation)
                          then
                            printf("%4.4s.", &pce->cl_station) ;
                         printf("Clock time-mark %s",
                              time_string(freebuf->user_data.header_time)) ;
                          printf(", received at %s\n", 
                              time_string(freebuf->user_data.reception_time)) ;
                        end
                    break ;
                  end
                case DETECTION_RESULT :
                  begin
                    if (checkmask (DETQ))
                      then
                        return ;
                      else
                        send_ack () ;
                    ppick = &dbuf.data_buf.ce.header_elog.det_res.pick ;
                    if (lnot linkstat.ultraon)
                      then
                        begin
                          setseed (ppick->component, ppick->stream,
                                   &ppick->seedname, &ppick->location) ;
                          memset((pchar) &ppick->detname, ' ', 24) ;
                          ppick->sedr_sp1 = 0 ;
                          memcpy((pchar) &ppick->ev_station, (pchar) &station, 4) ;
                          memcpy((pchar) &ppick->ev_network, (pchar) &network, 2) ;
                        end
                    if (override)
                      then
                        memcpy((pchar) &ppick->ev_station, (pchar) &station, 4) ;
                    else if ((lnot anystation) land (memcmp((pchar) &ppick->ev_station, (pchar) &station, 4) != 0))
                      then
                        printf ("Station %4.4s detection received instead of %4.4s\n", 
                           &ppick->ev_station, &station) ;
                    /* put into blockette ring */
                    freebuf = getbuffer (DETQ) ;
                    pseed = (seed_fixed_data_record_header *) &freebuf->user_data.data_bytes ;
                    freebuf->user_data.reception_time = dtime () ;
                    freebuf->user_data.header_time = seedblocks ((seed_record_header *)pseed, &dbuf.data_buf) ;
                    if (rambling)
                      then
                        begin
                          if (anystation)
                            then
                              printf("%4.4s.", &ppick->ev_station) ;
                          printf("%3.3s Detection   %s",
                              &dbuf.data_buf.ce.header_elog.det_res.pick.seedname,
                              time_string(freebuf->user_data.header_time)) ;
                          printf(", received at %s\n", 
                              time_string(freebuf->user_data.reception_time)) ;
                        end
                    break ;
                  end
                case END_OF_DETECTION :
                  begin
                    if (checkmask (DATAQ))
                      then
                        return ;
                      else
                        send_ack () ;
                    freebuf = getbuffer (DATAQ) ;                  /* get free buffer */
                    pseed = (seed_fixed_data_record_header *) &freebuf->user_data.data_bytes ;
                    ppick = &dbuf.data_buf.ce.header_elog.det_res.pick ;
                    if (lnot linkstat.ultraon)
                      then
                        begin
                          setseed (ppick->component, ppick->stream,
                                   &ppick->seedname, &ppick->location) ;
                          ppick->sedr_sp1 = 0 ;
                          memcpy((pchar) &ppick->ev_station, (pchar) &station, 4) ;
                          memcpy((pchar) &ppick->ev_network, (pchar) &network, 2) ;
                        end
                    if (override)
                      then
                        memcpy((pchar) &ppick->ev_station, (pchar) &station, 4) ;
                    else if ((lnot anystation) land (memcmp((pchar) &ppick->ev_station, (pchar) &station, 4) != 0))
                      then
                        printf ("Station %4.4s end of detection received instead of %4.4s\n", 
                           &ppick->ev_station, &station) ;
                    freebuf->user_data.reception_time = dtime () ;    /* reception time */
                    freebuf->user_data.header_time = seedblocks ((seed_record_header *)pseed, &dbuf.data_buf) ;
                    if (rambling)
                      then
                        begin
                          if (anystation)
                            then
                              printf("%4.4s.", &ppick->ev_station) ;
                          printf("%3s Detect End  %s",
                              &dbuf.data_buf.ce.header_elog.det_res.pick.seedname,
                              time_string(freebuf->user_data.header_time)) ;
                          printf(", received at %s\n", 
                              time_string(freebuf->user_data.reception_time)) ;
                        end
                    break ;
                  end
                case CALIBRATION :
                  begin
                    if (checkmask (CALQ))
                      then
                        return ;
                      else
                        send_ack () ;
                    pcal = &dbuf.data_buf.ce.header_elog.cal_res ;
                    if (lnot linkstat.ultraon)
                      then
                        begin
                          setseed (pcal->cr_component, pcal->cr_stream,
                                   &pcal->cr_seedname, &pcal->cr_location) ;
                          setseed (pcal->cr_input_comp, pcal->cr_input_strm,
                                   &pcal->cr_input_seedname, &pcal->cr_input_location) ;
                          pcal->cr_flags2 = 0 ;
                          pcal->cr_0dB = 0 ;
                          pcal->cr_0dB_low = 0 ;
                          pcal->cr_sfrq = Hz1_0000 ;
                          pcal->cr_filt = 0 ;
                          memcpy ((pchar) &pcal->cr_station, (pchar) &station, 4) ;
                          memcpy ((pchar) &pcal->cr_network, (pchar) &network, 2) ;
                        end
                    if (override)
                      then
                        memcpy((pchar) &pcal->cr_station, (pchar) &station, 4) ;
                    else if ((lnot anystation) land (memcmp((pchar) &pcal->cr_station, (pchar) &station, 4) != 0))
                      then
                        printf ("Station %4.4s calibration received instead of %4.4s\n", 
                           &pcal->cr_station, &station) ;
                    /* store in blockette ring */
                    freebuf = getbuffer (CALQ) ;
                    pseed = (seed_fixed_data_record_header *) &freebuf->user_data.data_bytes ;
                    freebuf->user_data.reception_time = dtime () ;    /* reception time */
                    freebuf->user_data.header_time = seedblocks ((seed_record_header *)pseed, &dbuf.data_buf) ;
                    if (rambling)
                      then
                        begin
                          if (anystation)
                            then
                              printf("%4.4s.", &pcal->cr_station) ;
                          printf("%3.3s Calibration %s",
                              &dbuf.data_buf.ce.header_elog.cal_res.cr_seedname, 
                              time_string(freebuf->user_data.header_time)) ;
                          printf(", received at %s\n", 
                              time_string(freebuf->user_data.reception_time)) ;
                        end
                    break ;
                  end
                case ULTRA_PKT :
                  begin
                    if (checkmask (-1))
                      then
                        return ;
                      else
                        send_ack () ;
                    if (lnot linkstat.ultrarecv)
                      then
                        begin
                          preply = &dbuf.data_buf.cy ;
                          full = TRUE ;
                          if (ultra_seg_empty)
                            then
                              begin
                                ultra_seg_empty = FALSE ;
                                pultra = (ultra_type *) malloc(preply->total_bytes) ;
                              end
                          ultra_seg[preply->this_seg div 8] = ultra_seg[preply->this_seg div 8] or
                                          (byte) (1 << (preply->this_seg mod 8))  ;
                          ta = (pchar) ((long) pultra + preply->byte_offset) ;
                          memcpy(ta, (pchar) &preply->bytes, preply->byte_count) ;
                          j = 0 ;
                          for (i = 1 ; i <= preply->total_seg ; i++)
                            if ((ultra_seg[i div 8] and ((byte) (1 << (i mod 8)))) == 0)
                              then
                                full = FALSE ;
                              else
                                j++ ;
                          ultra_percent = (short) ((j / preply->total_seg) * 100.0) ;
                          if (full)
                            then
                              begin
                                linkstat.ultrarecv = TRUE ;
                                vcovalue = pultra->vcovalue ;
                                if (pultra->ultra_rev >= 1)
                                  then
                                    comm_mask = pultra->comm_mask ;
                                if (rambling)
                                  then
                                    printf("Ultra record received with %d bytes\n",
                                           (unsigned short) preply->total_bytes) ;
                              end
                        end
                    break ;
                  end
                case DET_AVAIL :
                  begin
                    if (checkmask (-1))
                      then
                        return ;
                      else
                        send_ack () ;
                    if (detavail_ok land (lnot detavail_loaded))
                      then
                        begin
                          preply = &dbuf.data_buf.cy ;
                          full = TRUE ;
                          pdetavail = NULL ;
                          /* Make sure there is a valid client for this data */
                          if (checkbusy ())
                            then
                              begin
                                pcr = (comstat_rec *) clients[combusy].outbuf ;
                                if ((unsigned int) clients[combusy].outsize >= (unsigned int) preply->total_bytes)
                                  then
                                    pdetavail = (det_request_rec *) &pcr->moreinfo ;
                              end
                          if (pdetavail == NULL)
                            then
                              begin
                                detavail_ok = FALSE ;
                                combusy = NOCLIENT ;
                                break ;
                              end
                          detavail_seg[preply->this_seg div 8] = detavail_seg[preply->this_seg div 8] or
                                          (byte) (1 << (preply->this_seg mod 8))  ;
                          ta = (pchar) ((long) pdetavail + preply->byte_offset) ;
                          memcpy (ta, (pchar) &preply->bytes, preply->byte_count) ;
                          j = 0 ;
                          for (i = 1 ; i <= preply->total_seg ; i++)
                            if ((detavail_seg[i div 8] and ((byte) (1 << (i mod 8)))) == 0)
                              then
                                full = FALSE ;
                              else
                                j++ ;
                          if (full)
                            then
                              begin
                                detavail_loaded = TRUE ;
                                pcr->completion_status = CSCS_FINISHED ;
                                combusy = NOCLIENT ;
                              end
                        end
                    break ;
                  end
                case DOWNLOAD :
                  begin
                    if (checkmask (-1))
                      then
                        return ;
                      else
                        send_ack () ;
                    if (xfer_down_ok land checkbusy ())
                      then
                        begin
                          preply = &dbuf.data_buf.cy ;
                          pcom = (comstat_rec *) clients[combusy].outbuf ;
                          pdr = (download_result *) &pcom->moreinfo ;
                          if (preply->this_seg == 1)
                            then
                              begin
                                pds = (download_struc *) &preply->bytes ;
                                xfer_total = pds->file_size ;
                                pdr->fsize = xfer_total ;
                                strpcopy (s1, pds->file_name) ;
                                strpcopy (s2, xfer_source) ;
                                if (strcasecmp((pchar) &s1, (pchar) &s2) != 0)
                                  then
                                    begin
                                      do_abort () ;
                                      break ;
                                   end
                                if (lnot pds->filefound)
                                  then
                                    begin
                                      pcom->completion_status = CSCS_NOTFOUND ;
                                      do_abort () ;
                                      break ;
                                    end
                                if (pds->toobig)
                                  then
                                    begin
                                      pcom->completion_status = CSCS_TOOBIG ;
                                      break ;
                                    end
                              end
                          if (xfer_size == 0)
                            then
                              begin
                                xfer_size = preply->total_bytes ;
                                pdr->dpshmid = shmget(IPC_PRIVATE, xfer_size, IPC_CREAT or PERM) ;
                                if (pdr->dpshmid == ERROR)
                                  then
                                    begin
                                      pcom->completion_status = CSCR_PRIVATE ;
                                      do_abort () ;
                                      break ;
                                    end
                                pdownload = (download_struc *) shmat (pdr->dpshmid, NULL, 0) ;
                                if ((int) pdownload == ERROR)
                                  then
                                    begin
                                      pcom->completion_status = CSCR_PRIVATE ;
                                      do_abort () ;
                                      break ;
                                    end
                                xfer_segments = preply->total_seg ;
                              end
/* Isolate client from header, start the data module with the actual file contents */
                          xfer_offset = sizeof(download_struc) - 65000 ; /* source bytes to skip */
                          ta = (pchar) ((long) pdownload + preply->byte_offset - xfer_offset) ; /* destination */
                          p1 = (pchar) &preply->bytes ; /* source */
                          bc = preply->byte_count ; /* number of bytes */
                          if (preply->this_seg == 1)
                            then
                              begin
                                bc = bc - xfer_offset ; /* first record contains header */
                                p1 = p1 + xfer_offset ;
                                ta = ta + xfer_offset ;
                              end
                          memcpy (ta, p1, bc) ;
                          i = preply->this_seg - 1 ;
                          j = i div 8 ;
                          k = (byte) (1 << (i mod 8)) ;
                          if ((xfer_seg[j] and k) == 0)
                            then
                              pdr->byte_count = pdr->byte_count + bc ;  /* not already received */
                          xfer_seg[j] = xfer_seg[j] or k ;
                          l = 0 ;
                          for (i = 0 ; i <= 127 ; i++)
                            begin
                              k = xfer_seg[i] ;
                              for (j = 0 ; j <= 7 ; j++)
                                if ((k and (byte) (1 << j)) != 0)
                                  then
                                    l++ ;
                            end
                          if ((unsigned int) l >= (unsigned int) xfer_segments)
                            then
                              begin
                                xfer_down_ok = FALSE ;
                                down_count = 0 ;
                                pdr->byte_count = xfer_total ;
                                shmdt((pchar)pdownload) ;
                                pcom->completion_status = CSCS_FINISHED ;
                                combusy = NOCLIENT ;
                                xfer_size = 0 ;
                              end
                        end
                    else if (++down_count > 5)
                      then
                        begin
                          xfer_down_ok = TRUE ;
                          do_abort () ;
                        end
                    break ;
                  end
                case UPMAP :
                  begin
                    if (checkmask (-1))
                      then
                        return ;
                      else
                        send_ack () ;
                    if (xfer_up_ok)
                      then
                        process_upmap () ;
                    break ;
                  end
                case CMD_ECHO : 
                  begin
                    if (checkmask (-1))
                      then
                        return ;
                      else
                        send_ack () ;
                    if (checkbusy ())
                      then
                        begin
                          pcom = (comstat_rec *) clients[combusy].outbuf ;
                          preply = &dbuf.data_buf.cy ;
                          memcpy ((pchar) &replybuf, (pchar) &preply->bytes, preply->byte_count) ;
                          if ((replybuf.ces.dp_seq == pcom->command_tag) land
                              (pcom->completion_status == CSCS_INPROGRESS))
                            then
                              if (follow_up)
                                then
                                  begin /* Set AUTO DAC, now send prepared ACCURATE DAC */
                                    if (++cmd_seq == 0)
                                      then
                                        cmd_seq = 1 ;
                                    gmsg.mmc.dp_seq = cmd_seq ;
                                    send_tx_packet (0, gmsg.mmc.cmd_type, &gmsg) ;
                                    pcom->command_tag = cmd_seq ;
                                    follow_up = FALSE ;
                                  end
                                else
                                  begin
                                    pcom->completion_status = CSCS_FINISHED ;
                                    combusy = NOCLIENT ;
                                  end
                        end 
                    break ;
                  end
                default :
                  begin
                    linkstat.last_bad = dtime () ;
                    printf ("INVALID RECORD TYPE=%d\n", dbuf.data_buf.cr.h.frame_type) ;
                    if (checkmask (-1))
                      then
                        return ;
                      else
                        send_ack () ;
                    break ;
                  end
              end
          end
        else
          begin
            linkstat.last_bad = dtime () ;
            linkstat.check_errors++ ;
            if (verbose)
              then
                printf ("CHECKSUM ERROR ON PACKET %d, BYTE COUNT=%d, CHECKSUM ERRORS=%d\n",
                        last_packet_received, size, linkstat.check_errors) ;
          end
    end

  void fillbuf (void)
    begin
      int numread ;
#ifdef _OSK
      u_int32 err, count, clilen ;
#elif defined(__GNU_LIBRARY__) && __GNU_LIBRARY__ < 2
      int clilen;
#else
      socklen_t clilen;
#endif

      src = (pchar) &sbuf ;
      srcend = src ;
      if (lnot serial)
        then
          begin
            if ((path < 0) land (netdly_cnt >= netdly))
              then
                begin
                  clilen = sizeof(cli_addr) ;
                  path = accept(sockfd, (psockaddr) &cli_addr, &clilen) ;
                  netdly_cnt = 0 ;
                  if ((verbose) land (path >= 0))
                    then
                      printf ("Network connection with DA opened\n") ;
                  if (linkstat.ultraon)
                    then
                      begin
                        linkstat.linkrecv = FALSE ;
                        linkstat.ultrarecv = FALSE ;
                        seq_valid = FALSE ;
                        linkpoll = link_retry ;
                      end
                end
            if (path < 0)
              then
                return ;
          end
#ifdef _OSK
      if (serial)
        then /* make sure we don't block here */
          begin
            err = _os_gs_ready(path, &count) ;
            if ((err == EOS_NOTRDY) lor (count == 0))
              then
                return ;
            else if (err != 0)
              then
                begin
                  numread = -1 ;
                  errno = err ;
                end
              else
                begin
                  if (count > BLOB)
                    then
                      count = BLOB ;
                  err = blockread (path, count, src) ;
                  if (err == 208)
                    then
                      numread = read(path, src, count) ;
                  else if (err == 0)
                    then
                      numread = count ;
                    else
                      begin
                        numread = -1 ;
                        errno = err ;
                      end
                end
          end
        else
          numread = read(path, src, BLOB) ;  
#else
      numread = read(path, src, BLOB) ;
#endif
      if (numread > 0)
       then
         begin
           srcend = (pchar) ((long) srcend + numread) ;
           if ((insane) /* land (numread > maxbytes) */)
             then
               begin
                 maxbytes = numread ;
                 printf ("%d bytes read\n", numread) ;
               end
         end
      else if (numread < 0)
        then
          if (errno != EWOULDBLOCK)
            then
              begin
                linkstat.io_errors++ ;
                linkstat.lastio_error = errno ;
                if (serial)
                  then
                    begin
                      if (verbose)
                        then
                          perror ("Error reading from port ") ;
                    end
                  else
                    begin
                      if (verbose)
                        then
                          perror ("Network connection with DA closed\n") ;
                      shutdown(path, 2) ;
                      close(path) ;
                      seq_valid = FALSE ;
                      if (linkstat.ultraon)
                        then
                          begin
                            linkstat.linkrecv = FALSE ;
                            linkstat.ultrarecv = FALSE ;
                          end
                       path = -1 ; /* signal not open */
                    end
              end
    end

  short inserial (pchar b)
    begin
      int numread ;

      if (src == srcend)
        then
          fillbuf () ;

      if (src != srcend)
        then
          begin
            *b = *src++ ;
            return 1 ;
          end
        else
          return 0 ;
    end

  void dlestrip (void)
    begin
      boolean indle ;

      term = NULL ;
      indle = (lastchar == DLE) ;
      while ((src != srcend) land (dest != destend))
        begin
          if (indle)
            then
              begin
                *dest++ = *src ;
                indle = FALSE ;
              end
          else if (*src == DLE)
            then
              indle = TRUE ;
          else if (*src == ETX)
            then
              begin
                term = dest ;
                src++ ;
                lastchar = NUL ;
                return ;
              end
            else
              *dest++ = *src ;
          src++ ;
        end
      if (indle)
        then
          lastchar = DLE ;
        else
          lastchar = NUL ;
    end

  void comlink_check_input (void)
    begin
      int numread ;
      short err ;
      char b ;

      if (udplink)
        then
          begin
            numread = recv(path, dest, (int) destend - (int) dest, 0) ;
            if (numread > 0)
              then
                begin
                  term = (pchar) ((long) dest + numread) ;
                  process () ;
                end
            else if (numread < 0)
              then
                if (errno != EWOULDBLOCK)
                  then
                    begin
                      linkstat.io_errors++ ;
                      linkstat.lastio_error = errno ;
                      seq_valid = FALSE ;
                      if (linkstat.ultraon)
                        then
                          begin
                            linkstat.linkrecv = FALSE ;
                            linkstat.ultrarecv = FALSE ;
                          end
                    end
            return ;
          end
      if (src == srcend)
        then
          fillbuf () ;
      switch (inphase)
        begin
          case SOHWAIT :
            dest = (pchar) &dbuf.seq ;
          case SYNWAIT :
            begin
              while (inphase != INBLOCK)
                begin
                  lastchar = NUL ;
                  err = inserial(&b) ;
                  if (err != 1)
                    then
                      return ;
                  if ((b == SOH))
                    then
                      inphase = SYNWAIT ;
                  else if ((b == SYN) land (inphase == SYNWAIT))
                    then
                      inphase = INBLOCK ;
                    else 
                      inphase = SOHWAIT ;
                end
            end
          case INBLOCK :
            begin
              if (src == srcend)
                then
                  fillbuf ;  /* Statement with no effect! Bug? */
              if (src != srcend)
                then
                  begin
                    dlestrip () ;
                    if (dest == destend)
                      then
                        inphase = ETXWAIT ;
                    else if (term != NULL)
                      then
                        begin
                          inphase = SOHWAIT ;
                          process () ;
                          break ;
                        end
                      else
                        break ;
                  end
                else
                  break ;
            end
          case ETXWAIT :
            begin
              if (src == srcend)
                then
                  fillbuf ;  /* Statement with no effect! Bug? */
              err = inserial (&b) ;
              if (err == 1)
                then
                  if (b == ETX)
                    then
                      begin
                        inphase = SOHWAIT ;
                        term = dest ;
                        process () ;
                        break ;
                      end
                    else
                      begin
                        inphase = SOHWAIT ;
                        break ;
                      end
            end
        end
    end
