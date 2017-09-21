/***************************************************************************** 
 * qpdisplay.c
 *
 * Display routines for slqplot
 *
 * (c) 1998, 2003 Andres Heinloo, GFZ Potsdam
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any later
 * version. For more information, see http://www.gnu.org/
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>

#include "slqplot.h"

#define DU ((XDISPSIZE + YDISPSIZE) / 1000)  /* "display unit" */

#define FULLSCALE (1 << 23)

#define PAGE(h,m) \
    (((60 * (h) + (m)) / c_tracelen) / c_traces)
#define TRACE(h,m) \
    (((60 * (h) + (m)) / c_tracelen) % c_traces)
#define XSTEP(f) \
    ((double)XPLSIZE/(60.0 * (f) * (double)c_tracelen))
#define X(rate,sn) \
   ((double)XPLSIZE * (double)(sn) / ((double)(c_tracelen * 60) * (rate)) + \
   (double)XPLOFFS)
#define YSTEP \
    ((double)YPLSIZE/(double)(chcount * (1 + c_traces)))
#define Y(c,t) \
    ((double)(1 + (t) + (c) * (1 + c_traces)) * YSTEP + (double)YPLOFFS)

static int average[NCHANS];
static void setup_page(int start_trace);
static int disp(int chn, int tracenum, int tracepos, double samprate,
  float *data, int nsamples, int first_trace_is_red);

static int page_start_trace;
static int **trace_seq;
static int new_page = 1;
static int channels_finished = 0;
  
void restore_state(const char *state_file, int *seqnum)
  {
    int i, c;
    FILE *fp;
    
    page_start_trace = 0;
    *seqnum = -1;
    
    if(state_file != NULL)
      {
        if((fp = fopen(state_file, "r")) != NULL)
          {
            int seq, start_time;
            if(fscanf(fp, "%X %d", &seq, &start_time) == 2)
              {
                page_start_trace = start_time / c_tracelen;
                *seqnum = seq;
              }
            else
              {
                sl_log (1, 0, "cannot read %s\n", state_file);
              }

            fclose(fp);
          }
      }
    
    trace_seq = (int **) malloc(chcount * sizeof(int *));

    for(c = 0; c < chcount; ++c)
      {
        trace_seq[c] = (int *) malloc((c_traces + 1) * sizeof(int));

        for(i = 0; i < c_traces + 1; ++i)
          {
            trace_seq[c][i] = *seqnum;
          }
      }

    new_page = 1;
  }

void save_state(const char *state_file)
  {
    int c, seqnum;
    FILE *fp;
    
    if(state_file != NULL)
      {
        seqnum = 0xffffff;
        for(c = 0; c < chcount; ++c)
          {
            if(trace_seq[c][0] != -1 &&
              trace_seq[c][0] < seqnum)
                seqnum = trace_seq[c][0];
          }
                
        if((fp = fopen(state_file, "w")) != NULL)
          {
            fprintf(fp, "%06X %d", seqnum, page_start_trace * c_tracelen);
            fclose(fp);
          }
        else
          {
            sl_log (1, 0, "cannot open %s\n", state_file);
          }
      }
    
    for(c = 0; c < chcount; ++c)
        free(trace_seq[c]);
    
    free(trace_seq);
  }

void redisplay(int *seqnum)
  {
    int c;
    for(c = 0; c < chcount; ++c)
      {
        if(trace_seq[c][0] != -1 &&
          trace_seq[c][0] < *seqnum)
            *seqnum = trace_seq[c][0];
      }
                
    new_page = 1;
  }

void display(int chn, struct ms_hdrdata *head, int32_t *data, int *seqnum)
  {
    int i, n, curtrace, tracepos, dispcount, samples_displayed;
    time_t seconds;
    float fdata[MAX_SAMPLES];
    
    if(head->nsamples == 0) return;

    seconds = mktime(&head->hdrtime);
    curtrace = seconds / (c_tracelen * 60);
    tracepos = ((double)(seconds % (c_tracelen * 60)) +
      (double)(head->tenth_millisec) / 10000.0) * head->sample_rate;
    
    if(curtrace < page_start_trace - 1)
        return;

    samples_displayed = 0;
    
    while(samples_displayed < head->nsamples)
      {
        if(curtrace > page_start_trace + c_traces - 1)
          {
            if(~c_flags & FL_COMPLETE_PAGES || channels_finished == chcount)
              {
                n = curtrace - (page_start_trace + c_traces - 1);

                if((n % c_scroll_step) != 0)
                    n += c_scroll_step - (n % c_scroll_step);
                
                for(i = 0; i + n < c_traces + 1; ++i)
                  {
                    trace_seq[chn][i] = trace_seq[chn][i + n];
                  }

                for(; i < c_traces + 1; ++i)
                  {
                    trace_seq[chn][i] = (*seqnum - 1) & 0xffffff;
                  }
                
                page_start_trace += n;
                channels_finished = 0;

                for(i = 0; i < chcount; ++i)
                  {
                    if(trace_seq[i][0] != -1 &&
                      trace_seq[i][0] < *seqnum)
                        *seqnum = trace_seq[i][0];
                  }
                
                new_page = 1;
                return;
              }
              
            ++channels_finished;
            return;
          }

        n = curtrace - page_start_trace;
            
        if(new_page)
          {
            setup_page(page_start_trace);
            new_page = 0;
          }

        for(i = 0; i < (int)head->nsamples; i++)
            fdata[i] = (float)data[i];                
                        
        if(strlen(c_channel[chn].filter.filname))
          {
            static int nrec = 0; 
           
            if(c_channel[chn].filter.nband < 0)
              {
                fprintf(stderr,"\nNo filter '%s' found in config file ...\n",c_channel[chn].filter.filname);
                exit(-1);
              }
           
            sifil((int)head->nsamples, fdata, (int)chcount, 
              &(c_channel[chn].filter.w1)[0], 
              &(c_channel[chn].filter.w2)[0], 
              (int)chn, nrec, 
              c_channel[chn].filter.nband);
                        
            if(nrec < 1) nrec = 2;

            for(i = 0; i < (int)head->nsamples; i++)
                fdata[i] *= ((c_channel[chn].filter.fmag == 0)? 100 : c_channel[chn].filter.fmag);
          }

        while(samples_displayed < head->nsamples &&
          (n = curtrace - page_start_trace) < c_traces)
          {
            dispcount = disp(chn, n, tracepos, head->sample_rate,
              fdata + samples_displayed, head->nsamples - samples_displayed,
              page_start_trace % 2);

            samples_displayed += dispcount;
            tracepos += dispcount;
            
            if(samples_displayed < head->nsamples)
              {
                trace_seq[chn][n + 1] = *seqnum;
                tracepos = 0;
                ++curtrace;
              }
          }
      }
  }
  
static void setup_page(int start_trace)
  {
    int i, j;
    time_t t;
    char str[50];
    double y, y0, y1;
    struct tm *page_time;

    t = (start_trace + c_traces - 1) * c_tracelen * 60;
    page_time = gmtime(&t);
    
    strftime(str, 20, "%Y%m%d%H%M", page_time);
    newpage(str);
    
    if(c_flags & FL_COLOURED_TRACES)
        setcolor("black");
    
    for(i = 0; i < DESCLINES; ++i)
      {
        if(c_description[i] != 0)
            htext(20 * DU, (12 + 16 * i) * DU, 'l', 't', FNTNAME_BOLD, 1.5 * FNTSIZE, c_description[i]);
      }
    
    strftime(str, 11, "%Y-%m-%d", page_time);
    htext(XPLOFFS + XPLSIZE, 12 * DU, 'r', 't', FNTNAME_NORMAL, 1.5 * FNTSIZE, str);
    
    for(i = 0; i < chcount; ++i)
      {
        average[i] = 0;
        sprintf(str, "%s - %d", c_channel[i].chname, c_channel[i].mag);
            
        vtext(20 * DU, (i + 0.5) * YPLSIZE / chcount + YPLOFFS, 'c', 't', FNTNAME_NORMAL, FNTSIZE, str);
        
        // t = (page_time->tm_hour) * 60 + page_time->tm_min -
        //   (c_traces - 1) * c_tracelen;
        //
        // while(t < 0) t += 24 * 60;
        
        y0 = 0; y1 = Y(i + 1, 0);
        t = (start_trace * c_tracelen) % (24 * 60);
        for(j = 0, y = Y(i, 0); j < c_traces; ++j, y += YSTEP,
          t = (t + c_tracelen) % (24 * 60))
          {
            if(y - y0 < FNTSIZE + DU || y1 - y < FNTSIZE + DU) continue;
            y0 = y;
            sprintf(str, "%02ld:%02ld", t / 60, t % 60);
            htext(XPLOFFS - 6 * DU, Y(i, j), 'r', 'c', FNTNAME_NORMAL, FNTSIZE, str);
          }
      }
  }

int disp(int chn, int tracenum, int tracepos, double samprate, float *data,
  int nsamples, int first_trace_is_red)
  {
    int i, int_x;
    double x, y, xstep, yscale;
    int32_t min = 0, max = 0;
    struct c_channel_struct *channel = &c_channel[chn];
    static int32_t prev_value[NCHANS];
    char str[50];

    if(tracenum < 0)
      {
        int tracelen = (double)(c_tracelen * 60) * samprate;
        return ((nsamples > tracelen - tracepos)? tracelen - tracepos: nsamples);
      }
    
    if(c_flags & FL_COLOURED_TRACES)
        setcolor(((tracenum + first_trace_is_red) % 2)? "red": "blue");
    
    x = X(samprate, tracepos);
    y = Y(chn, tracenum);
    xstep = XSTEP(samprate);
    yscale = ((double)(channel->mag) * YSTEP / (double)FULLSCALE);
    
    if(tracepos == 0) prev_value[chn] = 0;
    
    moveto(x, y + (double)prev_value[chn] * yscale);
    
#ifdef SHOW_AVERAGE
    if((int) average[chn] == 0)
      {
        for(i = 0; i < head->nsamples; i++)
            average[chn] += channel->data[i];
        
        average[chn] = average[chn]/head->nsamples;
                
        sprintf(str, "%1.3e", (float)average[chn]);
        htext(XPLSIZE + XPLOFFS + XPAVERAGE, Y(chn, trace), 'r', 'c', str);
        moveto(x, y + (double)prev_value[chn] * yscale);
      }
#endif

    for(i = 0; i < nsamples;)
      {
        min = FULLSCALE + 1;
        max = -FULLSCALE - 1;
        for(int_x = (int)x; int_x == (int)x && i < nsamples;
            x += xstep, ++i)
          {
            if(data[i] < min) min = data[i] - average[chn];
            if(data[i] > max) max = data[i] - average[chn];
          }

        lineto(x, y + (double)min * yscale);
        if((int)((double)min * yscale) != (int)((double)max * yscale)) 
            lineto(x, y + (double)max * yscale);
 

        if((int)x >= XPLOFFS + XPLSIZE) 
            break;
      }

    prev_value[chn] = max;
    
    flush_plotters();
    return i;
  }

