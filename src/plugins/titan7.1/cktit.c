/*=====================================================================*
        cktit.c
 *=====================================================================*/
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>


#define GET_SYNC       (sync       = (int) (frame[11] & 0xF0))
#define GET_FRAME_TYPE (frame_type = (int) (frame[11] & 0x0F))
#define SYNC_KO        (sync != 0X50 && sync != 0XA0)
#define SYNC_OK        (sync == 0X50 || sync == 0XA0)


FILE *Fp_tit;
FILE *Fp_log;
int beg_offset, end_offset;


/*===================================================================*/
void main(argc, argv)
int    argc;
char **argv;
{
char fname[255];
int i;

    Fp_log = stdout;
    if (argc < 2)
    {
        printf("usage: cktit titan_file\n");
        exit(1);
    }
    beg_offset      = 0;
    end_offset      = -1;

    for (i=2; i<argc; i++)
    {
        if      (!strncmp(argv[i],"bof=", 4))
            beg_offset   = atol(&argv[i][4]);
        else if (!strncmp(argv[i],"eof=", 4))
            end_offset   = atol(&argv[i][4]);
    }

    sprintf(fname, "%s", argv[1]);

    if (!(Fp_tit = fopen(fname, "r")))
    {
        exit(1);
    }
    process_file(beg_offset);

    exit(0);
}

/*===================================================================*/
int process_file(int file_ofs)
{
int   sync;
int   frame_type;
char  frame[20];

    file_ofs = sync_phase1(file_ofs);
    fprintf(Fp_log,"  process_file: synchro @ ofs %d\n", file_ofs);

    while (fread(frame, 1, 12, Fp_tit))
    {
        GET_SYNC;
        GET_FRAME_TYPE;
        if (SYNC_KO)
        {
          check_sync(frame);
          file_ofs = sync_phase1(file_ofs);
          fprintf(Fp_log,"  process_file: synchro @ ofs %d\n", file_ofs);
        }

        file_ofs = ftell(Fp_tit);
        if (end_offset > 0 && file_ofs >= end_offset)
        {
            fprintf(Fp_log,"  end offset reached: %d\n", file_ofs);
            return 0;
        }
    }
    fprintf(Fp_log,"  end of file reached: %d\n", file_ofs);
    return 0;
}


/*===================================================================*/
int sync_phase1(int file_ofs)
{
int   byte_cnt, nframe_ok, j;
int   sync_byte_ofs;
int   sync, sync1, sync2;
int   frame_type;
char  c, frame[20];
int   jjj = 0;
int   kkk = 0;
int   nn = 0;
 
    byte_cnt = 0;
    sync_byte_ofs = 0;
    fseek(Fp_tit, file_ofs, SEEK_SET);

    while (fread(&c, 1, 1, Fp_tit))
    {
// printf("-- %02X\n", (int)(c & 0xFF));
        if (++kkk > 100000)
        {
          printf("  sync_phase1: %.6f MBytes\n",
                    (double)(ftell(Fp_tit)) / 1000000.0);
          kkk = 0;
        }
        ++byte_cnt;
        sync1 = c;
        sync1 = (int)(c & 0xF0);
        if (sync1 == 0X50 || sync1 == 0XA0)
        {
// printf("found %02X @ ofs %d\n", (int)(c & 0xF0), ftell(Fp_tit));
            sync_byte_ofs = ftell(Fp_tit);
            for (j=0; j<12; j++)
            {
                fread(&c, 1, 1, Fp_tit);
// printf("-- %02X\n", (int)(c & 0xFF));
                if (feof(Fp_tit)) goto eof;
            }
            sync2 = c;
            sync2 = (int)(c & 0xF0);

            if (((sync1^sync2) & 0XF0) == 0XF0)
            {
                break;
            }
            else
            {
          /* Come back to previous file offset */
                fseek(Fp_tit, sync_byte_ofs, SEEK_SET);
                continue;
            }
        }

    /* Don't check farther than 100000 bytes */
        if (byte_cnt > 100000) break;
    }

    if (feof(Fp_tit)) goto eof;


    /* If frames not found, return error */
    if (sync_byte_ofs == 0)
    {
        fprintf(Fp_log,"  ERROR: sync_phase1: can't find TITAN format; ");
        fprintf(Fp_log,"file ofs %ld\n", ftell(Fp_tit));
        return -1;
    }

    fseek(Fp_tit, sync_byte_ofs, SEEK_SET);

    fprintf(Fp_log,"  sync_phase1: synchro at ");
    fprintf(Fp_log,"file ofs %d\n", ftell(Fp_tit));

    return sync_byte_ofs;

eof:
    fprintf(Fp_log,"  sync_phase1: EOF at ");
    fprintf(Fp_log,"file ofs %ld\n", ftell(Fp_tit));
    return -1;
}


/*==================================================================*/
check_sync(char *frame)
{
int   file_ofs;
int   frame_type;
char  str[40];
char  prev_f[14], curr_f[14], next_f[14];
int   prev_ofs, curr_ofs, next_ofs;
int i;

    file_ofs = ftell(Fp_tit);

/* Get previous, current, next synchro bits */

/* rewind 2 streams */
    fseek(Fp_tit, (file_ofs-24), SEEK_SET);

/* read previous stream */
    if (fread(frame, 1, 12, Fp_tit) != 12) return 0;
    memcpy(prev_f, frame, 12);
    prev_ofs = ftell(Fp_tit);



/* read current stream */
    if (fread(frame, 1, 12, Fp_tit) != 12) return 0;
    memcpy(curr_f, frame, 12);
    curr_ofs = ftell(Fp_tit);

    if (feof(Fp_tit))
        return 0;



/* read next stream */
    if (fread(frame, 1, 12, Fp_tit) != 12) return 0;
    memcpy(next_f, frame, 12);
    next_ofs = ftell(Fp_tit);


    fprintf(Fp_log,"\n  lost frame synchro @ ofs %ld ",
        file_ofs);
    fprintf(Fp_log,"\n");


    printf("++++ %10d ", prev_ofs);
    for (i=0; i<10; i++)
    {
        if (i && !(i%4)) printf(" ");
        printf("%02X",prev_f[i]&0XFF);
    }
    printf("  %02X  %02X\n", prev_f[10]&0XFF,prev_f[11]&0XFF);


    printf("++++ %10d ", curr_ofs);
    for (i=0; i<10; i++)
    {
        if (i && !(i%4)) printf(" ");
        printf("%02X",curr_f[i]&0XFF);
    }
    printf("  %02X  %02X\n", curr_f[10]&0XFF,curr_f[11]&0XFF);


    printf("++++ %10d ", next_ofs);
    for (i=0; i<10; i++)
    {
        if (i && !(i%4)) printf(" ");
        printf("%02X",next_f[i]&0XFF);
    }
    printf("  %02X  %02X\n", next_f[10]&0XFF,next_f[11]&0XFF);

    printf("++++\n");


/* rewind and read current stream */
    fseek(Fp_tit, file_ofs-12, SEEK_SET);
    fread(frame, 1, 12, Fp_tit);

    return 0;

}


