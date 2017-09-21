/* Public header file for steim.c	*/

#ifndef DMC_STEIM_H
#define DMC_STEIM_H

#include <sys/types.h>

typedef struct _BTIME {
	u_short year;		/* e.g. 1991				*/
	u_short day;		/* 1..366				*/
	u_char	hours;		/* 0..23				*/
	u_char	minutes;	/* 0..59				*/
	u_char	seconds;	/* 0..59, 60 for leap 			*/
	u_char	alignment_1;	/* seed v2.3 SEED manual		*/
	u_short frac_secs;	/* 0.0001 seconds, 0..9999		*/
} BTIME;

typedef struct _DATA_HEADER {
	char SequenceNumber[6];		/* offset 0, "######"		*/
	char Data_header_indicator;	/* offset 6, ('D' = 68)		*/
	char Reserved_bytes_A;		/* offset 7, (' ' = 32)		*/
	char Station_identifier_code[5];/* offset 8, station ID "#####"	*/
	char Location_identifier[2];	/* offset 13, location ID "##"	*/
	char Channel_identifier[3];	/* offset 15, channel ID "###"	*/
	char network_code[2];		/* offset 18, new in v2.3	*/
	BTIME Record_start_time;	/* offset 20,			*/
	u_short Number_of_samples;	/* offset 30,			*/
	short Sample_rate_factor;	/* offset 32,			*/
	short Sample_rate_multiplier;	/* offset 34,			*/
	char Activity_flags;		/* offset 36,			*/
	char IO_flags;			/* offset 37,			*/
	char Data_quality_flags;	/* offset 38,			*/
	char Number_of_blockettes_follow;/* offset 39,			*/
	long Time_correction;		/* offset 40, time correction	*/
	u_short Beginning_of_data;	/* offset 44, start of data	*/
	u_short First_blockette;	/* offset 46, start of 1st block*/
} DATA_HEADER;

typedef struct _BLK_1000 {
	u_short blockette_type;
	u_short next_blockette_byte;
	u_char encoding_format;
	u_char word_order;
	u_char data_rec_length;
	u_char reserved;
} BLK_1000;

typedef struct _MINISEEDHDR {
	DATA_HEADER data_hdr;
	BLK_1000 block_1000;
	u_char unused[8];
} MINISEEDHDR;

#endif /* DMC_STEIM_H	*/
