#include <stdio.h>		/* Standard I/O header file      */
#include <malloc.h>		
#include <rpc/rpc.h>
#include "ahhead.h"
#include "parse.h"

#define  MAXAH	10000

char           *this_program;

struct ah_file {
    unsigned long   epoch;
    ahhed           head;
    char           *data;
};

/******   from here to main --> definitions for command line parser ******/

#define my_entry(swit,count,type,store,size) p_entry \
        ("-", (swit), P_CASE_INSENSITIVE, (count), (type), (store), (size))
#define MAX_OTHERS 10000

int             show_help = 0;
int             length, tolerance;
char           *start_time;

arg_info        table[] = {
    my_entry("h", P_NO_ARGS, P_INT, &show_help, 1),
    my_entry("?", P_NO_ARGS, P_INT, &show_help, 1),
    my_entry("s", P_ONE_ARG, P_STRING, &start_time, 0),
    my_entry("l", P_ONE_ARG, P_INT, &length, -1),
    my_entry("t", P_ONE_ARG, P_INT, &tolerance, 0),
};				/* table */

#define table_size (sizeof (table) / sizeof (arg_info))

#define VER_NO	"96.032"
char *progname;

main(argc, argv)
    int             argc;
    char           *argv[];
{
    XDR             xdr_in, xdr_out;
    FILE           *ifp;

    char           *others[MAX_OTHERS + 1];
    unsigned long   smallest;
    int             i, j, tot_num_samps, no_files, sort_flag;
    short           sorted_flag[MAXAH], sorted[MAXAH];

    struct ah_file *files[MAXAH];
    ahhed           outhead;
    struct ah_time  date;
    int             s_time, time, start_file, end_file;
    int		    msecs_per_samp;

    char            code[CODESIZE], chan[CHANSIZE], *data;
    float           delta, offset;
    double	    epoch;

    progname = this_program = argv[0]; /* progname needed for ahio libs */

    fprintf(stderr, "%s:  Version Number  %s\n", this_program, VER_NO);

    for (i = 0; i < MAX_OTHERS + 1; ++i)
	others[i] = NULL;

    if (!parse_args(argc, argv, table, table_size, others, MAX_OTHERS)) {
	fprintf(stderr, "%s:  Illegal arguments, quitting\n",
		this_program);
	exit(1);
    }				/* if (!parse_args) */

    if (show_help) {
	do_show_help();
	exit(0);
    }				/* if (show_help) */

    if (start_time != 0) {
	sscanf(start_time, "%hd:%hd:%hd:%hd:%hd:%f", &date.yr, &date.mo,
	       &date.day, &date.hr, &date.mn, &date.sec);
	if (date.yr < 1900)
	    date.yr += 1900;
	s_time = passcal_htoe(&date);
    }

    if (!others[0]) {
	xdrstdio_create(&xdr_in, stdin, XDR_DECODE);
	no_files = read_file(files, &xdr_in);	/* read from stdin  */
    }
    else
	for (i = 0, no_files = 0; others[i]; ++i) {
	    if (!(ifp = fopen(others[i], "r"))) {
		fprintf(stderr, "Unable to open file %s.  This file not processed.\n",
			others[i]);
		continue;
	    }			/* read all traces from input files */
	    xdrstdio_create(&xdr_in, ifp, XDR_DECODE);
	    no_files += read_file(&files[no_files], &xdr_in);
	    fclose(ifp);
	}

    for (i = 0; i < no_files; ++i) {
	sorted_flag[i] = FALSE;
	sorted[i] = 0;		/* set flags and epoch time  */
	files[i]->epoch = passcal_htoe(&(files[i]->head.record.abstime));
    }

    for (i = 0; i < no_files; ++i) {
	smallest = 0xffffffff;
	for (j = 0, sort_flag = FALSE; j < no_files; ++j)
	    if ((files[j]->epoch < smallest) && (sorted_flag[j] == FALSE)) {
		smallest = files[j]->epoch;
		sorted[i] = j;
		sort_flag = TRUE;
	    }			/* sort input list by start time */

	if (sort_flag == TRUE) {
	    sorted_flag[sorted[i]] = TRUE;
	}
    }

	if (files[sorted[0]]->epoch > s_time) {
		s_time = files[sorted[0]]->epoch;
    }
    for (i = 0; files[i + 1]; ++i) {
	if (s_time == 0)
	    break;
	if (files[sorted[i]]->epoch <= s_time &&
	    files[sorted[i + 1]]->epoch > s_time)
	    break;		/* find trace containing user's start time */
    }

    if (!files[i + 1]) {

	/*
	 * reached end of files array, want to check if start time of user
	 * occurs before end of last trace
	 */
	if (s_time < (files[i]->epoch + files[i]->head.record.ndata *
		      files[i]->head.record.delta)) {
	    start_file = i;	/* it does  (see above)  */
	}
	else {
	    fprintf(stderr, "No ah files found which fall within time window.\n");
	    exit(-1);		/* user's start time falls after end of last
				 * trace  */
	}
    }
    else
	start_file = i;

    if (s_time == 0)
	for (i = 0; files[i]; ++i);
    else			/* find trace containing ending time  */
	for (++i; files[i]; ++i)
	    if (files[sorted[i]]->epoch > s_time + length)
		break;

    end_file = --i;

    /* check equality of parameters  */

    strcpy(chan, files[sorted[start_file]]->head.station.chan);
    strcpy(code, files[sorted[start_file]]->head.station.code);
    delta = files[sorted[start_file]]->head.record.delta;

    for (i = start_file + 1; i < end_file + 1; ++i) {
	if (strcmp(chan, files[sorted[i]]->head.station.chan)) {
	    fprintf(stderr, "Unable to merge files, channel numbers do not match.\n");
	    exit(-1);
	}

	if (strcmp(code, files[sorted[i]]->head.station.code)) {
	    fprintf(stderr, "Unable to merge files, station codes do not match.\n");
	    exit(-1);
	}

	if (delta != files[sorted[i]]->head.record.delta) {
	    fprintf(stderr, "Unable to merge files, sample rates do not match.\n");
	    exit(-1);
	}

/*  convert epoch to relative milliseconds  */
	files[sorted[i]]->epoch = 1000 * (files[sorted[i]]->epoch
				       - files[sorted[start_file]]->epoch) +
	    ((1000 * (files[sorted[i]]->head.record.abstime.sec
		      - (int) files[sorted[i]]->head.record.abstime.sec)) -
	     (1000 * (files[sorted[start_file]]->head.record.abstime.sec
		      - (int)
		    (files[sorted[start_file]]->head.record.abstime.sec))));

    }

    /*
     * convert user's start time to relative msecs
     */
    if (s_time != 0) {

	s_time = 1000 * (s_time - (int) files[sorted[start_file]]->epoch) -
	    1000 * (files[sorted[start_file]]->head.record.abstime.sec
		- (int) files[sorted[start_file]]->head.record.abstime.sec);

/*  adjust s_time to even boundary of sample rate in milliseconds  */
	msecs_per_samp = 1000 * delta;
	s_time = msecs_per_samp * (int) ((s_time + msecs_per_samp / 2) / 
			msecs_per_samp);
	length *= 1000;
    }
	if (s_time < 0) {
		s_time = 0;
    }

    files[sorted[start_file]]->epoch = 0;

/* check to make sure that data is continuous  */

    for (i = start_file + 1; i < end_file + 1; ++i) {
	time = (int) (files[sorted[i - 1]]->epoch + 1000 *
		      (files[sorted[i - 1]]->head.record.ndata *
		       (files[sorted[i - 1]]->head.record.delta)));
	if (!((files[sorted[i]]->epoch <= (time + tolerance)) &&
	      (files[sorted[i]]->epoch >= (time - tolerance)))) {
	    fprintf(stderr, "Unable to merge files, data not continuous\n");
	    fprintf(stderr, "	files differ by %d milliseconds.\n", 
			files[sorted[i]]->epoch - time);
	    exit(-1);
	}
    }

/*  adjust length if it's longer than what the input represents  */

    if ((files[sorted[end_file]]->epoch + 1000 *
	 (files[sorted[end_file]]->head.record.ndata * delta)) <
	(s_time + length))
	length = (files[sorted[end_file]]->epoch + 1000 *
		  (files[sorted[end_file]]->head.record.ndata * delta))
	    - s_time;

/*  compute number of data points for this output trace  */

    if (s_time == 0)
	for (i = start_file, tot_num_samps = 0; i < end_file + 1; ++i)
	    tot_num_samps += files[sorted[i]]->head.record.ndata;
    else
	tot_num_samps = (length / 1000.) / delta;

    if (s_time != 0) {
/*  offset is in milliseconds	*/
	offset = s_time;
	files[sorted[start_file]]->head.record.ndata -= ((offset / 1000.) /
	    delta );
    }
    else
	offset = 0;

    outhead = files[sorted[start_file]]->head;
    outhead.record.ndata = tot_num_samps;	/* create new header  */
    if (s_time != 0) {
	epoch = passcal_htoe(&(files[sorted[start_file]]->head.record.abstime));
	epoch += (offset / 1000);
	passcal_etoh(&(outhead.record.abstime), epoch);
	outhead.record.abstime.sec += 
		(files[sorted[start_file]]->head.record.abstime.sec - 
		    (int) files[sorted[start_file]]->head.record.abstime.sec);
    }
    logger(progname, &outhead);

    xdrstdio_create(&xdr_out, stdout, XDR_ENCODE);
    xdr_puthead(&outhead, &xdr_out);	/* output header  */

    data = (char *) (((int) files[sorted[start_file]]->data) +
		     ((int) ((offset / 1000.) / (delta) * sizeof(float)/
		      sizeof(float)) * sizeof(float)));
/*  this funny looking code aligns data to a 4-byte boundary  */

    if (end_file == start_file)
	if (s_time != 0)	/* end of desired trace is within start file */
	    files[sorted[start_file]]->head.record.ndata = (length / 1000.) /
		delta;

    tot_num_samps = files[sorted[start_file]]->head.record.ndata;
    xdr_putdata(&(files[sorted[start_file]]->head), data, &xdr_out);

    for (i = start_file + 1; i < end_file; ++i) {
	tot_num_samps += files[sorted[i]]->head.record.ndata;
	xdr_putdata(&(files[sorted[i]]->head), files[sorted[i]]->data,
		    &xdr_out);
    }

    if (end_file != start_file) {
	if (s_time != 0) {
	    offset = s_time + length - files[sorted[end_file]]->epoch;
	    files[sorted[end_file]]->head.record.ndata = (length / 1000.) /
			delta - tot_num_samps;
	}

	xdr_putdata(&(files[sorted[end_file]]->head),
		    files[sorted[end_file]]->data, &xdr_out);
    }

    for (i = 0; i < no_files; ++i) {
	cfree(files[sorted[i]]->data);
	cfree(files[sorted[i]]);
    }
}


do_show_help()
{
    fprintf(stderr, "%s: [ -s YY:MM:DD:HH:MM:SS -l length -t tolerance ah_files ] [ -h ]\n", this_program);

    fprintf(stderr, "\t-h\tShow this HELP message\n");
}				/* show_help */

read_file(file, xdr_ptr)
    struct ah_file **file;
    XDR            *xdr_ptr;
{
    int             i = 0;

    while (1) {

	if (!(file[i] = (struct ah_file *) malloc(sizeof(struct ah_file)))) {
	    fprintf(stderr, "Error allocating space in %s\n", this_program);
	    exit(-1);
	}

/* ModifOC for TITAN */
	ahAllowIntz();
/* end Modif */

	if (xdr_gethead(&(file[i]->head), xdr_ptr) != 1) {
	    cfree(file[i]);
	    file[i] = 0;
	    break;
	}
	

	if ((file[i]->data =
	     (char *) calloc((unsigned) (file[i]->head.record.ndata),
			     size(&(file[i]->head)))) == (char *) NULL) {
	    fprintf(stderr, "Error allocating space in %s\n", this_program);
	    exit(-1);
	}

	if (xdr_getdata(&(file[i]->head), file[i]->data, xdr_ptr) <= 0) {
	    fprintf(stderr, "Error reading data in %s\n", this_program);
	    exit(-1);
	}

	i++;
    }
    return (i);
}
