#include "titan.h"
#include "proto.h"
/*
 * !!! THIS version of AH I/O routines contains some extensions !!!
 *
 * 1) integer format
 * 2) compressed integer format
 */

/*    low level i/o routines for ah format records
 *        -- witte    6 june 85
 */

/* prototypes compression, private */

#ifdef ANSI_C
static void    fstdif          (int,int*,int*);
static void    rmfdif          (int,int*,int*);
static int     longcmprss      (int,char*,int*);
static int     longdcpress     (int,char*,int*);
static int     compressBlock   (int , char *, int *);
static int     decompressBlock (int , int , char *, int *);
#else
static void    fstdif          ();
static void    rmfdif          ();
static int     longcmprss      ();
static int     longdcpress     ();
static int     compressBlock   ();
static int     decompressBlock ();
#endif

/* ah error processing */

int    ah_errno = 0;
int    ah_nerr = 10;

/* ah error numbers */
#define    AE_RHED     1    /* error reading header    */
#define    AE_DTYPE    2    /* bad data type    */
#define    AE_WHED     3    /* error writing header    */
#define    AE_RDATA    4    /* error reading data    */
#define    AE_WDATA    5    /* error writing data    */
#define    AE_WRECORD  6    /* error writing record    */
#define    AE_RRECORD  7    /* error reading record    */
#define    AE_TTYOUT   8    /* binary going to tty    */
#define    AE_TTYIN    9    /* binary coming from tty    */

/* ah errlist */

char *ah_errlist[] = {
        "no error",                /* 0    no error    */
        "read header error",       /* 1    AE_RHED        */
        "bad data type",           /* 2    AE_DTYPE    */
        "write header error",      /* 3    AE_WHED        */
        "read data error",         /* 4    AE_RDATA    */
        "write data error",        /* 5    AE_WDATA    */
        "write record error",      /* 6    AE_WRECORD    */
        "read record error",       /* 7    AE_RRECORD    */
        "tty can't get binary",    /* 8    AE_TTYOUT    */
        "tty can't send binary"    /* 9    AE_TTYIN    */
};

/* compress flag */
/* automatic translation of compressed Integer to float when reading */
static int _intz2float=TRUE; 
/* automatic translation of float to compressed Integer when writing */
static int _float2intz=FALSE;

/*    gethead
 *        gets the next header from the stream pointed to by
 *        file_pt and returns this header in the structure head.
 *        file_pt is assumed to be positioned at the next header,
 *        and does not search.
 *    returns:
 *            1        ->    no error
 *           -1        ->    not enough head to read
 *           -2        ->    bad data type
 */
int gethead(head,file_pt)
FILE  *file_pt;
ahhed *head;
{
    int    ierr = 0;

    if((ierr = fread((char *)head,sizeof(ahhed),1,file_pt)) == 1)
    {
        if((head->record.type < TYPEMIN) || (head->record.type > TYPEMAX))
        {
            get_null_head(head);
            ierr = -2;    /* bad data type */
            ah_errno= AE_DTYPE;
        }
    }
    else        /* not enough head */
    {
        get_null_head(head);
        ierr = -1;
        ah_errno= AE_RHED;
    }
    return(ierr);
}



/*    puthead
 *        writes the header head onto the stream pointed to by
 *        file_pt.
 *    returns:
 *            1        ->    no error
 *            -1        ->    error writing header
 */
int    puthead(head,file_pt)
    FILE    *file_pt;
    ahhed    *head;
{
    int    ierr = 0;

    if((ierr= fwrite((char *)head,sizeof(ahhed),1,file_pt)) != 1)
    {
        ah_errno= AE_WHED;
        ierr= -1;
    }
    return(ierr);
}



/*    size
 *        returns the size (in bytes) of the data type given by
 *        head->record.type.
 *    returns:
 *            size of data type    ->    no error
 *            -1            ->    unknown data type
 *    Alors ici ATTENTION, Nous introduisons un nouveau type de variable
 *    entier short (2 octets), on le lit comme un entier, mais pour ne pas
 *    mettre le foutoir dans tous les programmes, on convertit en reel pour
 *    tous les traitements. Aussi la place a allouer correspond elle au meme
 *    format que les reels !...
 */
int    size(head)
    ahhed    *head;
{
    int    type_size = 0;

    switch(head->record.type)
    {
    case INT:           /* integer time series */
    case INTZ:          /* compressed integer time series */
        type_size= sizeof(int);
        break;
    case FLOAT:         /* real time series */
    case SHORT:         /* short integer    */
    case USGS:          /* 12 bits short integer*/
        type_size= sizeof(float);
        break;
    case COMPLEX:       /* complex time series */
        type_size= sizeof(complex);
        break;
    case VECTOR:        /* real x,y pairs */
        type_size= sizeof(vector);
        break;
    case TENSOR:        /* x real, y complex, or real x,y,z */
        type_size= sizeof(tensor);
        break;
    case 5:             /* complex x,y pairs */
        type_size= 2*sizeof(complex);
        break;
    case DOUBLE:        /* double */
        type_size=sizeof(double);
        break;
    default:            /* unknown data type */
        type_size= -1;
        ah_errno= AE_DTYPE;
        break;
    }
    return(type_size);
}


/*    tohead
 *        positions the read/write head to the beginning of the
 *        n-th header in the file pointed to by file_pt.
 *    returns:
 *            n    ->    no error
 *           -1    ->    not enough heads
 *           -2    ->    bad seek
 */
int    tohead(n,file_pt)
    FILE    *file_pt;
    int    n;
{
    ahhed    head;
    int    i,ierr;

    rewind(file_pt);
    for(i=1; i<n; ++i)
    {
        if(gethead(&head,file_pt) == 1)
        {
            if(fseek(file_pt,(long)(head.record.ndata)*(size(&head)),1) == -1)
            {
                ierr = -2;    /* bad seek */
                ah_errno= AE_RHED;
                return(ierr);
            }
        }
        else
        {
            ierr = -1;    /* not enough head */
            ah_errno= AE_RHED;
            return(ierr);
        }
    }
    return(i);    /* success */
}



/*    getdata
 *        reads from the file pointed to by file_pt into
 *        the array pointed to by array.  It assumes that
 *        the read/write head is positioned correctly 
 *        (i.e., right after the header), and does not
 *        search.  Works for any allowed data type.
 *    returns:
 *            number of elements read    ->    OK
 *            -1            ->    error
 */
int    getdata(head,array,file_pt)
    ahhed   *head;
    char    *array;
    FILE    *file_pt;

{
    int ierr = 0;

    if((ierr = fread(array,size(head),(int)head->record.ndata,file_pt)) != (int)head->record.ndata)
    {
        ah_errno= AE_RDATA;
        ierr = -1;
    }

    return(ierr);
}


/*    putdata
 *        writes array to the file pointed to by
 *        file_pt.  Works for any allowed data type.
 *    returns:
 *            number of elements written    ->    OK
 *            -1            ->    error
 */
int    putdata(head,array,file_pt)
    ahhed    *head;
    char    *array;
    FILE    *file_pt;
{
    int    ierr = 0;

    if((ierr = fwrite(array,size(head),(int)head->record.ndata,file_pt)) != (int)head->record.ndata)
    {
        ah_errno= AE_WDATA;
        ierr = -1;
    }
    return(ierr);
}


/*    putrecord
 *        writes head and array to the file pointed to by
 *        file_pt.  Works for any allowed data type.
 *    returns:
 *            0    ->    OK
 *            -1    ->    error writing header
 *            -2    ->    error writing data
 */
int    putrecord(head,array,file_pt)
    ahhed   *head;
    char    *array;
    FILE    *file_pt;
{
    int    ierr = 0;

    (puthead(head,file_pt) == 1) ? ((putdata(head,array,file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    if(ierr)
        ah_errno= AE_WRECORD;

    return(ierr);
}


/*    getrecord
 *        gets header and data from the file pointed to by
 *        file_pt and puts them in head and array.  It assumes
 *        that the read/write head is positioned at the beginning
 *        of the header, and does not search.  Obviously, calling
 *        routine must have allocated enough space.
 *    returns:
 *            0    ->    OK
 *           -1    ->    error reading header
 *           -2    ->    error reading data
 */
int    getrecord(head,array,file_pt)
    ahhed   *head;
    char    *array;
    FILE    *file_pt;
{
    int    ierr = 0;

    (gethead(head,file_pt) == 1) ? ((getdata(head,array,file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    if(ierr)
        ah_errno= AE_RRECORD;
    return(ierr);
}

/*
 *    getrecord2
 *        gets header and data from the file pointed to by
 *        file_pt and puts them in head and array.  It assumes
 *        that the read/write head is positioned at the beginning
 *        of the header, and does not search (although it does
 *        some error checking).  Space for array is allocated, so
 *        be sure to pass a pointer to the data pointer. Got it?
 *    returns:
 *            0    ->    ok
 *           -1    ->    error reading record
 *           -2    ->    error allocating space for data
 */
int    getrecord2(head,array,file_pt)
    ahhed    *head;
    char    **array;
    FILE    *file_pt;
{
    int    ierr = 0;
    int    gethead();
    char    *mkdatspace();

    if(gethead(head, file_pt) != 1) {
        ierr = -1;
        return(ierr);
    }

    *array= mkdatspace(head);
    if(*array == NULL) {
        ierr= -2;
        return(ierr);
    }

    if(getdata(head,*array,file_pt) < 0)
        ierr= -1;

    return(ierr);
}


/*    gogethead
 *        gets n-th header from the stream pointed to by
 *        file_pt and returns this header in the structure
 *        head.
 *    returns:
 *            0    ->    OK
 *           -1    ->    stream not long enough
 *           -2    ->    error reading header
 */
int    gogethead(n,head,file_pt)
    int    n;
    ahhed    *head;
    FILE    *file_pt;
{
    int    ierr = 0;

    (tohead(n,file_pt) == n) ? ((gethead(head,file_pt) < 1) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    return(ierr);
}


/*    gogetrecord
 *        gets n-th record (header and data) from the stream
 *        pointed to by file_pt and places it in head and array.
 *        Calling routine must allocate enough space.
 *    returns:
 *            0    ->    OK
 *           -1    ->    stream not long enough
 *           -2    ->    error reading record
 */
int    gogetrecord(n,head,array,file_pt)
    int    n;
    ahhed    *head;
    char    *array;
    FILE    *file_pt;

{
    int    ierr = 0;

    (tohead(n,file_pt) == n) ? ((getrecord(head,array,file_pt) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    return(ierr);
}

/* logger adds a 10 character comment to the log section of the header
 * comment should be passed as a character pointer must be terminated
 * by a ';' and a `/0`
 * returns:
 *    logger =  0  -> log info added to header structure
 *    logger = -1  -> no ';', added
 *    logger = -2  -> input string greater than LOGENT
 *                      input truncated to allowable limit
 *    logger = -3  -> attempt to make log string greater than LOGSIZE
 *                      input comment truncated to fit 
 *
 *            written by Tom Boyd   6/10/85
 */

int logger(char_pt,head_pt)
ahhed *head_pt;
char *char_pt;

{
    int org,in,err,diff;

    err=0;

/* find length of log array and input array  */

    org=strlen(head_pt->record.log);  /*log array*/
    in=strlen(char_pt);  /*input array*/

/* check for a terminating ':' in the input array */

    if(*(char_pt+in-1) != ';')
    {                   /* no semicolon----add it*/
        err=(-1);
        *(char_pt+in)=';';
        *(char_pt+in+1)='\0';
        in+=1;
    }

/* check the length of the input array */

    if(in > LOGENT)
    {                  /* entry length too long-----truncate it*/
        err=(-2);
        *(char_pt+LOGENT-1)=';';
        *(char_pt+LOGENT)='\0';
        in=LOGENT;
    }

/* check combined length of array and new input and add it */

    diff=LOGSIZE-(org+in);
    if(diff == -in) return(-3);  /* no room left in log array */
    if(diff < 0)diff*=(-1),err=(-3);  /*partial room left----use it */    
    strncat(head_pt->record.log,char_pt,diff); /* cat two strings */

    return(err);
}



/*    out_is_tty
 *        determines whether stdout is being sent to screen.
 *    returns:
 *            0    ->    stdout is not tty
 *            1    ->    stdout is tty
 */
int    out_is_tty()

{

    if(isatty(1))    /* sun specific --- stdout */
    {
        ah_errno= AE_TTYOUT;
        return(1);
    }
    return(0);
}


/*    in_is_tty
 *        determines whether stdin is tty
 *    returns:
 *            0    ->    stdin is not tty
 *            1    ->    stdin is tty
 */
int    in_is_tty()
{

    if(isatty(0))    /* sun specific --- stdin */
    {
        ah_errno= AE_TTYIN;
        return(1);
    }
    return(0);
}


/*    mkdatspace
 *        allocates enough space for the data array, and
 *        returns a pointer to the memory location, or
 *        NULL if failure.
 *    returns:
 *            character pointer    ->    success
 *            NULL            ->    failure
 */
char    *mkdatspace(head)
    ahhed    *head;
{
    int    dsize;
    dsize=head->record.ndata*size(head);
    return((char*)malloc(dsize));
    
}



void get_null_head(hed)
ahhed    *hed;
{
int    i;

    strcpy(hed->station.code,"null");
    strcpy(hed->station.chan,"null");
    strcpy(hed->station.stype,"null");
    hed->station.slat= 0.0;
    hed->station.slon= 0.0;
    hed->station.elev= 0.0;
    hed->station.DS= 0.0;
    hed->station.A0= 0.0;
    for(i=0; i< NOCALPTS; ++i)
    {
        hed->station.cal[i].pole.r= 0.0;
        hed->station.cal[i].pole.i= 0.0;
        hed->station.cal[i].zero.r= 0.0;
        hed->station.cal[i].zero.i= 0.0;
    }

    hed->event.lat= 0.0;
    hed->event.lon= 0.0;
    hed->event.dep= 0.0;
    hed->event.ot.yr= (short)0;
    hed->event.ot.mo= (short)0;
    hed->event.ot.day= (short)0;
    hed->event.ot.hr= (short)0;
    hed->event.ot.mn= (short)0;
    hed->event.ot.sec= 0.0;
    strcpy(hed->event.ecomment,"null");

    hed->record.type= (short)0;
    hed->record.ndata= 0L;
    hed->record.delta= 0.0;
    hed->record.maxamp= 0.0;
    hed->record.abstime.yr= (short)0;
    hed->record.abstime.mo= (short)0;
    hed->record.abstime.day= (short)0;
    hed->record.abstime.hr= (short)0;
    hed->record.abstime.mn= (short)0;
    hed->record.abstime.sec= 0.0;
    hed->record.rmin= 0.0;
    strcpy(hed->record.rcomment,"null");
    strcpy(hed->record.log,"null");

    for(i=0; i< NEXTRAS; ++i)
        hed->extra[i]= 0.0;

    return;
}

/* acpy(from,to,nbytes) copies nbytes from the array "from" to the
 *    array "to".
 */
void acpy(from,to,nbytes)
char    *from;
char    *to;
unsigned    nbytes;
{
    while(nbytes--)
        *from++ = *to++;
    return;
}


void ah_error(s1,s2,status)   /* print ah format error message and die */
char    *s1,*s2;
int    status;
{
    static    char    progname[]="AH:";

    if(progname)
        fprintf(stderr,"%s: ",progname);
    fprintf(stderr,s1,s2);
    if(ah_errno > 0 && ah_errno < ah_nerr)
        fprintf(stderr," (%s)",ah_errlist[ah_errno]);
    fprintf(stderr,"\n");
    exit(status);
}


/*
 *    maxamp
 *        determines the maximum absolute amplitude of the data array, and
 *        places that number in head.record.maxamp.
 *    returns:
 *            0    ->    ok
 *           -1    ->    error
 */
int    maxamp(head,data)
     ahhed   *head;
     char    *data;
{
    float  *fpt;
    double *dpt, dmin, dmax;
    float  fmax, fmin;
    long   n_data_pts;

    switch(head->record.type)
    {
    case FLOAT:
        n_data_pts= head->record.ndata;
        break;
    case COMPLEX:
    case VECTOR:
        n_data_pts= 2 * head->record.ndata;
        break;
    case TENSOR:
        n_data_pts= 3 * head->record.ndata;
        break;
    case 5:
        n_data_pts= 4 * head->record.ndata;
        break;
    case DOUBLE:
        n_data_pts= head->record.ndata;
        break;
    case SHORT:
    case USGS:
        n_data_pts= head->record.ndata;
        break;
    default:
        ah_errno= AE_DTYPE;
        return(-1);
        break;
    }

    if (head->record.type == DOUBLE) {
        dpt= (double *)data;
        dmax= dmin= *dpt;
        while(n_data_pts--)
        {
            dmax= MAX(dmax,*dpt);
            dmin= MIN(dmin,*dpt);
            ++dpt;
        }
        ((fabs(dmax) > fabs(dmin)) ? (head->record.maxamp= (float) dmax) : (head->record.maxamp= (float) -dmin));
    }
    else {
        fpt= (float *)data;
        fmax= fmin= *fpt;
        while(n_data_pts--)
        {
            fmax= MAX(fmax,*fpt);
            fmin= MIN(fmin,*fpt);
            ++fpt;
        }
        ((fabs((double)fmax) > fabs((double)fmin)) ? (head->record.maxamp= fmax) : (head->record.maxamp= -fmin));
    }

    return(0);
}
/*    xdr_gethead
 *        gets the next header from the xdr stream pointed to by
 *        xdrs and returns this header in the structure head.
 *        xdrs is assumed to be positioned at the next header,
 *        and does not search.
 *    returns:
 *            1        ->    no error
 *           -1        ->    not enough head to read
 *           -2        ->    bad data type
 */
int    xdr_gethead(head,xdrs)
    XDR    *xdrs;
    ahhed  *head;
{
    int    ierr = 0;

    if((ierr = xdr_ahhead(xdrs, head)) == 1)
    {
        if((head->record.type < TYPEMIN) || (head->record.type > TYPEMAX))
        {
            get_null_head(head);
            ierr = -2;    /* bad data type */
            ah_errno= AE_DTYPE;
        }
    }
    else        /* not enough head */
    {
        get_null_head(head);
        ierr = -1;
        ah_errno= AE_RHED;
    }
    head->record.ftype=head->record.type;
    if ((head->record.type==INTZ) && _intz2float) 
            head->record.type=FLOAT;
    return(ierr);
}



/*    xdr_puthead
 *        writes the header head onto the xdr stream pointed to by
 *        xdrs.
 *    returns:
 *            1        ->    no error
 *           -1        ->    error writing header
 */
int    xdr_puthead(head,xdrs)
    ahhed  *head;
    XDR    *xdrs;
{
    int    ierr = 0, type;

    type=head->record.type;
    if ((head->record.type==FLOAT) && _float2intz) 
        head->record.type=INTZ;
    ierr= xdr_ahhead(xdrs, head);
    head->record.type=type;
    if(ierr != 1)
    {
        ah_errno= AE_WHED;
        ierr= -1;
    }
    return(ierr);
}



/*    xdr_tohead
 *        positions the read/write head to the beginning of the
 *        n-th header in the xdr stream pointed to by xdrs.
 *    returns:
 *            n    ->    no error
 *           -1    ->    not enough heads
 *           -2   ->    bad seek
 */
int    xdr_tohead(n,xdrs)
    XDR    *xdrs;
    int    n;
{
    ahhed    head;
    int      i,ierr;

/* be warned: the following xdr_setpos call may not work at all     */
/* depending on the stream.  The use of 0 to get to the beginning     */
/* works empirically, but is not documented  ... sigh    - dws        */
    xdr_setpos(xdrs, (u_int) 0);

    for(i=1; i<n; ++i)
    {
        if(xdr_gethead(&head,xdrs) == 1)
        {
            skip_data(&head,xdrs);
        }
        else
        {
            ierr = -1;    /* not enough head */
            ah_errno= AE_RHED;
            return(ierr);
        }
    }
    return(i);    /* success */
}



/*    xdr_getdata
 *        reads from the xdr stream pointed to by xdrs into
 *        the array pointed to by array.  It assumes that
 *        the read/write head is positioned correctly 
 *        (i.e., right after the header), and does not
 *        search.  Works for any allowed data type.
 *    returns:
 *            number of elements read    ->    OK
 *            -1            ->    error
 */
int    xdr_getdata(head,array,xdrs)
    ahhed  *head;
    char   *array;
    XDR    *xdrs;

{
    int ierr = 0;
    int     *pint;
    float   *pfloat,dummy;
    double  *pdouble;
    complex *pcomplex;
    tensor  *ptensor;
    int i;

    switch(head->record.ftype) {
    case INTZ:
      {
        char   *strp=(char*)malloc(sizeof(int)*head->record.ndata);
        int    nchar;
        pint = (int *) array;
        pfloat = (float *) array;
        if (! xdr_int(xdrs, &nchar) ) {
            ah_errno= AE_RDATA;
            ierr = -1;
            return(ierr);
        }
        xdr_bytes(xdrs,&strp,&nchar,nchar);
        decompressBlock(nchar,head->record.ndata,strp,pint);
/* translate to float values */
        if (_intz2float) {
            for (i=0;i<head->record.ndata;i++)
                pfloat[i]=(float)pint[i];
        }
        free(strp);
        ierr=head->record.ndata;
       }
        break;
    case INT:
        pint = (int *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (! xdr_int(xdrs, pint++) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case FLOAT:
        pfloat = (float *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (! xdr_float(xdrs, pfloat++) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case COMPLEX:
    case VECTOR:
        pcomplex = (complex *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (!xdr_float(xdrs, &(pcomplex->r)) ||
                !xdr_float(xdrs, &(pcomplex++->i))) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case TENSOR:
        ptensor = (tensor *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (!xdr_float(xdrs, &(ptensor->xx)) ||
                !xdr_float(xdrs, &(ptensor->yy)) ||
                !xdr_float(xdrs, &(ptensor++->xy))) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case 5:
        pfloat = (float *) array;
        for (i = 0; i < 4 * head->record.ndata; i++) {
            if (! xdr_float(xdrs, pfloat++) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case DOUBLE:
        pdouble = (double *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (! xdr_double(xdrs, pdouble++) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case SHORT:
        pfloat = (float *) array;
        for (i = 0; i < head->record.ndata/2; i++) {
            if (! xdr_short16(xdrs, &pfloat[2*i], &pfloat[2*i+1])) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ierr+=2;
        }
        if ((i=head->record.ndata) & 0x0001) {
            if (! xdr_short16(xdrs, &pfloat[i-1], &pfloat[i-1])) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
        ++ierr;
        }
        break;
    case USGS:
        pfloat = (float *) array;
        for (i = 0; i < head->record.ndata /2; i++) {
            if (! xdr_short12(xdrs, &pfloat[2*i], &pfloat[2*i+1])) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ierr+=2;
        }
        if ((i=head->record.ndata) & 0x0001) {
            if (! xdr_short12(xdrs, &pfloat[i-1], &dummy)) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    default:
        ierr = -1;
        ah_errno= AE_DTYPE;
        return(ierr);
    }
    return(ierr);
}


/*    xdr_putdata
 *        writes array to the xdr stream pointed to by xdrs.
 *        Works for any allowed data type.
 *    returns:
 *            number of elements written    ->    OK
 *            -1            ->    error
 */
int    xdr_putdata(head,array,xdrs)
    ahhed  *head;
    char   *array;
    XDR    *xdrs;
{
    int     ierr = 0;
    int     *pint;
    float   *pfloat,dummy=0;
    double  *pdouble;
    complex *pcomplex;
    tensor  *ptensor;
    int i;

    if ((head->record.type==FLOAT) && _float2intz) {
        head->record.type=INTZ;
        pfloat = (float *) array;
        pint = (int *) array;
        for (i=0;i<head->record.ndata;i++)
                pint[i]=(int)pfloat[i];
    }

    switch(head->record.type) {
    case INTZ:
          {
                char    *strp=(char*)malloc(sizeof(int)*head->record.ndata);
                int     nchar;
                pint = (int *) array;
                nchar=compressBlock(head->record.ndata,strp,pint);
                if (! xdr_int(xdrs, &nchar) ) {
                         ah_errno= AE_RDATA;
                         ierr = -1;
                          return(ierr);
                }
                xdr_bytes(xdrs,&strp,&nchar,nchar);
                free(strp);
           }
                break;
    case INT:
        pint = (int *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (! xdr_int(xdrs, pint++) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case FLOAT:
        pfloat = (float *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (! xdr_float(xdrs, pfloat++) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case COMPLEX:
    case VECTOR:
        pcomplex = (complex *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (!xdr_float(xdrs, &(pcomplex->r)) ||
                !xdr_float(xdrs, &(pcomplex++->i))) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case TENSOR:
        ptensor = (tensor *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (!xdr_float(xdrs, &(ptensor->xx)) ||
                !xdr_float(xdrs, &(ptensor->yy)) ||
                !xdr_float(xdrs, &(ptensor++->xy))) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case 5:
        pfloat = (float *) array;
        for (i = 0; i < 4 * head->record.ndata; i++) {
            if (! xdr_float(xdrs, pfloat++) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case DOUBLE:
        pdouble = (double *) array;
        for (i = 0; i < head->record.ndata; i++) {
            if (! xdr_double(xdrs, pdouble++) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case SHORT:
        pfloat = (float *) array;
        for (i = 0; i < head->record.ndata/2; i++) {
            if (! xdr_short16(xdrs, &pfloat[2*i], &pfloat[2*i+1])) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ierr+=2;
        }
        if ((i=head->record.ndata) & 0x0001) {
            if (! xdr_short16(xdrs, &pfloat[i-1], &pfloat[i-1])) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    case USGS:
        pfloat = (float *) array;
        for (i = 0; i < head->record.ndata/2; i++) {
            if (! xdr_short12(xdrs, &pfloat[2*i], &pfloat[2*i+1])) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ierr+=2;
        }
        if ((i=head->record.ndata) & 0x0001) {
            if (! xdr_short12(xdrs, &pfloat[i-1], &dummy) ) {
                ah_errno= AE_RDATA;
                ierr = -1;
                return(ierr);
            }
            ++ierr;
        }
        break;
    default:
        ierr = -1;
        ah_errno= AE_DTYPE;
        return(ierr);
    }
    return(ierr);
}


/*    xdr_putrecord
 *        writes head and array to the xdr stream pointed to by xdrs.
 *        Works for any allowed data type.
 *    returns:
 *            0    ->    OK
 *           -1    ->    error writing header
 *           -2    ->    error writing data
 */
int    xdr_putrecord(head,array,xdrs)
    ahhed  *head;
    char   *array;
    XDR    *xdrs;
{
    int    ierr = 0;

    (xdr_puthead(head,xdrs) == 1) ? ((xdr_putdata(head,array,xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    if(ierr)
        ah_errno= AE_WRECORD;

    return(ierr);
}


/*    xdr_getrecord
 *        gets header and data from the xdr stream pointed to by
 *        xdrs and puts them in head and array.  It assumes
 *        that the read/write head is positioned at the beginning
 *        of the header, and does not search.  Obviously, calling
 *        routine must have allocated enough space.
 *    returns:
 *            0    ->    OK
 *           -1    ->    error reading header
 *           -2    ->    error reading data
 */
int    xdr_getrecord(head,array,xdrs)
    ahhed    *head;
    char    *array;
    XDR    *xdrs;
{
    int    ierr = 0;

    (xdr_gethead(head,xdrs) == 1) ? ((xdr_getdata(head,array,xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    if(ierr)
        ah_errno= AE_RRECORD;
    return(ierr);
}

/*
 *    xdr_getrecord2
 *        gets header and data from the xdr stream pointed to by
 *        xdrs and puts them in head and array.  It assumes
 *        that the read/write head is positioned at the beginning
 *        of the header, and does not search (although it does
 *        some error checking).  Space for array is allocated, so
 *        be sure to pass a pointer to the data pointer. Got it?
 *    returns:
 *            0    ->    ok
 *           -1    ->    error reading record
 *           -2    ->    error allocating space for data
 */
int    xdr_getrecord2(head,array,xdrs)
    ahhed  *head;
    char   **array;
    XDR    *xdrs;
{
    int    ierr = 0;
    int    xdr_gethead();
    char   *mkdatspace();

    if(xdr_gethead(head, xdrs) != 1) {
        ierr = -1;
        return(ierr);
    }

    *array= mkdatspace(head);
    if(*array == NULL)
    {
        ierr= -2;
        return(ierr);
    }

    if(xdr_getdata(head,*array,xdrs) < 0)
        ierr= -1;

    return(ierr);
}


/*    xdr_gogethead
 *        gets n-th header from the xdr stream pointed to by
 *        xdrs and returns this header in the structure
 *        head.
 *    returns:
 *            0    ->    OK
 *           -1    ->    stream not long enough
 *           -2    ->    error reading header
 */
int    xdr_gogethead(n,head,xdrs)
    int    n;
    ahhed  *head;
    XDR    *xdrs;
{
    int    ierr = 0;

    (xdr_tohead(n,xdrs) == n) ? ((xdr_gethead(head,xdrs) < 1) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    return(ierr);
}


/*    xdr_gogetrecord
 *        gets n-th record (header and data) from the xdr stream
 *        pointed to by xdrs and places it in head and array.
 *        Calling routine must allocate enough space.
 *    returns:
 *            0    ->    OK
 *           -1    ->    stream not long enough
 *           -2    ->    error reading record
 */
int    xdr_gogetrecord(n,head,array,xdrs)
    int    n;
    ahhed  *head;
    char   *array;
    XDR    *xdrs;

{
    int    ierr = 0;

    (xdr_tohead(n,xdrs) == n) ? ((xdr_getrecord(head,array,xdrs) < 0) ? (ierr = -2) : (ierr = 0)) : (ierr = -1);
    return(ierr);
}






int xdr_ahhead(xdrsp, ahheadp)
XDR *xdrsp;
ahhed *ahheadp;
{
    u_int l;
    char  **pp,  *p;
    float **ppf, *pf;

    l = CODESIZE;
    p = ahheadp->station.code;
    pp = &p;
    if (!xdr_bytes(xdrsp, pp, &l, (u_int) CODESIZE))
        return(0);
    l = CHANSIZE;
    p = ahheadp->station.chan;
    pp = &p;
    if (!xdr_bytes(xdrsp, pp, &l, CHANSIZE))
        return(0);
    l = STYPESIZE;
    p = ahheadp->station.stype;
    pp = &p;
    if (!xdr_bytes(xdrsp, pp, &l, STYPESIZE))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->station.slat))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->station.slon))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->station.elev))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->station.DS))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->station.A0))
        return(0);
    for (l = 0; l < NOCALPTS; l++) {
        if (!xdr_float(xdrsp, &ahheadp->station.cal[l].pole.r))
            return(0);
        if (!xdr_float(xdrsp, &ahheadp->station.cal[l].pole.i))
            return(0);
        if (!xdr_float(xdrsp, &ahheadp->station.cal[l].zero.r))
            return(0);
        if (!xdr_float(xdrsp, &ahheadp->station.cal[l].zero.i))
            return(0);
    }
    if (!xdr_float(xdrsp, &ahheadp->event.lat))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->event.lon))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->event.dep))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->event.ot.yr))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->event.ot.mo))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->event.ot.day))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->event.ot.hr))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->event.ot.mn))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->event.ot.sec))
        return(0);
    l = COMSIZE;
    p = ahheadp->event.ecomment;
    pp = &p;
    if (!xdr_bytes(xdrsp, pp, &l, COMSIZE))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->record.type))
        return(0);
    if (!xdr_long(xdrsp, &ahheadp->record.ndata))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->record.delta))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->record.maxamp))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->record.abstime.yr))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->record.abstime.mo))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->record.abstime.day))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->record.abstime.hr))
        return(0);
    if (!xdr_short(xdrsp, &ahheadp->record.abstime.mn))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->record.abstime.sec))
        return(0);
    if (!xdr_float(xdrsp, &ahheadp->record.rmin))
        return(0);
    l = COMSIZE;
    p = ahheadp->record.rcomment;
    pp = &p;
    if (!xdr_bytes(xdrsp, pp, &l, COMSIZE))
        return(0);
    l = LOGSIZE;
    p = ahheadp->record.log;
    pp = &p;
    if (!xdr_bytes(xdrsp, pp, &l, LOGSIZE))
        return(0);
    l = NEXTRAS;
    pf = ahheadp->extra;
    ppf = &pf;
    if (!xdr_array(xdrsp, (caddr_t *)ppf, &l, NEXTRAS,
                   sizeof(float), (xdrproc_t) xdr_float))
        return(0);
    
    return(1);
}


int skip_data(head, xdrs)
/* skip xdr ah data            */
/*                             */
/* returns                     */
/*         0 ---> OK           */
/*        -1 ---> error        */
ahhed *head;
XDR *xdrs;
{
    int ierr;
    unsigned int pos,offset;

    pos=xdr_getpos(xdrs);
    ierr = 0;
    switch(head->record.type) {
    case INTZ:
        {
        int nchar;
        xdr_int(xdrs,&nchar);
        offset=nchar;
        pos+=sizeof(int);
        }
        break;
    case INT:
        offset = head->record.ndata * sizeof(int);
        break;
    case FLOAT:
        offset = head->record.ndata * sizeof(float);
        break;
    case COMPLEX:
    case VECTOR:
        offset = head->record.ndata * 2 *sizeof(float);
        break;
    case TENSOR:
        offset = head->record.ndata * 3 *sizeof(float);
        break;
    case 5:
        offset = head->record.ndata * 4 *sizeof(float);
        break;
    case DOUBLE:
        offset = head->record.ndata * 2 *sizeof(float);
        break;
    case SHORT:
    case USGS:
        offset = head->record.ndata/2 * sizeof(float);
        if (0x0001 & head->record.ndata) offset += 4;
        break;
    default:
        ierr = -1;
        return(ierr);
    }

    xdr_setpos(xdrs, offset + pos);
    return(ierr);
}

bool_t xdr_short12 (xdrs,fp1,fp2)

XDR *xdrs;
float *fp1,*fp2;
{
float fp;
short *sp,bs=0x8000,seize=16;
unsigned short shp;
bool_t ret = 0;

sp = (short *) &fp;
if (xdrs->x_op == XDR_ENCODE ) {
    shp = ((short) (fp1[0])) * seize;
    shp = shp >> 4;
    sp[0] = shp | bs;
    shp = ((short) (fp2[0])) * seize;
    shp = shp >> 4;
    sp[1] = shp | bs;
    ret = xdr_float(xdrs,&fp);
} else if (xdrs->x_op == XDR_DECODE ) {
    ret = xdr_float(xdrs,&fp);
    shp = sp[0];
    shp = shp << 4;
    fp1[0] = (float) ( ((short) shp) / seize);
    shp = sp[1];
    shp = shp << 4;
    fp2[0] = (float) ( ((short) shp) / seize);
}
return(ret);
}

bool_t xdr_short16 (xdrs,fp1,fp2)
XDR *xdrs;
float *fp1,*fp2;
{
float fp;
short *sp;
bool_t ret = 0;
sp = (short *) &fp;

if (xdrs->x_op == XDR_ENCODE ) {
    sp[0] = (short) *fp1;
    sp[1] = (short) *fp2;
    ret = xdr_float(xdrs,&fp);
} else if (xdrs->x_op == XDR_DECODE ) {
    ret = xdr_float(xdrs,&fp);
    fp1[0] = (float) sp[0];
    fp2[0] = (float) sp[1];
}
return(ret);
}

static int compressBlock(nin, dataout, datain)
int nin;
char *dataout;
int *datain;
{
    int    nchar;
    int    *tmp=(int*)malloc(nin*sizeof(int));

    fstdif(nin,datain,tmp);
    fstdif(nin,tmp,tmp);

    nchar=longcmprss(nin,dataout,tmp);

    free(tmp);
    return(nchar);
}
static int decompressBlock(nchar, maxsize, datain, dataout)
int nchar;
int maxsize;
char *datain;
int  *dataout;
{
    int    nlong;
    int    *tmp=(int*)malloc(maxsize*sizeof(int));

    nlong=longdcpress(nchar,datain,tmp);

    rmfdif(nlong,tmp,tmp);
    rmfdif(nlong,tmp,dataout);

    free(tmp);
    return(nlong);
}

/*  @(#)fstdif.c    1.1  6/2/92  */

/*     Suboutine to first-difference long data.
       Second differences can be performed by calling this
       routine twice.
       Inputs are integer array in of length num.
       Output is integer array out. */

static void fstdif(num, in, out)
int num;
int *in, *out;
{
int i;
int temp, old;

    old = 0;
    for (i = 0; i < num; i++)
    {
        temp = in[i];
        out[i] = temp - old;
        old = temp;
    }
}

/*     Suboutine to remove first-difference on long data.
       Second differences can be removed by calling this
       routine twice.
       Inputs are integer array in of length num.
       Output is integer array out. */

static void rmfdif(num, in, out)
int num;
int *in, *out;
{
int i;
out[0] = in[0];

    for (i = 1; i < num; i++)
        out[i] = out[i-1] + in[i];
}

/* @(#)cmprss.c    1.3  14 Sep 1992  */

/*     Suboutine to word-compress long data.
       8-bit version
       Inputs are integer array in of length numin.
           Input data must be cast to long.
       Outputs are character string out of length ioff. */

static int longcmprss(nsamp, out, in)
int nsamp;
int *in;
char *out;
{
    int carryflag, negflag;
    int ioff=0, i;
    int j, jn, in1;
    int n6=64;
    int n7=128,       n7m1=127;
    int n13=8192;
    int n14=16384,    n14m1=16383;
    int n20=1048576;
    int n21=2097152,  n21m1=2097151;
    int n28m1=134217727;

    carryflag = n7;

    for (i = 0; i < nsamp; i++)
    {
      jn = in[i];
      in1 = in[i];
      negflag = 0;
      if (in1 < 0)
      {
          negflag = n6;                  /* If here, number is -ve. */
          jn = -jn;
          in1 = -in1;
      }
      
      if (in1 >= n20)
      {
          if (in1 > n28m1) jn = n28m1;
          j = jn / n21 + negflag + carryflag;  /* Fill a byte */
          out[ioff] = j;
          ioff++;
          jn = jn & n21m1;
          negflag = 0;
      }

      if (in1 >= n13)
      {
      /* Fill next most significant byte */
          j = jn / n14 + negflag + carryflag;
          out[ioff] = j;
          ioff++;
          jn = jn & n14m1;
          negflag = 0;
      }

      if (in1 >= n6)
      {
      /* Fill next most significant byte */
          j = jn / n7 + negflag + carryflag;
          out[ioff] = j;
          ioff++;
          jn = jn & n7m1;
          negflag = 0;
      }

      /* Fill next most significant byte */
      j = jn + negflag;
      out[ioff] = j;
      ioff++;
    }
    return(ioff);         /* Finished, so return length of output */
}


/*  @(#)dcpress.c    1.2  9/14/92  */

/* Subroutine decompresses compressed data and returns it in 
   an array of long ints
   8-bit version
   compression technique
   in - input string of characters containing compressed data
   out - ouput array of long ints containing decompressed data
   numin - number of characters in input character string
*/

static int longdcpress(numin, in, out)
int numin, *out;
char *in;
{
  int isign=64, ioflow=128, i, j;
  int jsign, joflow, mask1, mask2;
  int itemp;
  mask1 = 63;
  mask2 = 127;
  i = 0;
  j = 0;
  
  /*start of decoding */
  /* and for sign */
  
  for (i = 0; i < numin; )
  {
    jsign = in[i] & isign;
    joflow = in[i] & ioflow;
    itemp = in[i] & mask1;

    for ( ; ; )
    {
      i = i + 1; 
      if (joflow == 0) break;
      
      /* there is another byte in this sample */
      itemp = itemp << 7;
      joflow = in[i] & ioflow;
      itemp = itemp + (in[i] & mask2);
    }

    if (jsign != 0) itemp = -itemp;
    out[j] = itemp;
    j = j + 1;
  }

  /* have finished get number of output samples and return */
  
  return (j);
}

int intz2float(flag)
int flag;
{
_intz2float=flag;
return 0;
}

int float2intz(flag)
int flag;
{
_float2intz=flag;
return 0;
}
