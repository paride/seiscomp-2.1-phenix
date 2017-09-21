/*=========================================================================
        proto_seed.h
 *========================================================================*/

#ifdef ANSI_C

/* stahead.c */
void MakeStationHeaders  (struct station*);
void blk_030             (void);
void blk_033             (void);
void blk_034             (void);
void blk_031_S           (void);
void blk_031_C           (void);
void add_blockette       (char*, char);

/* loadDBsis.c */
int  LoadDBsisResponses   (char**, int, char*);

/* loadresp.c */
int  LoadStationResponses (char*, struct station*);
void print_channel        (FILE*, struct station*, int);
void calc_resp            (struct channel*, double, double*, char*);
void print_filter         (FILE*, struct filter*, int);

/* libseed.c */
int  seed_srate          (double, struct seed_data_hdr*);
int  calc_channel_gain   (struct station*);
int  calc_channel_sint   (struct station*);
void check_sensor_orient (struct station*);
int  check_sym           (struct filter*);
void analog_trans        (double*,int,double*,int,double,double,double*,int);
void calc_filter_A0      (struct filter*);
char *dbdecode           (int);
int  dbencode            (char*);
void dbt_to_asc          (double, char*);
void asc_to_dbt          (char*, struct seed_time*, double*);
void dbt_to_tstruct      (double, struct seed_time*);
void tstruct_to_dbt      (struct seed_time*, double*);
void time_asc            (struct seed_time*, char*);

/* seed_chan.c */
void seed_channel_map    (char*, char, char*, double, double, char*);

/* steim.c */
long Steim_comp          (FILE*, float*, char*, int*, struct data_segment*);
void blk_1000_1001       (char*, int);

/* libutil.c */
int  read_stadir         (char*, char*);
char *mem                (int);
int  mem_bytes           (void);

#else

/* stahead.c */
void MakeStationHeaders  ();
void blk_030             ();
void blk_033             ();
void blk_034             ();
void blk_031_S           ();
void blk_031_C           ();
void add_blockette       ();

/* loadDBsis.c */
int  LoadDBsisResponses   ();

/* loadresp.c */
int  LoadStationResponses ();
void print_channel        ();
void calc_resp            ();
void print_filter         ();

/* libseed.c */
int  seed_srate          ();
int  calc_channel_gain   ();
int  calc_channel_sint   ();
void check_sensor_orient ();
int  check_sym           ();
void analog_trans        ();
void calc_filter_A0      ();
char *dbdecode           ();
int  dbencode            ();
void dbt_to_asc          ();
void asc_to_dbt          ();
void dbt_to_tstruct      ();
void tstruct_to_dbt      ();
void time_asc            ();

/* seed_chan.c */
void seed_channel_map    ();

/* steim.c */
long Steim_comp          ();
void blk_1000_1001       ();

/* libutil.c */
int  read_stadir         ();
char *mem                ();
int  mem_bytes           ();

#endif

