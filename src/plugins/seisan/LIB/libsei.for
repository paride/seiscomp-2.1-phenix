CSTART***************************************************************************
C                                                                              *
C             List of modifications in this particular release                 *
C             ================================================                 *
C                                                                              *
C             Please remove the following lines when installed...              *C                                                                              *
CJAB(BGS)Jul27/1995 ...add wavefile handler routines...(in file LIBLOAD.FOR)   *
CJAB(BGS)Jul27/1995 ...remove th need for libsei1, by incorporating message    *
C                      handling on PC and UNIX in both graphics states in this *
C                      same routine                                            *
CJAB(BGS)Jul27/1995 ...Place all diagnostics flagged by "B_F_DEBUG$" to        *
C                      DBGUNIT$ rather than standard output                    *
CJAB(BGS)Sep 5/1995 ...Allocate debug unit to DBG_OUT$ in libsei.inc, this = 4 *
c
C oct  20 95 jh        Save record length when opening direct access files
c                      remove form= when opening direct acces files
c jul 22 98  mv jh     added routine seilcs to conver to lower case
c aug  16 by jh    :   -------- version 7.0 check------------------------------ 
c                       century, changed wav
c                      directory so it must have century, however file names
c                      can still be with only year until 2019. Not checked !!
c                      apparently no change needed for 5 letter wav data base
c                      names
C  sep 98    linux :   ok                                                      *
c  nov 25   jh     :   change search in wav data bases
c  feb 24   lo     :   bug in get_full_wav
c  mar 30 99 bmt   :   add pc_to_text 
c  mar 31 99 lo    :   add print_ver
c  jun 16 99 jh    :   change .cmd to .inp
c  sep 5 99  jh    :   change get_full_wav_name: was using wrong month when
c                      files had format 1999-11 ... Thanks griselda
c  sep 23          :   initialize geo_depths and herkij_distance
c  sep 24    lo    :   read epimap projection as real
c  feb 23    lo    :   read keep_auto_reg
c  feb 28    jh    :   add read of copy_wav_dir
c  mar 10    jh    :   add read of text_print
c  may 10          :   initialize merge_wav
c  oct 23    jh    :   add routine remove_letters, add read of high_accuracy
c  nov 7     lo    :   add cont parameters
c  mar 7     lo    :   bug fix in r_index, return 0 if char not found
c  april 4   lo    :   change in get_full_wav if file does not start with number
c  oct 9     jh    :   put cont variables to get_seisan_def
c  nov 1     lo    :   add cursortype to get_seisan_def
c  apr 10    lo    :   add myxfont to get_seisan_def, increased data to 120
c  may       jh    :   add AUTO_PROCESS
c  july      lo    :   also read continuous bases into wav_base
c
CEND****************************************************************************
C
CSTART**************************************************************************
C                                                                              *
C                             BGS/GSRG Computer Unit                           *
C                             ======================                           *
C                                                                              *
C      System  : SEISAN                                                        *
C      Library : LIBSEI.FOR                                                    *
C                                                                              *
C      Note 1) : In this library, all variables are explicitly defined,        *
C                including those found in the "INCLUDE" files (see note 2).    *
C                Thus their type is not limited to initial letter, except for  *
C                the following:                                                *
C                                                                              *
C              - character strings are prefixed with "CHR" and are defined in  *
C                the form CHARACTER  CHR_STRING *("value") where "value" is an *
C                integer. This is consistant where "value" is a parameter.     *
C                                                                              *
C              - logicals (booleans) are prefixed by "B".                      *
C                                                                              *
C                All external routines and function type are defined at the    *
C                start of the sub-program unit.                                *
C                                                                              *
C                Most sub-programs have one entry and one return point (the    *
C                exception is where an abort is made via "STOP"), usually      *
C                labelled "9999", and possibly an error section placed just    *
C                before the return point, with labels "9000" and greater.      *
C                                                                              *
C                BLOCK data sub-programs are to be found at the end of the     *
C                library, prefixed by "BD_".                                   *
C                                                                              *
C           2) : All variables with suffix "$" are those which are found in    *
C                their respective "INCLUDE" files and, due to this naming      *
C                convention, can be used throughout the user's program         *
C                without fear of conflict, as long as the user does not adopt  *
C                this same convention for his/her usual variable names.        *
C                However, the relevant "INCLUDE" file must appear in the       *
C                calling program or sub-program. All definitions and COMMON    *
C                variables are linked in this way.                             *
C                                                                              *
C           3) : When in PC graphics mode, text-writing to standard output     *
C                hangs the screen. If this occurs then the only course of      *
C                action is to re-boot the PC.                                  *
C                                                                              *
C                Messages should only be written when the common boolean       *
C                variable B_F_MESSAGE$ is set TRUE.                            *
C                                                                              *
C                By default, this is always initialised TRUE by block data.    *
C                However, it should only be modified by graphics initialisation*
C                and state changing which are found in the plotting library    *
C                "seiplot.for". Thus when graphics are not encountered, this is*
C                not affected. Otherwise, the boolean is set by these routines *
C                either explicitly, or by:                                     *
C                                                                              *
C                          B_F_MESSAGE$ = .not.( B_F_PC$ .and. B_F_GRAPH$ )    *
C                                                                              *
C                See the block data and the include file "libsei.inc" for      *
C                definitions and meanings.                                     *
C                                                                              *
C                With PC fatal errors, the graphics state is changed to alpha  *
C                before writing the error message, using routine "PC_TO_TEXT", *
C                which when active also sets this boolean TRUE. All other      *
C                messages are suppressed if the boolean is set FALSE.          *
C                                                                              *
C                On the UNIX platform, this is a dummy routine linked via      *
C                "comp_sun.o".                                                 *
C                On the PC, when no graphics are encountered, this is a dummy  *
C                routine linked via "pcdumt.obj".                              *
C                On the PC, when graphics are encountered, this is an active   *
C                routine which calls "CLEAR_TO_ALPHA", linked via "pctext.obj".*
C                                                                              *
C           4) : When the common variable "B_F_DEBUG$" is set TRUE, all        *
C                diagnostic messages are written to the debug file which is    *
C                given by "CHR_DBG_FILE$", defaulted to "libsei_debug$", and   *
C                is opened on unit "DBGUNIT$ ".                                *
C                                                                              *
C      WARN:     If this boolean (logical) variable is used in the user source *
C                code as a flag to write diagnostic messages to standard output*
C                and the program is in the PC graphics state then see note 3.  *
C                                                                              *
C      Author  : J. A. Bolton                                                  *
C      Date    : July 1995                                                     *
C      Version : V01                                                           *
C                                                                              *
CEND****************************************************************************
C
C      EXTERNAL   SEI OPEN        ! Open file handler.
C     &          ,SEI CLEN        ! Find length of a character string.
C     &          ,SEI CLOSE       ! Close files opened by SEI OPEN.
C     &          ,SEI CODE        ! Error condition handler.
C     &          ,SEI GETU        ! Get next available fortran unit (SEI OPEN).
C     &          ,SEI UPC         ! Convert a character string to uppercase.
C     &          ,SEI GET FILE    ! To open file in current/ standard directory.
C     &          ,SEI LEFT        ! To left justify a character string.
C     &          ,SEI CMD PRO     ! To process standard I/O or command stream.
C     &          ,SEI CMD SET     ! Open up command stream I/O files.
C     &          ,SEI GET VALUES  ! Parse a text string for number items.
C     &          ,SEI REAL NUM    ! Return a real number from a text item.
C     &          ,SEI INTEGER     ! Return an integer from a text item.
C     &          ,BD_ERROR        ! Block data common for error handling.
C     &          ,BD_FILEIO       ! Block data common for file handling.
C      INTEGER    SEI CLEN        ! Function.
C     &          ,SEI INTEGER     ! Ditto.
C      REAL       SEI REAL NUM    ! Et ditto.
C
C     Note the following routines in the extension library, LIBLOAD.FOR:
C
C      EXTERNAL   SEI LOAD        ! Load waveform file data
C     &          ,SEI BASELINE    ! Remove DC component from wave on baseline.
C     &          ,SEI MEAN AMP    ! Mean amplitude algorithm.
C      REAL       SEI BASELINE    ! Remove DC component from wave on baseline.
C     &          ,SEI MEAN AMP    ! Mean amplitude algorithm.
C
C************************* End of list ***********************************
C
C
      INTEGER FUNCTION SEI CLEN(CHR_STRING)
CSTART*************************************************************************
C                                                                             *
C    System           : SEI  Application Library                              *
C    Supplier         : BGS/SEI  Systems Programming Unit                     *
C    Module Name      : SEI CLEN                                              *
C    Purpose          : Return the actual length of a character string        *
C                       ignoring trailing spaces and non-printables.          *
C    Arguments -Input : CHR_STRING (C*) Character string                      *
C    Author           : J. A. Bolton                                          *
C    Date             : 23 November 1994                                      *
C    Version          : V01                                                   *
C                                                                             *
CEND***************************************************************************
C
      INTRINSIC    ICHAR, LEN
C
C    Argument
C    ========
C
      CHARACTER CHR_STRING*(*)               ! String to find length of.
C
C    Local variables...
C    ==================
C
      INTEGER        IX, IY,                 ! Very local.
     &               BLANK,                  ! Values of characters.
     &               A,                      ! Lowercase "a".
     &               Z,                      ! Ditto "z".
     &               LENGTH                  ! Dimension of string.
C
C    Initialise...
C    =============
C    The following are the delimiter characters to non-printing characters...
C    ------------------------------------------------------------------------
C
      BLANK = ICHAR(' ')                     ! Space.
      Z     = ICHAR('z')                     ! Small z.
      A     = ICHAR('a')                     ! Small a.
C
C    Empty string...
C    ===============
C
      IF( CHR_STRING .EQ. ' ' ) THEN           ! Blank string.
      SEI CLEN = 0                             ! Zero length.
C
C    Work backwards from end of string....
C    =====================================
C    Initialise....
C    --------------
C
      ELSE                                       ! Otherwise.
      LENGTH = LEN( CHR_STRING )                 ! Maximum length of string. 
      DO 1 IX = LENGTH, 0, -1                    ! Loop backwards.
      IF(IX .GT. 0) IY = ICHAR(CHR_STRING(IX:IX))! Get character representation.
C
C    Empty...
C
         IF( IX .EQ. 0 ) THEN                   ! Empty string.
         SEI CLEN = 0                           ! & set value.
C
C    Not empty..
C
         ELSE IF( IY .GT. BLANK   .AND.         ! Skip loop.
     &            IY .LE. Z       .AND.         ! Found.
     &            IY .NE. A-1   ) THEN          ! 
         SEI CLEN = IX                          ! Set value.
         GOTO 9999                              ! & return.
         END IF                                 !
1     CONTINUE                                  !
      END IF                                    !
C
C    Return to Caller...
C    ===================
C
9999  RETURN
      END
C
      SUBROUTINE SEI CLOSE( FUNC, UNIT, CODE )
C
CSTART**************************************************************************
C                                                                              *
C   System            : SEI  Validation & Application Library                  *
C   Supplier          : BGS/GSRG  Applications Programming Unit                *
C   Procedure Name    : SEI CLOSE                                              *
C   Purpose           : To close the file on unit and delete depending on:     *
C                       FUNC     = CLOSE$  Close file on unit.                 *
C                       FUNC     = DELETE$ Close file on unit & delete         *
C                       Add        ALL$    to process each file open           *
C                                                                              *
C                       The routine will always HALT with a message on any     *
C                       error condition, unless the following                  *
C                                                                              *
C                       add        WARN$   to exit to caller on non-fatal error*
C                                          with a message.                     *
C                                                                              *
C   Arguments  -Input : FUNC       (I*4) The operations vector.                *
C                     : UNIT       (I*4) Fortran unit number                   *
C              -Output: CODE       (I*4) Returned condition                    *
C                     : UNIT       (I*4) Fortran unit number= 0 for single file*
C                                                                              *
C   Author            : J. A. Bolton                                           *
C   Date              : 23 November 1994                                       *
C   Version           : V01                                                    *
C                                                                              *
CEND***************************************************************************
C
      EXTERNAL     SEI CLEN       ! String length function
     &            ,PC_TO_TEXT     ! Close down graphics.
      INTEGER      SEI CLEN       ! & function.
C
C    System definitions....
C    ======================
C
      INCLUDE  'libsei.inc'      ! Seisan library definitions & data definitions
C
C    Argument declarations....
C    =========================
C
      INTEGER*4         FUNC,               ! Operational functionality.
     &                  CODE,               ! Returned condition.
     &                  UNIT                ! File unit.
C
C    Local variables...
C    ==================
C
      INTEGER*4         F_UNIT,             ! Local file unit.
     &                  FUNCT,              ! Working function.
     &                  FUNCTION,           ! Local function.
     &                  IX,     IZ          ! Very local.
      LOGICAL*4         B_OPEN,             ! File open?
     &                  B_WARN,             ! Warn, not halt, on error?.
     &                  B_ALL               ! Operate on all files open.
C
C     1. Initialise....
C     =================
C
      CODE = E_OK$                                ! Set returned condition.
      FUNCT    = FUNC                             ! Set working function.
      FUNCTION = MOD(FUNCT,WARN$)                 ! Local function.
      FUNCT    = FUNCT - FUNCTION                 ! Major function.
      B_WARN   = FUNCT .EQ. WARN$                 ! Warn caller?.
C
      IF( FUNCT .EQ. IGNORE$ ) GOTO 9300          ! Invalid function.
      FUNCT    = FUNCTION                         ! Working function
      FUNCTION = MOD(FUNCT,OLD$)                  ! Local function.
      FUNCT    = FUNCT - FUNCTION                 ! Major function.
      B_ALL    = FUNCT .EQ. ALL$                  ! All files?.
C
C     2.  Loop all open files...
C     ==========================
C
      IF( B_ALL ) THEN                      !  Operate on all open units.
C
C    2.1. Check the file unit is actually open...
C    --------------------------------------------
C
2000  IZ = INDEX( CHR_MAP$, 'O' )                         ! Location in map.
      IX = F_MAXU$ - IZ                                   ! Next open unit.
C
         IF( IX .LT. F_MAXU$ ) THEN                       ! Unit flagged open.
         INQUIRE( UNIT= IX, OPENED=B_OPEN, IOSTAT= CODE ) ! Unit open?.
         IF( CODE .NE. E_OK$ ) GOTO 9000                  ! On error.
C
C    2.2. Close & delete if required....
C    -----------------------------------
C
            IF( B_OPEN ) THEN                            !
               if( b_f_debug$ ) then                     ! Debug information.
               write(dbgunit$,*)'...closing: ',          ! Message.
     &         chr_file_open$(ix)(:seiclen(chr_file_open$(ix)))
               write(dbgunit$,*)'   on unit: ', ix       !
               end if                                    !
C
               IF( FUNCTION .EQ. CLOSE$ ) THEN           ! Close only.
               CLOSE( IX, STATUS='KEEP', IOSTAT=CODE )   !
               IF( CODE .NE. E_OK$ ) GOTO 9100           ! On error.
C
               ELSE IF( FUNCTION .EQ. DELETE$ ) THEN     ! Close & delete.
               CLOSE( IX, STATUS='DELETE', IOSTAT=CODE ) !
               IF( CODE .NE. E_OK$ ) GOTO 9200           ! On error.
C
               ELSE                                      ! Invalid option.
               GOTO 9300                                 ! Error.
               END IF                                    !
C
C    2.2.1 Remove details from map...
C
            CHR_MAP$(IZ:IZ) = ' '                 ! & remove from map.
            CHR_FILE_OPEN$(IX) = ' '              ! & list of open files.
            GOTO 2000                             ! Next unit.
C
C    2.3. Unit should be open... it is flagged so....
C    ------------------------------------------------
C
            ELSE                                       !
            GOTO 9400                                  ! Error.
            END IF                                     !
         END IF                                        !
C
C     3. Close and/or delete a single file unit supplied.....
C     =======================================================
C     3.1 Unit supplied is zero and invalid...
C     ----------------------------------------
C
      ELSE IF( UNIT .LE. 0 ) THEN                           ! Invalid.
      F_UNIT = UNIT                                         ! Copy input.
      GOTO 9600                                             ! & message.
C
C     3.2 Check that the file is open...
C     ----------------------------------
C
      ELSE                                                  ! Single file.
      F_UNIT = UNIT                                         ! Copy unit.
      IX     = F_UNIT                                       ! & save
      IZ     = F_MAXU$ - F_UNIT                             ! Map position.
      INQUIRE( UNIT= F_UNIT, OPENED= B_OPEN, IOSTAT= CODE ) ! Unit open?.
      IF( CODE .NE. E_OK$ ) GOTO 9000                       ! On error.
C
C     3.3.   Close & delete if required....
C     =====================================
C
         IF( B_OPEN .AND. CHR_MAP$(IZ:IZ) .EQ. 'O' ) THEN ! 
            if( b_f_debug$ ) then                         ! Debug information.
            write(dbgunit$,*)'...closing: ',              ! Message.
     &      chr_file_open$(ix)(:seiclen(chr_file_open$(ix)))
            write(dbgunit$,*)'   on unit: ', f_unit       !
            end if                                        !

            IF( FUNCTION .EQ. CLOSE$ ) THEN               ! Close only.
            CLOSE( F_UNIT, STATUS='KEEP', IOSTAT=CODE )   !
            IF( CODE .NE. E_OK$ ) GOTO 9100               ! On error.
C
            ELSE IF( FUNCTION .EQ. DELETE$ ) THEN         ! Close &delete.
            CLOSE( F_UNIT, STATUS='DELETE', IOSTAT=CODE ) !
            IF( CODE .NE. E_OK$ ) GOTO 9200               ! On error.
C
            ELSE                                          ! Invalid.
            GOTO 9300                                     ! Error.
            END IF                                        !
C
         UNIT = -1                                        ! Re-initialise.
         CHR_MAP$(IZ:IZ) = ' '                            ! & remove from map.
         CHR_FILE_OPEN$(IX) = ' '                         ! & open files list.
C
C    3.3.1. Open but not flagged.....
C    --------------------------------
C
         ELSE IF( B_OPEN ) THEN                           !
         GOTO 9500                                        ! Force error.
C
C    3.3.2. Flagged but unit not open...
C    -----------------------------------
C
         ELSE IF( CHR_MAP$(IZ:IZ) .EQ. '0' ) THEN         !
         GOTO 9400                                        ! Force error.
C
C    3.3.3  Not open and not flagged....
C    -----------------------------------
C
         ELSE                                             ! Otherwise.
         GOTO 9600                                        ! Invalid.
         END IF                                           ! 
      END IF                                              !
      GOTO 9999                                           ! Successful.
C
C     9. Errors.....
C     ==============
C
9000  IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) ) ! Fatal; graphics on pc.
     &    CALL PC_TO_TEXT                           ! Then text mode.
C
      IF( B_F_MESSAGE$ ) THEN                       ! & message.
      WRITE(*,*) 
      WRITE(*,*)
     &'**** ERROR: problem checking file open             ****'
      WRITE(*,*)'            ',
     &CHR_FILE_OPEN$(IX)(:SEICLEN(CHR_FILE_OPEN$(IX)))
      WRITE(*,*)'     FORTRAN condition ', CODE           !
      WRITE(*,*)
     &'****                                               ****'
      END IF
      GOTO 9998                                           ! 
C
9100  IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) ) ! Fatal; graphics on pc.
     &    CALL PC_TO_TEXT                           ! Then text mode.
C
      IF( B_F_MESSAGE$ ) THEN                       ! & message.
      WRITE(*,*)
      WRITE(*,*)
     &'**** ERROR: problem closing file                   ****'
      WRITE(*,*)'            ',
     &CHR_FILE_OPEN$(IX)(:SEICLEN(CHR_FILE_OPEN$(IX)))
      WRITE(*,*)'     FORTRAN condition ', CODE           !
      WRITE(*,*)
     &'****                                               ****'
      END IF
      GOTO 9998                                           !
C
9200  IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) ) ! Fatal; graphics on pc.
     &    CALL PC_TO_TEXT                           ! Then text mode.
C
      IF( B_F_MESSAGE$ ) THEN                       ! & message.
      WRITE(*,*)
      WRITE(*,*)
     &'**** ERROR: problem deleting file                  ****'
      WRITE(*,*)'            ',
     &CHR_FILE_OPEN$(IX)(:SEICLEN(CHR_FILE_OPEN$(IX)))
      WRITE(*,*)'     FORTRAN condition ', CODE           !
      WRITE(*,*)
     &'****                                               ****'
      END IF
      GOTO 9998                                           !
C
9300  IF( (.NOT.B_F_MESSAGE$) )                     ! Fatal; graphics on pc.
     &    CALL PC_TO_TEXT                           ! Then text mode.
      WRITE(*,*)
      WRITE(*,*)
     &'**** ERROR: invalid argument FUNCTION              ****'
      WRITE(*,*)'            ',
     &CHR_FILE_OPEN$(IX)(:SEICLEN(CHR_FILE_OPEN$(IX)))
      WRITE(*,*)
     &'****                                               ****'
      B_WARN = .FALSE.
      GOTO 9998                                           !
C
9400  IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) ) ! Fatal; graphics on pc.
     &    CALL PC_TO_TEXT                           ! Then text mode.
C
      IF( B_F_MESSAGE$ ) THEN                       ! & message.
      WRITE(*,*)
      WRITE(*,*)
     &'**** ERROR: file opened by SEI OPEN is not open    ****'
      WRITE(*,*)'            ',
     &CHR_FILE_OPEN$(IX)(:SEICLEN(CHR_FILE_OPEN$(IX)))
      WRITE(*,*)
     &'****                                               ****'
      END IF
      GOTO 9998                                           !
C
9500  IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) ) ! Fatal; graphics on pc.
     &    CALL PC_TO_TEXT                           ! Then text mode.
C
      IF( B_F_MESSAGE$ ) THEN                       ! & message.
      WRITE(*,*)
      WRITE(*,*)
     &'**** ERROR: open file not opened by SEI OPEN       ****'
      WRITE(*,*)'             unit ', F_UNIT
      WRITE(*,*)
     &'****                                               ****'
      END IF
      GOTO 9998                                           !
C
9600  IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) ) ! Fatal; graphics on pc.
     &    CALL PC_TO_TEXT                           ! Then text mode.
C
      IF( B_F_MESSAGE$ ) THEN                       ! & message.
      WRITE(*,*)
      WRITE(*,*)
     &'**** ERROR: ambiguous unit supplied to SEI CLOSE   ****'
      WRITE(*,*)'             unit ', F_UNIT
      WRITE(*,*)
     &'****                                               ****'
      END IF
      GOTO 9998                                           !
C
C    10. ABORT point.....
C    ====================
C
9998  IF( B_WARN ) THEN                                   ! Warn, no action.
      CONTINUE                                            !
C
      ELSE                                                ! Otherwise.
      WRITE(*,*)
     &'****                                               ****'
      WRITE(*,*)
      WRITE(*,*)
     &'**** WARN: halting program in SEI CLOSE            ****'
      STOP                                       !
      END IF                                     !
C
C     Return to caller....
C     ====================
C
9999  RETURN
      END                                    
C
      SUBROUTINE SEI CMD PRO( CHR_PROMPT, CHR_TEXT )
C
CSTART*************************************************************************
C                                                                             *
C   System          : SEISAN                                                  *
C   Supplier        : BGS/GSRG Applications Programming Unit                  *
C   Procedure Name  : SEI CMD PRO                                             *
C   Purpose         : To process text retrieved either from standard input or *
C                     from a command file and write this to an output file    *
C                     if required.                                            *
C                     In particular, if both arguments are blank, then write  *
C                     the premature end of file message and stop.             *
C   Note            : No PC graphics treatment of messages to standard output *
C                     is required here!.                                      *
C                                                                             *
C   Arguments-Input : CHR_TEXT   (C*)  Input text string.                     *
C                   : CHR_PROMPT (C*)  Input prompt string. If blank, do not  *
C                                      prompt and retrieve data. Instead      *
C                                      process to the output file when necess-*
C                                      ary.                                   *
C           -Output : CHR_TEXT   (C*)  Retrieved text in reply to a prompt    *
C                                      when given.                            *
C                                                                             *
C   Author          : J. A. Bolton                                            *
C   Date            : 2 December 1994                                         *
C   Version         : V01                                                     *
C                                                                             *
CEND***************************************************************************
C
      EXTERNAL   SEI CLEN, SEI LEFT
      INTEGER*4  SEI CLEN
C
C    System inserts....
C    ==================
C
      INCLUDE  'libsei.inc'                ! Library definitions & data defns.
C
C    Arguments.....
C    ==============
C
      CHARACTER   CHR_PROMPT *(*),         ! Prompt string.
     &            CHR_TEXT   *(*)          ! Text to process.
C
C    Local variables...
C    ==================
C
      INTEGER*4   CODE                     ! Local condition.
      LOGICAL*4   B_FLAG,                  ! End of file?.
     &            B_PEOF                   ! Premature eof only.
C
C    Initialise...
C    =============
C
      CODE = E_OK$                         ! Everything's fine!!!.
      B_PEOF = CHR_PROMPT .EQ. ' '  .AND.  ! Force premature end of file!.
     &         CHR_TEXT   .EQ. ' '         !
C
C    =============================================
C    Prompt when required and process the reply...
C    =============================================
C
      IF( CHR_PROMPT .NE. ' ' ) THEN                        ! A prompt to make.
      WRITE(*,'(A,A,A,$)') ' ',CHR_PROMPT(:SEICLEN(CHR_PROMPT))
     &,' ' ! Prompt it.
      READ(CMDUNIT$,'(A)',IOSTAT=CODE) CHR_TEXT             ! Get a reply.
      CALL SEI LEFT( CHR_TEXT )                             ! Left justify
      CALL SEI CODE( FORT$,                                 ! Fortran condition.
     &               CODE,                                  ! From reply.
     &               CMDUNIT$,                              ! From command file.
     &               B_PEOF )                               ! End of file?.
C
C    =================================================
C    No prompt, read from input command file stream...
C    =================================================
C
      ELSE IF( CMDUNIT$ .NE. STD_IN$ ) THEN     ! Command file input.
      READ(CMDUNIT$,'(A)',IOSTAT=CODE) CHR_TEXT ! Get a record.
      CALL SEI LEFT( CHR_TEXT )                 ! Left justify
      CALL SEI CODE( FORT$,                     ! Fortran condition.
     &               CODE,                      ! From reply.
     &               CMDUNIT$,                  ! From command file.
     &               B_PEOF )                   ! End of file?.
C
C    ==========================================
C    Otherwise left justify the input string...
C    ==========================================
C
      ELSE                                              ! Otherwise.
      CALL SEI LEFT( CHR_TEXT )                         ! Left justify
      END IF                                            !
C
C    =============================================
C    Force premature end of file message & stop...
C    =============================================
C
1000  IF( B_PEOF ) THEN                                 ! Force stop.
      WRITE(*,*)
     &'****                                                ****'
      WRITE(*,*)                                        ! 1st line of
     &'**** ERROR: contents of command file, specified on  ****'
      WRITE(*,*)                                        ! Special message.
     &'****        the command line, are incomplete        ****'
      CHR_ERR_MSG$ =
     &'****                                                ****'
      CALL SEI CODE( STOP$,                             ! Force halt.
     &               E_PEOF$,                           ! On premature eof.
     &               0,                                 ! Unit (n/a).
     &               B_FLAG )                           ! Flag (n/a).
C
C    ========================================================
C    Write text to the output command stream file, or else if
C    unavailable, read from the input stream & echo to the user...
C    ========================================================
C
      ELSE IF( PARUNIT$ .NE. STD_OUT$ ) THEN ! Not standard output.
      WRITE(PARUNIT$,'(A)',IOSTAT=CODE)      ! Write to output file.
     &      CHR_TEXT(:SEICLEN(CHR_TEXT))     ! The input or retrieved text.
      CALL SEI CODE( FORT$,                  ! Fortran condition.m
     &               CODE,                   ! Returned from.
     &               PARUNIT$,               ! Output command file.
     &               B_FLAG )                ! Flag (n/a).
C
C    Echo after reading from command stream...
C    -----------------------------------------
C
      ELSE IF( CMDUNIT$ .NE. STD_IN$ ) THEN  ! Command file input.
      WRITE(*,*)CHR_TEXT(:SEICLEN(CHR_TEXT)) ! & echo to user.
      END IF                                 !
C
C    Return to Caller...
C    ===================
C
9999  RETURN
      END
C
      SUBROUTINE SEI CMD SET( CHR_PROG )
C
CSTART*************************************************************************
C                                                                             *
C   System          : SEISAN                                                  *
C   Supplier        : BGS/GSRG Applications Programming Unit                  *
C   Procedure Name  : SEI CMD SET                                             *
C   Purpose         : To determine and retrieve any argument from the command *
C                     line and dependent on this, open up either the command  *
C                     stream file for input or for output.                    *
C                                                                             *
C   Arguments-Input : CHR_PROG   (C*)  Name of the routine which is the basis *
C                                      of the command file name.              *
C           -Output : None                                                    *
C   Note            : CMDUNIT$ = STD_IN$ for standard input (*)               *
C                     PARUNIT$ = "unit"  for output             OR            *
C                     CMDUNIT$ = "unit"  for input                            *
C                     and strings echoed to STD_OUT$        (*)               *
C                                                                             *
C   Author          : J. A. Bolton                                            *
C   Date            : 2 December 1994                                         *
C   Version         : V01                                                     *
C                                                                             *
CEND***************************************************************************
C
      EXTERNAL    GET_ARGUMENTS        ! From command line.
      EXTERNAL    SEI CLEN             ! String length.
      INTEGER     SEI CLEN             ! Function.
C
C    System inserts....
C    ==================
C
      INCLUDE  'libsei.inc'      ! Seisan library definitions & data definitions
C
C    Arguments.....
C    ==============
C
      CHARACTER   CHR_PROG *(*)  ! program name.
C
C    Local variables...
C    ==================
C
      INTEGER     CODE,              ! Local condition.
     &            ARGS_N,            ! # arguments on command line.
     &            TEXT_C             ! Text string length.
      PARAMETER  (TEXT_C = 80)       ! & value.
      CHARACTER   CHR_TEXT *(TEXT_C) ! & text.
      LOGICAL     B_FLAG             ! Flag it?.
C
C    Initialise...
C    =============
C
      CALL GET_ARGUMENTS( ARGS_N, CHR_TEXT ) ! Get text of arguent.
C
C    Command file input...
C    =====================
C
      IF( ARGS_N .GT. 0 ) THEN       ! Argument given.
      CALL SEI OPEN( OLD$,           ! Open (default stop on error).
     &         ' ',                  ! No prompt.
     &         CHR_TEXT,             ! Filename.
     &         CMDUNIT$,             ! Opened command stream.
     &         B_FLAG,               ! Flag exists.
     &         CODE )                ! Condition (n/a).
C
C    Open the command file to output the commands from standard input...
C    ===================================================================
C
      ELSE                                       ! Otherwise standard input.
      CHR_TEXT = CHR_PROG(:SEICLEN(CHR_PROG)) // ! Output file name.
     &           '.inp'                          ! 
      CALL SEI OPEN( UNKNOWN$,                   ! Open (default stop on error).
     &               ' ',                        ! No prompt.
     &               CHR_TEXT,                   ! File.
     &               PARUNIT$,                   ! Output unit.
     &               B_FLAG,                     ! Flag exists.
     &               CODE )                      ! Condition (n/a).
      END IF                                     !
C
C    Return to Caller...
C    ===================
C
9999  RETURN
      END
C
      SUBROUTINE SEI CODE( FUNC, CODE, UNIT, B_FLAG )
C
CSTART*************************************************************************
C                                                                             *
C   System          : SEISAN  Application Library                             *
C   Supplier        : BGS/SEI  Applications Programming Unit                  *
C   Procedure Name  : SEI CODE                                                *
C   Purpose         : To encode and store error codes depending on whether the*
C                     input code is a SEISAN (local) or FORTRAN condition,    *
C                     dependant on the condition.                             *
C
C                     The routine will normally HALT the program on error, or *
C                     if WARN$ is included, will exit to the caller.          *
C                                                                             *
C                     The allocated message can be replaced by a customised   *
C                     message set up by the caller, in the string CHR_ERR_MSG$*
C                     but this will be re-initialised to blank on exit        *
C                                                                             *
C   Arguments - NOTE: All arguments are assumed to be 4 bytes.                *
C                     Whereas all SEISAN (local) conditions can be supplied by*
C                     a constant, FORTRAN conditions (when UNIT not zero) must*
C                     be supplied as a variable, since it is converted into a *
C                     local condition...we must not try to change a machine   *
C                     constant!!!                                             *
C                                                                             *
C            -Input : FUNC       (I*4) Functionality of routine:              *
C                                      = STOP$ (=0) HALT program on error     *
C                                      = WARN$      Return condition to caller*
C                                      + FORT$      To trap any fortran       *
C                                                   conditions where unit is  *
C                                                   not open                  *
C                   : CODE       (I*4) Input code (local or fortran)          *
C                   : UNIT       (I*4) Fortran unit or zero for local         *
C            -Output: CODE       (I*4) Output (local) code                    *
C                     B_FLAG     (L*4) Flag end of file condition or change in*
C                                      error condition.                       *
C                                                                             *
C   Author          : J. A. Bolton                                            *
C   Date            : 23 November 1994                                        *
C   Version         : V01                                                     *
C                                                                             *
CEND***************************************************************************
C
      EXTERNAL     SEI CLEN       ! String length function
     &            ,SEI CLOSE      ! File closure.
     &            ,PC_TO_TEXT     ! Close down graphics.
      INTEGER      SEI CLEN       ! & function.
C
C    System inserts....
C    ==================
C
      INCLUDE  'libsei.inc'      ! Seisan library definitions & data definitions
C
C    Arguments.....
C    ==============
C
      INTEGER*4    FUNC,                    ! Functionality of routine.
     &             UNIT,                    ! Fortran unit or zero.
     &             CODE                     ! Input/output condition.
      LOGICAL*4    B_FLAG                   ! Flag end of file condition.
C
C    Local variables....
C    ===================
C
      INTEGER*4    IX,                      ! Very local.
     &             KODE,                    ! Alternative condition.
     &             FUNCTION                 ! Working functionality.
      LOGICAL*4    B_OK,                    ! Everything ok?.
     &             B_FORTRAN,               ! Fortran condition?.
     &             B_WARN                   ! Warn caller, not halt?.
C
C    Preliminaries...
C    ================
C
      IX        = MOD( FUNC, WARN$ )        ! Fortran forcing function.
      FUNCTION  = FUNC - IX                 ! Working function.
C
      B_FLAG    = .FALSE.                   ! Re-initialise.
      B_OK      = CODE     .EQ. E_OK$       ! Re-initalise error vector?.
      B_WARN    = FUNCTION .EQ. WARN$       ! Warn user.?
      B_FORTRAN = UNIT     .GT. 0      .OR. ! Fortran file error.
     &            IX       .EQ. FORT$       ! Or forced fortran functionality.
C
C    Re-initialise error vector...
C    =============================
C
      IF( B_OK ) THEN                       ! No error.                  (L11).
      ERROR$(1) = E_OK$                     ! Local error.
      ERROR$(2) = E_OK$                     ! System error.
      GOTO 9999                             ! Return to caller.
C
C    Process FORTRAN I/O condition...
C    ================================
C
      ELSE IF( B_FORTRAN ) THEN             ! Fortran i/o
C
C    Unit not open...
C    ----------------
C
         IF( UNIT .EQ. -1 ) THEN            ! Unit is not open.
         ERROR$(1) = E_RDWR$                ! Read-write-file error.
         ERROR$(2) = CODE                   ! & system code.
         CODE      = ERROR$(1)              ! Set local condition.
C
         IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) ) ! Fatal; graphics on pc.
     &       CALL PC_TO_TEXT                           ! Then text mode.
C
            IF( B_F_MESSAGE$ ) THEN                    ! & message.
            WRITE(*,*)
     &'****                                                ****'
            WRITE(*,*)
     &'           FORTRAN condition : ', ERROR$(2)
            END IF   
C
         CHR_ERR_MSG$ =                     ! Set message, no over-ride.
     &'           ...file unit not open'
         GOTO 9000                              ! & message.
C
C    End of file....
C    ---------------
C
         ELSE IF( CODE .LT. E_OK$ ) THEN    ! End of file.
         ERROR$(1) = E_OK$                  ! Re-set error.
         ERROR$(2) = E_OK$                  ! Ditto.
         CODE      = ERROR$(1)              ! Set local condition.
         B_FLAG    = .TRUE.                 ! Flag end of file.
         GOTO 9999                          ! Return to caller.
C
C    System errors....
C    -----------------
C
         ELSE                                   ! File function error.
         ERROR$(1) = E_RDWR$                    ! Read-write-file error.
         ERROR$(2) = CODE                       ! & system code.
         CODE      = ERROR$(1)                  ! Set local condition.
C
         IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) ) ! Fatal; graphics on pc.
     &       CALL PC_TO_TEXT                           ! Then text mode.
C
            IF( B_F_MESSAGE$ ) THEN                    ! & message.
            WRITE(*,*)
     &'****                                                ****'
            WRITE(*,*)
     &'           FORTRAN condition : ', ERROR$(2)
            WRITE(*,*)
     &'           FORTRAN I/O unit  : ', UNIT
            END IF
C
            IF( CHR_ERR_MSG$ .EQ. ' ' ) THEN    ! Can allocate message.
            CHR_ERR_MSG$ = CHR_ERR_TXT$(E_RDWR$)! Set the error message.
            END IF                              !
         GOTO 9000                              ! & message.
         END IF                                 ! 
C
C    Process local condition...
C    ==========================
C
      ELSE                                  ! Local code.
      B_FLAG    = ERROR$(1) .NE. CODE .AND. ! Changed the error?.
     &            ERROR$(1) .NE. E_OK$      !
      ERROR$(1) = CODE                      ! & store.
      ERROR$(2) = E_OK$                     ! Not a fortran condition.
C
         IF( CHR_ERR_MSG$ .EQ. ' ' ) THEN   ! Can allocate a message.
         CHR_ERR_MSG$ = CHR_ERR_TXT$(CODE)  ! Set the error message.
         END IF                             !
C
C    Unrecoverable errors...
C
      DO 1000 IX = 1, E_FAIL_MX$                     ! Loop unrecoverables.
         IF( CODE .EQ. E_FAIL$(IX) )THEN             ! Unrecoverable.   
         B_WARN = .FALSE.                            ! Must halt.
         IF( .NOT.B_F_MESSAGE$ ) CALL PC_TO_TEXT     ! Fatal; graphics on pc.
         GOTO 9000                                   ! & message.
         END IF                                      !       
1000  CONTINUE                                       !
      END IF                                         !   
C
C    Errors....
C    ==========
C
9000  IF( B_F_MESSAGE$ ) THEN
      WRITE(*,*)
     &'****                                                ****'
      WRITE(*,*)
     &    CHR_ERR_MSG$(:SEICLEN(CHR_ERR_MSG$))
      END IF
C
C    Abort the program?...
C    =====================
C
      IF( B_WARN ) THEN                          ! Warning only.
      CONTINUE                                   ! No further action.
C
      ELSE                                       ! Otherwise abort.
      IF( .NOT.B_F_MESSAGE$ ) CALL PC_TO_TEXT    ! Not necessary!!
      WRITE(*,*)
     &'****                                                ****'
      WRITE(*,*)
     &'**** WARN: halting program in SEI CODE              ****'
C
      WRITE(*,*)
     &'****       ...closing down files                    ****'
C
      CALL SEI CLOSE(CLOSE$+ALL$,                ! Close all files.
     &               0,                          ! File unit (n/a).
     &               KODE )                      ! Returned condition (n/a).
C
      STOP                                       !
      END IF                                     !
C
C    Return to caller...
C    ===================
C
9999  CHR_ERR_MSG$ = ' '                         ! Empty the message.
      RETURN                                    
      END                                    
C
      SUBROUTINE SEI GET FILE( FUNCTION,
     &                         UNIT, CODE,
     &                         CHR_UFD_PATH, CHR_FILE)
CSTART**************************************************************************
C                                                                              *
C   System            : SEI  Validation & Application Library                  *
C   Supplier          : BGS/GSRG  Applications Programming Unit                *
C   Procedure Name    : SEI GET FILE                                           *
C   Purpose           : To find &/or open a particular file in the current     *
C                       directory or if not found, in the directory            *
C                           $SEISAN_TOP/"CHR_UFD_PATH"                         *
C                       depending on function                                  *
C                       If the path is the Wave directory "WAV", then finally  *
C                       search the year/month sub-directory specified in the   *
C                       filename pre-fix text.                                 *
C                                                                              *
C   Arguments  -Input : FUNCTION     (I*) Function to perform. Values are:     *
C                                         OPEN$ - find and open file           *
C                                         CHECK$- find file & return pathname  *
C                                         To cancel messages, add IGNORE$      *
C                     : CHR_FILE     (C*)  filename                            *
C                     : CHR_UFD_NAME (C*) Particular standard directory path   *
C                                         eg. DAT, WAV, REA/BER/91/10          *
C              -Output: UNIT         (I)   Fortran unit number                 *
C                     : CODE         (I)   Returned condition                  *
C                                                                              *
C   Note              : If just checking for existance, or ignoring messages   *
C                       and file not found, then return condition and re-set   *
C                       the "file-open-set-up-detail" to nullify the default   *
C                       re-set action taken by "sei open".                     *
C                                                                              *
C   Author            : J. A. Bolton                                           *
C   Date              : 23 November 1994                                       *
C   Version           : V01                                                    *
C                                                                              *
CEND***************************************************************************
C
      EXTERNAL     SEI CLEN      ! String length function
      INTEGER      SEI CLEN      ! & function.
      EXTERNAL     SEI OPEN,     ! Open file handler.
     &             SEI CODE      ! & error handler.
C
      EXTERNAL     TOP DIR       ! Root pathname from environment.
     &            ,DIR_CHAR      ! Directory path delimiter.
     &            ,GET_ENV_BASE  ! Get environment database.
     &            ,GET_DEF_BASE  ! Get default database.
C
C    System definitions....
C    ======================
C
      INCLUDE  'libsei.inc'      ! Seisan library definitions & data definitions
      include  'seisan.inc'      ! general seisan
C
C    Argument declarations....
C    =========================
C
      INTEGER    CODE,               ! Returned condition.
     &           FUNCTION,           ! Function to perform.
     &           UNIT                ! File unit.
      CHARACTER  CHR_FILE     *(*),  ! Filename.
     &           CHR_UFD_PATH *(*)   ! User file directory path under seisan_top
C
C    Local variables...
C    ==================
C
      INTEGER    TEXT_C,                  ! Text string length.
     &           FUNC                     ! Loacl function.
      PARAMETER (TEXT_C = 80)             ! & value.
      CHARACTER  CHR_PATH_ROOT *(F_LEN$), ! Pathname root.
     &           CHR_TEXT      *(TEXT_C), ! Text string.
     &           CHR_DELIM     *(1)       ! Path delimiter. 
      LOGICAL    B_OLD,                   ! File exists?.
     &           B_MESSAGE,               ! Issue messages?.
     &           B_CHECK,                 ! Check where file exists?.
     &           B_WAVE,                  ! Wave directory?.
     &           B_FLAG                   ! General flagged situation?.
C
      CHARACTER  CHR_F_STAT    *(F_STATC$), ! Copies of file-open details.
     &           CHR_F_ACCESS  *(F_ACCSC$), ! Ditto.
     &           CHR_F_FORM    *(F_FORMC$), ! Ditto.
     &           CHR_F_ACTION  *(F_ACTNC$)  ! Et ditto.
                 integer         f_recl
      character*2 century
C
      CHARACTER  CHR_BASE      *(3)         ! Databasename.
C
C    Initialise...
C    =============
C
      CODE = E_OK$                          ! Local condition.
C
      FUNC = MOD(FUNCTION,CHECK$)           ! Subfunction.
      B_MESSAGE = FUNC .NE. IGNORE$         ! Issue messages?.
      FUNC = FUNCTION - FUNC                ! Major function.
      B_CHECK = FUNC .EQ. CHECK$            ! Check where it is?.
      B_WAVE  = CHR_UFD_PATH(:3) .EQ. 'WAV' ! Wave directory?.
C
      CALL TOP DIR(  CHR_PATH_ROOT )        ! Get root directory.
      CALL DIR_CHAR( CHR_DELIM )            ! Get delimiter.
C 
C     Copy the file-open-set-up-detail...
C     -----------------------------------
C     These are used by the OPEN statement...
C
      CHR_F_STAT   = CHR_F_STAT$      ! Copies of file-open details.
      CHR_F_ACCESS = CHR_F_ACCESS$    ! Ditto.
      CHR_F_FORM   = CHR_F_FORM$      ! Ditto.
      CHR_F_ACTION = CHR_F_ACTION$    ! Et ditto.
      f_recl=f_recl$
C
C    Search the current directory for the file...
C    ============================================
C
      CHR_TEXT = CHR_FILE                   ! Copy filename.
      CALL SEI OPEN( CHECK$,                ! Check pathname exists.
     &               ' ', CHR_TEXT,         ! For this pathname.
     &               0,                     ! Unit (n/a)
     &               B_OLD,                 ! File exists?.
     &               CODE )                 ! Returned condition (n/a).
C
C    Found file...
C    =============
C    Re-set file-open details unset by call to "sei open"...
C    -------------------------------------------------------
C

 1    B_FLAG = B_OLD                          ! File exists to open!.
      IF( B_FLAG .AND. B_CHECK ) THEN         ! Checks out ok!.
         CHR_FILE      = CHR_TEXT                ! Install for output.
         CHR_F_STAT$   = CHR_F_STAT              ! Copy back file-open details.
         CHR_F_ACCESS$ = CHR_F_ACCESS            ! Ditto.
         CHR_F_FORM$   = CHR_F_FORM              ! Ditto.
         CHR_F_ACTION$ = CHR_F_ACTION            ! Et ditto.
         f_recl$       = f_recl

C
C    Open the file...
C    ================
C
      ELSE IF( B_FLAG ) THEN                     ! Open file.
         IF( B_MESSAGE .AND. B_F_MESSAGE$ ) THEN ! Notify user.
            WRITE(*,*)                              !
            WRITE(*,*) CHR_TEXT(:SEICLEN(CHR_TEXT)) !
         END IF                                  !
C
         CHR_F_STAT$   = CHR_F_STAT              ! Copy back file-open details.
         CHR_F_ACCESS$ = CHR_F_ACCESS            ! Ditto.
         CHR_F_FORM$   = CHR_F_FORM              ! Ditto.
         CHR_F_ACTION$ = CHR_F_ACTION            ! Et ditto.
         f_recl$       = f_recl
C
         CALL SEI OPEN( OLD$,                 ! Open, stop on error.
     &               ' ',                     ! Prompt (n/a).
     &               CHR_TEXT,                ! Existing waveform file.
     &               UNIT,                    ! File unit.
     &               B_OLD,                   ! Exists!!?
     &               CODE )                   ! Condition (n/a).
C
C    Search the directory name supplied...
C    =====================================
C
      ELSE                                                 ! Otherwise.
         CHR_TEXT = CHR_PATH_ROOT(:SEICLEN(CHR_PATH_ROOT)) // ! Construct path.
     &              CHR_DELIM                              // !
     &              CHR_UFD_PATH(:SEICLEN(CHR_UFD_PATH))   // !
     &              CHR_DELIM                              // !
     &              CHR_FILE(:SEICLEN(CHR_FILE))              ! Filename.
C
 2       CALL SEI OPEN( CHECK$,               ! Check pathname exists.
     &               ' ', CHR_TEXT,           ! For this pathname.
     &               0,                       ! Unit (n/a)
     &               B_OLD,                   ! File exists?.
     &               CODE )                   ! Returned condition (n/a).
c
c   get here if file not found in loop, to terminate error messages
c
 3    continue

C
C    File exists in this directory...
C    --------------------------------
C
         IF( B_OLD ) THEN                     ! Exists.
            GOTO 1                               ! Then open it.
C
C    Check the wave directory...
C    ---------------------------
C
         ELSE IF( B_WAVE ) THEN                   ! Check sub-directory

            call get_full_wav_name(chr_file,chr_text)
            IF( chr_text(1:4).ne.'    ') then
               full_path_wav_name=chr_text
               b_wave=.false.
               b_old=.true.
              goto 1                                ! Exists, go open it
            endif

c
c   if here, no wav file found, go terminate error messages
c
          b_wave=.false.
          goto 3
C
C    Checking if the file does not exist...
C    --------------------------------------
C
         ELSE IF( B_CHECK           .OR.      ! Checking or no messages.
     &           (.NOT.B_MESSAGE) ) THEN      ! 
            CODE          = E_MISF$              ! Missing file.
            CHR_F_STAT$   = CHR_F_STAT           ! Copy back file-open details.
            CHR_F_ACCESS$ = CHR_F_ACCESS         ! Ditto.
            CHR_F_FORM$   = CHR_F_FORM           ! Ditto.
            CHR_F_ACTION$ = CHR_F_ACTION         ! Et ditto.
            f_recl$       = f_recl
C
C    File doesn't exist...
C    ---------------------
C
         ELSE                                 ! Otherwise.
            CHR_ERR_MSG$ = CHR_FILE(:SEICLEN(CHR_FILE)) //
     &                     ' does not exist'
            CALL SEI CODE( STOP$,                ! Halt the program with user-.
     &                     E_MISF$,              ! Message; dummy use of code.
     &                     0,                    ! Unit (n/a).
     &                     B_FLAG )              ! Flag (n/a).
         END IF                               !
      END IF                                  !
C
C    Return to Caller...
C    ===================
C
9999  RETURN
      END
C
      SUBROUTINE  SEI GETU( UNIT, CODE )
CSTART**************************************************************************
C                                                                              *
C   System            : SEI  Validation & Application Library                  *
C   Supplier          : BGS/SEI  Applications Programming Unit                 *
C   Procedure Name    : SEI GETU                                               *
C   Purpose           : To find the next available unit for opening a file on. *
C   Arguments  -Input : None                                                   *
C              -Output: UNIT       (I*4) Fortran unit opened on                *
C                     : CODE       (I*4) Returned condition                    *
C   Author            : J. A. Bolton                                           *
C   Date              : 22 November 1994                                       *
C   Version           : V01                                                    *
C                                                                              *
CEND****************************************************************************
C
      EXTERNAL     PC_TO_TEXT    ! Clear graphics.
C
C    System definitions....
C    ======================
C
      INCLUDE  'libsei.inc'      ! Seisan library definitions & data definitions
C
C    Argument declarations....
C    =========================
C
      INTEGER*4         UNIT,               !  File unit.
     &                  CODE                !  Returned condition.
C
C    Local variables......
C    =====================
C
      INTEGER*4         IX                  !  Very local.
      LOGICAL*4         B_OPEN              !  Unit open?.
C
C    Initialise....
C    ==============
C
      CODE = E_OK$                          !  Set returned condition.
      UNIT = 0                              !  & returned device #.
C
C    Find next available unit....
C    ============================
C
      IX = F_MAXU$ - INDEX( CHR_MAP$, ' ' ) !  Next non-protected, non-opened.
C
C    Check that unit found is not already opened...
C    ==============================================
C
      IF( IX .LT. F_MAXU$ ) THEN                        ! Found.
      INQUIRE( UNIT= IX, OPENED= B_OPEN, IOSTAT= CODE ) ! Unit open?.
         IF( CODE .NE. E_OK$ ) THEN                     ! Already open!.
         IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT          ! Fatal; pc graphics.
         WRITE(*,*)
         WRITE(*,*)
     &   '**** FATAL: problem finding next available unit    ****'
         WRITE(*,*)'     FORTRAN condition ', CODE      !
         WRITE(*,*)
     &   '****                                               ****'
         CODE = E_INIT$                                 ! Initialisation problem
C
C    Already open....
C    ----------------
C
         ELSE IF( B_OPEN ) THEN                           ! Open.
         CODE = E_INIT$                                   ! Already open!.
         IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT            ! Fatal; pc graphics.
         WRITE(*,*)
         WRITE(*,*)
     &   '**** FATAL: attempting to get a unit already open  ****'
         WRITE(*,*)
     &   '****                                               ****'
C
C    Unit available....
C    ------------------
C
         ELSE                                ! Otherwise.
         UNIT = IX                           ! Set up returned unit.
         END IF                              !
C
C    No more units available...
C    ==========================
C
      ELSE                                      ! Otherwise none found.
      CODE = E_INIT$                            ! Set condition.
      IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT     ! Fatal pc; graphics.
      WRITE(*,*)
      WRITE(*,*)
     &'**** FATAL: too many files currently open          ****'
      WRITE(*,*)
     &'****                                               ****'
      END IF                                !
C
C    Return to caller....
C    ====================
C
9999  RETURN                                !
      END                                   !
C
      SUBROUTINE SEI GET VALUES( NVAL, CHR_STRING, CODE )
CSTART*************************************************************************
C                                                                             *
C    System           : SEISAN Utilities                                      *
C    Supplier         : BGS/GSRG Programming Unit                             *
C    Routine          : SEI GET VALUES                                        *
C    Purpose          : To parse the input text string for numbers and return *
C                       these in the common decode real array ARRAY$. In      *
C                       addition, if any number element cannot be decoded,    *
C                       return a non-zero condition. Likewise for the count   *
C                       of decoded number elements not being NVAL. In all     *
C                       cases, issue an appropriate message                   *
C    Arguments -Input : CHR_STRING     (C**) character string to parse        *
C                     : NVAL           (I)   # items to be returned           *
C              -Output: CODE           (I)   Returned condition code          *
C                     : ARRAY$ contains the output number items in the order  *
C                              they are found.                                *
C    Author           : J A Bolton                                            *
C    Date             : 6 December 1994                                       *
C    Version          : V01                                                   *
C                                                                             *
CEND***************************************************************************
C
      EXTERNAL   SEI REAL NUM            ! Get real number.
     &          ,SEI CODE                ! Condition handler.
     &          ,SEI CLEN                ! Get string length.
     &          ,PC_TO_TEXT              ! Clear graphics mode.
      INTEGER    SEI CLEN                ! Function.
      REAL       SEI REAL NUM            ! Ditto.
C
C    System inserts...
C    =================
C
      include    'libsei.inc'            ! Library definitions & data defns.
C
C    Arguments...
C    ============
C
      INTEGER     NVAL,                  ! # items in list.
     &            CODE                   ! Returned condition.
      CHARACTER   CHR_STRING *(*)        ! Text to parse.
C
C    Local variables...
C    ==================
C
      INTEGER     IX, IY, IZ,            ! Very local.
     &            COUNT,                 ! List items count.
     &            TEXT_C                 ! Text string length.
      PARAMETER  (TEXT_C = 80)           ! Length of text.
      CHARACTER   CHR_TEXT *(TEXT_C),    ! & message text string.
     &            CHR_A    *(1)          ! Single character.
      LOGICAL     B_FLAG,                ! Flag variable?.
     &            B_LAST                 ! Last character flagged?.
C
C    Initialise...
C    =============
C
      CODE   = E_OK$                     ! Local condition.
      COUNT  = 0                         ! Item count.
      B_FLAG = .FALSE.                   ! Item not found yet.
C
C    Check output array is large enough...
C    -------------------------------------
C
      IF( NVAL .GT. ARRAY_N$ ) THEN        ! Fatal initialisation.
      IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT! In pc graphics.
      WRITE(*,*)
     &'**** In call to SEI GET VALUES:                     ****'
      CHR_ERR_MSG$ =                     ! Set up message.
     &'**** FATAL: no. of requested items exceeds ARRAY$   ****'
      CALL SEI CODE( STOP$,              ! Stop processing.
     &               E_INIT$,            ! Bad initialisation.
     &               0,                  ! Fortran unit (n/a).
     &               B_FLAG )            ! Flag (n/a).
C
C    =========================================================
C    Loop the characters & look for numbers...
C    Each number will start with any one of "+-0123456789"...
C    Once found, they may include exponent flags "E" or "D"...
C    Each field will be delimited by blank or non-alphanumeric
C    characters...
C    =========================================================
C
      ELSE                               ! Otherwise.
      IX = SEI CLEN( CHR_STRING )        ! Length of string.
      if(b_f_debug$)write(dbgunit$,*)'length..', ix
      DO 1000 IY = 1, IX                 ! Loop characters.
      CHR_A = CHR_STRING(IY:IY)          ! This character!.
      B_LAST = B_FLAG                    ! Last character flagged?.
      if(b_f_debug$)write(dbgunit$,*)'chra:', chr_a
C  
C    Check character is a possible item...
C    -------------------------------------
C
         IF( B_LAST ) THEN                   ! Already an item?.
         B_FLAG = INDEX('+-0123456789.EDed', ! Still flagged?.
     &                  CHR_A) .GT. 0        !
         ELSE                                ! Otherwise.
         B_FLAG = INDEX('+-0123456789.',     ! Is this an item?.
     &                  CHR_A) .GT. 0        !
         END IF                              !
         if(b_f_debug$)write(dbgunit$,*)'last?flag?:',b_last,b_flag
C
C    Item found...
C    -------------
C
         IF( B_FLAG .AND. (.NOT.B_LAST) ) THEN ! Yes.
         if(b_f_debug$)write(dbgunit$,*)'incrementing..',count+1
         COUNT = COUNT + 1                     ! Increment item count.
         IF( COUNT .GT. NVAL ) GOTO 2000       ! Skip & message.
         IZ    = IY                            ! Store start location.
C
C    Item closed...                           
C    --------------
C
         ELSE IF( B_LAST .AND. (.NOT.B_FLAG) ) THEN      ! Closure.
         if(b_f_debug$)write(dbgunit$,*)'columns..', iz, iy-1
         ARRAY$(COUNT) = SEIREALNUM(CHR_STRING(IZ:IY-1), !
     &                              CODE)                !
         IF( CODE .NE. E_OK$ ) GOTO 9000                 ! Return on error.
         END IF                                          !
1000  CONTINUE                                           !
C
C    Fall through loop...decode any remaining valid item...
C    ------------------------------------------------------
C
         if(b_f_debug$)write(dbgunit$,*)'fall through columns..',iz,ix
         IF( B_FLAG ) THEN                               ! Item flagged.
         ARRAY$(COUNT) = SEIREALNUM(CHR_STRING(IZ:IX),   !
     &                              CODE)                !
         IF( CODE .NE. E_OK$ ) GOTO 9000                 ! Return on error.
         END IF                                          !
C
C    Check number of items is the same as required...
C    ================================================
C
2000     IF( COUNT .LT. NVAL ) THEN                   ! Too few items.
         CODE = E_CONV$                               ! Flag condition.
         CHR_TEXT =                                   !
     &'**** WARN: $$$ item(s) are missing from the list.   ****'
         IX = INDEX( CHR_TEXT,'$$$' )                 ! Position indicator.
         WRITE(CHR_TEXT(IX:IX+2),'(I3)') (NVAL-COUNT) ! Indicate items missing.
            IF( B_F_MESSAGE$ ) THEN                   ! Write message.
            WRITE(*,*) CHR_TEXT(:SEICLEN(CHR_TEXT))   !
            END IF                                    !
C
         ELSE IF( COUNT .GT. NVAL ) THEN ! Too many items.
         CODE = E_CONV$                               ! Flag condition.
         CHR_TEXT =                                   !
     &'**** WARN: $$$ item(s) too many are in the list.    ****'
         IX = INDEX( CHR_TEXT,'$$$' )                 ! Position indicator.
         WRITE(CHR_TEXT(IX:IX+2),'(I3)') (COUNT-NVAL) ! Indicate items missing.
            IF( B_F_MESSAGE$ ) THEN                   ! Write message.
            WRITE(*,*) CHR_TEXT(:SEICLEN(CHR_TEXT))   !
            END IF                                    !
         END IF                                       !
      END IF                                          !
C
C    Errors...
C    =========
C
      GOTO 9999                                       ! Bypass if required.
9000  IF( B_F_MESSAGE$ ) THEN                         ! Write message.
      CHR_TEXT =                                      ! Otherwise setup message.
     &'**** WARN: item is in position $$$ in the string    ****'
      IX = INDEX( CHR_TEXT,'$$$' )                    ! Position indicator.
      WRITE(CHR_TEXT(IX:IX+2),'(I3)') COUNT           ! Indicate item.
      WRITE(*,*) CHR_TEXT(:SEICLEN(CHR_TEXT))         !
      END IF                                          !
C
C    Return to caller...
C    ===================
C
9999  if( b_f_debug$ ) then                                  ! Write out values.
      write(dbgunit$,*)'sei get values (',nval,' required):' !
      write(dbgunit$,*) array$                               !
      end if                                                 !
C
      RETURN
      END
C
      INTEGER FUNCTION SEI INTEGER( CHR_STRING, CODE )
CSTART*************************************************************************
C                                                                             *
C    System           : SEISAN Utilities                                      *
C    Supplier         : BGS/GSRG Programming Unit                             *
C    Routine          : SEI INTEGER                                           *
C    Purpose          : To decode an integer from a character string          *
C    Arguments -Input : CHR_STRING     (C**) Number character string          *
C              -Output: CODE           (I)   Returned condition code          *
C    Note             : Based on PGG INTEGER in libugeo.for (BGS/GSRG)        *
C    Author           : J A Bolton                                            *
C    Date             : 6 December 1994                                       *
C    Version          : V01                                                   *
C                                                                             *
CEND***************************************************************************
C
      EXTERNAL  SEI CLEN                    ! String length.
      INTEGER   SEI CLEN                    ! Function.
C
C    System definitions....
C    ======================
C
      INCLUDE  'libsei.inc'                 ! Library definitions & data defns.
C
C    Arguments.....
C    ==============
C
      INTEGER      CODE                     ! Returned condition.
      CHARACTER    CHR_STRING *(*)          ! Number string.
C
C    Local variables...
C    ==================
C
      INTEGER      IX,                      ! Very local.
     &             NUMBER,                  ! Decoded number.
     &             FORM_C                   ! Length of format string.
      PARAMETER   (FORM_C = 8)              ! & value.
      CHARACTER    CHR_FORMAT *(FORM_C)     ! Format string.
C
C    Initialise....
C    ==============
C
      CODE   = E_OK$                        ! Initialise return code.
      NUMBER = 0                            ! Value.
      IX     = SEICLEN( CHR_STRING )        ! Length.
C
C    Decode integer...
C    =================
C
      IF( IX .GT. 9 )     THEN                       ! Long.
      WRITE(CHR_FORMAT,'(''(I'',I2,'')'')')  IX      ! Encode format
      READ(CHR_STRING, CHR_FORMAT, ERR=9000 ) NUMBER ! Decode number.
C
      ELSE                                           ! Short.
      WRITE(CHR_FORMAT,'(''(I'',I1,'')'')')  IX      ! Encode format.
      READ(CHR_STRING, CHR_FORMAT, ERR=9000 ) NUMBER ! Decode number.
      END IF                                         !
C
C    Finished....
C    ------------
C
      GOTO 9999                          ! 
C
C    Errors.....
C    ===========
C
9000  CODE = E_CONV$                     ! Bad conversion.
      IF( B_F_MESSAGE$ ) THEN
      WRITE(*,*)                              !
      WRITE(*,*)                              !
     &'**** WARN: unable to decode integer in the string   ****'
      END IF
C
C    Return to caller...
C    ===================
C
9999  SEI INTEGER = NUMBER                     ! Assign.
      if( b_f_debug$ ) then                    ! Debug detail.
      write(dbgunit$,*)'sei integer: ',        !
     &      chr_string(:seiclen(chr_string)),  !
     &      ' ... ', number                    !
      end if
C
      RETURN         
      END
C
      SUBROUTINE SEI LEFT( CHR_TEXT )
C
CSTART*************************************************************************
C                                                                             *
C   System          : SEISAN                                                  *
C   Supplier        : BGS/GSRG Applications Programming Unit                  *
C   Procedure Name  : SEI LEFT                                                *
C   Purpose         : To left justify the text string (must be a variable)    *
C                                                                             *
C   Arguments-Input : CHR_TEXT   (C*)  Input text string.                     *
C           -Output : CHR_TEXT   (C*)  Left justified output string.          *
C                                                                             *
C   Author          : J. A. Bolton                                            *
C   Date            : 2 December 1994                                         *
C   Version         : V01                                                     *
C                                                                             *
CEND***************************************************************************
C
      EXTERNAL   SEI CLEN
      INTEGER*4  SEI CLEN
C
C    Arguments.....
C    ==============
C
      CHARACTER    CHR_TEXT *(*)            ! Input text string.
C
C    Local variables....
C    ===================
C
C    None
C
C    Process...
C    ==========
C
1000  IF( CHR_TEXT      .NE. ' '   .AND.    ! Non-empty string and.
     &    CHR_TEXT(1:1) .EQ. ' ' ) THEN     ! Leading blank found.
      CHR_TEXT = CHR_TEXT(2:)               ! Left justify.
      GOTO 1000                             ! Test again.
      END IF                                !
C
C    Return to Caller...
C    ===================
C
9999  RETURN
      END
C
      SUBROUTINE SEI OPEN( FUNC,     CHR_PROMPT,
     &                     CHR_FILE, UNIT, B_OLD,
     &                     CODE )
C
CSTART**************************************************************************
C                                                                              *
C   System            : SEI  Validation & Application Library                  *
C   Supplier          : BGS/GSRG Applications Programming Unit                 *
C   Procedure Name    : SEI OPEN                                               *
C   Purpose           : To open a file on the next available unit and STOP the *
C                       current program on error unless directed by function.  *
C                       If the program is stopped, all files will be closed.   *
C                       Alternatively, the routine may be used to check the    *
C                       existance of a file.                                   *
C                                                                              *
C                       In addition, if prompt text is given, then prompt user *
C                       for the file name which is returned. Otherwise use the *
C                       given filename. Not availble in PC graphics state.     *
C                                                                              *
C                       The routine will open the file on the next available   *
C                       unit and save its name for error processing.           *
C                                                                              *
C                       For other options, required for the open command, use  *
C                       COMMON block variables as seen in libsei.inc". eg.     *
C                                                                              *
C                         CHR_F_ACCESS$ = 'DIRECT', CHR_F_ACCESS$ = 'APPEND'   *
C                                                                              *
C   Arguments  -Input : FUNC       (I*4) The operations vector depending on    *
C                                        file option. Values are:              *
C                                                                              *
C                                        NEW$         - open new file (default)*
C                                        OLD$         - open existing file     *
C                                        UNKNOWN$     - open empty file        *
C                                        SCRATCH$     - open a scratch file    *
C                                        CHECK$       - check existance        *
C                                                                              *
C                                        add one of the following to disable   *
C                                        STOP on non-fatal errors:             *
C                                                                              *
C                                        WARN$        - exit routine with      *
C                                                       condition CODE set     *
C                                                       but issue message.     *
C                                        IGNORE$      - exit routine with      *
C                                                       with no message.       *
C                                                                              *
C                                        To check the existance of a directory *
C                                        specify:                              *
C                                                                              *
C                                        CHECK$+DIRECTORY$                     *
C                                                                              *
C                                        in this case, B_OLD will be returned  *
C                                        true or false and non-fatal errors    *
C                                        will be ignored. It is found by trying*
C                                        to open a file in the target directory*
C                                                                              *
C                                                                              *
C                                        In addition, if B_F_DEBUG$ is TRUE,   *
C                                        open up the debug file given in       *
C                                        CHR_DBG_FILE$, otherwise if blank     *
C                                        then open up "libsei_debug$"          *
C                                                                              *
C                     : CHR_PROMPT (C*)  Prompt text to standard output        *
C                     : CHR_FILE   (C*)  Pathname of file to open or directory.*
C              -Output: UNIT       (I*4) Fortran unit opened on                *
C                     : CHR_FILE   (C*)  Pathname of file opened or directory. *
C                     : B_OLD      (L*4) File already exists?.                 *
C                     : CODE       (I*4) Returned condition                    *
C                                                                              *
C   Author            : J. A. Bolton                                           *
C   Date              : 23 November 1994                                       *
C   Version           : V01                                                    *
C                                                                              *
CEND****************************************************************************
C
C    System definitions....
C    ======================
C
      INCLUDE  'libsei.inc'      ! Seisan library definitions & data definitions
C
C    Application references...
C    =========================
C
      EXTERNAL   DIR_CHAR        ! Get directory path delimiter.
      EXTERNAL   SEI UPC         ! String uppercase function.
     &          ,SEI CLOSE       ! File closure.
     &          ,SEI CLEN        ! & length function.
     &          ,PC_TO_TEXT      ! Cancel graphics.
      INTEGER    SEI CLEN        ! & define the function.
C
C    Argument declarations....
C    =========================
C
      INTEGER*4         FUNC,               ! Operational functionality.
     &                  UNIT,               ! File unit.
     &                  CODE                ! Returned condition.
      CHARACTER         CHR_PROMPT*(*),     ! User PROMPT.
     &                  CHR_FILE  *(*)      ! File to open.
      LOGICAL*4         B_OLD               ! File already exists?.
C
C     Local variables...
C     ==================
C
      INTEGER*4         TEXT_C,             ! Text string length.
     &                  FUNCT,              ! Working function.
     &                  FUNCTION,           ! Working function. 
     &                  COPY_UNIT,          ! Copy of current unit.
     &                  STATE,              ! State of open operation.
     &                  IX,   IY,   IZ      ! Very local.
      PARAMETER        (TEXT_C = 80)        ! & value.
      CHARACTER         CHR_TEXT *(TEXT_C), ! & string.
     &                  CHR_DELIM*(1)       ! & path delimiter.
C
      LOGICAL*4         B_PROMPT,           ! Prompt user for file name?.
     &                  B_OPEN,             ! File opened?.
     &                  B_WARN,             ! Warn the caller of an error?.
     &                  B_IGNORE,           ! Ignore non-fatal errors?.
     &                  B_CHECK,            ! Check file exists?.
     &                  B_DIRECTORY,        ! Check directory exists?.
     &                  B_EXIST,            ! File must exist?.
     &                  B_UNKNOWN,          ! File status is unknown.?
     &                  B_SCRATCH,          ! Scratch file?.
     &                  B_APPEND,           ! Append sam file?.
     &                  B_DAMF              ! Dam file?.
C
C     ===============
C     1.Initialise...
C     ===============
C     1.1. Preliminaries...
C     =====================
C
      CODE      = E_OK$                               ! Set return condition.
      FUNCT     = FUNC                                ! Working function
      FUNCTION  = MOD(FUNC,WARN$)                     ! Local function.
      FUNCT     = FUNC - FUNCTION                     ! Major function.
      COPY_UNIT = F_UNIT$                             ! Copy current unit.
      CALL SEI UPC( CHR_F_ACCESS$ )                   ! Ensure uppercase.
      CALL SEI UPC( CHR_F_FORM$ )                     ! Ditto.
C
      B_WARN   = FUNCT .EQ. WARN$                     ! Warn user?.
      B_CHECK  = FUNCT .EQ. CHECK$                    ! Check file exists?.
      B_IGNORE = FUNCT .EQ. IGNORE$                   ! Ignore errors?.
C
      FUNCT    = FUNCTION                             ! Working function.
      FUNCTION = MOD(FUNCT,OLD$)                      ! Local function.
      FUNCT    = FUNCT - FUNCTION                     ! Major function.
C
      B_EXIST     = FUNCT .EQ. OLD$                   ! File must exist?.
      B_UNKNOWN   = FUNCT .EQ. UNKNOWN$               ! File status unknown?.
      B_SCRATCH   = FUNCT .EQ. SCRATCH$               ! File to be scratch?.
      B_DIRECTORY = FUNCT .EQ. DIRECTORY$             ! Check directory exists?
C
      B_PROMPT = CHR_PROMPT .NE. ' '                  ! Prompt for filename?
      B_APPEND = INDEX(CHR_F_ACCESS$,'APPEND') .GT. 0 ! Append sam access?.
      B_DAMF   = INDEX(CHR_F_ACCESS$,'DIRECT') .GT. 0 ! Direct access?.
C
C     1.1.1 Initialise units...
C     -------------------------
C
      IF( CHR_MAP$ .EQ. ' ' ) THEN          ! Set-up.
      DO 1    IX = 1, F_MAX$                ! Loop protected units.
      IY = F_MAXU$ - F_PROTECT$(IX)         ! Get protected unit.
      CHR_MAP$(IY:IY) = 'P'                 ! & flag it.
1     CONTINUE                              !
      END IF                                !
C
C     1.1.2 Open up the debug file if required...
C     -------------------------------------------
C
      IF( B_F_DEBUG$          .AND.           ! Debugging required.
     &    DBGUNIT$   .EQ. 0 ) THEN            ! & unit not open.
C
         IF( CHR_DBG_FILE$ .EQ. ' ' ) THEN    ! Setup debug filename.
         CHR_DBG_FILE$ = 'libsei_debug$'      !
         END IF                               !
C
      IX = DBG_OUT$                           ! Set to debug unit.
         if( b_f_message$ ) then              ! Message allowed.
         write(*,*)'...opening debug file...'
         write(*,*)'   file   : ', 
     & chr_dbg_file$(:seiclen(chr_dbg_file$))
         write(*,*)'   on unit: ', ix
         end if                               !
C
      OPEN( UNIT=IX,                                     ! Open the file.
     &      FILE=chr_dbg_file$(:seiclen(chr_dbg_file$)), !
     &      STATUS='UNKNOWN', IOSTAT=CODE )              !
C
         IF( CODE .EQ. E_OK$ ) THEN                      ! Successful
         DBGUNIT$ = IX                                   ! Setup unit.
         IZ = F_MAXU$ - IX                               ! Map position.
         CHR_MAP$(IZ:IZ) = 'O'                           ! Mapped unit open.
         CHR_FILE_OPEN$(IX) = CHR_DBG_FILE$              ! & copy filename.
C
         ELSE                                            ! Otherwise fatal error
         IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT           ! Fatal; pc graphics.
         WRITE(*,*)
         WRITE(*,*)
     &'**** FATAL: problem opening debug file             ****'
         WRITE(*,*)'          ',
     &'on pre-allocated unit ', IX
         WRITE(*,*)'          ',
     &   CHR_DBG_FILE$(:SEICLEN(CHR_DBG_FILE$))
         WRITE(*,*)'     FORTRAN condition ', CODE       !
         WRITE(*,*)
     &'****                                               ****'
         GOTO 9998                                       ! & exit.
         END IF                                          !
      END IF                                             !
C
C     1.2. Prompt the user for a filename?....
C     ========================================
C
1000  CODE = E_OK$                           !
      IF( B_PROMPT ) THEN                    ! & prompt
C
C     1.2.0 In message mode...
C     ------------------------
C
         IF( B_F_MESSAGE$ ) THEN                ! Prompting allowed.
         WRITE(*,*)
         WRITE(*,*) CHR_PROMPT                  ! Write prompt.
         WRITE(*,*)'(press <return> for exit prompt)'
C
C     1.2.1 Fatal if PC graphics at this point...
C     -------------------------------------------
C
         ELSE                                   ! Otherwise.
         CALL PC_TO_TEXT                        ! Get out of graphics.
         WRITE(*,*)
         WRITE(*,*)
     &'**** FATAL: unable to PROMPT for information in    ****'
         WRITE(*,*)
     &'****        PC graphics mode                       ****'
         WRITE(*,*)
     &'****                                               ****'
         GOTO 9998                              ! & abort.
         END IF                                 !
C
C     -------------------------------------
C     1.2.2 Ok to proceed with prompting...
C     -------------------------------------
C
      READ(*,'(A)') CHR_TEXT                 ! Get the response.
C
C     No response..does the user want to abort?...
C     --------------------------------------------
C
         IF( CHR_TEXT .EQ. ' ' ) THEN        ! & again.
         WRITE(*,*)
            IF( B_DIRECTORY ) THEN           ! Directory pathname.
            WRITE(*,*)
     &   '**** WARN: no directory pathname supplied          ****'
            ELSE                             ! File pathname.
            WRITE(*,*)
     &   '**** WARN: no filename/file pathname supplied      ****'
            END IF                           !
C
         WRITE(*,*)
         WRITE(*,*)'           enter "(Y)es" to exit'
C
         READ(*,'(A)') CHR_TEXT               ! Get reply.
C
C        abort the program...
C        --------------------
C
            IF( CHR_TEXT(1:1) .EQ. 'Y'   .OR. ! Abort.
     &          CHR_TEXT(1:1) .EQ. 'y' ) THEN !
            GOTO 9998                         ! & stop.     
C
C        try again...
C        ------------
C
            ELSE                              ! Otherwise.
            GOTO 1000                         !
            END IF                            !
C
C     Setup the filename...
C     ---------------------
C
         ELSE                                 ! Otherwise.
         CHR_FILE = CHR_TEXT                  ! & copy.
         END IF                               !
      END IF                                  !
C
C     ==============================
C     2.   See if the file exists...
C     ==============================
C     2.0.1. Check directory exists...
C     --------------------------------
C
      IF( B_DIRECTORY ) THEN                  ! Check directory exists.
      CHR_TEXT = CHR_FILE                     ! Copy directory pathname.
      IY = SEI CLEN( CHR_TEXT )               ! Length of filename.
      CALL DIR_CHAR( CHR_DELIM )              ! Get path delimiter.
C
C     Set up pathname of a special file to create in this directory...
C
         IF( CHR_TEXT(IY:IY) .EQ. CHR_DELIM ) THEN ! Ends with delimiter.
         CHR_TEXT(IY+1:) = 'LIBSEI$DIR$'           ! File pathname.
         ELSE                                      ! Otherwise.
         CHR_TEXT(IY+1:) = CHR_DELIM     //        ! File pathname.
     &                     'LIBSEI$DIR$'           ! Ditto.
         END IF                                    !
C
C     Get next available unit...
C
      CALL SEI GETU( IX, CODE )               ! Next unit.
      IF( CODE .NE. E_OK$ ) GOTO 9998         ! Fatal error.
C
C     Open unknown file, then delete it...    
C
         if( b_f_debug$ ) then
         write(dbgunit$,*)'...opening test file...'
         write(dbgunit$,*)'   file   : ',chr_text(:seiclen(chr_text))
         write(dbgunit$,*)'   on unit: ',ix
         end if
C
      OPEN( UNIT=IX, FILE=CHR_TEXT(:SEICLEN(CHR_TEXT)), ! Open file.
     &      STATUS='UNKNOWN', IOSTAT=CODE )             !
      B_OLD = CODE .EQ. E_OK$                           ! Exists.
C
C     Directory exists, reserve unit just in case we can't delete
C     without an error...
C
         IF( B_OLD ) THEN                      ! Found ok, reserve.
         IZ = F_MAXU$ - IX                     ! Map position.
         CHR_MAP$(IZ:IZ) = 'O'                 ! Mapped unit open.
         CHR_FILE_OPEN$(IX) = CHR_TEXT         ! & copy filename.
         CALL SEI CLOSE(DELETE$+WARN$,IX,CODE) ! Delete file & warn of error.
C
C     Problem...
C   
         ELSE IF( B_F_MESSAGE$ ) THEN          ! Otherwise.
         WRITE(*,*)
         WRITE(*,*)
     &   '**** WARN: unable to open files in this directory  ****'
         WRITE(*,*)
     &   '****       or directory does not exist.            ****'
         WRITE(*,*)
         WRITE(*,*)'           ', CHR_FILE(:SEICLEN(CHR_FILE))
         END IF                               !
C
      CODE = E_OK$                            ! No error condition.
      GOTO 9999                               ! Return to caller.
      END IF                                  !
C
C     2.0.2. Check file exists...
C     ---------------------------
C
      INQUIRE(FILE=CHR_FILE(:SEICLEN(CHR_FILE))           ! Find out.
     &                     ,EXIST=B_F_EXIST$,IOSTAT=CODE) ! File exists?.
      B_OLD = B_F_EXIST$                                  ! Existance?.
C
C     2.1  Errors from enquiry...
C     ===========================
C
      IF( CODE .NE. E_OK$ ) THEN                          ! Fatal error.
      IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT               ! Cancel pc graphics.
      WRITE(*,*)
      WRITE(*,*)
     &'**** FATAL: problem checking file exists           ****'
      WRITE(*,*)'          ',CHR_FILE(:SEICLEN(CHR_FILE))
      WRITE(*,*)'     FORTRAN condition ', CODE           !
      WRITE(*,*)
     &'****                                               ****'
c         
         if( b_f_debug$ ) then
         WRITE(dbgunit$,*)
     &'**** FATAL: problem checking file exists           ****'
         WRITE(dbgunit$,*)'          ',
     &   CHR_FILE(:SEICLEN(CHR_FILE))
         WRITE(dbgunit$,*)'     FORTRAN condition ', CODE           !
         end if
c      
      GOTO 9998                                           ! & exit.
C
C     2.2  Caller only wished to check existance of file...
C     =====================================================
C
      ELSE IF( B_CHECK ) THEN                            ! Just checking.
      GOTO 9999                                          ! & return to caller.
C
C     2.3  File status is considered UNKNOWN or SCRATCH...
C     ====================================================
C
      ELSE IF( B_UNKNOWN .OR. B_SCRATCH ) THEN           ! No action.
      CONTINUE                                           !
C
C     2.4  File must exist if caller indicates this...
C     ================================================
C
      ELSE IF( B_EXIST .AND. (.NOT.B_F_EXIST$) ) THEN    ! Error.
      CODE = E_INIT$                                     ! Bad initialisation.
      IF( B_IGNORE ) GOTO 9999                           ! Ignore.
C
      IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) )      ! Fatal; PC graphics.
     &    CALL PC_TO_TEXT                                ! Change to text mode.
         IF( B_F_MESSAGE$ ) THEN                         ! Write message.
         WRITE(*,*)
         WRITE(*,*)
     &'**** WARN: specified file does not exist           ****'
         WRITE(*,*)'          ',CHR_FILE(:SEICLEN(CHR_FILE))
         WRITE(*,*)
     &'****                                               ****'
         END IF
c
         if( b_f_debug$ ) then
         WRITE(dbgunit$,*)
     &'**** WARN: specified file does not exist           ****'
         WRITE(dbgunit$,*)'          ',
     &   CHR_FILE(:SEICLEN(CHR_FILE))
         end if
C
         IF( B_PROMPT ) THEN                          ! Prompted filename.
         GOTO 1000                                    ! & prompt again.
C
         ELSE IF( B_WARN ) THEN                       ! Back to caller.
         CODE = E_INIT$                               ! Bad initialisation.
         GOTO 9999                                    ! & return to caller.
C
         ELSE                                         ! Otherwise.
         GOTO 9998                                    ! Exit program
         END IF                                       ! 
C
C     2.5  Caller expects a new file but file already exists...
C     =========================================================
C
      ELSE IF( B_F_EXIST$ .AND. (.NOT.B_EXIST) ) THEN ! New file exists!!
      CODE = E_INIT$                                  ! Bad initialisation.
      IF( B_IGNORE ) GOTO 9999                        ! Ignore.
C
      IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) )   ! Fatal; PC graphics.
     &    CALL PC_TO_TEXT                             ! Change to text mode.
         IF( B_F_MESSAGE$ ) THEN                      ! Messages allowed.
         WRITE(*,*)
         WRITE(*,*)
     &'**** WARN: specified file already exists           ****'
         WRITE(*,*)'          ',CHR_FILE(:SEICLEN(CHR_FILE))
         WRITE(*,*)
     &'****                                               ****'
         END IF
c
         if( b_f_debug$ ) then
         WRITE(dbgunit$,*)
     &'**** WARN: specified file already exists           ****'
         WRITE(dbgunit$,*)'          ',
     &   CHR_FILE(:SEICLEN(CHR_FILE))
         end if
C
         IF( B_PROMPT ) THEN                          ! Prompted filename.
         GOTO 1000                                    ! & prompt again.
C
         ELSE IF( B_WARN ) THEN                       ! Back to caller.
         CODE = E_INIT$                               ! Bad initialisation.
         GOTO 9999                                    ! & return to caller.
C
         ELSE                                         ! Otherwise.
         GOTO 9998                                    ! Exit program
         END IF                                       !
      END IF                                           !
C
C    ========================
C    3.    Open the file.....
C    ========================
C    3.1   Set up for opening...
C    ===========================
C
3000  IF( B_UNKNOWN ) THEN                  ! Assume unknown status.
      CHR_F_STAT$ = 'UNKNOWN'               !
C
      ELSE IF( B_SCRATCH ) THEN             ! Scratch file.
      CHR_F_STAT$ = 'SCRATCH'               !
C
      ELSE IF( B_F_EXIST$ ) THEN            ! File exists.
      CHR_F_STAT$ = 'OLD'                   !
C
      ELSE                                  ! Otherwise new.
      CHR_F_STAT$ = 'NEW'                   !
      END IF                                !
C
C    3.1.1 Get the next available unit....
C    -------------------------------------
C
      CALL SEI GETU( UNIT, CODE )           ! That's it.
      IF( CODE .NE. E_OK$ ) THEN            ! Problem getting unit.
      GOTO 9998                             ! & abort.
      END IF                                !
C
C    3.1.2 Final check to see if the unit is physically open...
C    ----------------------------------------------------------
C
      IY = UNIT                                         ! Copy unit.
      INQUIRE( UNIT= IY, OPENED= B_OPEN, IOSTAT= CODE ) ! Unit open?.
C
C    3.1.2.1 Errors from enquiry...
C
      IF( CODE .NE. E_OK$ ) THEN                          ! Fatal error.
      IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT               ! Cancel  pc graphics.
      WRITE(*,*)
      WRITE(*,*)
     &'**** FATAL: problem checking file open             ****'
      WRITE(*,*)'          ',CHR_FILE(:SEICLEN(CHR_FILE))
      WRITE(*,*)'     FORTRAN condition ', CODE           !
      WRITE(*,*)
     &'****                                               ****'
c      
         if( b_f_debug$ ) then
         WRITE(dbgunit$,*)
     &'**** FATAL: problem checking file open             ****'
         WRITE(dbgunit$,*)'          ',
     &   CHR_FILE(:SEICLEN(CHR_FILE))
         WRITE(dbgunit$,*)'     FORTRAN condition ', CODE           !
         end if
c
      GOTO 9998                                           ! & exit.
C
C    3.1.2.2 Already open...fatal since outside scope of file handler...
C
      ELSE IF( B_OPEN ) THEN                              ! Already open.
      IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT               ! Cancel  pc graphics.
      WRITE(*,*)
      WRITE(*,*)
     &'**** FATAL: specified file already open            ****'
      WRITE(*,*)'          ',CHR_FILE(:SEICLEN(CHR_FILE))
      WRITE(*,*)'     File open outside scope of SEISAN'
      WRITE(*,*)'     file handler.'
      WRITE(*,*)
     &'****                                               ****'
c      
         if( b_f_debug$ ) then
         WRITE(dbgunit$,*)
     &'**** FATAL: specified file already open            ****'
         WRITE(dbgunit$,*)'          ',
     &   CHR_FILE(:SEICLEN(CHR_FILE))
         WRITE(dbgunit$,*)
     &   '     File open outside scope of SEISAN'
         WRITE(dbgunit$,*)'     file handler.'
         end if
c
      GOTO 9998                                      ! Abort.
C
C    3.1.2.3 Store current unit...
C    -----------------------------
C
      ELSE                                           ! Otherwise ok.
      F_UNIT$ = UNIT                                 ! Set the unit to open.
      END IF                                         !
C
C    3.2.   Open the file as required...
C    ===================================
C    3.2.1. DAM file....
C    -------------------
C
      IF( B_DAMF ) THEN                                 ! Dam file operation.
      CHR_F_ACCESS$  = 'DIRECT'                         !
C
         IF( B_SCRATCH ) THEN                           ! Scratch file.
            if( b_f_debug$ ) then                       ! Debug information.
            write(dbgunit$,*)
            write(dbgunit$,*)'opening direct access scratch file...'
            write(dbgunit$,*)'on unit:', f_unit$
            write(dbgunit$,*)'recl   :', f_recl$
            write(dbgunit$,*)'details:' ,chr_f_stat$
     &                             ,' ' ,chr_f_access$
     &                             ,' ' ,chr_f_form$     
            end if                                      !
C
         OPEN( UNIT  = F_UNIT$,                         !
     &         STATUS= CHR_F_STAT$, ACCESS= CHR_F_ACCESS$, 
     &         FORM  = CHR_F_FORM$, RECL  = F_RECL$,    ! Caller supplies these.
     &         IOSTAT= STATE )                          !
C
         ELSE                                           ! Otherwise named file.
            if( b_f_debug$ ) then                       ! Debug information.
            write(dbgunit$,*)
            write(dbgunit$,*)'opening direct access file...'
            write(dbgunit$,*)'file   :',chr_file(:seiclen(chr_file))
            write(dbgunit$,*)'on unit:', f_unit$
            write(dbgunit$,*)'recl   :', f_recl$
            write(dbgunit$,*)'details:' ,chr_f_stat$
     &                             ,' ' ,chr_f_access$
     &                             ,' ' , chr_f_form$ 
            end if                                      !
C
         OPEN( UNIT  = F_UNIT$,                         !
     &         FILE  = CHR_FILE(:SEICLEN(CHR_FILE)),    !
     &         STATUS= CHR_F_STAT$, ACCESS= CHR_F_ACCESS$,  
     &         RECL  = F_RECL$,                         ! Caller supplies these.
     &         IOSTAT= STATE )                          ! jh: remove form=
         END IF                                         !
C
C    3.2.2. SAM file....
C    -------------------
C    Consider access types...
C
      ELSE                                              ! SAM file operation.
         IF( B_APPEND ) THEN                            ! Open for append.
         CHR_F_ACCESS$ = 'APPEND'                       !
         ELSE                                           ! Otherwise.
         CHR_F_ACCESS$ = 'SEQUENTIAL'                   !
         END IF                                         !
C
C    Unformatted files...
C
         IF( CHR_F_FORM$ .EQ. 'UNFORMATTED' ) THEN      ! Unformatted.
            IF( B_SCRATCH ) THEN                        ! Scratch filename.
            if( b_f_debug$ ) then                       ! Debug information.
            write(dbgunit$,*)
            write(dbgunit$,*)'opening sam scratch file...'
            write(dbgunit$,*)'on unit:', f_unit$
            write(dbgunit$,*)'details:' ,chr_f_stat$
     &                             ,' ' ,chr_f_access$
     &                             ,' ' ,chr_f_form$ 
            end if                          
C
            OPEN( UNIT  = F_UNIT$,                         !
     &            FORM  = CHR_F_FORM$,                     ! 
     &            STATUS= CHR_F_STAT$, ACCESS= CHR_F_ACCESS$, 
     &            IOSTAT= STATE )                          !
C
            ELSE                                           ! Otherwise named.
            if( b_f_debug$ ) then                          ! Debug information.
            write(dbgunit$,*)
            write(dbgunit$,*)'opening sam file...'
            write(dbgunit$,*)'file   :', chr_file(:seiclen(chr_file))
            write(dbgunit$,*)'on unit:', f_unit$
            write(dbgunit$,*)'details:' ,chr_f_stat$
     &                             ,' ' ,chr_f_access$
     &                             ,' ' ,chr_f_form$ 
            end if                       
C
            OPEN( UNIT  = F_UNIT$,                         !
     &            FILE  = CHR_FILE(:SEICLEN(CHR_FILE)),    !
     &            FORM  = CHR_F_FORM$,                     !
     &            STATUS= CHR_F_STAT$, ACCESS= CHR_F_ACCESS$, 
     &            IOSTAT= STATE )                          !
            END IF                                         !
C
C    Formatted files....
C
         ELSE                                              ! Otherwise format.
            IF( B_SCRATCH ) THEN                           ! Scratch file. 
            if( b_f_debug$ ) then                          ! Debug information.
            write(dbgunit$,*)
            write(dbgunit$,*)'opening sam formatted scratch file...'
            write(dbgunit$,*)'on unit:', f_unit$
            write(dbgunit$,*)'details:' ,chr_f_stat$
     &                             ,' ' ,chr_f_access$ 
            end if                                 
C
            OPEN( UNIT  = F_UNIT$,                         !
     &            STATUS= CHR_F_STAT$, ACCESS= CHR_F_ACCESS$, 
     &            IOSTAT= STATE )                          !
C
            ELSE                                           ! Otherwise named.
            if( b_f_debug$ ) then                          ! Debug information.
            write(dbgunit$,*)
            write(dbgunit$,*)'opening sam formatted file...'
            write(dbgunit$,*)'file   :', chr_file(:seiclen(chr_file))
            write(dbgunit$,*)'on unit:', f_unit$
            write(dbgunit$,*)'details:' ,chr_f_stat$
     &                             ,' ' ,chr_f_access$ 
            end if                                    
C
            OPEN( UNIT  = F_UNIT$,                         !
     &            FILE  = CHR_FILE(:SEICLEN(CHR_FILE)),    !
     &            STATUS= CHR_F_STAT$, ACCESS= CHR_F_ACCESS$, 
     &            IOSTAT= STATE )                          !
            END IF                                         !
         END IF                                            !
      END IF                                               !
C
C    3.3. Process the outcome...
C    ===========================
C    Successful...
C    -------------
C
      IF( STATE .EQ. E_OK$ ) THEN                        !
      UNIT = F_UNIT$                                     ! & copy.
      IZ = F_MAXU$ - F_UNIT$                             ! Map position.
      CHR_MAP$(IZ:IZ) = 'O'                              ! Mapped unit open.
      CHR_FILE_OPEN$(UNIT) = CHR_FILE                    ! & copy filename.
      GOTO 9999                                          ! Return.
C
C     Not successful...
C     -----------------
C
      ELSE                                               ! Otherwise.
      F_UNIT$ = COPY_UNIT                                ! Put back current
      CODE = E_INIT$                                     ! Bad initialisation.
      IF( B_IGNORE ) GOTO 9999                           ! Ignore.
C
      IF( (.NOT.B_WARN) .AND. (.NOT.B_F_MESSAGE$) )      ! Fatal; PC graphics.
     &    CALL PC_TO_TEXT                                ! Change to text mode.
      WRITE(*,*)
      WRITE(*,*)
     &'**** WARN: problem opening file                    ****'
      WRITE(*,*)'          ',CHR_FILE(:SEICLEN(CHR_FILE))
      WRITE(*,*)
     &'****                                               ****'
c
         if( b_f_debug$ ) then
         WRITE(dbgunit$,*)
     &'**** WARN: problem opening file                    ****'
         WRITE(dbgunit$,*)'          ',
     &   CHR_FILE(:SEICLEN(CHR_FILE))
         end if
C
         IF( B_PROMPT ) THEN                          ! Prompted filename.
         GOTO 1000                                    ! & prompt again.
C
         ELSE IF( B_WARN ) THEN                       ! Back to caller.
         CODE = E_INIT$                               ! Bad initialisation.
         GOTO 9999                                    ! & return to caller.
C
         ELSE                                         ! Otherwise.
         GOTO 9998                                    ! Exit program
         END IF                                       !
      END IF                                          !
C
C    4. ABORT point.....
C    ===================
C
9998  IF(.NOT.B_F_MESSAGE$) CALL PC_TO_TEXT           ! Not required!!.
      WRITE(*,*)
      WRITE(*,*)
     &'**** WARN: halting program in SEIOPEN              ****'
      WRITE(*,*)
     &'****       ...closing down files                   ****'
c
      if( b_f_debug$ ) then
      WRITE(dbgunit$,*)
     &'**** WARN: halting program in SEIOPEN              ****'
      WRITE(dbgunit$,*)
     &'****       ...closing down files                   ****'
      end if
C
      CALL SEI CLOSE(CLOSE$+ALL$,             ! Close all files.
     &               0,                       ! File unit (n/a).
     &               CODE )                   ! Returned condition (n/a).
      STOP                                    !
C
C    ===================
C    Return to caller...
C    ===================
C    Re-set "file-open" attributes...
C    ================================
C
9999  F_RECL$       = 0
      CHR_F_STAT$   = ' '
      CHR_F_ACCESS$ = ' '
      CHR_F_FORM$   = ' '
      CHR_F_ACTION$ = CHR_SET_ACTION$
C
C    & return...
C    ===========
C
      RETURN    
      END
C
      REAL FUNCTION SEI REAL NUM( CHR_STR, CODE )
CSTART*************************************************************************
C                                                                             *
C    System           : SEISAN Utilities                                      *
C    Supplier         : BGS/GSRG Programming Unit                             *
C    Routine          : SEI REAL NUM                                          *
C    Purpose          : To decode a number from a character string containing *
C                       either an integer or real number.                     *
C    Arguments -Input : CHR_STR     (C*) Number character string              *
C              -Output: CODE        (I)  Returned condition code              *
C                     : SEI REAL NUM returns the real value of the decode     *
C    Note             : Based on PGG REAL NUM from libugeo.for (BGS/GSRG)     *
C                       by same author.                                       *
C    Author           : J A Bolton                                            *
C    Date             : 6 December 1994                                       *
C    Version          : V01                                                   *
C                                                                             *
CEND***************************************************************************
C
      EXTERNAL  SEI INTEGER                 ! Get integer.
     &         ,SEI UPC                     ! Uppercase.
     &         ,SEI CLEN                    ! String length.
      INTEGER   SEI CLEN                    ! Function.
     &         ,SEI INTEGER                 ! Ditto. 
C
C    System definitions....
C    ======================
C
      INCLUDE  'libsei.inc'                 ! Library definitions & data defns.
C
C    Arguments.....
C    ==============
C
      INTEGER      CODE                     ! Returned condition.
      CHARACTER    CHR_STR *(*)             ! Number string.
C
C    Local variables...
C    ==================
C
      INTEGER      IW, IX, IY, IZ,          ! Very local.
     &             FORM_C                   ! Length of format string.
      REAL*8       NUMBER                   ! Decoded number.
      PARAMETER   (FORM_C = 8)              ! & value.
      CHARACTER    CHR_FORMAT *(FORM_C)     ! Format string.
     &            ,CHR_STRING *(80)         ! & working text string.
      LOGICAL      B_EXP,                   ! Simple exponent.
     &             B_DEXP,                  ! Double exponent.
     &             B_REAL,                  ! Real format.
     &             B_INTEGER                ! Integer format.
C
C    Initialise....
C    ==============
C
      CODE       = E_OK$                          ! Initialise return code.
      CHR_STRING = CHR_STR                        ! Working string.
C
      NUMBER = 0.0D0                              ! Working decoded number.
      IX     = SEICLEN( CHR_STRING )              ! Length of string.
      CALL SEI UPC( CHR_STRING(:IX) )             ! In uppercase.
C
      IW     = INDEX( CHR_STRING(:IX), 'E' )      ! Exponential format.
      IZ     = INDEX( CHR_STRING(:IX), 'D' )      ! Exponential format.
      IY     = IX - INDEX( CHR_STRING(:IX), '.' ) ! Decimal point.
C
      B_EXP     = IW .GT. 0                 ! Exponent input?.
      B_DEXP    = IZ .GT. 0                 ! Double precisison exponent?.
      B_REAL    = IX .NE. IY      .AND.     ! Real number?.
     &            (.NOT.B_EXP )   .AND.     !
     &            (.NOT.B_DEXP)             !
      B_INTEGER = IX .EQ. IY      .AND.     ! An integer?.
     &            (.NOT.B_EXP )   .AND.     !
     &            (.NOT.B_DEXP)             !
C
      IW        = MAX0( IW, IZ )            ! Use for both single & double exp.
      IF( IW .GT. 0 ) IY = IY + IW - IX - 1 ! Update.
C
C    Decode integer...
C    -----------------
C
      IF( B_INTEGER ) THEN                          ! Integer.
      NUMBER = DBLE( SEI INTEGER(CHR_STRING,CODE) ) ! & convert.
      IF( CODE .NE. E_OK$ ) GOTO 9999               ! On error.
C
C    Decode real...
C    --------------
C
      ELSE IF( B_REAL      .AND.                              ! Real.
     &         IX .GT. 9 ) THEN                               ! & long length.
        IF( IY .GT. 9 ) THEN                                  ! Lots of decimal
        WRITE(CHR_FORMAT,'(''(F'',I2,''.'',I2,'')'')') IX, IY ! Format.
        ELSE                                                  ! Few decimals.
        WRITE(CHR_FORMAT,'(''(F'',I2,''.'',I1,'')'')') IX, IY ! Format.
        END IF                                                !
      READ(CHR_STRING,CHR_FORMAT,ERR=9000) NUMBER             ! & convert.
C
      ELSE IF( B_REAL ) THEN                                  ! Small real.
      WRITE(CHR_FORMAT,'(''(F'',I1,''.'',I1,'')'')')   IX, IY ! Format.
      READ(CHR_STRING,CHR_FORMAT,ERR=9000) NUMBER             ! & convert.
C
C    Decode real with exponent...
C    ----------------------------
C
      ELSE IF( B_EXP       .AND.                              ! Real.
     &         IX .GT. 9 ) THEN                               !
        IF( IY .GT. 9 ) THEN                                  ! Lots of decimal
        WRITE(CHR_FORMAT,'(''(E'',I2,''.'',I2,'')'')') IX, IY ! Format.
        ELSE                                                  ! Few decimals.
        WRITE(CHR_FORMAT,'(''(E'',I2,''.'',I1,'')'')') IX, IY ! Format.
        END IF                                                !
      READ(CHR_STRING,CHR_FORMAT,ERR=9000) NUMBER             ! & convert.
C
      ELSE IF( B_EXP ) THEN                                   ! Small real.
      WRITE(CHR_FORMAT,'(''(E'',I1,''.'',I1,'')'')')   IX, IY ! Format.
      READ(CHR_STRING,CHR_FORMAT,ERR=9000) NUMBER             ! & convert.
C
C    Decode real with Double precision exponent...
C    ---------------------------------------------
C
      ELSE IF( IX .GT. 9 ) THEN                               ! Real.
        IF( IY .GT. 9 ) THEN                                  ! Lots of decimal
        WRITE(CHR_FORMAT,'(''(D'',I2,''.'',I2,'')'')') IX, IY ! Format.
        ELSE                                                  ! Few decimals.
        WRITE(CHR_FORMAT,'(''(D'',I2,''.'',I1,'')'')') IX, IY ! Format.
        END IF                                                !
      READ(CHR_STRING,CHR_FORMAT,ERR=9000) NUMBER             ! & convert.
C
      ELSE                                                    ! Small real.
      WRITE(CHR_FORMAT,'(''(D'',I1,''.'',I1,'')'')')   IX, IY ! Format.
      READ(CHR_STRING,CHR_FORMAT,ERR=9000) NUMBER             ! & convert.
      END IF                                                  !
C
C    Errors...
C    =========
C
      GOTO 9999                               ! Bypass if ok.
9000  CODE = E_CONV$                          ! Unable to convert.
      IF( B_F_MESSAGE$ ) THEN                 !
      WRITE(*,*)                              !
      WRITE(*,*)                              !
     &'**** WARN: unable to decode a number in the string  ****'
      END IF                                  !
C
C    Return to caller...
C    ===================
C
9999  SEI REAL NUM = NUMBER                    ! Assign.
      if( b_f_debug$ ) then                    ! Debug detail.
      write(dbgunit$,*)'sei real num: '
     &                 ,chr_string(:seiclen(chr_string))
     &                 ,' ... ' ,number
      end if
C
      RETURN
      END
C
      SUBROUTINE SEI UPC( CHR_STRING )
C
CSTART*************************************************************************
C                                                                             *
C   System          : SEI  Validation & Application Library                   *
C   Supplier        : BGS/GSRG  Applications Programming Unit                 *
C   Procedure Name  : SEI UPC                                                 *
C   Purpose         : Converts any lowercase letters in CHR_STRING to         *
C                     uppercase                                               *
C   Arguments -Input: CHR_STRING  (C*) String to convert                      *
C            -Output: CHR_STRING  (C*) Converted string                       *
C   Author          : J. A. Bolton                                            *
C   Date            : 23 November 1994                                        *
C   Version         : V01                                                     *
C                                                                             *
CEND***************************************************************************
C
C    Arguments...
C    ============
C
      CHARACTER     CHR_STRING *(*)        ! String to convert.
C
C    Local variables....
C    ===================
C
      INTEGER*4     I,                                !  Loop counter
     &              OFFSET                            !  Case offset.
      CHARACTER     CHR1*(1)                          !  Temporary character
C
C    Set-up local case offset...
C    ===========================
C
      OFFSET = ICHAR('A') - ICHAR('a')                !  Displacement.
C
C    Loop round whole string...
C    ==========================
C
      DO 100 I = 1, LEN(CHR_STRING)                   !  Loop input string.
      CHR1 = CHR_STRING(I:I)                          !  & get character.
         IF( CHR1 .GE. 'a'   .AND.                    !  If lower case... (L11).
     &       CHR1 .LE. 'z' ) THEN                     !
         CHR_STRING(I:I) = CHAR(ICHAR(CHR1)+OFFSET)   !  ...convert to upper
         ENDIF                                        !                   (L11).
100   CONTINUE                                        !
C
C    Return to caller...
C    ===================
C
9999  RETURN
      END
C
CSTART*************************************************************************
C                                                                             *
C                    BGS/GSRG Applications Programming Unit                   *
C                                                                             *
C      Name    : ERROR_BD Error messages for the SEISAN system                *
C      Author  : J. A. Bolton                                                 *
C      Date    : 23 November 1994                                             *
C      Version : V01                                                          *
C                                                                             *
CEND***************************************************************************
C
      BLOCK DATA BD_ERROR
C
C     Insert files....
C     ================
C
      INCLUDE  'libsei.inc'      ! Seisan library definitions & data definitions
C
C     Initialise Local & system error vector...
C     =========================================
C
      DATA     ERROR$ / 2*0 /
C
C     Initialise Unrecoverable Local Errors....
C     =========================================
C
      DATA     E_FAIL$ / E_INIT$,
     &                   19*0     /
C
C     Initialise the text string buffer & pointer...
C     ==============================================
C
      DATA     CHR_ERR_MSG$
     &/'                                                            '/,
     &         ERR_LEN$ / 0 /
C
C     Messages texts...
C     =================
C
      DATA CHR_ERR_TXT$(1)
     &/'Unable to convert                                           '/
C
      DATA CHR_ERR_TXT$(2)
     &/'Initialisation error                                        '/
C
      DATA CHR_ERR_TXT$(3)
     &/'Command error, command line not executed                    '/
C
      DATA CHR_ERR_TXT$(4)
     &/'Invalid option supplied                                     '/
C
      DATA CHR_ERR_TXT$(5)
     &/'FORTRAN read-write function error                           '/
C
      DATA CHR_ERR_TXT$(6)
     &/'Unable to open the index file                               '/
C
      DATA CHR_ERR_TXT$(7)
     &/'Empty file or premature end of file                         '/
C
      DATA CHR_ERR_TXT$(8)
     &/'Missing file                                                '/
C
      DATA CHR_ERR_TXT$(9)
     &/'Denominator is too small                                    '/
C
      END
C
CSTART*************************************************************************
C                                                                             *
C                    BGS/GSRG Applications Programming Unit                   *
C                                                                             *
C      Name    : FORTIO_BD File information for current file to open or opened*
C      Author  : J. A. Bolton                                                 *
C      Date    : 23 November 1994                                             *
C      Version : V01                                                          *
C                                                                             *
CEND***************************************************************************
C

      BLOCK DATA BD_FILEIO
C
C     Insert files....
C     ================
C
      INCLUDE  'libsei.inc'      ! Seisan library definitions & data definitions
C
C     Initialise...
C     =============
C
      DATA
C
     &             F_UNIT$       / F_MAXU$ /,
     &             F_RECL$       / 512 /,
     &             CHR_F_STAT$   / ' ' /,
     &             CHR_F_ACCESS$ / ' ' /,
     &             CHR_F_FORM$   / ' ' /,
     &             CHR_F_ACTION$ / CHR_SET_ACTION$ /,
     &             B_F_EXIST$    / .FALSE. /,
     &             B_F_DEBUG$    / .FALSE. /
C
C    The following are protected FORTRAN device numbers....
C    ======================================================
C    The error unit 0 is always protected....
C    ========================================
C
     &             F_PROTECT$/ 
     &                          STD_IN$,    ! Sunos input.
     &                          STD_OUT$ /  ! Sunos ouput.
C
C    Command & output parameter file units...
C    ========================================
C
      DATA         CMDUNIT$ /STD_IN$   /,
     &             PARUNIT$ /STD_OUT$  /,
     &             DBGUNIT$ / 0        /,
     &             CHR_DBG_FILE$ / ' ' /,
C
C    For PC graphics (message suppression)...
C    ========================================
C
     &             B_F_GRAPH$  /.FALSE./ 
     &            ,B_F_PC$     /.FALSE./
     &            ,B_F_MESSAGE$/.TRUE./
C
C    Open file mapping...
C    ====================
C
      DATA
     &             CHR_MAP$       / ' ' /,
     &             CHR_FILE_OPEN$ / MAP_C$*' ' /
C
      END

C
      SUBROUTINE SEI LCS( CHR_STRING )
C
CSTART*************************************************************************
C   Author          : M.Villagran
C   Date            : July 1998 
CEND***************************************************************************
C
C    Arguments...
C    ============
C
      CHARACTER     CHR_STRING *(*)        ! String to convert.
C
C    Local variables....
C    ===================
C
      INTEGER*4     I,                                !  Loop counter
     &              OFFSET                            !  Case offset.
      CHARACTER     CHR1*(1)                          !  Temporary character
C
C    Set-up local case offset...
C    ===========================
C
      OFFSET = ICHAR('a') - ICHAR('A')                !  Displacement.
C
C    Loop round whole string...
C    ==========================
C
      DO 100 I = 1, LEN(CHR_STRING)                   !  Loop input string.
      CHR1 = CHR_STRING(I:I)                          !  & get character.
         IF( CHR1 .GE. 'A'   .AND.                    !  If lower case... (L11).
     &       CHR1 .LE. 'Z' ) THEN                     !
         CHR_STRING(I:I) = CHAR(ICHAR(CHR1)+OFFSET)   !  ...convert to upper
         ENDIF                                        !                   (L11).
100   CONTINUE                                        !
C
C    Return to caller...
C    ===================
C
9999  RETURN
      END
C

c   Subroutines handling waveform file names and a few other things
c
c   changes
c
c sep 16 98 by jh : ----------  version 7.0 check ----------------
c                   waveform data bases to 5 chars
c ??? BM           linux changed line 118 , include comment
c nov 26 98 jh    : add posibility for many waveform directorires
c jan 5 99  jh    : change ,,,,, to ,,
c feb 16 99 jh    : include in libsei
c apr 27 00 lo    : get_full_wav_name search for compressed files
c feb 21 01 lo    : changed file_in,file_out to variable length


      subroutine get_full_wav_name(file_in,file_out)
c
c   Input is a waveform file without full pateh. The routine will search
c   all waveform directories given in wav_base and return the complete
c   file name with path. If the file is not found, the file name will be 
c   returned blank.

      implicit none
      include 'seisan.inc'    ! general seisan variables
      include 'libsei.inc'    ! for libsei
      character*(*) file_in    ! input file
      character*(120) file_in_save    ! input file saved
      character*(*) file_out   ! output file
      character*60 top_directory
      character*80 file_temp
      integer seiclen         ! function to get string length
      character*1 dchar       ! dir char
      character*2 century     ! century
      logical ex              ! file existance
      integer i,j,k
      integer nsearch         ! number of serach loops
      parameter (nsearch=5)
      character*4 suffix(nsearch) ! suffix array 

c
c define optional suffixes
c
      suffix(1)=''
      suffix(2)='.gz'
      suffix(3)='.zip'
      suffix(4)='.Z'
      suffix(5)='.bz2'

c
c keep file_in variable
c
      file_in_save = file_in
c
c loop for suffixes
c
      do k=1,nsearch
        file_out=' '            ! default is file not found
        file_in=file_in(1:seiclen(file_in_save)) // 
     *            suffix(k)(1:seiclen(suffix(k)))

        file_temp=file_in
c
c   search current directory
c
        inquire(file=file_temp,exist=ex)
        if(ex) then
           file_out=file_temp
           return
        endif

c
c   search TMP directory
c

        call topdir(top_directory)
        call dir_char(dchar)        ! get directory separater
        file_temp=top_directory(1:seiclen(top_directory))//dchar//'TMP'
     *  //dchar//file_in(1:seiclen(file_in))
c
c  check if exist
c
        inquire(file=file_temp,exist=ex)
        if(ex) then
           file_out=file_temp
           return
        endif

c
c   search wav directory
c

        call topdir(top_directory)
        call dir_char(dchar)        ! get directory separater
        file_temp=top_directory(1:seiclen(top_directory))//dchar//'WAV'
     *  //dchar//file_in(1:seiclen(file_in))
c
c  check if exist
c
        inquire(file=file_temp,exist=ex)
        if(ex) then
           file_out=file_temp
           return
        endif
c
c   check if an old name without century or a new with century
c
        read(file_in(1:2),'(i2)',err=800) j
        goto 801
 800    continue
        write(6,*)' Waveform file name does not start with a number'
        return
c        stop   ! dont stop, changed 04-04-2001 lo

 801  continue
c
c  now check in all wav directories
c
         do i=1,n_wav_bases
            if(j.ne.19.and.j.ne.20) then    ! check which century if not given
               if(j.gt.50) then
                  century='19'
               else
                  century='20'
               endif
               file_temp=top_directory(1:seiclen(top_directory))
     *         //dchar//
     *         'WAV'//dchar//wav_base(i)//dchar//
     *         century//                     ! century
     *         file_in(1:2)//dchar//         ! year
     *         file_in(3:4)//dchar//         ! month
     *         file_in(1:seiclen(file_in))
            else
               file_temp=top_directory(1:seiclen(top_directory))
     *         //dchar//
     *         'WAV'//dchar//wav_base(i)//dchar//
     *         file_in(1:4)//dchar//         ! year with century
     *         file_in(6:7)//dchar//         ! month
     *         file_in(1:seiclen(file_in))
            endif
c
c           write(6,'(1x,a)') file_temp
            inquire(file=file_temp,exist=ex)
            if(ex) then
               file_out=file_temp
               return
            endif
         enddo
c
c  now check in all full path directories
c
       do i=1,n_wav_dirs
           file_temp=wav_dirs(i)(1:seiclen(wav_dirs(i)))//
     *     dchar//file_in(1:seiclen(file_in))
 
c
           inquire(file=file_temp,exist=ex)
           if(ex) then
              file_out=file_temp
              return
           endif
        enddo
c
      enddo
      file_in = file_in_save
      return
      end

c ----------------------------------------------------------------------------------------

      subroutine get_seisan_def
c
c   read in seisan parameters
c
      implicit none
      external sei get file,                 ! Search directories & open file.
     &         sei close,                    ! Close file open.
     &         sei code                      ! Error processor.
      include 'libsei.inc'                   ! Library definitions & data defns
      include 'seisan.inc'
      integer  code                          ! Condition.
      logical  b_eof                         ! End of file?.
      real rmap_proj
      real parameter                         ! help variable 
      integer i
c
c-- unit for file
      integer def_unit
c-- directory separator char
      character*1 dchar
      character*5  waveform_base             ! one wav base name
      character*120 data                      ! one line of data
      integer n_cont_trace                   ! # of continiuous traces
      integer seiclen

      call dir_char(dchar)
c
c   set default parameters in case there is no seisan.def
c
      n_wav_dirs=0
      geo_depth1=50.0
      geo_depth2=100.0
      herkij_distance=100.0
      hypo71_offset=0.0
      copy_wav_dir=' '
      text_print=' '
      merge_wav=' '
      high_accuracy=.false.

      cont_before=60.
      n_cont_base=0
      n_cont_comp=0
      n_cont_trace=0
      cont_before=60.
      cont_after=1000.
      cont_int_s=300.
      cursortype=0.
      focmec_maxsol=100
      myxfont=' '
      myxfont=
     &  '-adobe-courier-bold-r-normal--12-120-75-75-m-70-iso8859-1'
      confirmation_level=1.
      
c
c   waveform data base
c
      call get_env_base(waveform_base)    ! check if a given base
      if(waveform_base(1:2).eq.',,'.      ! local base
     *or.waveform_base.eq.'     '.        ! index file
     *or.ichar(waveform_base(1:1)).eq.0) then                  
        call get_def_base(wav_base(1))    ! put the default into #1
        n_wav_bases=1
      else
        wav_base(1)=waveform_base         ! put choses base in #1
        call get_def_base(wav_base(2))    ! put default in #2
        n_wav_bases=2
      endif
c                                                     
                          
c                                                                               
c   open and read default file               
c   ---------------------------
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   def_unit,         ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'SEISAN.DEF' )    ! For this file.
c                                                                               
c   read file if there...
c   ---------------------
c                                                                               
      if(code.ne.e_ok$)  return

 10   continue
c
      read(def_unit,'(a)',iostat=code) data    ! Read from file.
      call sei code( fort$,                    ! Process fortran i/o condition.
     &               code,                     ! Condition.
     &               def_unit,                 ! On unit.
     &               b_eof )                   ! End of file?.
c
      if( .not.b_eof ) then                    ! Not end of file.
c
c   find default wav bases
c
         if(data(1:13).eq.'WAVEFORM_BASE'.and.
     *      data(41:50).ne.'          ') then
            n_wav_bases=n_wav_bases+1
            wav_base(n_wav_bases)=data(41:45)
c
c   check for blanks
c
            do i=2,5
              if(wav_base(n_wav_bases)(i:i).eq.' ') 
     *        wav_base(n_wav_bases)(i:i)='_'
            enddo
         elseif(data(1:13).eq.'WAVEFORM_DIRS'.and.
     *      data(41:80).ne.' ') then
            n_wav_dirs=n_wav_dirs+1
            wav_dirs(n_wav_dirs)=' '
            wav_dirs(n_wav_dirs)(1:40)=data(41:80)
         elseif (data(1:14).eq.'MERGE_WAVEFORM') then
            merge_wav=data(41:43)
         elseif (data(1:17).eq.'MAP_LAT_BORDER') then
            read(data(41:50),'(f10.1)') map_lat
         elseif (data(1:17).eq.'MAP_LON_BORDER') then
            read(data(41:50),'(f10.1)') map_lon
         elseif (data(1:17).eq.'EPIMAP_LAT_BORDER') then
            read(data(41:50),'(f10.1)') map_lat
         elseif (data(1:17).eq.'EPIMAP_LON_BORDER') then
            read(data(41:50),'(f10.1)') map_lon
         elseif (data(1:15).eq.'EPIMAP_STATIONS') then
            map_stations=data(41:41)
         elseif (data(1:15).eq.'EPIMAP_MAP_FILE') then
            map_file(1:10)=data(41:50)
         elseif (data(1:17).eq.'EPIMAP_PROJECTION') then
            read(data(41:50),'(f10.1)') rmap_proj
            map_proj = int (rmap_proj)
         elseif(data(1:19).eq.'SPECTRAL GEO_DEPTHS'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:60),'(2f10.1)') geo_depth1,geo_depth2
         elseif(data(1:15).eq.'HERKIJ_DISTANCE'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:50),'(f10.1)')herkij_distance 
         elseif(data(1:13).eq.'HYPO71_OFFSET'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:50),'(f10.1)')hypo71_offset 
         elseif(data(1:13).eq.'CONFIRMATION'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:50),'(f10.1)')confirmation_level
         elseif(data(1:13).eq.'REG_KEEP_AUTO'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:50),'(f10.1)') keep_auto
         elseif(data(1:12).eq.'COPY_WAV_DIR'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:45),'(a)') copy_wav_dir 
            do i=2,5
              if(copy_wav_dir(i:i).eq.' ') copy_wav_dir(i:i)='_'
            enddo
         elseif(data(1:10).eq.'TEXT_PRINT'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:80),'(a)') text_print(1:40) 
         elseif(data(1:13).eq.'HIGH_ACCURACY'.and.
     *      data(41:50).ne.'          ') then
            read(data(41:50),'(f10.1)') parameter
            if(parameter.gt.0.0) high_accuracy=.true.
         elseif(data(1:9).eq.'CONT_BASE'.and.
     *      data(41:45).ne.' ') then
            n_cont_base=n_cont_base+1
            cont_base(n_cont_base)=' '
            cont_base(n_cont_base)(1:5)=data(41:45)
c
c also store as wav_base, lot 07/07/2003
c
            n_wav_bases=n_wav_bases+1
            wav_base(n_wav_bases)=data(41:45)
c
c   check for blanks
c
            do i=2,5
              if(wav_base(n_wav_bases)(i:i).eq.' ') 
     *        wav_base(n_wav_bases)(i:i)='_'
            enddo
         elseif(data(1:9).eq.'CONT_COMP'.and.
     *      data(41:44).ne.' ') then
            n_cont_comp=n_cont_comp+1
            cont_comp(n_cont_comp)=' '
            cont_comp(n_cont_comp)(1:4)=data(41:44)
         elseif(data(1:13).eq.'CONT_NET_CODE'.and.
     *      data(41:45).ne.' ') then
            cont_net_code=' '
            cont_net_code(1:5)=data(41:45)
         elseif(data(1:13).eq.'CONT_OPERATOR'.and.
     *      data(41:44).ne.'    ') then
            cont_operator=' '
            cont_operator(1:4)=data(41:44)
         elseif(data(1:11).eq.'CONT_BEFORE') then
            read(data(41:50),'(f10.1)') parameter
            if(parameter.gt.0.0) cont_before=parameter
         elseif(data(1:11).eq.'CONT_AFTER') then
            read(data(41:50),'(f10.1)') parameter
            if(parameter.gt.0.0) cont_after=parameter
         elseif(data(1:13).eq.'CONT_INT_S') then
            read(data(41:50),'(f10.1)') parameter
            if(parameter.gt.0.0) cont_int_s=parameter
         elseif(data(1:6).eq.'CURSOR') then
            read(data(41:50),'(f10.0)') parameter
            cursortype=int(parameter)
         elseif(data(1:13).eq.'FOCMEC MAXSOL') then
            read(data(41:50),'(f10.0)') parameter
            focmec_maxsol=int(parameter)
         elseif(data(1:4).eq.'FONT') then
            myxfont=data(41:seiclen(data))
         elseif(data(1:12).eq.'AUTO_PROCESS'.and.
     *      data(41:50).ne.'          '
     *     .and.data(51:60).ne.'          ') then
            read(data(41:50),'(f10.1)') reg_autoprocess_flag
            reg_autoprocess_name(1:10)=data(51:60)
         endif

c
c   go to next line
c
         goto 10

      endif
c
      call sei close( close$, def_unit, code ) ! Close (Default stop on error).

      return
      end

c ----------------------------------------------------------------------------------------

      subroutine get_cplot_def

c routine to get default values for cplot (defined in CPLOT.DEF)
c    modified after get_seisan_def by Susanne Lund Jensen, May 01
c
c   read in cplot parameters
c
      implicit none
      external sei get file,                 ! Search directories & open file.
     &         sei close,                    ! Close file open.
     &         sei code                      ! Error processor.
      include 'libsei.inc'                   ! Library definitions & data defns
      include 'seisan.inc'
      integer  code                          ! Condition.
      logical  b_eof                         ! End of file?.
      real rmap_proj
      real parameter                         ! help variable 
      integer i
c
c-- unit for file
      integer def_unit
c-- directory separator char
      character*1 dchar
      character*5  waveform_base             ! one wav base name
      character *80 data                     ! one line of data
      integer n_cont_trace                   ! # of continiuous traces
                                        
c
      call dir_char(dchar)
c
c   set default parameters in case there is no seisan.def
c
      n_cont_base=0
      n_cont_comp=0
      n_cont_trace=0
      herkij_distance=100.0
      cont_before=60.
      cont_after=1000.
      cont_int_s=300.
c                                                                               
c   open and read default file               
c   ---------------------------
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   def_unit,         ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'CPLOT.DEF' )     ! For this file.
c                                                                               
c   read file if there...
c   ---------------------
c                                                                               
      if(code.ne.e_ok$)  return

 10   continue
c
      read(def_unit,'(a)',iostat=code) data    ! Read from file.
      call sei code( fort$,                    ! Process fortran i/o condition.
     &               code,                     ! Condition.
     &               def_unit,                 ! On unit.
     &               b_eof )                   ! End of file?.
c
      if( .not.b_eof ) then                    ! Not end of file.

         if(data(1:13).eq.'WAVEFORM_DIRS'.and.
     *      data(41:80).ne.' ') then
            n_wav_dirs=n_wav_dirs+1
            wav_dirs(n_wav_dirs)=' '
            wav_dirs(n_wav_dirs)(1:40)=data(41:80)
         elseif (data(1:14).eq.'MERGE_WAVEFORM') then
            merge_wav=data(41:43)
         elseif(data(1:9).eq.'CONT_BASE'.and.
     *      data(41:45).ne.' ') then
            n_cont_base=n_cont_base+1
            cont_base(n_cont_base)=' '
            cont_base(n_cont_base)(1:5)=data(41:45)
         elseif(data(1:9).eq.'CONT_COMP'.and.
     *      data(41:44).ne.' ') then
            n_cont_comp=n_cont_comp+1
            cont_comp(n_cont_comp)=' '
            cont_comp(n_cont_comp)(1:4)=data(41:44)
         elseif(data(1:13).eq.'CONT_NET_CODE'.and.
     *      data(41:45).ne.' ') then
            cont_net_code=' '
            cont_net_code(1:5)=data(41:45)
         elseif(data(1:13).eq.'CONT_OPERATOR'.and.
     *      data(41:44).ne.'    ') then
            cont_operator=' '
            cont_operator(1:4)=data(41:44)
         elseif(data(1:11).eq.'CONT_BEFORE') then
            read(data(41:50),'(f10.1)') parameter
            if(parameter.gt.0.0) cont_before=parameter
         elseif(data(1:11).eq.'CONT_AFTER') then
            read(data(41:50),'(f10.1)') parameter
            if(parameter.gt.0.0) cont_after=parameter
         elseif(data(1:13).eq.'CONT_INT_S') then
            read(data(41:50),'(f10.1)') parameter
            if(parameter.gt.0.0) cont_int_s=parameter
         endif

c
c   go to next line
c
         goto 10

      endif
c
      call sei close( close$, def_unit, code ) ! Close (Default stop on error).

      return
      end

c ---------------------------------------------------------------------------------------

      subroutine get_bgs_seisan_old(bgs_seisan)
c
c   read in seisan parameters
c
      implicit none
      external sei get file,                 ! Search directories & open file.
     &         sei close,                    ! Close file open.
     &         sei code                      ! Error processor.
      include 'libsei.inc'                   ! Library definitions & data defns
      include 'seisan.inc'
      integer  code                          ! Condition.
      logical  b_eof                         ! End of file?.
c whether to use BGS seisan modifications to output
      logical bgs_seisan
c test variable
      real var
c-- unit for file
      integer def_unit
c-- directory separator char
      character*1 dchar
      character *80 data                     ! one line of data
                                        
c
      call dir_char(dchar)

      bgs_seisan=.false.

c
c   set default parameters in case there is no seisan.def
c
c   open and read default file               
c   ---------------------------
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   def_unit,         ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'SEISAN.DEF' )    ! For this file.
c                                                                               
c   read file if there...
c   ---------------------
c                                                                               
      if(code.ne.e_ok$)  return

 10   continue
c
      read(def_unit,'(a)',iostat=code) data    ! Read from file.
      call sei code( fort$,                    ! Process fortran i/o condition.
     &               code,                     ! Condition.
     &               def_unit,                 ! On unit.
     &               b_eof )                   ! End of file?.
c
      if( .not.b_eof ) then                    ! Not end of file.
c
c   find default wav bases
c
         if(data(1:10).eq.'BGS_SEISAN') then
            read(data(41:50),'(f10.1)') var
            if (var.eq.1.0) then
               bgs_seisan=.true.
            endif
         endif

c
c   go to next line
c
         goto 10

      endif
c
      call sei close( close$, def_unit, code ) ! Close (Default stop on error).

      return
      end



      subroutine PC_TO_TEXT
      return
      end




c
c print version text if argument is '-version'
c
      subroutine print_ver
      implicit none
      include 'version.inc'
      integer nars
      character*80 arg(10)
      integer seiclen
c      character*(*) version

      if (seiclen(out_version_date).le.0) out_version_date=version_date
      call get_arguments(nars,arg) 
      if (nars.eq.1.and.arg(1).eq.'-version') then
        write(*,'(a)') ' '//version_text//
     &      ', '//out_version_date(1:seiclen(out_version_date))
        stop
      endif

      end


      subroutine file_ready(name)
c
c check if file is opened already and wait until it is closed
c
      implicit none
      character*(*)  name
      integer seiclen
      logical opened,exists
      inquire(file=name(1:seiclen(name)),exist=exists)
      if (.not.exists) goto 20
 10   continue
      inquire(file=name,opened=opened)
      if (opened) goto 10
 20   continue
      return
      end


       subroutine remove_letters(text)
c
c  cleans out all non numerical characters to leave numbers,
c  for use with sei get number. leave decimal point
c
c  jh oct 2000
c

       implicit none
       character*80 text
       integer i
       character*1 c

       do i=1,80
          c=text(i:i)
          if(c.ne.'1'.and.c.ne.'2'.and.c.ne.'3'.and.c.ne.'4'.and.
     *       c.ne.'5'.and.c.ne.'6'.and.c.ne.'7'.and.c.ne.'8'.and.
     *       c.ne.'9'.and.c.ne.'0'.and.c.ne.'.') text(i:i)=' '
       enddo
       return
       end


      subroutine exit_with_status(status)
c
c bjb 2001/02/14 set exit codes on unix systems
c in development
c

      implicit none
      integer status
      logical sun,pc,linux

      call computer_type(sun,pc,linux)

      if(pc) then

         stop

      else 

         call exit(status)

      endif


      return
      end

c
c function to find character c in text, starting search from last
c character, lo 21-02-2001
c
      integer function  r_index(text,c)
      implicit none
      character text*(*)
      character*1 c
      integer i,ind
      integer seiclen
      ind = 0
      do i=seiclen(text),1,-1
        if (ind.eq.0.and.text(i:i).eq.c) ind = i
      enddo
      r_index = ind
      end








