/*   Updates                      */
/*   March 95 by jh: font problems, background and forground      */
/*   Dec 96        : xcurs do not send back null char, remian in loop */
/*                   mouse sends a blank like on the pc, was l     */
/*   sep 98         ------------- verison 7.0 check, no change ------*/
/*   nov 5 98 jh    remove all '_' from routine names because of linux */
/*   mar 30 99 bmt: add more subroutine    */
/*   oct 25 01 lo  : set cursor to crosshair */
/*   1st March 2002 FMC : add crosshairs */
/*   7th March 2002 FMC : All drawing now done on pixmap which can be copied
                          into the window when required. This occurs when an
                          expose event occurs or when the crosshairs move.
                          Also window is now replotted when a resize event
                          occurs. */
/*   18th March 2002 FMC : Window only refreshed if it has really been resized.
                           When crosshairs are moved only part of the screen
                           is refreshed.
                           Added new subroutine xtextwin_ to draw text
                           directly to screen and not to pixmap. */
/*   10th April 2003 FMC: Added changes as suggested by F Tilmann:
                          - If 8bit PseudoColor not supported, check for TrueColour
			    and DirectColour before selecting black and white.
			  - Set font name instead of searching for default font.
			    Font name is passed as a parameter to xopen() from
			    the configuration file.
*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

static void draw_xhair(XEvent *motion_event, int last_xhair);
void xtextwin_(char *text, long *n,long *ix, long *iy);

char hello[] = {"Hello, World."};
char hi [] = {"Hi!"};
Display *mydisplay;
Window mywindow;
Window rootw,pointw;
Cursor mycursor;
int rootx,rooty,pointx,pointy, keybut;
int nDrawX,nDrawY;
XColor  col[7];             /* Array of colours */
int nvis;                   /* Number of visuals in list */
XVisualInfo visTmp;         /* Temporary used for checking visual type */
XVisualInfo *visList;       /* list of visuals */
GC mygc;
XFontStruct *font_info;
XEvent myevent;
KeySym mykey;
XSizeHints myhint;
XWindowAttributes winAtt;
int myscreen;
float wscale,xscale,yscale;            /*scaling factor for window*/
unsigned long myforeground, mybackground;
int i;
int ih;                   /*xwindow y size in picels*/
char text[10];
int done;
static int first_xhair;  /* FMC 01-03-2002 global flag for xhair status */
static int last_x, last_y;
Pixmap mypixmap;         /* FMC 07-03-2002 all drawing is now done on pixmap */


/**/
/*   open a new x window    */
/**/
/*xopen_(size,type,background) */
xopen_(size,type,cursortype, fontname)
int *size;       /* input:  window size in pixels */
int *type;       /* output: 1: color screen, 0: black white */
int *cursortype;   /* 0: arrow, 1:cross-hair pointer, 2:cross-hairs */
char *fontname;
{
  int argc;
  int  nxpix,nypix;    /* # pixels on screen */
  char **argv;
  int xsize;
  int last_xhair;         /* set to 1 if this is the last xhair
                              before window is redrawn, else 0 */
  char **fntList;            /* List of font names */
  int fix;                   /* index for accepted font */
  int fntCnt;                /* No. elements in fntList & fntInfolist */
  XFontStruct *fntInfolist;  /* List of font attributes */
  XFontStruct *defFnt;       /* Attributes for default font */
  Font f1id,f2id;            /* Font IDs */
  char *sp;                  /* pointer to first space in font name */

  XColor  dummyCol;   /* Just a dummy variable */

  *type=0;  /* default is bw */
  mydisplay = XOpenDisplay ("");
  myscreen  = DefaultScreen (mydisplay);
  mybackground = WhitePixel (mydisplay, myscreen);
  myforeground = BlackPixel (mydisplay, myscreen);

/* --------- GET ROOT WINDOW SIZE ------vvvv--------- */

  nxpix = DisplayWidth(mydisplay,myscreen);
  nypix = DisplayHeight(mydisplay,myscreen);

/* --------- GET ROOT WINDOW SIZE ------^^^^--------- */

/* scale window so it still fits with tektronics, calculate scaling factor*/
/* to be used in all subsequent operations*/

  *size = 90;

  xsize=((*size)*nxpix)/100 ;
  wscale=xsize/1024.0;
  myhint.x      = 1;
  myhint.y      = 1;
  myhint.width  = 1024*wscale;
  myhint.height = 780*wscale;
  xscale=wscale;                 /*initially same scale for x and y */
  yscale=wscale;
  ih=myhint.height;
  nDrawY = ih;
  nDrawX = myhint.width;
  myhint.flags  = PPosition | PSize;


/* Try to get RGB colours from default colour map if color screen: */
/* Checking display type (if 8bit PseudoColor not supported, check TrueColour
   and DirectColour, and if they are no good, use black&white) */
  visTmp.depth = 8;
  visTmp.screen = myscreen;
  visTmp.class = PseudoColor;
  visList = XGetVisualInfo (mydisplay, VisualScreenMask | VisualDepthMask,
                            &visTmp, &nvis);
/*     printf("First nvis %d\n",nvis); */
  if (nvis>0)
  {
    *type=1;    /* indicate a color screen */
  }
  else
  {
    /* Try direct colour */
    visTmp.class = DirectColor ;
    visTmp.class = DirectColor ;
    visList = XGetVisualInfo (mydisplay, VisualScreenMask,
	                      &visTmp, &nvis);
/*       printf("Second nvis %d\n",nvis); */
    if (nvis>0)
    {
      *type=1;    /* indicate a color screen */
    }
    else
    {
      /* Try true colour */
      visTmp.class = DirectColor ;
      visTmp.class = DirectColor ;
      visList = XGetVisualInfo (mydisplay, VisualScreenMask,
				&visTmp, &nvis);
/* 	  printf("Third nvis %d\n",nvis); */
      if (nvis>0)
      {
	*type=1;    /* indicate a color screen */
      } 
    }
  }

  if (*type==1)
  {
    if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "red",
                          &(col[3]),&dummyCol))
    {
      printf ("--colour-- ERROR: Unable to get red colour\n");
      exit(-1);
    }

    if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "green",
                          &(col[2]),&dummyCol))
    {
      printf ("--colour-- ERROR: Unable to get green colour\n");
      exit(-1);
    }

    if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "blue",
                          &(col[1]),&dummyCol))
    {
      printf ("--colour-- ERROR: Unable to get blue colour\n");
      exit(-1);
    }

    if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "yellow",
                          &(col[4]),&dummyCol))
    {
      printf ("--colour-- ERROR: Unable to get red colour\n");
      exit(-1);
    }
  }

  if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "white",
                        &(col[5]),&dummyCol))
  {
    printf ("--colour-- ERROR: Unable to get white colour\n");
    exit(-1);
  }

  if (!XAllocNamedColor(mydisplay, DefaultColormap(mydisplay,myscreen), "black",
                        &(col[6]),&dummyCol))
  {
    printf ("--colour-- ERROR: Unable to get black colour\n");
    exit(-1);
  }

  first_xhair = 1;          /* FMC 18-03-2002 initialise crosshair status */
  last_x = last_y = -1;     /* initialise crosshair position */

  /* set up window */
  mywindow = XCreateSimpleWindow (mydisplay, DefaultRootWindow (mydisplay),
                                  myhint.x, myhint.y,
                                  myhint.width, myhint.height, 5,
                                  myforeground, mybackground);

  mygc = XCreateGC (mydisplay, mywindow, 0, 0);

  /* FMC 07-03-2002 create a pixmap the same size as the screen as this
     is the maximum size that the window can take. */
  mypixmap = XCreatePixmap(mydisplay, mywindow, nxpix, nypix,
                           DefaultDepthOfScreen(XDefaultScreenOfDisplay(mydisplay)));

  /* initialise pixmap to background colour */
  XSetForeground(mydisplay, mygc, WhitePixel(mydisplay, myscreen));
  XFillRectangle(mydisplay, mypixmap, mygc, 0, 0, nxpix, nypix);
  XSetForeground(mydisplay, mygc, BlackPixel(mydisplay, myscreen));

  /* FMC 18-03-2002 get real size of window */
  XGetWindowAttributes(mydisplay,mywindow,&winAtt);

/**/

/* --------- FONTS ---------------------vvvv--------- */

  /* FMC 11-04-2003 Allow a font name to be passed from the
     configuration file. If no font is found, search for default. */
  if (fontname)
  {
    /* fontname should not contain any spaces so remove trailing
       blanks by searching for first space */
    sp = strchr (fontname, ' ');
    *sp = '\0';

     /* search fonts for the font name which has been passed in */
     fntList = XListFontsWithInfo (mydisplay, fontname,
                                   1,&fntCnt,&fntInfolist);
  }
  
  /* either font search was unsuccessful or no font name given */
  if (!fntList || !fontname)
  {
    /* generate list of fonts */
    fntList = XListFontsWithInfo (mydisplay,"-*-*-*-r-*-*-*-*-*-*-m-*-iso8859-*",
                                  1,&fntCnt,&fntInfolist);
  }

  if (fntList)
  {
    /* check that a font has been found */
    f1id = XLoadFont (mydisplay,*(fntList));
    /* destroy list of fonts */
    XFreeFontInfo (fntList,fntInfolist,fntCnt);
  }
  else
  {
    printf ("--fonts-- ERROR: Unable to get font\n");
    exit(-1);
  }

  XSetFont (mydisplay,mygc,f1id);

/*  fntList = XListFontsWithInfo (mydisplay,"-*-*-*-r-*-*-20-*-*-*-m-*-iso8859-*"
               ,1,&fntCnt,&fntInfolist);
  if (fntList) {
    f2id = XLoadFont (mydisplay,*(fntList));
    XFreeFontInfo (fntList,fntInfolist,fntCnt);
  }
  else {
   printf ("--fonts-- ERROR: Unable to get large font\n");
   exit(-1);

  } */
/* --------- FONTS ---------------------^^^^--------- */


  XSetBackground (mydisplay, mygc, mybackground);
  XSetForeground (mydisplay, mygc, myforeground);

  XSelectInput (mydisplay, mywindow,
                ButtonPressMask |     /* mouse click event */
                KeyPressMask |        /* keyboard event */
                ExposureMask |        /* exposure event */
                StructureNotifyMask); /* resize event */

  XMapRaised (mydisplay, mywindow);

/* setup cursor, lo 31 oct 01 */
  if(*cursortype>0)
  {
    /* cursortype=1, cursortype=2 */
    mycursor= XCreateFontCursor(mydisplay, XC_crosshair);
    XDefineCursor(mydisplay,mywindow,mycursor);

    if(*cursortype==2)
    {
      /* FMC 01-03-2002 if crosshairs are required, set mask
         to recognise mouse motion */
      XSelectInput (mydisplay, mywindow,
                    ButtonPressMask |     /* mouse click event */
                    KeyPressMask |        /* keyboard event */
                    ExposureMask |        /* exposure event */
                    StructureNotifyMask | /* resize event */
                    PointerMotionMask |   /* mouse motion event */
                    LeaveWindowMask);     /* leave window event */
    }
  }


/* FMC 07-03-2002 remove this events loop as it doesn't seem to do anything
                  and exits after first time through loop */
/*   done = 0; */
/*   while (done==0) */
/*   { */
/*     done=1; */
/*     XNextEvent (mydisplay, &myevent); */
/*     switch (myevent.type) */
/*     { */
/*     case Expose: */
/*       last_xhair = 1; */
/*       draw_xhair(&myevent,last_xhair); */
/*       if (myevent.xexpose.count ==0) */
/*       { */
/*         XDrawImageString (myevent.xexpose.display, myevent.xexpose.window,*/
/*                           mygc, 50, 50, hello, strlen(hello) ); */
/*         done=0; */
/*         break; */
/*       } */
/*     case MappingNotify: */
/*       last_xhair = 1; */
/*       draw_xhair(&myevent, last_xhair); */
/*       XRefreshKeyboardMapping (&myevent); */
/*       break; */
/*     case ButtonPress: */
/*       last_xhair = 1; */
/*       draw_xhair(&myevent, last_xhair); */
/*       XDrawImageString (myevent.xbutton.display, myevent.xbutton.window, */
/*                         mygc, myevent.xbutton.x, myevent.xbutton.y, hi, */
/*                         strlen(hi) ); */
/*       XDrawLine (myevent.xexpose.display, myevent.xexpose.window, mygc, */
/*                  myevent.xbutton.x, myevent.xbutton.y, */
/*                  myevent.xbutton.x, myevent.xbutton.y+20); */
/*       break; */
/*     case KeyPress: */
/*       last_xhair = 1; */
/*       draw_xhair(&myevent, last_xhair); */
/*       i = XLookupString (&myevent, text, 10, &mykey, 0); */
/*       XDrawImageString (myevent.xbutton.display, myevent.xbutton.window, */
/*                         mygc, myevent.xbutton.x, myevent.xbutton.y,text, */
/*                         strlen(text) ); */
/*       if (i==1 && text[0]=='q') done = 1; */
/*       break; */
/*     case MotionNotify: */
/*       last_xhair = 0; */
/*       draw_xhair(&myevent, last_xhair); */
/*       break; */
/*     } */  /* switch */
/*   } */  /* while */
}


xclose_()
{
  XFreeGC (mydisplay, mygc);
  XDestroyWindow (mydisplay, mywindow);
  XFreePixmap(mydisplay,mypixmap);       /* FMC 07-03-2002 */
  XCloseDisplay (mydisplay);
  /*exit(0);*/
}


/* FMC 07-03-2002 remove this subroutine as it doesn't seem to be called */
/* xmore_() */
/* { */
/*   done = 0; */
/*   while (done==0)  */
/*   { */
/*     XNextEvent (mydisplay, &myevent); */
/*     switch (myevent.type)  */
/*     { */
/*     case Expose: */
/*       if (myevent.xexpose.count ==0) */
/*         XDrawImageString (myevent.xexpose.display, myevent.xexpose.window, */
/*                           mygc, 50, 50, hello, strlen(hello) ); */
/*       break; */
/*     case MappingNotify: */
/*       XRefreshKeyboardMapping (&myevent); */
/*       break; */
/*     case ButtonPress: */
/*       XDrawImageString (myevent.xbutton.display, myevent.xbutton.window, */
/*                         mygc, myevent.xbutton.x, myevent.xbutton.y+50, hi, */
/*                         strlen(hi) ); */
/*       XDrawLine (myevent.xexpose.display, myevent.xexpose.window, mygc, */
/*                  myevent.xbutton.x, myevent.xbutton.y, */
/*                  myevent.xbutton.x, myevent.xbutton.y+20); */
/*       break; */
/*     case KeyPress: */
/*       i = XLookupString (&myevent, text, 10, &mykey, 0); */
/*       if (i==1 && text[0]=='q') done = 1; */
/*       break; */
/*     }  */ /* switch */
/*   }  */ /* while */

/* } */


xline_()
{
/*   XDrawLine (mydisplay, mywindow, mygc, 20,20,50,50);  FMC 03-07-2002 */
  XDrawLine (mydisplay, mypixmap, mygc, 20,20,50,50);
}


/***********************************************************************/
/* move cursor to ix,iy, units are tectronics*/

void xmoveto_(ix,iy)
long *ix;
long *iy;
{
  short int x,y;

  x=(*ix)*xscale;
  y=ih-*iy*yscale;

  /* XDrawLine (mydisplay, mywindow, mygc,x,y,x,y); FMC 07-03-2002
     XClearArea (mydisplay, mywindow,x,y,1,1,False); */
  XDrawLine (mydisplay, mypixmap, mygc,x,y,x,y);
}


/************************************************************************/
/* draw a line between ix1,iy1 and ix2, iy2, units are tectronics*/
void xlineto_(ix1,iy1,ix2,iy2)
long *ix1,*ix2;
long *iy1,*iy2;
{
  short int x1,y1,x2,y2;
  char str[20];

  x1=*ix1*xscale;
  x2=*ix2*xscale;
  y1=ih-*iy1*yscale;
  y2=ih-*iy2*yscale;

  /* XDrawLine (mydisplay, mywindow, mygc,x1,y1,x2,y2); FMC 07-03-2002 */
  XDrawLine (mydisplay, mypixmap, mygc,x1,y1,x2,y2);
}

/***********************************************************************/
/* plot a text string text with n characters starting at position ix,iy  */
void xtext_(text,n,ix,iy)
long *n,*ix,*iy;
char *text;
{
  short int x,y;

  x=*ix*xscale;
  y=ih-*iy*yscale;

/*   XDrawString (mydisplay, mywindow, mygc,x,y,text,*n); FMC 07-03-2002 */
  XDrawString (mydisplay, mypixmap,mygc,x,y,text,*n);
}


/***********************************************************************/
/* FMC 18-03-2002 new subroutine which draws text directly on screen   */
/* instead of drawing to pixmap. This is to be used to print a message */
/* when data is loading and screen would otherwise be blank. Once data */
/* has been plotted this is copied onto the screen and text from this  */
/* subroutine is overwritten.                                          */

void xtextwin_(text,n,ix,iy)
long *n,*ix,*iy;
char *text;
{
  short int x,y;

  XSetForeground(mydisplay, mygc, BlackPixel(mydisplay, myscreen));

  x=*ix*xscale;
  y=ih-*iy*yscale;

  XDrawString (mydisplay, mywindow,mygc,x,y,text,*n);
}


/**********************************************************************/
/* graphic cursor  */
void xcursr_(cha,ix,iy)
long *ix,*iy;
char *cha;
{
  int i;
  int last_xhair;

  XWindowAttributes attrib;

/* FMC 07-03-2002 this routine is called when window is waiting
   for input, update contents of window to include any drawing
   etc. that has taken place since the last window update */

  XCopyArea(mydisplay, mypixmap, mywindow, mygc, 0, 0,
            winAtt.width, winAtt.height, 0, 0);
  do
  {
    *cha=0;
    XNextEvent (mydisplay, &myevent);
    switch (myevent.type)
    {
    case Expose:
      /* FMC 07-03-2002 part of the window has been exposed, refresh
         by copying pixmap contents into window */
      XCopyArea(mydisplay, mypixmap, mywindow, mygc, 0, 0,
                winAtt.width, winAtt.height, 0, 0);
      break;

    case ConfigureNotify:
      /* FMC 07-03-2002 resize event. Get new size of window and
         calculate scaling factors. Redraw window contents */
      /* FMC 18-03-2002 check old size of window.
         If window has not really been resized, ignore this event */

      XGetWindowAttributes(mydisplay,mywindow,&attrib);
      if((attrib.height == winAtt.height)&&(attrib.width == winAtt.width))
        break;

      XGetWindowAttributes(mydisplay,mywindow,&winAtt);
      ih=winAtt.height;
      xscale=winAtt.width/1024.0;
      yscale=winAtt.height/780.0;
      strcpy(cha,"#");       /* send back flag to replot */
      *ix=myevent.xbutton.x/xscale;
      *iy=(ih-myevent.xbutton.y)/yscale;
      return;

    case ButtonPress:
      last_xhair = 1;
      draw_xhair(&myevent, last_xhair);
      strcpy(cha," ");       /* mouse press send a blank */
      *ix=myevent.xbutton.x/xscale;
      *iy=(ih-myevent.xbutton.y)/yscale;
      return;

    case KeyPress:
      last_xhair = 1;
      draw_xhair(&myevent, last_xhair);
      i = XLookupString (&myevent, cha, 10, &mykey, 0);
      if(strncmp(cha,"\0",1)==0) break;
      *ix=myevent.xbutton.x/xscale;
      *iy=(ih-myevent.xbutton.y)/yscale;
      return;

    case MotionNotify:
      last_xhair = 0;
      draw_xhair(&myevent, last_xhair);
      break;

    case LeaveNotify:
      /* remove last xhair */
      last_xhair = 1;
      draw_xhair(&myevent, last_xhair);
      break;
    }
  }while(1);
}


/**********************************************************************/
/*  clear screen    */
void xclear_()
{
  /* FMC 18-03-2002 clear pixmap by drawing rectangle of background
     colour and copy onto screen */
  XSetForeground(mydisplay, mygc, WhitePixel(mydisplay, myscreen));
  XFillRectangle(mydisplay, mypixmap, mygc, 0, 0,
                 DisplayWidth(mydisplay,myscreen),
                 DisplayHeight(mydisplay,myscreen));
  XSetForeground(mydisplay, mygc, BlackPixel(mydisplay, myscreen));
  XCopyArea(mydisplay, mypixmap, mywindow, mygc, 0, 0,
            winAtt.width, winAtt.height, 0, 0);
}


/**/
void updatewindowsize_()
{
  XGetWindowAttributes(mydisplay,mywindow,&winAtt);

  ih=winAtt.height;
  xscale=winAtt.width/1024.0;
  yscale=winAtt.height/780.0;
}


void  promptx (mDSP,mGC,mWIN,fg,bg,xmin,ymin,xlen,ylen,prompt,ans,nans)
/* ----------------------------------------------------------------------- */
/* A quick hack to read a string from the keyboard in an existing X11      */
/* application                                                             */
/*                                                                         */
/* Compile with '-DFTNSTRING', or uncomment the '#define   FTNSTRING   1'  */
/* line below to strip trailing blanks in FORTRAN strings.                 */
/*                                                                         */
/*                                                                         */
/*  NOTE: - The routine wipes out all graphics in the answer area !        */
/*        - *NO* checks are performed on the input parameters !            */
/*                                                                         */
/*                                                                         */
/* 940905as: Initial version.                                              */
/* 940906as: Now possible to strip trailing blanks (FORTRAN strings).      */
/*                                                                         */


#define   FTNSTRING   1  /* Uncomment to strip trailing blanks in *prompt */

/* Input parameters: */
  Display  *mDSP;       /* Display           */
  Window   mWIN;        /* Main window       */
  GC       mGC;         /* Graphics context  */
  unsigned long fg;     /* foreground colour */
  unsigned long bg;     /* background colour */
  int      xmin;        /* X coordinate for upper left corer of answer area */
  int      ymin;        /* Y coordinate for upper left corer of answer area */
  int      xlen;        /* Width of answer area */
  int      ylen;        /* Height of answer area */
  char     *prompt;     /* Prompt string     */
  int      nans;        /* Max number of characters in answer string */


/* Output parameters: */
  char  *ans;           /* answer string     */


{


/* Variables: */
  Window   pWIN;       /* Main window       */
  XEvent pEV;          /* X event */
  KeySym pKEY;         /* Keysym */
  int pSCR;            /* Screen */
  char text[10];
  char scrSTR[250];    
  char tmpSTR[250];    
  int done;
  int i,tmp;

#ifdef FTNSTRING
  for (i=strlen(prompt)-1;i>=0;i--)    
    if(!isspace(prompt[i]))
      break;
  tmpSTR[i+1]='\0';
  for ( ; i>=0; i--)
    tmpSTR[i]=prompt[i];
#define PROMPTSTRING tmpSTR
#else
#define PROMPTSTRING prompt
#endif

  pWIN = XCreateSimpleWindow(mDSP,mWIN,xmin,ymin,xlen,ylen,2,fg,bg);
  XSelectInput (mDSP,pWIN,  KeyPressMask | ExposureMask);
  XMapRaised (mDSP,pWIN);
  XFlush (mDSP);

  ans[0] = 0;
  sprintf (scrSTR,"%s %s",PROMPTSTRING,ans);
  
  done = 0;
  while (done==0) 
  {
    XNextEvent (mDSP, &pEV);
    switch (pEV.type) 
    {
    case Expose:
      if (pEV.xexpose.count ==0) 
      {
        XSetForeground (mDSP, mGC, fg);
        XSetBackground (mDSP, mGC, bg);
        XDrawImageString (pEV.xexpose.display, pEV.xexpose.window,
                          mGC,10,20,scrSTR,strlen(scrSTR));
        XFlush (mDSP);
      }
      break;
    case KeyPress:
      i = XLookupString (&pEV, text, 10, &pKEY, 0);
      if (i=1) 
      {
        if (text[0]==13)    /* RETURN */
          done = 1;
        else
        {
          if ((text[0]==8 || text[0]==127) && strlen(ans)>0)
          {  /* DEL/BS */
            ans[strlen(ans)-1] = 0;
            XClearWindow (mDSP,pWIN);
          }
          else if (text[0]>31)
          {  /* only accepts printable characters */ 
            tmp = strlen(ans);
            if (tmp>=nans)
            {      /* not room for more characters */
              XFillRectangle (mDSP,pWIN,mGC,0,0,xlen,ylen);
              XSetForeground (mDSP, mGC, bg);
              XSetBackground (mDSP, mGC, fg);
              XDrawImageString (mDSP, pWIN,mGC,10,20
                                ,"Not room for more characters",28);
              XFlush (mDSP);
              sleep(2);
              XSetForeground (mDSP, mGC, fg);
              XSetBackground (mDSP, mGC, bg);
              XClearWindow (mDSP,pWIN);
              XDrawImageString (mDSP,pWIN,mGC,10,20,scrSTR,strlen(scrSTR));
            }
            else 
            {
              ans[tmp] = text[0];
              ans[tmp+1] = 0;
            }
          }
          sprintf (scrSTR,"%s %s",PROMPTSTRING,ans);
          XDrawImageString (mDSP, pWIN,mGC,10,20,scrSTR,strlen(scrSTR));
          XFlush (mDSP);
          break;
        }  
      }  
    } 
  }  

  XUnmapWindow (mDSP,pWIN);
  for(done=strlen(ans);done<nans;done++)
    ans[done]=' ';
}


void oneline1_(question,nq,answer,na,x0,y0)
/* question: prompt question                               */
/* nq:       number of characters in question              */
/* naswer:   answer                                        */
/* na        number of characters in anaswe                */
/* x0,y0     posion of string                              */
   float *x0;
   float *y0;
   int *na;
   int *nq;
   char *answer;
   char *question;
{
  int xlen;
  int  ylen;
  int ix;
  int iy;

  ix=*x0;
  iy=*y0;
  xlen=(*nq+*na)*8;
  ylen=30;
  ix=ix*xscale;
  iy=ih-iy*yscale;
  printf("%d %d %d %d\n",ix,iy,xlen,ylen);
  promptx (mydisplay,mygc,mywindow,myforeground,mybackground,ix,iy,xlen,ylen,question,answer,*na);
}


void xgetabsposition_()
{
  /* Get the current abs cursor position: */
  if (!XQueryPointer(mydisplay, DefaultRootWindow (mydisplay), &rootw, &pointw,
                     &rootx,&rooty,&pointx,&pointy,&keybut) )
    exit (-1);
}


void xmovetoalpha_()
{
 /* Move the cursor back to where is was found: */
  XWarpPointer(mydisplay, None, DefaultRootWindow(mydisplay), None, None, 0, 0,
               rootx, rooty);
  XFlush(mydisplay);
}


void xmovetographics_()
{
  XWarpPointer(mydisplay, None , mywindow, 0, 0, 0, 0, 200, 100);
  XFlush (mydisplay);
}


/*    set color      */
void setcolorx_(coll)
int *coll;
{
  XSetForeground (mydisplay, mygc, col[*coll].pixel);
}


/*    set background color      */
/*    fill rectangel  with current color, x1,y1,x2,y2 are corners      */
void setbackx_(coll)
int *coll;
{
/*   xclear_; FMC 07-03-2002*/
/*   XSetWindowBackground (mydisplay, mywindow, col[*coll].pixel); */
  XSetBackground (mydisplay, mygc, col[*coll].pixel);
  xclear_;
}


void clearwindow_(x1,y1,x2,y2)
long  *x1,*y1,*x2,*y2;
{
  int c;
  float fx1,fx2,fy1,fy2;

  c = 5;
  setcolorx_(&c);
  fx1 = *x1;
  fx2 = *x2;
  fy1 = *y1;
  fy2 = *y2; 
  fillbox_(&fx1,&fy1,&fx2,&fy2); 
  c = 6;
  setcolorx_(&c);
}


int  fillbox_(x1,y1,x2,y2)
float *x1,*y1,*x2,*y2;
{
  int xlen,ylen,x0,y0;

  x0=(*x1)*xscale;
  y0=ih-(*y2)*yscale;
  xlen=((*x2)-(*x1))*xscale;
  ylen=((*y2)-(*y1))*yscale;

  /* XFillRectangle(mydisplay,mywindow,mygc,x0,y0,xlen,ylen); FMC 07-03-2002 */
  XFillRectangle (mydisplay,mypixmap,mygc,x0,y0,xlen,ylen);
}


void getnxypix_(x,y)
int *x,*y;
{
  *x = nDrawX;
  *y = nDrawY;
}


void fillcircle_(x1,y1,r)
long  *x1,*y1,*r;
{
  int xlen,ylen,x0,y0;

  x0=((*x1)-(*r))*xscale;
  y0=ih-((*y1)+(*r))*yscale;
  xlen=(*r)*xscale;
  ylen=(*r)*yscale;

  /* XFillArc(mydisplay,mywindow,mygc,x0,y0,2*xlen,2*ylen,0,360*64); FMC 07-03-2002 */
  XFillArc(mydisplay,mypixmap,mygc,x0,y0,2*xlen,2*ylen,0,360*64);
}


void circle_(x1,y1,r)
long  *x1,*y1,*r;
{
 int xlen,ylen,x0,y0;

  x0=((*x1)-(*r))*xscale;
  y0=ih-((*y1)+(*r))*yscale;
  xlen=(*r)*xscale;
  ylen=(*r)*yscale;

  /* XDrawArc(mydisplay,mywindow,mygc,x0,y0,2*xlen,2*ylen,0,360*64); FMC 07-03-2002 */
  XDrawArc(mydisplay,mypixmap,mygc,x0,y0,2*xlen,2*ylen,0,360*64);
}


void drawbox_(ix1,iy1,ix2,iy2)
long *ix1,*iy1;
long *ix2,*iy2;
{
  short int x1,y1,x2,y2;

  x1=*ix1*xscale;
  x2=*ix2*xscale;
  y1=ih-*iy1*yscale;
  y2=ih-*iy2*yscale;

/*  XDrawLine (mydisplay, mywindow, mygc,x1,y1,x2,y1);
    XDrawLine (mydisplay, mywindow, mygc,x2,y1,x2,y2);
    XDrawLine (mydisplay, mywindow, mygc,x1,y1,x1,y2);
    XDrawLine (mydisplay, mywindow, mygc,x1,y2,x2,y2); FMC 07-03-2002 */
  XDrawLine (mydisplay, mypixmap, mygc,x1,y1,x2,y1);
  XDrawLine (mydisplay, mypixmap, mygc,x2,y1,x2,y2);
  XDrawLine (mydisplay, mypixmap, mygc,x1,y1,x1,y2);
  XDrawLine (mydisplay, mypixmap, mygc,x1,y2,x2,y2);
}


/******************************************************************************
 * draw_xhair FMC 1st March 2002
 *            FMC 7th March 2002 changed to copy window onto pixmap,
 *                               draw crosshairs on window and remove old
 *                               crosshairs by copying pixmap onto window
 *
 * Description: called when mouse motion is detected in the window. The first
 *              time this routine is called the current window is copied onto
 *              the pixmap. Crosshairs are drawn on the window and not on the
 *              pixmap. Each subsequent time this routine is called the pixmap
 *              is copied back to the screen which removes the previous
 *              crosshairs.
 *
 * Input parameters: motion_event - the event which has triggered the callback
 *                   last_xhair - if this is the last xhair before the screen
 *                                is redrawn: 1 = yes, 0 = no
 *
 * Output parameters: the global variable first_xhair is set to 0 after first
 *                    call to this routine
 *
 * Returns: void
 *
 * Comments: if last_xhair is 1, the window is restored but a new set of
 *           crosshairs are not drawn.
 *
******************************************************************************/


void draw_xhair(XEvent *motion_event, int last_xhair)
{
  int x, y;

  /* get current mouse position */
  x = motion_event->xmotion.x;
  y = motion_event->xmotion.y;

  /* ensure crosshairs will be drawn in black */
  XSetForeground(mydisplay, mygc, BlackPixel(mydisplay, myscreen));

  if(!first_xhair)
  {
    /* restore positions where previous crosshairs were */
    XCopyArea(mydisplay, mypixmap, mywindow, mygc,
              last_x, 0, 1, winAtt.height, last_x, 0);
    XCopyArea(mydisplay, mypixmap, mywindow, mygc,
              0, last_y, winAtt.width, 1, 0, last_y);
  }
  if(last_xhair)
  {
    /* reset flags to starting values */
    first_xhair = 1;
    last_x = last_y = -1;
  }
  else
  {
    /* draw a new xhair in current mouse pos */
    XDrawLine(mydisplay, mywindow, mygc, 0, y, winAtt.width, y);
    XDrawLine(mydisplay, mywindow, mygc, x, 0, x, winAtt.height);
    first_xhair = 0;

    last_x = x;
    last_y = y;
  }
  return;
}
