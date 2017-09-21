#include <string.h>
/* #include <iostream.h>
#include <fstream.h> */
/* xview header files */

#define OWTOOLKIT_WARNING_DISABLED
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <xview/cms.h>
#include <xview/scrollbar.h>
#include <xview/xv_xrect.h> 
#include "arrows.h" 

/* X11 header files */
#include <X11/Xlib.h>

Frame frame,subframe;
Window subwindow;
Panel panel;
GC mygc;
Xv_singlecolor fg,bg;
Scrollbar h_scrollbar,v_scrollbar;
Canvas canvas;
Window mywindow;
Display *mydisplay;
Xv_xrectlist *xrects;
Xv_Window paint_window;
int x_max,y_max;                       /* max canvas size          */
char text[241];
char text2;
float wscale,xscale,yscale;            /*scaling factor for window */
int frame_xsize,frame_ysize;
int ih;
Panel_item panel_text;
Panel_item panel_text1;
Panel_item panel_text2;

/******** open xview window ********/
void 
xview_open_frame_(title,size,xx,yy)
  char *title;   /* frame title */
  int *size;     /* relative size of maximum pixels of display */
/* added max canvas size, lo 25.04.2001 */
  int *xx,*yy;   /* pass canvas size back to Fortran */
{
  int nxpix,nypix;
  int myscreen;
  mydisplay = XOpenDisplay ("");
  myscreen  = DefaultScreen (mydisplay);

  nxpix = DisplayWidth(mydisplay,myscreen);
  nypix = DisplayHeight(mydisplay,myscreen);
  frame_xsize=((*size)*nxpix)/100;
  frame_ysize=((*size)*nypix)/100-70;

/*  x_max=2000;    Canvas size
  y_max=1000; */

/* get max canvas size, lo 25.04.2001 */
  x_max=frame_xsize-100;    /* Canvas size */
  y_max=frame_ysize-150; 
  *xx=x_max;
  *yy=y_max;

  xscale=1.;
  yscale=1.;
  ih=y_max;

  xv_init(NULL);  
  frame=(Frame)xv_create(NULL,FRAME,
                         FRAME_INHERIT_COLORS,TRUE,
                         XV_WIDTH,frame_xsize,
                         XV_HEIGHT,75,
                         FRAME_LABEL,title,
                         FRAME_NO_CONFIRM,TRUE,
                         NULL);
  mydisplay=(Display *)xv_get(frame,XV_DISPLAY);
  mygc=DefaultGC(mydisplay,DefaultScreen(mydisplay)); 
}

/******** setup cplot menu ********/
void
xview_menu_cplot_(text)
{
  void quit();
  void set_start();
  void set_start_time();
  void set_interval();
  void forwardf();
  void forwardF();
  void backwardb();
  void backwardB();
  void zoom_a();
  void get_start_time_();
  void read_plot_bases_();
  void replot_();
  void cdouble();
  void write_cont_wavfile_();
  void my_repaint();
  void filter_menu_proc();
  Menu filter_menu;
  void zoom_menu_proc();
  Menu zoom_menu;
  Server_image smallright;
  Server_image smallleft;
  Server_image largeright;
  Server_image largeleft;
/******** setup panel ********/
  panel=(Panel)xv_create (frame,PANEL,NULL);
  (void) xv_create (panel,PANEL_BUTTON,
       PANEL_LABEL_X,5,
       PANEL_LABEL_Y,5,
       XV_HEIGHT,335,
       PANEL_LABEL_STRING,"Quit",
       PANEL_NOTIFY_PROC,quit, 
       NULL);
  (void) xv_create (panel,PANEL_BUTTON,
       PANEL_LABEL_STRING,"File",
       PANEL_NOTIFY_PROC,write_cont_wavfile_, 
       NULL); 
  panel_text1=xv_create(panel,PANEL_TEXT, 
               PANEL_LABEL_STRING,"Time Interval (min): ",
	       PANEL_VALUE_DISPLAY_LENGTH,3,
               PANEL_VALUE,"15",
               PANEL_NOTIFY_PROC,set_interval,
               NULL);      
  panel_text=xv_create(panel,PANEL_TEXT,
               PANEL_LABEL_STRING,"Start Time: ",
	       PANEL_VALUE_DISPLAY_LENGTH,12,
               PANEL_VALUE,"200109161300",
               PANEL_NOTIFY_PROC,set_start_time,
               NULL); 
  (void) xv_create (panel,PANEL_BUTTON,
       PANEL_NEXT_ROW, -1,  
       PANEL_LABEL_STRING,"Replot",
       PANEL_NOTIFY_PROC,replot_,
       NULL); 
  (void) xv_create (panel,PANEL_BUTTON,
       PANEL_LABEL_STRING,"Double",
       PANEL_NOTIFY_PROC,cdouble,
       NULL);
  filter_menu=(Menu)xv_create(NULL,MENU,
       MENU_NOTIFY_PROC, filter_menu_proc,
       MENU_STRINGS," 0.01-0.1","  0.1-1.0","  0.7-3.0","  1.0-5.0",
		    "  2.0-4.0","  5.0-10.0"," 10.0-15.0"," 15.0-23.0",
		    " no filter",  
       NULL);
  (void) xv_create (panel,PANEL_BUTTON,
       PANEL_LABEL_STRING,"Filter",
       PANEL_ITEM_MENU,filter_menu,
       NULL); 
  zoom_menu=(Menu)xv_create(NULL,MENU,
       MENU_NOTIFY_PROC, zoom_menu_proc,
       MENU_STRINGS," 10%"," 25%"," 50%","150%","200%",
       NULL);
  (void) xv_create (panel,PANEL_BUTTON,
       PANEL_LABEL_STRING,"Zoom",
       PANEL_ITEM_MENU,zoom_menu,
       NULL); 
  panel_text2=xv_create(panel,PANEL_TEXT, 
               PANEL_LABEL_STRING,"Zoom (min): ",
	       PANEL_VALUE_DISPLAY_LENGTH,3,
               PANEL_VALUE,"10",
               PANEL_NOTIFY_PROC,zoom_a,
               NULL);      
/******** start canvas ********/
  xv_set(frame,XV_HEIGHT,frame_ysize,NULL); 
  canvas =  xv_create(frame,CANVAS,
       CANVAS_REPAINT_PROC,my_repaint,
       CANVAS_X_PAINT_WINDOW,TRUE,
       CANVAS_HEIGHT,y_max,
       CANVAS_WIDTH,x_max,
       CANVAS_AUTO_EXPAND,FALSE,
       CANVAS_AUTO_SHRINK,FALSE,
       WIN_RETAINED,FALSE,
       NULL);

  mywindow=(Window)xv_get(canvas_paint_window(canvas),XV_XID);

/******** large left arrow ********/
    largeleft = (Server_image)xv_create((Xv_object) NULL, SERVER_IMAGE,
		                       XV_WIDTH,           64,
        			       XV_HEIGHT,          24,
	        		       SERVER_IMAGE_DEPTH, 1,
				       SERVER_IMAGE_BITS,  lleft_bits,
	        		       NULL);
    xv_create(panel, PANEL_BUTTON,
/*            PANEL_NEXT_ROW, -1,   */
              PANEL_LABEL_IMAGE,  largeleft,
              PANEL_NOTIFY_PROC,backwardB,
              NULL);

/******** small left arrow ********/
    smallleft = (Server_image)xv_create((Xv_object) NULL, SERVER_IMAGE,
				  XV_WIDTH,           24,
			          XV_HEIGHT,          24,
				  SERVER_IMAGE_DEPTH, 1,
				  SERVER_IMAGE_BITS,  sleft_bits,
			          NULL);
    xv_create(panel, PANEL_BUTTON,
	      PANEL_LABEL_IMAGE,  smallleft,
              PANEL_NOTIFY_PROC,backwardb,
              NULL);

/******** small right arrow ********/
    smallright = (Server_image)xv_create((Xv_object) NULL, SERVER_IMAGE,
			          XV_WIDTH,           24,
			          XV_HEIGHT,          24,
			          SERVER_IMAGE_DEPTH, 1,
			          SERVER_IMAGE_BITS,  sright_bits,
                                  NULL);
    xv_create(panel, PANEL_BUTTON,
	      PANEL_LABEL_IMAGE,  smallright,
              PANEL_NOTIFY_PROC,forwardf,
	      NULL);

/******** large right arrow ********/
    largeright = (Server_image)xv_create((Xv_object) NULL, SERVER_IMAGE,
				  XV_WIDTH,           64,
				  XV_HEIGHT,          24,
				  SERVER_IMAGE_DEPTH, 1,
				  SERVER_IMAGE_BITS,  lright_bits,
				  NULL);
    xv_create(panel, PANEL_BUTTON,
              PANEL_LABEL_IMAGE,  largeright,
              PANEL_NOTIFY_PROC,forwardF,
    	      NULL);


/******** setup scrollbar ********/
  h_scrollbar=(Scrollbar)xv_create(canvas,SCROLLBAR,
       SCROLLBAR_DIRECTION,SCROLLBAR_HORIZONTAL,
       NULL);
  v_scrollbar=(Scrollbar)xv_create(canvas,SCROLLBAR,
       SCROLLBAR_DIRECTION,SCROLLBAR_VERTICAL,
       NULL);
/******** main loop ********/
  xv_main_loop(frame);
}

/******** close frame ********/
void
quit()
{ 
/*  strcpy(text,"quit"); */
  xv_destroy_safe(frame);
}

/******** drawing routine ********/
void 
my_repaint(canvas,paint_window,mydisplay,mywindow,xrects)
{ 

}

/*void jump_left_proc()
{
  double f_tor = 0.75;

  left_proc(f_tor);
} */


/******** filter menu procedure ********/
void
filter_menu_proc(filter_menu,menu_item)
  Menu filter_menu;
  Menu_item menu_item;
{
  char interval[10];
  printf("filter ... %s \n",xv_get(menu_item,MENU_STRING)); 
  strcpy(interval,(char *) xv_get(menu_item,MENU_STRING));
  cplot_multi_trace_(interval);
  return;
}
/******** zoom menu procedure ********/
void
zoom_menu_proc(zoom_menu,menu_item)
  Menu zoom_menu;
  Menu_item menu_item;
{
  char zoom[5];
  printf("zoom ... %s \n",xv_get(menu_item,MENU_STRING)); 
  strcpy(zoom,(char *) xv_get(menu_item,MENU_STRING));
  zoom_pre_def_(zoom);
  return;
}

/******** change start time, big interval forward ********/
void 
forwardF()
{ 
  char choise[1];
  choise[0]='F';
  change_start_time_(choise); 
  return;
}

/******** change start time, small interval forward ********/
void 
forwardf()
{ 
  char choise[1];
  choise[0]='f';
  change_start_time_(choise); 
  return;
}
/******** change start time, big interval backward ********/
void 
backwardB()
{ 
  char choise[1];
  choise[0]='B';
  change_start_time_(choise); 
  return;
}

/******** change start time, small interval backward ********/
void 
backwardb()
{ 
  char choise[1];
  choise[0]='b';
  change_start_time_(choise); 
  return;
}

/******** read start time from panel 
	  and pass it to cplot ********/
void
set_start_time()
{
  char text[12];
  int i;
  char j;
  strcpy(text,(char *) xv_get(panel_text,PANEL_VALUE));
/*  printf("start time %s",text);*/ 
  get_start_time_(text);
  return;
}

/* double size of canvas */
void cdouble()
{
  void fdouble_();
  x_max=x_max*2.;
/*  y_max=y_max*2.; */
  xv_set(canvas,CANVAS_WIDTH,x_max,CANVAS_HEIGHT,y_max,NULL);  
/*  Problem with y size
  xv_set(canvas,CANVAS_WIDTH,x_max,NULL);  */
  fdouble_();
  return;
}
  


/******** read time interval from panel 
	  and pass it to cplot ********/
void
set_interval()
{
  char interval[3];
  strcpy(interval,(char *) xv_get(panel_text1,PANEL_VALUE));
/*  printf("interval time %s",interval);  */
  get_interval_(interval);
  return;
} 

/******** zoom individual choice ********/
void
zoom_a()
{
  char interval[3];
  strcpy(interval,(char *) xv_get(panel_text2,PANEL_VALUE));
/*  printf("zoom time %s",interval);*/ 
  zoom_(interval);
  return;
} 
