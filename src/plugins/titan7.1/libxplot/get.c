/*========================================================================
 *
 * Name - get.c
 *
 * ccs version:	1.1
 *
 * ccsid:	@(#)get.c	1.1 - 06/30/95 15:55:20
 * from: 	ccs/s.get.c
 * date: 	11/07/95 11:31:40
 *
 * Description:  image grabbing functions for xgrabsc
 *
 *               see cpyright.h for copyright information
 *
 *
 *========================================================================
 */
#include "globals.h"
#include <X11/cursorfont.h>
#include "proto.h"

Display   *hDisplay;
int        hScreen;
Window     hRoot;



/*========================================================================*/
/*
 * Get the image bounded by the given rectangle.
 * The associated colormap information is also extracted and returned.
 * TRUE is returned if an image was successfully grabbed, and FALSE
 * otherwise.
 */
int getImage(xrect, image, window)
XRectangle *xrect;
imageInfo *image;
Window window;
{
  XImage *ximage;
  int depth, ncolors, cmapSize, numCmaps;
  int h, w;
  int i;
  XColor colors[MAX_CELLS];
  Colormap *cmaps, cmap;
  int dbug = 0;

  if (xrect->width == 0  || xrect->height == 0)
    return FALSE;

  /* get the image */
  depth  = image->depth;

  if (dbug)
  {
    printf("++++ getImage: dpy %x window %x, box [%d,%d,%d,%d]\n",
      (int) hDisplay, (int) window,
      (int) (xrect->x), (int) (xrect->y),
      (int) (xrect->width), (int) (xrect->height));
    printf("++++ getImage: ximage %s\n",
     (depth==1 ? "XYPixmap" : "ZPixmap"));
  }

  ximage = XGetImage(hDisplay, window,
            xrect->x, xrect->y, xrect->width, xrect->height, AllPlanes,
            depth==1 ? XYPixmap : ZPixmap);
  image->ximage = ximage;


#if defined(DEFAULT_COLORMAP)
  cmap = DefaultColormap(hDisplay, hScreen);
  if (0) printf("++++ getImage: cmap = DefaultColormap\n");

#else
  /* get the colormap info */
  cmap = image->colormap;
  if (cmap == None)
  {
    cmaps = XListInstalledColormaps(hDisplay, window, &numCmaps);
    if (numCmaps == 0)
     cmap = DefaultColormap(hDisplay, hScreen);
    else
    {
      cmap = *cmaps;
      if (numCmaps > 1)
        fprintf(stderr,
          "more than one colormap found - using first encountered");
    }
    XFree(cmaps);
  }
  if (0) printf("++++ getImage: no DefaultColormap: cmap = %X\n", (int) cmap);
#endif

  image->colormap = cmap;

  ncolors = image->visual->map_entries;
if (0) printf("++++ getImage: ncolors=%d\n", ncolors);

  if (ncolors > MAX_CELLS) {
    fprintf(stderr, "color table is too big for this program\n");
    XCloseDisplay(hDisplay);
    exit(3);
  }
  /* this won't cut the mustard for DirectColor */
  for (i=0; i<ncolors; i++)
    colors[i].pixel = i;

  XQueryColors(hDisplay, cmap, colors, ncolors);
  for (i=0; i<ncolors; i++) {
    image->red[i]   = colors[i].red;
    image->green[i] = colors[i].green;
    image->blue[i]  = colors[i].blue;
  }

  /* figure out which colormap entries are actually used by the image */
  ncolors = cmapSize = 0;
  memset((char *)image->used, 0, MAX_CELLS);
if (0) printf("++++ getImage: h=%d w=%d xim=%0X cmapSize=%d\n",
    ximage->height,
    ximage->width,
    (int) ximage,
    cmapSize);

  for (h=0; h<ximage->height; h++)
  {
    for (w=0; w<ximage->width; w++)
    {
      i = XGetPixel(ximage, w, h);
      if (!image->used[i])
      {
        image->used[i] = TRUE;
        if ((i+1 > cmapSize))      /* keep track of colormap size */
          cmapSize = i+1;
        ncolors++;
      }
    }
  }
if (dbug) printf("++++ getImage: i=%d ncolors=%d cmapSize=%d\n",
              i, ncolors, cmapSize);

  image->numcells = cmapSize;

  return TRUE;
}



/*
 * Let the user stretch a rectangle on the screen and return its values.
 * It may be wise to grab the server before calling this routine.  If the
 * screen is allowed to change during XOR drawing video droppings may result.
 */
int getRectangle(xrect)
  XRectangle *xrect;
{
  XEvent event;
  unsigned int x, y, rootx, rooty;
  GC gc;
  Cursor pointer1, pointer2;
  int boxDrawn = False;
  int rx, ry, rw, rh;

  rw = rh = 0;
  /* get some cursors for rectangle formation */
  pointer1 = XCreateFontCursor(hDisplay, XC_ul_angle);
  pointer2 = XCreateFontCursor(hDisplay, XC_lr_angle);

  /* grab the pointer */
  if (GrabSuccess != XGrabPointer(hDisplay, hRoot, False, ButtonPressMask,
        GrabModeAsync, GrabModeAsync, hRoot, pointer1, CurrentTime))
  {
    fprintf(stderr,"- could not grab pointer!\n");
    exit(3);
  }

  /* create a graphics context to draw with */
  gc = XCreateGC(hDisplay, hRoot, 0, NULL);
  if (!gc)
  {
    fprintf(stderr,"- could not get drawing resources\n");
    exit(3);
  }
  XSetSubwindowMode(hDisplay, gc, IncludeInferiors);
  XSetForeground(hDisplay, gc, 255);
  XSetFunction(hDisplay, gc, GXxor);

  /* get a button-press and pull out the root location */
  XMaskEvent(hDisplay, ButtonPressMask, &event);

/* BEFORE, WAS
  rootx = rx = event.xbutton.x_root;
  rooty = ry = event.xbutton.y_root;
*/

  rootx = rx = event.xbutton.x;
  rooty = ry = event.xbutton.y;
/*
printf("getrect: rx=%d ry=%d\n", rx,ry);
*/


  /* get pointer motion events */
  XChangeActivePointerGrab(hDisplay, ButtonMotionMask | ButtonReleaseMask,
        pointer2, CurrentTime);


  /* MAKE_RECT converts the original root coordinates and the event root
   * coordinates into a rectangle in xrect */
/* BEFORE, WAS
  x = event.etype.x_root;       \
  y = event.etype.y_root;       \
*/

#define MAKE_RECT(etype) \
  x = event.etype.x;            \
  y = event.etype.y;            \
  rw  = x - rootx;              \
  if (rw  < 0) rw  = -rw;       \
  rh  = y - rooty;              \
  if (rh  < 0) rh  = -rh;       \
  rx = x < rootx ? x : rootx;   \
  ry = y < rooty ? y : rooty

  /* loop to let the user drag a rectangle */
  while (TRUE)
  {
    XNextEvent(hDisplay, &event);
    switch(event.type)
    {

      case ButtonRelease:
        if (boxDrawn)
        {
          XDrawRectangle(hDisplay, hRoot, gc, rx, ry, rw, rh);
          boxDrawn = False;
        }
        XFlush(hDisplay);
        /* record the final location */
        MAKE_RECT(xbutton);
        /* release resources */
        XFreeGC(hDisplay, gc);
        XFreeCursor(hDisplay, pointer1);
        XFreeCursor(hDisplay, pointer2);
        xrect->x      = rx;
        xrect->y      = ry;
        xrect->width  = rw;
        xrect->height = rh;
        XUngrabPointer(hDisplay, CurrentTime);
	XSync(hDisplay, FALSE);
        return True;

      case MotionNotify:
        if (boxDrawn)
        {
          XDrawRectangle(hDisplay, hRoot, gc, rx, ry, rw, rh);
          boxDrawn = False;
        }
        while (XCheckTypedEvent(hDisplay, MotionNotify, &event))
          {}
        MAKE_RECT(xmotion);
        XDrawRectangle(hDisplay, hRoot, gc, rx, ry, rw, rh);
        boxDrawn = True;
        break;
    }
  }
}

