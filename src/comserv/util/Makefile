CC	= g++
INCL	= ../include
CFLAGS	= -O2 -fwritable-strings -fpack-struct -I$(INCL)
LDLIBS	=
RANLIB	= /bin/true

########################################################################
# Solaris 2 definitions
#
#CC	= cc
#INCL	= ../include
#CFLAGS	= -I$(INCL) +w -DSOLARIS2 -g
#LDLIBS	= -lposix4 -lnsl -lsocket
#RANLIB	= /bin/true

########################################################################
# SunOS definitions
#
#CC	= acc
#INCL	= ../include
#CFLAGS	= -I$(INCL)
#LDLIBS	=
#RANLIB	= ranlib

########################################################################

LIB	= libutil.a

OBJECTS = service.o cfgutil.o stuff.o seedutil.o timeutil.o lockutil.o
# mmutil.o 


.PRECIOUS:	$(LIB)

ALL =		$(LIB)

all:		$(ALL)

$(LIB):		$(LIB)($(OBJECTS))
		$(RANLIB) $@

timeutil.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/stuff.h timeutil.ii
		$(CC) -c $(CFLAGS) timeutil.ii

seedutil.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/seedstrc.h $(INCL)/stuff.h seedutil.ii
		$(CC) -c $(CFLAGS) seedutil.ii

mmutil.o:	$(INCL)/dpstruc.h $(INCL)/quanstrc.h $(INCL)/service.h \
		$(INCL)/stuff.h mmutil.ii
		$(CC) -c $(CFLAGS) mmutil.ii

service.o:	$(INCL)/dpstruc.h $(INCL)/service.h $(INCL)/stuff.h \
		service.ii
		$(CC) -c $(CFLAGS) service.ii

cfgutil.o:	$(INCL)/dpstruc.h cfgutil.ii $(INCL)/stuff.h
		$(CC) -c $(CFLAGS) cfgutil.ii

stuff.o:	$(INCL)/dpstruc.h stuff.ii
		$(CC) -c $(CFLAGS) stuff.ii

lockutil.o:	$(INCL)/lockutil.h lockutil.c
		$(CC) -c $(CFLAGS) lockutil.c

%.ii: %.c
		$(CPP) $(CFLAGS) -D__cplusplus -x c $< > $@

clean:
		$(RM) *.o *.ii $(LIB)

veryclean:
		$(RM) *.o *.ii $(LIB)

install:
