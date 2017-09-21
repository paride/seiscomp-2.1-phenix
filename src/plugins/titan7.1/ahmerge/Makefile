# adapt PASSCAL var to your passcal source tree location
PASSCAL=../../../passcal1.9
# don't change below this line

EXEC=	ahmerge

INCLUDES= -I..

CFLAGS=	$(FLOAT) $(INCLUDES) $(ADD_CFLAGS)

SRCS=	ahmerge.c 

OBJS=	$(SRCS:%.c=%.o)

LFLAGS= -L$(PASSCAL)/src/lib

LIBS=  ../ioroutin.c -lm -lpasscal

all:		$(EXEC)
#
$(EXEC)::	$(OBJS)
		$(CC) -o $@ $(CFLAGS) $(OBJS) $(LFLAGS) $(LIBS) $(ADD_LIBS)
#
%.o:	%.c
		$(CC) $(CFLAGS) -c $(@F:.o=.c) -o $@ 
#
clean:
		@$(RM) $(ARCH)/*.o $(EXEC)
