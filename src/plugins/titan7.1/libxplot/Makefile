CC        = gcc
CC        = cc 
#
CFLAGS    =  -O -DANSI_C -Wall

.SUFFIXES: .c
.c.o:
	$(CC) -c $(CFLAGS) $<

libxplot.o: char.o libfilt.o libgif.o libplot.o libps.o util.o \
	libutil.o libcmd.o xvgifwr.o get.o
	ar rv libxplot.o char.o libfilt.o libgif.o libplot.o libps.o util.o \
	libutil.o libcmd.o xvgifwr.o get.o
	ranlib libxplot.o

clean:
	@/bin/rm *.o 


