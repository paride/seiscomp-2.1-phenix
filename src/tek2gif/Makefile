CC      = gcc
CFLAGS  = -O2
LDFLAGS = -L/opt/gd/lib
LDLIBS  = -lgd
INC     = -I/opt/gd/include
BINDIR  = ../bin

$(BINDIR)/tek2gif: tek2gif.o handler.o graph.o transform.o
	$(CC) $(LDFLAGS) $+ $(LDLIBS) -o $@

tek2gif.o: tek2gif.c settings.h tek2gif.h
	$(CC) -c $(CFLAGS) -c $< -o $@

handler.o: handler.c font.h settings.h tek2gif.h
	$(CC) -c $(CFLAGS) -c $< -o $@

graph.o: graph.c palette.h tek2gif.h
	$(CC) -c $(CFLAGS) $(INC) -c $< -o $@

transform.o: transform.c tek2gif.h
	$(CC) -c $(CFLAGS) -c $< -o $@

clean:
	$(RM) tek2gif.o handler.o graph.o transform.o

