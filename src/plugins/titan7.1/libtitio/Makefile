CC        = gcc
CC        = cc 
#
# directives de compilation
# ST_HAS_PARTITION: le fichier include/sys/mtio.h
#                   contient les commandes pour gerer 2 partitions
#                   sur DAT. Pour l'instant uniquement pour Linux >2.0.0
#
# USE_CACHE: entree/sortie sur DAT faites a travers un cache si defini
#            ou fait simplement en mode buffer si non defini
#
CFLAGS    =  -g -DUSE_CACHE
CFLAGS    =  -g -DST_HAS_PARTITION
CFLAGS    =  -DANSI_C -Wall

.SUFFIXES: .c
.c.o:
	$(CC) -c $(CFLAGS) $<

libio.o: titanio.o datio.o fileio.o diskio.o libst.o cache.o
	ar rv libio.o titanio.o datio.o fileio.o diskio.o libst.o cache.o; ranlib libio.o

clean:
	@/bin/rm *.o 

