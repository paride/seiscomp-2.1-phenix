###################################################################################
#
#               Makefile for ToolsLib including
#                
#               AVL tree 
#
################################################################################ 
CC	       = gcc -DCOMSERV
CFLAGS         = -O2 -fwritable-strings
#CFLAGS        = -ansi -g
INCLUDE        = 
LIBS           = 
CFLAGSLINE     = "$(CFLAGS) -I$(INCLUDE)"
MAKE           = make CFLAGS=$(CFLAGSLINE) LIBRARY=../$(LIBRARY)
EXECUTABLE     = ToolsLib
LIBRARY        = $(EXECUTABLE).a

################################################################################
# Sources for the AVL tree library
################################################################################

AVLDEMO     	= 	avl/avldemo.c 	# Demo of the AVL Routines
AVLSOURCES     	= 	avl/avlfile.c \
			avl/avlfind.c \
			avl/avlfree.c \
			avl/avlinsert.c \
			avl/avlminmax.c \
			avl/avlprint.c \
			avl/avlreplace.c \
			avl/avldelete.c
# 
FILESTRSOURCES  =	FileFind.c 
 
#
AVLHEADER	=	avl/avl.h \
			avl/avltree.h 
FILESTRHEADER	=	FileStr.h 
#
STRINGSOURCES	=	StrMatch.c \
			GetSubStr.c \
			StrToUpper.c \
			StrToLower.c \
			Substitute.c \
			ComservCmp.c 
#
AVLOBJECTS	=	$(AVLSOURCES:.c=.o)
FILESTROBJECTS	=	$(FILESTRSOURCES:.c=.o)
STRINGOBJECTS 	=	$(STRINGSOURCES:.c=.o)

################################################################################
# Rebuild the programs and libs if its subprocedures change.
################################################################################

library:	 $(LIBRARY)(	$(AVLOBJECTS)\
				$(FILESTROBJECTS)\
				$(STRINGOBJECTS)	)
	@echo "updating Routines ... "
	ar ruvs $(LIBRARY) $? | tee $(LIBRARY).content
	@echo " All files in archiv" 
	ar t $(LIBRARY)
	@echo ""
	ranlib $(LIBRARY)	
	@echo "Finished ..."

$(LIBRARY)(%.o): %.o
	@true 

#-------------------------------------------------------------------------------
#                              AVL ROUTINES
#-------------------------------------------------------------------------------
AVL_DIR = avl

avlfile.o: avlfile.c $(AVLHEADER)
	$(CC) $(CFLAGS) -c $<

avlfind.o: avlfind.c $(AVLHEADER)
	$(CC) $(CFLAGS) -c $<

avlfree.o: avlfree.c $(AVLHEADER)
	$(CC) $(CFLAGS) -c $<

avlinsert.o: avlinsert.c $(AVLHEADER)
	$(CC) $(CFLAGS) -c $<

avlminmax.o: avlminmax.c $(AVLHEADER)
	$(CC) $(CFLAGS) -c $<

avlprint.o: avlprint.c $(AVLHEADER)
	$(CC) $(CFLAGS) -c $<

avlreplace.o: avlreplace.c $(AVLHEADER)
	$(CC) $(CFLAGS) -c $<

avldelete.o: avldelete.c $(AVLHEADER)
	$(CC) $(CFLAGS) -c $<

#-------------------------------------------------------------------------------
#                              FILESTR ROUTINES
#-------------------------------------------------------------------------------
$(FILESSTROBJECTS): $(FILESTRSOURCES)	$(FILESTRHEADER) 
	$(CC) $(CFLAGS) -c $(FILESTRSOURCES)

#-------------------------------------------------------------------------------
#                              STRING ROUTINES
#-------------------------------------------------------------------------------
$(STRINGOBJECTS): $(STRINGSOURCES)	$(FILESTRHEADER)
	$(CC) $(CFLAGS)  -c $(STRINGSOURCES)

###################################################################################
#                              OTHER WORK
###################################################################################

avldemo:
	$(CC) $(CFLAGS)  avl/avldemo.c $(LIBRARY) -o avldemo

filefind:
	$(CC) $(CFLAGS)  $(FILESTRSOURCES) $(LIBRARY) -o filefind

clean:
	@echo "Cleaning Utilities AVL objects ... "
	$(RM) ToolsLib.a ToolsLib.a.content
	$(RM) *.o
	$(RM) avl/*.o
	@echo "Finished ..."
