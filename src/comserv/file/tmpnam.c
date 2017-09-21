#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
char caTmp[256];

if( argc == 2 ) {
	strcpy(caTmp, argv[1]);
	strcat(caTmp, "XXXXXX" );
} else {
	fprintf( stderr, "\nusage: argv[0] <short file name> !\n" );
	exit(1);
}

fprintf( stdout, "%s", (char*)mktemp(caTmp) );

exit(0);
}

