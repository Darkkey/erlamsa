#include <stdio.h>
#include <string.h>
#include <unistd.h>

char * my_strncpy(char *dest, char *src, int len){
	return strncpy(dest, src, len);
}

int main(int c, char* argv[]){
	char ar1[10];

	printf("Hello world!\n");

	bzero(ar1, 10);

	for(long i = 0; i < 1000000; i++){
	 	my_strncpy(ar1, argv[0], 10);
		printf("%lu %lu %s\n", i, strlen(ar1), ar1);
		sleep(1);
	}

	return 0;
}
