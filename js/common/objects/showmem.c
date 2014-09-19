#include <stdio.h>
#include <stdlib.h>

/* Your task is that add code to the program below so that the print
 * statements at the end of main print addresses that are in the 
 * correct range. In other words the first line will print an address
 * that really is a global data address. If you are adding more than 
 * about 5 lines of code, you should rethink your approach or ask for help.
 * (Adding constant address values will not get any marks even if correct.)
 *
 * NOTES:
 * - Do not change the printf statements except to add the appropriate
 * arguments.
 * 
 * - This file will not compile until the printf statements are fixed.
 *
 * - Your code will be run on a 64-bit CDF machine to test it.
 */



int main(int argc, char *argv[]) {



   /* WARNING: The only change that may be made below this line is to 
    * insert appropriate variables (expressions) as the arguments to 
	* printf. 
	*
	* The strange format string is just to print the addresses in a
	* format that is easy to compare them. The second argument to 
	* printf is cast to unsigned long so that the compiler doesn't 
	* complain.
    */
	printf("Global data address: \t%#014lx\n", (unsigned long)        );
	printf("Stack address: \t\t%#014lx\n", (unsigned long)            );
	printf("Code address: \t\t%#014lx\n", (unsigned long)             );
	printf("Heap address: \t\t%#014lx\n", (unsigned long)             );
	printf("Constant address: \t%#014lx\n",(unsigned long)            );

	return 0;
}