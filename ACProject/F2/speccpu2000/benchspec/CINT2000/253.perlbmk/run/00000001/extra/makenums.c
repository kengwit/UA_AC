/* makenum.c -- make 2 sets of random bits, such that when evaluated as 32-bit
                integer numbers, they have the same value on big- and little-
		endian systems.

   Written in support of 253.perlbmk for SPEC CPU2000
   Cloyce D. Spradling <cloyce@austin.ibm.com>
 */

/* Take a file of random bits, and output numbers in big and little
   endian.  This needs to run on a little endian machine! (It should
   probably be 32-bit as well.)  */

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>

void blast_array(char *fname, unsigned nums, unsigned char intsize,
		 unsigned magicnums, unsigned int *ary);

int main(void) {
  int fd = -1,
      i = 0;
  unsigned int *nums = NULL;
  unsigned int numnums = 0,
               intoffset = 0,
               numidx = 0;
  unsigned int min = 0xFFFFFFFF, max = 0;
  unsigned char numsize = 4;

  if (htonl(4L) == 4) {
    fprintf(stderr, "I really want to run on a little endian machine, please.\n");
    exit(5);
  }
  fd = open("numnums", O_RDONLY);
  if (fd < 0) {
    perror("open numnums");
    exit(1);
  }
  if (read(fd, (void *)&numsize, 1) != 1) {
    perror("read numsize");
    exit(3);
  }
  if (read(fd, (void *)&numnums, 4) != 4) {
    perror("read numnums");
    exit(2);
  }
  nums = (unsigned int *)calloc((size_t) numnums, sizeof(int));
  intoffset = sizeof(int) - numsize;
  if (intoffset < 0 || intoffset >= 256) {
    fprintf(stderr, "Oops!  Reading numnums on a system with bigger ints (%d) than we have (%d)!\n", numsize, sizeof(int));
    exit(4);
  }
  printf("numnums = %u\nnumsize= %hu\nintoffset = %d\n", numnums,
	 numsize, intoffset);
  /* This is dumb and slow, but there's no need to be clever here anyway */
  while(read(fd, (void *)((&nums[numidx])+intoffset), numsize) == numsize) {
    if (nums[numidx] < min) min = nums[numidx];
    if (nums[numidx] > max) max = nums[numidx];
    numidx++;
  }
  close(fd);
  printf("read %d\n", numidx);
  
  /* Now iterate over the array and output a set of little endian (host order)
     and big endian (network order) numbers, as well as the magic at the
     beginning */
  blast_array("lenums", numnums, sizeof(int), numnums, nums);
  for(i = 0; i < numnums; i++) {
    nums[i] = (unsigned int)htonl(nums[i]);
  }
  blast_array("benums", numnums, sizeof(int), htonl(numnums), nums);

  return 0;
}

void blast_array(char *fname, unsigned nums, unsigned char intsize,
		 unsigned magicnums, unsigned int *ary) {
  int fd = -1;

  printf("Writing %d nums to %s (%u bytes)...", nums, fname,
	 (nums+1)*intsize+1);
  fd = open(fname, O_CREAT|O_WRONLY, S_IRWXU|S_IRGRP|S_IROTH);
  if (fd < 0) {
    perror("blast_array file open");
    exit(6);
  }
  /* Write the magic bits of the file */
  if (write(fd, (void *)&intsize, 1) != 1) {
    perror("blast_array magic size write");
    exit(8);
  }
  if (write(fd, (void *)&magicnums, intsize) != intsize) {
    perror("blast_array magic nums write");
    exit(9);
  }
  if (write(fd, (void *)ary, nums*intsize) != (nums * intsize)) {
    perror("blast_array array write");
    exit(7);
  }
  close(fd);
  printf("done\n");
}



