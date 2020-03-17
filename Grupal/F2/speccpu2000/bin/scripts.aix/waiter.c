#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/file.h>
#include <errno.h>
#include <string.h>

void usage(char *name) {
    (void)printf ("Usage: %s <filename>\n", name);
    exit (1);
}

/*ARGSUSED*/
void donothing(int sig) {
    return;
}

int main (int argc, char *argv[]) {
    char buf[256];
    char *filename;
    int fd;
    pid_t pid;

    if (argc < 2)
	usage(argv[0]);

    filename = argv[1];

    fd = open(filename, O_RDWR|O_CREAT, 0644);
    if (fd < 0) {
	(void)printf ("Can't open '%s': %s(%d)\n", filename, strerror(errno), errno);
	exit (1);
    }
    if (argc < 3) {
	(void)sprintf(buf, "%d\n", getpid());
	(void)write(fd, buf, strlen(buf));
	(void)ftruncate(fd, strlen(buf));
	(void)close(fd);
	(void)signal(SIGUSR1, donothing); 
	(void)select(0, NULL, NULL, NULL, NULL);
	(void)unlink(filename);
    } else {
	(void)read(fd, buf, sizeof(buf));
	(void)close(fd);
	(void)sscanf(buf, "%d", &pid);
	if (pid != 0)
	    (void)kill(pid, SIGUSR1);
    }

    return 0;
}
