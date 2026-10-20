#include <spawn.h>
#include <stdio.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <time.h>
extern char **environ;
int main(int argc, char **argv) {
  struct timespec a,b; struct rusage ru; pid_t p; int st;
  clock_gettime(CLOCK_MONOTONIC,&a);
  if(argc<3 || posix_spawn(&p,argv[2],NULL,NULL,argv+2,environ) || wait4(p,&st,0,&ru)<0) return 127;
  clock_gettime(CLOCK_MONOTONIC,&b);
  FILE *f=fopen(argv[1],"w"); if(!f) return 127;
  fprintf(f,"%d %.9f %ld\n",WIFEXITED(st)?WEXITSTATUS(st):128+WTERMSIG(st),(double)(b.tv_sec-a.tv_sec)+(b.tv_nsec-a.tv_nsec)/1e9,(long)ru.ru_maxrss);
  return fclose(f);
}
