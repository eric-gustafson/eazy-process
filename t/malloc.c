

#include <stdio.h>
#include <stdlib.h>


int main(void)
{
  /* allocate 500 MiB (2^10=1024)^3-1 */
  int size = (1 << 29);
  int length = (size / sizeof (int));
  int* a;
  int i;
  printf("Try to allocate 1GB memory\n");
  printf("size: %d bytes\n",size);
  printf("size of int: %d\n",(int)(sizeof (int)));
  printf("length of array: %d\n",length);
  a = (int *)malloc(size);
  if(a == NULL) {
    printf("fail\n");
    return 1;
  }
  printf("allocated\n");
  for(i=0;i<length;i++){
    a[i]=i;
    if ((i*sizeof (int))%(1<<27)==0){
      printf("set %d bytes\n",i);
    }
  }
  printf("done\n");
  free(a);
  return 0;
}
