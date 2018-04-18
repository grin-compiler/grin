#include <stdio.h>
#include <stdlib.h>

int64_t* grinMain(int64_t*);

int64_t _prim_int_print(int64_t i) {
  printf("%ld\n", i);
  return i;
}

int main() {
  int64_t* heap = calloc(100*1024*1024,sizeof(int8_t));
  grinMain(heap);
  free(heap);
  return 0;
}
