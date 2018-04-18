#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int64_t* grinMain(int64_t*);

int64_t _prim_int_print(int64_t i) {
  printf("%ld\n", i);
  return i;
}

int main() {
  int64_t* heap = malloc(100*1024*1024);
  int64_t* heap_end = grinMain(heap);
  printf("used memory: %ld bytes\n", (uint64_t)heap_end - (uint64_t)heap);
  free(heap);
  return 0;
}
