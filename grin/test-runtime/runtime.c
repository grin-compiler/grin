#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

/*
TODO: Add statistics.
*/

extern int64_t _heap_ptr_;
int64_t grinMain();

void __runtime_error(int64_t c){
  exit(c);
}

int main() {
  int64_t* heap = malloc(100*1024*1024);
  _heap_ptr_ = (int64_t)heap;
  grinMain();
  free(heap);
  return 0;
}
