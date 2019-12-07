#include <stdio.h>
#include "IR.h"

int main(int argc, char **argv) {
  for (int i = 1 ; i < argc ; i++) {
    printf("loading: %s\n", argv[i]);
    eval_abstract_program(argv[i]);
  }

  return 0;
}