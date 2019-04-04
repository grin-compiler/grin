#include "IR.h"
#include <stdio.h>

int main() {
  abstract_program_t *prg = load_abstract_program("000.ghc_sumsimple.dfbin");
  if (prg) {
    printf("load OK\n");
  } else {
    printf("load error\n");
  }

  abstract_program_t *prg2 = load_abstract_program("000.ghc_sum_simple2.dfbin");
  if (prg2) {
    printf("load OK\n");
  } else {
    printf("load error\n");
  }



  return 0;
}