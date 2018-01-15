# Memory management

## Heap pointer
- do not use register pinning to store heap pointer
- instead always pass heap pointer as a function argument
  - each thread has own heap and heap pointer
  - save heap pointer to a global variable when calling foreign function
