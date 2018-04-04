# Memory management

## Heap pointer
- do not use register pinning to store heap pointer
- instead always pass heap pointer as a function argument
  - each thread has own heap and heap pointer
  - save heap pointer to a global variable when calling foreign function

## Compile time garbage collection
[ASAP: As Static As Possible memory management](http://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.html)

related: [Dead data elimination](http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/MoL-2010-19.text.pdf#page=55) in [A modern back-end for a dependently typed language](http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/MoL-2010-19.text.pdf)
