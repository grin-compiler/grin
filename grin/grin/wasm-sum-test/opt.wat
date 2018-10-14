(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (import "env" "__linear_memory" (memory (;0;) 1))
  (import "env" "__indirect_function_table" (table (;0;) 0 anyfunc))
  (import "env" "_prim_int_print" (func (;0;) (type 1)))
  (func (;1;) (type 0)
    i32.const 5050
    call 0)
  (data (i32.const 0) "\00\00\00\00"))
