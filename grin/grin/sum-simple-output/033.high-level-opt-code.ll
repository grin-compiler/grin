; ModuleID = 'basic'
source_filename = "<string>"

@_heap_ptr_ = global i64 0

declare void @_prim_int_print(i64)

define void @grinMain() #0 {
grinMain.entry:
  %unboxed.CInt.0.0 = tail call fastcc i64 @sum.unboxed(i64 1, i64 10000)
  call void @_prim_int_print(i64 %unboxed.CInt.0.0)
  ret void

error_block:                                      ; No predecessors!
  tail call void @_prim_int_print(i64 666)
  unreachable
}

define private fastcc i64 @sum.unboxed(i64 %l1, i64 %l2) #0 {
sum.unboxed.entry:
  %"b'.0.1" = icmp sgt i64 %l1, %l2
  switch i1 %"b'.0.1", label %error_block [
    i1 true, label %block.bool_True.2
    i1 false, label %block.bool_False.3
  ]

block.bool_True.2:                                ; preds = %sum.unboxed.entry
  br label %block.exit.7

block.bool_False.3:                               ; preds = %sum.unboxed.entry
  %"m1'.0.4" = add i64 %l1, 1
  %unboxed.CInt.1.0.5 = tail call fastcc i64 @sum.unboxed(i64 %"m1'.0.4", i64 %l2)
  %result.bool_False.6 = add i64 %l1, %unboxed.CInt.1.0.5
  br label %block.exit.7

block.exit.7:                                     ; preds = %block.bool_False.3, %block.bool_True.2
  %result.sum.unboxed.8 = phi i64 [ 0, %block.bool_True.2 ], [ %result.bool_False.6, %block.bool_False.3 ]
  ret i64 %result.sum.unboxed.8

error_block:                                      ; preds = %sum.unboxed.entry
  tail call void @_prim_int_print(i64 666)
  unreachable
}

attributes #0 = { "no-jump-tables"="true" }
