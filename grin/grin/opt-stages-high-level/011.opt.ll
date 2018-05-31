; ModuleID = 'basic'
source_filename = "<string>"

@_heap_ptr_ = global i64 0

declare void @_prim_int_print(i64)

define void @grinMain() #0 {
grinMain.entry:
  %"n13'.0" = tail call fastcc i64 @sum(i64 0, i64 1, i64 1000)
  call void @_prim_int_print(i64 %"n13'.0")
  ret void

error_block:                                      ; No predecessors!
  tail call void @_prim_int_print(i64 666)
  unreachable
}

define private fastcc i64 @sum(i64 %p10, i64 %p111, i64 %p112) #0 {
sum.entry:
  %"b1'.1" = icmp sgt i64 %p111, %p112
  switch i1 %"b1'.1", label %error_block [
    i1 true, label %block.bool_True.2
    i1 false, label %block.bool_False.3
  ]

block.bool_True.2:                                ; preds = %sum.entry
  br label %block.exit.7

block.bool_False.3:                               ; preds = %sum.entry
  %"n4'.4" = add i64 %p111, 1
  %"n7'_2.5" = add i64 %p10, %p111
  %result.bool_False.6 = tail call fastcc i64 @sum(i64 %"n7'_2.5", i64 %"n4'.4", i64 %p112)
  br label %block.exit.7

block.exit.7:                                     ; preds = %block.bool_False.3, %block.bool_True.2
  %result.sum.8 = phi i64 [ %p10, %block.bool_True.2 ], [ %result.bool_False.6, %block.bool_False.3 ]
  ret i64 %result.sum.8

error_block:                                      ; preds = %sum.entry
  tail call void @_prim_int_print(i64 666)
  unreachable
}

attributes #0 = { "no-jump-tables"="true" }
