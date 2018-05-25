; ModuleID = 'basic'
source_filename = "<string>"

@_heap_ptr_ = global i64 0

declare i64 @_prim_int_print(i64)

define i64 @grinMain() #0 {
grinMain.entry:
  %"n13'.0" = tail call fastcc i64 @sum(i64 0, i64 1, i64 1000)
  %result.grinMain.1 = call i64 @_prim_int_print(i64 %"n13'.0")
  ret i64 %result.grinMain.1

error_block:                                      ; No predecessors!
  %error_result.2 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

define private fastcc i64 @sum(i64 %p10, i64 %p111, i64 %p112) #0 {
sum.entry:
  %"b1'.3" = icmp sgt i64 %p111, %p112
  switch i1 %"b1'.3", label %error_block [
    i1 true, label %block.bool_True.4
    i1 false, label %block.bool_False.5
  ]

block.bool_True.4:                                ; preds = %sum.entry
  br label %block.exit.9

block.bool_False.5:                               ; preds = %sum.entry
  %"n4'.6" = add i64 %p111, 1
  %"n7'_2.7" = add i64 %p10, %p111
  %result.bool_False.8 = tail call fastcc i64 @sum(i64 %"n7'_2.7", i64 %"n4'.6", i64 %p112)
  br label %block.exit.9

block.exit.9:                                     ; preds = %block.bool_False.5, %block.bool_True.4
  %result.sum.10 = phi i64 [ %p10, %block.bool_True.4 ], [ %result.bool_False.8, %block.bool_False.5 ]
  ret i64 %result.sum.10

error_block:                                      ; preds = %sum.entry
  %error_result.11 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

attributes #0 = { "no-jump-tables"="true" }
