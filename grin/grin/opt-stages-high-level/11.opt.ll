; ModuleID = 'basic'
source_filename = "<string>"

declare i64 @_prim_int_print(i64)

define i64* @grinMain(i64* %_heap_ptr_.0) #0 {
grinMain.entry:
  %sum_result.1 = tail call <{ i64*, i64 }> @sum(i64* %_heap_ptr_.0, i64 0, i64 1, i64 1000)
  %heap_end.5 = extractvalue <{ i64*, i64 }> %sum_result.1, 0
  %"n13'.3" = extractvalue <{ i64*, i64 }> %sum_result.1, 1
  %grinMain_result.4 = call i64 @_prim_int_print(i64 %"n13'.3")
  ret i64* %heap_end.5

error_block:                                      ; No predecessors!
  %error_result.6 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

define <{ i64*, i64 }> @sum(i64* %_heap_ptr_.7, i64 %p10, i64 %p111, i64 %p112) #0 {
sum.entry:
  %"b1'.8" = icmp sgt i64 %p111, %p112
  switch i1 %"b1'.8", label %error_block [
    i1 true, label %switch.bool_True.9
    i1 false, label %switch.bool_False.10
  ]

switch.bool_True.9:                               ; preds = %sum.entry
  br label %switch.exit.16

switch.bool_False.10:                             ; preds = %sum.entry
  %"n4'.11" = add i64 %p111, 1
  %"n7'_2.12" = add i64 %p10, %p111
  %sum_result.13 = tail call <{ i64*, i64 }> @sum(i64* %_heap_ptr_.7, i64 %"n7'_2.12", i64 %"n4'.11", i64 %p112)
  %_heap_ptr_.14 = extractvalue <{ i64*, i64 }> %sum_result.13, 0
  %altResult.15 = extractvalue <{ i64*, i64 }> %sum_result.13, 1
  br label %switch.exit.16

switch.exit.16:                                   ; preds = %switch.bool_False.10, %switch.bool_True.9
  %_heap_ptr_.17 = phi i64* [ %_heap_ptr_.7, %switch.bool_True.9 ], [ %_heap_ptr_.14, %switch.bool_False.10 ]
  %sum_result.18 = phi i64 [ %p10, %switch.bool_True.9 ], [ %altResult.15, %switch.bool_False.10 ]
  %sum_wrapped_result.19 = insertvalue <{ i64*, i64 }> undef, i64* %_heap_ptr_.17, 0
  %sum_wrapped_result.20 = insertvalue <{ i64*, i64 }> %sum_wrapped_result.19, i64 %sum_result.18, 1
  ret <{ i64*, i64 }> %sum_wrapped_result.20

error_block:                                      ; preds = %sum.entry
  %error_result.21 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

attributes #0 = { "no-jump-tables"="true" }
