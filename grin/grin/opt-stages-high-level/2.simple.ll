; ModuleID = 'basic'
source_filename = "<string>"

declare i64 @_prim_int_print(i64)

define i64* @grinMain(i64* %_heap_ptr_.0) #0 {
grinMain.entry:
  %nodeAddress.4 = bitcast i64* %_heap_ptr_.0 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> zeroinitializer, <{ i64, [1 x i64] }>* %nodeAddress.4, align 1
  %_heap_ptr_.5 = bitcast i64* %_heap_ptr_.0 to <{ i64, [1 x i64] }>*
  %_heap_ptr_.6 = getelementptr inbounds <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %_heap_ptr_.5, i32 1
  %_heap_ptr_.7 = bitcast <{ i64, [1 x i64] }>* %_heap_ptr_.6 to i64*
  %nodeAddress.11 = bitcast i64* %_heap_ptr_.7 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] [i64 1] }>, <{ i64, [1 x i64] }>* %nodeAddress.11, align 1
  %_heap_ptr_.12 = bitcast i64* %_heap_ptr_.7 to <{ i64, [1 x i64] }>*
  %_heap_ptr_.13 = getelementptr inbounds <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %_heap_ptr_.12, i32 1
  %_heap_ptr_.14 = bitcast <{ i64, [1 x i64] }>* %_heap_ptr_.13 to i64*
  %nodeAddress.18 = bitcast i64* %_heap_ptr_.14 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] [i64 1000] }>, <{ i64, [1 x i64] }>* %nodeAddress.18, align 1
  %_heap_ptr_.19 = bitcast i64* %_heap_ptr_.14 to <{ i64, [1 x i64] }>*
  %_heap_ptr_.20 = getelementptr inbounds <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %_heap_ptr_.19, i32 1
  %_heap_ptr_.21 = bitcast <{ i64, [1 x i64] }>* %_heap_ptr_.20 to i64*
  %node.23 = insertvalue <{ i64, [2 x i64*] }> <{ i64 1, [2 x i64*] undef }>, i64* %_heap_ptr_.7, 1, 0
  %node.24 = insertvalue <{ i64, [2 x i64*] }> %node.23, i64* %_heap_ptr_.14, 1, 1
  %tag.25 = extractvalue <{ i64, [2 x i64*] }> %node.24, 0
  %nodeAddress.26 = bitcast i64* %_heap_ptr_.21 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node.24, <{ i64, [2 x i64*] }>* %nodeAddress.26, align 1
  %_heap_ptr_.27 = bitcast i64* %_heap_ptr_.21 to <{ i64, [2 x i64*] }>*
  %_heap_ptr_.28 = getelementptr inbounds <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %_heap_ptr_.27, i32 1
  %_heap_ptr_.29 = bitcast <{ i64, [2 x i64*] }>* %_heap_ptr_.28 to i64*
  %node.31 = insertvalue <{ i64, [2 x i64*] }> <{ i64 2, [2 x i64*] undef }>, i64* %_heap_ptr_.0, 1, 0
  %node.32 = insertvalue <{ i64, [2 x i64*] }> %node.31, i64* %_heap_ptr_.21, 1, 1
  %tag.33 = extractvalue <{ i64, [2 x i64*] }> %node.32, 0
  %nodeAddress.34 = bitcast i64* %_heap_ptr_.29 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node.32, <{ i64, [2 x i64*] }>* %nodeAddress.34, align 1
  %_heap_ptr_.35 = bitcast i64* %_heap_ptr_.29 to <{ i64, [2 x i64*] }>*
  %_heap_ptr_.36 = getelementptr inbounds <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %_heap_ptr_.35, i32 1
  %_heap_ptr_.37 = bitcast <{ i64, [2 x i64*] }>* %_heap_ptr_.36 to i64*
  %tag.38 = load i64, i64* %_heap_ptr_.29, align 1
  %nodeAddress.39 = bitcast i64* %_heap_ptr_.29 to <{ i64, [2 x i64*] }>*
  %node.40 = load <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %nodeAddress.39, align 1
  %p15 = extractvalue <{ i64, [2 x i64*] }> %node.40, 1, 0
  %p16 = extractvalue <{ i64, [2 x i64*] }> %node.40, 1, 1
  %sum_result.41 = tail call <{ i64*, i64 }> @sum(i64* %_heap_ptr_.37, i64* %p15, i64* %p16)
  %heap_end.45 = extractvalue <{ i64*, i64 }> %sum_result.41, 0
  %"n13'.43" = extractvalue <{ i64*, i64 }> %sum_result.41, 1
  %grinMain_result.44 = call i64 @_prim_int_print(i64 %"n13'.43")
  ret i64* %heap_end.45

error_block:                                      ; No predecessors!
  %error_result.46 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

define <{ i64*, i64 }> @sum(i64* %_heap_ptr_.47, i64* %p10, i64* %p11) #0 {
sum.entry:
  %tag.48 = load i64, i64* %p11, align 1
  %nodeAddress.49 = bitcast i64* %p11 to <{ i64, [2 x i64*] }>*
  %node.50 = load <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %nodeAddress.49, align 1
  %p17 = extractvalue <{ i64, [2 x i64*] }> %node.50, 1, 0
  %p18 = extractvalue <{ i64, [2 x i64*] }> %node.50, 1, 1
  %tag.51 = load i64, i64* %p17, align 1
  %nodeAddress.52 = bitcast i64* %p17 to <{ i64, [1 x i64] }>*
  %node.53 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %nodeAddress.52, align 1
  %"n2'" = extractvalue <{ i64, [1 x i64] }> %node.53, 1, 0
  %tag.54 = load i64, i64* %p18, align 1
  %nodeAddress.55 = bitcast i64* %p18 to <{ i64, [1 x i64] }>*
  %node.56 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %nodeAddress.55, align 1
  %"n3'" = extractvalue <{ i64, [1 x i64] }> %node.56, 1, 0
  %"b1'.57" = icmp sgt i64 %"n2'", %"n3'"
  switch i1 %"b1'.57", label %error_block [
    i1 true, label %switch.bool_True.58
    i1 false, label %switch.bool_False.64
  ]

switch.bool_True.58:                              ; preds = %sum.entry
  %tag.61 = load i64, i64* %p10, align 1
  %nodeAddress.62 = bitcast i64* %p10 to <{ i64, [1 x i64] }>*
  %node.63 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %nodeAddress.62, align 1
  %"n14'" = extractvalue <{ i64, [1 x i64] }> %node.63, 1, 0
  br label %switch.exit.104

switch.bool_False.64:                             ; preds = %sum.entry
  %"n4'.65" = add i64 %"n2'", 1
  %node.67 = insertvalue <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] undef }>, i64 %"n4'.65", 1, 0
  %tag.68 = extractvalue <{ i64, [1 x i64] }> %node.67, 0
  %nodeAddress.69 = bitcast i64* %_heap_ptr_.47 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> %node.67, <{ i64, [1 x i64] }>* %nodeAddress.69, align 1
  %_heap_ptr_.70 = bitcast i64* %_heap_ptr_.47 to <{ i64, [1 x i64] }>*
  %_heap_ptr_.71 = getelementptr inbounds <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %_heap_ptr_.70, i32 1
  %_heap_ptr_.72 = bitcast <{ i64, [1 x i64] }>* %_heap_ptr_.71 to i64*
  %node.74 = insertvalue <{ i64, [2 x i64*] }> <{ i64 1, [2 x i64*] undef }>, i64* %_heap_ptr_.47, 1, 0
  %node.75 = insertvalue <{ i64, [2 x i64*] }> %node.74, i64* %p18, 1, 1
  %tag.76 = extractvalue <{ i64, [2 x i64*] }> %node.75, 0
  %nodeAddress.77 = bitcast i64* %_heap_ptr_.72 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node.75, <{ i64, [2 x i64*] }>* %nodeAddress.77, align 1
  %_heap_ptr_.78 = bitcast i64* %_heap_ptr_.72 to <{ i64, [2 x i64*] }>*
  %_heap_ptr_.79 = getelementptr inbounds <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %_heap_ptr_.78, i32 1
  %_heap_ptr_.80 = bitcast <{ i64, [2 x i64*] }>* %_heap_ptr_.79 to i64*
  %node.82 = insertvalue <{ i64, [2 x i64*] }> <{ i64 4, [2 x i64*] undef }>, i64* %p17, 1, 0
  %node.83 = insertvalue <{ i64, [2 x i64*] }> %node.82, i64* %_heap_ptr_.72, 1, 1
  %node.85 = insertvalue <{ i64, [2 x i64*] }> <{ i64 4, [2 x i64*] undef }>, i64* %p17, 1, 0
  %node.86 = insertvalue <{ i64, [2 x i64*] }> %node.85, i64* %_heap_ptr_.72, 1, 1
  %p12_2 = extractvalue <{ i64, [2 x i64*] }> %node.86, 1, 0
  %p13_2 = extractvalue <{ i64, [2 x i64*] }> %node.86, 1, 1
  %tag.87 = load i64, i64* %p10, align 1
  %nodeAddress.88 = bitcast i64* %p10 to <{ i64, [1 x i64] }>*
  %node.89 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %nodeAddress.88, align 1
  %"n5'_2" = extractvalue <{ i64, [1 x i64] }> %node.89, 1, 0
  %tag.90 = load i64, i64* %p12_2, align 1
  %nodeAddress.91 = bitcast i64* %p12_2 to <{ i64, [1 x i64] }>*
  %node.92 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %nodeAddress.91, align 1
  %"n6'_2" = extractvalue <{ i64, [1 x i64] }> %node.92, 1, 0
  %"n7'_2.93" = add i64 %"n5'_2", %"n6'_2"
  %node.95 = insertvalue <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] undef }>, i64 %"n7'_2.93", 1, 0
  %tag.96 = extractvalue <{ i64, [1 x i64] }> %node.95, 0
  %nodeAddress.97 = bitcast i64* %_heap_ptr_.80 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> %node.95, <{ i64, [1 x i64] }>* %nodeAddress.97, align 1
  %_heap_ptr_.98 = bitcast i64* %_heap_ptr_.80 to <{ i64, [1 x i64] }>*
  %_heap_ptr_.99 = getelementptr inbounds <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %_heap_ptr_.98, i32 1
  %_heap_ptr_.100 = bitcast <{ i64, [1 x i64] }>* %_heap_ptr_.99 to i64*
  %sum_result.101 = tail call <{ i64*, i64 }> @sum(i64* %_heap_ptr_.100, i64* %_heap_ptr_.80, i64* %p13_2)
  %_heap_ptr_.102 = extractvalue <{ i64*, i64 }> %sum_result.101, 0
  %altResult.103 = extractvalue <{ i64*, i64 }> %sum_result.101, 1
  br label %switch.exit.104

switch.exit.104:                                  ; preds = %switch.bool_False.64, %switch.bool_True.58
  %_heap_ptr_.105 = phi i64* [ %_heap_ptr_.47, %switch.bool_True.58 ], [ %_heap_ptr_.102, %switch.bool_False.64 ]
  %sum_result.106 = phi i64 [ %"n14'", %switch.bool_True.58 ], [ %altResult.103, %switch.bool_False.64 ]
  %sum_wrapped_result.107 = insertvalue <{ i64*, i64 }> undef, i64* %_heap_ptr_.105, 0
  %sum_wrapped_result.108 = insertvalue <{ i64*, i64 }> %sum_wrapped_result.107, i64 %sum_result.106, 1
  ret <{ i64*, i64 }> %sum_wrapped_result.108

error_block:                                      ; preds = %sum.entry
  %error_result.109 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

attributes #0 = { "no-jump-tables"="true" }
