; ModuleID = 'basic'
source_filename = "<string>"

@_heap_ptr_ = global i64 0

declare i64 @_prim_int_print(i64)

define i64 @grinMain() #0 {
grinMain.entry:
  %new_node_ptr.2 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.3 = inttoptr i64 %new_node_ptr.2 to i64*
  %ptr_CInt.6 = bitcast i64* %new_node_ptr.3 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> zeroinitializer, <{ i64, [1 x i64] }>* %ptr_CInt.6, align 1
  %new_node_ptr.9 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.10 = inttoptr i64 %new_node_ptr.9 to i64*
  %ptr_CInt.13 = bitcast i64* %new_node_ptr.10 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] [i64 1] }>, <{ i64, [1 x i64] }>* %ptr_CInt.13, align 1
  %new_node_ptr.16 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.17 = inttoptr i64 %new_node_ptr.16 to i64*
  %ptr_CInt.20 = bitcast i64* %new_node_ptr.17 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] [i64 1000] }>, <{ i64, [1 x i64] }>* %ptr_CInt.20, align 1
  %new_node_ptr.23 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [2 x i64*] }>* getelementptr inbounds (<{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.24 = inttoptr i64 %new_node_ptr.23 to i64*
  %node_Fupto.25 = insertvalue <{ i64, [2 x i64*] }> <{ i64 1, [2 x i64*] undef }>, i64* %new_node_ptr.10, 1, 0
  %node_Fupto.26 = insertvalue <{ i64, [2 x i64*] }> %node_Fupto.25, i64* %new_node_ptr.17, 1, 1
  %tag.27 = extractvalue <{ i64, [2 x i64*] }> %node_Fupto.26, 0
  %ptr_Fupto.28 = bitcast i64* %new_node_ptr.24 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node_Fupto.26, <{ i64, [2 x i64*] }>* %ptr_Fupto.28, align 1
  %new_node_ptr.31 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [2 x i64*] }>* getelementptr inbounds (<{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.32 = inttoptr i64 %new_node_ptr.31 to i64*
  %node_Fsum.33 = insertvalue <{ i64, [2 x i64*] }> <{ i64 2, [2 x i64*] undef }>, i64* %new_node_ptr.3, 1, 0
  %node_Fsum.34 = insertvalue <{ i64, [2 x i64*] }> %node_Fsum.33, i64* %new_node_ptr.24, 1, 1
  %tag.35 = extractvalue <{ i64, [2 x i64*] }> %node_Fsum.34, 0
  %ptr_Fsum.36 = bitcast i64* %new_node_ptr.32 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node_Fsum.34, <{ i64, [2 x i64*] }>* %ptr_Fsum.36, align 1
  %tag.37 = load i64, i64* %new_node_ptr.32, align 1
  %ptr_Fsum.38 = bitcast i64* %new_node_ptr.32 to <{ i64, [2 x i64*] }>*
  %node_Fsum.39 = load <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %ptr_Fsum.38, align 1
  %p15 = extractvalue <{ i64, [2 x i64*] }> %node_Fsum.39, 1, 0
  %p16 = extractvalue <{ i64, [2 x i64*] }> %node_Fsum.39, 1, 1
  %"n13'.40" = tail call fastcc i64 @sum(i64* %p15, i64* %p16)
  %result.grinMain.41 = call i64 @_prim_int_print(i64 %"n13'.40")
  ret i64 %result.grinMain.41

error_block:                                      ; No predecessors!
  %error_result.42 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

define private fastcc i64 @sum(i64* %p10, i64* %p11) #0 {
sum.entry:
  %tag.43 = load i64, i64* %p11, align 1
  %ptr_Fupto.44 = bitcast i64* %p11 to <{ i64, [2 x i64*] }>*
  %node_Fupto.45 = load <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %ptr_Fupto.44, align 1
  %p17 = extractvalue <{ i64, [2 x i64*] }> %node_Fupto.45, 1, 0
  %p18 = extractvalue <{ i64, [2 x i64*] }> %node_Fupto.45, 1, 1
  %tag.46 = load i64, i64* %p17, align 1
  %ptr_CInt.47 = bitcast i64* %p17 to <{ i64, [1 x i64] }>*
  %node_CInt.48 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.47, align 1
  %"n2'" = extractvalue <{ i64, [1 x i64] }> %node_CInt.48, 1, 0
  %tag.49 = load i64, i64* %p18, align 1
  %ptr_CInt.50 = bitcast i64* %p18 to <{ i64, [1 x i64] }>*
  %node_CInt.51 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.50, align 1
  %"n3'" = extractvalue <{ i64, [1 x i64] }> %node_CInt.51, 1, 0
  %"b1'.52" = icmp sgt i64 %"n2'", %"n3'"
  switch i1 %"b1'.52", label %error_block [
    i1 true, label %block.bool_True.53
    i1 false, label %block.bool_False.57
  ]

block.bool_True.53:                               ; preds = %sum.entry
  %tag.54 = load i64, i64* %p10, align 1
  %ptr_CInt.55 = bitcast i64* %p10 to <{ i64, [1 x i64] }>*
  %node_CInt.56 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.55, align 1
  %"n14'" = extractvalue <{ i64, [1 x i64] }> %node_CInt.56, 1, 0
  br label %block.exit.93

block.bool_False.57:                              ; preds = %sum.entry
  %"n4'.58" = add i64 %"n2'", 1
  %new_node_ptr.61 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.62 = inttoptr i64 %new_node_ptr.61 to i64*
  %node_CInt.63 = insertvalue <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] undef }>, i64 %"n4'.58", 1, 0
  %tag.64 = extractvalue <{ i64, [1 x i64] }> %node_CInt.63, 0
  %ptr_CInt.65 = bitcast i64* %new_node_ptr.62 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> %node_CInt.63, <{ i64, [1 x i64] }>* %ptr_CInt.65, align 1
  %new_node_ptr.68 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [2 x i64*] }>* getelementptr inbounds (<{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.69 = inttoptr i64 %new_node_ptr.68 to i64*
  %node_Fupto.70 = insertvalue <{ i64, [2 x i64*] }> <{ i64 1, [2 x i64*] undef }>, i64* %new_node_ptr.62, 1, 0
  %node_Fupto.71 = insertvalue <{ i64, [2 x i64*] }> %node_Fupto.70, i64* %p18, 1, 1
  %tag.72 = extractvalue <{ i64, [2 x i64*] }> %node_Fupto.71, 0
  %ptr_Fupto.73 = bitcast i64* %new_node_ptr.69 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node_Fupto.71, <{ i64, [2 x i64*] }>* %ptr_Fupto.73, align 1
  %node_CCons.74 = insertvalue <{ i64, [2 x i64*] }> <{ i64 4, [2 x i64*] undef }>, i64* %p17, 1, 0
  %node_CCons.75 = insertvalue <{ i64, [2 x i64*] }> %node_CCons.74, i64* %new_node_ptr.69, 1, 1
  %node_CCons.76 = insertvalue <{ i64, [2 x i64*] }> <{ i64 4, [2 x i64*] undef }>, i64* %p17, 1, 0
  %node_CCons.77 = insertvalue <{ i64, [2 x i64*] }> %node_CCons.76, i64* %new_node_ptr.69, 1, 1
  %p12_2 = extractvalue <{ i64, [2 x i64*] }> %node_CCons.77, 1, 0
  %p13_2 = extractvalue <{ i64, [2 x i64*] }> %node_CCons.77, 1, 1
  %tag.78 = load i64, i64* %p10, align 1
  %ptr_CInt.79 = bitcast i64* %p10 to <{ i64, [1 x i64] }>*
  %node_CInt.80 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.79, align 1
  %"n5'_2" = extractvalue <{ i64, [1 x i64] }> %node_CInt.80, 1, 0
  %tag.81 = load i64, i64* %p12_2, align 1
  %ptr_CInt.82 = bitcast i64* %p12_2 to <{ i64, [1 x i64] }>*
  %node_CInt.83 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.82, align 1
  %"n6'_2" = extractvalue <{ i64, [1 x i64] }> %node_CInt.83, 1, 0
  %"n7'_2.84" = add i64 %"n5'_2", %"n6'_2"
  %new_node_ptr.87 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.88 = inttoptr i64 %new_node_ptr.87 to i64*
  %node_CInt.89 = insertvalue <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] undef }>, i64 %"n7'_2.84", 1, 0
  %tag.90 = extractvalue <{ i64, [1 x i64] }> %node_CInt.89, 0
  %ptr_CInt.91 = bitcast i64* %new_node_ptr.88 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> %node_CInt.89, <{ i64, [1 x i64] }>* %ptr_CInt.91, align 1
  %result.bool_False.92 = tail call fastcc i64 @sum(i64* %new_node_ptr.88, i64* %p13_2)
  br label %block.exit.93

block.exit.93:                                    ; preds = %block.bool_False.57, %block.bool_True.53
  %result.sum.94 = phi i64 [ %"n14'", %block.bool_True.53 ], [ %result.bool_False.92, %block.bool_False.57 ]
  ret i64 %result.sum.94

error_block:                                      ; preds = %sum.entry
  %error_result.95 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

attributes #0 = { "no-jump-tables"="true" }
