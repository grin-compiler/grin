; ModuleID = 'basic'
source_filename = "<string>"

@_heap_ptr_ = global i64 0

declare void @_prim_int_print(i64)

define void @grinMain() #0 {
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
  call void @_prim_int_print(i64 %"n13'.40")
  ret void

error_block:                                      ; No predecessors!
  tail call void @_prim_int_print(i64 666)
  unreachable
}

define private fastcc i64 @sum(i64* %p10, i64* %p11) #0 {
sum.entry:
  %tag.41 = load i64, i64* %p11, align 1
  %ptr_Fupto.42 = bitcast i64* %p11 to <{ i64, [2 x i64*] }>*
  %node_Fupto.43 = load <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %ptr_Fupto.42, align 1
  %p17 = extractvalue <{ i64, [2 x i64*] }> %node_Fupto.43, 1, 0
  %p18 = extractvalue <{ i64, [2 x i64*] }> %node_Fupto.43, 1, 1
  %tag.44 = load i64, i64* %p17, align 1
  %ptr_CInt.45 = bitcast i64* %p17 to <{ i64, [1 x i64] }>*
  %node_CInt.46 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.45, align 1
  %"n2'" = extractvalue <{ i64, [1 x i64] }> %node_CInt.46, 1, 0
  %tag.47 = load i64, i64* %p18, align 1
  %ptr_CInt.48 = bitcast i64* %p18 to <{ i64, [1 x i64] }>*
  %node_CInt.49 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.48, align 1
  %"n3'" = extractvalue <{ i64, [1 x i64] }> %node_CInt.49, 1, 0
  %"b1'.50" = icmp sgt i64 %"n2'", %"n3'"
  switch i1 %"b1'.50", label %error_block [
    i1 true, label %block.bool_True.51
    i1 false, label %block.bool_False.55
  ]

block.bool_True.51:                               ; preds = %sum.entry
  %tag.52 = load i64, i64* %p10, align 1
  %ptr_CInt.53 = bitcast i64* %p10 to <{ i64, [1 x i64] }>*
  %node_CInt.54 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.53, align 1
  %"n14'" = extractvalue <{ i64, [1 x i64] }> %node_CInt.54, 1, 0
  br label %block.exit.91

block.bool_False.55:                              ; preds = %sum.entry
  %"n4'.56" = add i64 %"n2'", 1
  %new_node_ptr.59 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.60 = inttoptr i64 %new_node_ptr.59 to i64*
  %node_CInt.61 = insertvalue <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] undef }>, i64 %"n4'.56", 1, 0
  %tag.62 = extractvalue <{ i64, [1 x i64] }> %node_CInt.61, 0
  %ptr_CInt.63 = bitcast i64* %new_node_ptr.60 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> %node_CInt.61, <{ i64, [1 x i64] }>* %ptr_CInt.63, align 1
  %new_node_ptr.66 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [2 x i64*] }>* getelementptr inbounds (<{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.67 = inttoptr i64 %new_node_ptr.66 to i64*
  %node_Fupto.68 = insertvalue <{ i64, [2 x i64*] }> <{ i64 1, [2 x i64*] undef }>, i64* %new_node_ptr.60, 1, 0
  %node_Fupto.69 = insertvalue <{ i64, [2 x i64*] }> %node_Fupto.68, i64* %p18, 1, 1
  %tag.70 = extractvalue <{ i64, [2 x i64*] }> %node_Fupto.69, 0
  %ptr_Fupto.71 = bitcast i64* %new_node_ptr.67 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node_Fupto.69, <{ i64, [2 x i64*] }>* %ptr_Fupto.71, align 1
  %node_CCons.72 = insertvalue <{ i64, [2 x i64*] }> <{ i64 4, [2 x i64*] undef }>, i64* %p17, 1, 0
  %node_CCons.73 = insertvalue <{ i64, [2 x i64*] }> %node_CCons.72, i64* %new_node_ptr.67, 1, 1
  %node_CCons.74 = insertvalue <{ i64, [2 x i64*] }> <{ i64 4, [2 x i64*] undef }>, i64* %p17, 1, 0
  %node_CCons.75 = insertvalue <{ i64, [2 x i64*] }> %node_CCons.74, i64* %new_node_ptr.67, 1, 1
  %p12_2 = extractvalue <{ i64, [2 x i64*] }> %node_CCons.75, 1, 0
  %p13_2 = extractvalue <{ i64, [2 x i64*] }> %node_CCons.75, 1, 1
  %tag.76 = load i64, i64* %p10, align 1
  %ptr_CInt.77 = bitcast i64* %p10 to <{ i64, [1 x i64] }>*
  %node_CInt.78 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.77, align 1
  %"n5'_2" = extractvalue <{ i64, [1 x i64] }> %node_CInt.78, 1, 0
  %tag.79 = load i64, i64* %p12_2, align 1
  %ptr_CInt.80 = bitcast i64* %p12_2 to <{ i64, [1 x i64] }>*
  %node_CInt.81 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %ptr_CInt.80, align 1
  %"n6'_2" = extractvalue <{ i64, [1 x i64] }> %node_CInt.81, 1, 0
  %"n7'_2.82" = add i64 %"n5'_2", %"n6'_2"
  %new_node_ptr.85 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %new_node_ptr.86 = inttoptr i64 %new_node_ptr.85 to i64*
  %node_CInt.87 = insertvalue <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] undef }>, i64 %"n7'_2.82", 1, 0
  %tag.88 = extractvalue <{ i64, [1 x i64] }> %node_CInt.87, 0
  %ptr_CInt.89 = bitcast i64* %new_node_ptr.86 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> %node_CInt.87, <{ i64, [1 x i64] }>* %ptr_CInt.89, align 1
  %result.bool_False.90 = tail call fastcc i64 @sum(i64* %new_node_ptr.86, i64* %p13_2)
  br label %block.exit.91

block.exit.91:                                    ; preds = %block.bool_False.55, %block.bool_True.51
  %result.sum.92 = phi i64 [ %"n14'", %block.bool_True.51 ], [ %result.bool_False.90, %block.bool_False.55 ]
  ret i64 %result.sum.92

error_block:                                      ; preds = %sum.entry
  tail call void @_prim_int_print(i64 666)
  unreachable
}

attributes #0 = { "no-jump-tables"="true" }
