; ModuleID = 'basic'
source_filename = "<string>"

@_heap_ptr_ = global i64 0

declare i64 @_prim_int_print(i64)

define i64 @grinMain() #0 {
grinMain.entry:
  %heap_ptr.2 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %heap_ptr.3 = inttoptr i64 %heap_ptr.2 to i64*
  %nodeAddress.7 = bitcast i64* %heap_ptr.3 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> zeroinitializer, <{ i64, [1 x i64] }>* %nodeAddress.7, align 1
  %heap_ptr.10 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %heap_ptr.11 = inttoptr i64 %heap_ptr.10 to i64*
  %nodeAddress.15 = bitcast i64* %heap_ptr.11 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] [i64 1] }>, <{ i64, [1 x i64] }>* %nodeAddress.15, align 1
  %heap_ptr.18 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %heap_ptr.19 = inttoptr i64 %heap_ptr.18 to i64*
  %nodeAddress.23 = bitcast i64* %heap_ptr.19 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] [i64 1000] }>, <{ i64, [1 x i64] }>* %nodeAddress.23, align 1
  %heap_ptr.26 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [2 x i64*] }>* getelementptr inbounds (<{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* null, i32 1) to i64) monotonic
  %heap_ptr.27 = inttoptr i64 %heap_ptr.26 to i64*
  %node.29 = insertvalue <{ i64, [2 x i64*] }> <{ i64 1, [2 x i64*] undef }>, i64* %heap_ptr.11, 1, 0
  %node.30 = insertvalue <{ i64, [2 x i64*] }> %node.29, i64* %heap_ptr.19, 1, 1
  %tag.31 = extractvalue <{ i64, [2 x i64*] }> %node.30, 0
  %nodeAddress.32 = bitcast i64* %heap_ptr.27 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node.30, <{ i64, [2 x i64*] }>* %nodeAddress.32, align 1
  %heap_ptr.35 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [2 x i64*] }>* getelementptr inbounds (<{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* null, i32 1) to i64) monotonic
  %heap_ptr.36 = inttoptr i64 %heap_ptr.35 to i64*
  %node.38 = insertvalue <{ i64, [2 x i64*] }> <{ i64 2, [2 x i64*] undef }>, i64* %heap_ptr.3, 1, 0
  %node.39 = insertvalue <{ i64, [2 x i64*] }> %node.38, i64* %heap_ptr.27, 1, 1
  %tag.40 = extractvalue <{ i64, [2 x i64*] }> %node.39, 0
  %nodeAddress.41 = bitcast i64* %heap_ptr.36 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node.39, <{ i64, [2 x i64*] }>* %nodeAddress.41, align 1
  %tag.42 = load i64, i64* %heap_ptr.36, align 1
  %nodeAddress.43 = bitcast i64* %heap_ptr.36 to <{ i64, [2 x i64*] }>*
  %node.44 = load <{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* %nodeAddress.43, align 1
  %p15 = extractvalue <{ i64, [2 x i64*] }> %node.44, 1, 0
  %p16 = extractvalue <{ i64, [2 x i64*] }> %node.44, 1, 1
  %"n13'.45" = tail call fastcc i64 @sum(i64* %p15, i64* %p16)
  %grinMain_result.46 = call i64 @_prim_int_print(i64 %"n13'.45")
  ret i64 %grinMain_result.46

error_block:                                      ; No predecessors!
  %error_result.47 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

define private fastcc i64 @sum(i64* %p10, i64* %p11) #0 {
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
  br label %switch.exit.105

switch.bool_False.64:                             ; preds = %sum.entry
  %"n4'.65" = add i64 %"n2'", 1
  %heap_ptr.68 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %heap_ptr.69 = inttoptr i64 %heap_ptr.68 to i64*
  %node.71 = insertvalue <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] undef }>, i64 %"n4'.65", 1, 0
  %tag.72 = extractvalue <{ i64, [1 x i64] }> %node.71, 0
  %nodeAddress.73 = bitcast i64* %heap_ptr.69 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> %node.71, <{ i64, [1 x i64] }>* %nodeAddress.73, align 1
  %heap_ptr.76 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [2 x i64*] }>* getelementptr inbounds (<{ i64, [2 x i64*] }>, <{ i64, [2 x i64*] }>* null, i32 1) to i64) monotonic
  %heap_ptr.77 = inttoptr i64 %heap_ptr.76 to i64*
  %node.79 = insertvalue <{ i64, [2 x i64*] }> <{ i64 1, [2 x i64*] undef }>, i64* %heap_ptr.69, 1, 0
  %node.80 = insertvalue <{ i64, [2 x i64*] }> %node.79, i64* %p18, 1, 1
  %tag.81 = extractvalue <{ i64, [2 x i64*] }> %node.80, 0
  %nodeAddress.82 = bitcast i64* %heap_ptr.77 to <{ i64, [2 x i64*] }>*
  store <{ i64, [2 x i64*] }> %node.80, <{ i64, [2 x i64*] }>* %nodeAddress.82, align 1
  %node.84 = insertvalue <{ i64, [2 x i64*] }> <{ i64 4, [2 x i64*] undef }>, i64* %p17, 1, 0
  %node.85 = insertvalue <{ i64, [2 x i64*] }> %node.84, i64* %heap_ptr.77, 1, 1
  %node.87 = insertvalue <{ i64, [2 x i64*] }> <{ i64 4, [2 x i64*] undef }>, i64* %p17, 1, 0
  %node.88 = insertvalue <{ i64, [2 x i64*] }> %node.87, i64* %heap_ptr.77, 1, 1
  %p12_2 = extractvalue <{ i64, [2 x i64*] }> %node.88, 1, 0
  %p13_2 = extractvalue <{ i64, [2 x i64*] }> %node.88, 1, 1
  %tag.89 = load i64, i64* %p10, align 1
  %nodeAddress.90 = bitcast i64* %p10 to <{ i64, [1 x i64] }>*
  %node.91 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %nodeAddress.90, align 1
  %"n5'_2" = extractvalue <{ i64, [1 x i64] }> %node.91, 1, 0
  %tag.92 = load i64, i64* %p12_2, align 1
  %nodeAddress.93 = bitcast i64* %p12_2 to <{ i64, [1 x i64] }>*
  %node.94 = load <{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* %nodeAddress.93, align 1
  %"n6'_2" = extractvalue <{ i64, [1 x i64] }> %node.94, 1, 0
  %"n7'_2.95" = add i64 %"n5'_2", %"n6'_2"
  %heap_ptr.98 = atomicrmw add i64* @_heap_ptr_, i64 ptrtoint (<{ i64, [1 x i64] }>* getelementptr inbounds (<{ i64, [1 x i64] }>, <{ i64, [1 x i64] }>* null, i32 1) to i64) monotonic
  %heap_ptr.99 = inttoptr i64 %heap_ptr.98 to i64*
  %node.101 = insertvalue <{ i64, [1 x i64] }> <{ i64 0, [1 x i64] undef }>, i64 %"n7'_2.95", 1, 0
  %tag.102 = extractvalue <{ i64, [1 x i64] }> %node.101, 0
  %nodeAddress.103 = bitcast i64* %heap_ptr.99 to <{ i64, [1 x i64] }>*
  store <{ i64, [1 x i64] }> %node.101, <{ i64, [1 x i64] }>* %nodeAddress.103, align 1
  %altResult.104 = tail call fastcc i64 @sum(i64* %heap_ptr.99, i64* %p13_2)
  br label %switch.exit.105

switch.exit.105:                                  ; preds = %switch.bool_False.64, %switch.bool_True.58
  %sum_result.106 = phi i64 [ %"n14'", %switch.bool_True.58 ], [ %altResult.104, %switch.bool_False.64 ]
  ret i64 %sum_result.106

error_block:                                      ; preds = %sum.entry
  %error_result.107 = tail call i64 @_prim_int_print(i64 666)
  unreachable
}

attributes #0 = { "no-jump-tables"="true" }
