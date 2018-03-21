
;;;; LLVM Code ;;;;
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux"



;;;; LLVM Code ;;;;
declare ccc i8* @memcpy$def(i8*, i8*, i64)



;;;; LLVM Code ;;;;
declare ccc i8* @memmove$def(i8*, i8*, i64)



;;;; LLVM Code ;;;;
declare ccc i8* @memset$def(i8*, i64, i64)



;;;; LLVM Code ;;;;
declare ccc i64 @newSpark$def(i8*, i8*)



;;;; LLVM Code ;;;;
!0 = !{!"root"}
!1 = !{!"top", !0}
!2 = !{!"stack", !1}
!3 = !{!"heap", !1}
!4 = !{!"rx", !3}
!5 = !{!"base", !1}



;;;; LLVM Code ;;;;



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtrModule4_bytes_struct = type <{[5 x i8]}>
@SumSimpleBasic_zdtrModule4_bytes$def = internal constant %SumSimpleBasic_zdtrModule4_bytes_struct<{[5 x i8] [i8 109, i8 97, i8 105, i8 110, i8 0]}>, align 1
@SumSimpleBasic_zdtrModule4_bytes = alias i8, bitcast (%SumSimpleBasic_zdtrModule4_bytes_struct* @SumSimpleBasic_zdtrModule4_bytes$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtrModule3_closure_struct = type <{i64, i64}>
@SumSimpleBasic_zdtrModule3_closure$def = internal global %SumSimpleBasic_zdtrModule3_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TrNameS_con_info to i64), i64 ptrtoint (%SumSimpleBasic_zdtrModule4_bytes_struct* @SumSimpleBasic_zdtrModule4_bytes$def to i64)}>
@SumSimpleBasic_zdtrModule3_closure = alias i8, bitcast (%SumSimpleBasic_zdtrModule3_closure_struct* @SumSimpleBasic_zdtrModule3_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtrModule2_bytes_struct = type <{[15 x i8]}>
@SumSimpleBasic_zdtrModule2_bytes$def = internal constant %SumSimpleBasic_zdtrModule2_bytes_struct<{[15 x i8] [i8 83, i8 117, i8 109, i8 83, i8 105, i8 109, i8 112, i8 108, i8 101, i8 66, i8 97, i8 115, i8 105, i8 99, i8 0]}>, align 1
@SumSimpleBasic_zdtrModule2_bytes = alias i8, bitcast (%SumSimpleBasic_zdtrModule2_bytes_struct* @SumSimpleBasic_zdtrModule2_bytes$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtrModule1_closure_struct = type <{i64, i64}>
@SumSimpleBasic_zdtrModule1_closure$def = internal global %SumSimpleBasic_zdtrModule1_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TrNameS_con_info to i64), i64 ptrtoint (%SumSimpleBasic_zdtrModule2_bytes_struct* @SumSimpleBasic_zdtrModule2_bytes$def to i64)}>
@SumSimpleBasic_zdtrModule1_closure = alias i8, bitcast (%SumSimpleBasic_zdtrModule1_closure_struct* @SumSimpleBasic_zdtrModule1_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtrModule_closure_struct = type <{i64, i64, i64, i64}>
@SumSimpleBasic_zdtrModule_closure$def = internal global %SumSimpleBasic_zdtrModule_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_Module_con_info to i64), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtrModule3_closure_struct* @SumSimpleBasic_zdtrModule3_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtrModule1_closure_struct* @SumSimpleBasic_zdtrModule1_closure$def to i64),i64 1), i64 3}>
@SumSimpleBasic_zdtrModule_closure = alias i8, bitcast (%SumSimpleBasic_zdtrModule_closure_struct* @SumSimpleBasic_zdtrModule_closure$def to i8*)



;;;; LLVM Code ;;;;
%r3k9_closure_struct = type <{i64, i64}>
@r3k9_closure$def = internal global %r3k9_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_KindRepVar_con_info to i64), i64 0}>
@r3k9_closure = internal alias i8, bitcast (%r3k9_closure_struct* @r3k9_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtcList2_bytes_struct = type <{[5 x i8]}>
@SumSimpleBasic_zdtcList2_bytes$def = internal constant %SumSimpleBasic_zdtcList2_bytes_struct<{[5 x i8] [i8 76, i8 105, i8 115, i8 116, i8 0]}>, align 1
@SumSimpleBasic_zdtcList2_bytes = alias i8, bitcast (%SumSimpleBasic_zdtcList2_bytes_struct* @SumSimpleBasic_zdtcList2_bytes$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtcList1_closure_struct = type <{i64, i64}>
@SumSimpleBasic_zdtcList1_closure$def = internal global %SumSimpleBasic_zdtcList1_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TrNameS_con_info to i64), i64 ptrtoint (%SumSimpleBasic_zdtcList2_bytes_struct* @SumSimpleBasic_zdtcList2_bytes$def to i64)}>
@SumSimpleBasic_zdtcList1_closure = alias i8, bitcast (%SumSimpleBasic_zdtcList1_closure_struct* @SumSimpleBasic_zdtcList1_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtcList_closure_struct = type <{i64, i64, i64, i64, i64, i64, i64, i64}>
@SumSimpleBasic_zdtcList_closure$def = internal global %SumSimpleBasic_zdtcList_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TyCon_con_info to i64), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtrModule_closure_struct* @SumSimpleBasic_zdtrModule_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtcList1_closure_struct* @SumSimpleBasic_zdtcList1_closure$def to i64),i64 1), i64 ptrtoint (i8* @ghczmprim_GHCziTypes_krepzdztArrzt_closure to i64), i64 8125128733883036046, i64 8522174167308563467, i64 0, i64 3}>
@SumSimpleBasic_zdtcList_closure = alias i8, bitcast (%SumSimpleBasic_zdtcList_closure_struct* @SumSimpleBasic_zdtcList_closure$def to i8*)



;;;; LLVM Code ;;;;
%r3ka_closure_struct = type <{i64, i64, i64, i64}>
@r3ka_closure$def = internal global %r3ka_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_ZC_con_info to i64), i64 add (i64 ptrtoint (%r3k9_closure_struct* @r3k9_closure$def to i64),i64 2), i64 add (i64 ptrtoint (i8* @ghczmprim_GHCziTypes_ZMZN_closure to i64),i64 1), i64 3}>
@r3ka_closure = internal alias i8, bitcast (%r3ka_closure_struct* @r3ka_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtczqNil1_closure_struct = type <{i64, i64, i64, i64}>
@SumSimpleBasic_zdtczqNil1_closure$def = internal global %SumSimpleBasic_zdtczqNil1_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_KindRepTyConApp_con_info to i64), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtcList_closure_struct* @SumSimpleBasic_zdtcList_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%r3ka_closure_struct* @r3ka_closure$def to i64),i64 2), i64 3}>
@SumSimpleBasic_zdtczqNil1_closure = alias i8, bitcast (%SumSimpleBasic_zdtczqNil1_closure_struct* @SumSimpleBasic_zdtczqNil1_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtczqNil3_bytes_struct = type <{[5 x i8]}>
@SumSimpleBasic_zdtczqNil3_bytes$def = internal constant %SumSimpleBasic_zdtczqNil3_bytes_struct<{[5 x i8] [i8 39, i8 78, i8 105, i8 108, i8 0]}>, align 1
@SumSimpleBasic_zdtczqNil3_bytes = alias i8, bitcast (%SumSimpleBasic_zdtczqNil3_bytes_struct* @SumSimpleBasic_zdtczqNil3_bytes$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtczqNil2_closure_struct = type <{i64, i64}>
@SumSimpleBasic_zdtczqNil2_closure$def = internal global %SumSimpleBasic_zdtczqNil2_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TrNameS_con_info to i64), i64 ptrtoint (%SumSimpleBasic_zdtczqNil3_bytes_struct* @SumSimpleBasic_zdtczqNil3_bytes$def to i64)}>
@SumSimpleBasic_zdtczqNil2_closure = alias i8, bitcast (%SumSimpleBasic_zdtczqNil2_closure_struct* @SumSimpleBasic_zdtczqNil2_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtczqNil_closure_struct = type <{i64, i64, i64, i64, i64, i64, i64, i64}>
@SumSimpleBasic_zdtczqNil_closure$def = internal global %SumSimpleBasic_zdtczqNil_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TyCon_con_info to i64), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtrModule_closure_struct* @SumSimpleBasic_zdtrModule_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtczqNil2_closure_struct* @SumSimpleBasic_zdtczqNil2_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtczqNil1_closure_struct* @SumSimpleBasic_zdtczqNil1_closure$def to i64),i64 1), i64 3225848470370281545, i64 8987752080532592311, i64 1, i64 3}>
@SumSimpleBasic_zdtczqNil_closure = alias i8, bitcast (%SumSimpleBasic_zdtczqNil_closure_struct* @SumSimpleBasic_zdtczqNil_closure$def to i8*)



;;;; LLVM Code ;;;;
%r3kb_closure_struct = type <{i64, i64, i64, i64}>
@r3kb_closure$def = internal global %r3kb_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_KindRepFun_con_info to i64), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtczqNil1_closure_struct* @SumSimpleBasic_zdtczqNil1_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtczqNil1_closure_struct* @SumSimpleBasic_zdtczqNil1_closure$def to i64),i64 1), i64 3}>
@r3kb_closure = internal alias i8, bitcast (%r3kb_closure_struct* @r3kb_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtczqCons1_closure_struct = type <{i64, i64, i64, i64}>
@SumSimpleBasic_zdtczqCons1_closure$def = internal global %SumSimpleBasic_zdtczqCons1_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_KindRepFun_con_info to i64), i64 add (i64 ptrtoint (%r3k9_closure_struct* @r3k9_closure$def to i64),i64 2), i64 add (i64 ptrtoint (%r3kb_closure_struct* @r3kb_closure$def to i64),i64 4), i64 3}>
@SumSimpleBasic_zdtczqCons1_closure = alias i8, bitcast (%SumSimpleBasic_zdtczqCons1_closure_struct* @SumSimpleBasic_zdtczqCons1_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtczqCons3_bytes_struct = type <{[6 x i8]}>
@SumSimpleBasic_zdtczqCons3_bytes$def = internal constant %SumSimpleBasic_zdtczqCons3_bytes_struct<{[6 x i8] [i8 39, i8 67, i8 111, i8 110, i8 115, i8 0]}>, align 1
@SumSimpleBasic_zdtczqCons3_bytes = alias i8, bitcast (%SumSimpleBasic_zdtczqCons3_bytes_struct* @SumSimpleBasic_zdtczqCons3_bytes$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtczqCons2_closure_struct = type <{i64, i64}>
@SumSimpleBasic_zdtczqCons2_closure$def = internal global %SumSimpleBasic_zdtczqCons2_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TrNameS_con_info to i64), i64 ptrtoint (%SumSimpleBasic_zdtczqCons3_bytes_struct* @SumSimpleBasic_zdtczqCons3_bytes$def to i64)}>
@SumSimpleBasic_zdtczqCons2_closure = alias i8, bitcast (%SumSimpleBasic_zdtczqCons2_closure_struct* @SumSimpleBasic_zdtczqCons2_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdtczqCons_closure_struct = type <{i64, i64, i64, i64, i64, i64, i64, i64}>
@SumSimpleBasic_zdtczqCons_closure$def = internal global %SumSimpleBasic_zdtczqCons_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TyCon_con_info to i64), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtrModule_closure_struct* @SumSimpleBasic_zdtrModule_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtczqCons2_closure_struct* @SumSimpleBasic_zdtczqCons2_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%SumSimpleBasic_zdtczqCons1_closure_struct* @SumSimpleBasic_zdtczqCons1_closure$def to i64),i64 4), i64 -7205385302398664762, i64 6836440343659544598, i64 1, i64 3}>
@SumSimpleBasic_zdtczqCons_closure = alias i8, bitcast (%SumSimpleBasic_zdtczqCons_closure_struct* @SumSimpleBasic_zdtczqCons_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdwsum_closure_struct = type <{i64}>
@SumSimpleBasic_zdwsum_closure$def = internal global %SumSimpleBasic_zdwsum_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_zdwsum_info$def to i64)}>
@SumSimpleBasic_zdwsum_closure = alias i8, bitcast (%SumSimpleBasic_zdwsum_closure_struct* @SumSimpleBasic_zdwsum_closure$def to i8*)



;;;; LLVM Code ;;;;
@SumSimpleBasic_zdwsum_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_zdwsum_info$def to i8*)
define ghccc void @SumSimpleBasic_zdwsum_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64}><{i64 4294967301, i64 0, i64 14}>
{
c3kG:
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %R2_Var = alloca i64, i32 1
  store i64 %R2_Arg, i64* %R2_Var
  %ln3l3 = load i64*, i64** %Sp_Var
  %ln3l4 = getelementptr inbounds i64, i64* %ln3l3, i32 -2
  %ln3l5 = ptrtoint i64* %ln3l4 to i64
  %ln3l6 = icmp ult i64 %ln3l5, %SpLim_Arg
  %ln3l8 = call ccc i1 (i1, i1) @llvm.expect.i1( i1 %ln3l6, i1 0 )
  br i1 %ln3l8, label %c3kH, label %c3kI
c3kI:
  %ln3la = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3kz_info$def to i64
  %ln3l9 = load i64*, i64** %Sp_Var
  %ln3lb = getelementptr inbounds i64, i64* %ln3l9, i32 -1
  store i64 %ln3la, i64* %ln3lb, !tbaa !2
  %ln3lc = load i64, i64* %R2_Var
  store i64 %ln3lc, i64* %R1_Var
  %ln3ld = load i64*, i64** %Sp_Var
  %ln3le = getelementptr inbounds i64, i64* %ln3ld, i32 -1
  %ln3lf = ptrtoint i64* %ln3le to i64
  %ln3lg = inttoptr i64 %ln3lf to i64*
  store i64* %ln3lg, i64** %Sp_Var
  %ln3lh = load i64, i64* %R1_Var
  %ln3li = and i64 %ln3lh, 7
  %ln3lj = icmp ne i64 %ln3li, 0
  br i1 %ln3lj, label %u3l2, label %c3kA
c3kA:
  %ln3ll = load i64, i64* %R1_Var
  %ln3lm = inttoptr i64 %ln3ll to i64*
  %ln3ln = load i64, i64* %ln3lm, !tbaa !4
  %ln3lo = inttoptr i64 %ln3ln to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3lp = load i64*, i64** %Sp_Var
  %ln3lq = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3lo( i64* %Base_Arg, i64* %ln3lp, i64* %Hp_Arg, i64 %ln3lq, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
u3l2:
  %ln3lr = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3kz_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3ls = load i64*, i64** %Sp_Var
  %ln3lt = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3lr( i64* %Base_Arg, i64* %ln3ls, i64* %Hp_Arg, i64 %ln3lt, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3kH:
  %ln3lu = load i64, i64* %R2_Var
  store i64 %ln3lu, i64* %R2_Var
  %ln3lv = ptrtoint %SumSimpleBasic_zdwsum_closure_struct* @SumSimpleBasic_zdwsum_closure$def to i64
  store i64 %ln3lv, i64* %R1_Var
  %ln3lw = getelementptr inbounds i64, i64* %Base_Arg, i32 -1
  %ln3lx = bitcast i64* %ln3lw to i64*
  %ln3ly = load i64, i64* %ln3lx, !tbaa !5
  %ln3lz = inttoptr i64 %ln3ly to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3lA = load i64*, i64** %Sp_Var
  %ln3lB = load i64, i64* %R1_Var
  %ln3lC = load i64, i64* %R2_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3lz( i64* %Base_Arg, i64* %ln3lA, i64* %Hp_Arg, i64 %ln3lB, i64 %ln3lC, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}
declare ccc i1 @llvm.expect.i1(i1, i1)



;;;; LLVM Code ;;;;
@c3kz_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3kz_info$def to i8*)
define internal ghccc void @c3kz_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 0, i64 30}>
{
c3kz:
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %ls3kg = alloca i64, i32 1
  %ln3lD = load i64, i64* %R1_Var
  %ln3lE = and i64 %ln3lD, 7
  switch i64 %ln3lE, label %c3kD [i64 1, label %c3kD
                                  i64 2, label %c3kE]
c3kD:
  store i64 0, i64* %R1_Var
  %ln3lF = load i64*, i64** %Sp_Var
  %ln3lG = getelementptr inbounds i64, i64* %ln3lF, i32 1
  %ln3lH = ptrtoint i64* %ln3lG to i64
  %ln3lI = inttoptr i64 %ln3lH to i64*
  store i64* %ln3lI, i64** %Sp_Var
  %ln3lJ = load i64*, i64** %Sp_Var
  %ln3lK = getelementptr inbounds i64, i64* %ln3lJ, i32 0
  %ln3lL = bitcast i64* %ln3lK to i64*
  %ln3lM = load i64, i64* %ln3lL, !tbaa !2
  %ln3lN = inttoptr i64 %ln3lM to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3lO = load i64*, i64** %Sp_Var
  %ln3lP = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3lN( i64* %Base_Arg, i64* %ln3lO, i64* %Hp_Arg, i64 %ln3lP, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3kE:
  %ln3lR = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3kO_info$def to i64
  %ln3lQ = load i64*, i64** %Sp_Var
  %ln3lS = getelementptr inbounds i64, i64* %ln3lQ, i32 -1
  store i64 %ln3lR, i64* %ln3lS, !tbaa !2
  %ln3lV = load i64, i64* %R1_Var
  %ln3lW = add i64 %ln3lV, 14
  %ln3lX = inttoptr i64 %ln3lW to i64*
  %ln3lY = load i64, i64* %ln3lX, !tbaa !4
  store i64 %ln3lY, i64* %ls3kg
  %ln3m1 = load i64, i64* %R1_Var
  %ln3m2 = add i64 %ln3m1, 6
  %ln3m3 = inttoptr i64 %ln3m2 to i64*
  %ln3m4 = load i64, i64* %ln3m3, !tbaa !4
  store i64 %ln3m4, i64* %R1_Var
  %ln3m6 = load i64, i64* %ls3kg
  %ln3m5 = load i64*, i64** %Sp_Var
  %ln3m7 = getelementptr inbounds i64, i64* %ln3m5, i32 0
  store i64 %ln3m6, i64* %ln3m7, !tbaa !2
  %ln3m8 = load i64*, i64** %Sp_Var
  %ln3m9 = getelementptr inbounds i64, i64* %ln3m8, i32 -1
  %ln3ma = ptrtoint i64* %ln3m9 to i64
  %ln3mb = inttoptr i64 %ln3ma to i64*
  store i64* %ln3mb, i64** %Sp_Var
  %ln3mc = load i64, i64* %R1_Var
  %ln3md = and i64 %ln3mc, 7
  %ln3me = icmp ne i64 %ln3md, 0
  br i1 %ln3me, label %u3l1, label %c3kP
c3kP:
  %ln3mg = load i64, i64* %R1_Var
  %ln3mh = inttoptr i64 %ln3mg to i64*
  %ln3mi = load i64, i64* %ln3mh, !tbaa !4
  %ln3mj = inttoptr i64 %ln3mi to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3mk = load i64*, i64** %Sp_Var
  %ln3ml = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3mj( i64* %Base_Arg, i64* %ln3mk, i64* %Hp_Arg, i64 %ln3ml, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
u3l1:
  %ln3mm = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3kO_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3mn = load i64*, i64** %Sp_Var
  %ln3mo = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3mm( i64* %Base_Arg, i64* %ln3mn, i64* %Hp_Arg, i64 %ln3mo, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
@c3kO_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3kO_info$def to i8*)
define internal ghccc void @c3kO_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 1, i64 30}>
{
c3kO:
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %ln3mp = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3kT_info$def to i64
  %ln3mq = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  store i64 %ln3mp, i64* %ln3mq, !tbaa !2
  %ln3mr = getelementptr inbounds i64, i64* %Sp_Arg, i32 1
  %ln3ms = bitcast i64* %ln3mr to i64*
  %ln3mt = load i64, i64* %ln3ms, !tbaa !2
  store i64 %ln3mt, i64* %R2_Var
  %ln3mu = add i64 %R1_Arg, 7
  %ln3mv = inttoptr i64 %ln3mu to i64*
  %ln3mw = load i64, i64* %ln3mv, !tbaa !4
  %ln3mx = getelementptr inbounds i64, i64* %Sp_Arg, i32 1
  store i64 %ln3mw, i64* %ln3mx, !tbaa !2
  %ln3my = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_zdwsum_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3mz = load i64, i64* %R2_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3my( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %R1_Arg, i64 %ln3mz, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
@c3kT_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3kT_info$def to i8*)
define internal ghccc void @c3kT_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 65, i64 30}>
{
c3kT:
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %ln3mA = load i64*, i64** %Sp_Var
  %ln3mB = getelementptr inbounds i64, i64* %ln3mA, i32 1
  %ln3mC = bitcast i64* %ln3mB to i64*
  %ln3mD = load i64, i64* %ln3mC, !tbaa !2
  %ln3mE = load i64, i64* %R1_Var
  %ln3mF = add i64 %ln3mD, %ln3mE
  store i64 %ln3mF, i64* %R1_Var
  %ln3mG = load i64*, i64** %Sp_Var
  %ln3mH = getelementptr inbounds i64, i64* %ln3mG, i32 2
  %ln3mI = ptrtoint i64* %ln3mH to i64
  %ln3mJ = inttoptr i64 %ln3mI to i64*
  store i64* %ln3mJ, i64** %Sp_Var
  %ln3mK = load i64*, i64** %Sp_Var
  %ln3mL = getelementptr inbounds i64, i64* %ln3mK, i32 0
  %ln3mM = bitcast i64* %ln3mL to i64*
  %ln3mN = load i64, i64* %ln3mM, !tbaa !2
  %ln3mO = inttoptr i64 %ln3mN to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3mP = load i64*, i64** %Sp_Var
  %ln3mQ = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3mO( i64* %Base_Arg, i64* %ln3mP, i64* %Hp_Arg, i64 %ln3mQ, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
%SumSimpleBasic_zdwupto_closure_struct = type <{i64}>
@SumSimpleBasic_zdwupto_closure$def = internal global %SumSimpleBasic_zdwupto_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_zdwupto_info$def to i64)}>
@SumSimpleBasic_zdwupto_closure = alias i8, bitcast (%SumSimpleBasic_zdwupto_closure_struct* @SumSimpleBasic_zdwupto_closure$def to i8*)



;;;; LLVM Code ;;;;
@s3kp_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @s3kp_info$def to i8*)
define internal ghccc void @s3kp_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 8589934592, i64 20}>
{
c3n4:
  %R3_Var = alloca i64, i32 1
  store i64 undef, i64* %R3_Var
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %ln3nh = load i64*, i64** %Sp_Var
  %ln3ni = getelementptr inbounds i64, i64* %ln3nh, i32 -2
  %ln3nj = ptrtoint i64* %ln3ni to i64
  %ln3nk = icmp ult i64 %ln3nj, %SpLim_Arg
  %ln3nl = call ccc i1 (i1, i1) @llvm.expect.i1( i1 %ln3nk, i1 0 )
  br i1 %ln3nl, label %c3n5, label %c3n6
c3n6:
  %ln3nn = ptrtoint i8* @stg_upd_frame_info to i64
  %ln3nm = load i64*, i64** %Sp_Var
  %ln3no = getelementptr inbounds i64, i64* %ln3nm, i32 -2
  store i64 %ln3nn, i64* %ln3no, !tbaa !2
  %ln3nq = load i64, i64* %R1_Var
  %ln3np = load i64*, i64** %Sp_Var
  %ln3nr = getelementptr inbounds i64, i64* %ln3np, i32 -1
  store i64 %ln3nq, i64* %ln3nr, !tbaa !2
  %ln3nu = load i64, i64* %R1_Var
  %ln3nv = add i64 %ln3nu, 24
  %ln3nw = inttoptr i64 %ln3nv to i64*
  %ln3nx = load i64, i64* %ln3nw, !tbaa !4
  store i64 %ln3nx, i64* %R3_Var
  %ln3nA = load i64, i64* %R1_Var
  %ln3nB = add i64 %ln3nA, 16
  %ln3nC = inttoptr i64 %ln3nB to i64*
  %ln3nD = load i64, i64* %ln3nC, !tbaa !4
  %ln3nE = add i64 %ln3nD, 1
  store i64 %ln3nE, i64* %R2_Var
  %ln3nF = load i64*, i64** %Sp_Var
  %ln3nG = getelementptr inbounds i64, i64* %ln3nF, i32 -2
  %ln3nH = ptrtoint i64* %ln3nG to i64
  %ln3nI = inttoptr i64 %ln3nH to i64*
  store i64* %ln3nI, i64** %Sp_Var
  %ln3nJ = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_zdwupto_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3nK = load i64*, i64** %Sp_Var
  %ln3nL = load i64, i64* %R1_Var
  %ln3nM = load i64, i64* %R2_Var
  %ln3nN = load i64, i64* %R3_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3nJ( i64* %Base_Arg, i64* %ln3nK, i64* %Hp_Arg, i64 %ln3nL, i64 %ln3nM, i64 %ln3nN, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3n5:
  %ln3nO = load i64, i64* %R1_Var
  store i64 %ln3nO, i64* %R1_Var
  %ln3nP = getelementptr inbounds i64, i64* %Base_Arg, i32 -2
  %ln3nQ = bitcast i64* %ln3nP to i64*
  %ln3nR = load i64, i64* %ln3nQ, !tbaa !5
  %ln3nS = inttoptr i64 %ln3nR to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3nT = load i64*, i64** %Sp_Var
  %ln3nU = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3nS( i64* %Base_Arg, i64* %ln3nT, i64* %Hp_Arg, i64 %ln3nU, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
@SumSimpleBasic_zdwupto_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_zdwupto_info$def to i8*)
define ghccc void @SumSimpleBasic_zdwupto_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64}><{i64 8589934604, i64 0, i64 14}>
{
c3na:
  %Hp_Var = alloca i64*, i32 1
  store i64* %Hp_Arg, i64** %Hp_Var
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %R3_Var = alloca i64, i32 1
  store i64 %R3_Arg, i64* %R3_Var
  %R2_Var = alloca i64, i32 1
  store i64 %R2_Arg, i64* %R2_Var
  %ln3nV = load i64*, i64** %Hp_Var
  %ln3nW = getelementptr inbounds i64, i64* %ln3nV, i32 9
  %ln3nX = ptrtoint i64* %ln3nW to i64
  %ln3nY = inttoptr i64 %ln3nX to i64*
  store i64* %ln3nY, i64** %Hp_Var
  %ln3nZ = load i64*, i64** %Hp_Var
  %ln3o0 = ptrtoint i64* %ln3nZ to i64
  %ln3o1 = getelementptr inbounds i64, i64* %Base_Arg, i32 107
  %ln3o2 = bitcast i64* %ln3o1 to i64*
  %ln3o3 = load i64, i64* %ln3o2, !tbaa !5
  %ln3o4 = icmp ugt i64 %ln3o0, %ln3o3
  %ln3o5 = call ccc i1 (i1, i1) @llvm.expect.i1( i1 %ln3o4, i1 0 )
  br i1 %ln3o5, label %c3ne, label %c3nd
c3nd:
  %ln3o6 = load i64, i64* %R2_Var
  %ln3o7 = load i64, i64* %R3_Var
  %ln3o8 = icmp sgt i64 %ln3o6, %ln3o7
  %ln3o9 = zext i1 %ln3o8 to i64
  switch i64 %ln3o9, label %c3n8 [i64 1, label %c3n9]
c3n8:
  %ln3ob = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @s3kp_info$def to i64
  %ln3oa = load i64*, i64** %Hp_Var
  %ln3oc = getelementptr inbounds i64, i64* %ln3oa, i32 -8
  store i64 %ln3ob, i64* %ln3oc, !tbaa !3
  %ln3oe = load i64, i64* %R2_Var
  %ln3od = load i64*, i64** %Hp_Var
  %ln3of = getelementptr inbounds i64, i64* %ln3od, i32 -6
  store i64 %ln3oe, i64* %ln3of, !tbaa !3
  %ln3oh = load i64, i64* %R3_Var
  %ln3og = load i64*, i64** %Hp_Var
  %ln3oi = getelementptr inbounds i64, i64* %ln3og, i32 -5
  store i64 %ln3oh, i64* %ln3oi, !tbaa !3
  %ln3ok = ptrtoint i8* @ghczmprim_GHCziTypes_Izh_con_info to i64
  %ln3oj = load i64*, i64** %Hp_Var
  %ln3ol = getelementptr inbounds i64, i64* %ln3oj, i32 -4
  store i64 %ln3ok, i64* %ln3ol, !tbaa !3
  %ln3on = load i64, i64* %R2_Var
  %ln3om = load i64*, i64** %Hp_Var
  %ln3oo = getelementptr inbounds i64, i64* %ln3om, i32 -3
  store i64 %ln3on, i64* %ln3oo, !tbaa !3
  %ln3oq = ptrtoint i8* @SumSimpleBasic_Cons_con_info to i64
  %ln3op = load i64*, i64** %Hp_Var
  %ln3or = getelementptr inbounds i64, i64* %ln3op, i32 -2
  store i64 %ln3oq, i64* %ln3or, !tbaa !3
  %ln3ou = load i64*, i64** %Hp_Var
  %ln3ov = ptrtoint i64* %ln3ou to i64
  %ln3ow = add i64 %ln3ov, -31
  %ln3os = load i64*, i64** %Hp_Var
  %ln3ox = getelementptr inbounds i64, i64* %ln3os, i32 -1
  store i64 %ln3ow, i64* %ln3ox, !tbaa !3
  %ln3oz = load i64*, i64** %Hp_Var
  %ln3oA = getelementptr inbounds i64, i64* %ln3oz, i32 -8
  %ln3oB = ptrtoint i64* %ln3oA to i64
  %ln3oy = load i64*, i64** %Hp_Var
  %ln3oC = getelementptr inbounds i64, i64* %ln3oy, i32 0
  store i64 %ln3oB, i64* %ln3oC, !tbaa !3
  %ln3oE = load i64*, i64** %Hp_Var
  %ln3oF = ptrtoint i64* %ln3oE to i64
  %ln3oG = add i64 %ln3oF, -14
  store i64 %ln3oG, i64* %R1_Var
  %ln3oH = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  %ln3oI = bitcast i64* %ln3oH to i64*
  %ln3oJ = load i64, i64* %ln3oI, !tbaa !2
  %ln3oK = inttoptr i64 %ln3oJ to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3oL = load i64*, i64** %Hp_Var
  %ln3oM = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3oK( i64* %Base_Arg, i64* %Sp_Arg, i64* %ln3oL, i64 %ln3oM, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3n9:
  %ln3oN = load i64*, i64** %Hp_Var
  %ln3oO = getelementptr inbounds i64, i64* %ln3oN, i32 -9
  %ln3oP = ptrtoint i64* %ln3oO to i64
  %ln3oQ = inttoptr i64 %ln3oP to i64*
  store i64* %ln3oQ, i64** %Hp_Var
  %ln3oR = ptrtoint i8* @SumSimpleBasic_Nil_closure to i64
  %ln3oS = add i64 %ln3oR, 1
  store i64 %ln3oS, i64* %R1_Var
  %ln3oT = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  %ln3oU = bitcast i64* %ln3oT to i64*
  %ln3oV = load i64, i64* %ln3oU, !tbaa !2
  %ln3oW = inttoptr i64 %ln3oV to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3oX = load i64*, i64** %Hp_Var
  %ln3oY = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3oW( i64* %Base_Arg, i64* %Sp_Arg, i64* %ln3oX, i64 %ln3oY, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3ne:
  %ln3oZ = getelementptr inbounds i64, i64* %Base_Arg, i32 113
  store i64 72, i64* %ln3oZ, !tbaa !5
  %ln3p0 = load i64, i64* %R3_Var
  store i64 %ln3p0, i64* %R3_Var
  %ln3p1 = load i64, i64* %R2_Var
  store i64 %ln3p1, i64* %R2_Var
  %ln3p2 = ptrtoint %SumSimpleBasic_zdwupto_closure_struct* @SumSimpleBasic_zdwupto_closure$def to i64
  store i64 %ln3p2, i64* %R1_Var
  %ln3p3 = getelementptr inbounds i64, i64* %Base_Arg, i32 -1
  %ln3p4 = bitcast i64* %ln3p3 to i64*
  %ln3p5 = load i64, i64* %ln3p4, !tbaa !5
  %ln3p6 = inttoptr i64 %ln3p5 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3p7 = load i64*, i64** %Hp_Var
  %ln3p8 = load i64, i64* %R1_Var
  %ln3p9 = load i64, i64* %R2_Var
  %ln3pa = load i64, i64* %R3_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3p6( i64* %Base_Arg, i64* %Sp_Arg, i64* %ln3p7, i64 %ln3p8, i64 %ln3p9, i64 %ln3pa, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
%SumSimpleBasic_hszusumzupure2_closure_struct = type <{i64, i64, i64, i64}>
@SumSimpleBasic_hszusumzupure2_closure$def = internal global %SumSimpleBasic_hszusumzupure2_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure2_info$def to i64), i64 0, i64 0, i64 0}>
@SumSimpleBasic_hszusumzupure2_closure = alias i8, bitcast (%SumSimpleBasic_hszusumzupure2_closure_struct* @SumSimpleBasic_hszusumzupure2_closure$def to i8*)



;;;; LLVM Code ;;;;
@SumSimpleBasic_hszusumzupure2_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure2_info$def to i8*)
define ghccc void @SumSimpleBasic_hszusumzupure2_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 0, i64 21}>
{
c3pj:
  %R3_Var = alloca i64, i32 1
  store i64 undef, i64* %R3_Var
  %R4_Var = alloca i64, i32 1
  store i64 undef, i64* %R4_Var
  %R5_Var = alloca i64, i32 1
  store i64 undef, i64* %R5_Var
  %R6_Var = alloca i64, i32 1
  store i64 undef, i64* %R6_Var
  %F1_Var = alloca float, i32 1
  store float undef, float* %F1_Var
  %D1_Var = alloca double, i32 1
  store double undef, double* %D1_Var
  %F2_Var = alloca float, i32 1
  store float undef, float* %F2_Var
  %D2_Var = alloca double, i32 1
  store double undef, double* %D2_Var
  %F3_Var = alloca float, i32 1
  store float undef, float* %F3_Var
  %D3_Var = alloca double, i32 1
  store double undef, double* %D3_Var
  %F4_Var = alloca float, i32 1
  store float undef, float* %F4_Var
  %D4_Var = alloca double, i32 1
  store double undef, double* %D4_Var
  %F5_Var = alloca float, i32 1
  store float undef, float* %F5_Var
  %D5_Var = alloca double, i32 1
  store double undef, double* %D5_Var
  %F6_Var = alloca float, i32 1
  store float undef, float* %F6_Var
  %D6_Var = alloca double, i32 1
  store double undef, double* %D6_Var
  %lc3pe = alloca i64, i32 1
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %ln3pA = load i64*, i64** %Sp_Var
  %ln3pB = getelementptr inbounds i64, i64* %ln3pA, i32 -3
  %ln3pC = ptrtoint i64* %ln3pB to i64
  %ln3pD = icmp ult i64 %ln3pC, %SpLim_Arg
  %ln3pE = call ccc i1 (i1, i1) @llvm.expect.i1( i1 %ln3pD, i1 0 )
  br i1 %ln3pE, label %c3pt, label %c3pu
c3pu:
  %ln3pF = ptrtoint i64* %Base_Arg to i64
  %ln3pG = inttoptr i64 %ln3pF to i8*
  %ln3pH = load i64, i64* %R1_Var
  %ln3pI = inttoptr i64 %ln3pH to i8*
  %ln3pJ = bitcast i8* @newCAF to i8* (i8*, i8*)*
  store i64 undef, i64* %R3_Var
  store i64 undef, i64* %R4_Var
  store i64 undef, i64* %R5_Var
  store i64 undef, i64* %R6_Var
  store float undef, float* %F1_Var
  store double undef, double* %D1_Var
  store float undef, float* %F2_Var
  store double undef, double* %D2_Var
  store float undef, float* %F3_Var
  store double undef, double* %D3_Var
  store float undef, float* %F4_Var
  store double undef, double* %D4_Var
  store float undef, float* %F5_Var
  store double undef, double* %D5_Var
  store float undef, float* %F6_Var
  store double undef, double* %D6_Var
  %ln3pK = call ccc i8* (i8*, i8*) %ln3pJ( i8* %ln3pG, i8* %ln3pI ) nounwind
  %ln3pL = ptrtoint i8* %ln3pK to i64
  store i64 %ln3pL, i64* %lc3pe
  %ln3pM = load i64, i64* %lc3pe
  %ln3pN = icmp eq i64 %ln3pM, 0
  br i1 %ln3pN, label %c3pg, label %c3pf
c3pf:
  %ln3pP = ptrtoint i8* @stg_bh_upd_frame_info to i64
  %ln3pO = load i64*, i64** %Sp_Var
  %ln3pQ = getelementptr inbounds i64, i64* %ln3pO, i32 -2
  store i64 %ln3pP, i64* %ln3pQ, !tbaa !2
  %ln3pS = load i64, i64* %lc3pe
  %ln3pR = load i64*, i64** %Sp_Var
  %ln3pT = getelementptr inbounds i64, i64* %ln3pR, i32 -1
  store i64 %ln3pS, i64* %ln3pT, !tbaa !2
  %ln3pV = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3ph_info$def to i64
  %ln3pU = load i64*, i64** %Sp_Var
  %ln3pW = getelementptr inbounds i64, i64* %ln3pU, i32 -3
  store i64 %ln3pV, i64* %ln3pW, !tbaa !2
  store i64 100000, i64* %R3_Var
  store i64 1, i64* %R2_Var
  %ln3pX = load i64*, i64** %Sp_Var
  %ln3pY = getelementptr inbounds i64, i64* %ln3pX, i32 -3
  %ln3pZ = ptrtoint i64* %ln3pY to i64
  %ln3q0 = inttoptr i64 %ln3pZ to i64*
  store i64* %ln3q0, i64** %Sp_Var
  %ln3q1 = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_zdwupto_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3q2 = load i64*, i64** %Sp_Var
  %ln3q3 = load i64, i64* %R1_Var
  %ln3q4 = load i64, i64* %R2_Var
  %ln3q5 = load i64, i64* %R3_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3q1( i64* %Base_Arg, i64* %ln3q2, i64* %Hp_Arg, i64 %ln3q3, i64 %ln3q4, i64 %ln3q5, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3pg:
  %ln3q7 = load i64, i64* %R1_Var
  %ln3q8 = inttoptr i64 %ln3q7 to i64*
  %ln3q9 = load i64, i64* %ln3q8, !tbaa !4
  %ln3qa = inttoptr i64 %ln3q9 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3qb = load i64*, i64** %Sp_Var
  %ln3qc = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3qa( i64* %Base_Arg, i64* %ln3qb, i64* %Hp_Arg, i64 %ln3qc, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3pt:
  %ln3qd = load i64, i64* %R1_Var
  store i64 %ln3qd, i64* %R1_Var
  %ln3qe = getelementptr inbounds i64, i64* %Base_Arg, i32 -2
  %ln3qf = bitcast i64* %ln3qe to i64*
  %ln3qg = load i64, i64* %ln3qf, !tbaa !5
  %ln3qh = inttoptr i64 %ln3qg to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3qi = load i64*, i64** %Sp_Var
  %ln3qj = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3qh( i64* %Base_Arg, i64* %ln3qi, i64* %Hp_Arg, i64 %ln3qj, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
@c3ph_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3ph_info$def to i8*)
define internal ghccc void @c3ph_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 0, i64 30}>
{
c3ph:
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %ln3qk = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3pm_info$def to i64
  %ln3ql = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  store i64 %ln3qk, i64* %ln3ql, !tbaa !2
  store i64 %R1_Arg, i64* %R2_Var
  %ln3qm = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_zdwsum_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3qn = load i64, i64* %R2_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3qm( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %R1_Arg, i64 %ln3qn, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
@c3pm_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3pm_info$def to i8*)
define internal ghccc void @c3pm_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 0, i64 30}>
{
c3pm:
  %R4_Var = alloca i64, i32 1
  store i64 undef, i64* %R4_Var
  %R3_Var = alloca i64, i32 1
  store i64 undef, i64* %R3_Var
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %ln3qo = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3pq_info$def to i64
  %ln3qp = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  store i64 %ln3qo, i64* %ln3qp, !tbaa !2
  %ln3qq = ptrtoint i8* @ghczmprim_GHCziTypes_ZMZN_closure to i64
  %ln3qr = add i64 %ln3qq, 1
  store i64 %ln3qr, i64* %R4_Var
  store i64 %R1_Arg, i64* %R3_Var
  store i64 0, i64* %R2_Var
  %ln3qs = bitcast i8* @base_GHCziShow_zdwshowSignedInt_info to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3qt = load i64, i64* %R2_Var
  %ln3qu = load i64, i64* %R3_Var
  %ln3qv = load i64, i64* %R4_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3qs( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %R1_Arg, i64 %ln3qt, i64 %ln3qu, i64 %ln3qv, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
@c3pq_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3pq_info$def to i8*)
define internal ghccc void @c3pq_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 0, i64 30}>
{
c3pq:
  %Hp_Var = alloca i64*, i32 1
  store i64* %Hp_Arg, i64** %Hp_Var
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %R2_Var = alloca i64, i32 1
  store i64 %R2_Arg, i64* %R2_Var
  %ln3qw = load i64*, i64** %Hp_Var
  %ln3qx = getelementptr inbounds i64, i64* %ln3qw, i32 3
  %ln3qy = ptrtoint i64* %ln3qx to i64
  %ln3qz = inttoptr i64 %ln3qy to i64*
  store i64* %ln3qz, i64** %Hp_Var
  %ln3qA = load i64*, i64** %Hp_Var
  %ln3qB = ptrtoint i64* %ln3qA to i64
  %ln3qC = getelementptr inbounds i64, i64* %Base_Arg, i32 107
  %ln3qD = bitcast i64* %ln3qC to i64*
  %ln3qE = load i64, i64* %ln3qD, !tbaa !5
  %ln3qF = icmp ugt i64 %ln3qB, %ln3qE
  %ln3qG = call ccc i1 (i1, i1) @llvm.expect.i1( i1 %ln3qF, i1 0 )
  br i1 %ln3qG, label %c3pz, label %c3py
c3py:
  %ln3qI = ptrtoint i8* @ghczmprim_GHCziTypes_ZC_con_info to i64
  %ln3qH = load i64*, i64** %Hp_Var
  %ln3qJ = getelementptr inbounds i64, i64* %ln3qH, i32 -2
  store i64 %ln3qI, i64* %ln3qJ, !tbaa !3
  %ln3qL = load i64, i64* %R1_Var
  %ln3qK = load i64*, i64** %Hp_Var
  %ln3qM = getelementptr inbounds i64, i64* %ln3qK, i32 -1
  store i64 %ln3qL, i64* %ln3qM, !tbaa !3
  %ln3qO = load i64, i64* %R2_Var
  %ln3qN = load i64*, i64** %Hp_Var
  %ln3qP = getelementptr inbounds i64, i64* %ln3qN, i32 0
  store i64 %ln3qO, i64* %ln3qP, !tbaa !3
  %ln3qR = load i64*, i64** %Hp_Var
  %ln3qS = ptrtoint i64* %ln3qR to i64
  %ln3qT = add i64 %ln3qS, -14
  store i64 %ln3qT, i64* %R1_Var
  %ln3qU = load i64*, i64** %Sp_Var
  %ln3qV = getelementptr inbounds i64, i64* %ln3qU, i32 1
  %ln3qW = ptrtoint i64* %ln3qV to i64
  %ln3qX = inttoptr i64 %ln3qW to i64*
  store i64* %ln3qX, i64** %Sp_Var
  %ln3qY = load i64*, i64** %Sp_Var
  %ln3qZ = getelementptr inbounds i64, i64* %ln3qY, i32 0
  %ln3r0 = bitcast i64* %ln3qZ to i64*
  %ln3r1 = load i64, i64* %ln3r0, !tbaa !2
  %ln3r2 = inttoptr i64 %ln3r1 to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3r3 = load i64*, i64** %Sp_Var
  %ln3r4 = load i64*, i64** %Hp_Var
  %ln3r5 = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3r2( i64* %Base_Arg, i64* %ln3r3, i64* %ln3r4, i64 %ln3r5, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3pz:
  %ln3r6 = getelementptr inbounds i64, i64* %Base_Arg, i32 113
  store i64 24, i64* %ln3r6, !tbaa !5
  %ln3r7 = load i64, i64* %R2_Var
  store i64 %ln3r7, i64* %R2_Var
  %ln3r8 = load i64, i64* %R1_Var
  store i64 %ln3r8, i64* %R1_Var
  %ln3r9 = bitcast i8* @stg_gc_pp to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3ra = load i64*, i64** %Sp_Var
  %ln3rb = load i64*, i64** %Hp_Var
  %ln3rc = load i64, i64* %R1_Var
  %ln3rd = load i64, i64* %R2_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3r9( i64* %Base_Arg, i64* %ln3ra, i64* %ln3rb, i64 %ln3rc, i64 %ln3rd, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
%SumSimpleBasic_hszusumzupure1_closure_struct = type <{i64, i64}>
@SumSimpleBasic_hszusumzupure1_closure$def = internal global %SumSimpleBasic_hszusumzupure1_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure1_info$def to i64), i64 0}>
@SumSimpleBasic_hszusumzupure1_closure = alias i8, bitcast (%SumSimpleBasic_hszusumzupure1_closure_struct* @SumSimpleBasic_hszusumzupure1_closure$def to i8*)



;;;; LLVM Code ;;;;
@SumSimpleBasic_hszusumzupure1_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure1_info$def to i8*)
define ghccc void @SumSimpleBasic_hszusumzupure1_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64, i64}><{i64 add (i64 sub (i64 ptrtoint (i8* @S3rl_srt to i64),i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure1_info$def to i64)),i64 0), i64 4294967299, i64 0, i64 30064771086}>
{
c3ri:
  %R4_Var = alloca i64, i32 1
  store i64 undef, i64* %R4_Var
  %R3_Var = alloca i64, i32 1
  store i64 undef, i64* %R3_Var
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %ln3rm = ptrtoint i8* @ghczmprim_GHCziTypes_True_closure to i64
  %ln3rn = add i64 %ln3rm, 2
  store i64 %ln3rn, i64* %R4_Var
  %ln3ro = ptrtoint %SumSimpleBasic_hszusumzupure2_closure_struct* @SumSimpleBasic_hszusumzupure2_closure$def to i64
  store i64 %ln3ro, i64* %R3_Var
  %ln3rp = ptrtoint i8* @base_GHCziIOziHandleziFD_stdout_closure to i64
  store i64 %ln3rp, i64* %R2_Var
  %ln3rq = bitcast i8* @base_GHCziIOziHandleziText_hPutStr2_info to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3rr = load i64, i64* %R2_Var
  %ln3rs = load i64, i64* %R3_Var
  %ln3rt = load i64, i64* %R4_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3rq( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %R1_Arg, i64 %ln3rr, i64 %ln3rs, i64 %ln3rt, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
%SumSimpleBasic_hszusumzupure_closure_struct = type <{i64, i64}>
@SumSimpleBasic_hszusumzupure_closure$def = internal global %SumSimpleBasic_hszusumzupure_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure_info$def to i64), i64 0}>
@SumSimpleBasic_hszusumzupure_closure = alias i8, bitcast (%SumSimpleBasic_hszusumzupure_closure_struct* @SumSimpleBasic_hszusumzupure_closure$def to i8*)



;;;; LLVM Code ;;;;
@SumSimpleBasic_hszusumzupure_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure_info$def to i8*)
define ghccc void @SumSimpleBasic_hszusumzupure_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64, i64}><{i64 add (i64 sub (i64 ptrtoint (i8* @S3rl_srt to i64),i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure_info$def to i64)),i64 24), i64 4294967299, i64 0, i64 4294967310}>
{
c3ry:
  %ln3rB = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_hszusumzupure1_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3rB( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %R1_Arg, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
%SumSimpleBasic_Nil_closure_struct = type <{i64}>
@SumSimpleBasic_Nil_closure$def = internal global %SumSimpleBasic_Nil_closure_struct<{i64 ptrtoint (i8* @SumSimpleBasic_Nil_con_info to i64)}>
@SumSimpleBasic_Nil_closure = alias i8, bitcast (%SumSimpleBasic_Nil_closure_struct* @SumSimpleBasic_Nil_closure$def to i8*)



;;;; LLVM Code ;;;;
%SumSimpleBasic_Cons_closure_struct = type <{i64}>
@SumSimpleBasic_Cons_closure$def = internal global %SumSimpleBasic_Cons_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_Cons_info$def to i64)}>
@SumSimpleBasic_Cons_closure = alias i8, bitcast (%SumSimpleBasic_Cons_closure_struct* @SumSimpleBasic_Cons_closure$def to i8*)



;;;; LLVM Code ;;;;
@SumSimpleBasic_Cons_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_Cons_info$def to i8*)
define internal ghccc void @SumSimpleBasic_Cons_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64}><{i64 8589934607, i64 0, i64 14}>
{
c3rH:
  %Hp_Var = alloca i64*, i32 1
  store i64* %Hp_Arg, i64** %Hp_Var
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %R3_Var = alloca i64, i32 1
  store i64 %R3_Arg, i64* %R3_Var
  %R2_Var = alloca i64, i32 1
  store i64 %R2_Arg, i64* %R2_Var
  %ln3rM = load i64*, i64** %Hp_Var
  %ln3rN = getelementptr inbounds i64, i64* %ln3rM, i32 3
  %ln3rO = ptrtoint i64* %ln3rN to i64
  %ln3rP = inttoptr i64 %ln3rO to i64*
  store i64* %ln3rP, i64** %Hp_Var
  %ln3rQ = load i64*, i64** %Hp_Var
  %ln3rR = ptrtoint i64* %ln3rQ to i64
  %ln3rS = getelementptr inbounds i64, i64* %Base_Arg, i32 107
  %ln3rT = bitcast i64* %ln3rS to i64*
  %ln3rU = load i64, i64* %ln3rT, !tbaa !5
  %ln3rV = icmp ugt i64 %ln3rR, %ln3rU
  %ln3rW = call ccc i1 (i1, i1) @llvm.expect.i1( i1 %ln3rV, i1 0 )
  br i1 %ln3rW, label %c3rL, label %c3rK
c3rK:
  %ln3rY = ptrtoint i8* @SumSimpleBasic_Cons_con_info to i64
  %ln3rX = load i64*, i64** %Hp_Var
  %ln3rZ = getelementptr inbounds i64, i64* %ln3rX, i32 -2
  store i64 %ln3rY, i64* %ln3rZ, !tbaa !3
  %ln3s1 = load i64, i64* %R2_Var
  %ln3s0 = load i64*, i64** %Hp_Var
  %ln3s2 = getelementptr inbounds i64, i64* %ln3s0, i32 -1
  store i64 %ln3s1, i64* %ln3s2, !tbaa !3
  %ln3s4 = load i64, i64* %R3_Var
  %ln3s3 = load i64*, i64** %Hp_Var
  %ln3s5 = getelementptr inbounds i64, i64* %ln3s3, i32 0
  store i64 %ln3s4, i64* %ln3s5, !tbaa !3
  %ln3s7 = load i64*, i64** %Hp_Var
  %ln3s8 = ptrtoint i64* %ln3s7 to i64
  %ln3s9 = add i64 %ln3s8, -14
  store i64 %ln3s9, i64* %R1_Var
  %ln3sa = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  %ln3sb = bitcast i64* %ln3sa to i64*
  %ln3sc = load i64, i64* %ln3sb, !tbaa !2
  %ln3sd = inttoptr i64 %ln3sc to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3se = load i64*, i64** %Hp_Var
  %ln3sf = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3sd( i64* %Base_Arg, i64* %Sp_Arg, i64* %ln3se, i64 %ln3sf, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3rL:
  %ln3sg = getelementptr inbounds i64, i64* %Base_Arg, i32 113
  store i64 24, i64* %ln3sg, !tbaa !5
  %ln3sh = load i64, i64* %R3_Var
  store i64 %ln3sh, i64* %R3_Var
  %ln3si = load i64, i64* %R2_Var
  store i64 %ln3si, i64* %R2_Var
  %ln3sj = ptrtoint %SumSimpleBasic_Cons_closure_struct* @SumSimpleBasic_Cons_closure$def to i64
  store i64 %ln3sj, i64* %R1_Var
  %ln3sk = getelementptr inbounds i64, i64* %Base_Arg, i32 -1
  %ln3sl = bitcast i64* %ln3sk to i64*
  %ln3sm = load i64, i64* %ln3sl, !tbaa !5
  %ln3sn = inttoptr i64 %ln3sm to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3so = load i64*, i64** %Hp_Var
  %ln3sp = load i64, i64* %R1_Var
  %ln3sq = load i64, i64* %R2_Var
  %ln3sr = load i64, i64* %R3_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3sn( i64* %Base_Arg, i64* %Sp_Arg, i64* %ln3so, i64 %ln3sp, i64 %ln3sq, i64 %ln3sr, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
%i3st_str_struct = type <{[24 x i8]}>
@i3st_str$def = internal constant %i3st_str_struct<{[24 x i8] [i8 109, i8 97, i8 105, i8 110, i8 58, i8 83, i8 117, i8 109, i8 83, i8 105, i8 109, i8 112, i8 108, i8 101, i8 66, i8 97, i8 115, i8 105, i8 99, i8 46, i8 78, i8 105, i8 108, i8 0]}>, align 1
@i3st_str = internal alias i8, bitcast (%i3st_str_struct* @i3st_str$def to i8*)



;;;; LLVM Code ;;;;
@SumSimpleBasic_Nil_con_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_Nil_con_info$def to i8*)
define ghccc void @SumSimpleBasic_Nil_con_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64}><{i64 add (i64 sub (i64 ptrtoint (%i3st_str_struct* @i3st_str$def to i64),i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_Nil_con_info$def to i64)),i64 0), i64 4294967296, i64 3}>
{
c3ss:
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %ln3sv = load i64, i64* %R1_Var
  %ln3sw = add i64 %ln3sv, 1
  store i64 %ln3sw, i64* %R1_Var
  %ln3sx = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  %ln3sy = bitcast i64* %ln3sx to i64*
  %ln3sz = load i64, i64* %ln3sy, !tbaa !2
  %ln3sA = inttoptr i64 %ln3sz to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3sB = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3sA( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %ln3sB, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
%i3sD_str_struct = type <{[25 x i8]}>
@i3sD_str$def = internal constant %i3sD_str_struct<{[25 x i8] [i8 109, i8 97, i8 105, i8 110, i8 58, i8 83, i8 117, i8 109, i8 83, i8 105, i8 109, i8 112, i8 108, i8 101, i8 66, i8 97, i8 115, i8 105, i8 99, i8 46, i8 67, i8 111, i8 110, i8 115, i8 0]}>, align 1
@i3sD_str = internal alias i8, bitcast (%i3sD_str_struct* @i3sD_str$def to i8*)



;;;; LLVM Code ;;;;
@SumSimpleBasic_Cons_con_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_Cons_con_info$def to i8*)
define ghccc void @SumSimpleBasic_Cons_con_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64}><{i64 add (i64 sub (i64 ptrtoint (%i3sD_str_struct* @i3sD_str$def to i64),i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @SumSimpleBasic_Cons_con_info$def to i64)),i64 0), i64 2, i64 4294967300}>
{
c3sC:
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %ln3sF = load i64, i64* %R1_Var
  %ln3sG = add i64 %ln3sF, 2
  store i64 %ln3sG, i64* %R1_Var
  %ln3sH = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  %ln3sI = bitcast i64* %ln3sH to i64*
  %ln3sJ = load i64, i64* %ln3sI, !tbaa !2
  %ln3sK = inttoptr i64 %ln3sJ to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3sL = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3sK( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %ln3sL, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



;;;; LLVM Code ;;;;
%S3rl_srt_struct = type <{i64, i64, i64, i64}>
@S3rl_srt$def = internal constant %S3rl_srt_struct<{i64 ptrtoint (i8* @base_GHCziIOziHandleziFD_stdout_closure to i64), i64 ptrtoint (i8* @base_GHCziIOziHandleziText_hPutStr2_closure to i64), i64 ptrtoint (%SumSimpleBasic_hszusumzupure2_closure_struct* @SumSimpleBasic_hszusumzupure2_closure$def to i64), i64 ptrtoint (%SumSimpleBasic_hszusumzupure1_closure_struct* @SumSimpleBasic_hszusumzupure1_closure$def to i64)}>
@S3rl_srt = internal alias i8, bitcast (%S3rl_srt_struct* @S3rl_srt$def to i8*)



;;;; LLVM Code ;;;;
@ghczmprim_GHCziTypes_TrNameS_con_info = external global i8
@ghczmprim_GHCziTypes_Module_con_info = external global i8
@ghczmprim_GHCziTypes_KindRepVar_con_info = external global i8
@ghczmprim_GHCziTypes_TyCon_con_info = external global i8
@ghczmprim_GHCziTypes_krepzdztArrzt_closure = external global i8
@ghczmprim_GHCziTypes_ZC_con_info = external global i8
@ghczmprim_GHCziTypes_ZMZN_closure = external global i8
@ghczmprim_GHCziTypes_KindRepTyConApp_con_info = external global i8
@ghczmprim_GHCziTypes_KindRepFun_con_info = external global i8
@stg_upd_frame_info = external global i8
@ghczmprim_GHCziTypes_Izh_con_info = external global i8
@newCAF = external global i8
@stg_bh_upd_frame_info = external global i8
@base_GHCziShow_zdwshowSignedInt_info = external global i8
@stg_gc_pp = external global i8
@ghczmprim_GHCziTypes_True_closure = external global i8
@base_GHCziIOziHandleziFD_stdout_closure = external global i8
@base_GHCziIOziHandleziText_hPutStr2_info = external global i8
@base_GHCziIOziHandleziText_hPutStr2_closure = external global i8



;;;; LLVM Code ;;;;
@llvm.used = appending constant [29 x i8*] [i8* bitcast (%S3rl_srt_struct* @S3rl_srt$def to i8*), i8* bitcast (%i3sD_str_struct* @i3sD_str$def to i8*), i8* bitcast (%i3st_str_struct* @i3st_str$def to i8*), i8* bitcast (%SumSimpleBasic_Cons_closure_struct* @SumSimpleBasic_Cons_closure$def to i8*), i8* bitcast (%SumSimpleBasic_Nil_closure_struct* @SumSimpleBasic_Nil_closure$def to i8*), i8* bitcast (%SumSimpleBasic_hszusumzupure_closure_struct* @SumSimpleBasic_hszusumzupure_closure$def to i8*), i8* bitcast (%SumSimpleBasic_hszusumzupure1_closure_struct* @SumSimpleBasic_hszusumzupure1_closure$def to i8*), i8* bitcast (%SumSimpleBasic_hszusumzupure2_closure_struct* @SumSimpleBasic_hszusumzupure2_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdwupto_closure_struct* @SumSimpleBasic_zdwupto_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdwsum_closure_struct* @SumSimpleBasic_zdwsum_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtczqCons_closure_struct* @SumSimpleBasic_zdtczqCons_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtczqCons2_closure_struct* @SumSimpleBasic_zdtczqCons2_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtczqCons3_bytes_struct* @SumSimpleBasic_zdtczqCons3_bytes$def to i8*), i8* bitcast (%SumSimpleBasic_zdtczqCons1_closure_struct* @SumSimpleBasic_zdtczqCons1_closure$def to i8*), i8* bitcast (%r3kb_closure_struct* @r3kb_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtczqNil_closure_struct* @SumSimpleBasic_zdtczqNil_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtczqNil2_closure_struct* @SumSimpleBasic_zdtczqNil2_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtczqNil3_bytes_struct* @SumSimpleBasic_zdtczqNil3_bytes$def to i8*), i8* bitcast (%SumSimpleBasic_zdtczqNil1_closure_struct* @SumSimpleBasic_zdtczqNil1_closure$def to i8*), i8* bitcast (%r3ka_closure_struct* @r3ka_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtcList_closure_struct* @SumSimpleBasic_zdtcList_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtcList1_closure_struct* @SumSimpleBasic_zdtcList1_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtcList2_bytes_struct* @SumSimpleBasic_zdtcList2_bytes$def to i8*), i8* bitcast (%r3k9_closure_struct* @r3k9_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtrModule_closure_struct* @SumSimpleBasic_zdtrModule_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtrModule1_closure_struct* @SumSimpleBasic_zdtrModule1_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtrModule2_bytes_struct* @SumSimpleBasic_zdtrModule2_bytes$def to i8*), i8* bitcast (%SumSimpleBasic_zdtrModule3_closure_struct* @SumSimpleBasic_zdtrModule3_closure$def to i8*), i8* bitcast (%SumSimpleBasic_zdtrModule4_bytes_struct* @SumSimpleBasic_zdtrModule4_bytes$def to i8*)], section "llvm.metadata"


