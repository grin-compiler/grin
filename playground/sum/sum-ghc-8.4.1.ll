; [1 of 1] Compiling Sum              ( Sum.hs, Sum.o )

; ==================== LLVM Code ====================
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux"



; ==================== LLVM Code ====================
declare ccc i8* @memcpy$def(i8*, i8*, i64)



; ==================== LLVM Code ====================
declare ccc i8* @memmove$def(i8*, i8*, i64)



; ==================== LLVM Code ====================
declare ccc i8* @memset$def(i8*, i64, i64)



; ==================== LLVM Code ====================
declare ccc i64 @newSpark$def(i8*, i8*)



; ==================== LLVM Code ====================
!0 = !{!"root"}
!1 = !{!"top", !0}
!2 = !{!"stack", !1}
!3 = !{!"heap", !1}
!4 = !{!"rx", !3}
!5 = !{!"base", !1}



; ==================== LLVM Code ====================



; ==================== LLVM Code ====================
%Sum_zdtrModule4_bytes_struct = type <{[5 x i8]}>
@Sum_zdtrModule4_bytes$def = internal constant %Sum_zdtrModule4_bytes_struct<{[5 x i8] [i8 109, i8 97, i8 105, i8 110, i8 0]}>, align 1
@Sum_zdtrModule4_bytes = alias i8, bitcast (%Sum_zdtrModule4_bytes_struct* @Sum_zdtrModule4_bytes$def to i8*)



; ==================== LLVM Code ====================
%Sum_zdtrModule3_closure_struct = type <{i64, i64}>
@Sum_zdtrModule3_closure$def = internal global %Sum_zdtrModule3_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TrNameS_con_info to i64), i64 ptrtoint (%Sum_zdtrModule4_bytes_struct* @Sum_zdtrModule4_bytes$def to i64)}>
@Sum_zdtrModule3_closure = alias i8, bitcast (%Sum_zdtrModule3_closure_struct* @Sum_zdtrModule3_closure$def to i8*)



; ==================== LLVM Code ====================
%Sum_zdtrModule2_bytes_struct = type <{[4 x i8]}>
@Sum_zdtrModule2_bytes$def = internal constant %Sum_zdtrModule2_bytes_struct<{[4 x i8] [i8 83, i8 117, i8 109, i8 0]}>, align 1
@Sum_zdtrModule2_bytes = alias i8, bitcast (%Sum_zdtrModule2_bytes_struct* @Sum_zdtrModule2_bytes$def to i8*)



; ==================== LLVM Code ====================
%Sum_zdtrModule1_closure_struct = type <{i64, i64}>
@Sum_zdtrModule1_closure$def = internal global %Sum_zdtrModule1_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_TrNameS_con_info to i64), i64 ptrtoint (%Sum_zdtrModule2_bytes_struct* @Sum_zdtrModule2_bytes$def to i64)}>
@Sum_zdtrModule1_closure = alias i8, bitcast (%Sum_zdtrModule1_closure_struct* @Sum_zdtrModule1_closure$def to i8*)



; ==================== LLVM Code ====================
%Sum_zdtrModule_closure_struct = type <{i64, i64, i64, i64}>
@Sum_zdtrModule_closure$def = internal global %Sum_zdtrModule_closure_struct<{i64 ptrtoint (i8* @ghczmprim_GHCziTypes_Module_con_info to i64), i64 add (i64 ptrtoint (%Sum_zdtrModule3_closure_struct* @Sum_zdtrModule3_closure$def to i64),i64 1), i64 add (i64 ptrtoint (%Sum_zdtrModule1_closure_struct* @Sum_zdtrModule1_closure$def to i64),i64 1), i64 3}>
@Sum_zdtrModule_closure = alias i8, bitcast (%Sum_zdtrModule_closure_struct* @Sum_zdtrModule_closure$def to i8*)



; ==================== LLVM Code ====================
%Sum_zdwsum_closure_struct = type <{i64}>
@Sum_zdwsum_closure$def = internal global %Sum_zdwsum_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_zdwsum_info$def to i64)}>
@Sum_zdwsum_closure = alias i8, bitcast (%Sum_zdwsum_closure_struct* @Sum_zdwsum_closure$def to i8*)



; ==================== LLVM Code ====================
@Sum_zdwsum_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_zdwsum_info$def to i8*)
define ghccc void @Sum_zdwsum_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64}><{i64 12884901904, i64 0, i64 14}>
{
c3is:
  %ls3ib = alloca i64, i32 1
  %ls3ia = alloca i64, i32 1
  %ls3i9 = alloca i64, i32 1
  %ls3id = alloca i64, i32 1
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  store i64 %R4_Arg, i64* %ls3ib
  store i64 %R3_Arg, i64* %ls3ia
  store i64 %R2_Arg, i64* %ls3i9
  br label %c3ik
c3ik:
  %ln3iA = load i64, i64* %ls3ia
  %ln3iB = load i64, i64* %ls3ib
  %ln3iC = icmp sgt i64 %ln3iA, %ln3iB
  %ln3iD = zext i1 %ln3iC to i64
  switch i64 %ln3iD, label %c3iq [i64 1, label %c3ir]
c3iq:
  %ln3iE = load i64, i64* %ls3i9
  %ln3iF = load i64, i64* %ls3ia
  %ln3iG = add i64 %ln3iE, %ln3iF
  store i64 %ln3iG, i64* %ls3id
  %ln3iH = load i64, i64* %ls3ia
  %ln3iI = add i64 %ln3iH, 1
  store i64 %ln3iI, i64* %ls3ia
  %ln3iJ = load i64, i64* %ls3id
  store i64 %ln3iJ, i64* %ls3i9
  br label %c3ik
c3ir:
  %ln3iK = load i64, i64* %ls3i9
  store i64 %ln3iK, i64* %R1_Var
  %ln3iL = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  %ln3iM = bitcast i64* %ln3iL to i64*
  %ln3iN = load i64, i64* %ln3iM, !tbaa !2
  %ln3iO = inttoptr i64 %ln3iN to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3iP = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3iO( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %ln3iP, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



; ==================== LLVM Code ====================
%Sum_hszusumzupure2_closure_struct = type <{i64, i64, i64, i64}>
@Sum_hszusumzupure2_closure$def = internal global %Sum_hszusumzupure2_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure2_info$def to i64), i64 0, i64 0, i64 0}>
@Sum_hszusumzupure2_closure = alias i8, bitcast (%Sum_hszusumzupure2_closure_struct* @Sum_hszusumzupure2_closure$def to i8*)



; ==================== LLVM Code ====================
@Sum_hszusumzupure2_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure2_info$def to i8*)
define ghccc void @Sum_hszusumzupure2_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 0, i64 21}>
{
c3iY:
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
  %lc3iT = alloca i64, i32 1
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %ln3ja = load i64*, i64** %Sp_Var
  %ln3jb = getelementptr inbounds i64, i64* %ln3ja, i32 -3
  %ln3jc = ptrtoint i64* %ln3jb to i64
  %ln3jd = icmp ult i64 %ln3jc, %SpLim_Arg
  %ln3jf = call ccc i1 (i1, i1) @llvm.expect.i1( i1 %ln3jd, i1 0 )
  br i1 %ln3jf, label %c3j4, label %c3j5
c3j5:
  %ln3jg = ptrtoint i64* %Base_Arg to i64
  %ln3jh = inttoptr i64 %ln3jg to i8*
  %ln3ji = load i64, i64* %R1_Var
  %ln3jj = inttoptr i64 %ln3ji to i8*
  %ln3jk = bitcast i8* @newCAF to i8* (i8*, i8*)*
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
  %ln3jl = call ccc i8* (i8*, i8*) %ln3jk( i8* %ln3jh, i8* %ln3jj ) nounwind
  %ln3jm = ptrtoint i8* %ln3jl to i64
  store i64 %ln3jm, i64* %lc3iT
  %ln3jn = load i64, i64* %lc3iT
  %ln3jo = icmp eq i64 %ln3jn, 0
  br i1 %ln3jo, label %c3iV, label %c3iU
c3iU:
  %ln3jq = ptrtoint i8* @stg_bh_upd_frame_info to i64
  %ln3jp = load i64*, i64** %Sp_Var
  %ln3jr = getelementptr inbounds i64, i64* %ln3jp, i32 -2
  store i64 %ln3jq, i64* %ln3jr, !tbaa !2
  %ln3jt = load i64, i64* %lc3iT
  %ln3js = load i64*, i64** %Sp_Var
  %ln3ju = getelementptr inbounds i64, i64* %ln3js, i32 -1
  store i64 %ln3jt, i64* %ln3ju, !tbaa !2
  %ln3jw = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3iW_info$def to i64
  %ln3jv = load i64*, i64** %Sp_Var
  %ln3jx = getelementptr inbounds i64, i64* %ln3jv, i32 -3
  store i64 %ln3jw, i64* %ln3jx, !tbaa !2
  store i64 100000, i64* %R4_Var
  store i64 1, i64* %R3_Var
  store i64 0, i64* %R2_Var
  %ln3jy = load i64*, i64** %Sp_Var
  %ln3jz = getelementptr inbounds i64, i64* %ln3jy, i32 -3
  %ln3jA = ptrtoint i64* %ln3jz to i64
  %ln3jB = inttoptr i64 %ln3jA to i64*
  store i64* %ln3jB, i64** %Sp_Var
  %ln3jC = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_zdwsum_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3jD = load i64*, i64** %Sp_Var
  %ln3jE = load i64, i64* %R1_Var
  %ln3jF = load i64, i64* %R2_Var
  %ln3jG = load i64, i64* %R3_Var
  %ln3jH = load i64, i64* %R4_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3jC( i64* %Base_Arg, i64* %ln3jD, i64* %Hp_Arg, i64 %ln3jE, i64 %ln3jF, i64 %ln3jG, i64 %ln3jH, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3iV:
  %ln3jJ = load i64, i64* %R1_Var
  %ln3jK = inttoptr i64 %ln3jJ to i64*
  %ln3jL = load i64, i64* %ln3jK, !tbaa !4
  %ln3jM = inttoptr i64 %ln3jL to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3jN = load i64*, i64** %Sp_Var
  %ln3jO = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3jM( i64* %Base_Arg, i64* %ln3jN, i64* %Hp_Arg, i64 %ln3jO, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3j4:
  %ln3jP = load i64, i64* %R1_Var
  store i64 %ln3jP, i64* %R1_Var
  %ln3jQ = getelementptr inbounds i64, i64* %Base_Arg, i32 -2
  %ln3jR = bitcast i64* %ln3jQ to i64*
  %ln3jS = load i64, i64* %ln3jR, !tbaa !5
  %ln3jT = inttoptr i64 %ln3jS to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3jU = load i64*, i64** %Sp_Var
  %ln3jV = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3jT( i64* %Base_Arg, i64* %ln3jU, i64* %Hp_Arg, i64 %ln3jV, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}
declare ccc i1 @llvm.expect.i1(i1, i1)



; ==================== LLVM Code ====================
@c3iW_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3iW_info$def to i8*)
define internal ghccc void @c3iW_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 0, i64 30}>
{
c3iW:
  %R4_Var = alloca i64, i32 1
  store i64 undef, i64* %R4_Var
  %R3_Var = alloca i64, i32 1
  store i64 undef, i64* %R3_Var
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %ln3jW = ptrtoint void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3j1_info$def to i64
  %ln3jX = getelementptr inbounds i64, i64* %Sp_Arg, i32 0
  store i64 %ln3jW, i64* %ln3jX, !tbaa !2
  %ln3jY = ptrtoint i8* @ghczmprim_GHCziTypes_ZMZN_closure to i64
  %ln3jZ = add i64 %ln3jY, 1
  store i64 %ln3jZ, i64* %R4_Var
  store i64 %R1_Arg, i64* %R3_Var
  store i64 0, i64* %R2_Var
  %ln3k0 = bitcast i8* @base_GHCziShow_zdwshowSignedInt_info to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3k1 = load i64, i64* %R2_Var
  %ln3k2 = load i64, i64* %R3_Var
  %ln3k3 = load i64, i64* %R4_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3k0( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %R1_Arg, i64 %ln3k1, i64 %ln3k2, i64 %ln3k3, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



; ==================== LLVM Code ====================
@c3j1_info = internal alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @c3j1_info$def to i8*)
define internal ghccc void @c3j1_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64}><{i64 0, i64 30}>
{
c3j1:
  %Hp_Var = alloca i64*, i32 1
  store i64* %Hp_Arg, i64** %Hp_Var
  %R1_Var = alloca i64, i32 1
  store i64 %R1_Arg, i64* %R1_Var
  %Sp_Var = alloca i64*, i32 1
  store i64* %Sp_Arg, i64** %Sp_Var
  %R2_Var = alloca i64, i32 1
  store i64 %R2_Arg, i64* %R2_Var
  %ln3k4 = load i64*, i64** %Hp_Var
  %ln3k5 = getelementptr inbounds i64, i64* %ln3k4, i32 3
  %ln3k6 = ptrtoint i64* %ln3k5 to i64
  %ln3k7 = inttoptr i64 %ln3k6 to i64*
  store i64* %ln3k7, i64** %Hp_Var
  %ln3k8 = load i64*, i64** %Hp_Var
  %ln3k9 = ptrtoint i64* %ln3k8 to i64
  %ln3ka = getelementptr inbounds i64, i64* %Base_Arg, i32 107
  %ln3kb = bitcast i64* %ln3ka to i64*
  %ln3kc = load i64, i64* %ln3kb, !tbaa !5
  %ln3kd = icmp ugt i64 %ln3k9, %ln3kc
  %ln3ke = call ccc i1 (i1, i1) @llvm.expect.i1( i1 %ln3kd, i1 0 )
  br i1 %ln3ke, label %c3j9, label %c3j8
c3j8:
  %ln3kg = ptrtoint i8* @ghczmprim_GHCziTypes_ZC_con_info to i64
  %ln3kf = load i64*, i64** %Hp_Var
  %ln3kh = getelementptr inbounds i64, i64* %ln3kf, i32 -2
  store i64 %ln3kg, i64* %ln3kh, !tbaa !3
  %ln3kj = load i64, i64* %R1_Var
  %ln3ki = load i64*, i64** %Hp_Var
  %ln3kk = getelementptr inbounds i64, i64* %ln3ki, i32 -1
  store i64 %ln3kj, i64* %ln3kk, !tbaa !3
  %ln3km = load i64, i64* %R2_Var
  %ln3kl = load i64*, i64** %Hp_Var
  %ln3kn = getelementptr inbounds i64, i64* %ln3kl, i32 0
  store i64 %ln3km, i64* %ln3kn, !tbaa !3
  %ln3kp = load i64*, i64** %Hp_Var
  %ln3kq = ptrtoint i64* %ln3kp to i64
  %ln3kr = add i64 %ln3kq, -14
  store i64 %ln3kr, i64* %R1_Var
  %ln3ks = load i64*, i64** %Sp_Var
  %ln3kt = getelementptr inbounds i64, i64* %ln3ks, i32 1
  %ln3ku = ptrtoint i64* %ln3kt to i64
  %ln3kv = inttoptr i64 %ln3ku to i64*
  store i64* %ln3kv, i64** %Sp_Var
  %ln3kw = load i64*, i64** %Sp_Var
  %ln3kx = getelementptr inbounds i64, i64* %ln3kw, i32 0
  %ln3ky = bitcast i64* %ln3kx to i64*
  %ln3kz = load i64, i64* %ln3ky, !tbaa !2
  %ln3kA = inttoptr i64 %ln3kz to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3kB = load i64*, i64** %Sp_Var
  %ln3kC = load i64*, i64** %Hp_Var
  %ln3kD = load i64, i64* %R1_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3kA( i64* %Base_Arg, i64* %ln3kB, i64* %ln3kC, i64 %ln3kD, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
c3j9:
  %ln3kE = getelementptr inbounds i64, i64* %Base_Arg, i32 113
  store i64 24, i64* %ln3kE, !tbaa !5
  %ln3kF = load i64, i64* %R2_Var
  store i64 %ln3kF, i64* %R2_Var
  %ln3kG = load i64, i64* %R1_Var
  store i64 %ln3kG, i64* %R1_Var
  %ln3kH = bitcast i8* @stg_gc_pp to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3kI = load i64*, i64** %Sp_Var
  %ln3kJ = load i64*, i64** %Hp_Var
  %ln3kK = load i64, i64* %R1_Var
  %ln3kL = load i64, i64* %R2_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3kH( i64* %Base_Arg, i64* %ln3kI, i64* %ln3kJ, i64 %ln3kK, i64 %ln3kL, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



; ==================== LLVM Code ====================
%Sum_hszusumzupure1_closure_struct = type <{i64, i64}>
@Sum_hszusumzupure1_closure$def = internal global %Sum_hszusumzupure1_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure1_info$def to i64), i64 0}>
@Sum_hszusumzupure1_closure = alias i8, bitcast (%Sum_hszusumzupure1_closure_struct* @Sum_hszusumzupure1_closure$def to i8*)



; ==================== LLVM Code ====================
@Sum_hszusumzupure1_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure1_info$def to i8*)
define ghccc void @Sum_hszusumzupure1_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64, i64}><{i64 add (i64 sub (i64 ptrtoint (i8* @S3kT_srt to i64),i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure1_info$def to i64)),i64 0), i64 4294967299, i64 0, i64 30064771086}>
{
c3kQ:
  %R4_Var = alloca i64, i32 1
  store i64 undef, i64* %R4_Var
  %R3_Var = alloca i64, i32 1
  store i64 undef, i64* %R3_Var
  %R2_Var = alloca i64, i32 1
  store i64 undef, i64* %R2_Var
  %ln3kU = ptrtoint i8* @ghczmprim_GHCziTypes_True_closure to i64
  %ln3kV = add i64 %ln3kU, 2
  store i64 %ln3kV, i64* %R4_Var
  %ln3kW = ptrtoint %Sum_hszusumzupure2_closure_struct* @Sum_hszusumzupure2_closure$def to i64
  store i64 %ln3kW, i64* %R3_Var
  %ln3kX = ptrtoint i8* @base_GHCziIOziHandleziFD_stdout_closure to i64
  store i64 %ln3kX, i64* %R2_Var
  %ln3kY = bitcast i8* @base_GHCziIOziHandleziText_hPutStr2_info to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  %ln3kZ = load i64, i64* %R2_Var
  %ln3l0 = load i64, i64* %R3_Var
  %ln3l1 = load i64, i64* %R4_Var
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3kY( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %R1_Arg, i64 %ln3kZ, i64 %ln3l0, i64 %ln3l1, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



; ==================== LLVM Code ====================
%Sum_hszusumzupure_closure_struct = type <{i64, i64}>
@Sum_hszusumzupure_closure$def = internal global %Sum_hszusumzupure_closure_struct<{i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure_info$def to i64), i64 0}>
@Sum_hszusumzupure_closure = alias i8, bitcast (%Sum_hszusumzupure_closure_struct* @Sum_hszusumzupure_closure$def to i8*)



; ==================== LLVM Code ====================
@Sum_hszusumzupure_info = alias i8, bitcast (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure_info$def to i8*)
define ghccc void @Sum_hszusumzupure_info$def(i64* noalias nocapture %Base_Arg, i64* noalias nocapture %Sp_Arg, i64* noalias nocapture %Hp_Arg, i64 %R1_Arg, i64 %R2_Arg, i64 %R3_Arg, i64 %R4_Arg, i64 %R5_Arg, i64 %R6_Arg, i64 %SpLim_Arg) align 8 nounwind prefix <{i64, i64, i64, i64}><{i64 add (i64 sub (i64 ptrtoint (i8* @S3kT_srt to i64),i64 ptrtoint (void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure_info$def to i64)),i64 24), i64 4294967299, i64 0, i64 4294967310}>
{
c3l6:
  %ln3l9 = bitcast void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)* @Sum_hszusumzupure1_info$def to void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64)*
  tail call ghccc void (i64*, i64*, i64*, i64, i64, i64, i64, i64, i64, i64) %ln3l9( i64* %Base_Arg, i64* %Sp_Arg, i64* %Hp_Arg, i64 %R1_Arg, i64 undef, i64 undef, i64 undef, i64 undef, i64 undef, i64 %SpLim_Arg ) nounwind
  ret void
}



; ==================== LLVM Code ====================
%S3kT_srt_struct = type <{i64, i64, i64, i64}>
@S3kT_srt$def = internal constant %S3kT_srt_struct<{i64 ptrtoint (i8* @base_GHCziIOziHandleziFD_stdout_closure to i64), i64 ptrtoint (i8* @base_GHCziIOziHandleziText_hPutStr2_closure to i64), i64 ptrtoint (%Sum_hszusumzupure2_closure_struct* @Sum_hszusumzupure2_closure$def to i64), i64 ptrtoint (%Sum_hszusumzupure1_closure_struct* @Sum_hszusumzupure1_closure$def to i64)}>
@S3kT_srt = internal alias i8, bitcast (%S3kT_srt_struct* @S3kT_srt$def to i8*)



; ==================== LLVM Code ====================
@ghczmprim_GHCziTypes_TrNameS_con_info = external global i8
@ghczmprim_GHCziTypes_Module_con_info = external global i8
@newCAF = external global i8
@stg_bh_upd_frame_info = external global i8
@ghczmprim_GHCziTypes_ZMZN_closure = external global i8
@base_GHCziShow_zdwshowSignedInt_info = external global i8
@ghczmprim_GHCziTypes_ZC_con_info = external global i8
@stg_gc_pp = external global i8
@ghczmprim_GHCziTypes_True_closure = external global i8
@base_GHCziIOziHandleziFD_stdout_closure = external global i8
@base_GHCziIOziHandleziText_hPutStr2_info = external global i8
@base_GHCziIOziHandleziText_hPutStr2_closure = external global i8



; ==================== LLVM Code ====================
@llvm.used = appending constant [10 x i8*] [i8* bitcast (%S3kT_srt_struct* @S3kT_srt$def to i8*), i8* bitcast (%Sum_hszusumzupure_closure_struct* @Sum_hszusumzupure_closure$def to i8*), i8* bitcast (%Sum_hszusumzupure1_closure_struct* @Sum_hszusumzupure1_closure$def to i8*), i8* bitcast (%Sum_hszusumzupure2_closure_struct* @Sum_hszusumzupure2_closure$def to i8*), i8* bitcast (%Sum_zdwsum_closure_struct* @Sum_zdwsum_closure$def to i8*), i8* bitcast (%Sum_zdtrModule_closure_struct* @Sum_zdtrModule_closure$def to i8*), i8* bitcast (%Sum_zdtrModule1_closure_struct* @Sum_zdtrModule1_closure$def to i8*), i8* bitcast (%Sum_zdtrModule2_bytes_struct* @Sum_zdtrModule2_bytes$def to i8*), i8* bitcast (%Sum_zdtrModule3_closure_struct* @Sum_zdtrModule3_closure$def to i8*), i8* bitcast (%Sum_zdtrModule4_bytes_struct* @Sum_zdtrModule4_bytes$def to i8*)], section "llvm.metadata"


