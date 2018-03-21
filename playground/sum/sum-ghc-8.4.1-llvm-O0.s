	.text
	.file	"SumSimpleBasic.ll"
	.globl	SumSimpleBasic_zdwsum_info$def # -- Begin function SumSimpleBasic_zdwsum_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_zdwsum_info$def,@function
	.quad	4294967301              # @"SumSimpleBasic_zdwsum_info$def"
                                        # 0x100000005
	.quad	0                       # 0x0
	.quad	14                      # 0xe
SumSimpleBasic_zdwsum_info$def:
# BB#0:                                 # %c3kG
	movq	%rbx, -16(%rsp)
	movq	%rbp, -24(%rsp)
	movq	%r14, -8(%rsp)
	addq	$-16, %rbp
	cmpq	%r15, %rbp
	jae	.LBB0_1
# BB#3:                                 # %c3kH
	movq	-8(%rsp), %r14
	movq	$SumSimpleBasic_zdwsum_closure$def, -16(%rsp)
	movq	-8(%r13), %rax
	movq	-24(%rsp), %rbp
	movl	$SumSimpleBasic_zdwsum_closure$def, %ebx
	jmpq	*%rax                   # TAILCALL
.LBB0_1:                                # %c3kI
	movq	-24(%rsp), %rax
	movq	$c3kz_info$def, -8(%rax)
	movq	-8(%rsp), %rax
	movq	%rax, -16(%rsp)
	addq	$-8, -24(%rsp)
	testb	$7, -16(%rsp)
	je	.LBB0_4
# BB#2:                                 # %u3l2
	movq	-24(%rsp), %rbp
	movq	-16(%rsp), %rbx
	jmp	c3kz_info$def           # TAILCALL
.LBB0_4:                                # %c3kA
	movq	-16(%rsp), %rbx
	movq	(%rbx), %rax
	movq	-24(%rsp), %rbp
	jmpq	*%rax                   # TAILCALL
.Lfunc_end0:
	.size	SumSimpleBasic_zdwsum_info$def, .Lfunc_end0-SumSimpleBasic_zdwsum_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3kz_info$def
	.type	c3kz_info$def,@function
	.quad	0                       # @"c3kz_info$def"
                                        # 0x0
	.quad	30                      # 0x1e
c3kz_info$def:
# BB#0:                                 # %c3kz
	movq	%rbx, -16(%rsp)
	movq	%rbp, -24(%rsp)
	andl	$7, %ebx
	cmpq	$2, %rbx
	jne	.LBB1_3
# BB#1:                                 # %c3kE
	movq	-24(%rsp), %rax
	movq	$c3kO_info$def, -8(%rax)
	movq	-16(%rsp), %rax
	movq	14(%rax), %rcx
	movq	%rcx, -8(%rsp)
	movq	6(%rax), %rax
	movq	%rax, -16(%rsp)
	movq	-24(%rsp), %rax
	movq	%rcx, (%rax)
	addq	$-8, -24(%rsp)
	testb	$7, -16(%rsp)
	je	.LBB1_4
# BB#2:                                 # %u3l1
	movq	-24(%rsp), %rbp
	movq	-16(%rsp), %rbx
	jmp	c3kO_info$def           # TAILCALL
.LBB1_3:                                # %c3kD
	movq	$0, -16(%rsp)
	movq	-24(%rsp), %rax
	leaq	8(%rax), %rbp
	movq	%rbp, -24(%rsp)
	movq	8(%rax), %rax
	xorl	%ebx, %ebx
	jmpq	*%rax                   # TAILCALL
.LBB1_4:                                # %c3kP
	movq	-16(%rsp), %rbx
	movq	(%rbx), %rax
	movq	-24(%rsp), %rbp
	jmpq	*%rax                   # TAILCALL
.Lfunc_end1:
	.size	c3kz_info$def, .Lfunc_end1-c3kz_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3kO_info$def
	.type	c3kO_info$def,@function
	.quad	1                       # @"c3kO_info$def"
                                        # 0x1
	.quad	30                      # 0x1e
c3kO_info$def:
# BB#0:                                 # %c3kO
	movq	$c3kT_info$def, (%rbp)
	movq	8(%rbp), %rax
	movq	%rax, -8(%rsp)
	movq	7(%rbx), %rax
	movq	%rax, 8(%rbp)
	movq	-8(%rsp), %r14
	jmp	SumSimpleBasic_zdwsum_info$def # TAILCALL
.Lfunc_end2:
	.size	c3kO_info$def, .Lfunc_end2-c3kO_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3kT_info$def
	.type	c3kT_info$def,@function
	.quad	65                      # @"c3kT_info$def"
                                        # 0x41
	.quad	30                      # 0x1e
c3kT_info$def:
# BB#0:                                 # %c3kT
	movq	%rbx, -8(%rsp)
	movq	%rbp, -16(%rsp)
	addq	8(%rbp), %rbx
	movq	%rbx, -8(%rsp)
	leaq	16(%rbp), %rax
	movq	%rax, -16(%rsp)
	movq	16(%rbp), %rcx
	movq	%rax, %rbp
	jmpq	*%rcx                   # TAILCALL
.Lfunc_end3:
	.size	c3kT_info$def, .Lfunc_end3-c3kT_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function s3kp_info$def
	.type	s3kp_info$def,@function
	.quad	8589934592              # @"s3kp_info$def"
                                        # 0x200000000
	.quad	20                      # 0x14
s3kp_info$def:
# BB#0:                                 # %c3n4
	movq	%rbp, -32(%rsp)
	movq	%rbx, -24(%rsp)
	addq	$-16, %rbp
	cmpq	%r15, %rbp
	jae	.LBB4_1
# BB#2:                                 # %c3n5
	movq	-24(%rsp), %rbx
	movq	-16(%r13), %rax
	movq	-32(%rsp), %rbp
	jmpq	*%rax                   # TAILCALL
.LBB4_1:                                # %c3n6
	movq	-32(%rsp), %rax
	movq	$stg_upd_frame_info, -16(%rax)
	movq	-24(%rsp), %rax
	movq	-32(%rsp), %rcx
	movq	%rax, -8(%rcx)
	movq	-24(%rsp), %rbx
	movq	24(%rbx), %rsi
	movq	%rsi, -8(%rsp)
	movq	16(%rbx), %r14
	incq	%r14
	movq	%r14, -16(%rsp)
	movq	-32(%rsp), %rbp
	addq	$-16, %rbp
	movq	%rbp, -32(%rsp)
	jmp	SumSimpleBasic_zdwupto_info$def # TAILCALL
.Lfunc_end4:
	.size	s3kp_info$def, .Lfunc_end4-s3kp_info$def
                                        # -- End function
	.globl	SumSimpleBasic_zdwupto_info$def # -- Begin function SumSimpleBasic_zdwupto_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_zdwupto_info$def,@function
	.quad	8589934604              # @"SumSimpleBasic_zdwupto_info$def"
                                        # 0x20000000c
	.quad	0                       # 0x0
	.quad	14                      # 0xe
SumSimpleBasic_zdwupto_info$def:
# BB#0:                                 # %c3na
	movq	%rbx, -8(%rsp)
	movq	%rsi, -16(%rsp)
	movq	%r14, -24(%rsp)
	addq	$72, %r12
	movq	%r12, -32(%rsp)
	cmpq	856(%r13), %r12
	jbe	.LBB5_1
# BB#3:                                 # %c3ne
	movq	$72, 904(%r13)
	movq	-16(%rsp), %rsi
	movq	-24(%rsp), %r14
	movq	$SumSimpleBasic_zdwupto_closure$def, -8(%rsp)
	movq	-8(%r13), %rax
	movq	-32(%rsp), %r12
	movl	$SumSimpleBasic_zdwupto_closure$def, %ebx
	jmpq	*%rax                   # TAILCALL
.LBB5_1:                                # %c3nd
	movq	-24(%rsp), %rax
	cmpq	-16(%rsp), %rax
	jle	.LBB5_4
# BB#2:                                 # %c3n9
	movq	-32(%rsp), %r12
	addq	$-72, %r12
	movq	%r12, -32(%rsp)
	movq	$SumSimpleBasic_Nil_closure+1, -8(%rsp)
	movq	(%rbp), %rax
	movl	$SumSimpleBasic_Nil_closure+1, %ebx
	jmpq	*%rax                   # TAILCALL
.LBB5_4:                                # %c3n8
	movq	-32(%rsp), %rax
	movq	$s3kp_info$def, -64(%rax)
	movq	-24(%rsp), %rax
	movq	-32(%rsp), %rcx
	movq	%rax, -48(%rcx)
	movq	-16(%rsp), %rax
	movq	-32(%rsp), %rcx
	movq	%rax, -40(%rcx)
	movq	-32(%rsp), %rax
	movq	$ghczmprim_GHCziTypes_Izh_con_info, -32(%rax)
	movq	-24(%rsp), %rax
	movq	-32(%rsp), %rcx
	movq	%rax, -24(%rcx)
	movq	-32(%rsp), %rax
	movq	$SumSimpleBasic_Cons_con_info, -16(%rax)
	movq	-32(%rsp), %rax
	leaq	-31(%rax), %rcx
	movq	%rcx, -8(%rax)
	movq	-32(%rsp), %rax
	leaq	-64(%rax), %rcx
	movq	%rcx, (%rax)
	movq	-32(%rsp), %r12
	leaq	-14(%r12), %rbx
	movq	%rbx, -8(%rsp)
	movq	(%rbp), %rax
	jmpq	*%rax                   # TAILCALL
.Lfunc_end5:
	.size	SumSimpleBasic_zdwupto_info$def, .Lfunc_end5-SumSimpleBasic_zdwupto_info$def
                                        # -- End function
	.globl	SumSimpleBasic_hszusumzupure2_info$def # -- Begin function SumSimpleBasic_hszusumzupure2_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_hszusumzupure2_info$def,@function
	.quad	0                       # @"SumSimpleBasic_hszusumzupure2_info$def"
                                        # 0x0
	.quad	21                      # 0x15
SumSimpleBasic_hszusumzupure2_info$def:
# BB#0:                                 # %c3pj
	subq	$136, %rsp
	movq	%rbp, (%rsp)
	movq	%rbx, 8(%rsp)
	addq	$-24, %rbp
	cmpq	%r15, %rbp
	jae	.LBB6_1
# BB#4:                                 # %c3pt
	movq	8(%rsp), %rbx
	movq	-16(%r13), %rax
	jmp	.LBB6_3
.LBB6_1:                                # %c3pu
	movq	8(%rsp), %rsi
	movq	%r13, %rdi
	callq	newCAF
	movq	%rax, 16(%rsp)
	testq	%rax, %rax
	je	.LBB6_2
# BB#5:                                 # %c3pf
	movq	(%rsp), %rax
	movq	$stg_bh_upd_frame_info, -16(%rax)
	movq	16(%rsp), %rax
	movq	(%rsp), %rcx
	movq	%rax, -8(%rcx)
	movq	(%rsp), %rax
	movq	$c3ph_info$def, -24(%rax)
	movq	$100000, 32(%rsp)       # imm = 0x186A0
	movq	$1, 24(%rsp)
	movq	(%rsp), %rbp
	addq	$-24, %rbp
	movq	%rbp, (%rsp)
	movq	8(%rsp), %rbx
	movl	$1, %r14d
	movl	$100000, %esi           # imm = 0x186A0
	addq	$136, %rsp
	jmp	SumSimpleBasic_zdwupto_info$def # TAILCALL
.LBB6_2:                                # %c3pg
	movq	8(%rsp), %rbx
	movq	(%rbx), %rax
.LBB6_3:                                # %c3pg
	movq	(%rsp), %rbp
	addq	$136, %rsp
	jmpq	*%rax                   # TAILCALL
.Lfunc_end6:
	.size	SumSimpleBasic_hszusumzupure2_info$def, .Lfunc_end6-SumSimpleBasic_hszusumzupure2_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3ph_info$def
	.type	c3ph_info$def,@function
	.quad	0                       # @"c3ph_info$def"
                                        # 0x0
	.quad	30                      # 0x1e
c3ph_info$def:
# BB#0:                                 # %c3ph
	movq	$c3pm_info$def, (%rbp)
	movq	%rbx, -8(%rsp)
	movq	%rbx, %r14
	jmp	SumSimpleBasic_zdwsum_info$def # TAILCALL
.Lfunc_end7:
	.size	c3ph_info$def, .Lfunc_end7-c3ph_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3pm_info$def
	.type	c3pm_info$def,@function
	.quad	0                       # @"c3pm_info$def"
                                        # 0x0
	.quad	30                      # 0x1e
c3pm_info$def:
# BB#0:                                 # %c3pm
	movq	$c3pq_info$def, (%rbp)
	movq	$ghczmprim_GHCziTypes_ZMZN_closure+1, -8(%rsp)
	movq	%rbx, -16(%rsp)
	movq	$0, -24(%rsp)
	xorl	%r14d, %r14d
	movl	$ghczmprim_GHCziTypes_ZMZN_closure+1, %edi
	movq	%rbx, %rsi
	jmp	base_GHCziShow_zdwshowSignedInt_info # TAILCALL
.Lfunc_end8:
	.size	c3pm_info$def, .Lfunc_end8-c3pm_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3pq_info$def
	.type	c3pq_info$def,@function
	.quad	0                       # @"c3pq_info$def"
                                        # 0x0
	.quad	30                      # 0x1e
c3pq_info$def:
# BB#0:                                 # %c3pq
	movq	%rbx, -16(%rsp)
	movq	%rbp, -24(%rsp)
	movq	%r14, -8(%rsp)
	addq	$24, %r12
	movq	%r12, -32(%rsp)
	cmpq	856(%r13), %r12
	jbe	.LBB9_1
# BB#2:                                 # %c3pz
	movq	$24, 904(%r13)
	movq	-8(%rsp), %r14
	movq	-16(%rsp), %rbx
	movq	-24(%rsp), %rbp
	movq	-32(%rsp), %r12
	jmp	stg_gc_pp               # TAILCALL
.LBB9_1:                                # %c3py
	movq	-32(%rsp), %rax
	movq	$ghczmprim_GHCziTypes_ZC_con_info, -16(%rax)
	movq	-16(%rsp), %rax
	movq	-32(%rsp), %rcx
	movq	%rax, -8(%rcx)
	movq	-8(%rsp), %rax
	movq	-32(%rsp), %rcx
	movq	%rax, (%rcx)
	movq	-32(%rsp), %r12
	leaq	-14(%r12), %rbx
	movq	%rbx, -16(%rsp)
	movq	-24(%rsp), %rax
	leaq	8(%rax), %rbp
	movq	%rbp, -24(%rsp)
	movq	8(%rax), %rax
	jmpq	*%rax                   # TAILCALL
.Lfunc_end9:
	.size	c3pq_info$def, .Lfunc_end9-c3pq_info$def
                                        # -- End function
	.globl	SumSimpleBasic_hszusumzupure1_info$def # -- Begin function SumSimpleBasic_hszusumzupure1_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_hszusumzupure1_info$def,@function
	.quad	S3rl_srt-SumSimpleBasic_hszusumzupure1_info$def # @"SumSimpleBasic_hszusumzupure1_info$def"
	.quad	4294967299              # 0x100000003
	.quad	0                       # 0x0
	.quad	30064771086             # 0x70000000e
SumSimpleBasic_hszusumzupure1_info$def:
# BB#0:                                 # %c3ri
	movq	$ghczmprim_GHCziTypes_True_closure+2, -8(%rsp)
	movq	$SumSimpleBasic_hszusumzupure2_closure$def, -16(%rsp)
	movq	$base_GHCziIOziHandleziFD_stdout_closure, -24(%rsp)
	movl	$base_GHCziIOziHandleziFD_stdout_closure, %r14d
	movl	$SumSimpleBasic_hszusumzupure2_closure$def, %esi
	movl	$ghczmprim_GHCziTypes_True_closure+2, %edi
	jmp	base_GHCziIOziHandleziText_hPutStr2_info # TAILCALL
.Lfunc_end10:
	.size	SumSimpleBasic_hszusumzupure1_info$def, .Lfunc_end10-SumSimpleBasic_hszusumzupure1_info$def
                                        # -- End function
	.globl	SumSimpleBasic_hszusumzupure_info$def # -- Begin function SumSimpleBasic_hszusumzupure_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_hszusumzupure_info$def,@function
	.quad	(S3rl_srt-SumSimpleBasic_hszusumzupure_info$def)+24 # @"SumSimpleBasic_hszusumzupure_info$def"
	.quad	4294967299              # 0x100000003
	.quad	0                       # 0x0
	.quad	4294967310              # 0x10000000e
SumSimpleBasic_hszusumzupure_info$def:
# BB#0:                                 # %c3ry
	jmp	SumSimpleBasic_hszusumzupure1_info$def # TAILCALL
.Lfunc_end11:
	.size	SumSimpleBasic_hszusumzupure_info$def, .Lfunc_end11-SumSimpleBasic_hszusumzupure_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function SumSimpleBasic_Cons_info$def
	.type	SumSimpleBasic_Cons_info$def,@function
	.quad	8589934607              # @"SumSimpleBasic_Cons_info$def"
                                        # 0x20000000f
	.quad	0                       # 0x0
	.quad	14                      # 0xe
SumSimpleBasic_Cons_info$def:
# BB#0:                                 # %c3rH
	movq	%rbx, -8(%rsp)
	movq	%rsi, -16(%rsp)
	movq	%r14, -24(%rsp)
	addq	$24, %r12
	movq	%r12, -32(%rsp)
	cmpq	856(%r13), %r12
	jbe	.LBB12_1
# BB#2:                                 # %c3rL
	movq	$24, 904(%r13)
	movq	-16(%rsp), %rsi
	movq	-24(%rsp), %r14
	movq	$SumSimpleBasic_Cons_closure$def, -8(%rsp)
	movq	-8(%r13), %rax
	movq	-32(%rsp), %r12
	movl	$SumSimpleBasic_Cons_closure$def, %ebx
	jmpq	*%rax                   # TAILCALL
.LBB12_1:                               # %c3rK
	movq	-32(%rsp), %rax
	movq	$SumSimpleBasic_Cons_con_info, -16(%rax)
	movq	-24(%rsp), %rax
	movq	-32(%rsp), %rcx
	movq	%rax, -8(%rcx)
	movq	-16(%rsp), %rax
	movq	-32(%rsp), %rcx
	movq	%rax, (%rcx)
	movq	-32(%rsp), %r12
	leaq	-14(%r12), %rbx
	movq	%rbx, -8(%rsp)
	movq	(%rbp), %rax
	jmpq	*%rax                   # TAILCALL
.Lfunc_end12:
	.size	SumSimpleBasic_Cons_info$def, .Lfunc_end12-SumSimpleBasic_Cons_info$def
                                        # -- End function
	.globl	SumSimpleBasic_Nil_con_info$def # -- Begin function SumSimpleBasic_Nil_con_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_Nil_con_info$def,@function
	.quad	i3st_str$def-SumSimpleBasic_Nil_con_info$def # @"SumSimpleBasic_Nil_con_info$def"
	.quad	4294967296              # 0x100000000
	.quad	3                       # 0x3
SumSimpleBasic_Nil_con_info$def:
# BB#0:                                 # %c3ss
	incq	%rbx
	movq	%rbx, -8(%rsp)
	movq	(%rbp), %rax
	jmpq	*%rax                   # TAILCALL
.Lfunc_end13:
	.size	SumSimpleBasic_Nil_con_info$def, .Lfunc_end13-SumSimpleBasic_Nil_con_info$def
                                        # -- End function
	.globl	SumSimpleBasic_Cons_con_info$def # -- Begin function SumSimpleBasic_Cons_con_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_Cons_con_info$def,@function
	.quad	i3sD_str$def-SumSimpleBasic_Cons_con_info$def # @"SumSimpleBasic_Cons_con_info$def"
	.quad	2                       # 0x2
	.quad	4294967300              # 0x100000004
SumSimpleBasic_Cons_con_info$def:
# BB#0:                                 # %c3sC
	addq	$2, %rbx
	movq	%rbx, -8(%rsp)
	movq	(%rbp), %rax
	jmpq	*%rax                   # TAILCALL
.Lfunc_end14:
	.size	SumSimpleBasic_Cons_con_info$def, .Lfunc_end14-SumSimpleBasic_Cons_con_info$def
                                        # -- End function
	.type	SumSimpleBasic_zdtrModule4_bytes$def,@object # @"SumSimpleBasic_zdtrModule4_bytes$def"
	.section	.rodata,"a",@progbits
SumSimpleBasic_zdtrModule4_bytes$def:
	.asciz	"main"
	.size	SumSimpleBasic_zdtrModule4_bytes$def, 5

	.type	SumSimpleBasic_zdtrModule3_closure$def,@object # @"SumSimpleBasic_zdtrModule3_closure$def"
	.data
	.p2align	3
SumSimpleBasic_zdtrModule3_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtrModule4_bytes$def
	.size	SumSimpleBasic_zdtrModule3_closure$def, 16

	.type	SumSimpleBasic_zdtrModule2_bytes$def,@object # @"SumSimpleBasic_zdtrModule2_bytes$def"
	.section	.rodata,"a",@progbits
SumSimpleBasic_zdtrModule2_bytes$def:
	.asciz	"SumSimpleBasic"
	.size	SumSimpleBasic_zdtrModule2_bytes$def, 15

	.type	SumSimpleBasic_zdtrModule1_closure$def,@object # @"SumSimpleBasic_zdtrModule1_closure$def"
	.data
	.p2align	3
SumSimpleBasic_zdtrModule1_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtrModule2_bytes$def
	.size	SumSimpleBasic_zdtrModule1_closure$def, 16

	.type	SumSimpleBasic_zdtrModule_closure$def,@object # @"SumSimpleBasic_zdtrModule_closure$def"
	.p2align	4
SumSimpleBasic_zdtrModule_closure$def:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	SumSimpleBasic_zdtrModule3_closure$def+1
	.quad	SumSimpleBasic_zdtrModule1_closure$def+1
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtrModule_closure$def, 32

	.type	r3k9_closure$def,@object # @"r3k9_closure$def"
	.p2align	3
r3k9_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepVar_con_info
	.quad	0                       # 0x0
	.size	r3k9_closure$def, 16

	.type	SumSimpleBasic_zdtcList2_bytes$def,@object # @"SumSimpleBasic_zdtcList2_bytes$def"
	.section	.rodata,"a",@progbits
SumSimpleBasic_zdtcList2_bytes$def:
	.asciz	"List"
	.size	SumSimpleBasic_zdtcList2_bytes$def, 5

	.type	SumSimpleBasic_zdtcList1_closure$def,@object # @"SumSimpleBasic_zdtcList1_closure$def"
	.data
	.p2align	3
SumSimpleBasic_zdtcList1_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtcList2_bytes$def
	.size	SumSimpleBasic_zdtcList1_closure$def, 16

	.type	SumSimpleBasic_zdtcList_closure$def,@object # @"SumSimpleBasic_zdtcList_closure$def"
	.p2align	4
SumSimpleBasic_zdtcList_closure$def:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	SumSimpleBasic_zdtrModule_closure$def+1
	.quad	SumSimpleBasic_zdtcList1_closure$def+1
	.quad	ghczmprim_GHCziTypes_krepzdztArrzt_closure
	.quad	8125128733883036046     # 0x70c24188e16af18e
	.quad	8522174167308563467     # 0x7644d840b46ff40b
	.quad	0                       # 0x0
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtcList_closure$def, 64

	.type	r3ka_closure$def,@object # @"r3ka_closure$def"
	.p2align	4
r3ka_closure$def:
	.quad	ghczmprim_GHCziTypes_ZC_con_info
	.quad	r3k9_closure$def+2
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3                       # 0x3
	.size	r3ka_closure$def, 32

	.type	SumSimpleBasic_zdtczqNil1_closure$def,@object # @"SumSimpleBasic_zdtczqNil1_closure$def"
	.p2align	4
SumSimpleBasic_zdtczqNil1_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	SumSimpleBasic_zdtcList_closure$def+1
	.quad	r3ka_closure$def+2
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtczqNil1_closure$def, 32

	.type	SumSimpleBasic_zdtczqNil3_bytes$def,@object # @"SumSimpleBasic_zdtczqNil3_bytes$def"
	.section	.rodata,"a",@progbits
SumSimpleBasic_zdtczqNil3_bytes$def:
	.asciz	"'Nil"
	.size	SumSimpleBasic_zdtczqNil3_bytes$def, 5

	.type	SumSimpleBasic_zdtczqNil2_closure$def,@object # @"SumSimpleBasic_zdtczqNil2_closure$def"
	.data
	.p2align	3
SumSimpleBasic_zdtczqNil2_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtczqNil3_bytes$def
	.size	SumSimpleBasic_zdtczqNil2_closure$def, 16

	.type	SumSimpleBasic_zdtczqNil_closure$def,@object # @"SumSimpleBasic_zdtczqNil_closure$def"
	.p2align	4
SumSimpleBasic_zdtczqNil_closure$def:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	SumSimpleBasic_zdtrModule_closure$def+1
	.quad	SumSimpleBasic_zdtczqNil2_closure$def+1
	.quad	SumSimpleBasic_zdtczqNil1_closure$def+1
	.quad	3225848470370281545     # 0x2cc48417afa86849
	.quad	8987752080532592311     # 0x7cbae8e5e96c56b7
	.quad	1                       # 0x1
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtczqNil_closure$def, 64

	.type	r3kb_closure$def,@object # @"r3kb_closure$def"
	.p2align	4
r3kb_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	SumSimpleBasic_zdtczqNil1_closure$def+1
	.quad	SumSimpleBasic_zdtczqNil1_closure$def+1
	.quad	3                       # 0x3
	.size	r3kb_closure$def, 32

	.type	SumSimpleBasic_zdtczqCons1_closure$def,@object # @"SumSimpleBasic_zdtczqCons1_closure$def"
	.p2align	4
SumSimpleBasic_zdtczqCons1_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	r3k9_closure$def+2
	.quad	r3kb_closure$def+4
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtczqCons1_closure$def, 32

	.type	SumSimpleBasic_zdtczqCons3_bytes$def,@object # @"SumSimpleBasic_zdtczqCons3_bytes$def"
	.section	.rodata,"a",@progbits
SumSimpleBasic_zdtczqCons3_bytes$def:
	.asciz	"'Cons"
	.size	SumSimpleBasic_zdtczqCons3_bytes$def, 6

	.type	SumSimpleBasic_zdtczqCons2_closure$def,@object # @"SumSimpleBasic_zdtczqCons2_closure$def"
	.data
	.p2align	3
SumSimpleBasic_zdtczqCons2_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtczqCons3_bytes$def
	.size	SumSimpleBasic_zdtczqCons2_closure$def, 16

	.type	SumSimpleBasic_zdtczqCons_closure$def,@object # @"SumSimpleBasic_zdtczqCons_closure$def"
	.p2align	4
SumSimpleBasic_zdtczqCons_closure$def:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	SumSimpleBasic_zdtrModule_closure$def+1
	.quad	SumSimpleBasic_zdtczqCons2_closure$def+1
	.quad	SumSimpleBasic_zdtczqCons1_closure$def+4
	.quad	-7205385302398664762    # 0x9c01543e44b4ffc6
	.quad	6836440343659544598     # 0x5edfea4583fa0016
	.quad	1                       # 0x1
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtczqCons_closure$def, 64

	.type	SumSimpleBasic_zdwsum_closure$def,@object # @"SumSimpleBasic_zdwsum_closure$def"
	.p2align	3
SumSimpleBasic_zdwsum_closure$def:
	.quad	SumSimpleBasic_zdwsum_info$def
	.size	SumSimpleBasic_zdwsum_closure$def, 8

	.type	SumSimpleBasic_zdwupto_closure$def,@object # @"SumSimpleBasic_zdwupto_closure$def"
	.p2align	3
SumSimpleBasic_zdwupto_closure$def:
	.quad	SumSimpleBasic_zdwupto_info$def
	.size	SumSimpleBasic_zdwupto_closure$def, 8

	.type	SumSimpleBasic_hszusumzupure2_closure$def,@object # @"SumSimpleBasic_hszusumzupure2_closure$def"
	.p2align	4
SumSimpleBasic_hszusumzupure2_closure$def:
	.quad	SumSimpleBasic_hszusumzupure2_info$def
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.size	SumSimpleBasic_hszusumzupure2_closure$def, 32

	.type	SumSimpleBasic_hszusumzupure1_closure$def,@object # @"SumSimpleBasic_hszusumzupure1_closure$def"
	.p2align	3
SumSimpleBasic_hszusumzupure1_closure$def:
	.quad	SumSimpleBasic_hszusumzupure1_info$def
	.quad	0                       # 0x0
	.size	SumSimpleBasic_hszusumzupure1_closure$def, 16

	.type	SumSimpleBasic_hszusumzupure_closure$def,@object # @"SumSimpleBasic_hszusumzupure_closure$def"
	.p2align	3
SumSimpleBasic_hszusumzupure_closure$def:
	.quad	SumSimpleBasic_hszusumzupure_info$def
	.quad	0                       # 0x0
	.size	SumSimpleBasic_hszusumzupure_closure$def, 16

	.type	SumSimpleBasic_Nil_closure$def,@object # @"SumSimpleBasic_Nil_closure$def"
	.p2align	3
SumSimpleBasic_Nil_closure$def:
	.quad	SumSimpleBasic_Nil_con_info
	.size	SumSimpleBasic_Nil_closure$def, 8

	.type	SumSimpleBasic_Cons_closure$def,@object # @"SumSimpleBasic_Cons_closure$def"
	.p2align	3
SumSimpleBasic_Cons_closure$def:
	.quad	SumSimpleBasic_Cons_info$def
	.size	SumSimpleBasic_Cons_closure$def, 8

	.type	i3st_str$def,@object    # @"i3st_str$def"
	.section	.rodata,"a",@progbits
i3st_str$def:
	.asciz	"main:SumSimpleBasic.Nil"
	.size	i3st_str$def, 24

	.type	i3sD_str$def,@object    # @"i3sD_str$def"
i3sD_str$def:
	.asciz	"main:SumSimpleBasic.Cons"
	.size	i3sD_str$def, 25

	.type	S3rl_srt$def,@object    # @"S3rl_srt$def"
	.p2align	4
S3rl_srt$def:
	.quad	base_GHCziIOziHandleziFD_stdout_closure
	.quad	base_GHCziIOziHandleziText_hPutStr2_closure
	.quad	SumSimpleBasic_hszusumzupure2_closure$def
	.quad	SumSimpleBasic_hszusumzupure1_closure$def
	.size	S3rl_srt$def, 32


	.globl	SumSimpleBasic_zdtrModule4_bytes
SumSimpleBasic_zdtrModule4_bytes = SumSimpleBasic_zdtrModule4_bytes$def
	.globl	SumSimpleBasic_zdtrModule3_closure
SumSimpleBasic_zdtrModule3_closure = SumSimpleBasic_zdtrModule3_closure$def
	.globl	SumSimpleBasic_zdtrModule2_bytes
SumSimpleBasic_zdtrModule2_bytes = SumSimpleBasic_zdtrModule2_bytes$def
	.globl	SumSimpleBasic_zdtrModule1_closure
SumSimpleBasic_zdtrModule1_closure = SumSimpleBasic_zdtrModule1_closure$def
	.globl	SumSimpleBasic_zdtrModule_closure
SumSimpleBasic_zdtrModule_closure = SumSimpleBasic_zdtrModule_closure$def
r3k9_closure = r3k9_closure$def
	.globl	SumSimpleBasic_zdtcList2_bytes
SumSimpleBasic_zdtcList2_bytes = SumSimpleBasic_zdtcList2_bytes$def
	.globl	SumSimpleBasic_zdtcList1_closure
SumSimpleBasic_zdtcList1_closure = SumSimpleBasic_zdtcList1_closure$def
	.globl	SumSimpleBasic_zdtcList_closure
SumSimpleBasic_zdtcList_closure = SumSimpleBasic_zdtcList_closure$def
r3ka_closure = r3ka_closure$def
	.globl	SumSimpleBasic_zdtczqNil1_closure
SumSimpleBasic_zdtczqNil1_closure = SumSimpleBasic_zdtczqNil1_closure$def
	.globl	SumSimpleBasic_zdtczqNil3_bytes
SumSimpleBasic_zdtczqNil3_bytes = SumSimpleBasic_zdtczqNil3_bytes$def
	.globl	SumSimpleBasic_zdtczqNil2_closure
SumSimpleBasic_zdtczqNil2_closure = SumSimpleBasic_zdtczqNil2_closure$def
	.globl	SumSimpleBasic_zdtczqNil_closure
SumSimpleBasic_zdtczqNil_closure = SumSimpleBasic_zdtczqNil_closure$def
r3kb_closure = r3kb_closure$def
	.globl	SumSimpleBasic_zdtczqCons1_closure
SumSimpleBasic_zdtczqCons1_closure = SumSimpleBasic_zdtczqCons1_closure$def
	.globl	SumSimpleBasic_zdtczqCons3_bytes
SumSimpleBasic_zdtczqCons3_bytes = SumSimpleBasic_zdtczqCons3_bytes$def
	.globl	SumSimpleBasic_zdtczqCons2_closure
SumSimpleBasic_zdtczqCons2_closure = SumSimpleBasic_zdtczqCons2_closure$def
	.globl	SumSimpleBasic_zdtczqCons_closure
SumSimpleBasic_zdtczqCons_closure = SumSimpleBasic_zdtczqCons_closure$def
	.globl	SumSimpleBasic_zdwsum_closure
SumSimpleBasic_zdwsum_closure = SumSimpleBasic_zdwsum_closure$def
	.globl	SumSimpleBasic_zdwsum_info
SumSimpleBasic_zdwsum_info = SumSimpleBasic_zdwsum_info$def
c3kz_info = c3kz_info$def
c3kO_info = c3kO_info$def
c3kT_info = c3kT_info$def
	.globl	SumSimpleBasic_zdwupto_closure
SumSimpleBasic_zdwupto_closure = SumSimpleBasic_zdwupto_closure$def
s3kp_info = s3kp_info$def
	.globl	SumSimpleBasic_zdwupto_info
SumSimpleBasic_zdwupto_info = SumSimpleBasic_zdwupto_info$def
	.globl	SumSimpleBasic_hszusumzupure2_closure
SumSimpleBasic_hszusumzupure2_closure = SumSimpleBasic_hszusumzupure2_closure$def
	.globl	SumSimpleBasic_hszusumzupure2_info
SumSimpleBasic_hszusumzupure2_info = SumSimpleBasic_hszusumzupure2_info$def
c3ph_info = c3ph_info$def
c3pm_info = c3pm_info$def
c3pq_info = c3pq_info$def
	.globl	SumSimpleBasic_hszusumzupure1_closure
SumSimpleBasic_hszusumzupure1_closure = SumSimpleBasic_hszusumzupure1_closure$def
	.globl	SumSimpleBasic_hszusumzupure1_info
SumSimpleBasic_hszusumzupure1_info = SumSimpleBasic_hszusumzupure1_info$def
	.globl	SumSimpleBasic_hszusumzupure_closure
SumSimpleBasic_hszusumzupure_closure = SumSimpleBasic_hszusumzupure_closure$def
	.globl	SumSimpleBasic_hszusumzupure_info
SumSimpleBasic_hszusumzupure_info = SumSimpleBasic_hszusumzupure_info$def
	.globl	SumSimpleBasic_Nil_closure
SumSimpleBasic_Nil_closure = SumSimpleBasic_Nil_closure$def
	.globl	SumSimpleBasic_Cons_closure
SumSimpleBasic_Cons_closure = SumSimpleBasic_Cons_closure$def
SumSimpleBasic_Cons_info = SumSimpleBasic_Cons_info$def
i3st_str = i3st_str$def
	.globl	SumSimpleBasic_Nil_con_info
SumSimpleBasic_Nil_con_info = SumSimpleBasic_Nil_con_info$def
i3sD_str = i3sD_str$def
	.globl	SumSimpleBasic_Cons_con_info
SumSimpleBasic_Cons_con_info = SumSimpleBasic_Cons_con_info$def
S3rl_srt = S3rl_srt$def
	.section	".note.GNU-stack","",@progbits
