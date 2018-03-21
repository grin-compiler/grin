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
	movq	%rbp, %rax
	leaq	-16(%rax), %rbp
	cmpq	%r15, %rbp
	jb	.LBB0_7
	.p2align	4, 0x90
.LBB0_1:                                # %c3kI
                                        # =>This Inner Loop Header: Depth=1
	movq	$c3kz_info$def, 8(%rbp)
	movl	%r14d, %eax
	andb	$7, %al
	cmpb	$2, %al
	jne	.LBB0_2
# BB#4:                                 # %c3kE.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$c3kO_info$def, (%rbp)
	movq	6(%r14), %rbx
	movq	14(%r14), %r14
	movq	%r14, 8(%rbp)
	testb	$7, %bl
	je	.LBB0_9
# BB#5:                                 # %u3l1.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$c3kT_info$def, (%rbp)
	movq	7(%rbx), %rax
	movq	%rax, 8(%rbp)
	addq	$-16, %rbp
	cmpq	%r15, %rbp
	jae	.LBB0_1
# BB#6:                                 # %c3kH.loopexit
	addq	$16, %rbp
	movq	%rbp, %rax
.LBB0_7:                                # %c3kH
	movq	-8(%r13), %rcx
	movl	$SumSimpleBasic_zdwsum_closure, %ebx
	movq	%rax, %rbp
	jmpq	*%rcx                   # TAILCALL
.LBB0_2:                                # %c3kI
	testb	%al, %al
	jne	.LBB0_3
# BB#8:                                 # %c3kA
	movq	(%r14), %rax
	addq	$8, %rbp
	movq	%r14, %rbx
	jmpq	*%rax                   # TAILCALL
.LBB0_9:                                # %c3kP.i
	movq	(%rbx), %rax
	jmpq	*%rax                   # TAILCALL
.LBB0_3:                                # %c3kD.i
	movq	16(%rbp), %rax
	addq	$16, %rbp
	xorl	%ebx, %ebx
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
	movl	%ebx, %eax
	andl	$7, %eax
	cmpq	$2, %rax
	jne	.LBB1_3
# BB#1:                                 # %c3kE
	leaq	-8(%rbp), %rax
	movq	$c3kO_info$def, -8(%rbp)
	movq	6(%rbx), %rcx
	movq	14(%rbx), %r14
	movq	%r14, (%rbp)
	testb	$7, %cl
	je	.LBB1_4
# BB#2:                                 # %u3l1
	movq	$c3kT_info$def, -8(%rbp)
	movq	7(%rcx), %rcx
	movq	%rcx, (%rbp)
	movq	%rax, %rbp
	jmp	SumSimpleBasic_zdwsum_info$def # TAILCALL
.LBB1_3:                                # %c3kD
	movq	8(%rbp), %rax
	addq	$8, %rbp
	xorl	%ebx, %ebx
	jmpq	*%rax                   # TAILCALL
.LBB1_4:                                # %c3kP
	movq	(%rcx), %rdx
	movq	%rax, %rbp
	movq	%rcx, %rbx
	jmpq	*%rdx                   # TAILCALL
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
	movq	8(%rbp), %r14
	movq	7(%rbx), %rax
	movq	%rax, 8(%rbp)
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
	addq	8(%rbp), %rbx
	movq	16(%rbp), %rax
	addq	$16, %rbp
	jmpq	*%rax                   # TAILCALL
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
	movq	%rbp, %rax
	leaq	-16(%rax), %rbp
	cmpq	%r15, %rbp
	jb	.LBB4_5
# BB#1:                                 # %c3n6
	movq	$stg_upd_frame_info, -16(%rax)
	movq	%rbx, -8(%rax)
	movq	16(%rbx), %r14
	movq	24(%rbx), %rsi
	incq	%r14
	leaq	72(%r12), %rax
	cmpq	%rax, 856(%r13)
	jb	.LBB4_4
# BB#2:                                 # %c3nd.i
	cmpq	%rsi, %r14
	jle	.LBB4_6
# BB#3:                                 # %c3n9.i
	movl	$SumSimpleBasic_Nil_closure+1, %ebx
	jmp	stg_upd_frame_info      # TAILCALL
.LBB4_6:                                # %c3n8.i
	leaq	8(%r12), %rcx
	movq	$s3kp_info$def, 8(%r12)
	movq	%r14, 24(%r12)
	movq	%rsi, 32(%r12)
	movq	$ghczmprim_GHCziTypes_Izh_con_info, 40(%r12)
	movq	%r14, 48(%r12)
	movq	$SumSimpleBasic_Cons_con_info$def, 56(%r12)
	leaq	-31(%rax), %rdx
	movq	%rdx, 64(%r12)
	movq	%rcx, 72(%r12)
	leaq	-14(%rax), %rbx
	movq	%rax, %r12
	jmp	stg_upd_frame_info      # TAILCALL
.LBB4_5:                                # %c3n5
	movq	-16(%r13), %rcx
	movq	%rax, %rbp
	jmpq	*%rcx                   # TAILCALL
.LBB4_4:                                # %c3ne.i
	movq	$72, 904(%r13)
	movq	-8(%r13), %rcx
	movl	$SumSimpleBasic_zdwupto_closure, %ebx
	movq	%rax, %r12
	jmpq	*%rcx                   # TAILCALL
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
	movq	%r12, %rax
	leaq	72(%rax), %r12
	cmpq	%r12, 856(%r13)
	jb	.LBB5_3
# BB#1:                                 # %c3nd
	cmpq	%rsi, %r14
	jle	.LBB5_4
# BB#2:                                 # %c3n9
	movq	(%rbp), %rcx
	movl	$SumSimpleBasic_Nil_closure+1, %ebx
	movq	%rax, %r12
	jmpq	*%rcx                   # TAILCALL
.LBB5_4:                                # %c3n8
	leaq	8(%rax), %rcx
	movq	$s3kp_info$def, 8(%rax)
	movq	%r14, 24(%rax)
	movq	%rsi, 32(%rax)
	movq	$ghczmprim_GHCziTypes_Izh_con_info, 40(%rax)
	movq	%r14, 48(%rax)
	movq	$SumSimpleBasic_Cons_con_info$def, 56(%rax)
	leaq	-31(%r12), %rdx
	movq	%rdx, 64(%rax)
	movq	%rcx, 72(%rax)
	movq	(%rbp), %rax
	leaq	-14(%r12), %rbx
	jmpq	*%rax                   # TAILCALL
.LBB5_3:                                # %c3ne
	movq	$72, 904(%r13)
	movq	-8(%r13), %rax
	movl	$SumSimpleBasic_zdwupto_closure, %ebx
	jmpq	*%rax                   # TAILCALL
.Lfunc_end5:
	.size	SumSimpleBasic_zdwupto_info$def, .Lfunc_end5-SumSimpleBasic_zdwupto_info$def
                                        # -- End function
	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4               # -- Begin function SumSimpleBasic_hszusumzupure2_info$def
.LCPI6_0:
	.quad	1                       # 0x1
	.quad	100000                  # 0x186a0
	.text
	.globl	SumSimpleBasic_hszusumzupure2_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_hszusumzupure2_info$def,@function
	.quad	0                       # @"SumSimpleBasic_hszusumzupure2_info$def"
                                        # 0x0
	.quad	21                      # 0x15
SumSimpleBasic_hszusumzupure2_info$def:
# BB#0:                                 # %c3pj
	movq	%rbp, %r14
	leaq	-24(%r14), %rbp
	cmpq	%r15, %rbp
	jb	.LBB6_5
# BB#1:                                 # %c3pu
	pushq	%rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	newCAF
	testq	%rax, %rax
	leaq	8(%rsp), %rsp
	je	.LBB6_4
# BB#2:                                 # %c3pf
	movq	%rax, -8(%r14)
	movl	$stg_bh_upd_frame_info, %eax
	movq	%rax, %xmm0
	movl	$c3ph_info$def, %eax
	movq	%rax, %xmm1
	punpcklqdq	%xmm0, %xmm1    # xmm1 = xmm1[0],xmm0[0]
	movdqu	%xmm1, -24(%r14)
	leaq	72(%r12), %rax
	cmpq	%rax, 856(%r13)
	jb	.LBB6_3
# BB#6:                                 # %c3nd.i
	leaq	8(%r12), %rcx
	movq	$s3kp_info$def, 8(%r12)
	movaps	.LCPI6_0(%rip), %xmm0   # xmm0 = [1,100000]
	movups	%xmm0, 24(%r12)
	movl	$1, %edx
	movq	%rdx, %xmm0
	movl	$ghczmprim_GHCziTypes_Izh_con_info, %edx
	movq	%rdx, %xmm1
	punpcklqdq	%xmm0, %xmm1    # xmm1 = xmm1[0],xmm0[0]
	movdqu	%xmm1, 40(%r12)
	movq	$SumSimpleBasic_Cons_con_info$def, 56(%r12)
	leaq	-31(%rax), %rdx
	movq	%rdx, 64(%r12)
	movq	%rcx, 72(%r12)
	movq	$c3pm_info$def, (%rbp)
	leaq	-14(%rax), %r14
	movq	%rax, %r12
	jmp	SumSimpleBasic_zdwsum_info$def # TAILCALL
.LBB6_4:                                # %c3pg
	movq	(%rbx), %rax
	movq	%r14, %rbp
	jmpq	*%rax                   # TAILCALL
.LBB6_5:                                # %c3pt
	movq	-16(%r13), %rax
	movq	%r14, %rbp
	jmpq	*%rax                   # TAILCALL
.LBB6_3:                                # %c3ne.i
	movq	$72, 904(%r13)
	movq	-8(%r13), %rcx
	movl	$SumSimpleBasic_zdwupto_closure, %ebx
	movl	$1, %r14d
	movl	$100000, %esi           # imm = 0x186A0
	movq	%rax, %r12
	jmpq	*%rcx                   # TAILCALL
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
	movq	%r12, %rax
	leaq	24(%rax), %r12
	cmpq	%r12, 856(%r13)
	jb	.LBB9_2
# BB#1:                                 # %c3py
	movq	$ghczmprim_GHCziTypes_ZC_con_info, 8(%rax)
	movq	%rbx, 16(%rax)
	movq	%r14, 24(%rax)
	movq	8(%rbp), %rax
	addq	$8, %rbp
	leaq	-14(%r12), %rbx
	jmpq	*%rax                   # TAILCALL
.LBB9_2:                                # %c3pz
	movq	$24, 904(%r13)
	jmp	stg_gc_pp               # TAILCALL
.Lfunc_end9:
	.size	c3pq_info$def, .Lfunc_end9-c3pq_info$def
                                        # -- End function
	.globl	SumSimpleBasic_hszusumzupure1_info$def # -- Begin function SumSimpleBasic_hszusumzupure1_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_hszusumzupure1_info$def,@function
	.quad	S3rl_srt$def-SumSimpleBasic_hszusumzupure1_info$def # @"SumSimpleBasic_hszusumzupure1_info$def"
	.quad	4294967299              # 0x100000003
	.quad	0                       # 0x0
	.quad	30064771086             # 0x70000000e
SumSimpleBasic_hszusumzupure1_info$def:
# BB#0:                                 # %c3ri
	movl	$base_GHCziIOziHandleziFD_stdout_closure, %r14d
	movl	$SumSimpleBasic_hszusumzupure2_closure, %esi
	movl	$ghczmprim_GHCziTypes_True_closure+2, %edi
	jmp	base_GHCziIOziHandleziText_hPutStr2_info # TAILCALL
.Lfunc_end10:
	.size	SumSimpleBasic_hszusumzupure1_info$def, .Lfunc_end10-SumSimpleBasic_hszusumzupure1_info$def
                                        # -- End function
	.globl	SumSimpleBasic_hszusumzupure_info$def # -- Begin function SumSimpleBasic_hszusumzupure_info$def
	.p2align	4, 0x90
	.type	SumSimpleBasic_hszusumzupure_info$def,@function
	.quad	(S3rl_srt$def-SumSimpleBasic_hszusumzupure_info$def)+24 # @"SumSimpleBasic_hszusumzupure_info$def"
	.quad	4294967299              # 0x100000003
	.quad	0                       # 0x0
	.quad	4294967310              # 0x10000000e
SumSimpleBasic_hszusumzupure_info$def:
# BB#0:                                 # %c3ry
	movl	$base_GHCziIOziHandleziFD_stdout_closure, %r14d
	movl	$SumSimpleBasic_hszusumzupure2_closure, %esi
	movl	$ghczmprim_GHCziTypes_True_closure+2, %edi
	jmp	base_GHCziIOziHandleziText_hPutStr2_info # TAILCALL
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
	movq	%r12, %rax
	leaq	24(%rax), %r12
	cmpq	%r12, 856(%r13)
	jb	.LBB12_2
# BB#1:                                 # %c3rK
	movq	$SumSimpleBasic_Cons_con_info$def, 8(%rax)
	movq	%r14, 16(%rax)
	movq	%rsi, 24(%rax)
	movq	(%rbp), %rax
	leaq	-14(%r12), %rbx
	jmpq	*%rax                   # TAILCALL
.LBB12_2:                               # %c3rL
	movq	$24, 904(%r13)
	movq	-8(%r13), %rax
	movl	$SumSimpleBasic_Cons_closure, %ebx
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
	movq	(%rbp), %rax
	jmpq	*%rax                   # TAILCALL
.Lfunc_end14:
	.size	SumSimpleBasic_Cons_con_info$def, .Lfunc_end14-SumSimpleBasic_Cons_con_info$def
                                        # -- End function
	.type	SumSimpleBasic_zdtrModule4_bytes,@object # @SumSimpleBasic_zdtrModule4_bytes
	.section	.rodata,"a",@progbits
	.globl	SumSimpleBasic_zdtrModule4_bytes
SumSimpleBasic_zdtrModule4_bytes:
	.asciz	"main"
	.size	SumSimpleBasic_zdtrModule4_bytes, 5

	.type	SumSimpleBasic_zdtrModule3_closure,@object # @SumSimpleBasic_zdtrModule3_closure
	.data
	.globl	SumSimpleBasic_zdtrModule3_closure
	.p2align	3
SumSimpleBasic_zdtrModule3_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtrModule4_bytes
	.size	SumSimpleBasic_zdtrModule3_closure, 16

	.type	SumSimpleBasic_zdtrModule2_bytes,@object # @SumSimpleBasic_zdtrModule2_bytes
	.section	.rodata,"a",@progbits
	.globl	SumSimpleBasic_zdtrModule2_bytes
SumSimpleBasic_zdtrModule2_bytes:
	.asciz	"SumSimpleBasic"
	.size	SumSimpleBasic_zdtrModule2_bytes, 15

	.type	SumSimpleBasic_zdtrModule1_closure,@object # @SumSimpleBasic_zdtrModule1_closure
	.data
	.globl	SumSimpleBasic_zdtrModule1_closure
	.p2align	3
SumSimpleBasic_zdtrModule1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtrModule2_bytes
	.size	SumSimpleBasic_zdtrModule1_closure, 16

	.type	SumSimpleBasic_zdtrModule_closure,@object # @SumSimpleBasic_zdtrModule_closure
	.globl	SumSimpleBasic_zdtrModule_closure
	.p2align	4
SumSimpleBasic_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	SumSimpleBasic_zdtrModule3_closure+1
	.quad	SumSimpleBasic_zdtrModule1_closure+1
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtrModule_closure, 32

	.type	r3k9_closure$def,@object # @"r3k9_closure$def"
	.p2align	3
r3k9_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepVar_con_info
	.quad	0                       # 0x0
	.size	r3k9_closure$def, 16

	.type	SumSimpleBasic_zdtcList2_bytes,@object # @SumSimpleBasic_zdtcList2_bytes
	.section	.rodata,"a",@progbits
	.globl	SumSimpleBasic_zdtcList2_bytes
SumSimpleBasic_zdtcList2_bytes:
	.asciz	"List"
	.size	SumSimpleBasic_zdtcList2_bytes, 5

	.type	SumSimpleBasic_zdtcList1_closure,@object # @SumSimpleBasic_zdtcList1_closure
	.data
	.globl	SumSimpleBasic_zdtcList1_closure
	.p2align	3
SumSimpleBasic_zdtcList1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtcList2_bytes
	.size	SumSimpleBasic_zdtcList1_closure, 16

	.type	SumSimpleBasic_zdtcList_closure,@object # @SumSimpleBasic_zdtcList_closure
	.globl	SumSimpleBasic_zdtcList_closure
	.p2align	4
SumSimpleBasic_zdtcList_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	SumSimpleBasic_zdtrModule_closure+1
	.quad	SumSimpleBasic_zdtcList1_closure+1
	.quad	ghczmprim_GHCziTypes_krepzdztArrzt_closure
	.quad	8125128733883036046     # 0x70c24188e16af18e
	.quad	8522174167308563467     # 0x7644d840b46ff40b
	.quad	0                       # 0x0
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtcList_closure, 64

	.type	r3ka_closure$def,@object # @"r3ka_closure$def"
	.p2align	4
r3ka_closure$def:
	.quad	ghczmprim_GHCziTypes_ZC_con_info
	.quad	r3k9_closure$def+2
	.quad	ghczmprim_GHCziTypes_ZMZN_closure+1
	.quad	3                       # 0x3
	.size	r3ka_closure$def, 32

	.type	SumSimpleBasic_zdtczqNil1_closure,@object # @SumSimpleBasic_zdtczqNil1_closure
	.globl	SumSimpleBasic_zdtczqNil1_closure
	.p2align	4
SumSimpleBasic_zdtczqNil1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepTyConApp_con_info
	.quad	SumSimpleBasic_zdtcList_closure+1
	.quad	r3ka_closure$def+2
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtczqNil1_closure, 32

	.type	SumSimpleBasic_zdtczqNil3_bytes,@object # @SumSimpleBasic_zdtczqNil3_bytes
	.section	.rodata,"a",@progbits
	.globl	SumSimpleBasic_zdtczqNil3_bytes
SumSimpleBasic_zdtczqNil3_bytes:
	.asciz	"'Nil"
	.size	SumSimpleBasic_zdtczqNil3_bytes, 5

	.type	SumSimpleBasic_zdtczqNil2_closure,@object # @SumSimpleBasic_zdtczqNil2_closure
	.data
	.globl	SumSimpleBasic_zdtczqNil2_closure
	.p2align	3
SumSimpleBasic_zdtczqNil2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtczqNil3_bytes
	.size	SumSimpleBasic_zdtczqNil2_closure, 16

	.type	SumSimpleBasic_zdtczqNil_closure,@object # @SumSimpleBasic_zdtczqNil_closure
	.globl	SumSimpleBasic_zdtczqNil_closure
	.p2align	4
SumSimpleBasic_zdtczqNil_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	SumSimpleBasic_zdtrModule_closure+1
	.quad	SumSimpleBasic_zdtczqNil2_closure+1
	.quad	SumSimpleBasic_zdtczqNil1_closure+1
	.quad	3225848470370281545     # 0x2cc48417afa86849
	.quad	8987752080532592311     # 0x7cbae8e5e96c56b7
	.quad	1                       # 0x1
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtczqNil_closure, 64

	.type	r3kb_closure$def,@object # @"r3kb_closure$def"
	.p2align	4
r3kb_closure$def:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	SumSimpleBasic_zdtczqNil1_closure+1
	.quad	SumSimpleBasic_zdtczqNil1_closure+1
	.quad	3                       # 0x3
	.size	r3kb_closure$def, 32

	.type	SumSimpleBasic_zdtczqCons1_closure,@object # @SumSimpleBasic_zdtczqCons1_closure
	.globl	SumSimpleBasic_zdtczqCons1_closure
	.p2align	4
SumSimpleBasic_zdtczqCons1_closure:
	.quad	ghczmprim_GHCziTypes_KindRepFun_con_info
	.quad	r3k9_closure$def+2
	.quad	r3kb_closure$def+4
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtczqCons1_closure, 32

	.type	SumSimpleBasic_zdtczqCons3_bytes,@object # @SumSimpleBasic_zdtczqCons3_bytes
	.section	.rodata,"a",@progbits
	.globl	SumSimpleBasic_zdtczqCons3_bytes
SumSimpleBasic_zdtczqCons3_bytes:
	.asciz	"'Cons"
	.size	SumSimpleBasic_zdtczqCons3_bytes, 6

	.type	SumSimpleBasic_zdtczqCons2_closure,@object # @SumSimpleBasic_zdtczqCons2_closure
	.data
	.globl	SumSimpleBasic_zdtczqCons2_closure
	.p2align	3
SumSimpleBasic_zdtczqCons2_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	SumSimpleBasic_zdtczqCons3_bytes
	.size	SumSimpleBasic_zdtczqCons2_closure, 16

	.type	SumSimpleBasic_zdtczqCons_closure,@object # @SumSimpleBasic_zdtczqCons_closure
	.globl	SumSimpleBasic_zdtczqCons_closure
	.p2align	4
SumSimpleBasic_zdtczqCons_closure:
	.quad	ghczmprim_GHCziTypes_TyCon_con_info
	.quad	SumSimpleBasic_zdtrModule_closure+1
	.quad	SumSimpleBasic_zdtczqCons2_closure+1
	.quad	SumSimpleBasic_zdtczqCons1_closure+4
	.quad	-7205385302398664762    # 0x9c01543e44b4ffc6
	.quad	6836440343659544598     # 0x5edfea4583fa0016
	.quad	1                       # 0x1
	.quad	3                       # 0x3
	.size	SumSimpleBasic_zdtczqCons_closure, 64

	.type	SumSimpleBasic_zdwsum_closure,@object # @SumSimpleBasic_zdwsum_closure
	.globl	SumSimpleBasic_zdwsum_closure
	.p2align	3
SumSimpleBasic_zdwsum_closure:
	.quad	SumSimpleBasic_zdwsum_info$def
	.size	SumSimpleBasic_zdwsum_closure, 8

	.type	SumSimpleBasic_zdwupto_closure,@object # @SumSimpleBasic_zdwupto_closure
	.globl	SumSimpleBasic_zdwupto_closure
	.p2align	3
SumSimpleBasic_zdwupto_closure:
	.quad	SumSimpleBasic_zdwupto_info$def
	.size	SumSimpleBasic_zdwupto_closure, 8

	.type	SumSimpleBasic_hszusumzupure2_closure,@object # @SumSimpleBasic_hszusumzupure2_closure
	.globl	SumSimpleBasic_hszusumzupure2_closure
	.p2align	4
SumSimpleBasic_hszusumzupure2_closure:
	.quad	SumSimpleBasic_hszusumzupure2_info$def
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.size	SumSimpleBasic_hszusumzupure2_closure, 32

	.type	SumSimpleBasic_hszusumzupure1_closure,@object # @SumSimpleBasic_hszusumzupure1_closure
	.globl	SumSimpleBasic_hszusumzupure1_closure
	.p2align	3
SumSimpleBasic_hszusumzupure1_closure:
	.quad	SumSimpleBasic_hszusumzupure1_info$def
	.quad	0                       # 0x0
	.size	SumSimpleBasic_hszusumzupure1_closure, 16

	.type	SumSimpleBasic_hszusumzupure_closure,@object # @SumSimpleBasic_hszusumzupure_closure
	.globl	SumSimpleBasic_hszusumzupure_closure
	.p2align	3
SumSimpleBasic_hszusumzupure_closure:
	.quad	SumSimpleBasic_hszusumzupure_info$def
	.quad	0                       # 0x0
	.size	SumSimpleBasic_hszusumzupure_closure, 16

	.type	SumSimpleBasic_Nil_closure,@object # @SumSimpleBasic_Nil_closure
	.globl	SumSimpleBasic_Nil_closure
	.p2align	3
SumSimpleBasic_Nil_closure:
	.quad	SumSimpleBasic_Nil_con_info$def
	.size	SumSimpleBasic_Nil_closure, 8

	.type	SumSimpleBasic_Cons_closure,@object # @SumSimpleBasic_Cons_closure
	.globl	SumSimpleBasic_Cons_closure
	.p2align	3
SumSimpleBasic_Cons_closure:
	.quad	SumSimpleBasic_Cons_info$def
	.size	SumSimpleBasic_Cons_closure, 8

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
	.quad	SumSimpleBasic_hszusumzupure2_closure
	.quad	SumSimpleBasic_hszusumzupure1_closure
	.size	S3rl_srt$def, 32


	.globl	SumSimpleBasic_zdwsum_info
SumSimpleBasic_zdwsum_info = SumSimpleBasic_zdwsum_info$def
	.globl	SumSimpleBasic_zdwupto_info
SumSimpleBasic_zdwupto_info = SumSimpleBasic_zdwupto_info$def
	.globl	SumSimpleBasic_hszusumzupure2_info
SumSimpleBasic_hszusumzupure2_info = SumSimpleBasic_hszusumzupure2_info$def
	.globl	SumSimpleBasic_hszusumzupure1_info
SumSimpleBasic_hszusumzupure1_info = SumSimpleBasic_hszusumzupure1_info$def
	.globl	SumSimpleBasic_hszusumzupure_info
SumSimpleBasic_hszusumzupure_info = SumSimpleBasic_hszusumzupure_info$def
	.globl	SumSimpleBasic_Nil_con_info
SumSimpleBasic_Nil_con_info = SumSimpleBasic_Nil_con_info$def
	.globl	SumSimpleBasic_Cons_con_info
SumSimpleBasic_Cons_con_info = SumSimpleBasic_Cons_con_info$def
	.section	".note.GNU-stack","",@progbits
