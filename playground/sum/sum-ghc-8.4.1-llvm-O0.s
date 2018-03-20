	.text
	.file	"sum.ll"
	.globl	Sum_zdwsum_info$def     # -- Begin function Sum_zdwsum_info$def
	.p2align	4, 0x90
	.type	Sum_zdwsum_info$def,@function
	.quad	12884901904             # @"Sum_zdwsum_info$def"
                                        # 0x300000010
	.quad	0                       # 0x0
	.quad	14                      # 0xe
Sum_zdwsum_info$def:
# BB#0:                                 # %c3is
	movq	%rbx, -24(%rsp)
	movq	%rdi, -16(%rsp)
	movq	%rsi, -32(%rsp)
	movq	%r14, -40(%rsp)
	jmp	.LBB0_1
	.p2align	4, 0x90
.LBB0_2:                                # %c3iq
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	-32(%rsp), %rax
	movq	-40(%rsp), %rcx
	addq	%rax, %rcx
	movq	%rcx, -8(%rsp)
	incq	%rax
	movq	%rax, -32(%rsp)
	movq	%rcx, -40(%rsp)
.LBB0_1:                                # %c3ik
                                        # =>This Inner Loop Header: Depth=1
	movq	-32(%rsp), %rax
	cmpq	-16(%rsp), %rax
	jle	.LBB0_2
# BB#3:                                 # %c3ir
	movq	-40(%rsp), %rbx
	movq	%rbx, -24(%rsp)
	movq	(%rbp), %rax
	jmpq	*%rax                   # TAILCALL
.Lfunc_end0:
	.size	Sum_zdwsum_info$def, .Lfunc_end0-Sum_zdwsum_info$def
                                        # -- End function
	.globl	Sum_hszusumzupure2_info$def # -- Begin function Sum_hszusumzupure2_info$def
	.p2align	4, 0x90
	.type	Sum_hszusumzupure2_info$def,@function
	.quad	0                       # @"Sum_hszusumzupure2_info$def"
                                        # 0x0
	.quad	21                      # 0x15
Sum_hszusumzupure2_info$def:
# BB#0:                                 # %c3iY
	subq	$136, %rsp
	movq	%rbp, (%rsp)
	movq	%rbx, 8(%rsp)
	addq	$-24, %rbp
	cmpq	%r15, %rbp
	jae	.LBB1_1
# BB#4:                                 # %c3j4
	movq	8(%rsp), %rbx
	movq	-16(%r13), %rax
	jmp	.LBB1_3
.LBB1_1:                                # %c3j5
	movq	8(%rsp), %rsi
	movq	%r13, %rdi
	callq	newCAF
	movq	%rax, 16(%rsp)
	testq	%rax, %rax
	je	.LBB1_2
# BB#5:                                 # %c3iU
	movq	(%rsp), %rax
	movq	$stg_bh_upd_frame_info, -16(%rax)
	movq	16(%rsp), %rax
	movq	(%rsp), %rcx
	movq	%rax, -8(%rcx)
	movq	(%rsp), %rax
	movq	$c3iW_info$def, -24(%rax)
	movq	$100000, 32(%rsp)       # imm = 0x186A0
	movq	$1, 40(%rsp)
	movq	$0, 24(%rsp)
	movq	(%rsp), %rbp
	addq	$-24, %rbp
	movq	%rbp, (%rsp)
	movq	8(%rsp), %rbx
	xorl	%r14d, %r14d
	movl	$1, %esi
	movl	$100000, %edi           # imm = 0x186A0
	addq	$136, %rsp
	jmp	Sum_zdwsum_info$def     # TAILCALL
.LBB1_2:                                # %c3iV
	movq	8(%rsp), %rbx
	movq	(%rbx), %rax
.LBB1_3:                                # %c3iV
	movq	(%rsp), %rbp
	addq	$136, %rsp
	jmpq	*%rax                   # TAILCALL
.Lfunc_end1:
	.size	Sum_hszusumzupure2_info$def, .Lfunc_end1-Sum_hszusumzupure2_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3iW_info$def
	.type	c3iW_info$def,@function
	.quad	0                       # @"c3iW_info$def"
                                        # 0x0
	.quad	30                      # 0x1e
c3iW_info$def:
# BB#0:                                 # %c3iW
	movq	$c3j1_info$def, (%rbp)
	movq	$ghczmprim_GHCziTypes_ZMZN_closure+1, -8(%rsp)
	movq	%rbx, -16(%rsp)
	movq	$0, -24(%rsp)
	xorl	%r14d, %r14d
	movl	$ghczmprim_GHCziTypes_ZMZN_closure+1, %edi
	movq	%rbx, %rsi
	jmp	base_GHCziShow_zdwshowSignedInt_info # TAILCALL
.Lfunc_end2:
	.size	c3iW_info$def, .Lfunc_end2-c3iW_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3j1_info$def
	.type	c3j1_info$def,@function
	.quad	0                       # @"c3j1_info$def"
                                        # 0x0
	.quad	30                      # 0x1e
c3j1_info$def:
# BB#0:                                 # %c3j1
	movq	%rbx, -16(%rsp)
	movq	%rbp, -24(%rsp)
	movq	%r14, -8(%rsp)
	addq	$24, %r12
	movq	%r12, -32(%rsp)
	cmpq	856(%r13), %r12
	jbe	.LBB3_1
# BB#2:                                 # %c3j9
	movq	$24, 904(%r13)
	movq	-8(%rsp), %r14
	movq	-16(%rsp), %rbx
	movq	-24(%rsp), %rbp
	movq	-32(%rsp), %r12
	jmp	stg_gc_pp               # TAILCALL
.LBB3_1:                                # %c3j8
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
.Lfunc_end3:
	.size	c3j1_info$def, .Lfunc_end3-c3j1_info$def
                                        # -- End function
	.globl	Sum_hszusumzupure1_info$def # -- Begin function Sum_hszusumzupure1_info$def
	.p2align	4, 0x90
	.type	Sum_hszusumzupure1_info$def,@function
	.quad	S3kT_srt-Sum_hszusumzupure1_info$def # @"Sum_hszusumzupure1_info$def"
	.quad	4294967299              # 0x100000003
	.quad	0                       # 0x0
	.quad	30064771086             # 0x70000000e
Sum_hszusumzupure1_info$def:
# BB#0:                                 # %c3kQ
	movq	$ghczmprim_GHCziTypes_True_closure+2, -8(%rsp)
	movq	$Sum_hszusumzupure2_closure$def, -16(%rsp)
	movq	$base_GHCziIOziHandleziFD_stdout_closure, -24(%rsp)
	movl	$base_GHCziIOziHandleziFD_stdout_closure, %r14d
	movl	$Sum_hszusumzupure2_closure$def, %esi
	movl	$ghczmprim_GHCziTypes_True_closure+2, %edi
	jmp	base_GHCziIOziHandleziText_hPutStr2_info # TAILCALL
.Lfunc_end4:
	.size	Sum_hszusumzupure1_info$def, .Lfunc_end4-Sum_hszusumzupure1_info$def
                                        # -- End function
	.globl	Sum_hszusumzupure_info$def # -- Begin function Sum_hszusumzupure_info$def
	.p2align	4, 0x90
	.type	Sum_hszusumzupure_info$def,@function
	.quad	(S3kT_srt-Sum_hszusumzupure_info$def)+24 # @"Sum_hszusumzupure_info$def"
	.quad	4294967299              # 0x100000003
	.quad	0                       # 0x0
	.quad	4294967310              # 0x10000000e
Sum_hszusumzupure_info$def:
# BB#0:                                 # %c3l6
	jmp	Sum_hszusumzupure1_info$def # TAILCALL
.Lfunc_end5:
	.size	Sum_hszusumzupure_info$def, .Lfunc_end5-Sum_hszusumzupure_info$def
                                        # -- End function
	.type	Sum_zdtrModule4_bytes$def,@object # @"Sum_zdtrModule4_bytes$def"
	.section	.rodata,"a",@progbits
Sum_zdtrModule4_bytes$def:
	.asciz	"main"
	.size	Sum_zdtrModule4_bytes$def, 5

	.type	Sum_zdtrModule3_closure$def,@object # @"Sum_zdtrModule3_closure$def"
	.data
	.p2align	3
Sum_zdtrModule3_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Sum_zdtrModule4_bytes$def
	.size	Sum_zdtrModule3_closure$def, 16

	.type	Sum_zdtrModule2_bytes$def,@object # @"Sum_zdtrModule2_bytes$def"
	.section	.rodata,"a",@progbits
Sum_zdtrModule2_bytes$def:
	.asciz	"Sum"
	.size	Sum_zdtrModule2_bytes$def, 4

	.type	Sum_zdtrModule1_closure$def,@object # @"Sum_zdtrModule1_closure$def"
	.data
	.p2align	3
Sum_zdtrModule1_closure$def:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Sum_zdtrModule2_bytes$def
	.size	Sum_zdtrModule1_closure$def, 16

	.type	Sum_zdtrModule_closure$def,@object # @"Sum_zdtrModule_closure$def"
	.p2align	4
Sum_zdtrModule_closure$def:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	Sum_zdtrModule3_closure$def+1
	.quad	Sum_zdtrModule1_closure$def+1
	.quad	3                       # 0x3
	.size	Sum_zdtrModule_closure$def, 32

	.type	Sum_zdwsum_closure$def,@object # @"Sum_zdwsum_closure$def"
	.p2align	3
Sum_zdwsum_closure$def:
	.quad	Sum_zdwsum_info$def
	.size	Sum_zdwsum_closure$def, 8

	.type	Sum_hszusumzupure2_closure$def,@object # @"Sum_hszusumzupure2_closure$def"
	.p2align	4
Sum_hszusumzupure2_closure$def:
	.quad	Sum_hszusumzupure2_info$def
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.size	Sum_hszusumzupure2_closure$def, 32

	.type	Sum_hszusumzupure1_closure$def,@object # @"Sum_hszusumzupure1_closure$def"
	.p2align	3
Sum_hszusumzupure1_closure$def:
	.quad	Sum_hszusumzupure1_info$def
	.quad	0                       # 0x0
	.size	Sum_hszusumzupure1_closure$def, 16

	.type	Sum_hszusumzupure_closure$def,@object # @"Sum_hszusumzupure_closure$def"
	.p2align	3
Sum_hszusumzupure_closure$def:
	.quad	Sum_hszusumzupure_info$def
	.quad	0                       # 0x0
	.size	Sum_hszusumzupure_closure$def, 16

	.type	S3kT_srt$def,@object    # @"S3kT_srt$def"
	.section	.rodata,"a",@progbits
	.p2align	4
S3kT_srt$def:
	.quad	base_GHCziIOziHandleziFD_stdout_closure
	.quad	base_GHCziIOziHandleziText_hPutStr2_closure
	.quad	Sum_hszusumzupure2_closure$def
	.quad	Sum_hszusumzupure1_closure$def
	.size	S3kT_srt$def, 32


	.globl	Sum_zdtrModule4_bytes
Sum_zdtrModule4_bytes = Sum_zdtrModule4_bytes$def
	.globl	Sum_zdtrModule3_closure
Sum_zdtrModule3_closure = Sum_zdtrModule3_closure$def
	.globl	Sum_zdtrModule2_bytes
Sum_zdtrModule2_bytes = Sum_zdtrModule2_bytes$def
	.globl	Sum_zdtrModule1_closure
Sum_zdtrModule1_closure = Sum_zdtrModule1_closure$def
	.globl	Sum_zdtrModule_closure
Sum_zdtrModule_closure = Sum_zdtrModule_closure$def
	.globl	Sum_zdwsum_closure
Sum_zdwsum_closure = Sum_zdwsum_closure$def
	.globl	Sum_zdwsum_info
Sum_zdwsum_info = Sum_zdwsum_info$def
	.globl	Sum_hszusumzupure2_closure
Sum_hszusumzupure2_closure = Sum_hszusumzupure2_closure$def
	.globl	Sum_hszusumzupure2_info
Sum_hszusumzupure2_info = Sum_hszusumzupure2_info$def
c3iW_info = c3iW_info$def
c3j1_info = c3j1_info$def
	.globl	Sum_hszusumzupure1_closure
Sum_hszusumzupure1_closure = Sum_hszusumzupure1_closure$def
	.globl	Sum_hszusumzupure1_info
Sum_hszusumzupure1_info = Sum_hszusumzupure1_info$def
	.globl	Sum_hszusumzupure_closure
Sum_hszusumzupure_closure = Sum_hszusumzupure_closure$def
	.globl	Sum_hszusumzupure_info
Sum_hszusumzupure_info = Sum_hszusumzupure_info$def
S3kT_srt = S3kT_srt$def
	.section	".note.GNU-stack","",@progbits
