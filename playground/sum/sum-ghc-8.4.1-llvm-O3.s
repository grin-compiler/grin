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
	cmpq	%rdi, %rsi
	jg	.LBB0_3
	.p2align	4, 0x90
.LBB0_1:                                # %c3iq
                                        # =>This Inner Loop Header: Depth=1
	addq	%rsi, %r14
	incq	%rsi
	cmpq	%rdi, %rsi
	jle	.LBB0_1
.LBB0_3:                                # %c3ir
	movq	(%rbp), %rax
	movq	%r14, %rbx
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
	movq	%rbp, %r14
	leaq	-24(%r14), %rbp
	cmpq	%r15, %rbp
	jb	.LBB1_3
# BB#1:                                 # %c3j5
	pushq	%rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	newCAF
	testq	%rax, %rax
	leaq	8(%rsp), %rsp
	je	.LBB1_2
# BB#4:                                 # %c3iU
	movq	%rax, -8(%r14)
	movl	$stg_bh_upd_frame_info, %eax
	movq	%rax, %xmm0
	movl	$c3j1_info$def, %eax
	movq	%rax, %xmm1
	punpcklqdq	%xmm0, %xmm1    # xmm1 = xmm1[0],xmm0[0]
	movdqu	%xmm1, -24(%r14)
	movabsq	$5000050000, %rbx       # imm = 0x12A06B550
	xorl	%r14d, %r14d
	movl	$ghczmprim_GHCziTypes_ZMZN_closure+1, %edi
	movq	%rbx, %rsi
	jmp	base_GHCziShow_zdwshowSignedInt_info # TAILCALL
.LBB1_2:                                # %c3iV
	movq	(%rbx), %rax
	movq	%r14, %rbp
	jmpq	*%rax                   # TAILCALL
.LBB1_3:                                # %c3j4
	movq	-16(%r13), %rax
	movq	%r14, %rbp
	jmpq	*%rax                   # TAILCALL
.Lfunc_end1:
	.size	Sum_hszusumzupure2_info$def, .Lfunc_end1-Sum_hszusumzupure2_info$def
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function c3j1_info$def
	.type	c3j1_info$def,@function
	.quad	0                       # @"c3j1_info$def"
                                        # 0x0
	.quad	30                      # 0x1e
c3j1_info$def:
# BB#0:                                 # %c3j1
	movq	%r12, %rax
	leaq	24(%rax), %r12
	cmpq	%r12, 856(%r13)
	jb	.LBB2_2
# BB#1:                                 # %c3j8
	movq	$ghczmprim_GHCziTypes_ZC_con_info, 8(%rax)
	movq	%rbx, 16(%rax)
	movq	%r14, 24(%rax)
	movq	8(%rbp), %rax
	addq	$8, %rbp
	leaq	-14(%r12), %rbx
	jmpq	*%rax                   # TAILCALL
.LBB2_2:                                # %c3j9
	movq	$24, 904(%r13)
	jmp	stg_gc_pp               # TAILCALL
.Lfunc_end2:
	.size	c3j1_info$def, .Lfunc_end2-c3j1_info$def
                                        # -- End function
	.globl	Sum_hszusumzupure1_info$def # -- Begin function Sum_hszusumzupure1_info$def
	.p2align	4, 0x90
	.type	Sum_hszusumzupure1_info$def,@function
	.quad	S3kT_srt$def-Sum_hszusumzupure1_info$def # @"Sum_hszusumzupure1_info$def"
	.quad	4294967299              # 0x100000003
	.quad	0                       # 0x0
	.quad	30064771086             # 0x70000000e
Sum_hszusumzupure1_info$def:
# BB#0:                                 # %c3kQ
	movl	$base_GHCziIOziHandleziFD_stdout_closure, %r14d
	movl	$Sum_hszusumzupure2_closure, %esi
	movl	$ghczmprim_GHCziTypes_True_closure+2, %edi
	jmp	base_GHCziIOziHandleziText_hPutStr2_info # TAILCALL
.Lfunc_end3:
	.size	Sum_hszusumzupure1_info$def, .Lfunc_end3-Sum_hszusumzupure1_info$def
                                        # -- End function
	.globl	Sum_hszusumzupure_info$def # -- Begin function Sum_hszusumzupure_info$def
	.p2align	4, 0x90
	.type	Sum_hszusumzupure_info$def,@function
	.quad	(S3kT_srt$def-Sum_hszusumzupure_info$def)+24 # @"Sum_hszusumzupure_info$def"
	.quad	4294967299              # 0x100000003
	.quad	0                       # 0x0
	.quad	4294967310              # 0x10000000e
Sum_hszusumzupure_info$def:
# BB#0:                                 # %c3l6
	movl	$base_GHCziIOziHandleziFD_stdout_closure, %r14d
	movl	$Sum_hszusumzupure2_closure, %esi
	movl	$ghczmprim_GHCziTypes_True_closure+2, %edi
	jmp	base_GHCziIOziHandleziText_hPutStr2_info # TAILCALL
.Lfunc_end4:
	.size	Sum_hszusumzupure_info$def, .Lfunc_end4-Sum_hszusumzupure_info$def
                                        # -- End function
	.type	Sum_zdtrModule4_bytes,@object # @Sum_zdtrModule4_bytes
	.section	.rodata,"a",@progbits
	.globl	Sum_zdtrModule4_bytes
Sum_zdtrModule4_bytes:
	.asciz	"main"
	.size	Sum_zdtrModule4_bytes, 5

	.type	Sum_zdtrModule3_closure,@object # @Sum_zdtrModule3_closure
	.data
	.globl	Sum_zdtrModule3_closure
	.p2align	3
Sum_zdtrModule3_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Sum_zdtrModule4_bytes
	.size	Sum_zdtrModule3_closure, 16

	.type	Sum_zdtrModule2_bytes,@object # @Sum_zdtrModule2_bytes
	.section	.rodata,"a",@progbits
	.globl	Sum_zdtrModule2_bytes
Sum_zdtrModule2_bytes:
	.asciz	"Sum"
	.size	Sum_zdtrModule2_bytes, 4

	.type	Sum_zdtrModule1_closure,@object # @Sum_zdtrModule1_closure
	.data
	.globl	Sum_zdtrModule1_closure
	.p2align	3
Sum_zdtrModule1_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	Sum_zdtrModule2_bytes
	.size	Sum_zdtrModule1_closure, 16

	.type	Sum_zdtrModule_closure,@object # @Sum_zdtrModule_closure
	.globl	Sum_zdtrModule_closure
	.p2align	4
Sum_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	Sum_zdtrModule3_closure+1
	.quad	Sum_zdtrModule1_closure+1
	.quad	3                       # 0x3
	.size	Sum_zdtrModule_closure, 32

	.type	Sum_zdwsum_closure,@object # @Sum_zdwsum_closure
	.globl	Sum_zdwsum_closure
	.p2align	3
Sum_zdwsum_closure:
	.quad	Sum_zdwsum_info$def
	.size	Sum_zdwsum_closure, 8

	.type	Sum_hszusumzupure2_closure,@object # @Sum_hszusumzupure2_closure
	.globl	Sum_hszusumzupure2_closure
	.p2align	4
Sum_hszusumzupure2_closure:
	.quad	Sum_hszusumzupure2_info$def
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.quad	0                       # 0x0
	.size	Sum_hszusumzupure2_closure, 32

	.type	Sum_hszusumzupure1_closure,@object # @Sum_hszusumzupure1_closure
	.globl	Sum_hszusumzupure1_closure
	.p2align	3
Sum_hszusumzupure1_closure:
	.quad	Sum_hszusumzupure1_info$def
	.quad	0                       # 0x0
	.size	Sum_hszusumzupure1_closure, 16

	.type	Sum_hszusumzupure_closure,@object # @Sum_hszusumzupure_closure
	.globl	Sum_hszusumzupure_closure
	.p2align	3
Sum_hszusumzupure_closure:
	.quad	Sum_hszusumzupure_info$def
	.quad	0                       # 0x0
	.size	Sum_hszusumzupure_closure, 16

	.type	S3kT_srt$def,@object    # @"S3kT_srt$def"
	.section	.rodata,"a",@progbits
	.p2align	4
S3kT_srt$def:
	.quad	base_GHCziIOziHandleziFD_stdout_closure
	.quad	base_GHCziIOziHandleziText_hPutStr2_closure
	.quad	Sum_hszusumzupure2_closure
	.quad	Sum_hszusumzupure1_closure
	.size	S3kT_srt$def, 32


	.globl	Sum_zdwsum_info
Sum_zdwsum_info = Sum_zdwsum_info$def
	.globl	Sum_hszusumzupure2_info
Sum_hszusumzupure2_info = Sum_hszusumzupure2_info$def
	.globl	Sum_hszusumzupure1_info
Sum_hszusumzupure1_info = Sum_hszusumzupure1_info$def
	.globl	Sum_hszusumzupure_info
Sum_hszusumzupure_info = Sum_hszusumzupure_info$def
	.section	".note.GNU-stack","",@progbits
