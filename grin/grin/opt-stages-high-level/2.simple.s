	.text
	.file	"<string>"
	.globl	grinMain                # -- Begin function grinMain
	.p2align	4, 0x90
	.type	grinMain,@function
grinMain:                               # @grinMain
	.cfi_startproc
# BB#0:                                 # %grinMain.entry
	pushq	%rbx
.Lcfi0:
	.cfi_def_cfa_offset 16
.Lcfi1:
	.cfi_offset %rbx, -16
	movq	%rdi, %rax
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	leaq	16(%rax), %rcx
	movq	$1, 24(%rax)
	movq	$0, 16(%rax)
	leaq	32(%rax), %rsi
	movq	$1000, 40(%rax)         # imm = 0x3E8
	movq	$0, 32(%rax)
	leaq	48(%rax), %rdx
	movq	%rsi, 64(%rax)
	movq	%rcx, 56(%rax)
	movq	$1, 48(%rax)
	movq	%rdx, 88(%rax)
	movq	%rax, 80(%rax)
	movq	$2, 72(%rax)
	leaq	96(%rax), %rdi
	movq	%rax, %rsi
	callq	sum
	movq	%rax, %rbx
	movq	%rdx, %rdi
	callq	_prim_int_print
	movq	%rbx, %rax
	popq	%rbx
	retq
.Lfunc_end0:
	.size	grinMain, .Lfunc_end0-grinMain
	.cfi_endproc
                                        # -- End function
	.globl	sum                     # -- Begin function sum
	.p2align	4, 0x90
	.type	sum,@function
sum:                                    # @sum
	.cfi_startproc
# BB#0:                                 # %sum.entry
	pushq	%rax
.Lcfi2:
	.cfi_def_cfa_offset 16
	movq	8(%rdx), %rcx
	movq	16(%rdx), %rax
	movq	8(%rcx), %rdx
	cmpq	8(%rax), %rdx
	setg	%r8b
	jle	.LBB1_3
# BB#1:                                 # %sum.entry
	testb	%r8b, %r8b
	je	.LBB1_5
# BB#2:                                 # %switch.bool_True.58
	movq	8(%rsi), %rdx
	jmp	.LBB1_4
.LBB1_3:                                # %switch.bool_False.64
	incq	%rdx
	movq	%rdx, 8(%rdi)
	movq	$0, (%rdi)
	movq	%rax, 32(%rdi)
	movq	%rdi, 24(%rdi)
	movq	$1, 16(%rdi)
	leaq	40(%rdi), %rax
	movq	8(%rsi), %rdx
	addq	8(%rcx), %rdx
	movq	%rdx, 48(%rdi)
	movq	$0, 40(%rdi)
	leaq	16(%rdi), %rdx
	addq	$56, %rdi
	movq	%rax, %rsi
	callq	sum
	movq	%rax, %rdi
.LBB1_4:                                # %switch.exit.104
	movq	%rdi, %rax
	popq	%rcx
	retq
.LBB1_5:                                # %error_block
	movl	$666, %edi              # imm = 0x29A
	callq	_prim_int_print
.Lfunc_end1:
	.size	sum, .Lfunc_end1-sum
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
