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
	xorl	%esi, %esi
	movl	$1, %edx
	movl	$1000, %ecx             # imm = 0x3E8
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
	cmpq	%rcx, %rdx
	setg	%al
	jg	.LBB1_3
# BB#1:                                 # %sum.entry
	testb	%al, %al
	jne	.LBB1_4
# BB#2:                                 # %switch.bool_False.10
	addq	%rdx, %rsi
	incq	%rdx
	callq	sum
	movq	%rax, %rdi
	movq	%rdx, %rsi
.LBB1_3:                                # %switch.exit.16
	movq	%rdi, %rax
	movq	%rsi, %rdx
	popq	%rcx
	retq
.LBB1_4:                                # %error_block
	movl	$666, %edi              # imm = 0x29A
	callq	_prim_int_print
.Lfunc_end1:
	.size	sum, .Lfunc_end1-sum
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
