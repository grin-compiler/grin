	.text
	.file	"<string>"
	.globl	grinMain                # -- Begin function grinMain
	.p2align	4, 0x90
	.type	grinMain,@function
grinMain:                               # @grinMain
	.cfi_startproc
# BB#0:                                 # %grinMain.entry
	pushq	%rax
.Lcfi0:
	.cfi_def_cfa_offset 16
	xorl	%edi, %edi
	movl	$1, %esi
	movl	$1000, %edx             # imm = 0x3E8
	callq	.Lsum
	movq	%rax, %rdi
	callq	_prim_int_print
	popq	%rcx
	retq
.Lfunc_end0:
	.size	grinMain, .Lfunc_end0-grinMain
	.cfi_endproc
                                        # -- End function
	.p2align	4, 0x90         # -- Begin function sum
	.type	.Lsum,@function
.Lsum:                                  # @sum
	.cfi_startproc
# BB#0:                                 # %sum.entry
	pushq	%rax
.Lcfi1:
	.cfi_def_cfa_offset 16
	cmpq	%rdx, %rsi
	setg	%al
	jg	.LBB1_3
# BB#1:                                 # %sum.entry
	testb	%al, %al
	jne	.LBB1_4
# BB#2:                                 # %switch.bool_False.5
	addq	%rsi, %rdi
	incq	%rsi
	popq	%rax
	jmp	.Lsum                   # TAILCALL
.LBB1_3:                                # %switch.exit.9
	movq	%rdi, %rax
	popq	%rcx
	retq
.LBB1_4:                                # %error_block
	movl	$666, %edi              # imm = 0x29A
	callq	_prim_int_print
.Lfunc_end1:
	.size	.Lsum, .Lfunc_end1-.Lsum
	.cfi_endproc
                                        # -- End function
	.type	_heap_ptr_,@object      # @_heap_ptr_
	.bss
	.globl	_heap_ptr_
	.p2align	3
_heap_ptr_:
	.quad	0                       # 0x0
	.size	_heap_ptr_, 8


	.section	".note.GNU-stack","",@progbits
