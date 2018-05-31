	.text
	.file	"<string>"
	.globl	grinMain                # -- Begin function grinMain
	.p2align	4, 0x90
	.type	grinMain,@function
grinMain:                               # @grinMain
	.cfi_startproc
# BB#0:                                 # %grinMain.entry
	movl	$16, %ecx
	movl	$16, %eax
	lock		xaddq	%rax, _heap_ptr_(%rip)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	$16, %edx
	lock		xaddq	%rdx, _heap_ptr_(%rip)
	movq	$0, (%rdx)
	movq	$1, 8(%rdx)
	lock		xaddq	%rcx, _heap_ptr_(%rip)
	movq	$0, (%rcx)
	movq	$1000, 8(%rcx)          # imm = 0x3E8
	movl	$24, %esi
	movl	$24, %edi
	lock		xaddq	%rdi, _heap_ptr_(%rip)
	movq	$1, (%rdi)
	movq	%rdx, 8(%rdi)
	movq	%rcx, 16(%rdi)
	lock		xaddq	%rsi, _heap_ptr_(%rip)
	movq	$2, (%rsi)
	movq	%rax, 8(%rsi)
	movq	%rdi, 16(%rsi)
	movq	8(%rdi), %rcx
	movq	16(%rdi), %rdx
	movq	8(%rcx), %rsi
	cmpq	8(%rdx), %rsi
	jg	.LBB0_3
# BB#1:                                 # %block.bool_False.55.i.preheader
	addq	$8, %rcx
	.p2align	4, 0x90
.LBB0_2:                                # %block.bool_False.55.i
                                        # =>This Inner Loop Header: Depth=1
	incq	%rsi
	movl	$16, %edi
	lock		xaddq	%rdi, _heap_ptr_(%rip)
	movq	$0, (%rdi)
	movq	%rsi, 8(%rdi)
	movl	$24, %esi
	lock		xaddq	%rsi, _heap_ptr_(%rip)
	movq	$1, (%rsi)
	movq	%rdi, 8(%rsi)
	movq	%rdx, 16(%rsi)
	movq	(%rcx), %rcx
	addq	8(%rax), %rcx
	movl	$16, %eax
	lock		xaddq	%rax, _heap_ptr_(%rip)
	movq	$0, (%rax)
	movq	%rcx, 8(%rax)
	movq	8(%rsi), %rcx
	movq	16(%rsi), %rdx
	movq	8(%rcx), %rsi
	addq	$8, %rcx
	cmpq	8(%rdx), %rsi
	jle	.LBB0_2
.LBB0_3:                                # %sum.exit
	movq	8(%rax), %rdi
	jmp	_prim_int_print         # TAILCALL
.Lfunc_end0:
	.size	grinMain, .Lfunc_end0-grinMain
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
