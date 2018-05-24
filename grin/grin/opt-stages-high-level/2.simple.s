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
	movl	$16, %eax
	movl	$16, %edi
	lock		xaddq	%rdi, _heap_ptr_(%rip)
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	movl	$16, %ecx
	lock		xaddq	%rcx, _heap_ptr_(%rip)
	movq	$1, 8(%rcx)
	movq	$0, (%rcx)
	lock		xaddq	%rax, _heap_ptr_(%rip)
	movq	$1000, 8(%rax)          # imm = 0x3E8
	movq	$0, (%rax)
	movl	$24, %edx
	movl	$24, %esi
	lock		xaddq	%rsi, _heap_ptr_(%rip)
	movq	%rax, 16(%rsi)
	movq	%rcx, 8(%rsi)
	movq	$1, (%rsi)
	lock		xaddq	%rdx, _heap_ptr_(%rip)
	movq	%rsi, 16(%rdx)
	movq	%rdi, 8(%rdx)
	movq	$2, (%rdx)
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
	movq	8(%rsi), %r8
	movq	16(%rsi), %rdx
	movq	8(%r8), %rsi
	cmpq	8(%rdx), %rsi
	setg	%al
	jg	.LBB1_3
# BB#1:                                 # %sum.entry
	testb	%al, %al
	jne	.LBB1_4
# BB#2:                                 # %switch.bool_False.64
	incq	%rsi
	movl	$16, %eax
	movl	$16, %ecx
	lock		xaddq	%rcx, _heap_ptr_(%rip)
	movq	%rsi, 8(%rcx)
	movq	$0, (%rcx)
	movl	$24, %esi
	lock		xaddq	%rsi, _heap_ptr_(%rip)
	movq	%rdx, 16(%rsi)
	movq	%rcx, 8(%rsi)
	movq	$1, (%rsi)
	movq	8(%rdi), %rcx
	addq	8(%r8), %rcx
	lock		xaddq	%rax, _heap_ptr_(%rip)
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, %rdi
	popq	%rax
	jmp	.Lsum                   # TAILCALL
.LBB1_3:                                # %switch.exit.105
	movq	8(%rdi), %rax
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
