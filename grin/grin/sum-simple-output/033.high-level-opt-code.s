	.text
	.file	"<string>"
	.globl	grinMain                # -- Begin function grinMain
	.p2align	4, 0x90
	.type	grinMain,@function
grinMain:                               # @grinMain
	.cfi_startproc
# BB#0:                                 # %grinMain.entry
	movl	$50005000, %edi         # imm = 0x2FB0408
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
