[1 of 1] Compiling Sum              ( sum.hs, sum.o )

==================== Asm code ====================
.section .data
.align 8
.align 1
.globl __stginit_main@main:Sum
.type __stginit_main@main:Sum, @object
__stginit_main@main:Sum:



==================== Asm code ====================
.section .rodata
.align 8
.align 1
c2qa_str:
	.byte	109
	.byte	97
	.byte	105
	.byte	110
	.byte	0



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Sum.$trModule2_closure
.type Sum.$trModule2_closure, @object
Sum.$trModule2_closure:
	.quad	GHC.Types.TrNameS_static_info
	.quad	c2qa_str



==================== Asm code ====================
.section .rodata
.align 8
.align 1
c2qe_str:
	.byte	83
	.byte	117
	.byte	109
	.byte	0



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Sum.$trModule1_closure
.type Sum.$trModule1_closure, @object
Sum.$trModule1_closure:
	.quad	GHC.Types.TrNameS_static_info
	.quad	c2qe_str



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Sum.$trModule_closure
.type Sum.$trModule_closure, @object
Sum.$trModule_closure:
	.quad	GHC.Types.Module_static_info
	.quad	Sum.$trModule2_closure+1
	.quad	Sum.$trModule1_closure+1
	.quad	3



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Sum.$wsum_closure
.type Sum.$wsum_closure, @object
Sum.$wsum_closure:
	.quad	Sum.$wsum_info



==================== Asm code ====================
.section .text
.align 8
.align 8
	.quad	12884901904
	.quad	0
	.quad	15
.globl Sum.$wsum_info
.type Sum.$wsum_info, @object
Sum.$wsum_info:
_c2qE:
_c2qz:
	cmpq %rdi,%rsi
	jg _c2qL
_c2qK:
	addq %rsi,%r14
	incq %rsi
	jmp _c2qz
_c2qL:
	movq %r14,%rbx
	jmp *(%rbp)
	.size Sum.$wsum_info, .-Sum.$wsum_info



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Sum.hs_sum_pure2_closure
.type Sum.hs_sum_pure2_closure, @object
Sum.hs_sum_pure2_closure:
	.quad	Sum.hs_sum_pure2_info
	.quad	0
	.quad	0
	.quad	0



==================== Asm code ====================
.section .text
.align 8
.align 8
	.quad	0
	.quad	22
.globl Sum.hs_sum_pure2_info
.type Sum.hs_sum_pure2_info, @object
Sum.hs_sum_pure2_info:
_c2r7:
	leaq -24(%rbp),%rax
	cmpq %r15,%rax
	jb _c2rd
_c2re:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je _c2r4
_c2r3:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movq $block_c2r5_info,-24(%rbp)
	movl $100000,%edi
	movl $1,%esi
	xorl %r14d,%r14d
	addq $-24,%rbp
	jmp Sum.$wsum_info
_c2ri:
	movq $24,904(%r13)
	jmp stg_gc_pp
.align 8
	.quad	0
	.quad	32
block_c2ra_info:
_c2ra:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja _c2ri
_c2rh:
	movq $:_con_info,-16(%r12)
	movq %rbx,-8(%r12)
	movq %r14,(%r12)
	leaq -14(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
_c2rd:
	jmp *-16(%r13)
.align 8
	.quad	0
	.quad	32
block_c2r5_info:
_c2r5:
	movq $block_c2ra_info,(%rbp)
	movl $GHC.Types.[]_closure+1,%edi
	movq %rbx,%rsi
	xorl %r14d,%r14d
	jmp GHC.Show.$wshowSignedInt_info
_c2r4:
	jmp *(%rbx)
	.size Sum.hs_sum_pure2_info, .-Sum.hs_sum_pure2_info



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Sum.hs_sum_pure1_closure
.type Sum.hs_sum_pure1_closure, @object
Sum.hs_sum_pure1_closure:
	.quad	Sum.hs_sum_pure1_info
	.quad	0



==================== Asm code ====================
.section .text
.align 8
.align 8
	.long	S2rw_srt-(Sum.hs_sum_pure1_info)+0
	.long	0
	.quad	4294967299
	.quad	0
	.quad	30064771087
.globl Sum.hs_sum_pure1_info
.type Sum.hs_sum_pure1_info, @object
Sum.hs_sum_pure1_info:
_c2rt:
	movl $GHC.Types.True_closure+2,%edi
	movl $Sum.hs_sum_pure2_closure,%esi
	movl $GHC.IO.Handle.FD.stdout_closure,%r14d
	jmp GHC.IO.Handle.Text.hPutStr2_info
	.size Sum.hs_sum_pure1_info, .-Sum.hs_sum_pure1_info



==================== Asm code ====================
.section .data
.align 8
.align 1
.globl Sum.hs_sum_pure_closure
.type Sum.hs_sum_pure_closure, @object
Sum.hs_sum_pure_closure:
	.quad	Sum.hs_sum_pure_info
	.quad	0



==================== Asm code ====================
.section .text
.align 8
.align 8
	.long	S2rw_srt-(Sum.hs_sum_pure_info)+24
	.long	0
	.quad	4294967299
	.quad	0
	.quad	4294967311
.globl Sum.hs_sum_pure_info
.type Sum.hs_sum_pure_info, @object
Sum.hs_sum_pure_info:
_c2rF:
	jmp Sum.hs_sum_pure1_info
	.size Sum.hs_sum_pure_info, .-Sum.hs_sum_pure_info



==================== Asm code ====================
.section .data.rel.ro
.align 8
.align 1
S2rw_srt:
	.quad	GHC.IO.Handle.Text.hPutStr2_closure
	.quad	GHC.IO.Handle.FD.stdout_closure
	.quad	Sum.hs_sum_pure2_closure
	.quad	Sum.hs_sum_pure1_closure


