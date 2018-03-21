
;;;; Asm code ;;;;
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl SumSimpleBasic.$trModule4_bytes
.type SumSimpleBasic.$trModule4_bytes, @object
SumSimpleBasic.$trModule4_bytes:
	.asciz "main"



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$trModule3_closure
.type SumSimpleBasic.$trModule3_closure, @object
SumSimpleBasic.$trModule3_closure:
	.quad	GHC.Types.TrNameS_con_info
	.quad	SumSimpleBasic.$trModule4_bytes



;;;; Asm code ;;;;
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl SumSimpleBasic.$trModule2_bytes
.type SumSimpleBasic.$trModule2_bytes, @object
SumSimpleBasic.$trModule2_bytes:
	.asciz "SumSimpleBasic"



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$trModule1_closure
.type SumSimpleBasic.$trModule1_closure, @object
SumSimpleBasic.$trModule1_closure:
	.quad	GHC.Types.TrNameS_con_info
	.quad	SumSimpleBasic.$trModule2_bytes



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$trModule_closure
.type SumSimpleBasic.$trModule_closure, @object
SumSimpleBasic.$trModule_closure:
	.quad	GHC.Types.Module_con_info
	.quad	SumSimpleBasic.$trModule3_closure+1
	.quad	SumSimpleBasic.$trModule1_closure+1
	.quad	3



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
$krep_r3k9_closure:
	.quad	GHC.Types.KindRepVar_con_info
	.quad	0



;;;; Asm code ;;;;
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl SumSimpleBasic.$tcList2_bytes
.type SumSimpleBasic.$tcList2_bytes, @object
SumSimpleBasic.$tcList2_bytes:
	.asciz "List"



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$tcList1_closure
.type SumSimpleBasic.$tcList1_closure, @object
SumSimpleBasic.$tcList1_closure:
	.quad	GHC.Types.TrNameS_con_info
	.quad	SumSimpleBasic.$tcList2_bytes



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$tcList_closure
.type SumSimpleBasic.$tcList_closure, @object
SumSimpleBasic.$tcList_closure:
	.quad	GHC.Types.TyCon_con_info
	.quad	SumSimpleBasic.$trModule_closure+1
	.quad	SumSimpleBasic.$tcList1_closure+1
	.quad	GHC.Types.krep$*Arr*_closure
	.quad	8125128733883036046
	.quad	8522174167308563467
	.quad	0
	.quad	3



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
$krep1_r3ka_closure:
	.quad	:_con_info
	.quad	$krep_r3k9_closure+2
	.quad	GHC.Types.[]_closure+1
	.quad	3



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$tc'Nil1_closure
.type SumSimpleBasic.$tc'Nil1_closure, @object
SumSimpleBasic.$tc'Nil1_closure:
	.quad	GHC.Types.KindRepTyConApp_con_info
	.quad	SumSimpleBasic.$tcList_closure+1
	.quad	$krep1_r3ka_closure+2
	.quad	3



;;;; Asm code ;;;;
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl SumSimpleBasic.$tc'Nil3_bytes
.type SumSimpleBasic.$tc'Nil3_bytes, @object
SumSimpleBasic.$tc'Nil3_bytes:
	.asciz "'Nil"



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$tc'Nil2_closure
.type SumSimpleBasic.$tc'Nil2_closure, @object
SumSimpleBasic.$tc'Nil2_closure:
	.quad	GHC.Types.TrNameS_con_info
	.quad	SumSimpleBasic.$tc'Nil3_bytes



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$tc'Nil_closure
.type SumSimpleBasic.$tc'Nil_closure, @object
SumSimpleBasic.$tc'Nil_closure:
	.quad	GHC.Types.TyCon_con_info
	.quad	SumSimpleBasic.$trModule_closure+1
	.quad	SumSimpleBasic.$tc'Nil2_closure+1
	.quad	SumSimpleBasic.$tc'Nil1_closure+1
	.quad	3225848470370281545
	.quad	8987752080532592311
	.quad	1
	.quad	3



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
$krep2_r3kb_closure:
	.quad	GHC.Types.KindRepFun_con_info
	.quad	SumSimpleBasic.$tc'Nil1_closure+1
	.quad	SumSimpleBasic.$tc'Nil1_closure+1
	.quad	3



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$tc'Cons1_closure
.type SumSimpleBasic.$tc'Cons1_closure, @object
SumSimpleBasic.$tc'Cons1_closure:
	.quad	GHC.Types.KindRepFun_con_info
	.quad	$krep_r3k9_closure+2
	.quad	$krep2_r3kb_closure+4
	.quad	3



;;;; Asm code ;;;;
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.globl SumSimpleBasic.$tc'Cons3_bytes
.type SumSimpleBasic.$tc'Cons3_bytes, @object
SumSimpleBasic.$tc'Cons3_bytes:
	.asciz "'Cons"



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$tc'Cons2_closure
.type SumSimpleBasic.$tc'Cons2_closure, @object
SumSimpleBasic.$tc'Cons2_closure:
	.quad	GHC.Types.TrNameS_con_info
	.quad	SumSimpleBasic.$tc'Cons3_bytes



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$tc'Cons_closure
.type SumSimpleBasic.$tc'Cons_closure, @object
SumSimpleBasic.$tc'Cons_closure:
	.quad	GHC.Types.TyCon_con_info
	.quad	SumSimpleBasic.$trModule_closure+1
	.quad	SumSimpleBasic.$tc'Cons2_closure+1
	.quad	SumSimpleBasic.$tc'Cons1_closure+4
	.quad	-7205385302398664762
	.quad	6836440343659544598
	.quad	1
	.quad	3



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$wsum_closure
.type SumSimpleBasic.$wsum_closure, @object
SumSimpleBasic.$wsum_closure:
	.quad	SumSimpleBasic.$wsum_info



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	0
	.quad	14
.globl SumSimpleBasic.$wsum_info
.type SumSimpleBasic.$wsum_info, @object
SumSimpleBasic.$wsum_info:
_c3kZ:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c3l0
_c3l1:
	movq $block_c3kS_info,-8(%rbp)
	movq %r14,%rbx
	addq $-8,%rbp
	testb $7,%bl
	jne _c3kS
_c3kT:
	jmp *(%rbx)
.align 8
	.quad	65
	.quad	30
block_c3lc_info:
_c3lc:
	movq %rbx,%rax
	movq 8(%rbp),%rbx
	addq %rax,%rbx
	addq $16,%rbp
	jmp *(%rbp)
.align 8
	.quad	0
	.quad	30
block_c3kS_info:
_c3kS:
	movq %rbx,%rax
	andl $7,%eax
	cmpq $1,%rax
	je _c3kW
_c3kX:
	movq $block_c3l7_info,-8(%rbp)
	movq 14(%rbx),%rax
	movq 6(%rbx),%rbx
	movq %rax,(%rbp)
	addq $-8,%rbp
	testb $7,%bl
	jne _c3l7
_c3l8:
	jmp *(%rbx)
.align 8
	.quad	1
	.quad	30
block_c3l7_info:
_c3l7:
	movq $block_c3lc_info,(%rbp)
	movq 8(%rbp),%r14
	movq 7(%rbx),%rax
	movq %rax,8(%rbp)
	jmp SumSimpleBasic.$wsum_info
_c3l0:
	movl $SumSimpleBasic.$wsum_closure,%ebx
	jmp *-8(%r13)
_c3kW:
	xorl %ebx,%ebx
	addq $8,%rbp
	jmp *(%rbp)
	.size SumSimpleBasic.$wsum_info, .-SumSimpleBasic.$wsum_info



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.$wupto_closure
.type SumSimpleBasic.$wupto_closure, @object
SumSimpleBasic.$wupto_closure:
	.quad	SumSimpleBasic.$wupto_info



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.quad	8589934592
	.quad	20
sat_s3kp_info:
_c3lF:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c3lG
_c3lH:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 24(%rbx),%rsi
	movq 16(%rbx),%rax
	leaq 1(%rax),%r14
	addq $-16,%rbp
	jmp SumSimpleBasic.$wupto_info
_c3lG:
	jmp *-16(%r13)
	.size sat_s3kp_info, .-sat_s3kp_info



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.quad	8589934604
	.quad	0
	.quad	14
.globl SumSimpleBasic.$wupto_info
.type SumSimpleBasic.$wupto_info, @object
SumSimpleBasic.$wupto_info:
_c3lL:
	addq $72,%r12
	cmpq 856(%r13),%r12
	ja _c3lP
_c3lO:
	cmpq %rsi,%r14
	jle _c3lJ
_c3lK:
	addq $-72,%r12
	movl $SumSimpleBasic.Nil_closure+1,%ebx
	jmp *(%rbp)
_c3lP:
	movq $72,904(%r13)
	movl $SumSimpleBasic.$wupto_closure,%ebx
	jmp *-8(%r13)
_c3lJ:
	movq $sat_s3kp_info,-64(%r12)
	movq %r14,-48(%r12)
	movq %rsi,-40(%r12)
	movq $GHC.Types.I#_con_info,-32(%r12)
	movq %r14,-24(%r12)
	movq $SumSimpleBasic.Cons_con_info,-16(%r12)
	leaq -31(%r12),%rax
	movq %rax,-8(%r12)
	leaq -64(%r12),%rax
	movq %rax,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
	.size SumSimpleBasic.$wupto_info, .-SumSimpleBasic.$wupto_info



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.hs_sum_pure2_closure
.type SumSimpleBasic.hs_sum_pure2_closure, @object
SumSimpleBasic.hs_sum_pure2_closure:
	.quad	SumSimpleBasic.hs_sum_pure2_info
	.quad	0
	.quad	0
	.quad	0



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.quad	0
	.quad	21
.globl SumSimpleBasic.hs_sum_pure2_info
.type SumSimpleBasic.hs_sum_pure2_info, @object
SumSimpleBasic.hs_sum_pure2_info:
_c3m9:
	leaq -24(%rbp),%rax
	cmpq %r15,%rax
	jb _c3mj
_c3mk:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je _c3m6
_c3m5:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movq $block_c3m7_info,-24(%rbp)
	movl $100000,%esi
	movl $1,%r14d
	addq $-24,%rbp
	jmp SumSimpleBasic.$wupto_info
_c3mp:
	movq $24,904(%r13)
	jmp stg_gc_pp
.align 8
	.quad	0
	.quad	30
block_c3mg_info:
_c3mg:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja _c3mp
_c3mo:
	movq $:_con_info,-16(%r12)
	movq %rbx,-8(%r12)
	movq %r14,(%r12)
	leaq -14(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
_c3mj:
	jmp *-16(%r13)
.align 8
	.quad	0
	.quad	30
block_c3mc_info:
_c3mc:
	movq $block_c3mg_info,(%rbp)
	movl $GHC.Types.[]_closure+1,%edi
	movq %rbx,%rsi
	xorl %r14d,%r14d
	jmp GHC.Show.$wshowSignedInt_info
.align 8
	.quad	0
	.quad	30
block_c3m7_info:
_c3m7:
	movq $block_c3mc_info,(%rbp)
	movq %rbx,%r14
	jmp SumSimpleBasic.$wsum_info
_c3m6:
	jmp *(%rbx)
	.size SumSimpleBasic.hs_sum_pure2_info, .-SumSimpleBasic.hs_sum_pure2_info



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.hs_sum_pure1_closure
.type SumSimpleBasic.hs_sum_pure1_closure, @object
SumSimpleBasic.hs_sum_pure1_closure:
	.quad	SumSimpleBasic.hs_sum_pure1_info
	.quad	0



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.long	S3mC_srt-(SumSimpleBasic.hs_sum_pure1_info)+0
	.long	0
	.quad	4294967299
	.quad	0
	.quad	30064771086
.globl SumSimpleBasic.hs_sum_pure1_info
.type SumSimpleBasic.hs_sum_pure1_info, @object
SumSimpleBasic.hs_sum_pure1_info:
_c3mz:
	movl $GHC.Types.True_closure+2,%edi
	movl $SumSimpleBasic.hs_sum_pure2_closure,%esi
	movl $GHC.IO.Handle.FD.stdout_closure,%r14d
	jmp GHC.IO.Handle.Text.hPutStr2_info
	.size SumSimpleBasic.hs_sum_pure1_info, .-SumSimpleBasic.hs_sum_pure1_info



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.hs_sum_pure_closure
.type SumSimpleBasic.hs_sum_pure_closure, @object
SumSimpleBasic.hs_sum_pure_closure:
	.quad	SumSimpleBasic.hs_sum_pure_info
	.quad	0



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.long	S3mC_srt-(SumSimpleBasic.hs_sum_pure_info)+24
	.long	0
	.quad	4294967299
	.quad	0
	.quad	4294967310
.globl SumSimpleBasic.hs_sum_pure_info
.type SumSimpleBasic.hs_sum_pure_info, @object
SumSimpleBasic.hs_sum_pure_info:
_c3mK:
	jmp SumSimpleBasic.hs_sum_pure1_info
	.size SumSimpleBasic.hs_sum_pure_info, .-SumSimpleBasic.hs_sum_pure_info



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.Nil_closure
.type SumSimpleBasic.Nil_closure, @object
SumSimpleBasic.Nil_closure:
	.quad	SumSimpleBasic.Nil_con_info



;;;; Asm code ;;;;
.section .data
.align 8
.align 1
.globl SumSimpleBasic.Cons_closure
.type SumSimpleBasic.Cons_closure, @object
SumSimpleBasic.Cons_closure:
	.quad	SumSimpleBasic.Cons_info



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	0
	.quad	14
SumSimpleBasic.Cons_info:
_c3mW:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja _c3n0
_c3mZ:
	movq $SumSimpleBasic.Cons_con_info,-16(%r12)
	movq %r14,-8(%r12)
	movq %rsi,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
_c3n0:
	movq $24,904(%r13)
	movl $SumSimpleBasic.Cons_closure,%ebx
	jmp *-8(%r13)
	.size SumSimpleBasic.Cons_info, .-SumSimpleBasic.Cons_info



;;;; Asm code ;;;;
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i3n5_str:
	.asciz "main:SumSimpleBasic.Nil"



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.long	i3n5_str-(SumSimpleBasic.Nil_con_info)+0
	.long	0
	.quad	4294967296
	.quad	3
.globl SumSimpleBasic.Nil_con_info
.type SumSimpleBasic.Nil_con_info, @object
SumSimpleBasic.Nil_con_info:
_c3n4:
	incq %rbx
	jmp *(%rbp)
	.size SumSimpleBasic.Nil_con_info, .-SumSimpleBasic.Nil_con_info



;;;; Asm code ;;;;
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
i3na_str:
	.asciz "main:SumSimpleBasic.Cons"



;;;; Asm code ;;;;
.section .text
.align 8
.align 8
	.long	i3na_str-(SumSimpleBasic.Cons_con_info)+0
	.long	0
	.quad	2
	.quad	4294967300
.globl SumSimpleBasic.Cons_con_info
.type SumSimpleBasic.Cons_con_info, @object
SumSimpleBasic.Cons_con_info:
_c3n9:
	addq $2,%rbx
	jmp *(%rbp)
	.size SumSimpleBasic.Cons_con_info, .-SumSimpleBasic.Cons_con_info



;;;; Asm code ;;;;
.section .data.rel.ro
.align 8
.align 1
S3mC_srt:
	.quad	GHC.IO.Handle.FD.stdout_closure
	.quad	GHC.IO.Handle.Text.hPutStr2_closure
	.quad	SumSimpleBasic.hs_sum_pure2_closure
	.quad	SumSimpleBasic.hs_sum_pure1_closure


