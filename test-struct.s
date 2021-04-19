	.text
	.file	"Seaflow"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	me@GOTPCREL(%rip), %rax
	movl	(%rax), %esi
	leaq	.Lfmt(%rip), %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	me,@object              # @me
	.data
	.globl	me
	.p2align	3
me:
	.long	4                       # 0x4
	.long	5                       # 0x5
	.size	me, 8

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.section	".note.GNU-stack","",@progbits
