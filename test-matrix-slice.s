	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%r15
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%rbx
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
	.cfi_def_cfa_offset 64
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	.cfi_offset %r15, -16
	movl	$48, %edi
	callq	_malloc
	movq	%rax, %rbx
	movabsq	$4607632778762754458, %rax ## imm = 0x3FF199999999999A
	movq	%rax, (%rbx)
	movabsq	$4612136378390124954, %rax ## imm = 0x400199999999999A
	movq	%rax, 8(%rbx)
	movabsq	$4614613358185178726, %rax ## imm = 0x400A666666666666
	movq	%rax, 16(%rbx)
	movabsq	$4616639978017495450, %rax ## imm = 0x401199999999999A
	movq	%rax, 24(%rbx)
	movabsq	$4617878467915022336, %rax ## imm = 0x4016000000000000
	movq	%rax, 32(%rbx)
	movabsq	$4619116957812549222, %rax ## imm = 0x401A666666666666
	movq	%rax, 40(%rbx)
	movl	$16, %edi
	callq	_malloc
	movq	%rbx, (%rax)
	movabsq	$12884901890, %rcx      ## imm = 0x300000002
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rsp)
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %rbx
	movq	$0, (%rax)
	movq	$0, 8(%rax)
	movl	$16, %edi
	callq	_malloc
	movq	%rbx, (%rax)
	movabsq	$4294967298, %r14       ## imm = 0x100000002
	movq	%r14, 8(%rax)
	movq	%rax, 8(%rsp)
	movq	16(%rsp), %rax
	movq	(%rax), %r15
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %rbx
	movq	$0, (%rax)
	movq	$0, 8(%rax)
	movl	$16, %edi
	callq	_malloc
	movq	%rbx, (%rax)
	movq	%r14, 8(%rax)
	movsd	8(%r15), %xmm0          ## xmm0 = mem[0],zero
	movsd	%xmm0, (%rbx)
	movsd	32(%r15), %xmm0         ## xmm0 = mem[0],zero
	movsd	%xmm0, 8(%rbx)
	movq	%rax, 8(%rsp)
	movq	(%rax), %rax
	movsd	(%rax), %xmm0           ## xmm0 = mem[0],zero
	movsd	%xmm0, 24(%rsp)
	leaq	L_fmt.2(%rip), %rdi
	movb	$1, %al
	callq	_printf
	xorl	%eax, %eax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%s\n"

L_fmt.1:                                ## @fmt.1
	.asciz	"%d\n"

L_fmt.2:                                ## @fmt.2
	.asciz	"%g\n"


.subsections_via_symbols
