	.seg	"data"
	.seg	"text"
	.proc 04

	.global _setjmp
_setjmp:	st	%g0, [%o0]
	st	%g0, [%o0 + 0x4]
	st	%sp, [%o0 + 0x8]
	add	%o7, 0x8, %g1
	st	%g1, [%o0 + 0xc]
	add	%g1, 0x4, %g1
	st	%g1, [%o0 + 0x10]
	st	%g0, [%o0 + 0x14]
	st	%g0, [%o0 + 0x18]
	jmp	%o7 + 0x8
	clr	%o0
	.global _longjmp
_longjmp:	ta	0x3
	sub	%sp, 0x40, %sp
	ld	[%o0 + 0x8], %fp
	ld	[%o0 + 0xc], %g1
	orcc	%g0, %o1, %g0
	bne	_longjmp + 0x20
	sub	%g1, 0x8, %o7
	mov	0x1, %o1
	jmp	%o7 + 0x8
	restore	%o1, 0x0, %o0
	.seg	"data"
