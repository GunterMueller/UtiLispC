	.section	".text"			! [internal]
	.proc	4
	.global	sigbus
	.align 4
sigbus:
!#PROLOGUE# 0
!#PROLOGUE# 1
	sethi	%hi(oregs),%g1		! [internal]
	or	%g1,%lo(oregs),%g1	! [internal]
	st	%i0,[%g1]
	st	%i1,[%g1+4]
	st	%i2,[%g1+8]
	st	%i3,[%g1+12]
	st	%i4,[%g1+16]
	st	%i5,[%g1+20]
	st	%i6,[%g1+24]
	st	%i7,[%g1+28]
	sethi	%hi(gregs),%g1		! [internal]
	or	%g1,%lo(gregs),%g1	! [internal]
	st	%g0,[%g1]
	st	%g1,[%g1+4]
	st	%g2,[%g1+8]
	st	%g3,[%g1+12]
	st	%g4,[%g1+16]
	st	%g5,[%g1+20]
	st	%g6,[%g1+24]
	st	%g7,[%g1+28]
	call	c_sigbus,4
	nop
	ret
	nop
	.proc	4
	.global	sigemt
	.align	4
sigemt:
!#PROLOGUE# 0
!#PROLOGUE# 1
	sethi	%hi(oregs),%g1		! [internal]
	or	%g1,%lo(oregs),%g1	! [internal]
	st	%i0,[%g1]
	st	%i1,[%g1+4]
	st	%i2,[%g1+8]
	st	%i3,[%g1+12]
	st	%i4,[%g1+16]
	st	%i5,[%g1+20]
	st	%i6,[%g1+24]
	st	%i7,[%g1+28]
	sethi	%hi(gregs),%g1		! [internal]
	or	%g1,%lo(gregs),%g1	! [internal]
	st	%g0,[%g1]
	st	%g1,[%g1+4]
	st	%g2,[%g1+8]
	st	%g3,[%g1+12]
	st	%g4,[%g1+16]
	st	%g5,[%g1+20]
	st	%g6,[%g1+24]
	st	%g7,[%g1+28]
	call	c_sigemt,4
	nop
	ret
	nop
	.section	".bss"			! [internal]
	.common	oregs,32,4
	.common	gregs,32,4
