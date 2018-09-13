	.seg	"text"			! [internal]
	.proc	4
	.global	_sigbus
_sigbus:
!#PROLOGUE# 0
!#PROLOGUE# 1
	sethi	%hi(_oregs),%g1		! [internal]
	or	%g1,%lo(_oregs),%g1	! [internal]
	st	%i0,[%g1]
	st	%i1,[%g1+4]
	st	%i2,[%g1+8]
	st	%i3,[%g1+12]
	st	%i4,[%g1+16]
	st	%i5,[%g1+20]
	st	%i6,[%g1+24]
	st	%i7,[%g1+28]
	sethi	%hi(_gregs),%g1		! [internal]
	or	%g1,%lo(_gregs),%g1	! [internal]
	st	%g0,[%g1]
	st	%g1,[%g1+4]
	st	%g2,[%g1+8]
	st	%g3,[%g1+12]
	st	%g4,[%g1+16]
	st	%g5,[%g1+20]
	st	%g6,[%g1+24]
	st	%g7,[%g1+28]
	call	_c_sigbus,4
	nop
	ret
	nop
	.proc	4
	.global	_sigemt
_sigemt:
!#PROLOGUE# 0
!#PROLOGUE# 1
	sethi	%hi(_oregs),%g1		! [internal]
	or	%g1,%lo(_oregs),%g1	! [internal]
	st	%i0,[%g1]
	st	%i1,[%g1+4]
	st	%i2,[%g1+8]
	st	%i3,[%g1+12]
	st	%i4,[%g1+16]
	st	%i5,[%g1+20]
	st	%i6,[%g1+24]
	st	%i7,[%g1+28]
	sethi	%hi(_gregs),%g1		! [internal]
	or	%g1,%lo(_gregs),%g1	! [internal]
	st	%g0,[%g1]
	st	%g1,[%g1+4]
	st	%g2,[%g1+8]
	st	%g3,[%g1+12]
	st	%g4,[%g1+16]
	st	%g5,[%g1+20]
	st	%g6,[%g1+24]
	st	%g7,[%g1+28]
	call	_c_sigemt,4
	nop
	ret
	nop
	.seg	"data"			! [internal]
	.common	_oregs,32,"data"
	.common	_gregs,32,"data"
