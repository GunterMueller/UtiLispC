!bignum subroutines for UtiLisp/C
!madd, msub, uadd, usub, mmul, umul, mdiv, mconv 
!as of 1989-04-06
!by Wada
!for detail, see coloquium note of Wadalab on 1989-01-20
!LLL0:
	.seg	"data"
	.seg	"text"
	.proc 04

	.global mtest
mtest:				!mtest returns the distance of sadd and ssub
	save %sp,64,%sp		!this constant is used by mdiv
	sethi %hi(sadd),%l4
	or %l4,%lo(sadd),%l4
	sethi %hi(ssub),%l5
	or %l5,%lo(ssub),%l5
	sub %l5,%l4,%l7
	ret
	restore %l7,0,%o0

	.global madd
madd:				!i0+i1->i0 (length in i2)
	save %sp,-64,%sp	!madd1(a0,a1,length)
	add %i0,%i2,%o4		!a0 a1 are the least significant words
	and %i2,0x3c,%o2	!of augend and addend
	add %i0,%o2,%o0		!sum is extended one word if overflow
	add %i1,%o2,%o1		!normalized otherwise
	sll %o2,2,%o2		!returns the length of sum in byte
	sethi %hi(sadd),%o3
	or %o3,%lo(sadd),%o3
	sub %o3,%o2,%o3		!entrance to sadd
	jmpl %o3,%o7		!call sadd
	addcc %g0,%g0,%g0	!delay instruction
	tst %o5
	bne madd2
	nop
	call norm		!call normalizer
	nop
	ret
	restore %l0,0,%o0	!delay instruction
madd2:
	srl %o3,31,%o3
	sub %o3,1,%o3
	st %o3,[%o4]		!add overflow
	add %o4,4,%o4
	sub %o4,%i0,%l0
	ret
	restore %l0,%g0,%o0	!delay instruction

	.global msub
msub:				!i0+i1->i0 (length in i2)
	save %sp,-64,%sp	!msub1(a0,a1,length)
	add %i0,%i2,%o4		!similar to madd1
	and %i2,0x3c,%o2
	add %i0,%o2,%o0
	add %i1,%o2,%o1
	sll %o2,2,%o2
	sethi %hi(ssub),%o3
	or %o3,%lo(ssub),%o3
	sub %o3,%o2,%o3
	jmpl %o3,%o7		!call ssub
	addcc %g0,%g0,%g0	!delay instruction
	tst %o5
	bne msub2
	nop
	call norm		!call normalizer
	nop
	ret
	restore %l0,0,%o0	!delay instruction
msub2:
	srl %o3,31,%o3
	sub %o3,1,%o3
	st %o3,[%o4]		!add overflow
	add %o4,4,%o4
	sub %o4,%i0,%l0
	ret
	restore %l0,%g0,%o0	!delay instruction

	.global uadd
uadd:				!add one word addend to long augend
	add %o0,4,%o3		!uadd1(a0,addend,length)
	tst %o1			!return the length of sum
	bpos uadd2
	add %o0,%o2,%o4		!delay instruction
uadd6:
	cmp %o3,%o4
	be uadd3
	ld [%o3-4],%o5		!delay instruction
	addcc %o5,%o1,%o5	!add less significant words for negative addend
	bcc uadd5
	st %o5,[%o3-4]		!delay instruction
	retl
	mov %o2,%o0
uadd5:
	sub %g0,1,%o1
	ba uadd6
	add %o3,4,%o3		!delay instruction
uadd2:
	cmp %o3,%o4
	be uadd3
	ld [%o3-4],%o5		!delay instruction
	addcc %o5,%o1,%o5	!add less significant words for positive addend
	bcs uadd4
	st %o5,[%o3-4]		!delay instruction
	retl
	mov %o2,%o0
uadd4:
	add %g0,1,%o1
	ba uadd2
	add %o3,4,%o3		!delay instruction
uadd3:
	addcc %o5,%o1,%o5	!add the most significant word
	bvs uadd7
	st %o5,[%o3-4]		!delay instruction
	save %sp,-64,%sp
	mov %i4,%o4
	call norm
	nop
	ret
	restore %l0,0,%o0
uadd7:
	sra %o5,31,%o5
	xor %o5,-1,%o5		!make overflow word
	st %o5,[%o3]		!store overflow word
	retl
	add %o2,4,%o0

	.global usub
usub:			 	!sub one word subtrahend from long minuend
	add %o0,4,%o3		!similar to uadd1
	tst %o1
	bpos usub2
	add %o0,%o2,%o4		!delay instruction
usub6:
	cmp %o3,%o4
	be usub3
	ld [%o3-4],%o5		!delay instruction
	subcc %o5,%o1,%o5
	bcc usub5
	st %o5,[%o3-4]		!delay instruction
	retl
	mov %o2,%o0
usub5:
	sub %g0,1,%o1
	ba usub6
	add %o3,4,%o3		!delay instruction
usub2:
	cmp %o3,%o4
	be usub3
	ld [%o3-4],%o5		!delay instruction
	subcc %o5,%o1,%o5
	bcs usub4
	st %o5,[%o3-4]		!delay instruction
	retl
	mov %o2,%o0
usub4:
	add %g0,1,%o1
	ba usub2
	add %o3,4,%o3		!delay instruction
usub3:
	subcc %o5,%o1,%o5
	bvs usub7
	st %o5,[%o3-4]		!delay instruction
	save %sp,-64,%sp
	mov %i4,%o4
	call norm
	nop
	ret
	restore %l0,0,%o0
usub7:
	sra %o5,31,%o5
	xor %o5,-1,%o5
	st %o5,[%o3]
	retl
	add %o2,4,%o0

norm:				!normalizer routine
	ld [%o4-4],%o2
	tst %o2
	beq norm1
	nop
	addcc %o2,1,%g0
	beq norm2
	nop
	sub %o4,%i0,%l0
	retl
	nop
norm1:
	ld [%i0],%o0		!search for non zero
	or %o0,3,%o2
	xor %o2,1,%o2
	st %o2,[%i0]		!set sentinel
norm3:
	ld [%o4-4],%o2
	tst %o2
	bne norm4
	nop
	sub %o4,4,%o4
	ba norm3
	nop
norm4:
	bpos norm7
	st %o0,[%i0]		!delay instruction
	add %o4,4,%o4
norm7:
	sub %o4,%i0,%l0
	retl
	nop
norm2:
	ld [%i0],%o0		!search for non minus one
	or %o0,3,%o2
	xor %o2,1,%o2
	st %o2,[%i0]		!set sentinel
norm5:
	ld [%o4-4],%o2
	addcc %o2,1,%o2
	bne norm6
	nop
	sub %o4,4,%o4
	ba norm5
	nop
norm6:
	bneg norm8
	st %o0,[%i0]		!delay instruction
	add %o4,4,%o4
norm8:
	sub %o4,%i0,%l0
	retl
	nop


sadd1:				!16 word adder, enterance is in the middle
	add %o0,64,%o0		!sadd - 4*mod(length,16)
	add %o1,64,%o1
	subcc %g0,%o2,%g0	!o2 remembers the carry of previous cycle
	ld [%o0-64],%o2
	ld [%o1-64],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-64]
	ld [%o0-60],%o2
	ld [%o1-60],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-60]
	ld [%o0-56],%o2
	ld [%o1-56],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-56]
	ld [%o0-52],%o2
	ld [%o1-52],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-52]
	ld [%o0-48],%o2
	ld [%o1-48],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-48]
	ld [%o0-44],%o2
	ld [%o1-44],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-44]
	ld [%o0-40],%o2
	ld [%o1-40],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-40]
	ld [%o0-36],%o2
	ld [%o1-36],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-36]
	ld [%o0-32],%o2
	ld [%o1-32],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-32]
	ld [%o0-28],%o2
	ld [%o1-28],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-28]
	ld [%o0-24],%o2
	ld [%o1-24],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-24]
	ld [%o0-20],%o2
	ld [%o1-20],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-20]
	ld [%o0-16],%o2
	ld [%o1-16],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-16]
	ld [%o0-12],%o2
	ld [%o1-12],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-12]
	ld [%o0-8],%o2
	ld [%o1-8],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-8]
	ld [%o0-4],%o2
	ld [%o1-4],%o3
	addxcc %o2,%o3,%o2
	st %o2,[%o0-4]
sadd:
	mov %o2,%o3
	addx %g0,%g0,%o2
	bvc sadd2
	mov %g0,%o5		!dealy instruction
	add %o5,1,%o5
sadd2:
	cmp %o0,%o4
	bl sadd1 
	nop
	retl
	nop
ssub1:				!sub version of sadd
	add %o0,64,%o0
	add %o1,64,%o1
	subcc %g0,%o2,%g0
	ld [%o0-64],%o2
	ld [%o1-64],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-64]
	ld [%o0-60],%o2
	ld [%o1-60],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-60]
	ld [%o0-56],%o2
	ld [%o1-56],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-56]
	ld [%o0-52],%o2
	ld [%o1-52],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-52]
	ld [%o0-48],%o2
	ld [%o1-48],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-48]
	ld [%o0-44],%o2
	ld [%o1-44],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-44]
	ld [%o0-40],%o2
	ld [%o1-40],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-40]
	ld [%o0-36],%o2
	ld [%o1-36],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-36]
	ld [%o0-32],%o2
	ld [%o1-32],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-32]
	ld [%o0-28],%o2
	ld [%o1-28],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-28]
	ld [%o0-24],%o2
	ld [%o1-24],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-24]
	ld [%o0-20],%o2
	ld [%o1-20],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-20]
	ld [%o0-16],%o2
	ld [%o1-16],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-16]
	ld [%o0-12],%o2
	ld [%o1-12],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-12]
	ld [%o0-8],%o2
	ld [%o1-8],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-8]
	ld [%o0-4],%o2
	ld [%o1-4],%o3
	subxcc %o2,%o3,%o2
	st %o2,[%o0-4]
ssub:
	mov %o2,%o3
	addx %g0,%g0,%o2
	bvc ssub2
	mov %g0,%o5		!dealy instruction
	add %o5,1,%o5
ssub2:
	cmp %o0,%o4
	bl ssub1 
	nop
	retl
	nop

	.global	mmul
mmul:				!multiplier of long * long
	save %sp,-64,%sp	!input dest,(dest+src)size,src,srcsize
	subcc %i1,%i3,%o0	!product is formed on dest and upward
	add %i0,%o0,%l3
	ld [%l3-4],%l3		!mr flag
	add %i2,%i3,%l4
	ld [%l4-4],%l4		!md flag
	mov 0,%i4		!carry of rs1-rs2
	add %i0,%i1,%o4
	mov %o0,%l0		!outer counter
L0:				!outer loop
	ble L1 
	nop
	add %i0,%o0,%l1
L3:
	cmp %l1,%i0
	be L2
	nop
	sub %l1,4,%l1
!	swap [%l1],%l6		!swap is simulated by next 3 instructions
	ld [%l1],%l5
	st %l6,[%l1]
	mov %l5,%l6
	ba L3
	nop
L2:
	mov 0,%l5
	mov %i2,%l2
	add %i0,%o0,%l1
L4:
	cmp %l1,%o4
	be L5
	nop
	ld [%l2],%o1
	mov %o1,%y
	mov %l5,%o2
	addcc %l5,%l5,%l5	!shift left one bit
	nop			!stabilizer, whithout this, result is uncertain
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%l6,%l5
	mulscc %l5,%g0,%l5
	mov %y,%o3
	ld [%l1],%o5
	addcc %o3,%o5,%o5
	st %o5,[%l1-4]
	addx %l5,%g0,%l5
	tst %l6
	bge L7
	nop
	add %l5,%o1,%l5
L7:
	tst %o2
	bge L6
	nop
	add %l5,1,%l5
L6:
	add %l1,4,%l1
	ba L4
	add %l2,4,%l2		!delay instruction
L5:
	tst %l4
	bpos L8
	nop
	subcc %g0,%i4,%g0
	subxcc %l5,%l6,%l5
	bcc L8
	mov 0,%i4		!delay instruction
	mov 1,%i4
L8:
	st %l5,[%o4-4]
	ba L0
	subcc %l0,4,%l0		!delay instruction
L1:
	tst %l3
	bpos L9
	nop
	add %i0,%o0,%o0
	mov %i2,%o1
	call msub
	mov %i3,%o2		!delay instruction
L9:
	call norm
	nop
	ret
	restore %l0,%g0,%o0

	.global umul
umul:				!multiplier of long * one word
	save %sp,-64,%sp
	add %i0,%i1,%o4
	ld [%o4-8],%l3		!mrflag
	mov 0,%i4		!carry
	mov 0,%l5		!rs1
	add %i0,4,%l1
LLL1:
	cmp %l1,%o4
	be LLL2
	nop
	ld [%l1-4],%o1
	mov %o1,%y
	addcc %l5,%l5,%l5
	nop			!stabilizer
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mulscc %l5,%i2,%l5
	mov %y,%o3
	st %o3,[%l1-4]
	tst %i2
	bpos LLL3
	nop
	subcc %g0,%i4,%g0
	bcc LLL3
	mov 0,%i4
	mov 1,%i4
LLL3:
	ba LLL1
	add %l1,4,%l1		!delay instruction
LLL2:
	tst %l3
	bpos LLL4
	nop
	sub %l5,%i2,%l5
LLL4:
	st %l5,[%o4-4]
	call norm
	nop
	ret
	restore %l0,%g0,%o0

	.global	mdiv
mdiv:				!divisor of long / long
	save %sp,-64,%sp
	add %i2,%i3,%l1
	ld [%l1-4],%l1		!sign of divisor, no change during division
LL11:
	add %i0,%i1,%o4		!end of dividend
	ld [%o4-4],%l0		!sign of dividend
	xorcc %l0,%l1,%l5	!sign comparison
	sub %o4,%i3,%o5		!end of quotient
	and %i3,0x3c,%o2
	mov %o2,%o3
	add %o5,%o2,%i4		!terget of dividend/remainder (+/-)
	add %i2,%o2,%i5		!terget f divisor (+/-)
	sll %o2,2,%o2
	sethi %hi(sadd),%l4
	or %l4,%lo(sadd),%l4
	sub %l4,%o2,%l4		!sadd entry, ssub entry = sadd+0x134
!	sethi %hi(ssub),%l5
!	or %l5,%lo(ssub),%l5
!	sub %l5,%o2,%l5		!ssub entry
	add %o3,%o3,%l6
	add %l6,%o3,%o3
	sethi %hi(shift),%l6
	or %l6,%lo(shift),%l6
	sub %l6,%o3,%l6		!shift entry
	mov %i4,%o0
	blt LL1			!jump if signs are different
	mov %i5,%o1		!delay instruction
!	msub (i0,i1,i0,i3)	!sub divisor from dividend
	jmpl %l4+0x134,%o7	!call ssub
	addcc %g0,%g0,%g0	!delay instruction
	xorcc %o3,%l1,%g0  
	blt LL3			!goto main loop
	nop
LL0:				!overflow
	mov %i4,%o0
	blt LL20
	mov %i5,%o1		!delay instruction
	jmpl %l4,%o7
	addcc %g0,%g0,%g0	!delay instruction
	ba LL21
	nop
LL20:
	jmpl %l4+0x134,%o7
	addcc %g0,%g0,%g0	!delay instruction
LL21:
	sra %o3,31,%o3
	st %o3,[%o4]
	ba LL11
	add %i1,4,%i1		!delay instruction
LL1:
!	madd (i0,i2,i0,i3)	!add divisor to dividend
	jmpl %l4,%o7
	addcc %g0,%g0,%g0	!delay instruction
	nop
	xorcc %o3,%l1,%g0 
	blt LL0			!overflow
	nop
LL3:				!entrance of main loop
	mov %i1,%l2		!counter of main loop 
LL8:
	cmp %i3,%l2
	bge LL9			!end of main loop, 
	nop			!exit program or final adjunstment
	mov 32,%l3		!counter on inner loop 32 times
	sub %o4,%i3,%o5
	ld [%o5-4],%l7
LL6:
	ld [%o4-4],%l0
!	madd (i0,i0,i0,i3+4)	!sift partial remainder 1 bit to left
	mov %i4,%o0
	jmpl %l6,%o7
	addcc %l7,%l7,%l7	!delay instruction
	xorcc %l0,%l1,%g0
	mov %i4,%o0
	blt LL4			!jump to add on different sign
	mov %i5,%o1		!delay instruction
!	msub(i0,i2,i0,i3)	!make new partial remainder
	jmpl %l4+0x134,%o7
	addcc %g0,%g0,%g0	!delay instruction
	ba LL5
	add %l7,1,%l7		!make qi 1, delay instruction
LL4:
!	madd(i0,i2,i0,i3)	!make new partial remainder
	jmpl %l4,%o7
	addcc %g0,%g0,%g0	!delay instruction
	nop
LL5:
	subcc %l3,1,%l3		!decr inner counter
	bne LL6
	nop
	sub %o4,%i3,%o5
	mov %i0,%o0		!circulate 
LL7:
	cmp %o0,%o5
	be LL2
	nop
!	swap [%o0],%l7		!simulated by three instructions
	ld [%o0],%o2
	st %l7,[%o0]
	mov %o2,%l7
	ba LL7
	add %o0,4,%o0		!delay instruction
LL2:
	ba LL8
	sub %l2,4,%l2		!delay instruction
LL9:				!shift q 1bit left
	sub %i1,%i3,%o0
	andcc %o0,0x3c,%o1	!clear carry
	add %o1,%i0,%o0		!terget of shift
	add %o1,%o1,%o2
	add %o2,%o1,%o1
	sethi %hi(shift),%o2
	or %o2,%lo(shift),%o2
	sub %o2,%o1,%o2
	jmpl %o2,%o7
	sub %o4,%i3,%o4		!delay instruction
	add %i0,%i1,%o4
	ld [%o4-4],%l0
	xorcc %l0,%l1,%g0
	blt LL10
	nop
	ld [%i0],%l2
	add %l2,1,%l2
	ba L12
	st %l2,[%i0]		!delay instruction
LL10:
	mov %i4,%o0
	mov %i5,%o1
	add %i0,%i1,%o4
	jmpl %l4,%o7
	addcc %g0,%g0,%g0	!delay instruction
L12:
	mov %i0,%i2
	sub %o4,%i3,%i0
	call norm		!normalize remainder
	nop
	sll %l0,16,%l1
	ld [%i0-4],%l0		!get quotinet sign
	xorcc %l0,%l5,%g0
	bpos LL13		!jump if no quotient overflow
	nop
	add %i2,%i1,%o4		!shift remainder
	sub %o4,%i3,%l0
	sra %l5,31,%l5
L14:
	cmp %l0,%o4
	be L15
	nop
	ld [%l0],%l4
	st %l5,[%l0]
	mov %l4,%l5
	ba L14
	add %l0,4,%l0		!delay instruction
L15:
	st %l5,[%l0]
	sub %i1,%i3,%l0
	add %l0,4,%l0
	ba L16
	nop
LL13:
	mov %i2,%i0
	add %i0,%i1,%o4
	sub %o4,%i3,%o4
	call norm
	nop
L16:
	ret
	restore %l0,%l1,%o0
	


shift1:
	add %o0,64,%o0
	subcc %g0,%o3,%g0
	ld [%o0-64],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-64]
	ld [%o0-60],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-60]
	ld [%o0-56],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-56]
	ld [%o0-52],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-52]
	ld [%o0-48],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-48]
	ld [%o0-44],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-44]
	ld [%o0-40],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-40]
	ld [%o0-36],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-36]
	ld [%o0-32],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-32]
	ld [%o0-28],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-28]
	ld [%o0-24],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-24]
	ld [%o0-20],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-20]
	ld [%o0-16],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-16]
	ld [%o0-12],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-12]
	ld [%o0-8],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-8]
	ld [%o0-4],%o2
	addxcc %o2,%o2,%o2
	st %o2,[%o0-4]
shift:
	addx %g0,%g0,%o3
	cmp %o0,%o4
	bl shift1 
	nop
	retl
	nop


	.global mconv
mconv:
!mconv(lsp of 32 bits bignum, length, lsp of 9 digits decimals)
!return length of dicimal words
!place one extra word with 0 content at the top of bignum
	save %sp,-64,%sp
	add %i0,%i1,%l1
	sub %l1,4,%l0
	mov %i2,%l2
	sethi 0xee6b2,%l4
	add %l4,512,%l4		!9th power of ten
L810:
L812:
	ld [%l0],%l6
	tst %l6
	bne L811
	nop
	sub %l0,4,%l0
	ba L812
	nop
L811:
	subcc %l6,%l4,%l6
	bcs L800
	nop
	add %l0,4,%l0
	ld [%l0],%l6
	sub %l6,%l4,%l6
L800:
	st %g0,[%l0]
	sub %l0,4,%l3
	subcc %l3,%i0,%g0
	bl L807
	nop
L801:
	subcc %l3,%i0,%g0
	bl L805 
	nop
	ld [%l3],%l7
	mov 31,%l5
	ba L803
	nop
L802:
	add %l6,%l4,%l6
	subcc %l5,1,%l5
	blt L804
	nop
L803:
	addcc %l7,%l7,%l7
	addxcc %l6,%l6,%l6
	blt L802
	nop
	sub %l6,%l4,%l6
	add %l7,1,%l7
	subcc %l5,1,%l5
	bge L803
	nop
L804:
	add %l7,%l7,%l7
	tst %l6
	blt L808
	nop
	add %l7,1,%l7
L808:
	st %l7,[%l3]
	sub %l3,4,%l3
	ba L801
	nop

L805:
	tst %l6
	bge L806
	nop
	add %l6,%l4,%l6
	st %l7,[%l3]
L806:
	st %l6,[%l2]
	add %l2,4,%l2
	sub %l0,4,%l0
	ba L810
	nop
L807:
	st %l7,[%l2]
	add %l2,4,%l2
	sub %l2,%i2,%l2
	ret
	restore %l2,%g0,%o0

!to get the remainder having the same sign as dividend, use the nest algorithm
!for UtiLisp/C, the algorithm is implemented in C language part
!(defun quo  (x y)
!     (cond (
!            (minusp x)
!            (cond ((minusp y) (quotient (sub1 x) y))
!                  (t (add1 (quotient (sub1 x) y)))))
!           (t (cond ((minusp y) (add1 (quotient x y))) (t (quotient x y))))))
!(defun rem  (x y)
!     (cond (
!            (minusp x)
!            (cond ((minusp y) (add1 (remainder (sub1 x) y)))
!                  (t (difference (add1 (remainder (sub1 x) y) y)))))
!           (t (cond ((minusp y) (difference (remainder x y) y))
!                    (t (remainder x y))))))

	.seg	"data"

