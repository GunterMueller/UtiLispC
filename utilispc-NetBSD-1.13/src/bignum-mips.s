/* bignum subroutines for UtiLisp/C on mips */
/* madd, musb, uadd, usub, mmul, umul, mdiv, mcomv */
/* as of 1991-01-28 */
/* by Wada */
/* for detail, see coloquium note of Wadalab on 1991-02-02 */
#include <regdef.h>
	.set	at
	.set	noreorder
	.text
	.align	2
	.globl	madd
	.ent	madd
madd:				## $4+$5->$4 (length in $6)
	subu	$29,4		## madd(a0,a1,length)
	sw	$31,($29)	## a0 a1 are the least significant words
	move	$8,$4		## of augend and addend
	move	$9,$5		## sum is extended on word if overflow
	addu	$10,$4,$6	## normalized otherwise
	jal	$100		## returns the length of sum in byte
	nop
	jal	$104
	nop
	subu	$2,$10,$4
	bne	$2,$6,$32
	nop
	jal	$610
	nop
$32:
	subu	$2,$10,$4
	lw	$31,($29)
	nop
	addu	$29,4
	j	$31
$100:
	lw	$2,($8)
	lw	$3,($9)
	nop
	addu	$12,$2,$3
	sw	$12,($8)
	addi	$8,4
	addi	$9,4
	beq	$8,$10,$102
	nop
	sltu	$11,$12,$3
	beq	$11,$0,$100
	nop
$101:
	lw	$2,($8)
	lw	$3,($9)
	nop
	addu	$12,$2,$11
	addu	$12,$12,$3
	sw	$12,($8)
	addi	$8,4
	addi	$9,4
	beq	$8,$10,$102
	nop
	not	$2,$2
	sltu	$11,$3,$2
	xor	$11,1
	bne	$11,$0,$101
	nop
	j	$100
	nop
$102:
	j	$31
	nop
$104:
	xor	$11,$2,$3
	bltz	$11,$103
	nop
	xor	$11,$12,$11
	move	$2,$12
	sra	$2,31
	bgez	$11,$103
	nop
	not	$2,$2
	sw	$2,($8)
	addi	$10,4
$103:
	j	$31
	nop
	.end	madd
	.globl	uadd
	.ent	uadd
uadd:
	move	$7,$4
	addu	$8,$4,$6
	move	$2,$6
	bgez	$5,$152
	nop
$150:
	lw	$9,($7)
	nop
	addu	$10,$9,$5
	sw	$10,($7)
	addu	$7,4
	beq	$7,$8,$154
	nop
	sltu	$3,$10,$9
	bnez	$3,$155
	nop
	subu	$5,$0,1
	j	$150
	nop
$152:
	lw	$9,($7)
	nop
	addu	$10,$9,$5
	sw	$10,($7)
	addu	$7,4
	beq	$7,$8,$154
	nop
	sltu	$3,$10,$9
	beqz	$3,$155
	nop
	li	$5,1
	j	$152
	nop
$154:
	xor	$3,$9,$5
	bltz	$3,$155		##jump when no overflow
	nop
	xor	$3,$10,$9
	bgez	$3,$155		##jump when no overflow
	nop
	srl	$10,31
	subu	$10,$10,1
	sw	$10,($8)
	addu	$2,$6,4
	j	$31
	nop
$155:
	subu	$29,4
	sw	$31,($29)
	move	$10,$8
	jal	$610
	nop
	move	$2,$6
	lw	$31,($29)
	addu	$29,4
	j	$31
	nop
	.end	uadd
	.globl	usub
	.ent	usub
usub:
	move	$7,$4
	addu	$8,$4,$6
	move	$2,$6
	bgez	$5,$252
	nop
$250:
	lw	$9,($7)
	nop
	subu	$10,$9,$5
	sw	$10,($7)
	addu	$7,4
	beq	$7,$8,$254
	nop
	sltu	$3,$9,$5
	bnez	$3,$255
	nop
	subu	$5,$0,1
	j	$250
	nop
$252:
	lw	$9,($7)
	nop
	subu	$10,$9,$5
	sw	$10,($7)
	addu	$7,4
	beq	$7,$8,$254
	nop
	sltu	$3,$9,$5
	beqz	$3,$255
	nop
	li	$5,1
	j	$252
	nop
$254:
	xor	$3,$9,$5
	bgez	$3,$255		##jump when no overflow
	nop
	xor	$3,$10,$9
	bgez	$3,$255		##jump when no overflow
	nop
	srl	$10,31
	subu	$10,$10,1
	sw	$10,($8)
	addu	$2,$6,4
	j	$31
	nop
$255:
	subu	$29,4
	sw	$31,($29)
	move	$10,$8
	jal	$610
	nop
	move	$2,$6
	lw	$31,($29)
	addu	$29,4
	j	$31
	nop
	.end	usub
	.globl	msub
	.ent	msub
msub:				## $4-$5->$4 (length in $6)
	subu	$29,4		## msub(a0,a1,length)
	sw	$31,($29)	## similar to madd
	move	$8,$4
	move	$9,$5
	addu	$10,$4,$6
	jal	$200
	nop
	jal	$204
	nop
	subu	$2,$10,$4
	bne	$2,$6,$32
	nop
	jal	$610
	nop
$33:
	subu	$2,$10,$4
	lw	$31,($29)
	nop
	addu	$29,4
	j	$31
	nop
$200:
	lw	$2,($8)
	lw	$3,($9)
	nop
	subu	$12,$2,$3
	sw	$12,($8)
	addi	$8,4
	addi	$9,4
	beq	$8,$10,$202
	nop
	sltu	$11,$2,$3
	beq	$11,$0,$200
	nop
$201:
	lw	$2,($8)
	lw	$3,($9)
	nop
	subu	$12,$2,$11
	subu	$12,$12,$3
	sw	$12,($8)
	addi	$8,4
	addi	$9,4
	beq	$8,$10,$202
	nop
	sltu	$11,$3,$2
	xor	$11,1
	bne	$11,$0,$201
	nop
	j	$200
	nop
$202:
	j	$31
	nop
$204:
	xor	$11,$2,$3
	bgez	$11,$203
	nop
	xor	$11,$12,$2
	sra	$2,31
	bgez	$11,$203
	nop
	not	$2,$12
	sw	$2,($8)
	addi	$10,4
$203:
	j	$31
	nop
	.end	msub
	.globl	mmul
	.ent	mmul
mmul:				##multiplier of long * long
	subu	$29,4		##input dest,(dest+src)size,src,srcsize
	sw	$31,($29)	##product is formed on dest and upward
	addu	$8,$4,$5
	subu	$9,$8,$7
	move	$12,$9	## clear i0[i1-i3]-i0[i1]
$300:
	beq	$12,$8,$301
	nop
	sw	$0,($12)
	addiu	$12,4
	j	$300
	nop
$301:
	lw	$10,-4($9)	##sign of multiplier (terget)
	addu	$12,$6,$7
	lw	$11,-4($12)	##sign of multiplicand
	li	$19,0
	subu	$12,$5,$7	##do (i1-i3)/4 times
$302:
	beqz	$12,$308
	nop
	lw	$13,($4)	##multiplier reg
	move	$14,$4
$303:
	addiu	$14,4
	beq	$14,$9,$304
	nop
	lw	$15,($14)
	nop
	sw	$15,-4($14)
	j	$303
	nop
$304:
	li	$3,0		##carry
	li	$18,0		##interim sum
	move	$14,$9
	move	$15,$6
$305:
	beq	$14,$8,$306	##add i2[i]*(multiplier reg)
	nop
	lw	$16,($15)
	nop
	multu	$16,$13
	lw	$17,($14)
	nop
	mflo	$16
	addu	$16,$3
	sltu	$3,$16,$3
	addu	$17,$16
	sltu	$2,$17,$16
	addu	$3,$2
	addu	$17,$18
	sltu	$2,$17,$18
	addu	$3,$2
	sw	$17,-4($14)
	mfhi	$18
	addu	$14,4
	addu	$15,4
	j	$305
	nop
$306:
	addu	$18,$19
	addu	$18,$3
	sltu	$19,$18,$3
	bgez	$11,$307
	nop
	sltu	$2,$18,$13
	subu	$18,$13
	subu	$19,$2
$307:
	sw	$18,-4($8)
	addiu	$12,-4
	j	$302
	nop
$308:
	bgez	$10,$321
	nop
	move	$10,$8
	move	$8,$9
	move	$9,$6
	jal	$200
	nop
$321:
	move	$6,$5
	addu	$10,$4,$5
	jal	$610
	nop
	move	$2,$6
	lw	$31,($29)
	nop
	addu	$29,4
	j	$31
	nop
	.end	mmul
	.globl	umul
	.ent	umul
umul:				##multiplier of long * one word
	subu	$29,4		##dest, (dest+src)size, oneword src
	sw	$31,($29)
	addu	$7,$4,$5
	lw	$8,-8($7)
	addu	$9,$4,4
	li	$10,0
	li	$13,0
$350:
	beq	$9,$7,$352
	nop
	lw	$11,-4($9)
	nop
	multu	$11,$6
	mflo	$12
	addu	$12,$10
	sw	$12,-4($9)
	sltu	$12,$12,$10
	mfhi	$10
	addu	$10,$12
	subu	$10,$13
	li	$13,0
	bgez	$6,$351
	nop
	sltu	$13,$10,$11
	subu	$10,$11
$351:
	addu	$9,4
	j	$350
	nop
$352:
	bgez	$8,$353
	nop
	subu	$10,$6
$353:
	sw	$10,-4($7)
	move	$10,$7
	jal	$610
	nop
	move	$2,$6
	lw	$31,($29)
	addu	$29,4
	j	$31
	nop
	.end	umul
	.globl	mdiv
	.ent	mdiv
mdiv:
	subu	$29,4
	sw	$31,($29)
	addu	$15,$6,$7
	lw	$15,-4($15)	##sign of divisor
	nop
$400:
	addu	$10,$4,$5	##end of dividend (used by madd msub)
	lw	$14,-4($10)	##sign of dividend
	nop
	subu	$13,$10,$7	##end of quotient, to be copied to $8
	xor	$19,$14,$15
	move	$8,$13
	move	$9,$6
	bltz	$19,$404	##jump if signs are different
	nop
	jal	$200		##same sign, subtract
	nop
	xor	$11,$12,$15	##compare signs
	bltz	$11,$405	##go to main loop
	nop
$401:
	move	$8,$13
	move	$9,$6
	bltz	$11,$402
	nop
	jal	$100		##restore
	nop
	j	$403
	nop
$402:
	jal	$200		##restore
	nop
$403:
	sra	$14,31
	sw	$14,($10)
	addu	$5,4
	j	$400
	nop
$404:
	jal	$100		##different sign, add
	nop
	xor	$11,$12,$15
	bltz	$11,$401
	nop
$405:
	lw	$18,-4($13)	##top word of quotient
	move	$16,$5		##set outer counter
$406:
	beq	$16,$7,$414	##test end of outer loop
	nop
	li	$17,32		##set inner counter
$407:
	lw	$12,-4($10)
	move	$3,$18
	sll	$18,1		##shift quotient word
	move	$8,$13
$408:
	beq	$8,$10,$409	 ##shift partial remainder to left
	nop
	srl	$11,$3,31
	lw	$2,($8)
	nop
	move	$3,$2
	sll	$2,1
	or	$2,$11
	sw	$2,($8)
	addu	$8,4		
	j	$408
	nop
$409:
	move	$8,$13
	move	$9,$6
	xor	$11,$12,$15
	bltz	$11,$410
	nop
	jal	$200
	nop
	addu	$18,1
	j	$411
	nop
$410:
	jal	$100
	nop
$411:
	subu	$17,1
	bnez	$17,$407
	nop
	move	$8,$4		##circulate quotient words
$412:
	addu	$8,4
	beq	$8,$13,$413
	nop
	lw	$2,-4($8)
	nop
	sw	$18,-4($8)
	move	$18,$2
	j	$412
	nop
$413:
	subu	$16,4		##dec outer counter
	j	$406
	nop
$414:
	sw	$18,-4($13)
	move	$8,$4
	li	$3,0
$415:
	beq	$8,$13,$416	##shift quotient left 1 bit
	nop
	srl	$11,$3,31
	lw	$2,($8)
	nop
	move	$3,$2
	sll	$2,1
	or	$2,$11
	sw	$2,($8)
	addu	$8,4
	j	$415
	nop
$416:
	lw	$2,-4($10)	##compare remainder and divisor
	nop
	xor	$3,$2,$15
	bltz	$3,$417		##jump if different to correct
	nop
	lw	$2,($4)
	nop
	addu	$2,1
	sw	$2,($4)		##make q0 1
	j	$418
	nop
$417:
	move	$8,$13
	move	$9,$6
	jal	$100		##add divisor to remainder and make q0 0
	nop
$418:
	move	$7,$4
	move	$4,$13
	move	$6,$5		##normalize remainder
	jal	$610
	nop
	move	$4,$7
	sll	$7,$6,16	##prepare to add remainder length
	lw	$2,-4($13)
	nop
	xor	$2,$19		##compare signs of quotinet and algebraic sign
	bgez	$2,$421
	nop
	move	$8,$13		##shift remainder
	sra	$3,$19,31	##sign extension of true quotient
$419:
	beq	$8,$10,$420
	nop
	lw	$2,($8)
	sw	$3,($8)
	move	$3,$2
	addu	$8,4
	j	$419
	nop
$420:
	sw	$3,($8)
	subu	$6,$13,$4
	addu	$6,4
	j	$422
	nop
$421:
	move	$10,$13
	jal	$610		##normalize quotient
	nop
$422:
	addu	$2,$6,$7	##make return value (remleng<<16+quoleng)
	lw	$31,($29)
	addu	$29,4
	j	$31
	nop
	.end	mdiv
	.globl	mconv
	.ent	mconv
mconv:
	subu	$29,4
	sw	$31,($29)
	li	$14,40000
	li	$13,25000
	move	$10,$6
	li	$2,0
	addu	$15,$4,$5
$500:
	beq	$15,$4,$505
	nop
	lw	$8,-4($15)
	nop
	bnez	$8,$502
	nop
	subu	$15,4
	j	$500
	nop
$502:
	sub	$3,$15,4
	li	$11,0
	li	$12,0
$503:
	move	$9,$8
	srl	$9,16
	sll	$12,16
	or	$9,$12	##add partial remainder
	remu	$12,$9,$14
	divu	$9,$9,$14
	sll	$11,16
	or	$9,$11	##add partial remainder
	remu	$11,$9,$13
	divu	$9,$9,$13
	sll	$9,16
	and	$8,65535
	sll	$12,16
	or	$8,$12
	remu	$12,$8,$14
	divu	$8,$8,$14
	sll	$11,16
	or	$8,$11
	remu	$11,$8,$13
	divu	$8,$8,$13
	or	$8,$9
	sw	$8,($3)
	beq	$3,$4,$504
	nop
	subu	$3,4
	lw	$8,($3)
	nop
	j	$503
	nop
$504:
	mulou	$8,$11,$14
	addu	$8,$12
	sw	$8,($10)
	addu	$10,4
	addu	$2,4
	j	$500
	nop
$505:
	lw	$31,($29)
	nop
	addu	$29,4
	j	$31
	nop
	.end	mconv
	.globl	norm
	.ent	norm
norm:
$610:
	lw	$2,-4($10)
	nop
	beqz	$2,$611
	nop
	not	$2
	beqz	$2,$613
	nop
	subu	$6,$10,$4
	j	$31
	nop
$611:
	lw	$2,($4)
	nop
	ori	$3,$2,3
	xori	$3,1
	sw	$3,($4)
$612:
	lw	$3,-4($10)
	nop
	addiu	$10,-4
	beqz	$3,$612
	nop
	bgez	$3,$615
	nop
	addiu	$10,4
$615:
	addu	$10,4
	sw	$2,($4)
	subu	$6,$10,$4
	j	$31
	nop
$613:
	lw	$2,($4)
	nop
	ori	$3,$2,3
	xori	$3,1
	sw	$3,($4)
$614:
	lw	$3,-4($10)
	nop
	addiu	$10,-4
	not	$3
	beqz	$3,$614
	nop
	bgez	$3,$616
	nop
	addiu	$10,4
$616:
	addu	$10,4
	sw	$2,($4)
	subu	$6,$10,$4
	j	$31
	nop
	.end	norm
	.globl	rshift
	.ent	rshift
rshift:
	li	$7,32
	subu	$7,$6
	addu	$8,$4,$5
	li	$12,0
$700:
	beq	$8,$4,$701
	nop
	lw	$10,-4($8)
	nop
	srl	$11,$10,$6
	or	$11,$12
	sw	$11,-4($8)
	sll	$12,$10,$7
	subu	$8,4
	j	$700
	nop
$701:
	j	$31
	nop
	.end	rshift
