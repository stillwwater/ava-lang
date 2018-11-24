
.data
	test:	word 4
	arr:	word 1, 2, 3, 4
.text
        lea     $a0, 4
        call    fact

	mov	$a0, $rv
	sys	1

	jmp	.halt

fact:	push	$ra
	push	$ax

	# Base case
	lea	$rv, 1
	cmpi	$a0, 0
	je	.done

	# fact(n-1)
	mov	$ax, $a0
	addi	$a0, -1
	call	fact

	mul	$rv, $ax

.done:	pop	$ax
	pop	$ra
	ret	$ra

.halt:	lea	$ax, 0
	halt	$ax
