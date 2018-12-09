
.data
	test:	word 4
	arr:	word 1, 2, 3, 4
.text
        lea     ecx, 10
        call    fact
	sys	1

	jmp	.halt

fact:
	push	ebp
	push	edx

	; Base case
	lea	eax, 1
	cmpi	ecx, 0
	je	.done

	; fact(n-1)
	mov	edx, ecx
	addi	ecx, -1
	call	fact

	mul	eax, edx

.done:
	pop	edx
	pop	ebp
	ret	ebp

.halt:
	lea	edx, 0
	halt	edx
