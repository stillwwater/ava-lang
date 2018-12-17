
.data
@msg:	byte	"Factorial of? "
@msg1:	byte	"Factorial(10) = %d"
.text
        lea     ecx, 10
        call    fact
	push 	eax
	push	@msg1
	sys	printf

	jmp	.halt

fact:
	push	edx

	; Base case
	lea	eax, 1
	cmp	ecx, 0
	je	.done

	; fact(n-1)
	mov	edx, ecx
	add	ecx, -1
	call	fact

	mul	eax, edx

.done:
	pop	edx
	pop	ebp
	ret	ebp

.halt:
	lea	edx, 0
	halt	edx
