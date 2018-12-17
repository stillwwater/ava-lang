.data
.text
	mov	ebp, esp

	lea	eax, 2
	lea	edx, 8

	push	edx		; Push last parameter first
	push	eax		; Push first parameter last

	call	pow		; Call function

	add	esp, 8		; Deallocate parameters

	jmp	_halt		; Halt

pow:
	push 	ebp		; Save ebp
	mov	ebp, esp

	add	esp, -4		; Make room for local variable

	ldw	ebp, 8		; eax <- first parameter
	stw	ebp, -4		; local variable <- eax
_loop:
	ldw	ebp, 12		; eax <- second parameter
	add	eax, -1		; eax--
	stw	ebp, 12		; p2 <- eax

	ldw	ebp, 8		; eax <- first parameter
	mov	edx, eax

	ldw	ebp, -4		; eax <- local var
	mul	eax, edx	; p1**p2
	stw	ebp, -4


	ldw	ebp, 12		; eax <- second parameter
	cmp	eax, 1
	jg	_loop

	; Epilogue
	ldw	ebp, -4		; return local var in eax
	mov	esp, ebp	; Deallocate local variables
	pop	ebp		; Restore caller's base
	ret	ebp

_halt:
	lea	eax, 0
	halt 	eax

