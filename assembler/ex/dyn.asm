.data
array:	word	12,1,2,3,4,0
.text

array_add:
	pop	edx
	ldw	edx, -4	; size
	add	eax, -4
	mov	ecx, eax	;size
	ldw	edx, 0	; count
	load
	stor

	11223344 44000000 hello

	load	0
	stb
	allc
