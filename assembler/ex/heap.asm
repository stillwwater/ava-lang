.data
@str:   byte    "Hello World!"
.text
	lea     edx, @str
	ldw     edx, -4          ; len(str)

	heap	eax

	lea	ecx, 0
loop:
	mov	ebp, edx
	add	ebp, ecx

	push	eax
	push	edx
	mov	edx, eax	; edx <- heap_address
	ldb	ebp, 0		; read at @str + i

	add	edx, ecx
	stb	edx, 0		; store at heap address + i

	pop	edx		; restore base address of @str

	ldb	edx, -4		; len(str)
	cmp	ecx, eax	; ecx < length
	pop	eax		; restore heap address
	add	ecx, 1
	jl	loop

	push	eax		; heap address
	sys	print_str

	free 	eax

	lea	edx, 0
	halt	edx

