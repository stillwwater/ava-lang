.data
@vec3:	word	2.2, 3.3, 4.4
@fmt:	byte	"(%f, %f, %f)"
.text
	lea	edx, @vec3
	push	2		; push arguments in reverse order
	push	edx

	call	vec3_mul

	pop	edx		; restore edx
	add	esp, 4		; deallocate second parameter

        ldw	edx, 0
	mov	ecx, eax	; loop counter @vec[3]
_loop0:
	mov	eax, edx
	add	eax, ecx
	ldw	eax, 0
	push	eax             ; push argument

	add	ecx, -4		; ecx--
	cmp	ecx, 0   	; ecx > 0
	jg	_loop0

        push   @fmt
        sys     printf

	jmp	_halt
vec3_mul:
	push	ebp
	mov	ebp, esp

	add	esp, -4		; make room for local variable
	ldw	ebp, 8		; eax <- first argument (address of @vec3)
	ldw	eax, -4		; eax <- @vec3[0] (size)
	stw	ebp, -4		; store local var (sizeof @vec3)

	lea	ecx, 0		; loop counter @vec[1]
_loop:
	ldw	ebp, 8		; address of @vec3
	add	eax, ecx
	ldw	eax, 0
	mov	edx, eax	; edx <- @vec[ecx]

	ldw	ebp, 12		; second param
	cvtwf	eax
	mulf	eax, edx
	push	eax		; save result

	ldw	ebp, 8		; address of @vec3
	add	eax, ecx
	mov	edx, eax	; @vec3[eax + ecx]
	pop	eax		; get result
	stw	edx, 0		; @vec[edx] <- eax

	add	ecx, 4		; ecx++

	ldw	ebp, -4
	cmp	ecx, eax	; ecx <= sizeof @vec3
	jl	_loop

	mov	esp, ebp
	pop	ebp		; Restore caller's ebp

	pop	edx		; Pop return address
	ret	edx
_halt:
	lea	eax, 0
	halt	eax
