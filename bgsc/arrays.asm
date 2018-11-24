.data
array:	word	1, 1, 2, 3, 5, 8

.text
	lea	$ac, array
	# First word in array stores the array size
	lw	$ax, $ac
	
	# Print array length
	mov	$a0, $ax
	lea	$t0, 4
	div	$a0, $t0
	sys	1

	# Counter starts at first index (4 bytes)
	lea	$cx, 4

.loop:	mov	$t0, $ac
	add	$t0, $cx	# Add counter to base address
	lw	$a0, $t0
	sys	1

	# Next word
	addi	$cx, 4
	cmp	$cx, $ax
	jl	.loop
	je	.loop

	lea	$ax, 0
	halt	$ax

print:	mov	$a0, $ax
	sys	1
	ret	$ra