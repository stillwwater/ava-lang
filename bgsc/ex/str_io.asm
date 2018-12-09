.data
name:	space   24
msg0:	byte	"Enter your name: "
msg1:	byte	"Hello "
.text
	push    msg0
	sys	print_str

	push	name
	sys	read_str

	push    msg1
	sys	print_str

	push    name
	sys	print_str

	lea	eax, 0
	halt	eax
