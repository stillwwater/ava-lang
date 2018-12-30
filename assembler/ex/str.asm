.data
@str:	byte	"Hello!\n"

.text
	lea	eax, @str
	push	eax
	sys	3
	halt	edx
