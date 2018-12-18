.data
@str:   byte    "Hello World!"
.text
        lea     eax, 10

loop:   push    eax
        add     eax, -1
        cmp     eax, 0
        jg      loop

loop0:  pop     eax
        cmp     eax, 10
        jne     loop0
        push    255
        halt    edx
