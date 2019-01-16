.data
.text
        call main
        jmp _L5
main:
        push ebp
        mov ebp, esp
        push 2 ; x -4
        push 1 ; z -8
        push 89 ; implicit -12
        push 0 ; v -16
        ldw ebp, -12
        stw ebp, -16
        lea eax, 4
        heap eax ; fixed
        push eax ; -20
        mov edx, eax
        lea eax, 121212
        stw edx, 0
        lea eax, 8
        heap eax ; a ; todo memset (a, 0)
        push eax ; -24
        ldw ebp, -24 ; a
        ldw eax, 8 ; a[2]
        stw ebp, -16 ; v = a[2]
        ldw ebp, -4 ; x
        mov edx, eax ; x
        ldw ebp, -24 ; a
        ldw eax, 4 ; a[1]
_L1:
        cmp edx, eax
        jne _L2
        push 9
        sys print_int
        jmp _L4
_L2:
        ldw ebp, -4
        mov edx, eax
        cmp edx, 0
        jge _L3
        push 10
        sys print_int
        jmp _L4
_L3:
        push 11
        sys print_int
_L4:
        mov esp, ebp
        pop ebp
        pop eax
        ret eax

_L5:
        halt eax
