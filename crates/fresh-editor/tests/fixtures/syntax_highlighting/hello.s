# hello.s — GNU as (AT&T syntax), x86-64 Linux
/* write(1, msg, len); exit(0) */
    .section .rodata
msg:
    .ascii "Hello, world!\n"
    .equ len, . - msg

    .text
    .globl _start
_start:
    movq $1, %rax               # sys_write
    movq $1, %rdi
    leaq msg(%rip), %rsi
    movq $len, %rdx
    syscall

    movq $60, %rax              # sys_exit
    xorq %rdi, %rdi
    syscall
