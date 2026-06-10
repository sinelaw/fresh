; hello.asm — NASM (Intel syntax), x86-64 Linux
%define SYS_WRITE 1
%define SYS_EXIT 60

section .data
msg db "Hello, world!", 10
len equ $ - msg

section .text
global _start

_start:
    mov rax, SYS_WRITE
    mov rdi, 1                  ; stdout
    lea rsi, [rel msg]
    mov rdx, len
    syscall

    mov rax, SYS_EXIT
    xor edi, edi
    syscall
