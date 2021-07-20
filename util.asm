; rdi, rsi, rdx, rcx, r8, r9
; rax, rdi, rsi, rdx, r10, r8, r9

global SystemCall
section .text

SystemCall:
	push rcx
	push r11

	mov rax, rdi
	mov rdi, rsi
	mov rsi, rdx
	mov rdx, rcx
	mov r10, r8
	mov r8, r9
	mov r9, QWORD [rsp+24]

	syscall

	pop r11
	pop rcx

	ret

