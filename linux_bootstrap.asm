section .text

extern Start_Linux

global _start
_start:
	; pop rdi
	; mov rsi, rsp
	; xor ebp, ebp
	; and rsp, -16
	; call main
	
	; LinuxBooty = (char*, char**, uint64)
	mov r8,  [rsp]   ; uint64
	dec r8
	mov r9,  [rsp+8] ; char**
	mov r10, [r9]
	lea r11, [r9+8]
	sub rsp, 8

	mov [rsp +  0], QWORD r9
	mov [rsp +  8], QWORD r11
	mov [rsp + 16], QWORD r8
	mov rdi, rsp

	and rsp, -16 ; Must be aligned
	xor rbp, rbp ; Must be zero

	call Start_Linux
	hlt

