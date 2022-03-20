section .text

global SystemCall
SystemCall:
	;      rdi, rsi, rdx, rcx, r8, r9, [rsp+8]
	;     /    /    /    /    /   /   /
	;    /    /    /    /    /   /   /
	; rax, rdi, rsi, rdx, r10, r8, r9
	mov rax, rdi
	mov rdi, rsi
	mov rsi, rdx
	mov rdx, rcx
	mov r10, r8
	mov r8, r9
	mov r9, QWORD [rsp+8]
	syscall
	ret

global LzCnt
LzCnt:
	lzcnt rax, rdi
	ret

global TzCnt
TzCnt:
	tzcnt rax, rdi
	ret

