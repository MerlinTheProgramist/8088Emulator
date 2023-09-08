cpu 8086

%define _EXIT	1	
section .text
start:			
MOV	AX,258	
MOV	CX,[tadat]
MOV	BX,muldat
MOV	AX,BX	
SUB AX,1


section .data		
tadat:	db	10	
muldat: dw  625

; section .text		
; PUSH	0	
; PUSH	_EXIT	
; SYS		
