%define __CS__ 0x11
%define __DS__ 0x10
%define __ES__ 0x0
%define __SS__ 0x0


; extern _main

cpu 8086
[BITS 16]

; default execution address
ORG 0xFFFF0 
; there is only 16 bytes of memory

; load default CS (this will jump)
MOV AX, __CS__
MOV CS, AX

; now we are in the desired code segment
; ORG __CS__

; load default DS
MOV AX, __DS__
MOV DS, AX

; load default ES
MOV AX, __ES__
MOV ES, AX

; load default SS
MOV AX, __SS__
MOV SS, AX

