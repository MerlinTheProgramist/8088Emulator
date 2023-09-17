[BITS 16]
cpu 8086

section .text

;SBB BX, DI
;SBB BX, [1]
;SBB [1], DI
;SBB AX, 10
;SBB BX, 10
;SBB BYTE[1], 1
LAHF
MOV SS, WORD[1]
LAHF
; LAHF
; LAHF
; LAHF

section .data
table: times 10 db 0xFF

; section .bss
; empty: times 5 db
