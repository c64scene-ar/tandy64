; Tandy64 Intro
; Pungas de Villa Martelli - http://pungas.space
;
; code: riq (http://retro.moe)

bits    16
cpu     8086

extern intro_start, pre_intro_start, post_intro_start
extern detect_card

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; CODE
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text
..start:
        mov     ax,data
        mov     ds,ax
        mov     ax,stack
        cli                                     ;disable interrupts while
        mov     ss,ax                           ; setting the stack pointer
        mov     sp,stacktop
        sti

        cld

        call    detect_card
        cmp     al,2
        jz      is_pcjr
        jmp     not_pcjr

is_pcjr:
        mov     ax,cs
        cmp     ax,0x1000
        jae     above_128k
        jmp     not_128k

above_128k:
        call    pre_intro_start

;        mov     ax,0x0009
;        int     0x10

        call    intro_start
        call    post_intro_start

        jmp     cleanup

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
not_pcjr:
        mov     ah,9
        mov     dx,label_not_pcjr
        int     0x21
        mov     ax,4c00h
        int     21h                             ;exit to DOS

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
not_128k:
        mov     ah,9
        mov     dx,label_not_128k
        int     0x21
        mov     ax,4c00h
        int     21h                             ;exit to DOS

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
cleanup:
        mov     ax,0x0001
        int     0x10

        mov     ax,4c00h
        int     21h                             ;exit to DOS

;        mov     ax,0xffff                       ;reboot machine
;        sub     bx,bx
;        push    ax
;        push    bx
;        retf

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data

label_not_pcjr:
        db 'Tandy64 Jr. only works on IBM PCjr computers.',13,10,'$'

label_not_128k:
        db 'PCjr with at least 256kb RAM needed. Code should run above 128kb RAM',13,10,'$'

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; STACK
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .stack stack
        resb 2048
stacktop:
