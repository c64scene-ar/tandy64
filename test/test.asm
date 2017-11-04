; Tandy64
; http://pungas.space
;
bits    16
cpu     8086


extern ZTimerOn, ZTimerOff, ZTimerReport

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; CODE
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text
..start:
        mov     ax,stack
        cli                                     ;disable interrupts while
        mov     ss,ax                           ; setting the stack pointer
        mov     sp,stacktop
        sti

        cld

        mov     ax,data
        mov     ds,ax

        call    ZTimerOn
        %assign XX 0
        %rep    2048
                mov     al,[buffer+XX]          ;fetch plasma X buffer
                add     al,[buffer+XX]          ; and add it to plasma Y buffer
        %assign XX XX+1
        %endrep
        call    ZTimerOff
        call    ZTimerReport

        mov     si,buffer
        mov     di,buffer
        call    ZTimerOn
        %assign XX 0
        %rep    2048
                mov     al,[si]
                add     al,[di]                 ; and add it to plasma Y buffer
                inc     si
                inc     di
        %assign XX XX+1
        %endrep
        call    ZTimerOff
        call    ZTimerReport

        mov     si,buffer
        mov     di,buffer
        call    ZTimerOn
        %assign XX 0
        %rep    2048
                lodsb
                add     al,[di]                 ; and add it to plasma Y buffer
                inc     di
        %assign XX XX+1
        %endrep
        call    ZTimerOff
        call    ZTimerReport

        mov     ax,4c00h
        int     21h                             ;exit to DOS


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data data

buffer:
        resb    4096

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; STACK
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .stack stack
        resb 2048
stacktop:
