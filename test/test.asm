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

;        call    test_0
        call    test_1

        mov     ax,4c00h
        int     21h                             ;exit to DOS

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
test_0:
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

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
test_1:
        mov     ax,data
        mov     ds,ax
        mov     es,ax

        call    ZTimerOn
        mov     cx,16384
        mov     di,buffer
        mov     si,buffer+1
        rep movsb
        call    ZTimerOff
        call    ZTimerReport


        call    ZTimerOn
        mov     cx,16384/2
        mov     di,buffer
        mov     si,buffer+1
        rep movsw
        call    ZTimerOff
        call    ZTimerReport

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data data

buffer:
        resb    32368

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; STACK
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .stack stack
        resb 2048
stacktop:
