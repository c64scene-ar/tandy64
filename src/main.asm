; Tandy64 Intro
; Pungas de Villa Martelli - http://pungas.space
;
; code: riq (http://retro.moe)

bits    16
cpu     8086

extern intro_start, pre_intro_start, post_intro_start
extern detect_card
extern ZTimerOn, ZTimerOff, ZTimerReport, ZTimerGetTime

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
        jnz     not_pcjr

        call    detect_mem_128kb
        jc      not_128k

        call    detect_8088
        jb      not_8088

        call    detect_jr_a_or_b

        call    pre_intro_start
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
not_8088:
        mov     ah,9
        mov     dx,label_not_8088
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
detect_mem_128kb:
        mov     ax,cs
        cmp     ax,0x1000
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
detect_jr_a_or_b:
        cli                                     ;disable interrupts
        sub     al,al
        out     0xa0,al                         ;disable nmi

        in      al,0x21                         ;Read primary PIC Interrupt Mask Register
        mov     [old_pic_imr],al                ;Store it for later
        mov     al,0b1111_1110                  ;mask off everything except Timer interrupt
        out     0x21,al

        mov     cx,1000
        call    ZTimerOn

.l0:    loop    .l0

        call    ZTimerOff
        call    ZTimerGetTime                   ;returns in AX epased time in microseconds
                                                ; PCjr A ~= 3829
                                                ; PCjr B ~= 3907
        cmp     ax,3868                         ;Middle value between 3829 and 3907
        jb      .is_a
        mov     al,'B'
        jmp     .store_value
.is_a:
        mov     al,'A'
.store_value:
        mov     [label_model],al
        mov     al,[old_pic_imr]
        out     0x21,al                         ;restore interrupts
        mov     al,0b1000_0000
        out     0xa0,al                         ;enable nmi

        mov     ah,9
        mov     dx,label_pcjr_a_b
        int     0x21

        mov     cx,0xffff                       ;delay
.l1:
        mul     al
        loop    .l1

        sti                                     ;re-enable interrupts
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
detect_8088:
        cli                                     ;disable interrupts
        sub     al,al
        out     0xa0,al                         ;disable nmi

        in      al,0x21                         ;Read primary PIC Interrupt Mask Register
        mov     [old_pic_imr],al                ;Store it for later
        mov     al,0b1111_1110                  ;mask off everything except Timer interrupt
        out     0x21,al

        mov     cx,1000
        call    ZTimerOn

        times 100 mul ax                        ;measure `mul` performance

        call    ZTimerOff
        call    ZTimerGetTime                   ;returns in AX epased time in microseconds
                                                ; intel 8088: ~1466
        cmp     ax,1450                         ;compare with 1450... a little faster than usual

        mov     al,[old_pic_imr]
        out     0x21,al                         ;restore interrupts
        mov     al,0b1000_0000
        out     0xa0,al                         ;enable nmi

        sti                                     ;re-enable interrupts
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data

old_pic_imr:
        db 0
label_not_pcjr:
        db 'Tandy64 Jr. only works on IBM PCjr computers.',13,10,'$'

label_not_128k:
        db 'PCjr with at least 256kb RAM needed. Code should run above 128kb RAM',13,10,'$'

label_not_8088:
        db 'An 8088 CPU is needed. No NEC v20, no emulators. Thanks.',13,10,'$'

label_pcjr_a_b:
        db 'PCjr '
label_model:
        db '?'
        db ' detected.',13,10,'$'
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; STACK
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .stack stack
        resb 2048
stacktop:
