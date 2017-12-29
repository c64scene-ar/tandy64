; Tandy64
; http://pungas.space

bits    16
cpu     8086

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; MACROS and CONSTANTS
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
GFX_SEG         equ     0x1800                  ;0x1800 for PCJr with 32k video ram
                                                ;0xb800 for Tandy
VGA_ADDRESS     equ     0x03da                  ;Tandy == PCJr.

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; inline vertical retrace
; IN:
;       dx      -> VGA_ADDRESS
%macro WAIT_VERTICAL_RETRACE 0
%%wait:
        in      al,dx                           ;wait for vertical retrace
        test    al,8                            ; to finish
        jnz     %%wait

%%retrace:
        in      al,dx                           ;wait for vertical retrace
        test    al,8                            ; to start
        jz      %%retrace
%endmacro

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; inline horizontal retrace
; IN:
;       dx      -> VGA_ADDRESS
%macro WAIT_HORIZONTAL_RETRACE 0
%%wait:
;FIXME PCJr
        in      al,dx                           ;wait for horizontal retrace
        ror     al,1
        jc      %%wait

%%retrace:
        in      al,dx                           ;wait for horizontal retrace
        ror     al,1
        jnc     %%retrace
%endmacro
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

        call    test_scrollbar

        mov     ax,0x0001
        int     0x10

        mov     ax,4c00h
        int     21h                             ;exit to DOS

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
test_scrollbar:
        mov     ax,0x0009
        int     0x10                            ;320x200 16 colors

        cld                                     ;forward direction

        mov     ax,data
        mov     ds,ax                           ;ds: data segment
        mov     ax,GFX_SEG
        mov     es,ax                           ;es: video segment: 0x1800

        mov     cx,0x4000                       ;16k words (32k bytes)
        sub     di,di                           ;dst: es:di
        mov     ax,0xffff                       ;color: white, white, white, white
        rep stosw

        call    irq_init

        call    main_loop

        call    irq_cleanup

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
main_loop:

.loop:
        cmp     byte [tick],0
        jz      .loop

        mov     byte [tick],0

        cli
        mov     cx,ds

        sub     ax,ax
        mov     ds,ax                           ;ds = zeor page
        mov     ax, [0x41a]                     ;keyboard buffer head
        cmp     ax, [0x41c]                     ;keyboard buffer tail

        mov     ds,cx
        sti

        jz      .loop

        ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_init:

PIT_DIVIDER equ (262*76)                        ;262 lines * 76 PIT cycles each
                                                ; make it sync with vertical retrace

        cli                                     ;disable interrupts

        mov     bp,es                           ;save es
        sub     ax,ax
        mov     es,ax                           ;es = page 0

        ;PIC
        mov     ax,new_i08
        mov     dx,cs
        xchg    ax,[es:8*4]                     ;new/old IRQ 8: offset
        xchg    dx,[es:8*4+2]                   ;new/old IRQ 8: segment
        mov     [old_i08],ax
        mov     [old_i08+2],dx

        mov     es,bp                           ;restore es

        mov     dx,VGA_ADDRESS
        WAIT_VERTICAL_RETRACE

        mov     cx,80                           ;and wait for scanlines
.repeat:
        WAIT_HORIZONTAL_RETRACE                 ;inlining, so timing in real machine
        loop    .repeat                         ; is closer to emulators

        mov     bx,PIT_DIVIDER                  ;Configure the PIT to
        call    setup_pit                       ;setup PIT

        in      al,0x21                         ;Read primary PIC Interrupt Mask Register
        mov     [old_pic_imr],al                ;Store it for later
        and     al,0b1111_1100                  ;Mask off everything except IRQ 0 (timer)
        out     0x21,al                         ;IRQ1 (keyboard)
        sti
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_cleanup:
        cli                                     ;disable interrupts

        mov     al,[old_pic_imr]                ;Get old PIC settings
        out     0x21,al                         ;Set primary PIC Interrupt Mask Register

        mov     bx,0                            ;Reset PIT to defaults (~18.2 Hz)
        call    setup_pit                       ; actually means 0x10000

        push    ds
        push    es

        xor     ax,ax
        mov     ds,ax                           ;ds = page 0

        mov     cx,data
        mov     es,cx

        les     si,[es:old_i08]
        mov     [8*4],si
        mov     [8*4+2],es                      ;Restore the old INT 08 vector (timer)

        pop     es
        pop     ds

        sti                                     ;enable interrupts
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
setup_pit:
        ; IN    bx = PIT clock period
        ;          (Divider to 1193180 Hz)
        mov     al,0b0011_0100                  ;0x34: channel 0, access mode lo/hi, rate generator, 16-bit binary
        out     0x43,al                         ;command port
        mov     ax,bx
        out     0x40,al                         ;data port for IRQ0: freq LSB
        mov     al,ah
        nop                                     ;some pause
        nop
        out     0x40,al                         ;data port for IRQ0: freq MSB

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
new_i08:
        mov     si,colors
        mov     dx,VGA_ADDRESS
        mov     bl,0x1f                         ;color to update (white)

        mov     cx,0x10

.loop:

        push    cx
        mov     cx,0x20
.loop2:
        loop    .loop2
        pop     cx


        mov     al,bl                           ;color to update
        out     dx,al                           ;dx=0x03da (register)

        lodsb                                   ;load one color value in al
        out     dx,al                           ;update color (data)

        sub     al,al                           ;set reg 0 so display works again
        out     dx,al                           ;(register)

        WAIT_HORIZONTAL_RETRACE                 ;reset to register again

        loop    .loop

        inc     byte [tick]

        mov     al,0x20
        out     0x20,al
        iret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data data

tick:
        db      0
old_i08:                                        ;segment + offset to old int 8 (timer)
        dd      0
old_pic_imr:                                    ;PIC IMR original value
        db      0

colors:
        db 0,1,2,3,4,5,6,7
        db 8,9,10,11,12,13,14,15


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; STACK
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .stack stack
        resb 2048
stacktop:
