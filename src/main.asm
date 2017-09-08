; Demonstration of how to write an entire .EXE format program as a .OBJ
; file to be linked. Tested with the VAL free linker.
; To build:
;    nasm -fobj objexe.asm
;    val objexe.obj,objexe.exe;
; To test:
;    objexe
; (should print `hello, world')

bits    16
cpu     8086

extern ZTimerOn, ZTimerOff, ZTimerReport

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; CODE
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text
..start:
        mov     ax,data                         ;init segments
        mov     ds,ax                           ; DS=ES: same segment
        mov     es,ax                           ; SS: stack
        mov     ax,stack
        cli                                     ;disable interrupts while
        mov     ss,ax                           ; setting the stack pointer
        mov     sp,stacktop
        sti

        call    test_320_200_16

        mov     ax,0x0002                       ;text mode 80x25
        int     0x10

        mov     ax,0x4c00
        int     0x21                            ;exit to DOS


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

test_320_200_16:
        mov     ax, 0x0009
        int     0x10                            ;switch video mode

        mov     si,logo                         ;ds:si (source)
        sub     di,di
        mov     ax,0xb800
        mov     es,ax                           ;es:di (dest)

        cld
        mov     cx,16384                        ;copy 32k
        rep     movsw

        call    wait_key

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
wait_retrace:

        mov     dx,0x3da
.l0:
        in      al,dx                           ;wait for vertical retrace
        test    al,8                            ; to start
        jz      .l0

.l1:
        in      al,dx                           ;wait for vertical retrace
        test    al,8                            ; to finish
        jnz     .l1
        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
clear_video_mem:
        mov     cx,0x4000                       ;32k = 16k * 2

        mov     bx,0xb800
        mov     es,bx
        mov     di,0x0000                       ;es:di: destination
        sub     ax,ax
        rep stosw
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
wait_key:
        xor     ah,ah                           ;Function number: get key
        int     0x16                            ;Call BIOS keyboard interrupt
        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data

logo:
        incbin 'src/logo.raw'


charset:
        incbin 'src/c64_charset-charset.bin'

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; STACK
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .stack stack
        resb 4096
stacktop:
