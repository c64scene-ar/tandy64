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
        mov     ax,gfx                          ;init segments
        mov     ds,ax                           ; DS=ES: same segment
        mov     es,ax                           ; SS: stack
        mov     ax,stack
        cli                                     ;disable interrupts while
        mov     ss,ax                           ; setting the stack pointer
        mov     sp,stacktop
        sti

        call    gfx_init
        call    music_init

        call    main_loop

        mov     ax,0x0002                       ;text mode 80x25
        int     0x10

        mov     ax,0x4c00
        int     0x21                            ;exit to DOS


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

gfx_init:
        mov     ax, 0x0009
        int     0x10                            ;switch video mode

        mov     si,logo                         ;ds:si (source)
        sub     di,di
        mov     ax,0xb800
        mov     es,ax                           ;es:di (dest)

        cld
        mov     cx,16384                        ;copy 32k
        rep     movsw

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
main_loop:
        call    wait_retrace
        call    music_tick

        mov     ah,1
        int     16h                             ; INT 16,AH=1, OUT:ZF=status
        jz      main_loop

        mov     ah,0
        int     16h

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_init:
        mov     ax,music
        mov     ds,ax

        mov     word [pvm_offset],pvm_song + 0x10       ;update start offset
        mov     byte [pvm_wait],0               ;don't wait at start
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_tick:

DATA    equ     0b0000_0000
DATA_EXTRA equ  0b0010_0000
DELAY   equ     0b0100_0000
DELAY_EXTRA equ 0b0110_0000
END     equ     0b1000_0000

        sub     cx,cx                           ;cx=0... needed later
        mov     si,[pvm_offset]

        cmp     byte [pvm_wait],0
        je      .l0

        dec     byte [pvm_wait]
        ret

.l0:
        lodsb                                   ;fetch command byte
        mov     ah,al
        and     al,0b1110_0000                  ;al=command only
        and     ah,0b0001_1111                  ;ah=command args only

        cmp     al,DATA                         ;data?
        je      .is_data
        cmp     al,DATA_EXTRA                   ;data extra?
        je      .is_data_extra
        cmp     al,DELAY                        ;delay?
        je      .is_delay
        cmp     al,DELAY_EXTRA                  ;delay extra?
        je      .is_delay_extra
        cmp     al,END                          ;end?
        je      .is_end

.unsupported:
        int     3
        mov     [pvm_offset],si                 ;save offset
        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.is_data:
        mov     cl,ah                           ;ch is already zero
        jmp     .repeat

.is_data_extra:
        lodsb                                   ;fetch lenght from next byte
        mov     cl,al
.repeat:
        lodsb
        out     0xc0,al
        loop    .repeat

        jmp     .l0                             ; repeat... fetch next command


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.is_delay:
        dec     ah                              ;minus one, since we are returning
        mov     [pvm_wait],ah                   ; from here now
        mov     [pvm_offset],si
        ret

.is_delay_extra:
        lodsb                                   ;fetch wait from next byte
        dec     al                              ;minus one, since we are returning
        mov     [pvm_wait],al                   ; from here now
        mov     [pvm_offset],si
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.is_end:
        mov     byte [pvm_wait], 5              ;wait 5 cycles before starting again
        mov     word [pvm_offset], pvm_song + 0x10      ; beginning of song

        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA GFX
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .gfx data

logo:
        incbin 'src/logo.raw'

charset:
        incbin 'src/c64_charset-charset.bin'

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA MUSIC
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .music data

pvm_song:
        incbin "src/uctumi-song.pvm"

pvm_wait:                                       ; cycles to read diviced 0x2df
        db 0
pvm_offset:                                     ; pointer to next byte to read
        dw 0

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; STACK
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .stack stack
        resb 4096
stacktop:
