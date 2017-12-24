; Tandy64 Intro
; Pungas de Villa Martelli - http://pungas.space
;
; code: riq (http://retro.moe)

bits    16
cpu     8086

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Structs and others
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
extern wait_vertical_retrace, wait_horiz_retrace
extern animated_print_screen

VGA_ADDRESS     equ     0x03da                  ;Tandy == PCJr.
VGA_DATA        equ     0x03da                  ;Tandy = 0x03de. PCJr. 0x03da

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; CODE
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text

global post_intro_start
post_intro_start:

        mov     ax,data
        mov     ds,ax

        call    init_screen

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
init_screen:
        mov     ax,0x0009                       ;320x200 16 colors
        int     0x10

        mov     dx,VGA_DATA
        mov     al,3                            ;select CRT mode control
        out     dx,al

        mov     al,0b0001_0100                  ;enable border color, enable 16 colors
        out     dx,al

        call    update_palette

        mov     cx,45                           ;do delay
.l1:
        call    wait_vertical_retrace
        loop    .l1

        mov     cx,C64_SCREEN_SIZE
        mov     si,c64_screen
        mov     dx,0x0000                       ;row=0, column=0

        call    animated_print_screen
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;converts palette to emulate a sort of c64 look & feel when ending
update_palette:
        mov     dx,VGA_DATA                     ;select border color register
        mov     al,2
        out     dx,al

        mov     al,9                            ;light blue
        out     dx,al


        mov     al,0x10                         ;select color=0
        out     dx,al                           ;select palette register

        mov     al,1                            ;color 0 is blue now (before it was black)
        out     dx,al


        mov     al,0x17                         ;select color=7
        out     dx,al                           ;select palette register

        mov     al,9                            ;color light gray is light blue now
        out     dx,al

        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data

border_color:
        db 0

delay_after_char:
        db 0

c64_screen:
           ;0123456789012345678901234567890123456789
        db `\n`
        db `    **** COMMODORE 64 BASIC V2 ****\n`
        db ` 64K RAM SYSTEM  38911 BASIC BYTES FREE\n`
        db `\n\n`
        db 2                                            ; turn on user input
        db `\n`
        db '          ',4,'HTTP://PUNGAS.SPACE',5,`\n`
        db 0,0,0,0,0
C64_SCREEN_SIZE equ $ - c64_screen

