; Tandy64 intro
; http://pungas.space
; code: riq

bits    16
cpu     8086

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Structs and others
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
extern wait_vertical_retrace, wait_horiz_retrace

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

        mov     dx,0x3da
        mov     al,3                            ;select CRT mode control
        out     dx,al

        mov     dx,0x3de
        mov     al,0b0001_0100                  ;enable border color, enable 16 colors
        out     dx,al

        call    update_palette

        mov     cx,90                           ;wait 1.5 seconds
.l1:
        call    wait_vertical_retrace
        loop    .l1

        mov     cx,C64_SCREEN_SIZE
        mov     si,c64_screen

        mov     dx,0x0000                       ;row=0, column=0

.repeat:
        mov     ah,2                            ;set cursor position
        mov     bh,0                            ;page 0 (ignored in gfx mode though)
        int     0x10

        lodsb                                   ;char to write
        cmp     al,0                            ;anim char
        je      .do_anim_cursor
        cmp     al,`\n`
        je      .do_enter

        mov     ah,0x0a                         ;write char
        mov     bh,0                            ;page to write to
        mov     bl,9                            ;color: light blue

        push    dx
        push    cx

        mov     cx,1                            ;number of times to write to
        int     0x10

        pop     cx
        pop     dx

        inc     dl                              ;cursor.x +=1


.l0:
        loop    .repeat
        ret

.do_enter:
        inc     dh                              ;inc row
        mov     dl,0                            ;reset column
        jmp     .l0

.do_anim_cursor:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
update_palette:
        mov     dx,0x3da                        ;select border color register
        mov     al,2
        out     dx,al

        add     dx,4                            ;change border color
        mov     al,9                            ;light blue
        out     dx,al


        sub     dx,4
        mov     al,0x10                         ;select color=0
        out     dx,al                           ;select palette register

        add     dx,4
        mov     al,1                            ;color 0 is blue now (before it was black)
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
        db `\n`
        db `READY.\n`
        db 0                                            ; pause / animate cursor
C64_SCREEN_SIZE equ $ - c64_screen

