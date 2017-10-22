; Tandy64 intro
; http://pungas.space
; code: riq

bits    16
cpu     8086

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Structs and others
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
extern c64_charset
extern wait_vertical_retrace, wait_horiz_retrace

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; CODE
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text

global pre_intro_start
pre_intro_start:
        mov     ax,data                         ;init segments
        mov     ds,ax                           ; DS=ES: same segment

        call    test_border

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
test_border:
        call    init_screen

        mov     dx,0x3da
        mov     al,3                            ;select CRT mode control
        out     dx,al

        mov     dx,0x3de
        mov     al,0b0001_0100                  ;enable border color, enable 16 colors
        out     dx,al

;        mov     cx,262 * 60 * 3                 ;wait 3 seconds (262 * 60 * 3)
;.repeat:
;        call    anim_border_color
;
;        loop    .repeat

        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
init_screen:
        mov     ax,0x0009                       ;320x200 16 colors
        int     0x10

        call    set_charset
        call    update_palette

        mov     cx,C64_SCREEN_SIZE
        mov     si,c64_screen
        mov     dx,0x0000                       ;row=0, column=0


global animated_print_screen
animated_print_screen:

.repeat:
        mov     ah,2                            ;set cursor position
        mov     bh,0                            ;page 0 (ignored in gfx mode though)
        int     0x10

        lodsb                                   ;char to write
        cmp     al,0                            ;anim char
        je      .do_anim_cursor
        cmp     al,1
        je      .do_delay
        cmp     al,`\n`
        je      .do_enter
        cmp     al,2
        je      .do_enable_user_input
        cmp     al,3
        je      .do_disable_user_input
        cmp     al,4
        je      .do_reverse_char_on
        cmp     al,5
        je      .do_reverse_char_off

        cmp     byte [reverse_char_enabled],1
        jne     .l2
        or      al,0x80                         ;use reverse chars charset
.l2:
        mov     ah,0x0a                         ;write char
        mov     bh,0                            ;page to write to
        mov     bl,9                            ;color: light blue

        push    dx
        push    cx

        mov     cx,1                            ;number of times to write to
        int     0x10

        mov     al,[delay_after_char]
        cmp     al,1
        jne     .l1
        call    wait_vertical_retrace

.l1:
        pop     cx
        pop     dx

        inc     dl                              ;cursor.x +=1


.l0:
        loop    .repeat

        ret

.do_anim_cursor:
        call    do_anim_cursor
        jmp     .l0

.do_delay:
        push    cx
        mov     cx,20                           ;wait for a bit
        call    do_delay
        pop     cx
        jmp     .l0

.do_enter:
        inc     dh                              ;inc row
        mov     dl,0                            ;reset column
        jmp     .l0

.do_enable_user_input:
        mov     byte [delay_after_char],1
        jmp     .l0

.do_disable_user_input:
        mov     byte [delay_after_char],0
        jmp     .l0

.do_reverse_char_on:
        mov     byte [reverse_char_enabled],1
        jmp     .l0

.do_reverse_char_off
        mov     byte [reverse_char_enabled],0
        jmp     .l0

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
do_anim_cursor:
        push    cx
        push    dx

        mov     cx,2
.repeat:
        push    cx
        mov     al,219                          ;block char
        mov     ah,0x0a                         ;write char
        mov     bh,0                            ;page to write to
        mov     bl,9                            ;color: light blue
        mov     cx,1                            ;only once
        int     0x10                            ;write char
        pop     cx

        push    cx
        mov     cx,20
        call    do_delay
        pop     cx

        push    cx
        mov     al,32                           ;empty char
        mov     ah,0x0a                         ;write char
        mov     bh,0                            ;page to write to
        mov     bl,9                            ;color: light blue
        mov     cx,1                            ;only once
        int     0x10                            ;write char
        pop     cx

        push    cx
        mov     cx,20
        call    do_delay
        pop     cx

        loop    .repeat

        pop     dx
        pop     cx

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; On entry:
;       cx:     number of vertical retraces to wait
do_delay:
        push    dx
.repeat:
        call    wait_vertical_retrace
;        call    wait_horiz_retrace
        loop    .repeat
        pop     dx

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
set_charset:
        push    ds
        mov     dx,seg c64_charset

        sub     ax,ax
        mov     ds,ax

        mov     ax,c64_charset                  ;charset 0-127 for graphics mode
        mov     [0x44 * 4 + 0],ax
        mov     [0x44 * 4 + 2],dx

        mov     ax,c64_charset + 128 * 8        ;charset 128-255 for graphics mode
        mov     [0x1f * 4 + 0],ax
        mov     [0x1f * 4 + 2],dx

        pop     ds

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
anim_border_color:
        mov     dx,0x3da

.wait_retrace_start:                            ;wait for horizontal retrace start
        in      al,dx
        test    al,1
        jz      .wait_retrace_start


        mov     al,2                            ;select border color
        out     dx,al

        add     dx,4
        mov     al,[border_color]               ;select color for border
        and     al,0x0f
        out     dx,al                           ;change border
        inc     byte [border_color]

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
video_on:
	    in      al,0x70                         ;enable NMI
	    or      al,0x80
	    out     0x70,al

	    sti                                     ;enable interrupts

	    mov     dx,0x03d8
	    mov     al,0b0000_1001                  ;no blink. intensity only
	    out     dx,al

        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
video_off:

        mov     dx,0x03d8                       ;disable video
        mov     al,1
        out     dx,al

	    cli                                     ;no interrupts
        in      al,0x70                         ;disable NMI
        and     al,0x7F
	    out     0x70,al

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data

border_color:
        db 0

delay_after_char:
        db 0

reverse_char_enabled:
        db 0                                            ;disabled by default

c64_screen:
           ;0123456789012345678901234567890123456789
        db `\n`
        db `    **** COMMODORE 64 BASIC V2 ****\n`
        db ` 64K RAM SYSTEM  38911 BASIC BYTES FREE\n`
        db `\n`
        db `READY.\n`
        db 0                                            ; pause / animate cursor
        db 2                                            ;turn on user input
        db `LOAD"TANDY 64",8,1\n\n`
        db 3                                            ;turn off user input
        db 1                                            ; pause / animate cursor
        db `SEARCHING FOR TANDY 64\n`
        db `LOADING`,1,1,1,1,1,1,1
        db `               (10 minutes later)\n`
        db 1,1,1,1
        db `READY.\n`
        db 0
        db 2                                            ;turn on user input
        db `RUN\n`
        db 3                                            ;turn off user input
C64_SCREEN_SIZE equ $ - c64_screen

