; Tandy64 Intro
; Pungas de Villa Martelli - http://pungas.space
;
; code: riq (http://retro.moe)

bits    16
cpu     8086

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; MACROS
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
%define DEBUG 0                                 ;0=diabled, 1=enabled

TEXT_WRITER_START_Y     equ 19                  ;start at line 19

BOTTOM_OFFSET   equ     21*2*160-160            ;start at line 21:160 bytes per line, lines are every 4 -> 8/4 =2

SCROLL_OFFSET   equ     22*2*160                ;start at line 22:160 bytes per line, lines are every 4 -> 8/4 =2
SCROLL_COLS_TO_SCROLL   equ 116                 ;how many cols to scroll. max 160 (width 320, but we scroll 2 pixels at the time)
SCROLL_COLS_MARGIN      equ ((160-SCROLL_COLS_TO_SCROLL)/2)
SCROLL_RIGHT_X  equ     (160-SCROLL_COLS_MARGIN-1)      ;col in which the scroll starts from the right
SCROLL_LEFT_X   equ     (SCROLL_COLS_MARGIN)    ;col in which the scroll ends from the left

PLASMA_TEX_OFFSET       equ 21*2*160            ;plasma texture: video offset. +160 bc it starts from right to left
PLASMA_TEX_WIDTH        equ 160                 ;plasma texture: pixels wide
PLASMA_TEX_HEIGHT       equ 32                  ;plasma texture: pixels height
PLASMA_OFFSET   equ 22*2*160+0                  ;plasma: video offset
PLASMA_WIDTH    equ 20                          ;plasma: pixels wide
PLASMA_HEIGHT   equ 16                          ;plasma: pixels height

RASTER_BAR_LOOP_FOR_EACH_COLOR  equ 4           ;4 loops for each color before switching to the next one

LETTER_P_COLOR_IDX      equ 1                   ;color index for the letters
LETTER_V_COLOR_IDX      equ 2
LETTER_M_COLOR_IDX      equ 3
LETTER_BKG_COLOR_IDX    equ 4
LETTER_BORDER_COLOR_IDX equ 5


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; render vertically 4 bits needed for the scroll. grabs the firts for bytes from the cache,
; use the MSB bit. If it is on, use white, else black color
;
; IN:   ds:si   -> bit to render (pointer to cache)
;       dx      -> pointer to pixel color table
;       bp      -> row index
;       cl      -> 0b1100_0000
; Args: %1: offset line.
%macro RENDER_BIT 1

        mov     di,SCROLL_OFFSET+160*%1+SCROLL_RIGHT_X  ;es:di points to video memory
        %rep    4
                lodsb                                   ;fetches byte from the cache
                mov     ah,al                           ;save value in ah for later use
                and     al,cl                           ;cl = 0b1100_0000
                rol     al,1
                rol     al,1
                mov     bx,dx
                xlat                                    ;al = [scroll_pixel_color_tbl+ al]
                stosb

                add     di,8192-1                       ;draw in next bank. di was incremented by
                                                        ; one in stosb.

                shl     ah,1                            ;al << 2. bit 7,6 contains next bits to render
                shl     ah,1                            ;
                mov     bx,bp                           ;index by bp
                mov     [cache_charset+bx],ah           ;update cache for next iteration
                inc     bp                              ;inc row index
        %endrep
%endmacro

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; render horizontally 4 bytes (8 pixels)
; useful to render a char from a 1x1 charset
;
; IN:
;       es:di   -> where to render (ponter to video memory)
;       bx      -> offset of nibble to render
;       ah      -> must be 0
%macro RENDER_CHAR_ROW 0

        mov     al,byte [c64_charset+bx]        ;first byte to print from charset. represents 8 pixels
        mov     dl,al                           ;save al
        shr     al,1                            ;process hi nibble. shift 3 times to right
        shr     al,1                            ; instead of shifting 4 times
        shr     al,1                            ; and then one shift left (needed for table offset)
        and     al,0b0001_1110                  ;turn off bit 0 in case it is one
        mov     si,text_writer_bitmap_to_video_tbl
        add     si,ax

        movsw                                   ;render MSB nibble (4 pixels, 2 bytes)

        mov     al,dl                           ;restore al, and process LSB nibble
        and     al,0b0000_1111
        shl     al,1                            ;times 2. offset to table

        mov     si,text_writer_bitmap_to_video_tbl      ;reset si
        add     si,ax                           ;offset to bytes

        movsw                                   ;render LSB nibble (4 pixels, 2 bytes)

        inc     bx                              ;get ready for next call
%endmacro

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; render horizontally 8 bytes (16 pixels)
; useful to render a blocky double char
;
; IN:
;       es:di   -> where to render (ponter to video memory)
;       bx      -> pattern for first char
;       cx      -> pattern for 2nd char
%macro RENDER_DOUBLE_ROW 0
        mov     ax,bx
        stosw
        stosw

        mov     ax,cx
        stosw
        stosw
%endmacro

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; renders a boy-walk/dance frame
;
; IN:
;       ds:si   -> source (frame data)
;       es:di   -> destination (video memory)
;
%macro RENDER_BOY_FRAME 0
        sub     al,al                           ;clean MSB bit from previous frame
        mov     cx,8192-5                       ;increment value after each row
        mov     dx,24576-155                    ;decrement value after 4 rows

        stosb                                   ;row 0
        movsw
        movsw
        add     di,cx                           ;row 1
        stosb
        movsw
        movsw
        add     di,cx                           ;row 2
        stosb
        movsw
        movsw
        add     di,cx                           ;row 3
        stosb
        movsw
        movsw

        sub     di,dx                           ;row 4
        stosb
        movsw
        movsw
        add     di,cx                           ;row 5
        stosb
        movsw
        movsw
        add     di,cx                           ;row 6
        stosb
        movsw
        movsw
        add     di,cx                           ;row 7
        stosb
        movsw
        movsw

        sub     di,dx                           ;row 8
        stosb
        movsw
        movsw
        add     di,cx                           ;row 9
        stosb
        movsw
        movsw
        add     di,cx                           ;row 10
        stosb
        movsw
        movsw
        add     di,cx                           ;row 11
        stosb
        movsw
        movsw

        sub     di,dx                           ;row 12
        stosb
        movsw
        movsw
        add     di,cx                           ;row 13
        stosb
        movsw
        movsw
        add     di,cx                           ;row 14
        stosb
        movsw
        movsw
        add     di,cx                           ;row 15
        stosb
        movsw
        movsw
%endmacro

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; renders an empty boy-walk/dance frame
;
; IN:
;       es:di   -> destination (video memory)
;
%macro RENDER_BOY_FRAME_EMPTY 0
        sub     ax,ax                           ;clean MSB bit from previous frame
        mov     cx,8192-5                       ;increment value after each row
        mov     dx,24576-155                    ;decrement value after 4 rows

        stosb                                   ;row 0
        stosw
        stosw
        add     di,cx                           ;row 1
        stosb
        stosw
        stosw
        add     di,cx                           ;row 2
        stosb
        stosw
        stosw
        add     di,cx                           ;row 3
        stosb
        stosw
        stosw

        sub     di,dx                           ;row 4
        stosb
        stosw
        stosw
        add     di,cx                           ;row 5
        stosb
        stosw
        stosw
        add     di,cx                           ;row 6
        stosb
        stosw
        stosw
        add     di,cx                           ;row 7
        stosb
        stosw
        stosw

        sub     di,dx                           ;row 8
        stosb
        stosw
        stosw
        add     di,cx                           ;row 9
        stosb
        stosw
        stosw
        add     di,cx                           ;row 10
        stosb
        stosw
        stosw
        add     di,cx                           ;row 11
        stosb
        stosw
        stosw

        sub     di,dx                           ;row 12
        stosb
        stosw
        stosw
        add     di,cx                           ;row 13
        stosb
        stosw
        stosw
        add     di,cx                           ;row 14
        stosb
        stosw
        stosw
        add     di,cx                           ;row 15
        stosb
        stosw
        stosw
%endmacro



;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; refreshes the palette. used as a macro, and not function, since it is being
; called from time-critical sections
;
; IN:
;       ds:si   -> table with the palette to update
;       cx      -> number of colors to update times 2, since it does 2 colors per h-line
;       bl      -> starting color + 0x10. example: use 0x1f for white: 0x10 + 0xf
;       bp      -> 0x03da
;
; Arg:  0       -> don't wait for horizontal retrace
;       1       -> wait fro horizontal retrace
%macro REFRESH_PALETTE 1
        mov     bh,0xde                         ;register is faster than memory

        mov     dx,bp                           ;dx = 0x03da. select color register
%%repeat:

        mov     al,bl                           ;color to update
        out     dx,al                           ;dx=0x03da

        lodsb                                   ;load one color value in al
        mov     ah,al                           ;move it ah

%if %1
        WAIT_HORIZONTAL_RETRACE
%endif

        mov     dl,bh                           ;dx = 0x03de
        mov     al,ah
        out     dx,al                           ;update color

        inc     bl

        mov     dx,bp                           ;dx = 0x03da. restore register after chaning palette
        sub     al,al                           ; needed for original tandy
        out     dx,al

        loop    %%repeat

%endmacro

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; inline vertical retrace
; IN:
;       dx      -> 0x03da
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
;       dx      -> 0x03da
%macro WAIT_HORIZONTAL_RETRACE 0
%%wait:
        in      al,dx                           ;wait for horizontal retrace
        ror     al,1
        jc      %%wait

%%retrace:
        in      al,dx                           ;wait for horizontal retrace
        ror     al,1
        jnc     %%retrace
%endmacro


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; CODE
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text

global intro_start
intro_start:
        mov     ax,data                         ;init segments
        mov     ds,ax                           ;these values must always be true
        mov     ax,0xb800                       ; through the whole intro.
        mov     es,ax                           ; push/pop otherwise

        cld

        call    intro_init
        call    irq_init

        call    main_loop

        call    sound_cleanup
        call    irq_cleanup

        call    fake_crash
        call    sound_cleanup

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; installs a timer IRQ that triggers at the correct horizontal scan line
; for the scroll
irq_init:

PIT_DIVIDER equ (262*76)                        ;262 lines * 76 PIT cycles each
                                                ; make it sync with vertical retrace

        cli                                     ;disable interrupts

        mov     bp,es                           ;save es
        sub     ax,ax
        mov     es,ax

        ;Keyboard
        mov     ax,new_i09
        mov     dx,cs
        xchg    ax,[es:9*4]                     ;new/old IRQ 9: offset
        xchg    dx,[es:9*4+2]                   ;new/old IRQ 9: segment
        mov     [old_i09],ax
        mov     [old_i09+2],dx

        ;PIC
        mov     ax,new_i08_simple
        mov     dx,cs
        xchg    ax,[es:8*4]                     ;new/old IRQ 8: offset
        xchg    dx,[es:8*4+2]                   ;new/old IRQ 8: segment
        mov     [old_i08],ax
        mov     [old_i08+2],dx

        mov     es,bp                           ;restore es

        mov     dx,0x03da
        WAIT_VERTICAL_RETRACE

        mov     cx,194                          ;and wait for scanlines
.repeat:
        WAIT_HORIZONTAL_RETRACE                 ;inlining, so timing in real machine
        loop    .repeat                         ; is closer to emulators

        mov     bx,PIT_DIVIDER                  ;Configure the PIT to
        call    setup_pit                       ;setup PIT

        in      al,0x21                         ;Read primary PIC Interrupt Mask Register
        mov     [old_pic_imr],al                ;Store it for later
        mov     al,0b1111_1100                  ;Mask off everything except IRQ 0
        out     0x21,al                         ; and IRQ1 (timer and keyboard)

        sti
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_new_i08_multi_color_init:
        cli                                     ;disable interrupts
                                                ; while setting the interrupt
        mov     bp,es                           ;save es
        sub     ax,ax
        mov     es,ax

        mov     ax,new_i08_bottom_multi_color
        mov     dx,cs
        mov     [es:8*4],ax                     ;new/old IRQ 8: offset
        mov     [es:8*4+2],dx                   ;new/old IRQ 8: segment

        mov     es,bp                           ;restore es

        mov     dx,0x03da
        WAIT_VERTICAL_RETRACE

        mov     cx,156                          ;and wait for scanlines
.repeat:
        WAIT_HORIZONTAL_RETRACE                 ;inlining, so timing in real machine
        loop    .repeat                         ; is closer to emulators

        mov     bx,PIT_DIVIDER                  ;Configure the PIT to
        call    setup_pit                       ;setup PIT

        sti
        jmp     main_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_new_i08_full_color_init:
        cli                                     ;disable interrupts
                                                ; while setting the interrupt
        mov     bp,es                           ;save es

        sub     ax,ax
        mov     es,ax

        mov     ax,new_i08_bottom_full_color
        mov     dx,cs
        mov     [es:8*4],ax                     ;new/old IRQ 8: offset
        mov     [es:8*4+2],dx                   ;new/old IRQ 8: segment

        mov     es,bp                           ;restore es

        mov     dx,0x03da
        WAIT_VERTICAL_RETRACE

        mov     cx,168                          ;and wait for scanlines
.repeat:
        WAIT_HORIZONTAL_RETRACE                 ;inlining, so timing in real machine
        loop    .repeat                         ; is closer to emulators

        mov     bx,PIT_DIVIDER                  ;Configure the PIT to
        call    setup_pit                       ;setup PIT

        sti
        jmp     main_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_signal_letter_state_sem_init:
        mov     byte [letter_state_semaphore],1
        jmp     letter_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_cleanup:
        cli

        mov     al,[old_pic_imr]                ;Get old PIC settings
        out     0x21,al                         ;Set primary PIC Interrupt Mask Register

        mov     bx,0                            ;Reset PIT to defaults (~18.2 Hz)
        call    setup_pit                       ; actually means 0x10000

        push    es

        les     si,[old_i08]
        push    ds
        xor     ax,ax
        mov     ds,ax
        mov     [8*4],si
        mov     [8*4+2],es                      ;Restore the old INT 08 vector (timer)
        pop     ds

        les     si,[old_i09]
        push    ds
        xor     ax,ax
        mov     ds,ax
        mov     [9*4],si
        mov     [9*4+2],es                      ;Restore the old INT 09 vector (keyboard)
        pop     ds

        pop     es

        sti
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
        out     0x40,al                         ;data port for IRQ0: freq MSB

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;waits until the beam is about to return to the top-left
;should be the one to call for the effects
global wait_vertical_retrace
wait_vertical_retrace:
        mov     dx,0x03da
        WAIT_VERTICAL_RETRACE
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
global wait_horiz_retrace
wait_horiz_retrace:
        mov     dx,0x03da
        WAIT_HORIZONTAL_RETRACE
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
fake_crash:

        ;uses 5 bit (31 values) LFSR maximal.

        mov     byte [fake_crash_lfsr_state],1  ;init LFSR state
.repeat:
        call    wait_vertical_retrace           ;wait x 2 to create
        call    wait_vertical_retrace           ; a better effect

        call    music_anim                      ;4x speed for music
        call    music_anim
        call    music_anim
        call    music_anim

        call    scroll_anim                     ;4x speed for scroll
        call    scroll_anim
        call    scroll_anim
        call    scroll_anim

        call    central_screen_anim             ;text writer and/or boy walk
        call    central_screen_anim             ;text writer and/or boy walk
        call    central_screen_anim             ;text writer and/or boy walk
        call    central_screen_anim             ;text writer and/or boy walk

        call    scroll_effect_anim              ;plasma / rasterbar from scroll
        call    scroll_effect_anim              ;plasma / rasterbar from scroll
        call    scroll_effect_anim              ;plasma / rasterbar from scroll
        call    scroll_effect_anim              ;plasma / rasterbar from scroll

        mov     al,[fake_crash_lfsr_state]

        mov     ah,al
        shr     al,1
        jnc     .skip
        xor     al,0b0001_0100                  ;Taps 5 and 3 for maximal
.skip:
        mov     [fake_crash_lfsr_state],al
        mov     ah,al                           ;use bx as new crtc start address
        and     ax,0b0000_1010_1000_0000        ; but limit its range for more
        mov     [crtc_start_addr],ax            ; pleasant visual effect
        call    crtc_addr_anim                  ; and update the start address

        cmp     byte [fake_crash_lfsr_state],1  ;lfsr cycle complete?
        jne     .repeat

.end:
        ret:

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
intro_init:
        sub     al,al
        mov     byte [letter_state],al
        mov     byte [main_state],al
        call    [main_state_inits+0]            ;init main state 0
        call    [letter_state_inits+0]          ;init letter state 0

        call    music_init
        call    central_screen_init
        call    crtc_addr_init
        call    palette_colors_init
        jmp     scroll_init

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
main_loop:
.loop:
        cmp     byte [tick],0                   ;in theory, the tick is not needed
        je      .loop                           ; since i'm not doing anything, but
                                                ; in practice, if not used, the interrupt could be triggered
                                                ; in the middle of the BIOS call, some intructions are longer than others,
                                                ; and it could generate some flicker in the raster bar routine

        mov     byte [tick],0                   ;mov ,0, instead of dec. since two inc could happen together
                                                ; if running on a slow machine. not a big issue, but ctrl+alt+del won't work
                                                ; and a switch on/off will be required (arggh.)

        cmp     byte [key_pressed],0            ;faster way to check keyboard than calling int 0x16
        jz      .loop

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
new_i09:
        ;own keyboard handler to make it faster to ready keys
        mov     dx,0x60
        in      al,dx

        in      al,0x61                         ;FIXME: not sure if really needed
        or      al,0x80                         ; This code was taken from the Tandy 1000 HX
        out     0x61,al                         ; BIOS.
        and     al,0x7f
        out     0x61,al

        mov     al,0x20                         ;Send the EOI signal
        out     0x20,al                         ; to the IRQ controller

        mov     ax,data
        mov     ds,ax

        mov     byte [key_pressed],1
        iret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
new_i08_simple:
        ;not saving any variable, since the code at main loop
        ;happens after the tick

;        mov     ax,data
;        mov     ds,ax

        ;update top-screen palette
        mov     si,top_palette                  ;points to colors used at the top of the screen
        mov     cx,6                            ;update 6 colors
        mov     bl,0x10                         ; starting with color 0 (black)
        mov     bp,0x03da                       ;bp should be 0x03da
        REFRESH_PALETTE 1                       ;refresh the palette, wait for horizontal retrace

        mov     dx,bp                           ;dx=0x03da
        mov     al,2                            ;select border color register
        out     dx,al
        mov     dl,0xde                         ;dx=0x03de
        mov     al,[border_color]
        out     dx,al                           ;update border color

        jmp     new_i08_main

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
new_i08_bottom_multi_color:
        ;not saving any variable, since the code at main loop
        ;happens after the tick

;        mov     ax,data
;        mov     ds,ax

        ;update bottom-screen palette
        mov     bp,0x03da                       ;register address
        mov     si,bottom_palette+1             ;points to colors used at the bottom. skips black
        mov     cx,6                            ;only update a few colors
        mov     bl,0x11                         ; starting with color 1 (skip black)
        REFRESH_PALETTE 1                       ;refresh the palette, wait for horizontal retrace


        ;wait a few raster lines
        ;FIXME: could be used to place logic code.
        mov     dx,bp
        mov     cx,BOTTOM_TOP_LINES_TO_WAIT     ;total number of raster bars
.l0:
        lodsb                                   ;fetch color
        mov     ah,al                           ; and save it for later

        WAIT_HORIZONTAL_RETRACE

        loop    .l0                             ;and do it 17 times


        ;update top-screen palette
        mov     si,top_palette+1                ;points to colors used at the top of the screen. skips black
        mov     cx,6                            ;update a few colors
        mov     bl,0x11                         ; starting with color 1 (skips black)
        REFRESH_PALETTE 1                       ;refresh the palette, wait for horizontal retrace

        jmp     new_i08_main

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
new_i08_bottom_full_color:
        ;not saving any variable, since the code at main loop
        ;happens after the tick

;        mov     ax,data
;        mov     ds,ax

        ;update bottom-screen palette
        mov     bp,0x03da                       ;register address
        mov     si,bottom_palette+1             ;points to colors used at the bottom. skips black
        mov     cx,6                            ;only update a few colors
        mov     bl,0x11                         ; starting with color 1 (skip black)
        REFRESH_PALETTE 1                       ;refresh the palette, wait for horizontal retrace

        mov     bx,0xdade                       ;used for 3da / 3de. registers faster than immediate
        mov     dl,bh                           ;dx = 0x03da
        mov     al,0x1f                         ;select palette color 15 (white)
        out     dx,al

        mov     si,raster_colors_tbl            ;where the colors are for each raster bar

        ;BEGIN raster bar code
        ;should be done as fast as possible
        %rep    17                              ;FIXME: must be RASTER_COLORS_MAX
                lodsb                           ;fetch color
                mov     ah,al                   ; and save it for later

                WAIT_HORIZONTAL_RETRACE

                mov     dl,bl                   ;dx = 0x03de
                mov     al,ah
                out     dx,al                   ;set new color

                sub     al,al                   ;after chaning palette
                mov     dl,bh                   ; set register to 0 to avoid noise
                out     dx,al                   ; and dx=0x3da
        %endrep
        ;END raster bar code

        ;update top-screen palette
        mov     si,top_palette+1                ;points to colors used at the top of the screen. skips black
        mov     cl,6                            ;update a few colors
        mov     bl,0x11                         ; starting with color 1. skips black
        REFRESH_PALETTE 1                       ;refresh the palette. don't wait for horizontal retrace

new_i08_main:

%if DEBUG
        call    inc_d020
%endif

        ;after raster baster finishes

        ;animate "main state machine"
        sub     bh,bh
        mov     bl,byte [main_state]            ;fetch main state machine value
        shl     bx,1                            ; and convert it into offset (2 bytes per offset)
        call    [main_state_callbacks+bx]       ; and call correct state callback

        ;animate "letter state machine"
        sub     bh,bh
        mov     bl,byte [letter_state]          ;fetch pvm-letters state machine value
        shl     bx,1                            ; and convert it into offset (2 bytes per offset)
        call    [letter_state_callbacks+bx]     ; and call correct state callback

        call    crtc_addr_anim                  ;change CRTC start address
        call    music_anim                      ;play music
        call    central_screen_anim             ;text writer and/or boy walk
        call    scroll_effect_anim              ;plasma / rasterbar from scroll
        call    scroll_anim                     ;anim scroll

%if DEBUG
        call    dec_d020
%endif

        mov     al,0x20                         ;Send the EOI signal
        out     0x20,al                         ; to the IRQ controller

        inc     byte [tick]                     ;tell main_loop that it could process
                                                ; whatever he wants
        iret                                    ;exit interrupt

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
sound_cleanup:
        mov     si,volume_0                     ;volume to 0 data
        mov     cx,VOLUME_0_MAX
.repeat:
        lodsb
        out     0xc0,al                         ;set volume to 0
        loop    .repeat

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
main_state_next:
        inc     byte [main_state]
        sub     bh,bh
        mov     bl,byte [main_state]
        shl     bx,1
        jmp     [main_state_inits+bx]

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_next:
        inc     byte [letter_state]
        sub     bh,bh
        mov     bl,byte [letter_state]
        shl     bx,1
        jmp     [letter_state_inits+bx]

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
palette_colors_init:
        mov     byte [top_palette+0],1          ;black in blue
        mov     byte [bottom_palette+0],1       ;black is blue
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_fade_to_black_init:
        mov     word [palette_black_delay],15
        mov     word [palette_black_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_fade_to_black_anim:
        dec     word [palette_black_delay]
        jz      .animate
        ret

.animate:
        mov     word [palette_black_delay],4    ;reset delay
        mov     bx,word [palette_black_idx]     ;fetch idx to table
        sub     ah,ah                           ;MSB for the index. used later

        mov     cx,6                            ;first six colors: 0-5
        sub     di,di                           ;di=index to colors (reverse of cx)
.loop:
        mov     al,[palette_black_tbl+bx]       ;new color for the color
        mov     [top_palette+di],al             ;update color in color table
        inc     di
        loop    .loop                           ; and loop

        mov     [bottom_palette+0],al           ;update black for bottom part as well

        mov     [border_color],al

        inc     word [palette_black_idx]
        cmp     word [palette_black_idx],PALETTE_BLACK_MAX
        je      .next_state
        ret
.next_state:
        jmp     main_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_500ms_init:
        mov     word [main_state_delay_frames],30       ;wait 30 cycles. half second
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_2s_init:
        mov     word [main_state_delay_frames],60*2     ;wait 2 seconds before showing logo
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_3s_init:
        mov     word [main_state_delay_frames],60*3     ;wait 3 seconds before showing logo
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_5s_init:
        mov     word [main_state_delay_frames],60*5     ;wait 5 seconds before showing logo
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_6s_init:
        mov     word [main_state_delay_frames],60*6     ;wait 6 seconds before showing logo
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_10s_init:
        mov     word [main_state_delay_frames],60*10    ;wait 5 seconds before showing logo
        ret



;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_anim:
        dec     word [main_state_delay_frames]
        jz      .next
        ret
.next:
        jmp     main_state_next                 ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_gfx_fade_in_init:
        mov     word [clemente_lfsr_current_state],LFSR_START_STATE

        ; set default for colors 8-16
        mov     cx,10                           ;update colors 10 colors
        mov     bl,0x10+6                       ; starting with color 6
        mov     si,palette_default+6            ;points to colors used at the top of the screen
        mov     bp,0x03da                       ;bp should be 0x03da
        REFRESH_PALETTE 0                       ;refresh the palette, don't wait for horizontal retrace

        ;logo should be turned off by default
        mov     ax,0x0101                       ;blue/blue color
        mov     word [top_palette+0],ax         ;color idx 0,1 = blue
        mov     word [top_palette+2],ax         ;color idx 2,3 = blue
        mov     word [top_palette+4],ax         ;color idx 4,5 = blue

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Uses Linear Feedback Shift Register 15-bit
; https://en.wikipedia.org/wiki/Linear-feedback_shift_register
state_gfx_fade_in_anim:

        mov     bp,ds                           ;save ds for later

        mov     ax,[clemente_lfsr_current_state]

        mov     bx,gfx                          ;set segments
        mov     ds,bx                           ;ds: gfx segment
                                                ;es: video memory (alread points to it)
        mov     cx,150                          ;pixels to draw per frame
.loop:
        mov     di,ax
        mov     si,ax
        mov     dx,di
        and     dx,0x1800                       ;don't write pixel if in scroll space
        cmp     dx,0x1800                       ;areas: 1800-1fff,3800-3fff,5800-5fff,7800-7fff should not be written
        je      .skip_write
        movsb                                   ;update pixel
.skip_write:
        shr     ax,1
        jnc     .skip
        xor     ax,0110_0000_0000_0000b         ;Taps 15,14 for maximum lenght
.skip:
        cmp     ax,LFSR_START_STATE
        je      .end
        loop    .loop

        mov     ds,bp                           ;restore ds
        mov     [clemente_lfsr_current_state],ax
        ret

.end:
        mov     ds,bp                           ;restore ds
        jmp     main_state_next                 ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_nothing_init:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_nothing_anim:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_skip_anim:
        jmp     main_state_next                 ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_delay_200ms_init:
        mov     word [letter_state_delay_frames],60/5   ;wait 200ms
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_delay_2s_init:
        mov     word [letter_state_delay_frames],60*2   ;wait 2 seconds
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_delay_5s_init:
        mov     word [letter_state_delay_frames],60*5   ;wait 5 seconds
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_delay_10s_init:
        mov     word [letter_state_delay_frames],60*10  ;wait 10 seconds
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_delay_anim:
        cmp     word [letter_state_delay_frames],0
        je      .next
        dec     word [letter_state_delay_frames]
        ret
.next:
        jmp     letter_state_next               ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_out_p_init:
        mov     byte [letter_state_color_to_fade],LETTER_P_COLOR_IDX
        mov     byte [palette_letter_color_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_out_v_init:
        mov     byte [letter_state_color_to_fade],LETTER_V_COLOR_IDX
        mov     byte [palette_letter_color_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_out_m_init:
        mov     byte [letter_state_color_to_fade],LETTER_M_COLOR_IDX
        mov     byte [palette_letter_color_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_out_letter_anim:
        mov     bl,[palette_letter_color_idx]
        cmp     bl,PALETTE_GRAYSCALE_OUT_MAX
        je      .next

        sub     bh,bh
        mov     al,[palette_grascale_out_tbl+bx]    ;get color to be used

        mov     bl,[letter_state_color_to_fade] ;color index to fade
        mov     [top_palette+bx],al             ;update letter color

        inc     byte [palette_letter_color_idx] ;next color to be used
        ret

.next:
        jmp     letter_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_in_p_init:
        mov     byte [letter_state_color_to_fade],LETTER_P_COLOR_IDX
        mov     byte [palette_letter_color_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_in_v_init:
        mov     byte [letter_state_color_to_fade],LETTER_V_COLOR_IDX
        mov     byte [palette_letter_color_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_in_m_init:
        mov     byte [letter_state_color_to_fade],LETTER_M_COLOR_IDX
        mov     byte [palette_letter_color_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_to_pink_letter_anim:
        mov     bl,[palette_letter_color_idx]
        cmp     bl,PALETTE_BLACK_TO_PINK_MAX
        je      .next

        sub     bh,bh
        mov     al,[palette_black_to_pink_tbl+bx]      ;get color to be used

        mov     bl,[letter_state_color_to_fade] ;color index to fade
        mov     [top_palette+bx],al             ;update letter color

        inc     byte [palette_letter_color_idx] ;next color to be used

        ret
.next:
        jmp     letter_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_to_green_letter_anim:
        mov     bl,[palette_letter_color_idx]
        cmp     bl,PALETTE_BLACK_TO_GREEN_MAX
        je      .next

        sub     bh,bh
        mov     al,[palette_black_to_green_tbl+bx]        ;get color to be used

        mov     bl,[letter_state_color_to_fade] ;color index to fade
        mov     [top_palette+bx],al             ;update letter color

        inc     byte [palette_letter_color_idx] ;next color to be used

        ret
.next:
        jmp     letter_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_to_cyan_letter_anim:
        mov     bl,[palette_letter_color_idx]
        cmp     bl,PALETTE_BLACK_TO_CYAN_MAX
        je      .next

        sub     bh,bh
        mov     al,[palette_black_to_cyan_tbl+bx]        ;get color to be used

        mov     bl,[letter_state_color_to_fade] ;color index to fade
        mov     [top_palette+bx],al             ;update letter color

        inc     byte [palette_letter_color_idx] ;next color to be used

        ret
.next:
        jmp     letter_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_in_1_at_time_init:
        mov     byte [palette_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_fade_in_1_at_time_anim:
        mov     bl,byte [palette_idx]
        cmp     bl,PALETTE_PVM_LOGO_FADE_MAX
        je      .end

        sub     bh,bh
        ;letter P
        mov     al,[palette_pvm_logo_fade_tbl+bx]       ;fetch color value
        mov     [top_palette+LETTER_P_COLOR_IDX],al     ;update color index for P


        ;letter V
        sub     bx,8                            ;offset -8
        mov     al,[palette_pvm_logo_fade_tbl+bx]       ;fetch color value
        mov     [top_palette+LETTER_V_COLOR_IDX],al     ;update color index for V

        ;letter M
        sub     bx,8                            ;offset -8
        mov     al,[palette_pvm_logo_fade_tbl+bx]       ;fetch color value
        mov     [top_palette+LETTER_M_COLOR_IDX],al     ;update color index for M

        inc     byte [palette_idx]              ;update palette offset
        ret
.end:
        jmp     letter_state_next               ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_outline_fade_init:
        mov     byte [palette_letter_color_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_outline_fade_in_anim:
        mov     bl,byte [palette_letter_color_idx]
        cmp     bl,PALETTE_GRAYSCALE_IN_MAX
        je      .end

        sub     bh,bh
        mov     al,[palette_grayscale_in_tbl+bx]     ;fetch color value
        mov     [top_palette+LETTER_BORDER_COLOR_IDX],al

        inc     byte [palette_letter_color_idx]
        ret
.end:
        jmp     letter_state_next               ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_outline_fade_out_anim:
        mov     bl,byte [palette_letter_color_idx]
        cmp     bl,PALETTE_GRAYSCALE_OUT_MAX
        je      .end

        sub     bh,bh
        mov     al,[palette_grascale_out_tbl+bx]    ;fetch color value
        mov     [top_palette+LETTER_BORDER_COLOR_IDX],al

        inc     byte [palette_letter_color_idx]
        ret
.end:
        jmp     letter_state_next               ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_outline_fade_to_final_anim:
        mov     bl,byte [palette_letter_color_idx]
        cmp     bl,PALETTE_OUTLINE_FADE_TO_FINAL_MAX
        je      .end

        sub     bh,bh
        mov     al,[palette_outline_fade_to_final_tbl+bx]       ;fetch color value
        mov     [top_palette+LETTER_BORDER_COLOR_IDX],al

        inc     byte [palette_letter_color_idx]
        ret
.end:
        jmp     letter_state_next               ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_bkg_in_out_init:
        mov     byte [palette_letter_color_idx],0       ;reusing variable
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_bkg_in_out_anim:
        mov     bl,[palette_letter_color_idx]
        cmp     bl,PALETTE_BLACK_WHITE_BLACK_MAX
        je      .end

        sub     bh,bh
        mov     al,[palette_black_white_black_tbl+bx]
        mov     [top_palette+LETTER_BKG_COLOR_IDX],al

        inc     byte [palette_letter_color_idx]
        ret
.end:
        jmp     letter_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_wait_sem_init:
        mov     byte [letter_state_semaphore],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_wait_sem_anim:
        cmp     byte [letter_state_semaphore],0
        jnz     .next_state
        ret
.next_state:
        jmp     letter_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_restart_loop_init:
        mov     byte [letter_state],1
        jmp     [letter_state_inits+2]          ;init entry 1 (skip the "sem wait" state)

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_restart_loop_anim:
        ret                                     ;nothing.

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_outline_noise_5s_init:
        mov     byte [noise_fade_color_idx],0
        mov     word [letter_state_delay_frames],60*5   ;for 10 seconds
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_outline_noise_10s_init:
        mov     byte [noise_fade_color_idx],0
        mov     word [letter_state_delay_frames],60*10   ;for 10 seconds
        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
letter_state_outline_noise_anim:
        dec     word [letter_state_delay_frames]
        jz      .next_state

        cmp     byte [noise_triggered],0
        je      .skip
        mov     byte [noise_fade_color_idx],0   ;if triggered, reset anim
.skip:
        cmp     byte [noise_fade_color_idx],NOISE_FADE_COLORS_MAX       ;end of anim?
        je      .exit

        sub     bh,bh
        mov     bl,[noise_fade_color_idx]       ;bx with index to table

        mov     al,[noise_fade_colors_tbl+bx]   ;fetch color
        mov     [top_palette+LETTER_BORDER_COLOR_IDX],al

        inc     byte [noise_fade_color_idx]
.exit:
        ret
.next_state:
        jmp     letter_state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_effect_anim:
        cmp     byte [scroll_effect_enabled],0
        jz     .exit
        call    plasma_anim
        call    plasma_effect_update
        jmp     raster_bars_anim
.exit:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
raster_bars_init:
        mov     byte [raster_colors_sine_idx],0
        mov     byte [raster_colors_loops_for_each_color],RASTER_BAR_LOOP_FOR_EACH_COLOR
        mov     word [raster_bars_colors_addr],raster_bars_colors_addr_start+RASTER_BARS_COLOR_MAX      ;point to next color
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
raster_bars_anim:
        mov     bp,es                           ;save for later
        sub     bh,bh

        mov     ax,ds
        mov     es,ax                           ;ds:di -> dst
        mov     di,raster_colors_tbl

        ; table_offset = sine_tbl[sine_idx]
        mov     bl,[raster_colors_sine_idx]     ;pointer to sine table
        mov     bl,[raster_colors_sine_tbl+bx]  ;sine index for source colors
        mov     si,raster_colors_anim_tbl
        add     si,bx

        mov     cx,(RASTER_COLORS_MAX-1)/2      ;don't overwrite last color
        ;assert (cx == 8)
        rep movsw

        mov     es,bp                           ;restore es

        inc     byte [raster_colors_sine_idx]   ;0?
        jnz     .exit
        dec     byte [raster_colors_loops_for_each_color]
        jz      .change_color
.exit:
        ret                                     ;exit

.change_color:
        mov     byte [raster_colors_loops_for_each_color],RASTER_BAR_LOOP_FOR_EACH_COLOR

        ;change raster bar colors
        mov     bx,es
        mov     ax,ds
        mov     es,ax                           ;ds=es
        mov     si,[raster_bars_colors_addr]    ;ds:si src
        mov     di,raster_colors_color_tbl      ;es:di: dst
        mov     cx,RASTER_BARS_COLOR_MAX        ;colors to copy
        rep movsb
        mov     es,bx                           ;restore es

        cmp     si,raster_bars_colors_addr_end
        jne     .update_addr
        mov     si,raster_bars_colors_addr_start

.update_addr:
        mov     [raster_bars_colors_addr],si    ;update position of next color bar
        mov     byte [plasma_effect_trigger],1  ;start effect transition
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_init:
        mov     byte [scroll_enabled],0         ;disabled by default
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_anim:
        cmp     byte [scroll_enabled],0
        jne     .anim
        ret

.anim:
        mov     bp,ds                           ;save ds for later
        mov     ax,es                           ;ds and es point to video memory
        mov     ds,ax

        mov     dx,SCROLL_COLS_TO_SCROLL/2      ;div 2 since we use movsw instead of movsb

        ;scroll 16 rows in total
        %assign XX 0
        %rep 4
                %assign YY 0
                %rep 4
                        mov     cx,dx                           ;scroll 4 lines of 80 chars
                        mov     si,SCROLL_OFFSET+8192*XX+160*YY+SCROLL_LEFT_X+1  ;source: last char of screen
                        mov     di,SCROLL_OFFSET+8192*XX+160*YY+SCROLL_LEFT_X    ;dest: last char of screen - 1
                        rep movsw                               ;do the copy
                %assign YY YY+1
                %endrep
        %assign XX XX+1
        %endrep

        mov     ds,bp                           ;restore ds

        ;HACK: scroll_bit_idx & scroll_col_used are contiguos in memory
        ;using "word" to compare them
        cmp     word [scroll_bit_idx],0         ;if scroll_bit_idx == 0 and scroll_col_used == 0
        jnz     .render_bits                    ; and scroll_col_used == 0

.read_and_process_char:
        ;update the cache with the next 32 bytes (2x2 chars)
        mov     bx,[scroll_char_idx]            ;scroll text offset
        mov     bl,byte [scroll_text+bx]        ;char to print
        test    bl,0b1000_0000                  ;control code?
        jnz     .control_code
        and     bl,0b0011_1111                  ;only use lower 63 numbers
        sub     bh,bh
        shl     bx,1                            ;bx * 8 since each char takes 8
        shl     bx,1                            ; bytes in the charset
        shl     bx,1
        lea     si,[charset+bx]                 ;ds:si: charset

        mov     bp,es                           ;save es for later
        mov     ax,ds
        mov     es,ax                           ;es = ds
        mov     di,cache_charset                ;es:di: cache

        mov     cx,4                            ;copy first char (4 words == 8 bytes)
        rep movsw

        mov     cl,4
        add     si,(128-1)*8                    ;point to next char. offset=128
        rep movsw

        mov     cl,4
        sub     si,(64+1)*8                     ;point to next char. offset=64
        rep movsw

        mov     cl,4
        add     si,(128-1)*8                    ;point to next char. offset=192
        rep movsw

        mov     es,bp                           ;restore es

        ;fall-through

.render_bits:
        mov     si,cache_charset                ;ds:si points to cache_charset
        sub     bp,bp                           ;used for the cache index in the macros
        mov     dx,scroll_pixel_color_tbl       ;table for colors used in the macros
        mov     cl,0b1100_0000                  ;mask used in macros

        RENDER_BIT 0
        RENDER_BIT 1
        RENDER_BIT 2
        RENDER_BIT 3

        add     byte [scroll_bit_idx],2         ;two incs, since it prints 2 bits at the time

        test    byte [scroll_bit_idx],8         ;should use 2nd chars?
        jz      .end                            ;if not, exit

        test    byte [scroll_col_used],1        ;reached bit 8... already using
        jnz     .next_char                      ; col 2? If so, next char

        ;update cache with remaing 16-bytes (the 2nd col)
        mov     bp,es                           ;save es
        mov     ax,ds                           ;copy 2nd-col chars to cache
        mov     es,ax                           ;es = data
        mov     si,cache_charset+16             ;pointer to 2nd-col chars
        mov     di,cache_charset                ;pointer to 1st-col chars
        mov     cx,8
        rep movsw                               ;copy 16 bytes
        mov     es,bp                           ;restore es
        inc     byte [scroll_col_used]          ;2nd column to be used
        mov     byte [scroll_bit_idx],cl        ;0
        ret

.control_code:
        inc     word [scroll_char_idx]          ;consume control code. offset += 1
        and     bl,0b0111_1111                  ;turn off bit 7
        shl     bl,1                            ;al * 2, since each address takes 2 bytes
        sub     bh,bh
        call    [scroll_control_code_tbl+bx]    ;FIXME: embarrasing. instead of call + ret + jmp
        jmp     .read_and_process_char          ; two jmp's should be enough

.next_char:
        sub     ax,ax
        mov     byte [scroll_col_used],al       ;reset to 0
        mov     byte [scroll_bit_idx],al        ;reset bit idx
        inc     word [scroll_char_idx]          ;scroll_char_idx++
        cmp     word [scroll_char_idx],SCROLL_TEXT_LEN  ;end of scroll?
        jnz     .end                            ; if so, reset index
        mov     word [scroll_char_idx],ax       ;reset to 0

.end:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_init:
        mov     al,0x6c                         ;PCJr only:
        out     0x61,al                         ; use 3-voice instead of speacker

        mov     word [pvm_offset],pvm_song + 0x10       ;update start offset
        sub     al,al
        mov     byte [pvm_wait],al              ;don't wait at start
        mov     byte [noise_triggered],al       ;noise not playing
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_anim:

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
        int 3
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

        mov     byte [noise_triggered],0
        and     al,0b1111_0000                  ;is noise?
        cmp     al,0b1110_0000
        jne     .not_noise
        inc     byte [noise_triggered]          ;notify noise is playing

.not_noise:
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
        mov     byte [pvm_wait],5               ;wait 5 cycles before starting again
        mov     word [pvm_offset],pvm_song+0x10 ; beginning of song

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
central_screen_init:
        mov     byte [central_screen_state],CENTRAL_SCREEN_STATE_WAIT
        call    boy_walk_init                   ;init its two other states
        jmp     text_writer_init

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
central_screen_anim:
        mov     al,[central_screen_state]
        or      al,al                           ;0 == exit
        jz      .exit
        shr     al,1                            ;1 == do boy_walk
        jc      .do_boy_walk
        jmp     text_writer_anim                ;>2 == do text_writer
.do_boy_walk:
        jmp     boy_do_anim
.exit:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
boy_walk_init:
        mov     word [boy_anim_vid_addr],TEXT_WRITER_START_Y*2*160      ;hardcode writing position
        sub     al,al
        mov     byte [boy_anim_delay],50        ;some delay to get sync with letter noise anim
        mov     byte [boy_walk_frame_idx],al
        mov     byte [boy_walk_col_pos],al
        mov     byte [boy_anim_state],al
        mov     word [boy_dance_timeout],10*60  ;dance for 10 seconds
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
boy_do_anim:
        mov     al,[boy_anim_state]
        or      al,al
        jz      boy_walk_anim
        shr     al,1
        jc      boy_dance_anim

        ;fall through
        mov     di,[boy_anim_vid_addr]          ;clear boy walk frame
        RENDER_BOY_FRAME_EMPTY

        mov     byte [central_screen_state],CENTRAL_SCREEN_STATE_TEXT_WRITTER   ;enable text writer
        jmp     text_writer_init                ;init text writer

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
boy_walk_anim:
        dec     byte [boy_anim_delay]
        jz      .anim
        ret

.anim:
        mov     byte [boy_anim_delay],5         ;reset delay
        sub     bh,bh
        mov     bl,[boy_walk_frame_idx]
        shl     bl,1                            ;each address takes 2 bytes
        mov     si,[boy_walk_animation_tbl+bx]  ;src: ds:si (frame data)
        mov     di,[boy_anim_vid_addr]          ;dst: es:di (video memory)

        RENDER_BOY_FRAME

        inc     word [boy_anim_vid_addr]        ;move sprite 2 pixels to the right
        inc     byte [boy_walk_col_pos]         ;next column

        cmp     byte [boy_walk_col_pos],70      ;half screen
        je      .enable_dance
        cmp     byte [boy_walk_col_pos],154     ;end of screen
        je      .enable_end_anim

        inc     byte [boy_walk_frame_idx]       ;points to next frame
        cmp     byte [boy_walk_frame_idx],BOY_WALK_ANIM_FRAME_MAX
        jne     .exit
        mov     byte [boy_walk_frame_idx],0

.exit:
        ret
.enable_dance:
        mov     byte [boy_anim_state],BOY_ANIM_STATE_DANCE
        ret
.enable_end_anim:
        mov     byte [boy_anim_state],BOY_ANIM_STATE_END
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
boy_dance_anim:
        dec     word [boy_dance_timeout]        ;only dance for 10 senconds
        jz      .end_dance

        mov     di,[boy_anim_vid_addr]

        cmp     byte [noise_triggered],0
        jne     .foot_up

.foot_down:
        dec     byte [boy_anim_delay]           ;if 0, draw foot down
        jnz     .exit                           ; after drawing foot down, delay will be 255
                                                ; exactly what we wanted... to just fake a delay
                                                ; so that we don't draw the foot down during all the frames

        mov     si,boy_anim_frame_1             ;foot_down
        jmp     .render

.foot_up:
        mov     si,boy_anim_frame_0             ;foot up
        mov     byte [boy_anim_delay],3         ;three frames with foot up

        ;fall-through

.render:
        RENDER_BOY_FRAME

.exit:
        ret

.end_dance:
        mov     byte [boy_anim_delay],1         ;reset delay to min
        mov     byte [boy_anim_state],BOY_ANIM_STATE_WALK       ;walk again...to right margin
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_init:
        sub     ax,ax
        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR
        mov     word [text_writer_idx],-1       ;HACK: data offset is -1, because we do a +1 at anim
        mov     byte [text_writer_delay],10     ;waits 10 refreshes
        mov     byte [text_writer_cursor_blink_delay],al ;how many blinks to wait
        mov     byte [text_writer_x_pos],al     ;at pos 0
        mov     byte [text_writer_x_dst],al     ;dst pos 0
        mov     byte [text_writer_y_pos],TEXT_WRITER_START_Y
        mov     byte [text_writer_y_dst],TEXT_WRITER_START_Y
        mov     word [text_writer_addr],TEXT_WRITER_START_Y*2*160+160   ;half line ahead
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_anim:
        sub     bh,bh                           ;fetch state
        mov     bl,[text_writer_state]          ; and get state address from
        shl     bl,1                            ; table, and call it
        jmp     [text_writer_callbacks_anim+bx] ; and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_print_char_init:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_print_char_anim:
        mov     bx,word [text_writer_idx]       ;data offset
        inc     bx                              ;data offset += 1
        cmp     bx,TEXT_WRITER_DATA_LEN         ;end of line?
        jne     .l0

        mov     byte [central_screen_state],CENTRAL_SCREEN_STATE_BOY_ANIM       ;switch to walk state
        jmp     boy_walk_init                   ; and init it

.l0:
        mov     word [text_writer_idx],bx       ;save offset

        mov     al,byte [text_writer_data+bx]   ;get char to print or state
        cmp     al,TW_STATE_MAX                 ;is it a char, or a new state?
        jb      .new_state

        call    text_writer_print_char          ;print the char
        inc     byte [text_writer_x_pos]        ;cursor pos += 1
        add     word [text_writer_addr],4       ;video address +4 (each char takes 4 bytes)
        mov     al,0x77                         ;select "reverse" space (all gray char)
        call    text_writer_fill_one_char       ; and print it

        mov     byte [text_writer_delay],2      ;cycles to wait
        mov     byte [text_writer_state],TW_STATE_IDLE  ;after writing a char, switch to
                                                ; delay state to make the writing slower
        ret

.new_state:
        mov     byte [text_writer_state],al     ;store new state

        sub     bh,bh                           ;move it to bx so we can use it
        mov     bl,al                           ; as index
        shl     bl,1                            ;bx *= 2 since each addr takes 2 bytes
        jmp     [text_writer_callbacks_init+bx] ;call init callback and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_idle_init:
        inc     word [text_writer_idx]          ;offset += 1
        mov     bx,[text_writer_idx]            ;get current data offset
        mov     al,[text_writer_data+bx]        ;fetch value value using offset
        mov     [text_writer_delay],al          ;move it to delay
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_idle_anim:
        dec     byte [text_writer_delay]
        jz      .end_delay
        ret

.end_delay:
        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR    ;print char is next state
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_goto_x_init:
        inc     word [text_writer_idx]          ;data offset += 1
        mov     bx,word [text_writer_idx]       ;get data offset
        mov     al,byte [text_writer_data+bx]   ;pos to go back to
        mov     byte [text_writer_x_dst],al     ;update destination

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_goto_x_anim:
        mov     al,byte [text_writer_x_dst]     ;fetch destination pos
        cmp     byte [text_writer_x_pos],al     ; and compare with current pos
        jb      .right
        ja      .left

        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR    ;if equal, means finished
        ret                                     ; then change state to read chars again

.right:
        mov     ax,0x0077                       ;ah=black/black, al=gray/gray
        call    text_writer_fill_two_chars
        inc     byte [text_writer_x_pos]        ;cursor += 1. destintation is to the right
        add     word [text_writer_addr],4       ;video addr += 4 (each char takes 4 bytes)
        ret

.left:
        dec     byte [text_writer_x_pos]        ;cursor -= 1. destintation is to the left
        sub     word [text_writer_addr],4       ;video addr -= 4 (each char takes 4 bytes)
        mov     ax,0x7700                       ;ah=gray/gray, black/black
        jmp     text_writer_fill_two_chars

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_goto_y_init:
        inc     word [text_writer_idx]          ;data offset += 1
        mov     bx,word [text_writer_idx]       ;get data offset
        mov     al,byte [text_writer_data+bx]   ;pos to go back to
        mov     byte [text_writer_y_dst],al     ;update destination

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_goto_y_anim:
        mov     al,byte [text_writer_y_dst]     ;fetch destination pos
        cmp     byte [text_writer_y_pos],al     ; and compare with current pos
        jb      .down
        ja      .up

        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR    ;if equal, means finished
        ret                                     ; then change state to read chars again

.down:
        sub     al,al                           ;char to write: empty
        call    text_writer_fill_one_char
        inc     byte [text_writer_y_pos]        ;cursor.y += 1 (going down)
        add     word [text_writer_addr],320     ;video addr += 320
        mov     al,0x77                         ;char to write: full
        jmp     text_writer_fill_one_char

.up:
        sub     al,al                           ;char to write: empty
        call    text_writer_fill_one_char
        dec     byte [text_writer_y_pos]        ;cursor.y -= 1. (going up)
        sub     word [text_writer_addr],320     ;video addr -= 320
        mov     al,0x77                         ;char to write: full
        jmp     text_writer_fill_one_char


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_call_action_init:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; fetch the next byte from the stream, and use it as an
; index to the action_call table, and call that action function
text_writer_state_call_action_anim:
        inc     word [text_writer_idx]          ;offset + 1
        mov     bx,word [text_writer_idx]       ;fetch next char
        mov     al,[text_writer_data+bx]        ; which is the action to perform

        ;al == 0 -> turn off cursor
.do_cursor_off:
        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR    ;print char is next state
        sub     al,al                           ;char to write: empty
        jmp     text_writer_fill_one_char

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_cursor_blink_init:
        inc     word [text_writer_idx]          ;offset += 1
        mov     bx,[text_writer_idx]            ;get current data offset
        mov     al,[text_writer_data+bx]        ;fetch value value using offset
        mov     [text_writer_cursor_blink_delay],al ;move it to blinks-to-wait
        mov     byte [text_writer_delay],30     ;delay blink lasts 30 cycles
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_cursor_blink_anim:
        dec     byte [text_writer_delay]
        jz      .next_blink
        cmp     byte [text_writer_delay],15     ;half cycles
        je      .cursor_off
        ret

.next_blink:
        dec     byte [text_writer_cursor_blink_delay]
        jz      .end_blink_state

        mov     byte [text_writer_delay],30     ;init blink delay
        mov     al,0x77                         ;select all gray char
        jmp     text_writer_fill_one_char       ; print it, and return

.cursor_off:
        sub     al,al                           ;select all black char
        jmp     text_writer_fill_one_char       ; print it, and return

.end_blink_state:
        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR    ;print char is next state
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
crtc_addr_init:
        mov     word [crtc_start_addr],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
crtc_addr_anim:
        mov     bx,[crtc_start_addr]

        mov     dx,0x03d4
        mov     al,0x0c                         ;select CRTC start address hi
        out     dx,al

        inc     dx                              ;set value for CRTC hi address
        mov     al,bh
        out     dx,al

        dec     dx
        mov     al,0x0d
        out     dx,al                           ;select CRTC start address lo

        inc     dx
        mov     al,bl
        out     dx,al                           ;set value for CRTC lo address

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_enable_boy_walk:
        mov     byte [central_screen_state],CENTRAL_SCREEN_STATE_BOY_ANIM
        jmp     boy_walk_init

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_enable_text_writer:
        mov     byte [central_screen_state],CENTRAL_SCREEN_STATE_TEXT_WRITTER
        jmp     text_writer_init

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_enable_scroll:
        mov     byte [scroll_enabled],1         ;start the scroll
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_enable_scroll_effects:
        inc     byte [scroll_effect_enabled]    ;enable effects
        call    raster_bars_init
        jmp     plasma_init

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_clear_bottom_init:
        mov     byte [clear_bottom_state],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_clear_bottom_anim:
        ;clear the bottom part with black pixels
        ;previously it was filled with the plasma pixels
        ;do it in two parts since there is not enough CPU power to do it in
        ;just one pass
        sub     ax,ax                           ;black * 4

        cmp     byte [clear_bottom_state],0
        jne     .second_half

.first_half:
        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*0         ;destination
        rep stosw                               ;do the 'clean screen'

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*1         ;destination
        rep stosw                               ;do the 'clean screen'

        inc     byte [clear_bottom_state]       ;ready for next half
        ret

.second_half:
        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*2         ;destination
        rep stosw                               ;do the 'clean screen'

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*3         ;destination
        rep stosw                               ;do the 'clean screen'

        jmp     main_state_next                 ;finish and set next state


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_tex_init_sine_table:

        mov     ax,ds
        mov     es,ax                           ;es = ds
        ; x
        mov     dh,[plasma_tex_inc_x0]
        mov     dl,[plasma_tex_inc_x1]

        ;HACK: works because fn_table_1 and
        ;fn_table_2 are 256-aligned
        mov     bx,fn_table_1
        mov     si,fn_table_2
        mov     cx,si
        mov     di,plasma_tex_xbuf              ;es:di -> dst

        %assign XX 0
        %rep    PLASMA_TEX_WIDTH
                mov     al,[bx]                 ;xbuf[idx] = sine[bx]+sine[si]
                add     al,[si]
                stosb
                add     bl,dh                   ;dh=5. update offsets to sine tables
                add     cl,dl                   ;cx=8
                mov     si,cx
        %assign XX XX+1
        %endrep

        ; y
        mov     dh,[plasma_tex_inc_y0]
        mov     dl,[plasma_tex_inc_y1]

        ;HACK: works because fn_table_1 and
        ;fn_table_2 are 256-aligned
        mov     bx,fn_table_1
        mov     si,fn_table_2
        mov     cx,si
        mov     di,plasma_tex_ybuf              ;es:di -> dst

        %assign YY 0
        %rep    PLASMA_TEX_HEIGHT
                mov     al,[bx]                 ;ybuf[idx] = sine[bx]+sine[si]
                add     al,[si]
                stosb
                add     bl,dh                   ;dh=3
                add     cl,dl                   ;cx=5
                mov     si,cx
        %assign YY YY+1
        %endrep

        mov     dx,0xb800
        mov     es,dx                           ;restore es

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_plasma_red_tex_init:
        sub     ax,ax                           ;faster to use register than value
        mov     word [plasma_tex_x_offset],ax
        mov     byte [plasma_tex_state],al      ;initial state
        mov     byte [plasma_tex_colors_updated],al
        mov     byte [plasma_tex_delay],1
        mov     word [plasma_tex_palette_addr],plasma_tex_red_palette
        mov     byte [plasma_tex_letter_color],LETTER_P_COLOR_IDX       ;letter P color idx
        mov     byte [plasma_tex_inc_x0],250     ;texture generator parameters
        mov     byte [plasma_tex_inc_x1],255
        mov     byte [plasma_tex_inc_y0],127
        mov     byte [plasma_tex_inc_y1],129

        mov     bx,es                           ;save es in bx for later

        mov     ax,ds
        mov     es,ax                           ;es is data segment
        mov     cx,8                            ;repeat it 8 times
        sub     ax,ax                           ;ax=0
        mov     di,bottom_palette               ;es:di -> bottom palette
        rep stosw                               ;bottom palette is all black

        mov     es,bx                           ;restore es. es=0xb800

        jmp     plasma_tex_init_sine_table

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_plasma_green_tex_init:
        sub     ax,ax                           ;faster to use register than value
        mov     word [plasma_tex_x_offset],ax
        mov     byte [plasma_tex_state],al      ;initial state
        mov     byte [plasma_tex_colors_updated],al
        mov     byte [plasma_tex_delay],1
        mov     word [plasma_tex_palette_addr],plasma_tex_green_palette
        mov     byte [plasma_tex_letter_color],LETTER_V_COLOR_IDX       ;letter V color idx
        mov     byte [plasma_tex_inc_x0],4      ;texture generator parameters
        mov     byte [plasma_tex_inc_x1],7
        mov     byte [plasma_tex_inc_y0],129
        mov     byte [plasma_tex_inc_y1],3

        mov     bx,es                           ;save es in bx for later

        mov     ax,ds
        mov     es,ax                           ;es is data segment
        mov     cx,8                            ;repeat it 8 times
        sub     ax,ax                           ;ax=0
        mov     di,bottom_palette               ;es:di -> bottom palette
        rep stosw                               ;bottom palette is all black

        mov     es,bx                           ;restore es. es=0xb800

        jmp     plasma_tex_init_sine_table

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_plasma_magenta_tex_init:
        sub     ax,ax                           ;faster to use register than value
        mov     word [plasma_tex_x_offset],ax
        mov     byte [plasma_tex_state],al      ;initial state
        mov     byte [plasma_tex_colors_updated],al
        mov     byte [plasma_tex_delay],1
        mov     word [plasma_tex_palette_addr],plasma_tex_magenta_palette
        mov     byte [plasma_tex_letter_color],LETTER_M_COLOR_IDX       ;letter M color idx
        mov     byte [plasma_tex_inc_x0],4      ;texture generator parameters
        mov     byte [plasma_tex_inc_x1],3
        mov     byte [plasma_tex_inc_y0],127
        mov     byte [plasma_tex_inc_y1],132

        mov     bx,es                           ;save es in bx for later

        mov     ax,ds
        mov     es,ax                           ;es is data segment
        mov     cx,8                            ;repeat it 8 times
        sub     ax,ax                           ;ax=0
        mov     di,bottom_palette               ;es:di -> bottom palette
        rep stosw                               ;bottom palette is all black

        mov     es,bx                           ;restore es. es=0xb800

        jmp     plasma_tex_init_sine_table

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_plasma_tex_anim:
        ;has 3 states:
        ;0 = render texture
        ;1 = texture fade in
        ;2 = texture fade out
        cmp     byte [plasma_tex_state],0
        je      .render_texture
        cmp     byte [plasma_tex_state],1
        je      .palette_fade_in

.palette_fade_out:
        ;shift right the palette colors one by one
        ;until all all black
        cmp     byte [plasma_tex_colors_updated],0
        je      .next_state

        dec     byte [plasma_tex_delay]
        jz      .do_out
        ret

.do_out:
        mov     byte [plasma_tex_delay],4       ;reset delay

        mov     bx,es                           ;save it for later
        mov     ax,ds
        mov     es,ax

        sub     ch,ch
        mov     cl,[plasma_tex_colors_updated]  ;numbers of colors to copy
        dec     cl                              ; minus one, since previous state left it +1

        std                                     ;copy backwards
        mov     si,bottom_palette+1
        mov     si,bottom_palette+1+PLASMA_TEX_PALETTE_MAX-2    ;last element -1
        mov     di,bottom_palette+1+PLASMA_TEX_PALETTE_MAX-1    ;last element
        rep movsb
        mov     byte [di],0                     ;fill the first element with black, since
                                                ; we are fading to black
        cld                                     ;restore direction flag

        dec     byte [plasma_tex_colors_updated];number of colors updated -= 1
        mov     es,bx                           ;restore es

        mov     al,[bottom_palette+1+PLASMA_TEX_PALETTE_MAX-1]  ;color value
        sub     bh,bh
        mov     bl,[plasma_tex_letter_color]    ;color to update
        mov     [top_palette+bx],al             ;update color with value
        ret

.next_state:
        jmp     main_state_next


.palette_fade_in:
        ;shift left the colors one by one
        ;until all of them are in the final position
        cmp     byte [plasma_tex_colors_updated],PLASMA_TEX_PALETTE_MAX
        je      .next_internal_state

        dec     byte [plasma_tex_delay]         ;end of wait?
        jz      .do_in                          ; if so, jump to the effect
        ret

.do_in:
        mov     byte [plasma_tex_delay],4       ;reset delay

        mov     bx,es                           ;save it for later
        mov     ax,ds
        mov     es,ax

        sub     ch,ch
        mov     cl,[plasma_tex_colors_updated]  ;number colors to copy
        inc     cl                              ;plus 1 (when 0, copy 1 color)

        mov     di,bottom_palette+1             ;skip color black
        mov     si,[plasma_tex_palette_addr]    ;fetch plasma palette to use
        add     si,PLASMA_TEX_PALETTE_MAX       ; and make it point to the end
        sub     si,cx                           ; minus the colors already copied
        rep movsb

        inc     byte [plasma_tex_colors_updated];number of colors updated +=1
        mov     es,bx

        mov     al,[bottom_palette+1]           ;color value
        sub     bh,bh                           ;MSB bx = 0
        mov     bl,[plasma_tex_letter_color]    ;color to update
        mov     [top_palette+bx],al             ;update color with value
        ret

.next_internal_state:
        mov     byte [plasma_tex_delay],4       ;delay before starting next state
        inc     byte [plasma_tex_state]         ;set next state
        ret

.render_texture:
        mov     cx,5
.anim:
        push    cx
        call    plasma_tex_render_to_video
        pop     cx
        loop    .anim
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_tex_render_to_video:

        mov     bx,luminances_6_colors          ;to be used by xlat. has the colors for the plasma
        mov     si,[plasma_tex_x_offset]        ;to be used for the plasma_tex_xbuff offset
        mov     di,si
        not     di                              ;going from right to left

        %assign YY 0
        %rep    PLASMA_TEX_HEIGHT

                mov     al,[plasma_tex_xbuf+si] ;fetch plasma X buffer
                add     al,[plasma_tex_ybuf+YY] ; and add it to plasma Y buffer
                xlat                            ; and get the color value from luminances_tble
                mov     [es:PLASMA_TEX_OFFSET+(YY/4)*160+(YY % 4)*8192+di],al
        %assign YY YY+1
        %endrep

        cmp     word [plasma_tex_x_offset],159  ;rendered 159 lines (whole width)?
        je      .next_internal_state            ; if so, trigger next state

        inc     word [plasma_tex_x_offset]      ;offset_x++. render in next column next time
        ret

.next_internal_state:
        inc     byte [plasma_tex_state]         ;trigger next internal state
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_control_code_color_red:
        mov     al,[scroll_pixel_red_tbl+1]   ;skip first byte. it is black in all conf
        mov     bx,[scroll_pixel_red_tbl+2]
        mov     [scroll_pixel_color_tbl+1],al
        mov     [scroll_pixel_color_tbl+2],bx
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_control_code_color_yellow:
        mov     al,[scroll_pixel_yellow_tbl+1]   ;skip first byte. it is black in all conf
        mov     bx,[scroll_pixel_yellow_tbl+2]
        mov     [scroll_pixel_color_tbl+1],al
        mov     [scroll_pixel_color_tbl+2],bx
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_control_code_color_anim:
        mov     al,[scroll_pixel_anim_tbl+1]   ;skip first byte. it is black in all conf
        mov     bx,[scroll_pixel_anim_tbl+2]
        mov     [scroll_pixel_color_tbl+1],al
        mov     [scroll_pixel_color_tbl+2],bx
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_init:
        mov     bp,es                           ;save es for later
        mov     ax,ds
        mov     es,ax                           ;es=ds

        sub     al,al                           ;black color
        mov     cx,PLASMA_TEX_PALETTE_MAX       ;set bottom palette to black
        mov     di,bottom_palette+1             ;destination: bottom palette, skipping black
        rep stosb                               ;copy the new palette
        mov     es,bp                           ;restore es

        mov     byte [plasma_effect_transition_state],0
        mov     byte [plasma_effect_idx],0
        mov     byte [plasma_effect_trigger],1  ;trigger effect
        mov     byte [plasma_effect_delay],3    ;delay
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; updates the plasma effect
plasma_effect_update:
        cmp     byte [plasma_effect_trigger],0
        jnz     .do
        ret

.do:
        dec     byte [plasma_effect_delay]
        jz      .do_effect
        ret

.do_effect:
        mov     byte [plasma_effect_delay],3
        sub     bh,bh
        mov     bl,[plasma_effect_transition_state]
        inc     byte [plasma_effect_transition_state]
        cmp     bl,13
        je      .finish_transition
        cmp     bl,6
        jb      .fade_out
        je      .change_plasma_effect

        ;fall through
.fade_in:                                       ;called with bl 7-12
        sub     bh,bh
        sub     bl,7                            ;re-range to 0-5
        mov     dl,5                            ;re-range to 5-0
        sub     dl,bl                           ;inverse it. fetch last color first

        mov     bp,es
        mov     ax,ds
        mov     es,ax
        mov     cx,5
        mov     si,bottom_palette+1+4           ;shift colors to the left
        mov     di,bottom_palette+1+5           ;after 6 iterations all colors should be in
        std
        rep movsb                               ; their final position
        cld
        mov     es,bp                           ;restore bp

        mov     bl,[plasma_effect_idx]          ;fetch plasma effect index
        shl     bl,1                            ;*2, since each entry takes 2 bytes

        mov     si,[plasma_palettes_tbl+bx]     ;pointer to palette to be used
        mov     bl,dl                           ;color to be used
        mov     al,[si+bx]
        mov     [bottom_palette+1+0],al         ;move it for 1st position
                                                ;and rotate colors to the right
        ret

.fade_out:                                      ;called with bl 0-5
        mov     bp,es
        mov     ax,ds
        mov     es,ax
        mov     cx,5
        mov     si,bottom_palette+1+1           ;shift colors to the left (black is color idx 5)
        mov     di,bottom_palette+1+0           ;after 6 iterations, everything will be black
        rep movsb
        mov     es,bp                           ;restore bp
        ret

.change_plasma_effect:                          ;called with bl==6
        sub     bh,bh
        mov     bl,[plasma_effect_idx]          ;fetch plasma effect index
        shl     bl,1                            ;*2, since each entry takes 2 bytes

        mov     ax,[plasma_inc_x0_x1_tbl+bx]    ;update plasma inc x0 x0
        mov     [plasma_inc_x0_x1],ax

        mov     ax,[plasma_inc_y0_y1_tbl+bx]    ;update plasma inc y0 y1
        mov     [plasma_inc_y0_y1],ax

        mov     ax,[plasma_off_x0_x1_inc_tbl+bx]        ;update plasma offset inc x0 x1
        mov     [plasma_off_x0_x1_inc],ax

        mov     ax,[plasma_off_y0_y1_inc_tbl+bx]        ;update plasma offset inc y0 y1
        mov     [plasma_off_y0_y1_inc],ax

        ret

.finish_transition:                             ;called with bl=13
        sub     al,al
        mov     [plasma_effect_trigger],al      ;disable the "trigger effect" condition
        mov     [plasma_effect_transition_state],al     ;reset state for next transition
        inc     byte [plasma_effect_idx]
        cmp     byte [plasma_effect_idx],PLASMA_EFFECT_MAX
        jne     .exit
        mov     byte [plasma_effect_idx],al
.exit:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_anim:
        ror     byte [plasma_anim_state],1      ;plasma at 30FPS:
        jc      .generate_tables                ; on frame it generates the tables
        jmp     .render_plasma                  ; on the other frame renders the plasma

.generate_tables:
        ;
        ;update xbuf and ybuf tables
        ;
        mov     ax,ds
        mov     es,ax                           ;es = ds

        ; x
        mov     ax,word [plasma_off_x0_x1]      ;fetches both xbuf_1 and xbuf_2
        mov     bp,ax                           ;save ax for later

        mov     dx,[plasma_inc_x0_x1]           ;inc x0 / x1 values
        ;HACK: works because fn_table_1 and
        ;fn_table_2 are 256-aligned
        mov     bx,fn_table_1
        add     bl,al
        mov     cx,fn_table_2
        add     cl,ah
        mov     si,cx
        mov     di,plasma_xbuf                  ;es:di -> dst

        %assign XX 0
        %rep    PLASMA_WIDTH
                mov     al,[bx]                 ;xbuf[idx] = sine[bx]+sine[si]
                add     al,[si]
                stosb                           ;update plasma xbuf
                add     bl,dl                   ;update offsets to sine tables
                add     cl,dh
                mov     si,cx
        %assign XX XX+1
        %endrep

        add     bp,[plasma_off_x0_x1_inc]       ;update offset x0/x1
        mov     word [plasma_off_x0_x1],bp      ; and save it for next iteration

        ; y
        mov     ax,word [plasma_off_y0_y1]      ;fetches both ybuf_1 and ybuf_2
        mov     bp,ax                           ;save ax for later

        mov     dx,[plasma_inc_y0_y1]           ;inc y0 / y1 values
        ;HACK: works because fn_table_1 and
        ;fn_table_2 are 256-aligned
        mov     bx,fn_table_1
        add     bl,al
        mov     cx,fn_table_2
        add     cl,ah
        mov     si,cx
        mov     di,plasma_ybuf                  ;es:di -> dst

        %assign YY 0
        %rep    PLASMA_HEIGHT
                mov     al,[bx]                 ;ybuff[YY] = sine[bx]+sine[si]
                add     al,[si]
                stosb                           ;update plasma ybuf
                add     bl,dl
                add     cl,dh
                mov     si,cx
        %assign YY YY+1
        %endrep

        add     bp,[plasma_off_y0_y1_inc]       ;update offset y0/y1
        mov     word [plasma_off_y0_y1],bp      ; and save it for next iteration

        mov     ax,0xb800
        mov     es,ax                           ;restore es
        ret

.render_plasma:
        ;
        ;render plasma
        ;
        mov     bx,luminances_6_colors          ;to be used by xlat. has the colors for the plasma

        mov     di,plasma_ybuf                  ;di -> pointer to plasma_ybuf
        mov     cx,plasma_xbuf                  ;dx -> pointer to plasma_xbuf
        %assign YY 0
        %rep    PLASMA_HEIGHT

                mov     ah,[di]                 ;fetch y buf value. to be used inside loop x
                mov     si,cx                   ;reset xbuf pointer before starting loop x

                %assign XX 0
                %rep    PLASMA_WIDTH
                        lodsb                   ;fetch plasma X buffer
                        add     al,ah           ; and add both plasma buf x and y
                        xlat                    ; and get the color value from luminances_tble
                        mov     [es:PLASMA_OFFSET+(YY/4)*160+(YY % 4)*8192+XX],al       ;left-to-right
                        mov     [es:PLASMA_OFFSET+(YY/4)*160+(YY % 4)*8192+159-XX],al   ;mirror: right-to-left
                %assign XX XX+1
                %endrep
                inc     di                      ;inc Y ref
        %assign YY YY+1
        %endrep

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IN:   al=char to print
; ASSUMES:  es: pointer to video segment
;           ds: pointer to charset segment
text_writer_print_char:

        sub     ah,ah
        mov     bx,ax                           ;bx = ax (char to print)
        shl     bx,1
        shl     bx,1
        shl     bx,1                            ;bx*8 since each char takes 8 bytes

        mov     bp,8192-4                       ;faster to operate with registers

        mov     di,[text_writer_addr]           ;video destination address
        RENDER_CHAR_ROW                         ;1st row
        add     di,bp
        RENDER_CHAR_ROW                         ;2nd row
        add     di,bp
        RENDER_CHAR_ROW                         ;3rd row
        add     di,bp
        RENDER_CHAR_ROW                         ;4th row


        sub     di,24576-156                    ;160-4
        RENDER_CHAR_ROW                         ;5th row
        add     di,bp
        RENDER_CHAR_ROW                         ;6th row
        add     di,bp
        RENDER_CHAR_ROW                         ;7th row
        add     di,bp
        RENDER_CHAR_ROW                         ;8th row

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Since text_writer_print_char is somewhat slow, this function renders two
; blocky chars (based) on the pattern passed. More efficient than calling
; text_writer_print_char twice, once with an empty char, and one with a full
; char
;
; IN:   ah=pattern (color) to be used for left char
;       al=pattern (color) to be used for right char
; ASSUMES:  es: pointer to video segment
text_writer_fill_two_chars:

        mov     bl,ah                           ;save for later
        mov     bh,bl                           ;bx = second char pattern
        mov     cl,al
        mov     ch,cl                           ;cx = first char pattern

        mov     bp,8192-8                       ;faster to do operations with registers

        mov     di,[text_writer_addr]
        RENDER_DOUBLE_ROW                       ;1st row
        add     di,bp
        RENDER_DOUBLE_ROW                       ;2nd row
        add     di,bp
        RENDER_DOUBLE_ROW                       ;3rd row
        add     di,bp
        RENDER_DOUBLE_ROW                       ;4th row

        sub     di,24576-152                    ;160-8
        RENDER_DOUBLE_ROW                       ;5th row
        add     di,bp
        RENDER_DOUBLE_ROW                       ;6th row
        add     di,bp
        RENDER_DOUBLE_ROW                       ;7th row
        add     di,bp
        RENDER_DOUBLE_ROW                       ;8th row
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Since text_writer_print_char is somewhat slow, this function renders one
; blocky chars (based) on the pattern passed.
;
; IN:   al=pattern (color) to be used for the char
; ASSUMES:  es: pointer to video segment
text_writer_fill_one_char:

        mov     ah,al                           ;ax has color info for 4 pixels
        mov     cx,8192-4                       ;faster to add from reg, than immediate

        mov     di,[text_writer_addr]
        stosw                                   ;1st row
        stosw
        add     di,cx
        stosw                                   ;2nd row
        stosw
        add     di,cx
        stosw                                   ;3rd row
        stosw
        add     di,cx
        stosw                                   ;4th row
        stosw

        sub     di,24576-156                    ;160-4
        stosw                                   ;5th row
        stosw
        add     di,cx
        stosw                                   ;6th row
        stosw
        add     di,cx
        stosw                                   ;7th row
        stosw
        add     di,cx
        stosw                                   ;8th row
        stosw
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
inc_d020:
        mov     dx,0x03da                       ;show how many raster barts it consumes
        mov     al,2                            ;select border color
        out     dx,al

        mov     dl,0xde                         ;dx=0x03de
        mov     al,0x0f
        out     dx,al                           ;change border to white
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
dec_d020:
        mov     dx,0x03da                       ;show how many raster barts it consumes
        mov     al,2                            ;select border color
        out     dx,al

        mov     dl,0xde                         ;dx=0x03de
        sub     al,al
        out     dx,al                           ;change border back to black

        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; DATA GFX
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .gfx data
        incbin 'src/logo.raw'                   ;MUST be the first variable in the segment

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;
; DATA MUSIC + CHARSET + MISC
;
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data data

pvm_song:
        incbin "src/uctumi-song.pvm"

pvm_wait:                                       ;cycles to read divided 0x2df
        db 0
pvm_offset:                                     ;pointer to next byte to read
        dw 0

noise_triggered:
        db 0                                    ;boolen. whether noise is playing
noise_fade_color_idx:                           ;offset in color table
        db 0
noise_fade_colors_tbl:
        db      8,7,11,8,1,0
NOISE_FADE_COLORS_MAX equ $-noise_fade_colors_tbl


LFSR_START_STATE equ 1973                       ;lfsr start state
clemente_lfsr_current_state     dw      0       ;lfsr current state

fake_crash_lfsr_state:          db      0       ;lfsr for crash

palette_black_delay:
        dw      0
palette_black_idx:
        dw      0
palette_black_tbl:                              ;fade to white/fade to black colors
        db      9,11,3,7,8,0
PALETTE_BLACK_MAX equ $-palette_black_tbl

global c64_charset
c64_charset:
        incbin 'src/c64_charset-charset.bin'

charset:
        incbin 'src/font_unknown_2x2-charset.bin'

scroll_control_code_tbl:
        dw      scroll_control_code_color_red
        dw      scroll_control_code_color_anim
        dw      scroll_control_code_color_yellow
        ; # = tm
        ; % = closing double quotes
        ; $ = smiley
        ; ; = ~ (kind of separator)
        ; < = heart
        ; bit 7=on (128) - control code
        ;       128 = color red
        ;       129 = color with raster bar
        ;       130 = color yellow
scroll_text:
;        db 128                                  ;color white
;        db 'HI THERE. '
;        db 129,'PUNGAS DE '
;        db 129,'VILLA '
;        db 129,'M'
;        db 128,'ARTELLI HERE, WITH OUR FIRST TANDY RELEASE. '
        db 129                                  ;color with raster bar
        db 'IT ALL BEGAN WHEN WE WENT TO PICK UP A COMMODORE 64 BUNDLE '
        db 'AND THE SELLER INCLUDED TWO TANDY 1000 HX IN IT. '
        db 'WTF IS A TANDY 1000 HX? WE GOOGLED IT, AND WE LIKED IT. '
        db `HEY, IT HAS AN 8088 (THAT CPU FROM 1979), SOME NON-`
        db 'STANDARD VIDEO MODES, A BETTER-THAN-SPEAKER SOUNDCARD, AND JOYSTICK PORTS '
        db '(UNIJOYSTICLE COMING SOON#). '
        db 'AND HERE WE ARE, WITH OUR FIRST TANDY 1000 RELEASE. WE CALL IT '
        db `"TANDY 64%... GOT IT ? `
        db  130,'&',129                      ;emoticons in yellow
        db '.'
        db '   ;   '
        db 'MANY THANKS TO DEMOSPLASH FOR GOING THE EXTRA MILE, AND ADDING TANDY 1000 SUPPORT!!! '
        db 128                                  ;color yellow
        db 27,28,29,30,31,42,43                 ;Radio Shack (using Radio Shack font)
        db 129                                  ;color with raster bar
        db ' DESERVES IT ! '
        db '   ;   '
        db 'SENDING OUR REGARDS TO ALL THE 8088 SCENE, AND TO OUR =64 / PC FRIENDS'
        db '   ;   '
        db 'BESO GRANDE A LA CANTANTE MAS GROSA DE TODAS:  '
        db 130,'<<<',129                       ;hears in yellow
        db ' LIA CRUCET '
        db 130,'<<<',129                       ;hears in yellow
        db '   ;   '
        db 'CODE:RIQ, MUSIC: UCTUMI, GRAPHICS: ALAKRAN'
        db '                   ',130,'>  >  >  >  >',129,
        db '                   '
SCROLL_TEXT_LEN equ $-scroll_text

scroll_char_idx:                                ;pointer to the next char
        dw 0
scroll_bit_idx:                                 ;pointer to the next bit in the char
        db 0                                    ;HACK: scroll_bit_idx must be placed RIGHT BEFORE scroll_col_used
scroll_col_used:                                ;HACK: scroll_col_used MUST be placed RIGHT AFTER scroll_bit_idx
        db 0                                    ;chars are 2x2. col indicates which col is being used

scroll_pixel_color_tbl:                         ;the colors for the scroll letters
        db      0,0,0,0                         ; it contains a copy of one of the tables below

scroll_pixel_red_tbl:                           ;these are pixels, not palette colors
        db      0x00                            ;00 - black/black
        db      0x0c                            ;01 - black/white
        db      0xc0                            ;10 - white/black
        db      0xcc                            ;11 - white/white
scroll_pixel_yellow_tbl:                        ;these are pixels, not palette colors
        db      0x00                            ;00 - black/black
        db      0x0e                            ;01 - black/white
        db      0xe0                            ;10 - white/black
        db      0xee                            ;11 - white/white
scroll_pixel_anim_tbl:                          ;these are pixels, not palette colors
        db      0x00                            ;00 - black/black
        db      0x0f                            ;01 - black/white
        db      0xf0                            ;10 - white/black
        db      0xff                            ;11 - white/white

scroll_enabled:                                 ;boolean: enabled?
        db      0
scroll_effect_enabled:                          ;boolean. whether to enable plasma + raster bar
        db      0
palette_letter_color_idx:                       ;index for table used in fade in/out effects
        db      0
palette_grayscale_in_tbl:                       ;fade in in grayscale
        db      8,8,7,7,15,15,15,15
PALETTE_GRAYSCALE_IN_MAX equ $-palette_grayscale_in_tbl

palette_grascale_out_tbl:                       ;fade out in grayscale
        db      7,7,8,8,0
PALETTE_GRAYSCALE_OUT_MAX equ $-palette_grascale_out_tbl

palette_black_to_pink_tbl:
        db      0,1,9,5,13
PALETTE_BLACK_TO_PINK_MAX equ $-palette_black_to_pink_tbl

palette_black_to_cyan_tbl:
        db      0,2,10,3,11
PALETTE_BLACK_TO_CYAN_MAX equ $-palette_black_to_cyan_tbl

palette_black_to_green_tbl:
        db      0,6,14,2,10
PALETTE_BLACK_TO_GREEN_MAX equ $-palette_black_to_green_tbl

palette_outline_fade_to_final_tbl:              ;fade in until gets final color
        db      8,8,7,7,11,11,11,11,7,7,8,8,1
PALETTE_OUTLINE_FADE_TO_FINAL_MAX equ $-palette_outline_fade_to_final_tbl

palette_black_white_black_tbl:
        db      0,8,8,8,7,7,15,15,7,7,8,8,8,0
PALETTE_BLACK_WHITE_BLACK_MAX equ $-palette_black_white_black_tbl

palette_idx:
        db      0

        db      0,0,0,0,0,0,0,0                 ;buffer for letter M
        db      0,0,0,0,0,0,0,0                 ;buffer for letter P
palette_pvm_logo_fade_tbl:                      ;inner logo color
        db      8,8,8,8,7,7,7,7
        db      11,11,11,11,9,9,9,9

        db      9,9,9,9,9,9,9,9
        db      9,9,9,9,9,9,9,9
PALETTE_PVM_LOGO_FADE_MAX equ $-palette_pvm_logo_fade_tbl

volume_0:
        db      0b1001_1111                     ;vol 0 channel 0
        db      0b1011_1111                     ;vol 0 channel 1
        db      0b1101_1111                     ;vol 0 channel 2
        db      0b1111_1111                     ;vol 0 channel 3
VOLUME_0_MAX equ $ - volume_0


main_state_delay_frames:
        dw      0                               ;frames to wait before doing something

main_state:                                     ;main state. index for the
        db      0                               ; function to call.

main_state_inits:
        dw      state_gfx_fade_in_init          ;a
        dw      state_fade_to_black_init        ;b
        dw      state_delay_500ms_init          ;c

        dw      state_new_i08_multi_color_init  ;d
        dw      state_plasma_red_tex_init       ;e
        dw      state_plasma_green_tex_init     ;f
        dw      state_plasma_magenta_tex_init   ;g
        dw      state_clear_bottom_init         ;h

        dw      state_new_i08_full_color_init   ;i
        dw      state_signal_letter_state_sem_init      ;j
        dw      state_clear_bottom_init         ;l
        dw      state_enable_scroll             ;m
        dw      state_delay_2s_init             ;n
        dw      state_enable_scroll_effects     ;p
        dw      state_delay_2s_init             ;n'
;        dw      state_enable_text_writer        ;o
        dw      state_enable_boy_walk           ;o

        dw      state_nothing_init              ;q

main_state_callbacks:
        dw      state_gfx_fade_in_anim          ;a
        dw      state_fade_to_black_anim        ;b
        dw      state_delay_anim                ;c

        dw      state_skip_anim                 ;d
        dw      state_plasma_tex_anim           ;e
        dw      state_plasma_tex_anim           ;f
        dw      state_plasma_tex_anim           ;g
        dw      state_clear_bottom_anim         ;h

        dw      state_skip_anim                 ;i
        dw      state_skip_anim                 ;j
        dw      state_clear_bottom_anim         ;l
        dw      state_skip_anim                 ;m
        dw      state_delay_anim                ;n
        dw      state_skip_anim                 ;p
        dw      state_delay_anim                ;n'
        dw      state_skip_anim                 ;o
        dw      state_nothing_anim              ;q


letter_state_delay_frames:
        dw      0                               ;frames to wait before doing something

letter_state:                                   ;PVM letters animation state machine
        db      0

letter_state_inits:                             ;initialization callbacks
        dw      letter_state_wait_sem_init      ;a

        ; PVM in blue, like IBM
        dw      letter_state_fade_in_1_at_time_init     ;b
        dw      letter_state_outline_fade_init  ;c
        dw      letter_state_delay_10s_init     ;d
;        dw      letter_state_bkg_in_out_init    ;e
        dw      letter_state_outline_noise_10s_init ;f
;        dw      letter_state_bkg_in_out_init    ;g
        dw      letter_state_delay_200ms_init      ;h

        dw      letter_state_outline_fade_init  ;i

        ;P in
        dw      letter_state_fade_out_p_init    ;j
        dw      letter_state_fade_in_p_init     ;k
        dw      letter_state_delay_200ms_init   ;l

        ;V in
        dw      letter_state_fade_out_v_init    ;m
        dw      letter_state_fade_in_v_init     ;n
        dw      letter_state_delay_200ms_init   ;o

        ;M in
        dw      letter_state_fade_out_m_init    ;p
        dw      letter_state_fade_in_m_init     ;q

        dw      letter_state_delay_5s_init      ;r

        ;PVM out
        dw      letter_state_fade_out_p_init    ;s
        dw      letter_state_delay_200ms_init   ;t
        dw      letter_state_fade_out_v_init    ;u
        dw      letter_state_delay_200ms_init   ;v
        dw      letter_state_fade_out_m_init    ;w

        dw      letter_state_delay_5s_init      ;x

        ;music rythm
        dw      letter_state_outline_noise_10s_init ;y
        dw      letter_state_outline_fade_init  ;y'

        ;PVM in (left to right)
        dw      letter_state_fade_in_p_init     ;z
        dw      letter_state_delay_200ms_init   ;aa
        dw      letter_state_fade_in_v_init     ;ab
        dw      letter_state_delay_200ms_init   ;ac
        dw      letter_state_fade_in_m_init     ;ad
        dw      letter_state_delay_200ms_init   ;ae

        ;music rythm
        dw      letter_state_outline_noise_10s_init ;af
        dw      letter_state_outline_fade_init  ;af'

        ;PVM out (left to right)
        dw      letter_state_fade_out_p_init    ;ag
        dw      letter_state_delay_200ms_init   ;ah
        dw      letter_state_fade_out_v_init    ;ai
        dw      letter_state_delay_200ms_init   ;aj
        dw      letter_state_fade_out_m_init    ;ak

        dw      letter_state_delay_5s_init      ;al

        ;PVM in (right to left)
        dw      letter_state_fade_in_m_init     ;am
        dw      letter_state_delay_200ms_init   ;an
        dw      letter_state_fade_in_v_init     ;ao
        dw      letter_state_delay_200ms_init   ;ap
        dw      letter_state_fade_in_p_init     ;aq
        dw      letter_state_delay_200ms_init   ;ar

        ;music rythm
        dw      letter_state_outline_noise_10s_init ;as
        dw      letter_state_outline_fade_init  ;as'

        ;PVM out (right to left)
        dw      letter_state_fade_out_m_init    ;at
        dw      letter_state_delay_200ms_init   ;au
        dw      letter_state_fade_out_v_init    ;av
        dw      letter_state_delay_200ms_init   ;aw
        dw      letter_state_fade_out_p_init    ;ax

        dw      letter_state_delay_5s_init      ;ay

        dw      letter_state_restart_loop_init  ;az

letter_state_callbacks:                         ;animation callbacks
        dw      letter_state_wait_sem_anim      ;a

        ; PVM in blue, like IBM
        dw      letter_state_fade_in_1_at_time_anim     ;b
        dw      letter_state_outline_fade_to_final_anim ;c
        dw      letter_state_delay_anim         ;d
;        dw      letter_state_bkg_in_out_anim    ;e
        dw      letter_state_outline_noise_anim ;f
;        dw      letter_state_bkg_in_out_anim    ;g
        dw      letter_state_delay_anim         ;h

        dw      letter_state_outline_fade_out_anim      ;i

        ;P in
        dw      letter_state_fade_out_letter_anim       ;j
        dw      letter_state_fade_to_cyan_letter_anim   ;k
        dw      letter_state_delay_anim         ;l

        ;V in
        dw      letter_state_fade_out_letter_anim       ;m
        dw      letter_state_fade_to_green_letter_anim  ;n
        dw      letter_state_delay_anim         ;o

        ;M in
        dw      letter_state_fade_out_letter_anim       ;p
        dw      letter_state_fade_to_pink_letter_anim   ;q
        dw      letter_state_delay_anim         ;r

        ;PVM out
        dw      letter_state_fade_out_letter_anim       ;s
        dw      letter_state_delay_anim         ;t
        dw      letter_state_fade_out_letter_anim       ;u
        dw      letter_state_delay_anim         ;v
        dw      letter_state_fade_out_letter_anim       ;w

        dw      letter_state_delay_anim         ;x

        ;music rythm
        dw      letter_state_outline_noise_anim ;y
        dw      letter_state_outline_fade_out_anim      ;y'

        ;PVM in (left to right)
        dw      letter_state_fade_to_cyan_letter_anim   ;z
        dw      letter_state_delay_anim         ;aa
        dw      letter_state_fade_to_green_letter_anim  ;ab
        dw      letter_state_delay_anim         ;ac
        dw      letter_state_fade_to_pink_letter_anim   ;ad
        dw      letter_state_delay_anim         ;ae

        ;music rythm
        dw      letter_state_outline_noise_anim ;af
        dw      letter_state_outline_fade_out_anim      ;af'

        ;PVM out (left to right)
        dw      letter_state_fade_out_letter_anim       ;ag
        dw      letter_state_delay_anim         ;ah
        dw      letter_state_fade_out_letter_anim       ;ai
        dw      letter_state_delay_anim         ;aj
        dw      letter_state_fade_out_letter_anim       ;ak

        dw      letter_state_delay_anim         ;al

        ;PVM in (right to left)
        dw      letter_state_fade_to_cyan_letter_anim   ;am
        dw      letter_state_delay_anim         ;an
        dw      letter_state_fade_to_green_letter_anim  ;ao
        dw      letter_state_delay_anim         ;ap
        dw      letter_state_fade_to_pink_letter_anim   ;aq
        dw      letter_state_delay_anim         ;ar

        ;music rythm
        dw      letter_state_outline_noise_anim ;as
        dw      letter_state_outline_fade_out_anim      ;as'

        ;PVM out (right to left)
        dw      letter_state_fade_out_letter_anim       ;at
        dw      letter_state_delay_anim         ;au
        dw      letter_state_fade_out_letter_anim       ;av
        dw      letter_state_delay_anim         ;aw
        dw      letter_state_fade_out_letter_anim       ;ax

        dw      letter_state_delay_anim         ;ay

        dw      letter_state_restart_loop_anim  ;az

letter_state_semaphore:                         ;semaphore used in letter state machine
        db      0

letter_state_color_to_fade:                     ;which color idx to fade
        db      0

CENTRAL_SCREEN_STATE_WAIT               equ 0
CENTRAL_SCREEN_STATE_BOY_ANIM           equ 1
CENTRAL_SCREEN_STATE_TEXT_WRITTER       equ 2
central_screen_state:                           ;state machine for the "central part of the screen"
        db      0

text_writer_addr:                               ;address where the char will be written
        dw      0
text_writer_x_pos:                              ;position x for the cursor. 0-39
        db      0                               ; but supports in the range of -127,128
text_writer_x_dst:                              ;dst position x for the cursor. 0-39
        db      0
text_writer_y_pos:                              ;position y for the cursor. 0-24
        db      0                               ; but supports in the range of -127,128
text_writer_y_dst:                              ;dst position 0 for the cursor. 0-24
        db      0

text_writer_bitmap_to_video_tbl:                ;converts charset (bitmap) to video bytes. nibble only
        db      0x00,0x00                       ;0000
        db      0x00,0x07                       ;0001
        db      0x00,0x70                       ;0010
        db      0x00,0x77                       ;0011
        db      0x07,0x00                       ;0100
        db      0x07,0x07                       ;0101
        db      0x07,0x70                       ;0110
        db      0x07,0x77                       ;0111

        db      0x70,0x00                       ;1000
        db      0x70,0x07                       ;1001
        db      0x70,0x70                       ;1010
        db      0x70,0x77                       ;1011
        db      0x77,0x00                       ;1100
        db      0x77,0x07                       ;1101
        db      0x77,0x70                       ;1110
        db      0x77,0x77                       ;1111

text_writer_idx:                                ;offset to the text_writer_data
        dw      0

text_writer_state:
        db      0
        ;text writer enums. must be synced with callbacks order
TW_STATE_PRINT_CHAR     equ 0
TW_STATE_IDLE           equ 1
TW_STATE_GOTO_X         equ 2
TW_STATE_GOTO_Y         equ 3
TW_STATE_CALL_ACTION    equ 4
TW_STATE_CURSOR_BLINK   equ 5
TW_STATE_MAX            equ 6                   ;should be the last state
text_writer_callbacks_init:
        dw      0                                       ;no init for print_char
        dw      text_writer_state_idle_init
        dw      text_writer_state_goto_x_init
        dw      text_writer_state_goto_y_init
        dw      text_writer_state_call_action_init
        dw      text_writer_state_cursor_blink_init

text_writer_callbacks_anim:
        dw      text_writer_state_print_char_anim
        dw      text_writer_state_idle_anim
        dw      text_writer_state_goto_x_anim
        dw      text_writer_state_goto_y_anim
        dw      text_writer_state_call_action_anim
        dw      text_writer_state_cursor_blink_anim

        ;control codes: think of it as a printer
        ; 0 - idle
        ; 2 - print char
        ; 3 - go to
        ; 4 - carriage return
        ; 5,n - perform an immediate action: eg: call enable_something
text_writer_data:
                ;0123456789012345678901234567890123456789
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,17              ;go to pos
        db      'Hi!'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,8               ;go to pos
        db      'Pungas de Villa Martelli'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,7               ;go to pos
        db      '    proudly presents',31
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,11              ;go to pos
        db      '*** Tandy 64 ***'
        db      TW_STATE_CURSOR_BLINK,4         ;wait blinks

        db      TW_STATE_GOTO_X,7               ;go to pos
        db      '(our first Tandy release)'
        db      TW_STATE_CURSOR_BLINK,5         ;wait blinks

        db      TW_STATE_GOTO_X,6               ;go to pos
        db      ' Run it using real hardware'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,6               ;go to pos 5
        db      '       Requirements:'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,6               ;go to pos 5
        db      'Tandy 1000 HX (or compatible)'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,5               ;go to pos 5
        db      '     at least 256Kb RAM'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,5               ;go to pos 5
        db      `If the colors don't look good`,31
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,4               ;go to pos 5
        db      '  ',31,`don't use composite output`,31
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,4               ;go to pos 5
        db      '    ',31,'use RGBI output instead'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,5               ;go to pos 5
        db      'Runs under DosBox / DosBox-x',31
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,5               ;go to pos 5
        db      '   ',31,'but with some glitches'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,6               ;go to pos 5
        db      'Feel free to contact us at:'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,4               ;go to pos 5
        db      '      http://pungas.space'
        db      TW_STATE_CURSOR_BLINK,4         ;wait blinks

        db      TW_STATE_GOTO_X,0               ;go to pos 5
        db      'Thanks Demosplash for supporting Tandy!'
        db      TW_STATE_CURSOR_BLINK,4         ;wait blinks

        db      TW_STATE_GOTO_X,0               ;go to pos 5
        db      '              code: riq'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,13               ;go to pos 5
        db      'music: Uctumi'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,11               ;go to pos 5
        db      'graphics: Alakran'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_X,8               ;go to pos 5
        db      'Until our next release!'
        db      TW_STATE_CURSOR_BLINK,5         ;wait blinks

        db      TW_STATE_GOTO_X,0               ;go to pos 5
        db      TW_STATE_CALL_ACTION,0          ;cursor off
        db      TW_STATE_IDLE,230               ;wait some cycles
                                                ; to make it sync with the letters
TEXT_WRITER_DATA_LEN equ $-text_writer_data

text_writer_cursor_blink_delay:                 ;how many cursor blinks to wait
        db      0
text_writer_delay:                              ;used by 'delay state' to know who many
        db      0                               ; vert retrace to wait

BOY_ANIM_STATE_WALK     equ 0
BOY_ANIM_STATE_DANCE    equ 1
BOY_ANIM_STATE_END      equ 2
boy_anim_state:                                 ;states for boy animation
        db      0
boy_anim_delay:                                 ;how many frames to wait
        db      0
boy_anim_vid_addr:                              ;video address to where draw the next frame
        dw      0
boy_anim_frame_0:                               ;dance: right root up
        db      0x00,0x00,0x00,0x00,0x07,0x77,0x70,0x00,0x7f,0x77,0x77,0x00,0xf0,0xff,0x0f,0x00
        db      0xff,0xff,0xff,0x00,0x8f,0x8f,0xf8,0x00,0x0f,0xff,0xf0,0x00,0x00,0x07,0x70,0x00
        db      0x00,0xff,0xff,0x00,0x08,0xff,0xff,0x80,0x07,0x77,0xff,0x80,0x00,0xff,0xff,0x00
        db      0x00,0x77,0x87,0x70,0x00,0x70,0x00,0x70,0x00,0x70,0x00,0x88,0x08,0x80,0x00,0x00
boy_anim_frame_1:                               ;dance: both feet on floor
        db      0x00,0x00,0x00,0x00,0x00,0x77,0x77,0x00,0x07,0x7f,0x77,0x70,0x0f,0x0f,0xf0,0xf0
        db      0x0f,0xff,0xff,0xf0,0x08,0xf8,0x8f,0x80,0x00,0xff,0xff,0x00,0x00,0x07,0x70,0x00
        db      0x00,0xff,0xff,0x00,0x08,0xff,0xff,0x80,0x08,0xff,0xff,0x80,0x00,0xff,0xff,0x00
        db      0x00,0x77,0x87,0x00,0x00,0x70,0x07,0x00,0x00,0x70,0x07,0x00,0x08,0x80,0x08,0x80
boy_anim_frame_2:                               ;dance: left foot up
        db      0x00,0x00,0x00,0x00,0x00,0x07,0x77,0x70,0x00,0x77,0x7f,0x77,0x00,0xf0,0xff,0x0f
        db      0x00,0xff,0xff,0xff,0x00,0x8f,0xf8,0xf8,0x00,0x0f,0xff,0xf0,0x00,0x07,0x70,0x00
        db      0x00,0xff,0xff,0x00,0x08,0xff,0xff,0x80,0x08,0xff,0x77,0x70,0x00,0xff,0xff,0x00
        db      0x07,0x78,0x77,0x00,0x07,0x00,0x07,0x00,0x88,0x00,0x07,0x00,0x00,0x00,0x08,0x80
boy_anim_frame_3:                               ;walk 0
        db      0x00,0x00,0x00,0x00,0x00,0x07,0x77,0x70,0x00,0x77,0x77,0x77,0x00,0x77,0xf0,0xf0
        db      0x00,0x7f,0xff,0xff,0x00,0x87,0xff,0x8f,0x00,0x08,0xff,0xf0,0x00,0x07,0x70,0x00
        db      0x00,0x7f,0xf8,0x00,0x00,0x7f,0xff,0x00,0x00,0x07,0x7f,0x00,0x00,0x7f,0xff,0x00
        db      0x00,0x77,0x87,0x00,0x87,0x70,0x07,0x00,0x80,0x00,0x07,0x00,0x00,0x00,0x08,0x80
boy_anim_frame_4:                               ;walk 1
        db      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x07,0x77,0x70,0x00,0x77,0x77,0x77
        db      0x00,0x77,0xf0,0xf0,0x00,0x7f,0xff,0xff,0x00,0x87,0xff,0x8f,0x00,0x08,0xff,0xf0
        db      0x00,0x07,0x70,0x00,0x00,0x7f,0xf8,0x00,0x00,0x7f,0xff,0x00,0x00,0x0f,0xff,0x00
        db      0x00,0x07,0x87,0x00,0x00,0x77,0x07,0x00,0x00,0x70,0x70,0x00,0x00,0x88,0x88,0x00
boy_anim_frame_5:                               ;walk 2
        db      0x00,0x00,0x00,0x00,0x00,0x07,0x77,0x70,0x00,0x77,0x77,0x77,0x00,0x77,0xf0,0xf0
        db      0x00,0x7f,0xff,0xff,0x00,0x87,0xff,0x8f,0x00,0x08,0xff,0xf0,0x00,0x07,0x70,0x00
        db      0x00,0x7f,0xf7,0x00,0x07,0x7f,0xff,0x00,0x07,0x0f,0xff,0x70,0x00,0x0f,0xff,0x00
        db      0x00,0x07,0x78,0x00,0x08,0x88,0x87,0x00,0x08,0x00,0x07,0x00,0x00,0x00,0x08,0x80
boy_dance_timeout:                              ;how many frames should the dance animation be active
        dw      0
boy_walk_col_pos:                               ;in which col the boy is positioned
        db      0
boy_walk_frame_idx:                             ;which frame is being displayed
        db      0
boy_walk_animation_tbl:                         ;frames for a full walk cycle
        dw      boy_anim_frame_3
        dw      boy_anim_frame_4
        dw      boy_anim_frame_5
        dw      boy_anim_frame_4
BOY_WALK_ANIM_FRAME_MAX equ ($-boy_walk_animation_tbl)/2        ;each frame addr takes 2 bytes

key_pressed:                                    ;boolean. non-zero when a key was pressed
        db      0
tick:                                           ;to trigger once the irq was called
        db      0
old_i09:                                        ;segment + offset to old int 9
        dd      0
old_i08:                                        ;segment + offset to old int 8
        dd      0
old_pic_imr:                                    ;PIC IMR original value
        db      0

raster_colors_tbl:                              ;used for the raster bars, at the
        db      15,15,15,15,15                  ; bottom of the screen
        db      15,15,15,15,15                  ;these are the default colors. used when the anim
        db      15,15,15,15,15,15               ; is not started yet
raster_color_restore:                           ;must be after raster_colors_tbl
        db      15                              ; it restores white color to its default palette
RASTER_COLORS_MAX equ $-raster_colors_tbl

raster_colors_anim_tbl:                         ;colors than will be copied to raster_colors_tbl
        db      15,15,15,15,15
        db      15,15,15,15,15
        db      15,15,15,15,15,15
        db      7
raster_colors_color_tbl:                        ;here starts the real colors to be updated
        db      9,8,1,8,9
        db      7
        db      15,15,15,15,15
        db      15,15,15,15,15
        db      15,15,15,15,15,15

        db      15,15,15,15,15                  ;buffer

raster_bars_colors_addr_start:                  ;used as an address: start of colors
raster_bars_blue_tbl:                           ;HACK: order must be the same as in
        db      9,8,1,8,9                       ; plasma_palettes_tbl
raster_bars_magenta_tbl:
        db      13,8,5,8,13
raster_bars_cyan_tbl:
        db      11,8,3,8,11
raster_bars_red_tbl:
        db      12,8,4,8,12
raster_bars_green_tbl:
        db      10,8,2,8,10
raster_bars_yellow_tbl:
        db      14,8,6,8,14
RASTER_BARS_COLOR_MAX   equ $-raster_bars_yellow_tbl
raster_bars_colors_addr_end:                    ;used as an address: end of colors
raster_bars_colors_addr:
        dw      0                               ;current address for colors


raster_colors_loops_for_each_color:             ;how many sine loops to do before chaning colors
        db      0
raster_colors_sine_idx:                         ;index to be used with the sine table
        db      0
raster_colors_sine_tbl:                         ;table that manipulates the raster_colors_anim_idx
        ; autogenerated table: easing_table_generator.py -s256 -m24 -aTrue sin
        db   0,  1,  1,  1,  1,  2,  2,  2
        db   3,  3,  3,  4,  4,  4,  4,  5
        db   5,  5,  6,  6,  6,  6,  7,  7
        db   7,  8,  8,  8,  8,  9,  9,  9
        db   9, 10, 10, 10, 11, 11, 11, 11
        db  12, 12, 12, 12, 13, 13, 13, 13
        db  14, 14, 14, 14, 15, 15, 15, 15
        db  15, 16, 16, 16, 16, 17, 17, 17
        db  17, 17, 18, 18, 18, 18, 18, 19
        db  19, 19, 19, 19, 19, 20, 20, 20
        db  20, 20, 20, 21, 21, 21, 21, 21
        db  21, 21, 22, 22, 22, 22, 22, 22
        db  22, 22, 22, 23, 23, 23, 23, 23
        db  23, 23, 23, 23, 23, 23, 23, 24
        db  24, 24, 24, 24, 24, 24, 24, 24
        db  24, 24, 24, 24, 24, 24, 24, 24
        db  24, 24, 24, 24, 24, 24, 24, 24
        db  24, 24, 24, 24, 24, 24, 24, 24
        db  23, 23, 23, 23, 23, 23, 23, 23
        db  23, 23, 23, 23, 22, 22, 22, 22
        db  22, 22, 22, 22, 22, 21, 21, 21
        db  21, 21, 21, 21, 20, 20, 20, 20
        db  20, 20, 19, 19, 19, 19, 19, 19
        db  18, 18, 18, 18, 18, 17, 17, 17
        db  17, 17, 16, 16, 16, 16, 15, 15
        db  15, 15, 15, 14, 14, 14, 14, 13
        db  13, 13, 13, 12, 12, 12, 12, 11
        db  11, 11, 11, 10, 10, 10,  9,  9
        db   9,  9,  8,  8,  8,  8,  7,  7
        db   7,  6,  6,  6,  6,  5,  5,  5
        db   4,  4,  4,  4,  3,  3,  3,  2
        db   2,  2,  1,  1,  1,  1,  0,  0

BOTTOM_TOP_LINES_TO_WAIT equ 32

top_palette:                                    ;palette used for the upper part of the screen
        db      0,1,2,3,4,5,6,7,8,9
        db      4                               ;color 10 should be 4 (red)
        db      11,12
        db      1                               ;color 13 should be 1 (blue)
        db      14,15

bottom_palette:                                 ;palette used for the bottom part of the screen
        db      0
        db      1,2,3,4,5,6                     ;only colors 1-6 are modified
        db      7,8,9,4,11,12,1,14,15           ;these colors are ignored

palette_default:
        db      0,1,2,3,4,5,6,7,8,9             ;default palette
        db      4                               ;color 10 should be 4 (red)
        db      11,12
        db      1                               ;color 13 should be 1 (blue)
        db      14,15

border_color:                                   ;border color
        db      9

; red, green and blue palettes, ALL MUST HAVE
; the same size
plasma_tex_red_palette:
        db      0xf                             ;white
        db      0x7                             ;light gray
        db      0xc                             ;light red
        db      0x8                             ;gray
        db      0x4                             ;red
        db      0                               ;black
PLASMA_TEX_PALETTE_MAX equ $-plasma_tex_red_palette

plasma_tex_green_palette:
        db      0xf                             ;white
        db      0x7                             ;light gray
        db      0xa                             ;light green
        db      0x8                             ;gray
        db      0x2                             ;green
        db      0                               ;black

plasma_tex_magenta_palette:
        db      0xf                             ;white
        db      0x7                             ;light gray
        db      0xd                             ;light magenta
        db      0x8                             ;gray
        db      0x5                             ;magenta
        db      0                               ;black

plasma_tex_blue_palette:
        db      0xf                             ;white
        db      0x7                             ;light gray
        db      0x9                             ;light blue
        db      0x8                             ;gray
        db      0x1                             ;blue
        db      0                               ;black

plasma_tex_cyan_palette:
        db      0xf                             ;white
        db      0x7                             ;light gray
        db      0xb                             ;light cyan
        db      0x8                             ;gray
        db      0x3                             ;cyan
        db      0                               ;black

plasma_tex_yellow_palette:
        db      0xf                             ;white
        db      0x7                             ;light gray
        db      0xe                             ;yellow
        db      0x8                             ;gray
        db      0x6                             ;brown
        db      0                               ;black

plasma_effect_delay:                            ;delay used while in transition to slow it down
        db      0
plasma_effect_trigger:                          ;boolean, when enabled start effect transition
        db      0
plasma_effect_transition_state:                 ;state of the transition effect
        db      0
plasma_effect_idx:                              ;plasma effect index
        db      0
plasma_palettes_tbl:                            ;table that contains the different palettes
        dw      plasma_tex_blue_palette         ; order must same as in raster_bars_colors_addr_start
        dw      plasma_tex_magenta_palette
        dw      plasma_tex_cyan_palette
        dw      plasma_tex_red_palette
        dw      plasma_tex_green_palette
        dw      plasma_tex_yellow_palette
PLASMA_EFFECT_MAX equ ($-plasma_palettes_tbl)/2 ;div 2, since each entry takes 2 bytes (dw)

plasma_inc_x0_x1_tbl:                           ;table that contains the different inc values for x
        dw      0x0406
        dw      0x0104
        dw      0x0304
        dw      0x0304
        dw      0x0305
        dw      0x8003

plasma_inc_y0_y1_tbl:                           ;table that contains the different inc values for y
        dw      0x0500
        dw      0x0202
        dw      0x8302
        dw      0x837f
        dw      0xfb02
        dw      0xff02

plasma_off_x0_x1_inc_tbl:                       ;table that contains the different off inc values for x
        dw      0x0300
        dw      0x0004
        dw      0x0004
        dw      0x0002
        dw      0x0703
        dw      0xff01

plasma_off_y0_y1_inc_tbl:                       ;table that contains the different off inc values for y
        dw      0xff00
        dw      0x0400
        dw      0x0403
        dw      0x0403
        dw      0xfcfd
        dw      0x03ff


plasma_tex_state:                               ;internal state used inside the state_plasma_tex state
        db      0                               ;0=render texture, 1=fade in palette
plasma_tex_colors_updated:                      ;number of palette colors that were updated
        db      0
plasma_tex_delay:                               ;refreshes to wait for the palette update
        db      0
plasma_tex_palette_addr:                        ;palette address to use
        dw      0
plasma_tex_letter_color:                        ;color of the PVM letter to update
        db      0
plasma_tex_x_offset:                            ;plama x offset. to make it scroll
        dw      0
plasma_tex_inc_x0:                              ;plasma xbuf increment a
        db      0
plasma_tex_inc_x1:                              ;plasma xbuf increment b
        db      0
plasma_tex_inc_y0:                              ;plasma ybuf increment a
        db      0
plasma_tex_inc_y1:                              ;plasma ybuf incrment b
        db      0

cycle_palette_delay:                            ;delay for the palette cycle animation
        db      1

clear_bottom_state:                             ;used by state_clearn_bottom_anim
        db      0                               ;when 0, clear first half. when 1, clear second half

crtc_start_addr:
        dw      0                               ;crtc start address

plasma_inc_x0_x1:                               ;plasma: inc x0 x1
        dw      0
plasma_inc_y0_y1:                               ;plasma: inc y0 y1
        dw      0
plasma_off_x0_x1:                               ;plasma: offset x0 y1
        dw      0
plasma_off_x0_x1_inc:                           ;plasma: offset increment x0 y1
        dw      0
plasma_off_y0_y1:                               ;plasma: offset x0 y1
        dw      0
plasma_off_y0_y1_inc:                           ;plasma: offset increment x0 y1
        dw      0
plasma_anim_state:
        db      0b1010_1010                     ;cycles between 1 and 0 with a ror
luminances_6_colors:
        db      0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11         ;white (0xff)
        db      0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11
        db      0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11
        db      0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22         ;should be: 0x77
        db      0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22
        db      0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22
        db      0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33         ;should be 0xcc
        db      0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33
        db      0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44         ;should be 0x88
        db      0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44
        db      0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44
        db      0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55         ;should be 0x44
        db      0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
        db      0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66         ;should be 0x00
        db      0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66
        db      0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66

;HACK: must be 256 aligned to be faster at calculating the plasma table
align 256
fn_table_1:
fn_table_2:
; autogenerated table: easing_table_generator.py -s128 -m128 -aTrue -r bezier:0,0.02,0.98,1
        db     0,  0,  0,  1,  1,  1,  1,  2
        db     2,  3,  3,  4,  4,  5,  5,  6
        db     7,  8,  8,  9, 10, 11, 12, 13
        db    13, 14, 15, 16, 17, 19, 20, 21
        db    22, 23, 24, 25, 27, 28, 29, 30
        db    32, 33, 34, 36, 37, 38, 40, 41
        db    42, 44, 45, 47, 48, 49, 51, 52
        db    54, 55, 57, 58, 60, 61, 63, 64
        db    65, 67, 68, 70, 71, 73, 74, 76
        db    77, 79, 80, 81, 83, 84, 86, 87
        db    88, 90, 91, 92, 94, 95, 96, 98
        db    99,100,101,103,104,105,106,107
        db   108,109,111,112,113,114,115,115
        db   116,117,118,119,120,120,121,122
        db   123,123,124,124,125,125,126,126
        db   127,127,127,127,128,128,128,128
; reversed
        db   128,128,128,127,127,127,127,126
        db   126,125,125,124,124,123,123,122
        db   121,120,120,119,118,117,116,115
        db   115,114,113,112,111,109,108,107
        db   106,105,104,103,101,100, 99, 98
        db    96, 95, 94, 92, 91, 90, 88, 87
        db    86, 84, 83, 81, 80, 79, 77, 76
        db    74, 73, 71, 70, 68, 67, 65, 64
        db    63, 61, 60, 58, 57, 55, 54, 52
        db    51, 49, 48, 47, 45, 44, 42, 41
        db    40, 38, 37, 36, 34, 33, 32, 30
        db    29, 28, 27, 25, 24, 23, 22, 21
        db    20, 19, 17, 16, 15, 14, 13, 13
        db    12, 11, 10,  9,  8,  8,  7,  6
        db     5,  5,  4,  4,  3,  3,  2,  2
        db     1,  1,  1,  1,  0,  0,  0,  0

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; resb/resw
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
cache_charset:
        resb    32                              ;the 32 bytes to print in the current frame
                                                ; char aligned like: top-left, bottom-left,
                                                ; top-right, bottom-right
plasma_tex_xbuf:                                ;plasma tex xbuffer
        resb   PLASMA_TEX_WIDTH
plasma_tex_ybuf:                                ;plasma tex ybuffer
        resb   PLASMA_TEX_HEIGHT
plasma_xbuf:                                    ;plasma xbuffer
        resb   PLASMA_WIDTH
plasma_ybuf:                                    ;plasma ybuffer
        resb   PLASMA_HEIGHT

