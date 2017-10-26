; Tandy64 intro
; http://pungas.space
; code: riq

bits    16
cpu     8086

extern ZTimerOn, ZTimerOff, ZTimerReport

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; MACROS
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
%define DEBUG 0                                 ;0=diabled, 1=enabled

TEXT_WRITER_START_Y     equ 19                  ;start at line 19
BOTTOM_OFFSET   equ     21*2*160-160            ;start at line 21:160 bytes per line, lines are every 4 -> 8/4 =2
SCROLL_OFFSET   equ     22*2*160                ;start at line 22:160 bytes per line, lines are every 4 -> 8/4 =2
PLASMA_TEX_OFFSET       equ 21*2*160            ;plasma texture: video offset. +160 bc it starts from right to left
PLASMA_TEX_WIDTH        equ 160                 ;plasma texture: pixels wide
PLASMA_TEX_HEIGHT       equ 32                  ;plasma texture: pixels height
PLASMA_OFFSET   equ 22*2*160+64                 ;plasma: video offset
PLASMA_WIDTH    equ 24                          ;plasma: pixels wide
PLASMA_HEIGHT   equ 16                          ;plasma: pixels height


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; render vertically 4 bits needed for the scroll. grabs the firts for bytes from the cache,
; use the MSB bit. If it is on, use white, else black color
;
; IN:   ds:si   -> bit to render (pointer to cache)
;       es:di   -> where to render (ponter to video memory)
;       dx      -> pointer to pixel color table
;       bp      -> row index
; Args: %1: offset line.
%macro RENDER_BIT 1

        mov     di,SCROLL_OFFSET+160*(%1+1)-1   ;es:di points to video memory
        mov     cx,4                            ;times to loop
%%loop_print:
        lodsb                                   ;fetches byte from the cache
        mov     ah,al                           ;save value in ah for later use
        and     al,1100_0000b
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

        loop    %%loop_print
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
        and     al,0001_1110b                   ;turn off bit 0 in case it is one
        mov     si,text_writer_bitmap_to_video_tbl
        add     si,ax

        movsw                                   ;render MSB nibble (4 pixels, 2 bytes)

        mov     al,dl                           ;restore al, and process LSB nibble
        and     al,0000_1111b
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
; refreshes the palette. used as a macro (and not function) since it is being
; called from time-critical sections
;
; IN:
;       ds:si   -> table with the palette to update
;       cx      -> number of colors to update times 2, since it does 2 colors per h-line
;       bl      -> starting color + 0x10. example: use 0x1f for white: 0x10 + 0xf
;       bp      -> 0x3da
;
; Arg:  0       -> don't wait for horizontal retrace
;       1       -> wait fro horizontal retrace
%macro REFRESH_PALETTE 1
        mov     bh,0xde                         ;register is faster than memory

%%repeat:
        mov     dx,bp                           ;dx = 0x3da. select color register

%if %1
        %%wait:
                in      al,dx                   ;inline wait horizontal retrace for performance reasons
                test    al,dh                   ;FIXME: using 0x3 instead of 0x1. Might break with light pen
                jnz     %%wait
        %%retrace:
                in      al,dx
                test    al,dh                   ;FIXME: using 0x3 instead of 0x1. might break with light pen
                jz      %%retrace               ;horizontal retrace after this
%endif

        mov     al,bl                           ;starting color
        out     dx,al

        lodsw                                   ;load two colors values: al,ah

        mov     dl,bh                           ;dx = 0x3de
        out     dx,al                           ;update color

        inc     bl

        mov     dx,bp                           ;dx = 0x3da
        mov     al,bl                           ;starting color
        out     dx,al

        mov     dl,bh                           ;dx = 0x3de
        mov     al,ah                           ;2nd value
        out     dx,al                           ;update color

        inc     bl
        loop    %%repeat
%endmacro


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; CODE
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
                                                ; while setting the interrupt
        call    wait_vertical_retrace

        mov     cx,194                          ;and wait for scanlines
.repeat:
        call    wait_horiz_retrace
        loop    .repeat

        push    es
        sub     ax,ax
        mov     es,ax

        mov     ax,new_i08_simple
        mov     dx,cs
        xchg    ax,[es:8*4]                     ;new/old IRQ 8: offset
        xchg    dx,[es:8*4+2]                   ;new/old IRQ 8: segment
        mov     [old_i08],ax
        mov     [old_i08+2],dx

        pop     es

        mov     ax,PIT_DIVIDER                  ;Configure the PIT to
        call    setup_pit                       ;setup PIT

        in      al,0x21                         ;Read primary PIC Interrupt Mask Register
        mov     [old_pic_imr],al                ;Store it for later
        mov     al,1111_1100b                   ;Mask off everything except IRQ 0
        out     0x21,al                         ; and IRQ1 (timer and keyboard)

        sti
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_new_i08_multi_color_init
        cli                                     ;disable interrupts
                                                ; while setting the interrupt
        call    wait_vertical_retrace

        mov     cx,160                          ;and wait for scanlines
.repeat:
        call    wait_horiz_retrace
        loop    .repeat

        push    es
        sub     ax,ax
        mov     es,ax

        mov     ax,new_i08_bottom_multi_color
        mov     dx,cs
        mov     [es:8*4],ax                     ;new/old IRQ 8: offset
        mov     [es:8*4+2],dx                   ;new/old IRQ 8: segment

        pop     es

        mov     ax,PIT_DIVIDER                  ;Configure the PIT to
        call    setup_pit                       ;setup PIT

        sti
        jmp     state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_new_i08_single_color_init
        cli                                     ;disable interrupts
                                                ; while setting the interrupt
        call    wait_vertical_retrace

        mov     cx,170                          ;and wait for scanlines
.repeat:
        call    wait_horiz_retrace
        loop    .repeat

        push    es
        sub     ax,ax
        mov     es,ax

        mov     ax,new_i08_bottom_single_color
        mov     dx,cs
        mov     [es:8*4],ax                     ;new/old IRQ 8: offset
        mov     [es:8*4+2],dx                   ;new/old IRQ 8: segment

        pop     es

        mov     ax,PIT_DIVIDER                  ;Configure the PIT to
        call    setup_pit                       ;setup PIT

        sti
        jmp     state_next

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
irq_cleanup:
        cli

        mov     al,[old_pic_imr]                ;Get old PIC settings
        out     0x21,al                         ;Set primary PIC Interrupt Mask Register

        mov     ax,0                            ;Reset PIT to defaults (~18.2 Hz)
        call    setup_pit                       ; actually means 0x10000

        push    es
        les     si,[old_i08]

        push    ds
        xor     ax,ax
        mov     ds,ax
        mov     [8*4],si
        mov     [8*4+2],es                      ;Restore the old INT 08 vector
        pop     ds
        pop     es

        sti
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
setup_pit:
        ; AX = PIT clock period
        ;          (Divider to 1193180 Hz)
        push    ax
        mov     al,0x34
        out     0x43,al
        pop     ax
        out     0x40,al
        mov     al,ah
        out     0x40,al

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
;waits until the beam is about to return to the top-left
;should be the one to call for the effects
global wait_vertical_retrace
wait_vertical_retrace:
        mov     dx,0x03da
.wait_retrace_finish:
        in      al,dx                           ;wait for vertical retrace
        test    al,8                            ; to finish
        jnz     .wait_retrace_finish

.wait_retrace_start:
        in      al,dx                           ;wait for vertical retrace
        test    al,8                            ; to start
        jz      .wait_retrace_start

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
global wait_horiz_retrace
wait_horiz_retrace:
        mov     dx,0x3da
.wait_retrace_finish:                            ;wait for horizontal retrace start
        in      al,dx
        test    al,1
        jnz      .wait_retrace_finish

.wait_retrace_start:
        in      al,dx                           ;wait until start of the retrace
        test    al,1
        jz      .wait_retrace_start
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
        mov     byte [current_state],0
        call    [states_inits]                  ;init state 0

        call    music_init
        call    text_writer_init
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

        mov     ah,1
        int     0x16                            ;INT 16,AH=1, OUT:ZF=status
        jz      .loop

        mov     ah,0
        int     0x16

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
new_i08_simple:
        ;not saving any variable, since the code at main loop
        ;happens after the tick

        mov     ax,data
        mov     ds,ax

        ;update top-screen palette
        mov     si,top_palette                  ;points to colors used at the top of the screen
        mov     cx,8                            ;update 16 colors (7*2)
        mov     bl,0x10                         ; starting with color 0 (black)
        mov     bp,0x3da                        ;bp should be 0x3da
        REFRESH_PALETTE 1                       ;refresh the palette. don't wait for horizontal retrace

        jmp     new_i08_main

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
new_i08_bottom_multi_color:
        ;not saving any variable, since the code at main loop
        ;happens after the tick

        mov     ax,data
        mov     ds,ax

        ;update bottom-screen palette
        mov     bp,0x3da                        ;register address
        mov     si,bottom_palette               ;points to colors used at the bottom
        mov     cx,4                            ;only update 8 colors (4 * 2)
        mov     bl,0x11                         ; starting with color 1 (skip black)
        REFRESH_PALETTE 1                       ;refresh the palette, wait for horizontal retrace


        ;wait a few raster lines
        ;FIXME: could be used to place logic code.
        mov     dx,bp
        mov     cx,BOTTOM_TOP_LINES_TO_WAIT     ;total number of raster bars
.l0:
        lodsb                                   ;fetch color
        mov     ah,al                           ; and save it for later
.wait:
        in      al,dx                           ;inline wait horizontal retrace for performance reasons
        test    al,dh                           ;FIXME: using 0x3 instead of 0x1. Might break with light pen
        jnz     .wait
.retrace:
        in      al,dx
        test    al,dh                           ;FIXME: using 0x3 instead of 0x1. Might break with light pen
        jz      .retrace                        ;horizontal retrace after this

        loop    .l0                             ;and do it 17 times


        ;update top-screen palette
        mov     si,top_palette                  ;points to colors used at the top of the screen
        mov     cx,7                            ;update 14 colors (7*2)
        mov     bl,0x10                         ; starting with color 0 (black)
        REFRESH_PALETTE 1                       ;refresh the palette. don't wait for horizontal retrace

        jmp     new_i08_main

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
new_i08_bottom_single_color:
        ;not saving any variable, since the code at main loop
        ;happens after the tick

        mov     ax,data
        mov     ds,ax

        ;update bottom-screen palette
        mov     bp,0x3da                        ;register address
        mov     si,bottom_palette               ;points to colors used at the bottom
        mov     cx,4                            ;only update 8 colors (4 * 2)
        mov     bl,0x11                         ; starting with color 1 (skip black)
        REFRESH_PALETTE 1                       ;refresh the palette, wait for horizontal retrace

        mov     cx,RASTER_COLORS_MAX            ;total number of raster bars
        mov     si,raster_colors_tbl            ;where the colors are for each raster bar
        mov     dx,0x03da
        mov     al,0x1f                         ;select palette color 15 (white)
        out     dx,al

        ;BEGIN raster bar code
        ;should be done as fast as possible
        mov     bx,0xdade                       ;used for 3da / 3de. faster than add / sub 4
.l0:
        lodsb                                   ;fetch color
        mov     ah,al                           ; and save it for later
.wait:
        in      al,dx                           ;inline wait horizontal retrace for performance reasons
        test    al,dh                           ;FIXME: using 0x3 instead of 0x1. Might break with light pen
        jnz     .wait
.retrace:
        in      al,dx
        test    al,dh                           ;FIXME: using 0x3 instead of 0x1. Might break with light pen
        jz      .retrace                        ;horizontal retrace after this

        mov     dl,bl                           ;dx = 0x3de
        mov     al,ah
        out     dx,al                           ;set new color

        mov     dl,bh                           ;dx = 0x3da
        loop    .l0                             ;and do it 17 times
        ;END raster bar code


        ;update top-screen palette
        mov     si,top_palette                  ;points to colors used at the top of the screen
        mov     cx,7                            ;update 14 colors (7*2)
        mov     bl,0x10                         ; starting with color 0 (black)
        mov     bp,dx                           ;bp should be 0x3da
        REFRESH_PALETTE 1                       ;refresh the palette. don't wait for horizontal retrace

new_i08_main:

%if DEBUG
        call    inc_d020
%endif

        ;after raster baster finishes

        sub     bh,bh
        mov     bl,byte [current_state]         ;fetch state
        shl     bx,1                            ; and convert it into offset (2 bytes per offset)
        call    [states_callbacks+bx]           ; and call correct state callback

        call    crtc_addr_anim                  ;change CRTC start address
        call    music_anim                      ;play music
        call    noise_fade_anim                 ;outline fade anim
        call    text_writer_anim                ;text writer
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
        mov     si,volume_0
        mov     cx,4
.repeat:
        lodsb
        out     0xc0,al
        loop    .repeat

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_next:
        inc     byte [current_state]
        sub     bh,bh
        mov     bl,byte [current_state]
        shl     bx,1
        call    [states_inits+bx]
        ret

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
        cmp     word [palette_black_delay],0
        je      .animate
        dec     word [palette_black_delay]
        ret

.animate:
        mov     word [palette_black_delay],3    ;reset delay
        mov     bx,word [palette_black_idx]     ;fetch idx to table
        sub     ah,ah                           ;MSB for the index. used later

        mov     cx,PALETTE_COLORS_TO_BLACK_MAX
        mov     si,palette_colors_to_black      ;pointer to colors to update
.loop:
        lodsb                                   ;fetch color to update
        mov     di,ax                           ;save it in di
        mov     al,[palette_black_tbl+bx]       ;new color for the color

        mov     [top_palette+di],al             ;update color in color table
        loop    .loop                           ; and loop

        mov     [bottom_palette+0],al       ;update black for bottom part as well

        mov     al,2                            ;select border color register
        out     dx,al
        mov     dl,0xde                         ;dx=0x03de
        mov     al,[palette_black_tbl+bx]
        out     dx,al                           ;update color

        inc     word [palette_black_idx]
        cmp     word [palette_black_idx], PALETTE_BLACK_MAX
        jnz     .end

        call    state_next
.end:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_500ms_init:
        mov     word [delay_frames],30          ;wait 30 cycles. half second
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_2s_init:
        mov     word [delay_frames],60*2        ;wait 2 seconds before showing logo
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_5s_init:
        mov     word [delay_frames],60*5        ;wait 5 seconds before showing logo
        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_delay_anim:
        cmp     word [delay_frames],0
        je      .next
        dec     word [delay_frames]
        ret
.next:
        jmp     state_next                      ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_gfx_fade_in_init:
        mov     word [clemente_lfsr_current_state],LFSR_START_STATE

        ;logo should be turned off by default
        mov     cx,PALETTE_COLORS_TO_BLACK_MAX
        sub     bh,bh                           ;MSB for bx. used later
        mov     si,palette_colors_to_black
        mov     dl,1                            ;color blue
.loop:
        lodsb                                   ;color to fade
        mov     bl,al                           ;convert it to index value
        mov     byte [top_palette+bx],dl        ;update color index with blue

        loop    .loop

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Uses Linear Feedback Shift Register 15-bit
; https://en.wikipedia.org/wiki/Linear-feedback_shift_register
state_gfx_fade_in_anim:

        push    ds

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

        pop     ds
        mov     [clemente_lfsr_current_state],ax
        ret

.end:
        pop     ds
        jmp     state_next                      ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_pvm_logo_fade_in_init:
        mov     word [palette_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_pvm_logo_fade_in_anim:
        mov     bx,word [palette_idx]
        cmp     bx,PALETTE_PVM_LOGO_FADE_MAX
        je      .end

        ;letter P
        mov     al,[palette_pvm_logo_fade_tbl+bx]       ;fetch color value
        mov     [top_palette+2],al              ;update color index 2


        ;letter V
        sub     bx,8                            ;offset -8
        mov     al,[palette_pvm_logo_fade_tbl+bx]       ;fetch color value
        mov     [top_palette+0xa],al            ;update color index 0xa

        ;letter M
        sub     bx,8                            ;offset -8
        mov     al,[palette_pvm_logo_fade_tbl+bx]       ;fetch color value
        mov     [top_palette+0xd],al            ;update color index 0xd

        inc     word [palette_idx]              ;update palette offset
        ret
.end:
        jmp     state_next                      ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_nothing_init:
        ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_nothing_anim:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_skip_anim:
        jmp     state_next                      ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_outline_fade_init:
        mov     word [palette_outline_fade_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_outline_fade_in_anim:
        mov     bx,word [palette_outline_fade_idx]
        cmp     bx,PALETTE_OUTLINE_FADE_IN_MAX
        je      .end

        mov     dx,0x03da                       ;select border color register
        mov     al,0x15                         ;logo outline color: 5
        out     dx,al                           ;select palette register

        mov     dl,0xde                         ;dx=0x03de
        mov     al,[palette_outline_fade_in_tbl+bx]
        out     dx,al

        inc     word [palette_outline_fade_idx]
        ret
.end:
        jmp     state_next                      ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_outline_fade_out_anim:
        mov     bx,word [palette_outline_fade_idx]
        cmp     bx,PALETTE_OUTLINE_FADE_OUT_MAX
        je      .end

        mov     dx,0x03da                       ;select border color register
        mov     al,0x15                         ;logo outline color: 5
        out     dx,al                           ;select palette register

        mov     dl,0xde                         ;dx=0x03de
        mov     al,[palette_outline_fade_out_tbl + bx]
        out     dx,al

        inc     word [palette_outline_fade_idx]
        ret
.end:
        jmp     state_next                      ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_outline_fade_to_final_anim:
        mov     bx,word [palette_outline_fade_idx]
        cmp     bx,PALETTE_OUTLINE_FADE_TO_FINAL_MAX
        je      .end

        mov     dx,0x03da                       ;select border color register
        mov     al,0x15                         ;logo outline color: 5
        out     dx,al                           ;select palette register

        mov     dl,0xde                         ;dx=0x03de
        mov     al,[palette_outline_fade_to_final_tbl+bx]
        out     dx,al

        inc     word [palette_outline_fade_idx]
        ret
.end:
        jmp     state_next                      ;set next state and return

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_init:
        mov     byte [scroll_state],SCROLL_STATE_WAIT   ;nothing by default
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_anim:
        mov     al,byte [scroll_state]
        cmp     al,SCROLL_STATE_SCROLL       ;scroll enabled?
        je      .do_scroll
        cmp     al,SCROLL_STATE_PLASMA
        je      .do_plasma
        ret

.do_plasma:
        jmp     plasma_anim

.do_scroll:
        mov     ax,0xb800                       ;ds points to video memory
        mov     ds,ax                           ;es already points to it

        mov     cx,320                          ;scroll 4 lines of 80 chars
        mov     si,SCROLL_OFFSET+1              ;source: last char of screen
        mov     di,SCROLL_OFFSET                ;dest: last char of screen - 1
        rep movsw                               ;do the copy

        mov     cx,320                          ;scroll 4 lines of 80 chars
        mov     si,SCROLL_OFFSET+8192+1         ;source: last char of screen
        mov     di,SCROLL_OFFSET+8192           ;dest: last char of screen - 1
        rep movsw                               ;do the copy

        mov     cx,320                          ;scroll 4 lines of 80 chars
        mov     si,SCROLL_OFFSET+16384+1        ;source: last char of screen
        mov     di,SCROLL_OFFSET+16384          ;dest: last char of screen - 1
        rep movsw                               ;do the copy

        mov     cx,320                          ;scroll 4 lines of 80 chars
        mov     si,SCROLL_OFFSET+24576+1        ;source: last char of screen
        mov     di,SCROLL_OFFSET+24576          ;dest: last char of screen - 1
        rep movsw                               ;do the copy

        mov     ax,data                         ;restore ds. points to data
        mov     ds,ax

        cmp     byte [scroll_bit_idx],0         ;only update cache if scroll_bit_idx == 0
        jnz     .render_bits                    ; and scroll_col_used == 0
        cmp     byte [scroll_col_used],0
        jnz     .render_bits

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

        ;fall-through

.render_bits:
        mov     ax,0xb800
        mov     es,ax

        mov     di,SCROLL_OFFSET+159            ;es:di points to video memory
        mov     si,cache_charset                ;ds:si points to cache_charset
        sub     bp,bp                           ;used for the cache index in the macros
        mov     dx,scroll_pixel_color_tbl       ;used in the macros

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
        push    ds                              ;copy 2nd-col chars to cache
        pop     es                              ;es = data
        mov     si,cache_charset+16             ;pointer to 2nd-col chars
        mov     di,cache_charset                ;pointer to 1st-col chars
        mov     cx,8
        rep movsw                               ;copy 16 bytes
        inc     byte [scroll_col_used]          ;2nd column to be used
        mov     byte [scroll_bit_idx],cl        ;0
        jmp     .end

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
        mov     ax,0xb800                       ;restore es to video memory
        mov     es,ax
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_init:
        mov     word [pvm_offset],pvm_song + 0x10       ;update start offset
        sub     al,al
        mov     byte [pvm_wait],al              ;don't wait at start
        mov     byte [noise_triggered],al       ;noise not playing
        mov     byte [noise_fade_idx],al
        mov     byte [noise_fade_enabled],al
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_anim:

DATA    equ     0000_0000b
DATA_EXTRA equ  0010_0000b
DELAY   equ     0100_0000b
DELAY_EXTRA equ 0110_0000b
END     equ     1000_0000b

        sub     cx,cx                           ;cx=0... needed later
        mov     si,[pvm_offset]

        cmp     byte [pvm_wait],0
        je      .l0

        dec     byte [pvm_wait]
        ret

.l0:
        lodsb                                   ;fetch command byte
        mov     ah,al
        and     al,1110_0000b                   ;al=command only
        and     ah,0001_1111b                   ;ah=command args only

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
        and     al,1111_0000b                   ;is noise?
        cmp     al,1110_0000b
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
text_writer_init:
        sub     ax,ax
        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR
        mov     word [text_writer_idx],-1       ;HACK: data offset is -1, because we do a +1 at anim
        mov     byte [text_writer_delay],10     ;delay waits 10 refreshes
        mov     byte [text_writer_enabled],al   ;disabled by default
        mov     byte [text_writer_cursor_blink_delay],al ;how many blinks to wait
        mov     byte [text_writer_x_pos],al     ;at pos 0
        mov     byte [text_writer_x_dst],al     ;dst pos 0
        mov     byte [text_writer_y_pos],TEXT_WRITER_START_Y
        mov     byte [text_writer_y_dst],TEXT_WRITER_START_Y
        mov     word [text_writer_addr],TEXT_WRITER_START_Y*2*160+160   ;half line ahead
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_anim:
        cmp     byte [text_writer_enabled],0    ;enabled?
        jne     .ok                             ; if not,return
        ret
.ok:
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
        sub     bx,bx                           ;new offset is 0
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

        mov     byte [text_writer_delay],1      ;cycles to wait
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
        cmp     byte [text_writer_delay],0
        je      .end_delay
        dec     byte [text_writer_delay]
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
        mov     al,0x00
        call    text_writer_fill_one_char
        inc     byte [text_writer_y_pos]        ;cursor.y += 1 (going down)
        add     word [text_writer_addr],320     ;video addr += 320
        mov     al,0x77
        jmp     text_writer_fill_one_char

.up:
        mov     al,0x00
        call    text_writer_fill_one_char
        dec     byte [text_writer_y_pos]        ;cursor.y -= 1. (going up)
        sub     word [text_writer_addr],320     ;video addr -= 320
        mov     al,0x77
        jmp     text_writer_fill_one_char


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_call_action_init:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; fetch the next byte from the stream, and use it as an
; index to the action_call table, and call that action function
text_writer_state_call_action_anim:
        inc     word [text_writer_idx]          ;offset + 1
        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR    ;print char is next state
        mov     bx,word [text_writer_idx]       ;FIXME: do something with bx
        jmp     state_enable_noise_fade_init

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
        cmp     byte [text_writer_cursor_blink_delay],0
        je      .end_blink_state

        dec     byte [text_writer_delay]
        cmp     byte [text_writer_delay],15     ;half cycles
        je      .cursor_off
        cmp     byte [text_writer_delay],0
        je      .next_blink
        ret

.next_blink:
        dec     byte [text_writer_cursor_blink_delay]
        cmp     byte [text_writer_cursor_blink_delay],0
        je      .end_blink_state
        mov     byte [text_writer_delay],30     ;init blink delay
        mov     al,0x77                         ;select all gray char
        jmp     text_writer_fill_one_char       ; print it, and return

.cursor_off:
        mov     al,0x00                         ;select all black char
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

        mov     dx,0x3d4
        mov     al,0xc                          ;select CRTC start address hi
        out     dx,al

        inc     dx                              ;set value for CRTC hi address
        mov     al,bh
        out     dx,al

        dec     dx
        mov     al,0xd
        out     dx,al                           ;select CRTC start address lo

        inc     dx
        mov     al,bl
        out     dx,al                           ;set value for CRTC lo address

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_enable_text_writer:
        mov     byte [text_writer_enabled],1
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_enable_noise_fade_init:
        inc     byte [noise_fade_enabled]
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_enable_scroll:
        mov     byte [scroll_state],SCROLL_STATE_SCROLL ;start the scroll

        mov     bx,es                           ;save es for later
        mov     ax,ds
        mov     es,ax

        mov     cx,8                            ;16 colors (16 bytes == 8 words)
        mov     di,bottom_palette               ;destination: bottom palette
        mov     si,palette_default              ;source: default palette
        rep movsw                               ;copy the new 16 colors

        mov     es,bx                           ;restore es
        ret

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
        mov     ax,0x0000                       ;black * 4

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

        jmp     state_next                      ;finish and set next state


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_tex_init_sine_table:
        ; x
        mov     bp,255                          ;register is faster than memory
        sub     si,si
        sub     bx,bx
        mov     dh,[plasma_tex_inc_x0]
        sub     cx,cx
        mov     cl,[plasma_tex_inc_x1]

        %assign XX 0
        %rep    PLASMA_TEX_WIDTH
                mov     al,[fn_table_1+bx]      ;xbuf[idx] = sine[bx]+sine[si]
                add     al,[fn_table_2+si]
                mov     [plasma_tex_xbuf+XX],al
                add     bl,dh                   ;dh=5. update offsets to sine tables
                add     si,cx                   ;cx=8
                and     si,bp                   ;only use LSB part of si
        %assign XX XX+1
        %endrep

        ; y
        sub     si,si
        sub     bx,bx
        mov     dh,[plasma_tex_inc_y0]
        sub     cx,cx
        mov     cl,[plasma_tex_inc_y1]

        %assign YY 0
        %rep    PLASMA_TEX_HEIGHT
                mov     al,[fn_table_1+bx]      ;ybuff[YY] = sine[bx]+sine[si]
                add     al,[fn_table_2+si]
                mov     [plasma_tex_ybuf+YY],al ;update y buffer with sine+sine
                add     bl,dh                   ;dh=3
                add     si,cx                   ;cx=5
                and     si,bp                   ;bp=255. update offets, and use only LSB part of si
        %assign YY YY+1
        %endrep

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_plasma_red_tex_init:
        sub     ax,ax                           ;faster to use register than value
        mov     word [plasma_tex_x_offset],ax
        mov     byte [plasma_tex_state],al      ;initial state
        mov     byte [plasma_tex_colors_updated],al
        mov     byte [plasma_tex_delay],al
        mov     word [plasma_tex_palette_addr],plasma_tex_red_palette
        mov     byte [plasma_tex_letter_color],2;letter P uses color 2
        mov     byte [plasma_tex_inc_x0],250     ;texture generator parameters
        mov     byte [plasma_tex_inc_x1],255
        mov     byte [plasma_tex_inc_y0],127
        mov     byte [plasma_tex_inc_y1],129

        mov     bx,es                           ;save es in bx for later

        mov     ax,data
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
        mov     byte [plasma_tex_delay],al
        mov     word [plasma_tex_palette_addr],plasma_tex_green_palette
        mov     byte [plasma_tex_letter_color],0xa      ;letter V uses color 0xa
        mov     byte [plasma_tex_inc_x0],4      ;texture generator parameters
        mov     byte [plasma_tex_inc_x1],7
        mov     byte [plasma_tex_inc_y0],129
        mov     byte [plasma_tex_inc_y1],3

        mov     bx,es                           ;save es in bx for later

        mov     ax,data
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
        mov     byte [plasma_tex_delay],al
        mov     word [plasma_tex_palette_addr],plasma_tex_magenta_palette
        mov     byte [plasma_tex_letter_color],0xd        ;letter M uses color 0xd
        mov     byte [plasma_tex_inc_x0],4      ;texture generator parameters
        mov     byte [plasma_tex_inc_x1],3
        mov     byte [plasma_tex_inc_y0],127
        mov     byte [plasma_tex_inc_y1],132

        mov     bx,es                           ;save es in bx for later

        mov     ax,data
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

        cmp     byte [plasma_tex_delay],0
        je      .do_out
        dec     byte [plasma_tex_delay]
        ret

.do_out:
        mov     byte [plasma_tex_delay],3       ;reset delay

        mov     bx,es                           ;save it for later
        mov     ax,data
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
        jmp     state_next


.palette_fade_in:
        ;shift left the colors one by one
        ;until all of them are in the final position
        cmp     byte [plasma_tex_colors_updated],PLASMA_TEX_PALETTE_MAX
        je      .next_internal_state

        cmp     byte [plasma_tex_delay],0       ;end of wait?
        je      .do_in                          ; if so, jump to the effect
        dec     byte [plasma_tex_delay]         ;delay--
        ret

.do_in:
        mov     byte [plasma_tex_delay],3       ;reset delay

        mov     bx,es                           ;save it for later
        mov     ax,data
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
        mov     byte [plasma_tex_delay],3       ;delay before starting next state
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
        not     di                              ;

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
scroll_control_code_color_white:
        mov     al,[scroll_pixel_white_tbl+1]   ;skip first byte. it is black in all conf
        mov     bx,[scroll_pixel_white_tbl+2]
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
scroll_control_code_plasma_init:
        sub     ax,ax

        mov     [plasma_counter],ax             ;ticks at 0

        ;update palette
        mov     ax,data
        mov     es,ax                           ;es=ds
        mov     cx,PLASMA_TEX_PALETTE_MAX       ;number of colors to update
        mov     di,bottom_palette+1             ;destination: bottom palette, skipping black
        mov     si,plasma_tex_blue_palette      ;source: blue palette
        rep movsb                               ;copy the new palette
        mov     ax,0xb800
        mov     es,ax                           ;restore es

        mov     byte [scroll_state],SCROLL_STATE_PLASMA
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_anim:
        inc     word [plasma_counter]
        cmp     word [plasma_counter],5*20     ;20 seconds
        jne     .do_plasma

        mov     byte [scroll_state],SCROLL_STATE_SCROLL ;enable scroll again
        ret

.do_plasma:
        call    plasma_update_sine_table
        jmp     plasma_render_to_video

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_update_sine_table:
        ; x
        sub     bh,bh                           ;bx MSB = 0
        mov     ah,bh                           ;ax MSB = 0
        mov     cx,word [plasma_off_x0]       ;fetches both xbuf_1 and xbuf_2
        mov     bl,cl                           ;bx = xbuf_1
        mov     al,ch                           ;ax = xbuf_2
        mov     si,ax                           ;si = xbuf_2

        push    cx                              ;save cx for later
        mov     dh,5                            ;register is faster than memory
        mov     cx,8                            ;register is faster than memory
        mov     bp,255                          ;register is faster than memory

        %assign XX 0
        %rep    PLASMA_WIDTH
                mov     al,[fn_table_1+bx]      ;xbuf[idx] = sine[bx]+sine[si]
                add     al,[fn_table_2+si]
                mov     [plasma_xbuf+XX],al
                add     bl,dh                   ;update offsets to sine tables
                sub     si,cx
                and     si,bp                   ;bp=255. only use LSB part of si
        %assign XX XX+1
        %endrep

        pop cx                                  ;restore cx
        add     cl,9                            ;update buffer x1 and x2 offsets
        sub     ch,5                            ; for the next frame
        mov     word [plasma_off_x0],cx       ;udpates both xbuf_1 and xbuf_2

        ; y
        sub     bh,bh                           ;bx MSB = 0
        mov     ah,bh                           ;ax MSB = 0
        mov     cx,word [plasma_off_y0]       ;fetches both ybuf_1 and ybuf_2
        mov     bl,cl                           ;bx = xbuf_1
        mov     al,ch                           ;ax = xbuf_2
        mov     si,ax                           ;si = xbuf_2

        push    cx                              ;save cx for later
        mov     dh,3                            ;register is faster than memory
        mov     cx,5

        %assign YY 0
        %rep    PLASMA_HEIGHT
                mov     al,[fn_table_1+bx]      ;ybuff[YY] = sine[bx]+sine[si]
                add     al,[fn_table_2+si]
                mov     [plasma_ybuf+YY],al     ;update y buffer with sine+sine
                add     bl,dh
                sub     si,cx
                and     si,bp                   ;bp=255. update offets, and use only LSB part of si
        %assign YY YY+1
        %endrep

        pop     cx                              ;restore cx
        add     cl,7                            ;update sine values for later
        sub     ch,3
        mov     word [plasma_off_y0],cx       ;udpates both ybuf_1 and ybuf_2

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_render_to_video:
        mov     bx,luminances_6_colors          ;to be used by xlat. has the colors for the plasma

        %assign YY 0
        %rep    PLASMA_HEIGHT

                %assign XX 0
                %rep    PLASMA_WIDTH
                        mov     al,[plasma_xbuf+XX]     ;fetch plasma X buffer
                        add     al,[plasma_ybuf+YY]     ; and add it to plasma Y buffer
                        xlat                            ; and get the color value from luminances_tble
                        mov     [es:PLASMA_OFFSET+(YY/4)*160+(YY % 4)*8192+XX],al
                %assign XX XX+1
                %endrep
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
noise_fade_anim:
        cmp     byte [noise_fade_enabled],0
        je      .exit

        cmp     byte [noise_triggered],0
        je      .skip
        mov     byte [noise_fade_idx],0         ;if triggered, reset anim
.skip:
        cmp     byte [noise_fade_idx],NOISE_FADE_MAX    ;end of anim?
        je      .exit

        sub     bh,bh
        mov     bl,[noise_fade_idx]             ;bx with idex to table

        mov     dx,0x03da
        mov     al,0x15                         ;color 5 is outline color
        out     dx,al

        mov     dl,0xde                         ;dx=0x03de
        mov     al,[noise_fade_tbl+bx]          ;fetch color
        out     dx,al

        inc     byte [noise_fade_idx]
.exit:
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
; DATA GFX
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .gfx data
        incbin 'src/logo.raw'                   ;MUST be the first variable in the segment

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA MUSIC + CHARSET + MISC
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data data

pvm_song:
        incbin "src/uctumi-song.pvm"

pvm_wait:                                       ;cycles to read diviced 0x2df
        db 0
pvm_offset:                                     ;pointer to next byte to read
        dw 0

noise_fade_enabled:                             ;effect is enabled?
        db 0
noise_triggered:
        db 0                                    ;boolen. whether noise is playing
noise_fade_idx:                                 ;index to table
        db 0
noise_fade_tbl:
        db      8,7,11,8,1
NOISE_FADE_MAX equ $-noise_fade_tbl


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

palette_colors_to_black:                        ;colors that should turn black
        db      0x00,0x05,0x02,0x0a,0x0d,0x03   ;black, outline, P, V, M, inner
PALETTE_COLORS_TO_BLACK_MAX equ $-palette_colors_to_black

delay_frames:
        dw      0                               ;frames to wait before doing something

global c64_charset
c64_charset:
        incbin 'src/c64_charset-charset.bin'

charset:
        incbin 'src/font_unknown_2x2-charset.bin'

cache_charset:
        resb    32                              ;the 32 bytes to print in the current frame
                                                ; char aligned like: top-left, bottom-left,
                                                ; top-right, bottom-right
scroll_control_code_tbl:
        dw      scroll_control_code_color_white
        dw      scroll_control_code_color_anim
        dw      scroll_control_code_plasma_init
        ; # = tm
        ; % = closing double quotes
        ; $ = smiley
        ; ; = ~ (kind of separator)
        ; < = heart
        ; bit 7=on (128) - control code
        ;       128 = color white
        ;       129 = color anim
        ;       130 = plasma init
scroll_text:
;        db 128                                  ;color white
;        db 'HI THERE. '
;        db 129,'P'
;        db 128,'UNGAS DE '
;        db 129,'V'
;        db 128,'ILLA '
;        db 129,'M'
;        db 128,'ARTELLI HERE, WITH OUR FIRST TANDY RELEASE. '
        db 129
        db 'IT ALL BEGAN WHEN WE WENT TO PICK UP A COMMODORE 64 BUNDLE '
        db 'AND THE SELLER INCLUDED TWO TANDY 1000 HX IN IT. '
        db 'WTF IS A TANDY 1000 HX? WE GOOGLED IT, AND WE LIKED IT. '
        db `HEY, IT HAS AN 8088 (WE DON'T NEED NO FANCY 386), SOME NON-`
        db 'STANDARD VIDEO MODES, A BETTER-THAN-SPEAKER SOUNDCARD, AND JOYSTICK PORTS '
        db '(UNIJOYSTICLE COMMING SOON#). '
        db 'AND HERE WE ARE, WITH OUR FIRST TANDY 1000 RELEASE. WE CALL IT '
        db `"TANDY 64%... GOT IT ? & . `
        db '   ;    '
        db 'SENDING OUR REGARDS TO ALL THE TANDY 1000 SCENE, STARTING WITH: '
        db '                      '
        db 130                                  ;start plasma
        db 'NO TANDY 1000 SCENE ??? HOW DARE YOU !!! '
        db '   ;   '
        db 'BIG THANKS TO DEMOSPLASH FOR GOING THE EXTRA MILE, AND ADDING TANDY 1000 TO THE LIST OF SUPPORTED SYSTEMS!!! '
        db 27,28,29,30,31,42,43                 ;Radio Shack (using Radio Shack font)
        db ' DESERVES IT ! '
        db '   ;   '
        db `BESO GRANDE A LA MAS GRANDE DE TODAS: <<< LIA CRUCET <<< LA MEJOR CANTANTE DE TODOS LOS TIEMPOS`
        db '   ;   '
        db 'CODE:RIQ, MUSIC: UCTUMI, GRAPHICS: ALAKRAN'
        db '   ;   '
SCROLL_TEXT_LEN equ $-scroll_text

scroll_char_idx:                                ;pointer to the next char
        dw 0
scroll_bit_idx:                                 ;pointer to the next bit in the char
        db 0
scroll_col_used:
        db 0                                    ;chars are 2x2. col indicates which col is being used

scroll_pixel_color_tbl:                         ;the colors for the scroll letters
        resb    4                               ; it contains a copy of one of the tables
                                                ; below
scroll_pixel_white_tbl:
        db      0x00                            ;00 - black/black
        db      0x08                            ;01 - black/white
        db      0x80                            ;10 - white/black
        db      0x88                            ;11 - white/white
scroll_pixel_anim_tbl:
        db      0x00                            ;00 - black/black
        db      0x0f                            ;01 - black/white
        db      0xf0                            ;10 - white/black
        db      0xff                            ;11 - white/white

SCROLL_STATE_WAIT       equ 0                   ;wait
SCROLL_STATE_SCROLL     equ 1                   ;do the scroll
SCROLL_STATE_PLASMA     equ 2                   ;do the plasma instead of the scroll
scroll_state:                                   ;scroll state machine
        db      0

palette_outline_fade_idx:                       ;index for table used in outline fade effect
        dw      0
palette_outline_fade_in_tbl:                   ;fade_out
        db      8,8,7,7,15,15,15,15
PALETTE_OUTLINE_FADE_IN_MAX equ $-palette_outline_fade_in_tbl

palette_outline_fade_out_tbl:                   ;fade_out
        db      7,7,8,8,0
PALETTE_OUTLINE_FADE_OUT_MAX equ $-palette_outline_fade_out_tbl

palette_outline_fade_to_final_tbl:              ;fade in until gets final color
        db      8,8,7,7,11,11,11,11,7,7,8,8,1
PALETTE_OUTLINE_FADE_TO_FINAL_MAX equ $-palette_outline_fade_to_final_tbl

palette_idx:
        dw      0

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


current_state:                                  ;current state. index for the
        db      0                               ; function to call.

states_inits:
        dw      state_gfx_fade_in_init          ;a
        dw      state_fade_to_black_init        ;b
        dw      state_delay_2s_init             ;c
        dw      state_new_i08_multi_color_init  ;i
;        dw      state_outline_fade_init        ;d
;        dw      state_outline_fade_init        ;e
        dw      state_plasma_red_tex_init       ;f
        dw      state_plasma_green_tex_init     ;g
        dw      state_plasma_magenta_tex_init   ;h
        dw      state_clear_bottom_init         ;l
        dw      state_new_i08_single_color_init ;i
        dw      state_pvm_logo_fade_in_init     ;j
        dw      state_outline_fade_init         ;k
;        dw      state_plasma_init              ;m
        dw      state_clear_bottom_init         ;n
        dw      state_enable_scroll             ;o
        dw      state_delay_2s_init             ;i'
        dw      state_enable_text_writer        ;i
        dw      state_nothing_init              ;p

states_callbacks:
        dw      state_gfx_fade_in_anim          ;a
        dw      state_fade_to_black_anim        ;b
        dw      state_delay_anim                ;c
        dw      state_skip_anim                 ;i
;        dw      state_outline_fade_in_anim     ;d
;        dw      state_outline_fade_out_anim    ;e
        dw      state_plasma_tex_anim           ;f
        dw      state_plasma_tex_anim           ;g
        dw      state_plasma_tex_anim           ;h
        dw      state_clear_bottom_anim         ;l
        dw      state_skip_anim                 ;i
        dw      state_pvm_logo_fade_in_anim     ;j
        dw      state_outline_fade_to_final_anim;k
;        dw      state_plasma_anim              ;m
        dw      state_clear_bottom_anim         ;n
        dw      state_skip_anim                 ;o
        dw      state_delay_anim                ;i'
        dw      state_skip_anim                 ;i
        dw      state_nothing_anim              ;p

text_writer_addr:                               ;address where the char will be written
        resw    1
text_writer_x_pos:                              ;position x for the cursor. 0-39
        resb    1                               ; but supports in the range of -127,128
text_writer_x_dst:                              ;dst position x for the cursor. 0-39
        resb    1
text_writer_y_pos:                              ;position y for the cursor. 0-24
        resb    1                               ; but supports in the range of -127,128
text_writer_y_dst:                              ;dst position 0 for the cursor. 0-24
        resb    1

text_writer_enabled:
        db      0                               ;boolean: whether the text_writer anim is enabled

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
        db      TW_STATE_GOTO_X,39
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      TW_STATE_GOTO_Y,15
        db      TW_STATE_GOTO_X,1
        db      TW_STATE_GOTO_Y,19
        db      TW_STATE_GOTO_X,0
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      '                Hi there'
        db      TW_STATE_CURSOR_BLINK,5         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos

        db      'Never saw a plasma effect so small ?'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_CALL_ACTION,0          ;execute action 0: enable rhythm
        db      TW_STATE_GOTO_X,0               ;go to pos

        db      'Ha ha, we neither'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos

        db      'At least it runs at 60 FPS'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos

        db      'And it has raster bars!'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos

        db      'And it can cycle colors'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos

        db      `And we run out of time`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      'Tip: run this intro in real hardware'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      'in particular, in a Tandy 1000 HX'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      'Tested with 256K RAM and DOS v2.0'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      'Should work with 128K too'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      'Might not work correctly on emulators'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `Or if run on non-8088 Tandy's`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `Works both in RGB and composite modes`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `But better on RGB mode (better colors)`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `We tried our best to support DosBox-x`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `But some glitches appear every now and then`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `Do you have a IBM PC Jr. ?`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `Do you mind testing it there ?`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `We know it will run rather slow`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5

        db      `but we want to know how slow it runs`
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO_X,0               ;go to pos 5
TEXT_WRITER_DATA_LEN equ $-text_writer_data

text_writer_cursor_blink_delay:                 ;how many cursor blinks to wait
        db      0
text_writer_delay:
        db      0                               ;used by 'delay state' to know who many
                                                ;vert retrace to wait

tick:                                           ;to trigger once the irq was called
        db      0
old_i08:                                        ;segment + offset to old int 8
        dd      0
old_pic_imr:                                    ;PIC IMR original value
        db      0

raster_colors_tbl:                              ;used for the raster bars, at the
        db      15,7,9,8,1,0                    ; bottom of the screen
        db      0,1,8,9,7,15
        db      15,7,9,8
raster_color_restore:                           ;must be after raster_colors_tbl
        db      15
RASTER_COLORS_MAX equ $-raster_colors_tbl
BOTTOM_TOP_LINES_TO_WAIT equ 32

top_palette:                                    ;palette used for the upper part of the screen
        db      0,1,2,3,4,5,6,7                 ;default palette
        db      8,9,10,11,12,13,14,15

bottom_palette:                                 ;palette used for the bottom part of the screen
        db      2,1,2,3,4,5,6,7                 ;default palette
        db      8,9,10,11,12,13,14,15

palette_default:
        db      0,1,2,3,4,5,6,7                 ;default palette
        db      8,9,10,11,12,13,14,15

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

plasma_tex_state:                               ;internal state used inside the state_plasma_tex state
        db      0                               ;0=render texture, 1=fade in palette
plasma_tex_colors_updated:                      ;number of palette colors that were updated
        db      0
plasma_tex_delay:                               ;refreshes to wait for the palette update
        db      0
plasma_tex_palette_addr:                        ;palette address to use
        resw    1
plasma_tex_letter_color:                        ;color of the PVM letter to update
        resb    1
plasma_tex_x_offset:                            ;plama x offset. to make it scroll
        dw      0
plasma_tex_inc_x0:                              ;plasma xbuf increment a
        resb    1
plasma_tex_inc_x1:                              ;plasma xbuf increment b
        resb    1
plasma_tex_inc_y0:                              ;plasma ybuf increment a
        resb    1
plasma_tex_inc_y1:                              ;plasma ybuf incrment b
        resb    1

cycle_palette_delay:                            ;delay for the palette cycle animation
        db      1

clear_bottom_state:                             ;used by state_clearn_bottom_anim
        db      0                               ;when 0, clear first half. when 1, clear second half

crtc_start_addr:
        dw      0                               ;crtc start address

plasma_off_x0:                                  ;plasma: xbuf idx 1
        db      0
plasma_off_x1:                                  ;plasma: xbuf idx 2
        db      0
plasma_off_y0:                                  ;plasma: ybuf idx 1
        db      0
plasma_off_y1:                                  ;plasma: ybuf idx 2
        db      0
plasma_tex_xbuf:                                ;plasma tex xbuffer
        resb   PLASMA_TEX_WIDTH
plasma_tex_ybuf:                                ;plasma tex ybuffer
        resb   PLASMA_TEX_HEIGHT
plasma_xbuf:                                    ;plasma xbuffer
        resb   PLASMA_WIDTH
plasma_ybuf:                                    ;plasma ybuffer
        resb   PLASMA_HEIGHT
plasma_counter:                                 ;ticks elapsed in plasma effect
        dw      0
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
