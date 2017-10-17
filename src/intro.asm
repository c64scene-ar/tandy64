; Tandy64 intro
; http://pungas.space
; code: riq

bits    16
cpu     8086

extern ZTimerOn, ZTimerOff, ZTimerReport

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; MACROS
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
%define DEBUG 1

TEXT_WRITER_OFFSET_Y    equ     19*2*160        ;start at line 19:160 bytes per line, lines are every 4 -> 8/4 =2
BOTTOM_OFFSET   equ     21*2*160                ;start at line 21:160 bytes per line, lines are every 4 -> 8/4 =2
SCROLL_OFFSET   equ     22*2*160                ;start at line 22:160 bytes per line, lines are every 4 -> 8/4 =2
PLASMA_TEX_OFFSET       equ 21*2*160+160        ;plasma texture: video offset
PLASMA_TEX_WIDTH        equ 160                 ;plasma texture: pixels wide
PLASMA_TEX_HEIGHT       equ 32                  ;plasma texture: pixels height
PLASMA_WIDTH    equ 32                          ;plasma: pixels wide
PLASMA_HEIGHT   equ 16                          ;plasma: pixels height


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; render vertically 4 bits needed for the scroll. grabs the firts for bytes from the cache,
; use the MSB bit. If it is on, use white, else black color
;
; IN:   ds:si   -> bit to render (pointer to cache)
;       es:di   -> where to render (ponter to video memory)
;       dx      -> pointer to pixel color table
;       bx      -> row index
; Args: %1: offset line.
%macro render_bit 1

        mov     di,SCROLL_OFFSET+160*(%1+1)-1   ;es:di points to video memory
        mov     cx,4                            ;times to loop
%%loop_print:
        lodsb                                   ;fetches byte from the cache
        mov     ah,al                           ;save value in ah for later use
        and     al,1100_0000b
        rol     al,1
        rol     al,1
        xchg    dx,bx                           ;save bx, load dx with pointer tbl

        xlat                                    ;al = [scroll_pixel_color_tbl+ al]
        stosb

        xchg    dx,bx                           ;restore bx

        add     di,8192-1                       ;draw in next bank. di was incremented by
                                                ; one in stosb.

        shl     ah,1                            ;al << 2. bit 7,6 contains next bits to render
        shl     ah,1                            ;
        mov     [cache_charset+bx],ah           ;update cache for next iteration
        inc     bx

        loop    %%loop_print
%endmacro

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; render horizontally 2 bytes (4 pixels)
; useful to render a char from a 1x1 charset
;
; IN:
;       es:di   -> where to render (ponter to video memory)
;       bx      -> offset of nibble to render
;       ah      -> must be 0
%macro render_nibble 0

        mov     al,byte [c64_charset+bx]        ;first byte to print from charset. represents 8 pixels
        mov     dl,al                           ;save al
        shr     al,1                            ;process hi nibble. shift 3 times to right
        shr     al,1                            ; instead of shifting 4 times
        shr     al,1                            ; and then one shift left
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

;        mov     al,65
;        call    text_writer_print_char
;        inc     byte [text_writer_x_pos]
;        mov     al,66
;        call    text_writer_print_char

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

        call    wait_vertical_retrace

        mov     cx,160                          ;and wait for scanlines
.repeat:
        call    wait_horiz_retrace
        loop    .repeat

        cli                                     ;disable interrupts
                                                ; while setting the interrupt
        push    es
        sub     ax,ax
        mov     es,ax

        mov     ax,new_i08
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
        mov     word [current_state],0
        call    [states_inits]                  ;init state 0

        call    music_init
        call    text_writer_init
        call    crtc_addr_init
        call    palette_colors_init
        call    scroll_init
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
main_loop:
.loop:
        cmp     byte [tick],0                   ;in theory, the tick is not needed
        je      .loop                           ; since i'm not doing anything, but
                                                ; in practice, if not used, the interrupt could be triggered
                                                ; in the middle of the BIOS call, some intructions are longer than others,
                                                ; and it could generate some flicker in the raster bar routine
        dec     byte [tick]

        mov     ah,1
        int     0x16                            ;INT 16,AH=1, OUT:ZF=status
        jz      .loop

        mov     ah,0
        int     0x16

        ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
new_i08:
        ;not saving any variable, since the code at main loop
        ;happens after the tick

        mov     ax,data
        mov     ds,ax

        cmp     byte [raster_state],0           ;which raster effect to do?
        jne     .raster_bars
        call    cycle_palette_anim              ;do cycle palette
        jmp     .l0
.raster_bars:
        call    raster_bars_anim                ;do raster bars
.l0:

        mov     si,top_palette                  ;points to colors used at the top of the screen
        call    refresh_palette                 ;refresh the palette

%if DEBUG
        call    inc_d020
%endif

        ;after raster baster finishes

        mov     bx,word [current_state]         ;fetch state
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
                                                ;whatever he wants

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
        inc     word [current_state]
        mov     bx,word [current_state]
        shl     bx,1
        call    [states_inits+bx]
        ret

        mov     si,raster_colors_tbl
        mov     cx,RASTER_COLORS_MAX

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; si: table with the palette to update
refresh_palette:
        mov     cx,16                           ;repeat it 16 times
        mov     bl,0x10                         ;starts with color 0 (0 + 0x10)
        mov     bh,0xde                         ;register is faster than memory

        mov     dx,0x03da                       ;select color register

.l0:
        mov     al,bl                           ;color to update
        out     dx,al

        lodsb                                   ;load new color value
        mov     dl,bh                           ;dx=0x3de
        out     dx,al

        mov     dl,0xda                         ;dx=0x3da
        inc     bl
        loop    .l0

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
cycle_palette_anim:

        mov     si,bottom_palette               ;points to colors used at the bottom
        call    refresh_palette                 ;refresh the palette

        dec     byte [cycle_palette_delay]      ;ready to cycle palette?
        cmp     byte [cycle_palette_delay],0
        jne     .end
        je      .end

        mov     byte [cycle_palette_delay],2    ;update delay counter

        mov     bx,es                           ;save bx for later
        mov     ax,data
        mov     es,ax                           ;update es use

        ;cycles the palette
        mov     dl,[bottom_palette+1]           ;save first value
        mov     cx,14                           ;shift table to the left once
        mov     si,bottom_palette+2             ; but don't touch color black
        mov     di,bottom_palette+1             ; needed to avoid flicker
        rep     movsb
        mov     [bottom_palette+15],dl          ;color[15] = color[1]

        mov     es,bx                           ;restore bx

.end:
        jmp     wait_vertical_retrace           ;keep these colors until the vert retrace

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
raster_bars_anim:
        mov     dx,0x03da
        mov     al,0x1f                         ;select palette color 15 (white)
        out     dx,al

        ;BEGIN raster bar code
        ;should be done as fast as possible
        mov     bx,0xdade                       ;used for 3da / 3de. faster than
                                                ;add / sub 4
.l0:
        lodsb                                   ;fetch color
        mov     ah,al                           ; and save it for later
.wait:
        in      al,dx                           ;inline wait horizontal retrace
        test    al,1                            ; for performance reasons
        jnz     .wait
.retrace:
        in      al,dx
        test    al,1
        jz      .retrace                        ;horizontal retrace after this

        mov     dl,bl                           ;dx = 0x3de
        mov     al,ah
        out     dx,al                           ;set new color

        mov     dl,bh                           ;dx = 0x3da

        loop    .l0                             ;and do it 17 times
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
        mov     byte [scroll_enabled],0         ;dsiabled by default
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_anim:
        cmp     byte [scroll_enabled],0         ;scroll enabled?
        jne     .doit
        ret

.doit:
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

        ;update the cache with the next 32 bytes (2x2 chars)
        mov     bx,[scroll_char_idx]            ;scroll text offset
        mov     bl,byte [scroll_text+bx]        ;char to print
        and     bl,0011_1111b                   ;only use lower 63 bits
        sub     bh,bh
        shl     bx,1                            ;bx * 8 since each char takes 8
        shl     bx,1                            ; bytes in the charset
        shl     bx,1
        lea     si,[charset+bx]                 ;ds:si: charset


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


.render_bits:
        mov     ax,0xb800
        mov     es,ax

        mov     di,SCROLL_OFFSET+159            ;es:di points to video memory
        mov     si,cache_charset                ;ds:si points to cache_charset
        sub     bx,bx                           ;used for the cache index in the macros
        mov     dx,scroll_pixel_color_tbl       ;used in the macros

        render_bit 0
        render_bit 1
        render_bit 2
        render_bit 3

        inc     byte [scroll_bit_idx]           ;two incs, since it prints 2 bits at the time
        inc     byte [scroll_bit_idx]

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

.next_char:
        sub     ax,ax
        mov     byte [scroll_col_used],al       ;reset to 0
        mov     byte [scroll_bit_idx],al        ;reset bit idx
        inc     word [scroll_char_idx]          ;scroll_char_idx++
        cmp     word [scroll_char_idx],SCROLL_TEXT_LEN  ;end of scroll?
        jnz     .end                            ; if so, reset index
        int 3
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
        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR
        mov     word [text_writer_idx],-1       ;HACK: data offset is -1, because we do a +1 at anim
        mov     byte [text_writer_delay],10     ;delay waits 10 refreshes
        mov     byte [text_writer_enabled],0    ;disabled by default
        mov     byte [text_writer_cursor_blink_delay],0 ;how many blinks to wait
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
        mov     al,160                          ;select reverse space
        call    text_writer_print_char          ; and print it

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
text_writer_state_goto_init:
        inc     word [text_writer_idx]          ;data offset += 1
        mov     bx,word [text_writer_idx]       ;get data offset
        mov     al,byte [text_writer_data+bx]   ;pos to go back to
        mov     byte [text_writer_x_dst],al     ;update destination

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_goto_anim:
        mov     al,byte [text_writer_x_dst]     ;fetch destination pos
        cmp     byte [text_writer_x_pos],al     ; and compare with current pos
        jb      .right
        ja      .left

        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR    ;if equal, means finished
        ret                                     ; then change state to read chars again

.right:
        mov     al,' '                          ;select space
        call    text_writer_print_char          ; and print it
        inc     byte [text_writer_x_pos]        ;cursor += 1. destintation is to the right
        mov     al,160                          ;select reverse space
        jmp     text_writer_print_char          ; print it, and return

.left:
        mov     al,' '                          ;select space
        call    text_writer_print_char          ; and print it
        dec     byte [text_writer_x_pos]        ;cursor -= 1. destintation is to the left
        mov     al,160                          ;select reverse space
        jmp     text_writer_print_char          ; print it, and return



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
        mov     al,160                          ;select full char
        jmp     text_writer_print_char          ; print it, and return

.cursor_off:
        mov     al,' '                          ;select space char (empty char)
        jmp     text_writer_print_char          ; print it, and return

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

state_enable_scroll:
        mov     byte [scroll_enabled],1         ;enable scroll

        mov     bx,es                           ;save es for later
        mov     ax,ds
        mov     es,ax

        mov     cx,8                            ;16 colors (16 bytes == 8 words)
        mov     di,bottom_palette               ;destination: bottom palette
        mov     si,palette_default              ;source: default palette
        rep movsw                               ;copy the new 16 colors

        mov     es,bx                           ;restore es

        ;clear the bottom part with black pixels
        ;previously it was filled with the plasma pixels
        sub     ax,ax

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*0         ;destination
        rep stosw                               ;do the 'clean screen'

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*1         ;destination
        rep stosw                               ;do the 'clean screen'

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*2         ;destination
        rep stosw                               ;do the 'clean screen'

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*3         ;destination
        rep stosw                               ;do the 'clean screen'

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_tex_init_sine_table:
        ; x
        sub     bh,bh
        mov     bl,byte [sine_xbuf_1_idx]       ;bx=offset for buffer x1
        mov     cl,bl
        sub     ah,ah
        mov     al,byte [sine_xbuf_2_idx]       ;si=offset for buffer x2
        mov     dl,al
        mov     si,ax

        %assign XX 0
        %rep    PLASMA_TEX_WIDTH
                mov     al,[sine_table+bx]      ;xbuf[idx] = sine[bx]+sine[si]
                add     al,[sine_table+si]
                mov     [plasma_tex_xbuf+XX],al
                add     bl,5;7                  ;update offsets to sine tables
                sub     si,8;9
                and     si,255                  ;only use LSB part of si
        %assign XX XX+1
        %endrep

        add     cl,9                            ;update buffer x1 and x2 offsets
        sub     dl,5                            ; for the next frame
        mov     byte [sine_xbuf_1_idx],cl
        mov     byte [sine_xbuf_2_idx],dl


        ; y
        sub     bh,bh                           ;do the same thing, but for buffer y
        mov     bl,byte [sine_ybuf_1_idx]       ;bx=offset for buffer y1
        mov     cl,bl
        sub     ah,ah
        mov     al,byte [sine_ybuf_2_idx]       ;si=offset for buffer y2
        mov     dl,al
        mov     si,ax

        %assign YY 0
        %rep    PLASMA_TEX_HEIGHT
                mov     al,[sine_table+bx]      ;ybuff[YY] = sine[bx]+sine[si]
                add     al,[sine_table+si]
                mov     [plasma_tex_ybuf+YY],al ;update y buffer with sine+sine
                add     bl,3;3
                sub     si,5;4
                and     si,255                  ;update offets, and use only LSB part of si
        %assign YY YY+1
        %endrep

        add     cl,7
        sub     dl,3
        mov     byte [sine_ybuf_1_idx],cl       ;update buffer y1 and y2 offests
        mov     byte [sine_ybuf_2_idx],dl       ; to be used in the next frame

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
state_plasma_blue_tex_init:
        sub     ax,ax                           ;faster to use register than value
        mov     word [plasma_tex_x_offset],ax
        mov     byte [plasma_tex_state],al      ;initial state
        mov     byte [plasma_tex_colors_updated],al
        mov     byte [plasma_tex_delay],al
        mov     word [plasma_tex_palette_addr],plasma_tex_blue_palette
        mov     byte [plasma_tex_letter_color],0xd        ;letter M uses color 0xd

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
state_plasma_init:
        sub     ax,ax

        mov     [plasma_counter],ax             ;ticks at 0

        ;clear the bottom part with black pixels
        ;previously it was filled with the plasma pixels

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*0         ;destination
        rep stosw                               ;do the 'clean screen'

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*1         ;destination
        rep stosw                               ;do the 'clean screen'

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*2         ;destination
        rep stosw                               ;do the 'clean screen'

        mov     cx,640                          ;8 rows
        mov     di,BOTTOM_OFFSET+8192*3         ;destination
        rep stosw                               ;do the 'clean screen'

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_plasma_anim:
        inc     word [plasma_counter]
        cmp     word [plasma_counter],60*5      ;5 seconds
        jne     .do_plasma

        jmp     state_next                      ;reached end of effect. trigger
                                                ; next state

.do_plasma:
        call    plasma_update_sine_table
        jmp     plasma_render_to_video

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_update_sine_table:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
plasma_render_to_video:
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

        mov     di,TEXT_WRITER_OFFSET_Y         ;writer start point
        mov     al,byte [text_writer_x_pos]     ;x pos, from 0 to 39
        shl     ax,1
        shl     ax,1                            ;times 4 (each char takes 4 bytes wide)
        add     di,ax                           ;or is cheaper than add

        sub     ah,ah

        render_nibble

        add     di,8192-4
        render_nibble

        add     di,8192-4
        render_nibble

        add     di,8192-4
        render_nibble


        sub     di,24576-156                    ;160-4
        render_nibble

        add     di,8192-4
        render_nibble

        add     di,8192-4
        render_nibble

        add     di,8192-4
        render_nibble

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
        ; # = tm
        ; % = closing double quotes
        ; $ = smiley
        ; ; = ~ (kind of separator)
scroll_text:
        db 'HI THERE. PUNGAS DE VILLA MARTELLI HERE, WITH OUR FIRST TANDY RELEASE. '
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
        db 'WHAT !? NO TANDY 1000 SCENE ??? HOW DARE YOU !!! '
        db `HOPEFULLY THIS WON'T BE OUR LAST TANDY RELEASE. `
        db 'PROBLEM IS THERE ARE ALMOST NO PARTIES ACCEPTING TANDY RELEASES. '
        db 'DO US A FAVOR: PING YOUR FAVORITE PARTY-ORGANIZER AND DEMAND HIM/HER '
        db 'TANDY SUPPORT. '
        db 27,28,29,30,31,42,43                 ; Radio Shack (using Radio Shack font)
        db ' DESERVES IT ! '
        db '   ;   '
        db 'AS MUCH AS WE WOULD LIKE TO SAY THAT WE DID THIS RELEASE AS A TRIBUTE TO '
        db 27,28,29,30,31,42,43                 ; Radio Shack (using Radio Shack font)
        db ', IT WAS JUST MERE CHANCE. HOWEVER, WE ARE FOND OF '
        db 27,28,29,30,31,42,43                 ; Radio Shack (using Radio Shack font)
        db ` .HEY, WHO DOESN'T?`
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
scroll_pixel_color_tbl:
        db      0x00                            ;00 - black/black
        db      0x0f                            ;01 - black/white
        db      0xf0                            ;10 - white/black
        db      0xff                            ;11 - white/white
scroll_enabled:                                 ;boolean. when 0, scroll is disabled
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
        dw      0                               ; function to call.

states_inits:
        dw      state_gfx_fade_in_init          ;b
        dw      state_fade_to_black_init        ;c
        dw      state_delay_2s_init             ;d
        dw      state_outline_fade_init         ;e
        dw      state_outline_fade_init         ;f
        dw      state_plasma_red_tex_init       ;d'
        dw      state_plasma_green_tex_init     ;d''
        dw      state_plasma_blue_tex_init      ;d'''
        dw      state_enable_text_writer        ;n
        dw      state_pvm_logo_fade_in_init     ;h
        dw      state_outline_fade_init         ;j
        dw      state_plasma_init               ;g
        dw      state_delay_2s_init             ;k
        dw      state_enable_scroll             ;l
        dw      state_nothing_init              ;o

states_callbacks:
        dw      state_gfx_fade_in_anim          ;b
        dw      state_fade_to_black_anim        ;c
        dw      state_delay_anim                ;d
        dw      state_outline_fade_in_anim      ;e
        dw      state_outline_fade_out_anim     ;f
        dw      state_plasma_tex_anim           ;d'
        dw      state_plasma_tex_anim           ;d''
        dw      state_plasma_tex_anim           ;d'''
        dw      state_skip_anim                 ;n
        dw      state_pvm_logo_fade_in_anim     ;h
        dw      state_outline_fade_to_final_anim;j
        dw      state_plasma_anim               ;g
        dw      state_delay_anim                ;k
        dw      state_skip_anim                 ;l
        dw      state_nothing_anim              ;o

text_writer_x_pos:                              ;position x for the cursor. 0-39
        db      0
text_writer_x_dst:                              ;dst position x for the cursor. 0-39
        db      0

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

        ;text writer enums. must be syncced with callbacks order
TW_STATE_PRINT_CHAR     equ 0
TW_STATE_IDLE           equ 1
TW_STATE_GOTO   equ 2
TW_STATE_CALL_ACTION    equ 3
TW_STATE_CURSOR_BLINK   equ 4
TW_STATE_MAX            equ 5                   ;should be the last state
text_writer_state:
        db      0

text_writer_callbacks_init:
        dw      0                                       ;no init for print_char
        dw      text_writer_state_idle_init             ;
        dw      text_writer_state_goto_init     ;
        dw      text_writer_state_call_action_init      ;
        dw      text_writer_state_cursor_blink_init     ;

text_writer_callbacks_anim:
        dw      text_writer_state_print_char_anim       ;
        dw      text_writer_state_idle_anim             ;
        dw      text_writer_state_goto_anim     ;
        dw      text_writer_state_call_action_anim      ;
        dw      text_writer_state_cursor_blink_anim     ;

        ;control codes: think of it as a printer
        ; 0 - idle
        ; 2 - print char
        ; 3 - go to
        ; 4 - carriage return
        ; 5,n - perform an immediate action: eg: call enable_something
text_writer_data:
                ;0123456789012345678901234567890123456789
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO,39                ;go to pos 38
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO,0                 ;go to pos 0
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks

        db      '                Hi there'
        db      TW_STATE_CURSOR_BLINK,5         ;wait blinks
        db      TW_STATE_GOTO,9                 ;go to pos 9

        db                'My name is cursorito'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_CALL_ACTION,0          ;execute action 0: enable rhythm
        db      TW_STATE_GOTO,0                 ;go to pos 0

        db      'eh?... que me contrusi'
        db      TW_STATE_CURSOR_BLINK,3         ;wait blinks
        db      TW_STATE_GOTO,0                 ;go to pos 0
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

RASTER_STATE_PALETTE equ 0
RASTER_STATE_RASTERBARS equ 1
raster_state:                                   ;raster state machine. which effect to perform?
        db      0                               ;0=palette, 1=raster bars

raster_colors_tbl:                              ;16 colors in total
        db      1,2,3,4,5,6,7,8
        db      9,10,11,12,13,14,15,0
raster_color_restore:                           ;must be after raster_colors_tbl
        db      15
RASTER_COLORS_MAX equ $-raster_colors_tbl

top_palette:                                    ;palette used for the upper part of the screen
        db      0,1,2,3,4,5,6,7                 ;default palette
        db      8,9,10,11,12,13,14,15

bottom_palette:                                 ;palette used for the bottom part of the screen
        db      0,1,2,3,4,5,6,7                 ;default palette
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

cycle_palette_delay:                            ;delay for the palette cycle animation
        db      1

crtc_start_addr:
        dw      0                               ;crtc start address

sine_xbuf_1_idx:                                ;plasma: xbuf idx 1
        db      0
sine_xbuf_2_idx:                                ;plasma: xbuf idx 2
        db      0
sine_ybuf_1_idx:                                ;plasma: ybuf idx 1
        db      0
sine_ybuf_2_idx:                                ;plasma: ybuf idx 2
        db      0
plasma_tex_xbuf:                                ;plasma tex xbuffer
        resb   PLASMA_TEX_WIDTH
plasma_tex_ybuf:                                ;plasma tex ybuffer
        resb   PLASMA_TEX_HEIGHT
plasma_counter:                                 ;ticks elapsed in plasma effect
        dw      0
sine_table:
; autogenerated table: easing_table_generator.py -s128 -m255 -aTrue -r bezier:0,0.02,0.98,1
        db        0,  0,  1,  1,  2,  2,  3,  4
        db        4,  5,  6,  7,  8, 10, 11, 12
        db       14, 15, 17, 18, 20, 21, 23, 25
        db       27, 29, 31, 33, 35, 37, 39, 41
        db       44, 46, 48, 51, 53, 55, 58, 60
        db       63, 66, 68, 71, 73, 76, 79, 82
        db       84, 87, 90, 93, 96, 98,101,104
        db      107,110,113,116,119,122,125,128
        db      130,133,136,139,142,145,148,151
        db      154,157,159,162,165,168,171,173
        db      176,179,182,184,187,189,192,195
        db      197,200,202,204,207,209,211,214
        db      216,218,220,222,224,226,228,230
        db      232,234,235,237,238,240,241,243
        db      244,245,247,248,249,250,251,251
        db      252,253,253,254,254,255,255,255
; reversed
        db      255,255,254,254,253,253,252,251
        db      251,250,249,248,247,245,244,243
        db      241,240,238,237,235,234,232,230
        db      228,226,224,222,220,218,216,214
        db      211,209,207,204,202,200,197,195
        db      192,189,187,184,182,179,176,173
        db      171,168,165,162,159,157,154,151
        db      148,145,142,139,136,133,130,128
        db      125,122,119,116,113,110,107,104
        db      101, 98, 96, 93, 90, 87, 84, 82
        db       79, 76, 73, 71, 68, 66, 63, 60
        db       58, 55, 53, 51, 48, 46, 44, 41
        db       39, 37, 35, 33, 31, 29, 27, 25
        db       23, 21, 20, 18, 17, 15, 14, 12
        db       11, 10,  8,  7,  6,  5,  4,  4
        db        3,  2,  2,  1,  1,  0,  0,  0

luminances_8_color_lo:
        db      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        db      0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        db      0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11
        db      0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x11
        db      0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22
        db      0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22,0x22
        db      0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33
        db      0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33
        db      0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44
        db      0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44,0x44
        db      0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
        db      0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
        db      0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66
        db      0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66,0x66
        db      0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77
        db      0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77,0x77

luminances_8_color_hi:
        db      0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88
        db      0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88,0x88
        db      0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99
        db      0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99,0x99
        db      0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa
        db      0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa,0xaa
        db      0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb
        db      0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb,0xbb
        db      0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc
        db      0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc,0xcc
        db      0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd
        db      0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd,0xdd
        db      0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee
        db      0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee,0xee
        db      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
        db      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff

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
