; Tandy64 intro
; http://pungas.space
; code: riq

bits    16
cpu     8086

extern ZTimerOn, ZTimerOff, ZTimerReport

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; MACROS
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

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

        mov     di,OFFSET_Y+160*(%1+1)-1        ;es:di points to video memory
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

        mov     cx,180                          ;and wait for scanlines
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

        mov     si,raster_colors_tbl
        mov     cx,RASTER_COLORS_MAX

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
.w:
        in      al,dx                           ;inline wait horizontal retrace
        test    al,1                            ; for performance reasons
        jnz     .w
.r:
        in      al,dx
        test    al,1
        jz      .r                              ;horizontal retrace after this

        mov     dl,bl                           ;add 4 = 3de
        mov     al,ah
        out     dx,al                           ;set new color

        mov     dl,bh                           ;sub 4 = 3da

        loop    .l0                             ;and do it 17 times

        ;END raster bar code

;        call    inc_d020


        ;after raster baster finishes

        mov     bx,word [current_state]         ;fetch state
        shl     bx,1                            ; and convert it into offset (2 bytes per offset)
        call    [states_callbacks+bx]           ; and call correct state callback

        call    crtc_addr_anim                  ;change CRTC start address
        call    music_anim                      ;play music
        call    noise_fade_anim                 ;outline fade anim
        call    text_writer_anim                ;text writer
        call    scroll_anim                     ;anim scroll

;        call    dec_d020

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

        mov     cx,PALETTE_COLORS_TO_BLACK_MAX
        mov     si,palette_colors_to_black

        mov     dx,0x03da                       ;select color register
.loop:
        lodsb                                   ;color to fade
        or      al,0x10                         ;color index start at 0x10
        out     dx,al

        mov     dl,0xde                         ;dx=0x03de
        mov     al,[palette_black_tbl+bx]
        out     dx,al

        mov     dl,0xda                         ;dx=0x03da
        loop    .loop

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
state_clemente_fade_in_init:
        mov     word [clemente_lfsr_current_state],LFSR_START_STATE

        ;logo should be turned off by default
        mov     cx,PALETTE_COLORS_TO_BLACK_MAX
        mov     dx,0x03da                       ;select color
        mov     si,palette_colors_to_black
.loop:
        lodsb                                   ;color to fade
        or      al,0x10                         ;colors start at 0x10
        out     dx,al

        mov     dl,0xde                         ;dx=0x03de
        mov     al,1                            ;should be blue
        out     dx,al

        mov     dl,0xda                         ;dx=0x03da
        loop    .loop

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Uses Linear Feedback Shift Register 15-bit
; https://en.wikipedia.org/wiki/Linear-feedback_shift_register
state_clemente_fade_in_anim:

        push    ds

        mov     ax,[clemente_lfsr_current_state]

        mov     bx,gfx                          ;set segments
        mov     ds,bx                           ;ds: gfx segment
                                                ;es: video memory (alread points to it)

;        sub     ax,ax
;        mov     si,ax
;        mov     di,ax
;        mov     cx,16384
;        rep     movsw
;        pop     ds
;        call    state_next
;        ret

        mov     cx,150                          ;pixels to draw per frame
.loop:
        mov     di,ax
        mov     si,ax
        mov     dx,di
        and     dx,0x1c00                       ;don't write pixel if in scroll space
        cmp     dx,0x1c00                       ;areas: 1c00-1fff,3c00-3fff,5c00-5fff,7c00-7fff should not be written
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
        mov     dx,0x03da
        mov     al,0x12                         ;logo inner color
        out     dx,al

        mov     dl,0xde                         ;dx=0x03de
        mov     al,[palette_pvm_logo_fade_tbl+bx]
        out     dx,al


        ;letter V
        mov     dl,0xda                         ;dx=0x03da
        mov     al,1ah                          ;logo inner color
        out     dx,al

        sub     bx,8                            ;offset -8
        mov     dl,0xde                         ;dx=0x03de
        mov     al,[palette_pvm_logo_fade_tbl+bx]
        out     dx,al


        ;letter M
        mov     dl,0xda                         ;dx=0x03da
        mov     al,1dh                          ;logo inner color
        out     dx,al

        sub     bx,8                            ;offset -8
        mov     dl,0xde                         ;dx=0x03de
        mov     al,[palette_pvm_logo_fade_tbl+bx]
        out     dx,al

        inc     word [palette_idx]
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
scroll_anim:
        mov     ax,0xb800                       ;ds points to video memory
        mov     ds,ax                           ;es already points to it

OFFSET_Y        equ     23*2*160                ;start at line 23:160 bytes per line, lines are every 4 -> 8/4 =2

        mov     cx,320                          ;scroll 4 lines of 80 chars
        mov     si,OFFSET_Y+1                   ;source: last char of screen
        mov     di,OFFSET_Y                     ;dest: last char of screen - 1
        rep movsw                               ;do the copy

        mov     cx,320                          ;scroll 4 lines of 80 chars
        mov     si,OFFSET_Y+8192+1              ;source: last char of screen
        mov     di,OFFSET_Y+8192                ;dest: last char of screen - 1
        rep movsw                               ;do the copy

        mov     cx,320                          ;scroll 4 lines of 80 chars
        mov     si,OFFSET_Y+16384+1             ;source: last char of screen
        mov     di,OFFSET_Y+16384               ;dest: last char of screen - 1
        rep movsw                               ;do the copy

        mov     cx,320                          ;scroll 4 lines of 80 chars
        mov     si,OFFSET_Y+24576+1             ;source: last char of screen
        mov     di,OFFSET_Y+24576               ;dest: last char of screen - 1
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

        mov     di,OFFSET_Y+159                 ;es:di points to video memory
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
text_writer_state_backspace_to_init:
        inc     word [text_writer_idx]          ;data offset += 1
        mov     bx,word [text_writer_idx]       ;get data offset
        mov     al,byte [text_writer_data+bx]   ;pos to go back to
        mov     byte [text_writer_x_dst],al     ;update destination

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
text_writer_state_backspace_to_anim:
        mov     al,byte [text_writer_x_dst]     ;fetch destination pos
        cmp     byte [text_writer_x_pos],al     ; and compare with current pos
        ja      .do_back
        mov     byte [text_writer_state],TW_STATE_PRINT_CHAR    ;if back finished
        ret                                     ; then change state to read chars again

.do_back:
        mov     al,' '                          ;select space
        call    text_writer_print_char          ; and print it
        dec     byte [text_writer_x_pos]        ;cursor -= 1
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
; IN:   al=char to print
; ASSUMES:  es: pointer to video segment
;           ds: pointer to charset segment
text_writer_print_char:

TEXT_WRITER_OFFSET_Y    equ     21*2*160        ;start at line 21:160 bytes per line, lines are every 4 -> 8/4 =2

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
        db 'WHAT !? THERE IS NO TANDY 1000 SCENE ??? HOW DARE YOU !!! '
        db `HOPEFULLY THIS WON'T BE OUR LAST TANDY RELEASE. `
        db 'PROBLEM IS THERE ARE ALMOST NO PARTIES ACCEPTING TANDY RELEASES. '
        db 'DO US A FAVOR: PING YOUR FAVORITE PARTY-ORGANIZER AND DEMAND HIM/HER '
        db 'TANDY SUPPORT. '
        db 27,28,29,30,31,42,43                 ; Radio Shack (using Radio Shack font)
        db ' DESERVES IT ! '
        db '   ;   '
        db 'AS MUCH AS WE WOULD LIKE TO SAY THAT WE DID THIS RELEASE AS A TRIBUTE TO '
        db 27,28,29,30,31,42,43                 ; Radio Shack (using Radio Shack font)
        db `, IT IS NOT, IT WAS JUST A COINCIDENCE. HOWEVER, DON'T GET US `
        db 'WRONG. WE LOVE '
        db 27,28,29,30,31,42,43                 ; Radio Shack (using Radio Shack font)
        db ', WE ARE FOND OF THIS MACHINE, AND THE TRS-80. '
        db `BTW, CURRENTLY WE DON'T HAVE ANY TRS-80, AND IF YOU WANT TO DONATE US ONE `
        db 'WE WILL HAPPILY ACCEPT IT $. '
        db '   ;   '
        db 'CODE:RIQ, MUSIC: UCTUMI, GRAPHICS: ALAKRAN'
        db '   ;   '
        db 'SIGNING OFF...                          '
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
        dw      state_delay_2s_init             ;a
        dw      state_clemente_fade_in_init     ;b
        dw      state_fade_to_black_init        ;c
        dw      state_delay_5s_init             ;d
        dw      state_outline_fade_init         ;e
        dw      state_outline_fade_init         ;f
        dw      state_delay_5s_init             ;g
        dw      state_pvm_logo_fade_in_init     ;h
        dw      state_outline_fade_init         ;j
        dw      state_delay_2s_init             ;k
        dw      state_enable_text_writer        ;n
        dw      state_nothing_init              ;o

states_callbacks:
        dw      state_delay_anim                ;a
        dw      state_clemente_fade_in_anim     ;b
        dw      state_fade_to_black_anim        ;c
        dw      state_delay_anim                ;d
        dw      state_outline_fade_in_anim      ;e
        dw      state_outline_fade_out_anim     ;f
        dw      state_delay_anim                ;g
        dw      state_pvm_logo_fade_in_anim     ;h
        dw      state_outline_fade_to_final_anim;j
        dw      state_delay_anim                ;k
        dw      state_skip_anim                 ;n
        dw      state_nothing_anim              ;o

text_writer_state:
        db      0

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
TW_STATE_IDLE           equ 0
TW_STATE_PRINT_CHAR     equ 1
TW_STATE_BACKSPACE_TO   equ 2
TW_STATE_CALL_ACTION    equ 3
TW_STATE_MAX            equ 4                   ;should be the last state

text_writer_callbacks_init:
        dw      text_writer_state_idle_init             ;
        dw      0                                       ;no init for print_char
        dw      text_writer_state_backspace_to_init     ;
        dw      text_writer_state_call_action_init      ;

text_writer_callbacks_anim:
        dw      text_writer_state_idle_anim             ;
        dw      text_writer_state_print_char_anim       ;
        dw      text_writer_state_backspace_to_anim     ;
        dw      text_writer_state_call_action_anim      ;

        ;control codes: think of it as a printer
        ; 0 - idle
        ; 2 - print char
        ; 3 - backspace
        ; 4 - carriage return
        ; 5,n - perform an immediate action: eg: call enable_something
text_writer_data:
                ;0123456789012345678901234567890123456789
        db      '                Hi there'
        db      TW_STATE_IDLE,30                ;wait cycles
        db      TW_STATE_BACKSPACE_TO,9         ;back to pos 9
        db                `let's move to the rythm`
        db      TW_STATE_IDLE,30                ;wait 5 cycles
        db      TW_STATE_CALL_ACTION,0          ;execute action 0: enable rhythm
        db      TW_STATE_BACKSPACE_TO,0         ;back to pos 0
        db      'eh?... que me contrusi'
        db      TW_STATE_IDLE,30                ;idle 5 cycles
        db      TW_STATE_BACKSPACE_TO,0         ;back to pos 0
TEXT_WRITER_DATA_LEN equ $-text_writer_data

text_writer_delay:
        db      0                               ;used by 'delay state' to know who many
                                                ;vert retrace to wait

tick:                                           ;to trigger once the irq was called
        db      0
old_i08:                                        ;segment + offset to old int 8
        dd      0
old_pic_imr:                                    ;PIC IMR original value
        db      0

raster_colors_tbl:                              ;16 colors in total
        db      1,2,3,4,5,6,7,8
        db      9,10,11,12,13,14,15,0
raster_color_restore:                           ;must be after raster_colors_tbl
        db      15
RASTER_COLORS_MAX equ $-raster_colors_tbl

raster_colors_ibm_tbl:
        db      1,1,0,1, 1,0,1,1                ;blue and black
        db      0,1,1,0, 1,1,0,1

raster_colors_grayscale_tbl:
        db      1,1,0,1, 1,0,1,1                ;blue and black
        db      0,1,1,0, 1,1,0,1

crtc_start_addr:
        dw      0                               ;crtc start address
