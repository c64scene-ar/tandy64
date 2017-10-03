; Tandy64
; http://pungas.space
;
bits    16
cpu     8086

extern ZTimerOn, ZTimerOff, ZTimerReport

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; MACROS
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; render 4 bits needed for the scroll. grabs the firts for bytes from the cache,
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
; CODE
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text

global intro_start
intro_start:
        mov     ax,data                         ;init segments
        mov     ds,ax

        cld

        call    music_init

        call    main_loop

        call    sound_cleanup

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_init:
        mov     ax,data
        mov     ds,ax

        mov     word [pvm_offset],pvm_song + 10h       ;update start offset
        sub     al,al
        mov     byte [pvm_wait],al              ;don't wait at start
        mov     byte [noise_triggered],al       ;noise not playing
        mov     byte [noise_fade_idx],al
        mov     byte [noise_fade_enabled],al
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
wait_retrace:

        mov     dx,03dah
.l1:
        in      al,dx                           ;wait for vertical retrace
        test    al,8                            ; to finish
        jnz     .l1

.l0:
        in      al,dx                           ;wait for vertical retrace
        test    al,8                            ; to start
        jz      .l0

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
main_loop:

        mov     ax,data
        mov     ds,ax                           ;defaults for main loop

        mov     word [current_state],0
        call    [states_inits]                  ;init state 0

.loop:
        call    wait_retrace

;        call    inc_d020

        mov     bx,word [current_state]         ;fetch state
        shl     bx,1                            ; and convert it into offset
        call    [states_callbacks + bx]         ; and call correct state callback

        call    music_anim                      ;play music
        call    noise_fade_anim                 ;outline fade anim
        call    scroll_anim                     ;anim scroll

;        call    dec_d020

        mov     ah,1
        int     16h                             ; INT 16,AH=1, OUT:ZF=status
        jz      .loop

        mov     ah,0
        int     16h

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
sound_cleanup:
        mov     si,volume_0
        mov     cx,4
.repeat:
        lodsb
        out     0c0h,al
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
        sub     si,si                           ;idx for colors

        mov     dx,03dah                        ;select color register
.loop:
        mov     al,[palette_colors_to_black+si] ;which color to fade
        or      al,10h                          ;color index start at 0x10
        out     dx,al

        add     dl,4
        mov     al,[palette_black_tbl+bx]
        out     dx,al

        sub     dl,4
        inc     si
        loop    .loop

        mov     al,2                            ;select border color register
        out     dx,al
        add     dl,4
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
        call    state_next
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_clemente_fade_in_init:
        mov     word [clemente_lfsr_current_state],LFSR_START_STATE

        ;logo should be turned off by default
        sub     bx,bx
        mov     cx,PALETTE_COLORS_TO_BLACK_MAX
        mov     dx,03dah                        ;select color
.loop:
        mov     al,[palette_colors_to_black+bx]
        or      al,10h                          ;colors start at 10h
        out     dx,al

        add     dl,4
        mov     al,1                            ;should be blue
        out     dx,al

        sub     dl,4
        inc     bx
        loop    .loop

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; Uses Linear Feedback Shift Register 15-bit
; https://en.wikipedia.org/wiki/Linear-feedback_shift_register
state_clemente_fade_in_anim:

        push    ds

        mov     ax,[clemente_lfsr_current_state]

        mov     bx,gfx                          ;set segments
        mov     ds,bx
        mov     bx,0b800h
        mov     es,bx

        mov     cx,150                          ;pixels to draw per frame
.loop:
        mov     di,ax
        mov     si,ax
        mov     dx,di
        and     dx,01c00h                       ;don't write pixel if in scroll space
        cmp     dx,01c00h                       ;areas: 1c00-1fff,3c00-3fff,5c00-5fff,7c00-7fff should not be written
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
        call    state_next
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_letters_fade_in_init:
        mov     word [palette_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_letters_fade_in_anim:
        mov     bx,word [palette_idx]
        cmp     bx,PALETTE_LETTERS_FADE_MAX
        je      .end

        mov     dx,03dah                        ;animate letter P
        mov     al,012h                         ;logo inner color
        out     dx,al

        add     dl,4
        mov     al,[palette_letters_fade_tbl+bx]
        out     dx,al


        sub     dl,4                            ;animate letter V
        mov     al,1ah                          ;logo inner color
        out     dx,al

        sub     bx,8
        add     dl,4
        mov     al,[palette_letters_fade_tbl+bx]
        out     dx,al


        sub     dl,4                            ;animate letter M
        mov     al,1dh                          ;logo inner color
        out     dx,al

        sub     bx,8
        add     dl,4
        mov     al,[palette_letters_fade_tbl+bx]
        out     dx,al

        inc     word [palette_idx]
        ret
.end:
        call    state_next
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_nothing_init:
        ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_nothing_anim:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_skip_anim:
        call    state_next
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_outline_fade_init:
        mov     word [palette_outline_fade_idx],0
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_outline_fade_in_anim:
        mov     bx,word [palette_outline_fade_idx]
        cmp     bx,PALETTE_OUTLINE_FADE_IN_MAX
        je      .end

        mov     dx,03dah                        ;select border color register
        mov     al,15h                          ;logo outline color: 5
        out     dx,al                           ;select palette register

        add     dl,4                            ;change color
        mov     al,[palette_outline_fade_in_tbl + bx]
        out     dx,al

        inc     word [palette_outline_fade_idx]
        ret
.end:
        call    state_next
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_outline_fade_out_anim:
        mov     bx,word [palette_outline_fade_idx]
        cmp     bx,PALETTE_OUTLINE_FADE_OUT_MAX
        je      .end

        mov     dx,03dah                        ;select border color register
        mov     al,15h                          ;logo outline color: 5
        out     dx,al                           ;select palette register

        add     dl,4                            ;change color
        mov     al,[palette_outline_fade_out_tbl + bx]
        out     dx,al

        inc     word [palette_outline_fade_idx]
        ret
.end:
        call    state_next
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_outline_fade_to_final_anim:
        mov     bx,word [palette_outline_fade_idx]
        cmp     bx,PALETTE_OUTLINE_FADE_TO_FINAL_MAX
        je      .end

        mov     dx,03dah                        ;select border color register
        mov     al,15h                          ;logo outline color: 5
        out     dx,al                           ;select palette register

        add     dl,4                            ;change color
        mov     al,[palette_outline_fade_to_final_tbl + bx]
        out     dx,al

        inc     word [palette_outline_fade_idx]
        ret
.end:
        call    state_next
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_anim:
        push    es                              ;no need to save ds. will be restored
                                                ; at the end of the function
        mov     ax,0b800h                       ;video memory
        mov     ds,ax
        mov     es,ax

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


        mov     ax,data
        mov     ds,ax

        cmp     byte [scroll_bit_idx],0         ;only update cache if scroll_bit_idx == 0
        jnz     .render_bits                    ; and scroll_col_used == 0
        cmp     byte [scroll_col_used],0
        jnz     .render_bits

        ;update the cache with the next 32 bytes (2x2 chars)
        mov     bx,[scroll_char_idx]            ;scroll text offset
        sub     bh,bh
        mov     bl,byte [scroll_text+bx]        ;char to print
        and     bl,0011_1111b                   ;only use lower 63 bits
        shl     bx,1                            ;bx * 8 since each char takes 8
        shl     bx,1                            ; bytes in the charset
        shl     bx,1
        lea     si,[charset+bx]                 ;ds:si: charset

        push    es                              ;save es for later

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

        pop     es                              ;restore es. points to video memory


.render_bits:
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
        pop     es
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
        mov     word [scroll_char_idx],ax

.end:
        pop     es

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
        out     0c0h,al

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
        mov     byte [pvm_wait], 5              ;wait 5 cycles before starting again
        mov     word [pvm_offset], pvm_song + 10h       ; beginning of song

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
state_enable_noise_fade_init:
        inc     byte [noise_fade_enabled]
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

        mov     dx,03dah
        mov     al,15h                          ;color 5 is outline color
        out     dx,al

        add     dl,4
        mov     al,[noise_fade_tbl+bx]        ;fetch color
        out     dx,al

        inc     byte [noise_fade_idx]
.exit:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
inc_d020:
        mov     dx,03dah                        ;show how many raster barts it consumes
        mov     al,2                            ;select border color
        out     dx,al

        add     dx,4
        mov     al,0fh
        out     dx,al                           ;change border to white
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
dec_d020:
        mov     dx,03dah                        ;show how many raster barts it consumes
        mov     al,2                            ;select border color
        out     dx,al

        add     dx,4
        sub     al,al
        out     dx,al                           ;change border back to black

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; DATA GFX
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .gfx data

logo:                                           ;'logo' MUST be the first variable in the segment
        incbin 'src/logo.raw'

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
        db      8,7,11,7,8,1
NOISE_FADE_MAX equ $-noise_fade_tbl


LFSR_START_STATE equ 1973                       ;lfsr start state
clemente_lfsr_current_state     dw      0       ;lfsr current state

palette_black_delay:
        dw      0
palette_black_idx:
        dw      0
palette_black_tbl:                              ;fade to white/fade to black colors
        db      1,9,11,15
        db      15,7,8,0,0
PALETTE_BLACK_MAX equ $-palette_black_tbl

palette_colors_to_black:                        ;colors that should turn black
        db      00h,05h,02h,0ah,0dh             ;black, outline, P,V,M
PALETTE_COLORS_TO_BLACK_MAX equ $-palette_colors_to_black

delay_frames:
        dw      0                               ;frames to wait before doing something

charset:
        incbin 'src/font_unknown_2x2-charset.bin'

cache_charset:
        resb    32                              ;the 32 bytes to print in the current frame
                                                ; char aligned like: top-left, bottom-left,
                                                ; top-right, bottom-right
scroll_text:
        db 'ABCDEFGHIJKLMNOPQRSTUVWXYZ A B C D E 0123456789    '
SCROLL_TEXT_LEN equ $-scroll_text

scroll_char_idx:                                ;pointer to the next char
        db 0
scroll_bit_idx:                                 ;pointer to the next bit in the char
        db 0
scroll_col_used:
        db 0                                    ;chars are 2x2. col indicates which col is being used
scroll_pixel_color_tbl:
        db       00h                            ; 00 - black/black
        db       0fh                            ; 01 - black/white
        db      0f0h                            ; 10 - white/black
        db      0ffh                            ; 11 - white/white

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
palette_letters_fade_tbl:                       ;inner logo color
        db      8,8,8,8,7,7,7,7
        db      11,11,11,11,9,9,9,9

        db      9,9,9,9,9,9,9,9
        db      9,9,9,9,9,9,9,9
PALETTE_LETTERS_FADE_MAX equ $-palette_letters_fade_tbl

volume_0:
        db      1001_1111b                      ;vol 0 channel 0
        db      1011_1111b                      ;vol 0 channel 1
        db      1101_1111b                      ;vol 0 channel 2
        db      1111_1111b                      ;vol 0 channel 3

current_state:
        dw      0                               ;current state. default: 0

states_inits:
        dw      state_delay_2s_init
        dw      state_clemente_fade_in_init
        dw      state_fade_to_black_init
        dw      state_delay_5s_init
        dw      state_outline_fade_init
        dw      state_outline_fade_init
        dw      state_delay_5s_init
        dw      state_letters_fade_in_init
        dw      state_delay_500ms_init
        dw      state_outline_fade_init
        dw      state_delay_2s_init
        dw      state_enable_noise_fade_init
        dw      state_nothing_init

states_callbacks:
        dw      state_delay_anim
        dw      state_clemente_fade_in_anim
        dw      state_fade_to_black_anim
        dw      state_delay_anim
        dw      state_outline_fade_in_anim
        dw      state_outline_fade_out_anim
        dw      state_delay_anim
        dw      state_letters_fade_in_anim
        dw      state_delay_anim
        dw      state_outline_fade_to_final_anim
        dw      state_delay_anim
        dw      state_skip_anim
        dw      state_nothing_anim
