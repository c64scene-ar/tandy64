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
; IN:  ds:si -> bit to render (pointer to cache)
;      es:di -> where to render (ponter to video memory)
; Args: %1: offset line.
%macro render_bit 1

        mov     di,OFFSET_Y+160*(%1+1)-1        ;es:di points to video memory
        mov     cx,4                            ;times to loop
%%loop_print:
        lodsb                                   ;fetches byte from the cache
        mov     ah,al                           ;save value in ah for later use
        and     al,1100_0000b                   ; and use al. useful for stosb.
                                                ; and only uses the first 2 MSB bits
        or      al,al                           ;black/black?
        jz      %%print_bit

        cmp     al,0100_0000b                   ;black, white?
        jnz     %%is_10
        mov     al,0000_1111b                   ;yes, black / white
        jmp     %%print_bit

%%is_10:
        cmp     al,1000_0000b
        jnz     %%is_11
        mov     al,1111_0000b                   ;yes, white / black
        jmp     %%print_bit

%%is_11:
        mov     al,1111_1111b                   ;yes, white, white

%%print_bit:
        stosb

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
        mov     ax,gfx                          ;init segments
        mov     ds,ax
        mov     ax,data
        mov     es,ax

        cld

        call    gfx_init
        call    music_init

        call    main_loop

        call    sound_cleanup

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
gfx_init:
        mov     ax,0009h
        int     10h                             ;switch video mode

        call    palette_init

        mov     si,logo                         ;ds:si (source)
        sub     di,di
        mov     ax,0b800h
        mov     es,ax                           ;es:di (dest)

        mov     cx,16384                        ;copy 32k
        rep     movsw

        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
palette_init:
        ;logo should be turned off by default
        mov     dx,03dah                        ;select border color register
        mov     al,19h                          ;select color=9 (light blue)
        out     dx,al                           ;select palette register

        add     dx,4
        mov     al,0                            ;black now
        out     dx,al

        sub     dx,4
        mov     al,15h                          ;select color5 (cyan?)
        out     dx,al

        add     dx,4
        mov     al,0
        out     dx,al                           ;black now

        sub     dx,4
        mov     al,1ah                          ;select 10
        out     dx,al

        add     dx,4
        mov     al,0
        out     dx,al                           ;black now


        sub     dx,4
        mov     al,1dh                          ;select 0xd
        out     dx,al

        add     dx,4
        mov     al,0
        out     dx,al                           ;black now


        mov     word [es:palette_delay],300     ;wait 5 seconds before showing logo
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_init:
        mov     ax,data
        mov     ds,ax

        mov     word [pvm_offset],pvm_song + 0x10       ;update start offset
        mov     byte [pvm_wait],0               ;don't wait at start
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
wait_retrace:

        mov     dx,0x3da
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
        call    wait_retrace

        call    inc_d020

        call    music_anim
        call    palette_anim
        call    scroll_anim

        call    dec_d020

        mov     ah,1
        int     16h                             ; INT 16,AH=1, OUT:ZF=status
        jz      main_loop

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
palette_anim:
        cmp     word [palette_delay],0
        je      .animate
        dec     word [palette_delay]
        ret

.animate:
        mov     bx,word [palette_idx]
        cmp     bx,143
        ja      .end

        mov     dx,0x3da                        ;select border color register
        mov     al,0x15                         ;logo outer color
        out     dx,al                           ;select palette register

        add     dx,4
        mov     al,[palette_logo_0 + bx]
        out     dx,al

        sub     dx,4                            ;aniamte letter P
        mov     al,0x19                         ;logo inner color
        out     dx,al

        add     dx,4
        mov     al,[palette_logo_1 + bx]
        out     dx,al


        sub     dx,4                            ;animate letter V
        mov     al,0x1a                         ;logo inner color
        out     dx,al

        sub     bx,8
        add     dx,4
        mov     al,[palette_logo_1 + bx]
        out     dx,al


        sub     dx,4                            ;animate letter M
        mov     al,0x1d                         ;logo inner color
        out     dx,al

        sub     bx,8
        add     dx,4
        mov     al,[palette_logo_1 + bx]
        out     dx,al

        inc     word [palette_idx]

.end:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_anim:
        push    es                              ;no need to save ds. will be restored
                                                ; at the end of the function
        mov     ax,0b800h                       ;video memory
        mov     ds,ax
        mov     es,ax

OFFSET_Y        equ     23*2*160                ;start at line 23

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
        sub     bx,bx                           ;used for the cache index in the
                                                ; macros
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

logo:
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

palette_delay:
        dw      0                               ;cycles to wait before showing logo
palette_enabled:
        db      0                               ;boolean. whether to animate the palette
palette_idx:
        dw      0

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

palette_logo_0:                                 ;outline logo color
        db      8,8,7,7,15,15,15,15
        db      7,7,8,8,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      8,8,8,8,8,8,8,8
        db      1,1,1,1,1,1,1,1


        db      0,0,0,0,0,0,0,0                 ;buffer for letter M
        db      0,0,0,0,0,0,0,0                 ;buffer for letter P
palette_logo_1:                                 ;inner logo color
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      0,0,0,0,0,0,0,0
        db      8,8,8,8,7,7,7,7
        db      11,11,11,11,9,9,9,9

        db      9,9,9,9,9,9,9,9
        db      9,9,9,9,9,9,9,9

volume_0:
        db      1001_1111b                      ;vol 0 channel 0
        db      1011_1111b                      ;vol 0 channel 1
        db      1101_1111b                      ;vol 0 channel 2
        db      1111_1111b                      ;vol 0 channel 3

