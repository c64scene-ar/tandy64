; Tandy64
; http://pungas.space
;
bits    16
cpu     8086

extern ZTimerOn, ZTimerOff, ZTimerReport

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; CODE
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text

global intro_start
intro_start:
        mov     ax,gfx                          ;init segments
        mov     ds,ax
        mov     ax,music
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


        mov     word [es:palette_delay],300     ;wait 5 seconds before showing logo
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
music_init:
        mov     ax,music
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
        cmp     bx,127
        ja      .end

        mov     dx,0x3da                        ;select border color register
        mov     al,0x15                         ;logo outer color
        out     dx,al                           ;select palette register

        add     dx,4
        mov     al,[palette_logo_0 + bx]
        out     dx,al

        sub     dx,4
        mov     al,0x19                         ;logo inner color
        out     dx,al

        add     dx,4
        mov     al,[palette_logo_1 + bx]
        out     dx,al

        inc     word [palette_idx]

.end:
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
scroll_anim:
        push    ds
        push    es

        mov     ax,0b800h
        mov     ds,ax
        mov     es,ax

OFFSET_Y        equ     22*2*160

        %assign i 0
        %rep    6

                mov     cx,159                  ;scroll 1 line of 80 chars
                mov     si,OFFSET_Y+i*160+1     ;source: last char of screen
                mov     di,OFFSET_Y+i*160       ;dest: last char of screen - 1
                rep movsb                       ;do the copy

                mov     cx,159                  ;scroll 1 line of 80 chars
                mov     si,OFFSET_Y+8192+i*160+1;source: last char of screen
                mov     di,OFFSET_Y+8192+i*160  ;dest: last char of screen - 1
                rep movsb                       ;do the copy

                mov     cx,159                  ;scroll 1 line of 80 chars
                mov     si,OFFSET_Y+16384+i*160+1       ;source: last char of screen
                mov     di,OFFSET_Y+16384+i*160         ;dest: last char of screen - 1
                rep movsb                       ;do the copy

                mov     cx,159                   ;scroll 1 line of 80 chars
                mov     si,OFFSET_Y+24576+i*160+1            ;source: last char of screen
                mov     di,OFFSET_Y+24576+i*160              ;dest: last char of screen - 1
                rep movsb                       ;do the copy

        %assign i i+1
        %endrep

        pop     es
        pop     ds

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
; DATA MUSIC
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .music data

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
palette_idx
        dw      0

charset:
        incbin 'src/font_unknown_2x2-charset.bin'

scroll_text:
        db 'hola como estas'
        db 0
scroll_char_idx:                                ;pointer to the next char
        db 0
scroll_bit_idx:                                 ;pointer to the next bit in the char
        db 0

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
        db      8,8,8,8,8,8,8,8
        db      1,1,1,1,1,1,1,1

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

volume_0:
        db      1001_1111b                      ;vol 0 channel 0
        db      1011_1111b                      ;vol 0 channel 1
        db      1101_1111b                      ;vol 0 channel 2
        db      1111_1111b                      ;vol 0 channel 3
