; Very simple VGM Player for Tandy 1000
; Only supports VGM v1.50, only output is SN76489, and only Freq is NTSC
;
; riq/pvm

bits    16
cpu     8086

; Timing settings:
IRQ_rate        equ 60
PIT_divider     equ 19886                       ; 1234DCh / IRQ_rate

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; CODE
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .text code
..start:

main:
        mov     ax,data                         ;init segments
        mov     ds,ax                           ; DS=ES: same segment
        mov     es,ax                           ; SS: stack
        mov     ax,stack
        cli                                     ;disable interrupts while
        mov     ss,ax                           ; setting the stack pointer
        mov     sp,stacktop
        sti

        ; Load the old INT 08 vector
        ; and install our own

        push    ds
        sub     ax,ax
        mov     ds,ax

        cli
        mov     ax,new_i08
        mov     dx,cs
        xchg    ax,[ds:8*4]
        xchg    dx,[ds:8*4+2]
        mov     [cs:old_i08],ax
        mov     [cs:old_i08+2],dx

        pop     ds

        ; Configure the PIT to
        ; issue IRQ at 60 Hz rate
        mov     ax,PIT_divider
        call    setup_PIT
        sti

        call    wait_key
        call    player_main

        cli
        ; Reset PIT to defaults (~18.2 Hz)
        mov    ax,0                             ;actually means 10000h
        call   setup_PIT

         ; Restore the old INT 08 vector
        xor     ax,ax
        mov     ds,ax
        les     si,[cs:old_i08]
        mov     [ds:8*4],si
        mov     [ds:8*4 + 2],es
        sti

        ; Terminate program
        mov     ax,4C00h
        int     21h                             ; INT 21, AH=4Ch, AL=exit code

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
player_main:
        mov     word [pvm_offset],pvm_song + 0x0c       ;update start offset

        ; Main loop
.mainloop:
        hlt ; wait for IRQ
        ; Check it was timer IRQ
        mov     al,0
        xchg    al,[cs:IRQ_ticked]
        test    al,al
        jz      .l2
        ; It was; advance the song.
        call    song_tick

.l2:
        ; Loop until some input is given
        mov     ah, 1
        int     16h                             ; INT 16,AH=1, OUT:ZF=status
        jz      .mainloop

        ; Read the input key
        xor     ax, ax
        int     16h                             ; INT 16,AH=0, OUT:AX=key
        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
wait_key:
        xor     ah,ah                           ;Function number: get key
        int     0x16                            ;Call BIOS keyboard interrupt
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; HARDWARE I/O ROUTINES
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;

setup_PIT:
        ; AX = PIT clock period
        ;          (Divider to 1193180 Hz)
        push    ax
        mov     al,34h
        out     43h,al
        pop     ax
        out     40h,al
        mov     al,ah
        out     40h,al
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
song_tick:

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
        out     0xc0,al
        loop    .repeat

        jmp     .l0                             ; repeat... fetch next command


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.is_delay:
        int     3
        dec     ah                              ;minus one, since we are returning
        mov     [pvm_wait],ah                   ; from here now
        mov     [pvm_offset],si
        ret

.is_delay_extra:
        int     3
        lodsb                                   ;fetch wait from next byte
        dec     al                              ;minus one, since we are returning
        mov     [pvm_wait],al                   ; from here now
        mov     [pvm_offset],si
        ret

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
.is_end:
        int     3
        mov     byte [pvm_wait], 5              ;wait 5 cycles before starting again
        mov     word [pvm_offset], pvm_song + 0xc      ; beginning of song

        ret


;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; IRQ
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
new_i08:; New INT 08 (timer IRQ) handler
        push    ax
        mov     byte [cs:IRQ_ticked],1
        add     word [cs:i08_counter],PIT_divider
        jnc     skip_old_i08
        pop     ax
        db      0EAh                            ; Jump far
old_i08:        dd 0                            ; Old INT 08 vector
skip_old_i08:
        mov     al,20h                          ; Send the EOI signal
        out     20h,al                          ; to the IRQ controller
        pop     ax
        iret                                    ; Exit interrupt

IRQ_ticked:     db 0
i08_counter:    dw 0
; I08counter makes it possible to call the
; the old IRQ vector at the right rate.
; At every INT, it is incremented by:
;       10000h * (oldrate/newrate)
; Which happens to evaluate into the same
; as PITdivider when the oldrate is the
; standard ~18.2 Hz. Whenever it overflows,
; it's time to call the old IRQ handler.
; This ensures that the old IRQ handler is
; called at the standard 18.2 Hz rate.

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; section DATA
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .data data

pvm_song:
;        incbin "music_test.pvm"
;        incbin "anime.pvm"
;        incbin "anime.pvm"
        incbin "src/uctumi-song.pvm"

pvm_wait:                                       ; cycles to read diviced 0x2df
        db 0
pvm_offset:                                     ; pointer to next byte to read
        dw 0

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
; section STACK
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-;
section .stack stack
        resb 4096
stacktop:
