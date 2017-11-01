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
..start:
        sub     ax,ax
        sub     al,al
        sub     ah,ah
        sub     cx,cx
        sub     cl,cl
        sub     ch,ch
        sub     di,di
        sub     si,si
        sub     bp,bp

        mov     ax,4c00h
        int     21h                             ;exit to DOS

