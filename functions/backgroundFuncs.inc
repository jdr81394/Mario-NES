;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 32 color palette values from ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadPalette
    PPU_SETADDR $3F00
    ldy #0                   ; Y = 0
:
    lda PaletteData,y        ; Lookup byte in ROM        The () syntax is to going to interpret this as a 16 bit address and then deference the address. The Y register is the only register that can use this syntax due to the way things are wired within the 6502 processor.
    sta PPU_DATA             ; Set value to send to PPU_DATA
    iny                      ; Y++
    cpy #32                  ; Is Y equal to 32?
    bne :-          ; Not yet, keep looping
    rts                      ; Return from subroutine
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 255 tiles in the first nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadBackground
    lda ParamLoByte      ; Fetch lo byte
    sta BgPtr
    lda ParamHiByte      ; Fetch hi byte
    sta BgPtr+1

    PPU_SETADDR $2000
    ldx #0
    ldy #0                   ; Y = 0
OuterLoop:
InnerLoop:
    lda (BgPtr),y        ;   ; Lookup byte in ROM        The () syntax is to going to interpret this as a 16 bit address and then deference the address. The Y register is the only register that can use this syntax due to the way things are wired within the 6502 processor.
    sta PPU_DATA             ; Set value to send to PPU_DATA
    iny                      ; Y++
    cpy #0                 ; Is Y equal to 0, did it wrap around?
    beq IncreaseHiByte                   ; Not yet, keep looping
    jmp InnerLoop
IncreaseHiByte:
    inc BgPtr+1              ; add 1 to the pointer
    inx                       ; See if not equal  
    cpx #4                      ; increment it by 1 anyway
    bne OuterLoop                  ; check the results of the if statement from previous x, if previous x = 3, jump from subroutine
    rts                      ; Return from subroutine
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 16 bytes for the first name table 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadAttributes
    ldy #0                   ; Y = 0
:
    lda AttributeData,y        ; Lookup byte in ROM
    sta PPU_DATA             ; Set value to send to PPU_DATA
    iny                      ; Y++
    cpy #16                  ; Is Y equal to 255?
    bne :-          ; Not yet, keep looping
    rts                      ; Return from subroutine
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle changing background tiles through buffering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw the score in the nametable/background using buffering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer format starting at memory address $7000:
;;
;; 03 20 52 00 00 02 01 20  78 00 00
;;  | \___/ \______/  | \___/   |  |
;;  |   |      |      |   |     |  |
;;  |   |      |      |   |     |  Length=0 (end of buffering)
;;  |   |      |      |   |     byte to copy
;;  |   |      |      |   PPU Address $2078
;;  |   |      |      Length=1
;;  |   |       bytes to copy
;;  |   PPU Address $2052
;;  Length=3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc BackgroundCopy
BackgroundCopy:
    lda #$70
    sta BufPtr+1             
    lda #$00
    sta BufPtr+0             ; Set BufPtr pointer to start at address $7000

    ldy #$00

    BufferLoop:    
        lda (BufPtr),y         ; Fetch the length;
        beq EndBackgroundCopy   ; 

        tax                     ; x  = length

        iny
        lda (BufPtr),y           ; Fetch hi-byte of PPU address to be updated
        sta PPU_ADDR
        iny
        lda (BufPtr),y           ; Fetch lo-byte of PPU address to be updated
        sta PPU_ADDR
        iny
    DataLoop:
        lda (BufPtr),y
        sta PPU_DATA
        iny
        dex                      ; X--
        bne DataLoop

        jmp BufferLoop           ; Loop back until we finish the buffer (find an entry with Length=0)
EndBackgroundCopy:

.endproc