;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 32 color palette values from ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadPaletteNameTable0
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
;; Subroutine to load all 32 color palette values from ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc LoadPaletteNameTable1
    PPU_SETADDR $3F20
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
;; 3 Parameters
.proc LoadNameTable0
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

    ; Now let's check if its one of the collidable
    jsr AddToCollidablesArray

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
;; Subroutine to load all 255 tiles in the second nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3 Parameters
.proc LoadNameTable1
    lda ParamLoByte      ; Fetch lo byte
    sta BgPtr
    lda ParamHiByte      ; Fetch hi byte
    sta BgPtr+1

    PPU_SETADDR $2400
    ldx #0
    ldy #0                   ; Y = 0
OuterLoop:
InnerLoop:
    lda (BgPtr),y        ;   ; Lookup byte in ROM        The () syntax is to going to interpret this as a 16 bit address and then deference the address. The Y register is the only register that can use this syntax due to the way things are wired within the 6502 processor.
    sta PPU_DATA             ; Set value to send to PPU_DATA

    ; Now let's check if its one of the collidables

    jsr AddToCollidablesArray

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
;; Subroutine to check tile and add it to the collidables array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddToCollidablesArray

    ; If there is a brick, that is 30, above it that means it is directly above and we do not need to add it
    ; TODO Also we will need to exclude if it is in the attribute table.. we'll handle that in a bit
    
    ; Possible collidables: $47
    DetermineIfCollidable:
        ; Accumulator has the value to compare, it is already in the accumulator
        ; Is it a block we want? Yes
        cmp #$45
        bne CheckNext      ; If not equal, check next value
            ; TODO If it does equal , then let's see if there is a collidable above it, to the left, and to the right 
            ; push it into the collidables array

            
            ; WE DO NOT NEED TO PUSH ACCUMULATOR TO STACK THERE IS NOTHING IMPORTANT IN THERE FROM PREVIOUS FUNCTION

            txa                 ; transfer x to accumulator
            pha                 ; push x in accumulator to stack
            sta HoldX

            ; Since each screen is 32 columns long ( its also 8 pixels wide per column )
            ; In order to get the x placement we need to load up the 255 bytes are find the remainder value
            ; Also, for every power of 8 it goes over it will also increase the Y position by 8
            
            ; Divide by 32 until it is less than 32 to find the x position
            tya                  ; Transfer y ( little loop ) to accumulator
            sta HoldY             ; hold the little loop

            DetermineXParam:
                ldy #0                  ; This will be used to help us get a precise Y value 
                Loop:
                    cmp #32              ; If it is greater than 32, subtract 32 by looping
                    bcc SetXParam
                        iny              ; This increments Y by 1 
                        sec
                        sbc #32
                        jmp Loop
        
                SetXParam:
                    tax                 ; Transfer accumulator to 8
                    lda #0              ; Set it equal to 0
                    ; Now in the accumulator, we have the x placement of the tile we just need to multiply this by 8 for the pixels
                    AddLoop:
                        clc 
                        adc #8
                        ; Decrement y one time to move up the x position that it will be at
                        dex
                        bne AddLoop ; If not equal to 0, loop again

                    ; Finally, store the ParamXPos
                    sta ParamXPos
            
            DetermineYParam:
                pla                 ; PUll the old x off the stack  that has the counter for the outer loop ( Big loop )
                tax                 ; Transfer the accumulator that has the old X ( big loop counter ) to X
                tya                 ; Transfer Y to accumulator with precise Y value within the bands 
                ; X register = x counter (big loop counter) , accumulator has precise y value within bands
                SetYLoop:
                    cpx #0
                    beq FinishYLoop
                        clc 
                        adc #64     ; This is 8 rows of pixels which is 8x8 = 64
                        dex
                        jmp SetYLoop
                FinishYLoop:

            sta ParamYPos
                
            ; Must preserve the x value, the a value in the previous function is already used, y is not touched
            ; txa     ; Transfer X to Accumulator
            ; pha     ; Push X on accumulator to stack
            jsr AddNewCollidable
            ; pla     ; Pull old X off stack onto accumulator
            ; tax     ; Transfer the X value on the accumulator to the stack
            ; Pull off the original values that were pushed onto the stack
            ldx HoldX
            ldy HoldY

        CheckNext:

    rts
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to load all 16 bytes for the first name table 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .proc LoadAttributes
;     ldy #0                   ; Y = 0
; :
;     lda AttributeData,y        ; Lookup byte in ROM
;     sta PPU_DATA             ; Set value to send to PPU_DATA
;     iny                      ; Y++
;     cpy #16                  ; Is Y equal to 255?
;     bne :-          ; Not yet, keep looping
;     rts                      ; Return from subroutine
; .endproc

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
    rts
.endproc