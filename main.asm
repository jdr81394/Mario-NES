.include "./headers/consts.inc"
.include "./headers/header.inc"
.include "./headers/reset.inc"
.include "./headers/utils.inc"
.include "./datatypes/actors.inc"
.include "./datatypes/gameStates.inc"

.segment "ZEROPAGE"
MenuItem:       .res 1       ; Keep track of the menu item that is selected

Score:          .res 4       ; Score (1s, 10s, 100s, and 1000s digits in decimal)

Collision:      .res 1       ; Flag if a collision happened or not

Buttons:        .res 1       ; Pressed buttons (A|B|Sel|Start|Up|Dwn|Lft|Rgt)
PrevButtons:    .res 1       ; Stores the previous buttons from the last frame

XPos:           .res 2       ; Player X 16-bit position (8.8 fixed-point): hi+lo/256px
YPos:           .res 2       ; Player Y 16-bit position (8.8 fixed-point): hi+lo/256px

XVel:           .res 2       ; Player X (signed) velocity (in pixels per 256 frames)
YVel:           .res 2       ; Player Y (signed) velocity (in pixels per 256 frames)

PrevSubmarine:  .res 1       ; Stores the seconds that the last submarine was added
PrevAirplane:   .res 1       ; Stores the seconds that the last airplane was added

Frame:          .res 1       ; Counts frames (0 to 255 and repeats)
IsDrawComplete: .res 1       ; Flag to indicate when VBlank is done drawing
Clock60:        .res 1       ; Counter that increments per second (60 frames)

BgPtr:          .res 2       ; Pointer to background address - 16bits (lo,hi)
SprPtr:         .res 2       ; Pointer to the sprite address - 16bits (lo,hi)
BufPtr:         .res 2       ; Pointer to the buffer address - 16bits (lo,hi)
PalPtr:         .res 2       ; Pointer to the palette address - 16bits (lo,hi)

XScroll:        .res 1       ; Store the horizontal scroll position
CurrNametable:  .res 1       ; Store the current starting nametable (0 or 1)
Column:         .res 1       ; Stores the column (of tiles) we are in the level
NewColAddr:     .res 2       ; The destination address of the new column in PPU
SourceAddr:     .res 2       ; The source address in ROM of the new column tiles

ParamType:      .res 1       ; Used as parameter to subroutine
ParamXPos:      .res 1       ; Used as parameter to subroutine
ParamYPos:      .res 1       ; Used as parameter to subroutine
ParamTileNum:   .res 1       ; Used as parameter to subroutine
ParamNumTiles:  .res 1       ; Used as parameter to subroutine
ParamAttribs:   .res 1       ; Used as parameter to subroutine
ParamRectX1:    .res 1       ; Used as parameter to subroutine
ParamRectY1:    .res 1       ; Used as parameter to subroutine
ParamRectX2:    .res 1       ; Used as parameter to subroutine
ParamRectY2:    .res 1       ; Used as parameter to subroutine

ParamLoByte:     .res 1       ; Used as parameter to subroutine
ParamHiByte:     .res 1       ; Used as parameter to subroutine

PrevOAMCount:   .res 1       ; Store the previous number of bytes that were sent to the OAM

Seed:           .res 2       ; Initialize 16-bit seed to any value except 0

CurrentGameState:      .res 1       ; Keep track of game state

ActorsArray:    .res MAX_ACTORS * .sizeof(Actor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"
.include "./functions/actorFuncs.inc"
.include "./functions/backgroundFuncs.inc"
.include "./functions/controllerFuncs.inc"
.include "./functions/updateRenderDrawFuncs.inc"
.include "./functions/funcs.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    INIT_NES                 ; Macro to initialize the NES to a known state

Main:
    ;; Load Color Palette!
    jsr LoadPalette          ; Jump to subroutine LoadPalette

;; Load the Title Screen
.proc TitleScreen
    lda #<BackgroundData
    sta ParamLoByte
    lda #>BackgroundData
    sta ParamHiByte
    
    jsr LoadBackground       ; Jump to subroutine LoadBackground



    EnableNMI:
        lda #%10010000           ; Enable NMI & set background to use 2nd pattern table (at $1000)
        sta PPU_CTRL
        lda #%00011110           ; Enable sprites, enable background, no clipping on left side
        sta PPU_MASK
    
    PPURendering:
        lda #0
        sta PPU_SCROLL           ; prevents background scroll. First strobe prevents scroll in x plane
        sta PPU_SCROLL           ; have to strobe it again because its also a latch circuit. first strobe prevents scroll in y plane 

    SetGameState:
        lda #GameState::TITLE
        sta CurrentGameState
        DrawMenuArrow:
            lda #142                  ; Sprite Y position at $0200
            sta $0200
            lda #$74                 ; Sprite Tile # at $0201
            sta $0201
            lda #%00000001           ; Sprite attribs at $0202
            sta $0202
            lda #70                 ; Sprite X position at $0203
            sta $0203

        ClearButtonValues:
            lda #0 
            sta Buttons
            sta PrevButtons
            sta MenuItem
    GameLoop:
        ListenForController:
            jsr ReadControllers
        HandleControllerInput:
            CheckAButton:
                lda Buttons
                and #BUTTON_A
                beq CheckUpButton    ; If Buttons value and Button_A is equal that means it wasnt pressed..
                    lda PrevButtons
                    and #BUTTON_A
                    beq CheckUpButton
                        lda #GameState::WORLD1
                        sta CurrentGameState
                        jmp World1
            CheckUpButton:
                lda MenuItem
                cmp #0          ; If menu item is already at 0, let's not listen to this because its at top
                beq CheckDownButton
                    ; Else, let's see if it was pressed
                    lda Buttons
                    and #BUTTON_UP      ; If it wasn't pressed, check down button
                    beq CheckDownButton
                        ; Check previous button to prevent crazy movements
                        lda PrevButtons
                        and #BUTTON_UP
                        beq CheckDownButton
                        ; If it was pressed, then let's move it up
                            lda $0200               ; Get its y value that is located at $0200
                            sec 
                            sbc #17
                            sta $0200               ; Add 10 and put it back
                            lda #0
                            sta MenuItem
            CheckDownButton:
                lda MenuItem
                cmp #1      ; If menu item is already at 1, let's not listen to this because its at the bottom
                beq EndHandleControllerInput    ; beq checks to see if acumulator and cmp value are equal by checking 0 flag
                    ; lets see if it was pressed
                    lda Buttons
                    and #BUTTON_DOWN
                    beq EndHandleControllerInput
                        ; Check previous button to prevent crazy movements
                        lda PrevButtons
                        and #BUTTON_DOWN
                        beq EndHandleControllerInput
                         ; If it was pressed, then let's move it down
                            lda $0200               ; Get its y value that is located at $0200
                            clc 
                            adc #17
                            sta $0200               ; Add 10 and put it back 
                            lda #1
                            sta MenuItem
        EndHandleControllerInput:

        jmp GameLoop



.endproc

.proc World1
; GameLoop:    
;     CreateSprite0:               ; Push onto stack in order: Type, X, Y
;     lda #ActorType::SPRITE0  ; Type variable
;     sta ParamType                      ; Push type onto stack

;     lda #0                   ; X variable
;     sta ParamXPos                     ; Push X onto stack

;     lda #27                  ; Y Variable
;     sta ParamYPos                      ; Push Y onto stack

;     jsr AddNewActor        ; Push


;     jmp GameLoop          ; Force an infinite execution loop
.endproc

NMI:
    inc Frame

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Make the OAM Read
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    OAMStartDMACopy:             ; DMA copy of OAM data from RAM to PPU
        lda #$02                 ; Every frame, we copy spite data starting at $02**
        sta PPU_OAM_DMA          ; The OAM-DMA copy starts when we write to $4014

    ; TODO: increment clock 60 every 60 frames 
    lda Frame
    cmp #60
    bne Skip                  ; jump ahead if Frame does not equal 60, if does, inc Clock60 and set frame to 0 again
    inc Clock60
    lda #0
    sta Frame


Skip:
    rti                      ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRQ interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti                      ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hardcoded list of color values in ROM to be loaded by the PPU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PaletteData:
.byte $22,$29,$1A,$0F ,$22,$36,$17,$0F, $22,$30,$21,$0F, $22,$27,$17,$0F; Background
.byte $22,$16,$27,$1B ,$22,$1A,$30,$27, $22, $16,$30,$27, $22,$0F,$36,$17 ; Sprites

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Background data that must be copied to the nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BackgroundData:
.incbin "finished-mario-titlescreen2.nam"
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$36,$37,$36,$37,$36,$37,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$35,$25,$25,$25,$25,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$39,$3A,$3B,$3A,$3B,$3A,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$53,$54,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45,$53,$54,$45,$45,$53,$54,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$55,$56,$24,$24,$24,$24,$24,$24,$24,$24,$47,$47,$55,$56,$47,$47,$55,$56,$47,$47,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$60,$61,$62,$63,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$24,$31,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$64,$65,$66,$67,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$24,$30,$26,$34,$33,$24,$24,$24,$24,$36,$37,$36,$37,$24,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6A,$24,$24,$24,$24
; .byte $24,$24,$24,$24,$30,$26,$26,$26,$26,$33,$24,$24,$35,$25,$25,$25,$25,$38,$24,$24,$24,$24,$24,$24,$68,$69,$26,$6A,$24,$24,$24,$24
; .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
; .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
; .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B6
; .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7
; .byte $B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5,$B4,$B5
; .byte $B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7,$B6,$B7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes tell which palette is used by a group of tiles in the nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
AttributeData:
; .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
; .byte %00000000, %10101010, %10101010, %00000000, %00000000, %00000000, %10101010, %00000000
; .byte %00000000, %00000000, %00000000, %00000000, %11111111, %00000000, %00000000, %00000000
; .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
; .byte %11111111, %00000000, %00000000, %00001111, %00001111, %00000011, %00000000, %00000000
; .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
; .byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
; .byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here we add the CHR-ROM data, included from an external .CHR file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CHARS"
.incbin "mario.chr"         ;; pattern table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                    ; Address (2 bytes) of the NMI handler
.word Reset                  ; Address (2 bytes) of the Reset handler
.word IRQ                    ; Address (2 bytes) of the IRQ handler
