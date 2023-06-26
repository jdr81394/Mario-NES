.include "./headers/consts.inc"
.include "./headers/header.inc"
.include "./headers/reset.inc"
.include "./headers/utils.inc"
.include "./datatypes/actors.inc"
.include "./datatypes/gameStates.inc"

.segment "ZEROPAGE"

Collision:      .res 1       ; Flag if a collision happened or not

Buttons:        .res 1       ; Pressed buttons (A|B|Sel|Start|Up|Dwn|Lft|Rgt)
PrevButtons:    .res 1       ; Stores the previous buttons from the last frame

YPos:           .res 2       ; Player Y 16-bit position (8.8 fixed-point): hi+lo/256px

HoldX:           .res 1      
HoldY:           .res 1
HoldA1:         .res 1
HoldX2:          .res 1


ParamXPos:      .res 1       ; Used as parameter to subroutine
ParamYPos:      .res 1       ; Used as parameter to subroutine
ParamRectX1:    .res 1       ; Used as parameter to subroutine
ParamRectY1:    .res 1       ; Used as parameter to subroutine
ParamRectX2:    .res 1       ; Used as parameter to subroutine
ParamRectY2:    .res 1       ; Used as parameter to subroutine


CollidablesArray:    .res MAX_COLLIDABLES * .sizeof(Collidable)



XPos:           .res 2       ; Player X 16-bit position (8.8 fixed-point): hi+lo/256px
XVel:           .res 2       ; Player X (signed) velocity (in pixels per 256 frames)

BufPtr:         .res 2       ; Pointer to the buffer address - 16bits (lo,hi)

MenuItem:       .res 1       ; Keep track of the menu item that is selected

PlayerOneLives: .res 1       ; Lives for Mario

Score:          .res 4       ; Score (1s, 10s, 100s, and 1000s digits in decimal)



YVel:           .res 2       ; Player Y (signed) velocity (in pixels per 256 frames)


Frame:          .res 1       ; Counts frames (0 to 255 and repeats)
IsDrawComplete: .res 1       ; Flag to indicate when VBlank is done drawing
Clock60:        .res 1       ; Counter that increments per second (60 frames)
WaitUntilVar:    .res 1       ; Use this to compare against the Clock60

BgPtr:          .res 2       ; Pointer to background address - 16bits (lo,hi)
SprPtr:         .res 2       ; Pointer to the sprite address - 16bits (lo,hi)
PalPtr:         .res 2       ; Pointer to the palette address - 16bits (lo,hi)

XScroll:        .res 1       ; Store the horizontal scroll position
CurrNametable:  .res 1       ; Store the current starting nametable (0 or 1)
Column:         .res 1       ; Stores the column (of tiles) we are in the level
NewColAddr:     .res 2       ; The destination address of the new column in PPU
SourceAddr:     .res 2       ; The source address in ROM of the new column tiles

ParamType:      .res 1       ; Used as parameter to subroutine

ParamTileNum:   .res 1       ; Used as parameter to subroutine
ParamNumTiles:  .res 1       ; Used as parameter to subroutine
ParamAttribs:   .res 1       ; Used as parameter to subroutine

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
    jsr LoadPaletteNameTable0          ; Jump to subroutine LoadPalette

;; Load the Title Screen
.proc TitleScreen
    lda #<TitleScreenData     ; Lo byte
    sta ParamLoByte
    lda #>TitleScreenData     ; Hi byte
    sta ParamHiByte

    jsr LoadNameTable0       ; Jump to subroutine LoadBackground

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
                        ; Clean up the arrow to select the stage
                        lda #$FF
                        sta $0200
                        sta $0201
                        sta $0202
                        sta $0203
                        lda #3
                        sta PlayerOneLives
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
    LoadBlankScreen:

        lda #0
        sta PPU_CTRL    ; freezes the vblank
        sta PPU_MASK    ; doesn't show background

        lda #<BlankScreenData
        sta ParamLoByte
        lda #>BlankScreenData
        sta ParamHiByte

        jsr LoadNameTable1       ; Jump to subroutine LoadBackground

        lda #%10010000           ; Enable NMI & set background to use 2nd pattern table (at $1000)
        sta PPU_CTRL
        lda #%00011110           ; Enable sprites, enable background, no clipping on left side
        sta PPU_MASK
    
        lda #255
        sta PPU_SCROLL
        lda #0
        sta PPU_SCROLL
    

    CreateMario:
        ; Load Top Lft Head of mario, the tile is $32
        TopLeft:
            lda #142                  ; Sprite Y position at $0200
            sta $0200
            
            lda #$32                 ; Sprite Tile # at $0201
            sta $0201
            
            lda #%00000010           ; Sprite attribs at $0202, [flip vert] [flip hor] [priority: 0 in front, 1 behind background] [?] [?] [?] [colorpalette][colorpalette aswell]
            sta $0202
            
            lda #70                 ; Sprite X position at $0203
            sta $0203

        TopRight:
            ; Load Top Right Head of mario, the tile is $33
            lda #142            ; Y position
            sta $0204
            
            lda #$33             ; TOp Right Head 
            sta $0205

            lda #%00000010           ; Sprite attribs at $0202, [flip vert] [flip hor] [priority: 0 in front, 1 behind background] [?] [?] [?] [colorpalette][colorpalette aswell]
            sta $0206

            lda #78
            sta $0207
        
        BottomLeft:
            ; Load Botom Right Part of Mario
            lda #150
            sta $0208

            lda #$42        ; botom left leg
            sta $0209
            
            lda #%00000010 
            sta $020A

            lda #70
            sta $020B

        BottomRight:
            ; Load Botom Right Part of Mario
            lda #150
            sta $020C

            lda #$43        ; botom left leg
            sta $020D
            
            lda #%00000010 
            sta $020E

            lda #78
            sta $020F

    ; TOOD BUFFERING NOT WORKING FIGURE OUT LATER
    ; PutLivesIntoBackgroundBuffer:
    ;     ; Set 0 into buffer
    ;     lda #0
    ;     sta $7004
    ;     ; First see how many digits the players live are
    ;     lda PlayerOneLives
    ;     cmp #10 
    ;     bcc OnesDigit  ; If it is less than 10, do not add the second digit, skip ahead
    ;         ; Else, lets put hte first digit into the buffer
    ;         ; Now get the specific value, its a product of 10
    ;         TensDigit:
    ;             ldx #1      ; Set counter, it starts at 1 for 10
    ;             Loop:
    ;                 inx
    ;                 sec
    ;                 sbc #10         ; subtract 10 from player one lives
    ;                 cmp #10         ; If it is greater than 10, loop, else, get that digit in x
    ;                 bcs Loop
    ;                 ; else, let's get that , put x into the address
    ;                 stx $7007
    ;             EndLoop:
    ;             lda #1          ; Length is one
    ;             sta $7004

    ;             lda #$50       ; Address on nametable
    ;             sta $7005       

    ;             lda #$24
    ;             sta $7006

    ;             ; Length 0 now to signal end of buffer
    ;             lda #0
    ;             sta $7008

    ;     OnesDigit:
    ;         lda #1          ; Set the length
    ;         sta $7000

    ;         lda #$00       ; Address on the name table
    ;         sta $7001

    ;         lda #$24
    ;         sta $7002

    ;         ; Let's do another comparison to see if greater than 10
    ;         lda PlayerOneLives
    ;         cmp #10
    ;         bcc :+      ; If it is less, than just jump
    ;             sec 
    ;             sbc #10
    ;         :
    ;         sta  $7003






    

    LoadUpWorld1:
        lda Clock60
        clc
        adc #3          ; Wait 3 seconds
        sta WaitUntilVar
        ; Waste a few seconds
        Loop:
            lda Clock60
            cmp WaitUntilVar
            bne Loop
        EndLoop:

        
        ; Prep for background change
        lda #0
        sta PPU_CTRL    ; freezes the vblank
        sta PPU_MASK    ; doesn't show background

        ; Clean up mario from previous frame, mario data goes up to #$0F
        CleanMario:
            lda #$02
            sta SprPtr+1
            lda #$00
            sta SprPtr

            lda #$FF
            ldy #0
            ClearMarioLoop:
                sta (SprPtr),y
                iny
                cpy #$A0
                bne ClearMarioLoop

        ; Change the background
        lda #<Screen1Data
        sta ParamLoByte
        lda #>Screen1Data
        sta ParamHiByte

        jsr LoadNameTable0       ; Jump to subroutine LoadBackground

        lda #%10010000           ; Enable NMI & set background to use 2nd pattern table (at $1000)
        sta PPU_CTRL
        lda #%00011110           ; Enable sprites, enable background, no clipping on left side
        sta PPU_MASK
    
        ; Move it back to first name table
        lda #0
        sta PPU_SCROLL
        lda #0
        sta PPU_SCROLL

        ; CreateSprite0:               ; Push onto stack in order: Type, X, Y
        ;     lda #ActorType::SPRITE0  ; Type variable
        ;     sta ParamType                      ; Push type onto stack
        ;     lda #0
        ;     sta ParamXPos
        ;     lda #20
        ;     sta ParamYPos
        ;     jsr AddNewActor
        
        CreatePlayer:
            lda #ActorType::PLAYER      ; Actor type
            sta ParamType
            lda #50                 ; x
            sta ParamXPos           
            sta XPos
            lda #10
            sta ParamYPos           ; y 
            sta YPos
            jsr AddNewActor

        
        lda #GameState::WORLD1
        sta CurrentGameState


    GameLoop:

        jsr ReadControllers
        jsr HandleControllerInput

        ; Jake Call Gravity Subroutine, pass Actor's Y Position to Param
        jsr ImplementGravity
        jsr UpdateActors
        jsr RenderActors

    ; We want to clamp the GameLoop since it can happen many times per frame
    ClampGameLoop:
        lda IsDrawComplete
        WaitForVBlank:
            cmp IsDrawComplete
            beq WaitForVBlank

        lda #0
        sta IsDrawComplete

        jmp GameLoop
        
    
.endproc

NMI:
    SetDrawComplete:
    lda #1
    sta IsDrawComplete
    inc Frame

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Make the OAM Read
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    OAMStartDMACopy:             ; DMA copy of OAM data from RAM to PPU
        lda #$02                 ; Every frame, we copy spite data starting at $02**
        sta PPU_OAM_DMA          ; The OAM-DMA copy starts when we write to $4014


    jsr BackgroundCopy


    ; Reset collision
    ; lda #0
    ; sta Collision


    ; Jake THIS IS FREEZING GAME FIGURE OUT LATER JAKE
    lda CurrentGameState        ; If it is equal to 0 then its in the title and there is no sprite 0 so we can skip          
    beq FinishSprite0
            
        lda XScroll
        sta PPU_SCROLL
        lda #0
        sta PPU_SCROLL

    ; WaitForNoSprite0:
    ;     lda PPU_STATUS
    ;     and #%01000000           ; PPU address $2002 bit 6 is the sprite 0 hit flag
    ;     bne WaitForNoSprite0     ; Loop until we do *not* have a sprite 0 hit

    ; WaitForSprite0:
    ;     lda PPU_STATUS
    ;     and #%01000000           ; PPU address $2002 bit 6 is the sprite 0 hit flag
    ;     beq WaitForSprite0       ; Loop until we do have a sprite 0 hit

    FinishSprite0:


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
TitleScreenData:
.incbin "finished-mario-titlescreen2.nam"

BlankScreenData:
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$29,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$50,$27,$27,$27,$27,$27,$04,$44,$15,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
.byte $27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27,$27
EndBlankScreen:

Screen1Data:
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$31,$32,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$30,$26,$34,$33,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$30,$26,$34,$26,$34,$33,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$36,$37,$36,$37,$24,$24
    .byte $24,$24,$30,$26,$26,$26,$26,$26,$26,$33,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$25,$25,$25,$25,$38,$24
    .byte $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45
    .byte $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47
    AttributeData:
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %10101010, %10101010, %00000000, %00000000, %00000000, %10101010, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %11111111, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %11111111, %00000000, %00000000, %00001111, %00001111, %00000011, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
    .byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
    .byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111


EndScreen1:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes tell which palette is used by a group of tiles in the nametable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AttributeData:
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
