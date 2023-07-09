.include "./headers/consts.inc"
.include "./headers/header.inc"
.include "./headers/reset.inc"
.include "./headers/utils.inc"
.include "./datatypes/actors.inc"
.include "./datatypes/gameStates.inc"
.include "./datatypes/marioAnimations.inc"

.segment "ZEROPAGE"
VerticalFlag:    .res 1       ; Determines if the player has positive or negative velocity, if its greatest bit is 1, then mario is up and its negative, if 1 and its positive marios is going down. 0 is neutral
YVel:           .res 2       ; Player Y (signed) velocity (in pixels per 256 frames)

MarioAnimationStates: .res 1  ; Tells us what he is doing

MarioPeakJumpFrameCountdown: .res 1

MarioAnimationFrameCountdown: .res 1 ; Holds the frame that this has started on, also holds number of frames until peak of marios jump

Collision:      .res 1       ; Flag if a collision happened or not


XPos:           .res 2       ; Player X 16-bit position (8.8 fixed-point): hi+lo/256px
XVel:           .res 2       ; Player X (signed) velocity (in pixels per 256 frames)
HorizontalFlag:  .res 1       ; Determines if player has positive or negative velocity, if greatest bit is 1, then negative, if 1 then positive, if 0, then neutral

MarioAnimationFrame: .res 1   ; Tells us which frame he is on\


SprPtr:         .res 2       ; Pointer to the sprite address in OAM RAM - 16bits (lo,hi)   Is used in RenderingActors and in selecting proper animation frame!
RomSprPtr:      .res 2       ; Pointer to the ROM sprite address


Buttons:        .res 1       ; Pressed buttons (A|B|Sel|Start|Up|Dwn|Lft|Rgt)
PrevButtons:    .res 1       ; Stores the previous buttons from the last frame

YPos:           .res 2       ; Player Y 16-bit position (8.8 fixed-point): hi+lo/256px

VarX:           .res 1      
VarY:           .res 1


ParamXPos:      .res 1       ; Used as parameter to subroutine
ParamYPos:      .res 1       ; Used as parameter to subroutine
ParamRectX1:    .res 1       ; Used as parameter to subroutine
ParamRectY1:    .res 1       ; Used as parameter to subroutine
ParamRectX2:    .res 1       ; Used as parameter to subroutine
ParamRectY2:    .res 1       ; Used as parameter to subroutine

ActorsArray:    .res MAX_ACTORS * .sizeof(Actor)
CollidablesArray:    .res MAX_COLLIDABLES * .sizeof(Collidable)




BufPtr:         .res 2       ; Pointer to the buffer address - 16bits (lo,hi)




Frame:          .res 1       ; Counts frames (0 to 255 and repeats)
IsDrawComplete: .res 1       ; Flag to indicate when VBlank is done drawing
WaitUntilVar:    .res 1       ; Use this to compare against the Clock60

BgPtr:          .res 2       ; Pointer to background address - 16bits (lo,hi)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RAM located at $0300 until $0800
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "RAM"
Clock60:        .res 1       ; Counter that increments per second (60 frames)
PlayerOneLives: .res 1       ; Lives for Mario
Score:          .res 4       ; Score (1s, 10s, 100s, and 1000s digits in decimal)
MenuItem:       .res 1       ; Keep track of the menu item that is selected
CurrentGameState:      .res 1       ; Keep track of game state


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to manipulate the pixel placements for all the sprites properly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SpritePlacement
    ; If x equals 2, then it we need to add 8 to the x parameter
    cpx #2
    ; If it doesn't equal 2 skip ahead
    bne :+
        lda ParamXPos
        ; If it does, then add it to  the parameter
        clc 
        adc #8
        sta ParamXPos
    :

    ; lda ActorsArray+Actor::YPos,x
    ; If y equals 4, then it must be bottom left
    cpx #4
    bne :+
        ; Undo param x addition
        lda ParamXPos
        sec
        sbc #8
        sta ParamXPos

        lda ParamYPos
        clc 
        adc #8
        sta ParamYPos
    :

    
    ; If x equals 6, then it's the bottom right and we should add 8 to x again.
    ; Y param ALREADY has 8 added
    cpx #6
    bne :+
        lda ParamXPos
        ; If it does, then add it to  the parameter
        clc 
        adc #8
        sta ParamXPos
    :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all actors and send their tiles to the OAM-RAM at $200
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc RenderActors
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr                         ; Point SprPtr to $0200

    ldy #0                             ; Count how many tiles we are sending
    ldx #0                             ; Counts how many actors we are looping
    ActorsLoop:
      lda ActorsArray+Actor::Type,x
      
      cmp #ActorType::SPRITE0
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$70
        sta ParamTileNum
        lda #%00100000
        sta ParamAttribs
        lda #1
        sta ParamNumTiles 
        jsr DrawSprite                 ; Call routine to draw 1 SPRITE0 tile to the OAM
        jmp NextActor
      :
      ; Let's get mario
      cmp #ActorType::PLAYER
      beq :+
        jmp EndOfPlayer     ; branch not good enough to jump that far, so 
                            ; it really means: if branch not equal then go to end of player
      :

        ; Push the X Position into the variable first, add the offsets later
        lda ActorsArray+Actor::XPos+1,x
        sta ParamXPos

        lda ActorsArray+Actor::YPos+1,x
        sta ParamYPos       ; Store the parameter Y

        

        txa                     ; push X onto accumulator to transfer to stack
        pha                     ; push onto stack

        ldx #0                  ; Set this counter to 0 to go through mario's tiles
        ; Get mario's state, then render based on the state
        lda MarioAnimationStates
        bne EndOfStandingRight                        ; If its not 0, jump to next check
            .proc StandingRightAnimation
            ; 0 is for STANDING_RIGHT
            ; This will load from top to bottom, left first then to right: TopLeft, TopRight, BottomLeft, BottomRIght
            Loop:
                lda StandingRight0,x     ; Get tile then attribute
                sta ParamTileNum        ; Tile Num


                jsr SpritePlacement
                
                inx                     ; Go to attribute and increement
                
                lda StandingRight0,x     ; attribute
                sta ParamAttribs

                lda #1
                sta ParamNumTiles

                jsr DrawSprite

                inx
                cpx #8
                bcc Loop
                
            EndLoop:
          .endproc
        EndOfStandingRight:

        cmp #MarioAnimations::STANDING_LEFT
        bne EndOfStandingLeft
            .proc StandingLeftAnimation
                ; 0 is for STANDING_RIGHT
                ; This will load from top to bottom, left first then to right: TopLeft, TopRight, BottomLeft, BottomRIght
                Loop:
                    lda StandingLeft0,x     ; Get tile then attribute
                    sta ParamTileNum        ; Tile Num


                    jsr SpritePlacement
                    
                    inx                     ; Go to attribute and increement
                    
                    lda StandingLeft0,x     ; attribute
                    sta ParamAttribs

                    lda #1
                    sta ParamNumTiles

                    jsr DrawSprite

                    inx
                    cpx #8
                    bcc Loop
                    
                EndLoop:
            .endproc

        EndOfStandingLeft:
        
        ; Mario running right is 2
        cmp #MarioAnimations::RUNNING_RIGHT
        bne EndOfRunningRight
            .proc RunningRightAnimation
                RunningRightLoop:

                    ; Determine if we should increment the mario animation frame
                    lda MarioAnimationFrameCountdown            
                
                    ; If A > value -> carry is set, if  A < value -> carry is cleared
                    bne DetermineSprite    ; If marioAnimationFrame is not equal to 0, then we don't need to change the sprite
                        ; Update Frame
                        lda MarioAnimationFrame
                        clc
                        adc #1
                        cmp #3
                        bne :+
                            ; if equal to 3, then just put it back to 0 again
                            lda #0
                        :
                        sta MarioAnimationFrame

                        lda #FRAMES_PER_ANIMATION            ; Reload the 33 frames
                        sta MarioAnimationFrameCountdown
                    DetermineSprite:

                    lda MarioAnimationFrame
                    bne Not0                ; If its not 0 frame,
                        ; Based on the animation number we'll jump to a different memory in ROM 
                        lda RunningRight0,x
                        jmp Not2
                    Not0:
                        cmp #1
                        bne Not1
                            lda RunningRight1,x
                            jmp Not2

                    Not1:
                        cmp #2
                        bne Not2    
                            lda RunningRight2,x
                    Not2:
                        sta ParamTileNum
                        
                        jsr SpritePlacement

                        inx         ; go to the attribute now

                        ; Let's determine the attribute
                        lda MarioAnimationFrame
                        bne Not0Attribute
                            lda RunningRight0,x
                    Not0Attribute:
                        cmp #1
                        bne Not1Attribute
                            lda RunningRight1,x
                    Not1Attribute:
                        cmp #2
                        bne Not2Attribute
                            lda RunningRight2,x
                    Not2Attribute:
            
                    sta ParamAttribs
                        
                    lda #1
                    sta ParamNumTiles

                    jsr DrawSprite

                    inx
                    cpx #8
                    bcc RunningRightLoop

                EndOfRunningRightLoop:
            .endproc

        EndOfRunningRight:

        ; Mario running left is 3
        cmp #MarioAnimations::RUNNING_LEFT
        bne EndOfRunningLeft
            .proc RunningLeftAnimation
                Loop:

                    ; Determine if we should increment the mario animation frame
                    lda MarioAnimationFrameCountdown            
                
                    ; If A > value -> carry is set, if  A < value -> carry is cleared
                    bne DetermineSprite    ; If marioAnimationFrame is not equal to 0, then we don't need to change the sprite
                        ; Update Frame
                        lda MarioAnimationFrame
                        clc
                        adc #1
                        cmp #3
                        bne :+
                            ; if equal to 3, then just put it back to 0 again
                            lda #0
                        :
                        sta MarioAnimationFrame

                        lda #FRAMES_PER_ANIMATION            ; Reload the 33 frames
                        sta MarioAnimationFrameCountdown
                    DetermineSprite:

                    lda MarioAnimationFrame
                    bne Not0                ; If its not 0 frame,
                        ; Based on the animation number we'll jump to a different memory in ROM 
                        lda RunningLeft0,x
                        jmp Not2
                    Not0:
                        cmp #1
                        bne Not1
                            lda RunningLeft1,x
                            jmp Not2

                    Not1:
                        cmp #2
                        bne Not2    
                            lda RunningLeft2,x
                    Not2:
                        sta ParamTileNum
                        
                        jsr SpritePlacement

                        inx         ; go to the attribute now

                        ; Let's determine the attribute
                        lda MarioAnimationFrame
                        bne Not0Attribute
                            lda RunningLeft0,x
                    Not0Attribute:
                        cmp #1
                        bne Not1Attribute
                            lda RunningLeft1,x
                    Not1Attribute:
                        cmp #2
                        bne Not2Attribute
                            lda RunningLeft2,x
                    Not2Attribute:
            
                    sta ParamAttribs
                        
                    lda #1  
                    sta ParamNumTiles

                    jsr DrawSprite

                    inx
                    cpx #8
                    bcc Loop

                EndOfRunningLeftLoop:
            .endproc

        EndOfRunningLeft:


        cmp #MarioAnimations::JUMPING_RIGHT
        bne EndOfAirborneRight
            .proc JumpingRightAnimation
                Loop:
                    ; Only 1 animation so its simple to get the sprite
                    lda JumpingRight0,x         ; ParamTileNum
                    sta ParamTileNum

                    jsr SpritePlacement

                    inx                         ; Go to attribute

                    lda JumpingRight0,x         ; This is attribute
                    sta ParamAttribs

                    lda #1
                    sta ParamNumTiles

                    jsr DrawSprite
                    
                    inx
                    cpx #8
                    bcc Loop

                EndOfLoop:
            .endproc
        EndOfAirborneRight:

        cmp #MarioAnimations::JUMPING_LEFT
        bne EndOfAirborneLeft
            .proc JumpingLeftAnimation
                Loop:
                    ; Only 1 animation so its simple to get the sprite
                    lda JumpingLeft0,x         ; ParamTileNum
                    sta ParamTileNum

                    jsr SpritePlacement

                    inx                         ; Go to attribute

                    lda JumpingLeft0,x         ; This is attribute
                    sta ParamAttribs

                    lda #1
                    sta ParamNumTiles

                    jsr DrawSprite
                    
                    inx
                    cpx #8
                    bcc Loop

                EndOfLoop:
            .endproc
        EndOfAirborneLeft:
        
        
        ; This should always be at the end of the mario animations and should always be hit.
        pla                     ; pull old x off accumulator
        tax                     ; transfer it to the X Register
        jmp NextActor

      EndOfPlayer:
      cmp #ActorType::SUBMARINE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$04
        sta ParamTileNum
        lda #%00100000
        sta ParamAttribs
        lda #4
        sta ParamNumTiles
        jsr DrawSprite                 ; Call routine to draw 4 SUBMARINE tiles to the OAM
        jmp NextActor
      :
      cmp #ActorType::AIRPLANE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$10
        sta ParamTileNum
        lda #%00000011
        sta ParamAttribs
        lda #3
        sta ParamNumTiles
        jsr DrawSprite                 ; Call routine to draw 3 AIRPLANE tiles to the OAM
        jmp NextActor
      :
      cmp #ActorType::MISSILE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$50
        sta ParamTileNum
        lda #%00000001
        sta ParamAttribs
        lda #1
        sta ParamNumTiles
        jsr DrawSprite                 ; Call routine to draw 1 MISSILE tile to the OAM
        jmp NextActor
      :
      NextActor:
        txa
        clc
        adc #.sizeof(Actor)
        tax
        cmp #MAX_ACTORS * .sizeof(Actor)
        
        beq :+
          jmp ActorsLoop               ; Use absolute jump to avoid branch limit of [-128..127]
        :
      
        tya
        pha                            ; Save the Y register to the stack

      LoopTrailingTiles:
        cpy PrevOAMCount
        bcs :+
          sta (SprPtr),y               ; Set Y position to $FF (to hide tile)
          iny
          sta (SprPtr),y               ; Set tile number as $FF
          iny
          sta (SprPtr),y               ; Set attribs as $FF
          iny
          sta (SprPtr),y               ; Set X position to $FF (to hide tile)
          iny
          jmp LoopTrailingTiles
        :

        pla                            ; Save the previous value of Y into PrevOAMCount
        sta PrevOAMCount               ; This is the total number of bytes that we just sent to the OAM

    rts
.endproc
.include "./functions/actorFuncs.inc"
.include "./functions/backgroundFuncs.inc"
.include "./functions/controllerFuncs.inc"
.include "./functions/updateRenderDrawFuncs.inc"
.include "./functions/collisionFuncs.inc"

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
            ; Set mario to standing
            lda #MarioAnimations::STANDING_RIGHT
            sta MarioAnimationStates

            lda #ActorType::PLAYER      ; Actor type
            sta ParamType
            lda #50                 ; x
            sta ParamXPos           
            sta XPos+1
            lda #10
            sta ParamYPos           ; y 
            sta YPos+1
            jsr AddNewActor

        
        lda #GameState::WORLD1
        sta CurrentGameState

        ; Set fall to 0 current
        lda #0
        sta MarioPeakJumpFrameCountdown

        ; TODO: Mario falls from sky so perhaps remove this after the falling is done
        lda #1
        sta VerticalFlag


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


    ; If Mario is animating, then let's decrement the animation frame countdown
    lda MarioAnimationStates
    cmp #1                      ; If equal to or less than 1, he must be standing left or right 
    bcc :+                      ; If carry clear, then it means accumulator is one of those so just skip
        ; Also let's take into account a weird edge case:
        ; if MarioAnimationFrameCountdown is already 0, just skip
        lda MarioAnimationFrameCountdown
        beq :+
            dec MarioAnimationFrameCountdown    ; Decrement the countdown by 1
    :

    lda MarioPeakJumpFrameCountdown
    beq :+                  ; If it is equal to 0
        dec MarioPeakJumpFrameCountdown
    :



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
;; Mario Animation Frames - Action - Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StandingRight0:
.proc StandingRight0Scope
    TopLeft:
        .byte $32                       ; Sprite Tile #
        .byte %00000000
    TopRight:
        .byte $33                       ; index 2 - Sprite Tile #
        .byte %00000000
    BottomLeft:
        .byte $4F                       ; index 4
        .byte %00000000
    BottomRight:
        .byte $4F                      ; index 6 - This is just flipped
        .byte %01000000
.endproc


StandingLeft0:
.proc StandingLeft0Scope
    TopLeft:
        .byte $33                       ; index 2 - Sprite Tile #
        .byte %01000000
    TopRight:
        .byte $32                       ; Sprite Tile #
        .byte %01000000
    BottomLeft:
        .byte $4F                       ; index 4
        .byte %00000000
    BottomRight:
        .byte $4F                      ; index 6 - This is just flipped
        .byte %01000000
.endproc

RunningLeft0:
.proc RunningLeft0Scope
    TopLeft:
        .byte $33                   ; Sprite tile
        .byte %01000000
    TopRight:
        .byte $32
        .byte %01000000
    BottomLeft:
        .byte $3C
        .byte %01000000
    BottomRight:
        .byte $3B
        .byte %01000000
.endproc

RunningLeft1:
.proc RunningLeft1Scope
    TopLeft:
        .byte $33                       ; index 2 - Sprite Tile #
        .byte %01000000
    TopRight:
        .byte $32                       ; Sprite Tile #
        .byte %01000000

    BottomLeft:
        .byte $39                      ; index 6 
        .byte %01000000
    BottomRight:
        .byte $38                       ; index 4
        .byte %01000000

.endproc

RunningLeft2:
.proc RunningLeft2Scope
    TopLeft:
        .byte $33                       ; index 2 - Sprite Tile #
        .byte %01000000
    TopRight:
        .byte $32                       ; Sprite Tile #
        .byte %01000000
    BottomLeft:
        .byte $35                      ; index 6 
        .byte %01000000
    BottomRight:
        .byte $34                       ; index 4
        .byte %01000000

.endproc



RunningRight0:
.proc RunningRight0Scope
    TopLeft:
        .byte $32                       ; Sprite Tile #
        .byte %00000000
    TopRight:
        .byte $33                       ; index 2 - Sprite Tile #
        .byte %00000000
    BottomLeft:
        .byte $3B                       ; index 4
        .byte %00000000
    BottomRight:
        .byte $3C                      ; index 6 
        .byte %00000000
.endproc

RunningRight1:
.proc Running1Scope
    TopLeft:
        .byte $32                       ; Sprite Tile #
        .byte %00000000
    TopRight:
        .byte $33                       ; index 2 - Sprite Tile #
        .byte %00000000
    BottomLeft:
        .byte $38                       ; index 4
        .byte %00000000
    BottomRight:
        .byte $39                      ; index 6 
        .byte %00000000
.endproc
RunningRight2:
.proc Running2Scope
    TopLeft:
        .byte $32                       ; Sprite Tile #
        .byte %00000000
    TopRight:
        .byte $33                       ; index 2 - Sprite Tile #
        .byte %00000000
    BottomLeft:
        .byte $34                       ; index 4
        .byte %00000000
    BottomRight:
        .byte $35                      ; index 6 
        .byte %00000000
.endproc


JumpingRight0:
    .proc JumpingRight0Scope
        TopLeft:
            .byte $32                       ; Sprite Tile #
            .byte %00000000
        TopRight:
            .byte $33                       ; index 2 - Sprite Tile #
            .byte %00000000
        BottomLeft:
            .byte $42                       ; index 4
            .byte %00000000
        BottomRight:
            .byte $43                      ; index 6 
            .byte %00000000
    .endproc

JumpingLeft0:
    .proc JumpingLeft0Scope
        TopLeft:
            .byte $33                       ; index 0 - Sprite Tile #
            .byte %01000000
        TopRight:
            .byte $32                       ; Sprite Tile #
            .byte %01000000
        BottomLeft:
            .byte $43                      ; index 4
            .byte %01000000
        BottomRight:
            .byte $42                       ; index 6
            .byte %01000000

    .endproc


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
    .byte %00000000, %00000000, %00000000, %00001111, %00001111, %00000011, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000011
    .byte %11110000, %11110000, %11110000, %11110011, %11111111, %11110000, %11110000, %11110000
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
