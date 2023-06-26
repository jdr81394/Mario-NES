
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all actors and update them (position, velocity, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc UpdateActors
    ldx #0
    ActorsLoop:
      lda ActorsArray+Actor::Type,x

      cmp #ActorType::PLAYER
      bne FinishActor
        lda XVel
        bpl :+
          dec XPos+1                        ; This does a sign extension to keep it either +/-
        :
        clc
        adc XPos
        sta ActorsArray+Actor::XPos,x       ; Store into the actors array so it can be rendered   
        sta XPos
        lda #0
        adc XPos+1
        sta XPos+1
        sta ActorsArray+Actor::XPos+1,x


        txa       ; transfer x to a
        pha       ; push to stack

        jsr CheckCollidableCollision

        pla      ; pull old x off stack
        tax      ; push it back onto x
        
        lda Collision
        ; If collision, then collision will not be equal to 0, and we want to use this value, so jump to sta, 
        
        bne :+
          lda YPos
          clc
          adc YVel
        :

        sta ActorsArray+Actor::YPos,x
        sta YPos
        lda #0
        adc YPos+1
        sta YPos+1
        sta ActorsArray+Actor::YPos+1,x



        jmp NextActor
      FinishActor:

      cmp #ActorType::MISSILE
      bne :+
          lda ActorsArray+Actor::YPos,x
          sec
          sbc #1                          ; Decrement Y position of missiles by 1
          sta ActorsArray+Actor::YPos,x
          bcs SkipMissile
            lda #ActorType::NULL
            sta ActorsArray+Actor::Type,x ; Remove the missile from the array
          SkipMissile:

        CheckCollision:
          lda ActorsArray+Actor::XPos,x
          clc
          adc #3
          sta ParamXPos                   ; Missile position to be checked X += 3 (plus 3 pixels from the left)

          lda ActorsArray+Actor::YPos,x
          clc
          adc #1
          sta ParamYPos                   ; Missile position to be checked Y += 1 (plus 1 pixels from the top)

          jsr CheckEnemyCollision         ; Perform collision check between the missile and other enemy actor

          lda Collision                   ; If collision happened, delete the missile
          beq NoCollisionFound
            lda #ActorType::NULL          ; Delete missile actor
            sta ActorsArray+Actor::Type,x

            jsr IncrementScore            ; Increment the Score variable using BCD-ish mode
            jsr DrawScore                 ; Send the score bytes to the background buffer at $7000

          NoCollisionFound:

          jmp NextActor
        :
      cmp #ActorType::SUBMARINE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sec
        sbc #1                          ; Decrement Y position of submarine by 1
        sta ActorsArray+Actor::XPos,x
        bcs SkipSubmarine
          lda #ActorType::NULL
          sta ActorsArray+Actor::Type,x ; Remove the submarine from the array
        SkipSubmarine:
        jmp NextActor
      :
      cmp #ActorType::AIRPLANE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sec
        sbc #1                          ; Decrement Y position of airplane by 1
        sta ActorsArray+Actor::XPos,x
        bcs SkipAirplane
          lda #ActorType::NULL
          sta ActorsArray+Actor::Type,x ; Remove the airplane from the array
        SkipAirplane:
        jmp NextActor
      :
      NextActor:
        txa
        clc
        adc #.sizeof(Actor)
        tax
        cmp #MAX_ACTORS * .sizeof(Actor)
        beq :+ 
          jmp ActorsLoop
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
      bne :+
        ; Each piece of mario
        ; Top left small head
        TopLeftSmallHead:
          lda ActorsArray+Actor::XPos,x
          sta ParamXPos
          lda ActorsArray+Actor::YPos,x
          sta ParamYPos

          lda #$32
          sta ParamTileNum
          lda #%00000000
          sta ParamAttribs
          lda #1
          sta ParamNumTiles
          jsr DrawSprite                 ; Call routine to draw 4 PLAYER tiles to the OAM will need to call each one individually
        EndTopLeftSmallHead:

        TopRightSmallHead:
          lda ActorsArray+Actor::XPos,x
          clc 
          adc #8
          sta ParamXPos
          lda ActorsArray+Actor::YPos,x
          sta ParamYPos

          lda #$33
          sta ParamTileNum
          lda #%00000000
          sta ParamAttribs
          lda #1
          sta ParamNumTiles
          jsr DrawSprite                 ; Call routine to draw 4 PLAYER tiles to the OAM will need to call each one individually
        EndTopRightSmallHead:

        
        BottomLeftSmallFoot:
          lda ActorsArray+Actor::XPos,x
          sta ParamXPos
          lda ActorsArray+Actor::YPos,x
          clc 
          adc #8
          sta ParamYPos

          lda #$4F
          sta ParamTileNum
          lda #%00000000
          sta ParamAttribs
          lda #1
          sta ParamNumTiles
          jsr DrawSprite                 ; Call routine to draw 4 PLAYER tiles to the OAM will need to call each one individually
        EndBottomLeftSmallFoot:

        BottomRightSmallFoot:

          lda ActorsArray+Actor::XPos,x
          clc
          adc #8
          sta ParamXPos

          lda ActorsArray+Actor::YPos,x
          clc 
          adc #8
          sta ParamYPos

          lda #$4F
          sta ParamTileNum
          lda #%01000000
          sta ParamAttribs
          lda #1
          sta ParamNumTiles
          jsr DrawSprite                 ; Call routine to draw 4 PLAYER tiles to the OAM will need to call each one individually
        EndBottomRightSmallFoot:



        jmp NextActor
      :
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to loop "NumTiles" times, sending bytes to the OAM-RAM.
;; Params = ParamXPos, ParamYPos, ParamTileNum, ParamAttribs, ParamNumTiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DrawSprite
    txa
    pha                                ; Save the value of the X register before anything
    
    ldx #0
    TileLoop:
       lda ParamYPos                   ; Send Y position to the OAM
       sta (SprPtr),y
       iny

       lda ParamTileNum                ; Send the Tile # to the OAM
       sta (SprPtr),y
       inc ParamTileNum                ; ParamTileNum++
       iny

       lda ParamAttribs                ; Send the attributes to the OAM
       sta (SprPtr),y
       iny

       lda ParamXPos                   ; Send X position to the OAM
       sta (SprPtr),y
       clc
       adc #8
       sta ParamXPos                   ; ParamXPos += 8

       iny

       inx                             ; X++
       cpx ParamNumTiles               ; Loop until X == NumTiles
       bne TileLoop

    pla
    tax                                ; Restore the previous value of X register

    rts
.endproc

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
.proc DrawScore
    lda #$70
    sta BufPtr+1
    lda #$00
    sta BufPtr+0

    ldy #0
    
    lda #3                   ; Length = 3 (how many bytes we will send)
    sta (BufPtr),y
    iny

    lda #$20
    sta (BufPtr),y           ; Hi-Byte of the PPU address to be updated
    iny
    lda #$52
    sta (BufPtr),y           ; Lo-Byte of the PPU address to be updated
    iny

    ;; Send the 3 digits of the score (from MSB to LSB) 100s, 10s, 1s

    lda Score+2              ; 100s digit of the Score
    clc
    adc #$60                 ; Offset by $60 to point to the correct tile
    sta (BufPtr),y
    iny

    lda Score+1              ; 10s digit of the Score
    clc
    adc #$60                 ; Offset by $60 to point to the correct tile
    sta (BufPtr),y
    iny

    lda Score+0              ; 1s digit of the Score
    clc
    adc #$60                 ; Offset by $60 to point to the correct tile
    sta (BufPtr),y
    iny

    lda #0
    sta (BufPtr),y           ; Length=0 to signal the end of the buffer
    iny

    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Returns a random 8-bit number inside A (0-255), clobbers Y (0).
;; Requires a 2-byte value on the zero page called "Seed".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is a 16-bit Galois linear feedback shift register with polynomial $0039.
; The sequence of numbers it generates will repeat after 65535 calls.
; Execution time is an average of 125 cycles (excluding jsr and rts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc GetRandomNumber
    ldy #8                   ; Loop counter (generates 8 bits)
    lda Seed+0
:   asl                      ; Shift the register
    rol Seed+1
    bcc :+
      eor #$39               ; Apply XOR feedback when a 1 bit is shifted out
    :
    dey
    bne :--
    sta Seed+0               ; Saves the value in A into the Seed
    cmp #0                   ; Set flags
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Increment the Score value simulating BCD mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc IncrementScore
  Increment1sDigit:
    lda Score+0              ; Load the lowest digit of the number
    clc 
    adc #1                   ; Add 1
    sta Score+0 
    cmp #$A                  ; Check for overflow (equals 10)
    bne DoneIncrementing     ; If no overflow happened, we're done!
  Increment10sDigit:
    lda #0
    sta Score+0              ; Reset one's digit from 9 to 0
    lda Score+1              ; Load second digit
    clc 
    adc #1                   ; Add 1 (the carry from the previous digit)
    sta Score+1
    cmp #$A                  ; Check for overflow
    bne DoneIncrementing     ; If no overflow happened, we're done!
  Increment100sDigit:
    lda #0
    sta Score+1              ; Reset ten's digit from 9 to 0
    lda Score+2              ; Load the third digit
    clc 
    adc #1                   ; Add 1 (the carry from the previous digit)
    sta Score+2
    cmp #$A                  ; Check for overflow
    bne DoneIncrementing     ; If no overflow happened, we're done!
  Increment1000sDigit:
    lda #0
    sta Score+2              ; Reset hundred's digit from 9 to 0
    lda Score+3              ; Load the last digit
    clc 
    adc #1                   ; Add 1 (the carry from the previous digit)
    sta Score+3              ; Store the final addition in the last digit
DoneIncrementing:
    rts
.endproc


