;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to Implement Gravity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ImplementGravity ;; ParamY = Actor's Y Position
    ldx #0
    ActorsLoop:
    lda ActorsArray+Actor::Type,x
    cmp #ActorType::PLAYER
    bne NextActor
      ; If vertical flag is equal to 0, then just jump to next actor
      lda VerticalFlag
      beq NextActor
      ; If it is positive, meaning going down, skip
      bpl IfPositive
        ; if it is negative, meaning mario has upwards velocity, handle it here
        ; Then, lets see if marioanimationframecountdown is equal to 0, if not, then we just skip everything

          ; If Mario Animation Frame Countdown, then let's do the things we were going to do
            dec YPos+1            ; sign extension
      
            ; Gravity when going up will be a bit different versus going down
            lda YVel
            sec
            sbc #INITIAL_TAKEOFF_FRICTION          ; using break for this should work for now
            
            bcs :+              ; if carry cleared means an overflow happened from 0 -> 255          
              lda #1
              sta VerticalFlag  ; Make player go in opposite direction
              lda #0
            :
            sta YVel
            lda #0
            sbc YVel+1
            jmp NextActor
      IfPositive:
      bmi IfMinus
        ; First, let see if mario is jumping or just falling, if falling we handle it different
        lda MarioAnimationStates
        cmp MarioAnimations::FALLING_LEFT
        bne :+     ; If not equal, then not falling left
            
            ; Do stuff here for falling left
        :
        cmp MarioAnimations::FALLING_RIGHT    
        bne :+          ; If not equal, then not falling right
                ; Do stuff here for falling left
        :

          ; If vertical flag is positive: 
          lda YVel
          clc
          adc #GRAVITY_CONSTANT                  ; this is gravity for a second
          
          ; If YVel + Gravity Constant overflowed from 255 -> 0, then a carry would be present and we can just set to max velocity
          bcc StoreYVel
            ; Overflow occured so:
            lda #MAX_VEL
          
          ; If YVel is greater than the max velocity, then load the max velocity instead so we can clamp it
          cmp #MAX_VEL
          bcc StoreYVel
            ; YVel was greater so were in here now
            lda #MAX_VEL
          StoreYVel:
          sta YVel
      IfMinus:


      
      NextActor:
        txa
        clc
        adc #.sizeof(Actor)
        tax
        cmp #MAX_ACTORS * .sizeof(Actor)
        beq :+ 
          jmp ActorsLoop
        :
    Finish:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all enemy actors checking for collision with missile
;; Params = ParamXPos, ParamYPos (are the X and Y position of the missile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckCollidableCollision

  CollisionLoop:
    lda Collision
    bne FinishCollisionCheck ; If it is not equal to 0 jump to finish collision check, means there was already a collision on this loop
      cpx #MAX_COLLIDABLES * .sizeof(Collidable)   ; We loop all entities, looking for enemies (airplanes)
      beq FinishCollisionCheck
        lda CollidablesArray+Collidable::Type,x    ; Load the type of the actor we are looping
        cmp #CollidableType::COLLIDABLE            
        bne NextCollidable                    ; If it's NOT collidable, bypass this and move check the next one

        ;; LOAD BOUNDING BOX X1, Y1, X2, and Y2
        lda CollidablesArray+Collidable::XPos,x    ; Bounding Box X1
        sta ParamRectX1
        lda CollidablesArray+Collidable::YPos,x    ; Bouding Box Y1
        ; TODO FIX: THIS IS HACKY JAKE, THIS IS JUST USED TO GET THE COLLISION JUST RIGHT
        sec
        sbc #3
        ;
        sta ParamRectY1

        lda CollidablesArray+Collidable::XPos,x
        clc
        adc #24                        ; Get right value of the collidable bounding box by adding 8 pixels to the right
        sta ParamRectX2                  ; Bounding Box X2

        lda CollidablesArray+Collidable::YPos,x
        clc
        adc #8                           ; Get the bottom of the collidable bounding box by adding 8
        sta ParamRectY2                  ; Bouding Box Y2

        jsr DidUnderCollisionOccur     ; Proceed to test if point is inside bounding box

        ; Collision is really under collision
        lda Collision
        beq NextCollidable                    ; If no collision, don't do anything


          jmp FinishCollisionCheck       ; Also, if collision happened we stop looping other enemies and leave the subroutine

    NextCollidable:
        txa
        clc
        adc #.sizeof(Collidable)              ; X += sizeof(Actor)
        tax
        jmp CollisionLoop         ; Loop to check the next actor to see if it's an enemy (collidable)

  FinishCollisionCheck:

    rts
.endproc




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all enemy actors checking for collision with missile
;; Params = ParamXPos, ParamYPos (are the X and Y position of the missile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .proc CheckEnemyCollision
;     txa
;     pha                                ; Push and save X register in the stack

;     ldx #0
;     stx Collision                      ; Collision = 0

;   EnemiesCollisionLoop:
;     cpx #MAX_ACTORS * .sizeof(Actor)   ; We loop all entities, looking for enemies (airplanes)
;     beq FinishCollisionCheck
;       lda ActorsArray+Actor::Type,x    ; Load the type of the actor we are looping
;       cmp #ActorType::AIRPLANE
;       bne NextEnemy                    ; If it's NOT Airplane, bypass this enemy and move check the next one

;       ;; LOAD BOUNDING BOX X1, Y1, X2, and Y2
;       lda ActorsArray+Actor::XPos,x    ; Bounding Box X1
;       sta ParamRectX1
;       lda ActorsArray+Actor::YPos,x    ; Bouding Box Y1
;       sta ParamRectY1

;       lda ActorsArray+Actor::XPos,x
;       clc
;       adc #22                          ; Get right value of the airplane bounding box by adding 22 pixels to the right
;       sta ParamRectX2                  ; Bounding Box X2

;       lda ActorsArray+Actor::YPos,x
;       clc
;       adc #8                           ; Get the bottom of the airplane bounding box by adding 8
;       sta ParamRectY2                  ; Bouding Box Y2

;       jsr IsPointInsideBoundingBox     ; Proceed to test if point is inside bounding box

;       lda Collision
;       beq NextEnemy                    ; If no collision, don't do anything
;         lda #ActorType::NULL           ; Else, destroy airplane
;         sta ActorsArray+Actor::Type,x  ; If collision happened, destroy airplane entity
;         jmp FinishCollisionCheck       ; Also, if collision happened we stop looping other enemies and leave the subroutine

;   NextEnemy:
;       txa
;       clc
;       adc #.sizeof(Actor)              ; X += sizeof(Actor)
;       tax
;       jmp EnemiesCollisionLoop         ; Loop to check the next actor to see if it's an enemy (airplane)

; FinishCollisionCheck:

;     rts
; .endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check if a collision below occurred
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Params:
;;   (ParamXPos,ParamYPos) are the coords of the point to be tested
;;   (ParamRectX1,ParamRectY1,ParamRectX2,ParamRectY2) are rectangle coords
;; Output:
;;   Collision flag is either 1 or 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DidUnderCollisionOccur
    lda ParamXPos                      ; Fetch the point X coordinate
    cmp ParamRectX1                    ; Compare it with the enemy rectangle X
    bcc PointIsOutside                 ; If it's less, we stop checking because there is no collision

    lda ParamYPos                      ; Fetch the point Y coordinate
    cmp ParamRectY1                    ; Compare it with the enemy bounding box Y value
    bcc PointIsOutside                 ; If it's less, we stop checking because there is no collision

    lda ParamXPos                      ; Fetch the point X coorrinate
    cmp ParamRectX2                    ; Compare it with the enemy bounding box right
    bcs PointIsOutside                 ; If it's greater than, we stop checking because there is no collision

    lda ParamYPos                      ; Fetch the point X coorrinate
    cmp ParamRectY2                    ; Compare it with the enemy bounding box right
    bcs PointIsOutside                 ; If it's greater than, we stop checking because there is no collision

    PointIsInside:
      lda ParamYPos                             ; If we reach here, the point is inside the bounding box!
      sta Collision                      ; Collision detected!

      ; Let's have player running left or right if under collision is detected and they have velocity
      lda XVel
      bmi :+
        ; if positive set to right
        lda #MarioAnimations::RUNNING_RIGHT
        bpl :++
      : 
        ; if negative set to left
        lda #MarioAnimations::RUNNING_LEFT
      :
      sta MarioAnimationStates

      jmp EndCollisionCheck

    PointIsOutside:
      lda #0                             ; If we branch here, the point was outside the bounding box
      sta Collision                      ; No collision detected!

  EndCollisionCheck:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to add new collidable to the array in the first empty slot found
;; ParamXPos, ParamYPos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddNewCollidable
    ldx #0                            ; X = 0
  ArrayLoop:
    cpx #MAX_COLLIDABLES * .sizeof(Collidable)  ; Reached maximum number of collidables allowed in the array?
    beq EndRoutine                    ; Then we skip and don't add a new actor
    lda CollidablesArray+Collidable::Type,x
    cmp #CollidableType::NULL              ; If the collidable type of this array position is NULL
    beq AddNewCollidableToArray            ;   Then: we found an empty slot, proceed to add collidable to position [x]
   NextCollidable:
    txa
    clc
    adc #.sizeof(Collidable)               ; Otherwise, we offset to the check the next collidable in the array
    tax                               ; X += sizeof(collidable)
    jmp ArrayLoop

  AddNewCollidableToArray:                 ; Here we add a new collidable at index [x] of the array
    lda #CollidableType::COLLIDABLE         ; They should all be of type collidable
    sta CollidablesArray+Collidable::Type,x
    lda ParamXPos                     ; Fetch parameter "collidable position X" from RAM
    sta CollidablesArray+Collidable::XPos,x
    lda ParamYPos                     ; Fetch parameter "collidable position Y" from RAM
    sta CollidablesArray+Collidable::YPos,x
EndRoutine:
    rts
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check tile and add it to the collidables array - TODO: Clean up!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddToCollidablesArray

    ; If there is a brick, that is 30, above it that means it is directly above and we do not need to add it
    ; TODO Also we will need to exclude if it is in the attribute table.. we'll handle that in a bit
    
    ; Possible collidables: $45, $47
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
            sta VarX

            ; Since each screen is 32 columns long ( its also 8 pixels wide per column )
            ; In order to get the x placement we need to load up the 255 bytes are find the remainder value
            ; Also, for every power of 8 it goes over it will also increase the Y position by 8
            
            ; Divide by 32 until it is less than 32 to find the x position
            tya                  ; Transfer y ( little loop ) to accumulator
            sta VarY             ; hold the little loop

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
            ldx VarX
            ldy VarY

        CheckNext:

    rts
.endproc





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to check if a point is inside a bounding box.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Params:
;;   (ParamXPos,ParamYPos) are the coords of the point to be tested
;;   (ParamRectX1,ParamRectY1,ParamRectX2,ParamRectY2) are rectangle coords
;; Output:
;;   Collision flag is either 1 or 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .proc IsPointInsideBoundingBox
;     lda ParamXPos                      ; Fetch the point X coordinate
;     cmp ParamRectX1                    ; Compare it with the enemy rectangle X
;     bcc PointIsOutside                 ; If it's less, we stop checking because there is no collision

;     lda ParamYPos                      ; Fetch the point Y coordinate
;     cmp ParamRectY1                    ; Compare it with the enemy bounding box Y value
;     bcc PointIsOutside                 ; If it's less, we stop checking because there is no collision

;     lda ParamXPos                      ; Fetch the point X coorrinate
;     cmp ParamRectX2                    ; Compare it with the enemy bounding box right
;     bcs PointIsOutside                 ; If it's greater than, we stop checking because there is no collision

;     lda ParamYPos                      ; Fetch the point X coorrinate
;     cmp ParamRectY2                    ; Compare it with the enemy bounding box right
;     bcs PointIsOutside                 ; If it's greater than, we stop checking because there is no collision

;   PointIsInside:
;     lda ParamYPos                             ; If we reach here, the point is inside the bounding box!
;     sta Collision                      ; Collision detected!
;     jmp EndCollisionCheck

;   PointIsOutside:
;     lda #0                             ; If we branch here, the point was outside the bounding box
;     sta Collision                      ; No collision detected!

;   EndCollisionCheck:
;     rts
; .endproc
