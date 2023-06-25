;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to Implement Gravity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ImplementGravity ;; ParamY = Actor's Y Position
    ldx #0
    ActorsLoop:
    lda ActorsArray+Actor::Type,x
    cmp #ActorType::PLAYER
    bne NextActor
      ; jmp Reset
      lda YVel
      bpl :+
        dec YPos+1            ; sign extension
      :
      clc
      adc #GRAVITY_CONSTANT                  ; this is gravity for a second
      clc
      adc YPos   

      ; TODO : Is there a collision, loop through the collidables array 
      jsr CheckCollidableCollision

      pha                      ; Push accumulator which is the YVel onto the + Grav constant +  ypost onto stack
      lda Collision
      ; If no collision skip and store accumulator value, 
      bne :+
        ; If collision, set position to Collision because collision now has the y value
        pla       ; Pull old accumulator value off 
      : 
      sta ActorsArray+Actor::YPos,x
      sta YPos
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
    pha                                ; Push the accumulator onto the stack
    txa
    pha                                ; Push and save X register in the stack

    ldx #0
    stx Collision                      ; Collision = 0

  CollisionLoop:
    cpx #MAX_COLLIDABLES * .sizeof(Collidable)   ; We loop all entities, looking for enemies (airplanes)
    beq FinishCollisionCheck
      lda CollidablesArray+Collidable::Type,x    ; Load the type of the actor we are looping
      cmp #CollidableType::COLLIDABLE            
      bne NextCollidable                    ; If it's NOT collidable, bypass this and move check the next one

      ;; LOAD BOUNDING BOX X1, Y1, X2, and Y2
      lda CollidablesArray+Collidable::XPos,x    ; Bounding Box X1
      sta ParamRectX1
      lda CollidablesArray+Collidable::YPos,x    ; Bouding Box Y1
      sta ParamRectY1

      lda CollidablesArray+Collidable::XPos,x
      clc
      adc #8                         ; Get right value of the collidable bounding box by adding 8 pixels to the right
      sta ParamRectX2                  ; Bounding Box X2

      lda CollidablesArray+Collidable::YPos,x
      clc
      adc #8                           ; Get the bottom of the collidable bounding box by adding 8
      sta ParamRectY2                  ; Bouding Box Y2

      jsr DidUnderCollisionOccur     ; Proceed to test if point is inside bounding box

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
    pla
    tax                                ; Pull and restore the old value of X
    pla

    rts
.endproc




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to loop all enemy actors checking for collision with missile
;; Params = ParamXPos, ParamYPos (are the X and Y position of the missile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc CheckEnemyCollision
    txa
    pha                                ; Push and save X register in the stack

    ldx #0
    stx Collision                      ; Collision = 0

  EnemiesCollisionLoop:
    cpx #MAX_ACTORS * .sizeof(Actor)   ; We loop all entities, looking for enemies (airplanes)
    beq FinishCollisionCheck
      lda ActorsArray+Actor::Type,x    ; Load the type of the actor we are looping
      cmp #ActorType::AIRPLANE
      bne NextEnemy                    ; If it's NOT Airplane, bypass this enemy and move check the next one

      ;; LOAD BOUNDING BOX X1, Y1, X2, and Y2
      lda ActorsArray+Actor::XPos,x    ; Bounding Box X1
      sta ParamRectX1
      lda ActorsArray+Actor::YPos,x    ; Bouding Box Y1
      sta ParamRectY1

      lda ActorsArray+Actor::XPos,x
      clc
      adc #22                          ; Get right value of the airplane bounding box by adding 22 pixels to the right
      sta ParamRectX2                  ; Bounding Box X2

      lda ActorsArray+Actor::YPos,x
      clc
      adc #8                           ; Get the bottom of the airplane bounding box by adding 8
      sta ParamRectY2                  ; Bouding Box Y2

      jsr IsPointInsideBoundingBox     ; Proceed to test if point is inside bounding box

      lda Collision
      beq NextEnemy                    ; If no collision, don't do anything
        lda #ActorType::NULL           ; Else, destroy airplane
        sta ActorsArray+Actor::Type,x  ; If collision happened, destroy airplane entity
        jmp FinishCollisionCheck       ; Also, if collision happened we stop looping other enemies and leave the subroutine

  NextEnemy:
      txa
      clc
      adc #.sizeof(Actor)              ; X += sizeof(Actor)
      tax
      jmp EnemiesCollisionLoop         ; Loop to check the next actor to see if it's an enemy (airplane)

FinishCollisionCheck:
    pla
    tax                                ; Pull and restore the old value of X

    rts
.endproc


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
    jmp EndCollisionCheck

  PointIsOutside:
    lda #0                             ; If we branch here, the point was outside the bounding box
    sta Collision                      ; No collision detected!

  EndCollisionCheck:
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
.proc IsPointInsideBoundingBox
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
    jmp EndCollisionCheck

  PointIsOutside:
    lda #0                             ; If we branch here, the point was outside the bounding box
    sta Collision                      ; No collision detected!

  EndCollisionCheck:
    rts
.endproc
