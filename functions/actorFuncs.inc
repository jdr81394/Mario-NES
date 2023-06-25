;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to add new collidable to the array in the first empty slot found
;; ParamXPos, ParamYPos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddNewCollidable
    ldx #0                            ; X = 0
  ArrayLoop:
    cpx #MAX_COLLIDABLES * .sizeof(Collidable)  ; Reached maximum number of actors allowed in the array?
    beq EndRoutine                    ; Then we skip and don't add a new actor
    lda CollidablesArray+Collidable::Type,x
    cmp #CollidableType::NULL              ; If the actor type of this array position is NULL
    beq AddNewActorToArray            ;   Then: we found an empty slot, proceed to add actor to position [x]
   NextActor:
    txa
    clc
    adc #.sizeof(Collidable)               ; Otherwise, we offset to the check the next actor in the array
    tax                               ; X += sizeof(Actor)
    jmp ArrayLoop

  AddNewActorToArray:                 ; Here we add a new actor at index [x] of the array
    lda #CollidableType::COLLIDABLE         ; They should all be of type collidable
    sta CollidablesArray+Collidable::Type,x
    lda ParamXPos                     ; Fetch parameter "actor position X" from RAM
    sta CollidablesArray+Collidable::XPos,x
    lda ParamYPos                     ; Fetch parameter "actor position Y" from RAM
    sta CollidablesArray+Collidable::YPos,x
EndRoutine:
    rts
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to add new actor to the array in the first empty slot found
;; Params = ParamType, ParamXPos, ParamYPos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddNewActor
    ldx #0                            ; X = 0
  ArrayLoop:
    cpx #MAX_ACTORS * .sizeof(Actor)  ; Reached maximum number of actors allowed in the array?
    beq EndRoutine                    ; Then we skip and don't add a new actor
    lda ActorsArray+Actor::Type,x
    cmp #ActorType::NULL              ; If the actor type of this array position is NULL
    beq AddNewActorToArray            ;   Then: we found an empty slot, proceed to add actor to position [x]
   NextActor:
    txa
    clc
    adc #.sizeof(Actor)               ; Otherwise, we offset to the check the next actor in the array
    tax                               ; X += sizeof(Actor)
    jmp ArrayLoop

  AddNewActorToArray:                 ; Here we add a new actor at index [x] of the array
    lda ParamType                     ; Fetch parameter "actor type" from RAM
    sta ActorsArray+Actor::Type,x
    lda ParamXPos                     ; Fetch parameter "actor position X" from RAM
    sta ActorsArray+Actor::XPos,x
    lda ParamYPos                     ; Fetch parameter "actor position Y" from RAM
    sta ActorsArray+Actor::YPos,x
EndRoutine:
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to spawn actors when certain conditions are met
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc SpawnActors
  ; SpawnSubmarine:
  ;   lda Clock60                        ; Submarines are added in intervals of 3 seconds
  ;   sec
  ;   sbc PrevSubmarine
  ;   cmp #3                             ; Only add a new submarine if the difference in time from the previous one is equal to 3
  ;   bne :+
  ;     lda #ActorType::SUBMARINE
  ;     sta ParamType                    ; Load parameter for the actor type
  ;     lda #223
  ;     sta ParamXPos                    ; Load parameter for actor position X
      
  ;     jsr GetRandomNumber              ; Fetch a new random number for the Y position of the submarine
  ;     lsr
  ;     lsr
  ;     lsr                              ; Divide the random number by 8 to get a healthy Y position range
  ;     clc
  ;     adc #180                         ; And then add 180 to to it to move the submarine to the bottom part of the screen
  ;     sta ParamYPos

  ;     sta ParamYPos                    ; Load parameter for actor position Y

  ;     jsr AddNewActor                  ; Call the subroutine to add the new submarine actor
    
  ;     lda Clock60
  ;     sta PrevSubmarine                ; Save the current Clock60 time as the submarine last spawn time
  ;   :
  ; SpawnAirplane:
  ;   lda Clock60                        ; Submarines are added in intervals of 2 seconds
  ;   sec
  ;   sbc PrevAirplane
  ;   cmp #2                             ; Only add a new airplane if the difference in time from the previous one is equal to 2
  ;   bne :+
  ;     lda #ActorType::AIRPLANE
  ;     sta ParamType                    ; Load parameter for the actor type
  ;     lda #235
  ;     sta ParamXPos                    ; Load parameter for actor position X
      
  ;     jsr GetRandomNumber
  ;     lsr
  ;     lsr                              ; Divide the random number (0-255) by 4 to adjust for the correct Y position range
  ;     clc
  ;     adc #35                          ; And then add 35 to to it to place the airplane in the correct place in the sky

  ;     sta ParamYPos                    ; Load parameter for actor position Y

  ;     jsr AddNewActor                  ; Call the subroutine to add the new airplane actor
    
  ;     lda Clock60
  ;     sta PrevAirplane                 ; Save the current Clock60 time as the airplane last spawn time
  ;   :
    rts
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to clear out ALL the actors when certain conditions are met
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ClearActors
  ldx #0        ; X will be the counter
  Loop:
    cpx #MAX_ACTORS     ; Compare it to the max number of actors
    beq EndLoop         ; If x - max_actors = 0, that means it hit the max and the zero flag is set. Lets jump to the end of the loop
      ; Else, clear out the current position
      lda #ActorType::NULL             ; Load the null value
      sta ActorsArray+Actor::Type,x   ; Put it into the type 
      inx                             ; Increment the counter
      jmp Loop                        ; Jump back to the top of the loop
  EndLoop:
  rts
.endproc