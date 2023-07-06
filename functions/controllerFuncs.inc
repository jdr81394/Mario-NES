;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to read controller state and store it inside "Buttons" in RAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc ReadControllers
    lda Buttons
    sta PrevButtons
    
    lda #1                   ; A = 1
    sta Buttons              ; Buttons = 1
    sta JOYPAD1              ; Set Latch=1 to begin 'Input'/collection mode
    lsr                      ; A = 0
    sta JOYPAD1              ; Set Latch=0 to begin 'Output' mode
LoopButtons:
    lda JOYPAD1              ; This reads a bit from the controller data line and inverts its value,
                             ; And also sends a signal to the Clock line to shift the bits
    lsr                      ; We shift-right to place that 1-bit we just read into the Carry flag
    rol Buttons              ; Rotate bits left, placing the Carry value into the 1st bit of 'Buttons' in RAM
    bcc LoopButtons          ; Loop until Carry is set (from that initial 1 we loaded inside Buttons)
    rts
.endproc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to read controller state and store it inside "Buttons" in RAM
;; THIS IS NOT COMPLETE YET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc HandleControllerInput
    CheckNoButtons:
      lda Buttons
      bne EndNoButtons
        ; Let's make sure mario is not in the air
        lda MarioAnimationStates
        cmp MarioAnimations::JUMPING_RIGHT
        bcs CheckCollision

        ; The above and below ensure that its between 4 and 7

        lda MarioAnimationStates
        cmp MarioAnimations::DEATH
        bcc CheckCollision      

        SetToStanding:
        ; Let's set the animation time to zero
        lda #0
        sta MarioAnimationFrameCountdown
        sta MarioAnimationFrame
        ; If it is not equal to 0, that means something is being pressed
        ; If it is equal to 0, nothing is being pressed and we need to put mario to standing
        ; Let's get the direction
        lda MarioAnimationStates
        and #%00000001              ; Let's see if the 1st bit is a 1 or 0, this will tell us what direction
        bne :+ ; if it is equal to 0, this means that the and operation did NOT work, meaning the right most bit is 0, and the direction is to the right
          lda #MarioAnimations::STANDING_RIGHT
          jmp StoreMarioAnimation
        :
          lda #MarioAnimations::STANDING_LEFT
        StoreMarioAnimation:
        sta MarioAnimationStates
        jmp EndNoButtons

        CheckCollision:
          lda Collision
          bne SetToStanding     ; If its not 0, means theres a collision then jump back up and set animation

    EndNoButtons:
    CheckAButton:
        lda Buttons
        and #BUTTON_A
        beq EndAButton
          ; Let's check collision, can only jump if there is currently a collision
          ; If the value is equal to 0, then there is no collision, so we should NOT be able to jump 
          lda Collision
          beq EndOfAButtonCollisionCheck
            ; If the value is NOT zero, meaning there is an under collision, then lets jump


            ; Let's set state of Mario jumping, first lets get his direction
            lda MarioAnimationStates
            and #%00000001                  ; if accumulator is equal to 1, then he was facing left, else he was facing right
            bne :+
                ; so inside that means it was equal to 0, therefore he was facing right
              lda #MarioAnimations::JUMPING_RIGHT
              bpl StoreJumpAnimation                             ; Just use branch if positive (since it will always be positive) to go to store
            :
            lda #MarioAnimations::JUMPING_LEFT

            StoreJumpAnimation:
            sta MarioAnimationStates          ; Set jumping state

            
            ; Lets set the vertical flag to a negative value to show the player is going up
            lda #%10000000
            sta VerticalFlag

            ; HACKY - TODO - Jake collision detection should not need to be reset here. Collision function should take care of this
            lda #0
            sta Collision

            lda #FRAMES_UNTIL_PEAK_JUMP
            sta MarioPeakJumpFrameCountdown

            ; Lets set initial takeoff velocity
            lda #INITIAL_TAKEOFF
            sta YVel

          
          
          EndOfAButtonCollisionCheck:

          ; PrevButton Must have been A as well
          lda PrevButtons
          and #BUTTON_A
          beq EndAButton
          ; Let check if there was no collision, but the player is in the air and can add speed to the jump
          lda MarioPeakJumpFrameCountdown
          beq PeakJumpAlreadyReached
            ; In here if it is not 0 meaning the player can add some velocity on
            ; Lets just make sure that they jumped and are not falling
            lda MarioAnimationStates
            cmp #MarioAnimations::JUMPING_RIGHT
            bcc PeakJumpAlreadyReached      ; If carry is set then it is 4, or the value above it 5
          
            ; Must use falling right because if it is equal to jumping left then the carry would be set
            cmp #MarioAnimations::FALLING_RIGHT ; If carry is clear then it is 5, or the value below it 4
            bcs PeakJumpAlreadyReached      ; If it is not equal, go to end

            ; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            ; The above comparisons ensure it is either 4 or 5
            lda #INITIAL_TAKEOFF
            sta YVel
            sta YVel+1
          PeakJumpAlreadyReached:

    EndAButton:
    CheckLeftButton:
        lda Buttons
        and #BUTTON_LEFT
        beq NotLeft

          ; However if mario is in air we do not need to set this up so lets check
          ; Let's make sure mario is not in the air
          lda MarioAnimationStates
          cmp #MarioAnimations::JUMPING_RIGHT
          bcc :+

          ; The above and below ensure that its between 4 and 7
          cmp #MarioAnimations::DEATH
          bcs :+
            lda #MarioAnimations::JUMPING_LEFT
            sta MarioAnimationStates
            jmp EndOfSetUpAnimation      
          :

          ; Check to see if mario was already running left first to restart animation
          cmp #MarioAnimations::RUNNING_LEFT
          ; If equals, then it already was running left and we don't need to start the animation timer
          beq EndOfSetUpAnimation

            ; Set animation start time
            lda #FRAMES_PER_ANIMATION                 ; Each movement can be 33 frames
            sta MarioAnimationFrameCountdown

            ; Set mario to running left
            lda #MarioAnimations::RUNNING_LEFT
            sta MarioAnimationStates
          EndOfSetUpAnimation:

          ; We load th velocity to check if its 0. If it is 0, then that mean mario's break has brough any previous acceleration down to 0, and we can therefore have mario 
          ; begin to accelerate in the other direction
          lda XVel
          bne :+
          ; Lets set a flag equal to negative so that way we can determine that he is going to the left
          ; This works because mario never gets knocked back by anything horizontally. So his velocity horizontally should always be from user directed inputs
            lda #%10000000              ; this is negative - this technically represents -128
            sta HorizontalFlag
          :

          lda XPos+1
          cmp #10
          bcc PreventScrollToLeft             ; if XPos is equal to 10, we want to branch it because it shouldn't be less than 0! 
              
              ; If the horizontal flag is set to minus, then mario has finally stopped sliding from any previous momentum, and we can actually make him go to the left
              lda HorizontalFlag
              bmi :+
                  bpl NotLeft                 ; Skip if horizontal flag is equal to negative
              :
              lda XVel
              clc 
              adc #ACCEL                    ; Subtract the acceleration from the velocity
              cmp #MAX_VEL              ; If velocity - acceleration is greater than 256 - max speed, carry will be set , else cleared But prevent it from going over the max. speed
              bcs :+
                  lda #MAX_VEL
              :
              sta XVel
              jmp FinishCheckLeft

          NotLeft:
              ; Check if its negative, this means mario still has negative momentum, but is not pressing left
              ; if so, then load x velocity
              ; to apply the break
              lda HorizontalFlag
              bpl CheckHorizontalFlagLeft

              ; Load XVel,
              lda XVel
              cmp #BRAKE
              bcc :+
                ; if it is greater than the break, subtract break from it
                sec
                sbc #BRAKE
                jmp StoreXVel
              :

              ; If it is not greater than the break, perhaps it is 2 and break is 5,
              ; just set XVel to 0
              lda #0

              StoreXVel:
              sta XVel
        
          CheckHorizontalFlagLeft:
            ; If XVel is equal to 0, then we should set the Horizontal Flag to 0
            ; If it is not equal, then let's just jump ahead
            lda XVel
            bne :+
              lda #0
              sta HorizontalFlag
            :
            jmp FinishCheckLeft

          PreventScrollToLeft:
            lda #0
            sta XVel

          
    FinishCheckLeft:
    CheckRightButton:
      lda Buttons
      and #BUTTON_RIGHT
      beq NotRight

        ;   However if mario is in air we do not need to set this up so lets check
        ; Let's make sure mario is not in the air
        lda MarioAnimationStates
        cmp #MarioAnimations::JUMPING_RIGHT
        bcc :+

        ; The above and below ensure that its between 4 and 7
        cmp #MarioAnimations::DEATH
        bcs :+
            ; The above 2 conditions passed meaning mario is in the air
            lda #MarioAnimations::JUMPING_RIGHT
            sta MarioAnimationStates
            jmp EndOfSetUpAnimationRight   
        :

        ; Check to see if mario was already running right first to restart animation
        cmp #MarioAnimations::RUNNING_RIGHT
        ; If equals, then it already was running right and we don't need to start the animation timer
        beq EndOfSetUpAnimationRight

          ; Set animation start time
          lda #FRAMES_PER_ANIMATION                 ; Each movement can be 33 frames
          sta MarioAnimationFrameCountdown

          ; Set mario to running left
          lda #MarioAnimations::RUNNING_RIGHT
          sta MarioAnimationStates


        EndOfSetUpAnimationRight:

        ; We load the velocity to check if its 0. If it is 0, then that mean mario's break has brought any previous acceleration down to 0, and we can therefore have mario 
        ; begin to accelerate in the other direction
        lda XVel
        bne :+
        ; Lets set a flag equal to positive so that way we can determine that he is going to the left
        ; This works because mario never gets knocked back by anything horizontally. So his velocity horizontally should always be from user directed inputs
          lda #%00000001              ; this is positive - this technically represents 1
          sta HorizontalFlag
        :

        lda XPos+1
        cmp #200              ; if XPos is equal or greater, than move on because we do not want to go further than 140 pixels on screen
        bcs MoveScreenRight   ; The carry will be set if the accumulator is greater, since a subtraction was never need to be made from the accumulator
          
          ; If the horizontal flag is plus, then it's ready to move to the right
          ; if not then let's skip
          lda HorizontalFlag
          bpl :+
            bmi NotRight
          :
          
          lda XVel            ; If Velocity isn't greater than Max Velocity
          clc                 ; Clear the carry
          adc #ACCEL          ; Add the acceleration to the velocity
          cmp #MAX_VEL        ; Compare to MAX VEL, if 
          bcc :+              ; If the carry is cleared, that means the max vel is less than or equal to accumulator
            ; If it is set, the accumulator is greater, and we don't want it to be greater than the max velocity, so lets set it
            lda #MAX_VEL       
          :
          sta XVel            ; Lets put it into the XVel
          jmp FinishCheckRight

        MoveScreenRight:
        inc XScroll           ; If XPos is already greater than 200, just scroll right
      
      NotRight:
        lda HorizontalFlag          
        bmi CheckHorizontalFlagRight     ; If Horizontal Flag is negative, meaning the entity is already going to the left, then just bypass this logic

        lda XVel
        cmp #BRAKE              ; If XVel is less than BRAKE then make it be the break
        bcs :+
          lda #0         
          jmp StoreXVelRight
        :
        ; If Xvel is greater than break, just subtract break from XVel
        sec                       
        sbc #BRAKE

        StoreXVelRight:
        sta XVel

      CheckHorizontalFlagRight:
        lda XVel
        bne :+
          lda #0
          sta HorizontalFlag
        :
      FinishCheckRight:

    rts
.endproc