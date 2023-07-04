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

    EndNoButtons:
    ; CheckAButton:
    ;     lda Buttons
    ;     and #BUTTON_A
    ;     beq EndAButton
    ;         lda Buttons
    ;         and #BUTTON_A
    ;         cmp PrevButtons        ; Compare the current buttons and check if button A was previously pressed
    ;         beq EndAButton                 ; If it was, we bypass the creation of a new missile
    ;         ; lets do something....
    ; EndAButton:
    CheckLeftButton:
        lda Buttons
        and #BUTTON_LEFT
        beq NotLeft

            ; Check to see if mario was already running left first to restart animation
            lda MarioAnimationStates
            cmp #MarioAnimations::RUNNING_LEFT
            ; If equals, then it already was running left and we don't need to start the animation timer
            beq :+
              ; Set animation start time
              lda #FRAMES_PER_ANIMATION                 ; Each movement can be 33 frames
              sta MarioAnimationFrameCountdown

              ; Set mario to running left
              lda #MarioAnimations::RUNNING_LEFT
              sta MarioAnimationStates
            :

            lda XPos
            cmp #10
            bcc NotLeft             ; if XPos is equal to 10, we want to branch it because it shouldn't be less than 0! 
                lda XVel
                beq :+
                    bpl NotLeft                 ; Skip if velocity is positive
                :
                sec 
                sbc #ACCEL                    ; Subtract the acceleration from the velocity
                cmp #256-MAX_VEL              ; If velocity - acceleration is greater than 256 - max speed, carry will be set , else cleared But prevent it from going over the max. speed
                bcs :+
                    lda #256-MAX_VEL
                :
                sta XVel



                jmp FinishCheckLeft

            NotLeft:
                lda XVel
                bpl FinishCheckLeft
                cmp #256-BRAKE
                bcc :+
                    lda #256-BRAKE
                :
                adc #BRAKE
                sta XVel
    FinishCheckLeft:
    CheckRightButton:
      lda Buttons
      and #BUTTON_RIGHT
      beq NotRight

          ; Check to see if mario was already running right first to restart animation
          lda MarioAnimationStates
          cmp #MarioAnimations::RUNNING_RIGHT
          ; If equals, then it already was running right and we don't need to start the animation timer
          beq :+
            ; Set animation start time
            lda #FRAMES_PER_ANIMATION                 ; Each movement can be 33 frames
            sta MarioAnimationFrameCountdown

            ; Set mario to running left
            lda #MarioAnimations::RUNNING_RIGHT
            sta MarioAnimationStates
          :

        lda XPos
        cmp #200              ; if XPos is equal or greater, than move on because we do not want to go further than 140 pixels on screen
        bcs MoveScreenRight   ; The carry will be set if the accumulator is greater, since a subtraction was never need to be made from the accumulator
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
        inc XScroll           ; If XPos is already greater than 140, just scroll right
      
      NotRight:
        lda XVel
        bmi FinishCheckRight     ; If Velocity is negative, meaning the entity is already going to the left, then just bypass this logic
        cmp #BRAKE              ; If XVel is less than BRAKE then make it be the break
        bcs :+
          lda #BRAKE+1          ; Magic
        :
        ; sec                   is the carry already set from the previous instruction? I think so
        sbc #BRAKE
        sta XVel
      FinishCheckRight:

    rts
.endproc