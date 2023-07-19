         ; lda #0
                ; sta ParamYPos
                ; sta ParamXPos

                ; lda Column
                ; and #%00011111
                abcdef:
                    ; problem starts here
                   ; /*
                        ; 128   64  32    16  8   4   2   1
                        ; [x]  [x]  [x]  []  []  []  []  []

                    ;*/
                    ; cmp #32
                    ; bcc DetermineY
                        ; sec
                        ; sbc #32
                        ; bpl abcdef
                DetermineY:
            ;     tax     ; hold the counter that was column
            ;     Loop2:
            ;       lda ParamYPos 
            ;       clc
            ;       adc #8
            ;       sta ParamYPos
            ;       dex
            ;       bne Loop2

            ;   SetXParam:
            ;     lda XScroll
            ;     sta ParamXPos

            ;     ; jsr AddNewCollidable
            ; ldx #255
            ; af:
            ;     dex
            ;     bne af

            ;     ldx #0                            ; X = 0
            ; ArrayLoop:
            ;     cpx #MAX_COLLIDABLES * .sizeof(Collidable)  ; Reached maximum number of collidables allowed in the array?
            ;     beq EndRoutine                    ; Then we skip and don't add a new actor
            ;     lda CollidablesArray+Collidable::Type,x
            ;     cmp #CollidableType::NULL              ; If the collidable type of this array position is NULL
            ;     beq AddNewCollidableToArray            ;   Then: we found an empty slot, proceed to add collidable to position [x]
            ; NextCollidable:
            ;     ; txa
            ;     ; clc
            ;     ; adc #.sizeof(Collidable)               ; Otherwise, we offset to the check the next collidable in the array
            ;     ; tax                               ; X += sizeof(collidable)
            ;     jmp ArrayLoop

            ; AddNewCollidableToArray:                 ; Here we add a new collidable at index [x] of the array
            ;     ; lda #CollidableType::COLLIDABLE         ; They should all be of type collidable
            ;     ; sta CollidablesArray+Collidable::Type,x
            ;     ; lda ParamXPos                     ; Fetch parameter "collidable position X" from RAM
            ;     ; sta CollidablesArray+Collidable::XPos,x
            ;     ; lda ParamYPos                     ; Fetch parameter "collidable position Y" from RAM
            ;     ; sta CollidablesArray+Collidable::YPos,x
            ; EndRoutine:
