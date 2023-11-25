

;-----------------------------------------;
;----------------CONTROL 1----------------;
;-----------------------------------------;

;-----read inputs of control 1-------;
  readController1:                   ;
    lda #1                           ;
    sta INPUT1                       ;
                                     ;
    sta $4016                        ;
    lda #0                           ;
    sta $4016                        ;
  @readLoop:                         ;
    lda $4016                        ;
    lsr a                            ;
    rol INPUT1                       ;
    bcc @readLoop                    ;
    rts                              ;
;------------------------------------;


;-----------check left button-----------;
  checkLeftButtonPressed1:              ;
    lda INPUT1                          ;
    and #%00000010                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER1_INFORMATION            ;
    sta PARAMETRO1                      ; 
    lda #PLAYER1_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr moveToLeftPlayer                ;
                                        ;
    lda #PLAYER1_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER1_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr loadLeftAnimation               ;                    
                                        ;
    jmp endCheckLeftButtonPressed1      ; 
  @else:                                ;
                                        ; 
  endCheckLeftButtonPressed1:           ;
    rts                                 ;
;---------------------------------------;

;-----------check right button----------;
  checkRightButtonPressed1:             ;
    lda INPUT1                          ;
    and #%00000001                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER1_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER1_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr moveToRightPlayer               ;
                                        ;
    lda #PLAYER1_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER1_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr loadRightAnimation              ;
                                        ;
                                        ;
    jmp endCheckRightButtonPressed1     ; 
  @else:                                ;
                                        ;  
  endCheckRightButtonPressed1:          ;
    rts                                 ;
;---------------------------------------;

;-----------check up button-------------;
  checkUpButtonPressed1:                ;
    lda INPUT1                          ;
    and #%00001000                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER1_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER1_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr moveToUpPlayer                  ;
                                        ;
    ldx PARAMETRO1                      ;
    lda $A, X ;Grounded                 ;
    cmp #$1                             ;
    bne @continue                       ;
      sta $8, x ;JUMPING                ;
    @continue:                          ;
                                        ;  
    jmp endCheckUpButtonPressed1        ;
  @else:                                ;

                                        ;  
  endCheckUpButtonPressed1:             ;
    rts                                 ;
;---------------------------------------;

;-----------check down button-----------;
  checkDownButtonPressed1:              ;
    lda INPUT1                          ;
    and #%00000100                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER1_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER1_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr moveToDownPlayer                ;
    jsr moveToDownPlayer                ;
                                        ;
    lda #$0                             ;
    ldx PARAMETRO1                      ;
    sta $8, x ;JUMPING                  ; 
    sta $9, x ;JUMPING_COUNTER          ;
                                        ;
                                        ;  
    jmp endCheckDownButtonPressed1      ;
  @else:                                ;
                                        ; 
                                        ;
  endCheckDownButtonPressed1:           ;
    rts                                 ;
;---------------------------------------;

;-----------check select button---------;
  checkSelectButtonPressed1:            ;
    lda INPUT1                          ;
    and #%00100000                      ;
    beq @else                           ; 
                                        ; 
    jmp endCheckSelectButtonPressed1    ;
  @else:                                ;
                                        ; 
  endCheckSelectButtonPressed1:         ;
    rts                                 ;
;---------------------------------------;

;-----------check start button----------;
  checkStartButtonPressed1:             ;
    lda INPUT1                          ;
    and #%00010000                      ;
    beq @else                           ;
                                        ;  
    jmp endCheckStartButtonPressed1     ;
  @else:                                ;
                                        ;  
  endCheckStartButtonPressed1:          ;
    rts                                 ;
;---------------------------------------;

;-----------check A button--------------;
  checkAButtonPressed1:                 ;
    lda INPUT1                          ;
    and #%01000000                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER1_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER1_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr initializeProjectile            ;
                                        ;
    jsr loadAtack                       ; 
                                        ;  
    jmp endCheckAButtonPressed1         ;
  @else:                                ;
                                        ;  
  endCheckAButtonPressed1:              ;
    rts                                 ;
;---------------------------------------;

;-----------check B button--------------;
  checkBButtonPressed1:                 ;
    lda INPUT1                          ;
    and #%10000000                      ;
    beq @else                           ;
                                        ;                                    
    ldx #PLAYER1_INFORMATION            ;
    lda #$1                             ;
    sta $E, x ;TAKE_DAMEGE1             ;
                                        ;  
    jmp endCheckBButtonPressed1         ;
  @else:                                ;
                                        ; 
  endCheckBButtonPressed1:              ;
    rts                                 ;
;---------------------------------------;

;-----------check if not moving---------;
  checkNotMovingPressed1:               ;
    lda INPUT1                          ;
    and #%00000011                      ;
    bne @else                           ;
                                        ; 
    lda #PLAYER1_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER1_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr loadStillFrame                  ;
                                        ;
    jmp endcheckNotMovingPressed1       ;
  @else:                                ;
                                        ;  
  endcheckNotMovingPressed1:            ;
    rts                                 ;
;---------------------------------------;







;-----------------------------------------;
;----------------CONTROL 2----------------;
;-----------------------------------------;

;-----read inputs of control 2-------;
  readController2:                   ;
    lda #1                           ;
    sta INPUT2                       ;
                                     ;
    sta $4017                        ;
    lda #0                           ;
    sta $4017                        ;
  @readLoop:                         ;
    lda $4017                        ;
    lsr a                            ;
    rol INPUT2                       ;
    bcc @readLoop                    ;
    rts                              ;
;------------------------------------;



;-----------check left button-----------;
  checkLeftButtonPressed2:              ;
    lda INPUT2                          ;
    and #%00000010                      ;
    beq @else                           ;
                                        ;
                                        ;   
    lda #PLAYER2_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER2_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr moveToLeftPlayer                ;
                                        ;
    lda #PLAYER2_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER2_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr loadLeftAnimation               ;
                                        ;
    jmp endCheckLeftButtonPressed2      ; 
  @else:                                ;
                                        ;  
  endCheckLeftButtonPressed2:           ;
    rts                                 ;
;---------------------------------------;

;-----------check right button----------;
  checkRightButtonPressed2:             ;
    lda INPUT2                          ;
    and #%00000001                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER2_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER2_SPRITES                ;
    sta PARAMETRO2                      ;         
    jsr moveToRightPlayer               ;
                                        ;
    lda #PLAYER2_INFORMATION            ;        
    sta PARAMETRO1                      ;
    lda #PLAYER2_SPRITES                ;
    sta PARAMETRO2                      ;
    JSR loadRightAnimation              ;
                                        ;
                                        ;
    jmp endCheckRightButtonPressed2     ; 
  @else:                                ;
                                        ; 
  endCheckRightButtonPressed2:          ;
    rts                                 ;
;---------------------------------------;

;-----------check up button-------------;
  checkUpButtonPressed2:                ;
    lda INPUT2                          ;
    and #%00001000                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER2_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER2_SPRITES                ;
    sta PARAMETRO2                      ;         
    jsr moveToUpPlayer                  ;
                                        ;
    ldx PARAMETRO1                      ;
    lda $A, X ;Grounded                 ;
    cmp #$1                             ;
    bne @continue                       ;
      sta $8, x ;JUMPING                ;
    @continue:                          ;
                                        ; 
    jmp endCheckUpButtonPressed2        ;
  @else:                                ;
                                        ;
                                        ; 
  endCheckUpButtonPressed2:             ;
    rts                                 ;
;---------------------------------------;

;-----------check down button-----------;
  checkDownButtonPressed2:              ;
    lda INPUT2                          ;
    and #%00000100                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER2_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER2_SPRITES                ;
    sta PARAMETRO2                      ;         
    jsr moveToDownPlayer                ;
    jsr moveToDownPlayer                ;
                                        ;
    lda #$0                             ;
    ldx PARAMETRO1                      ;
    sta $8, x ;JUMPING                  ;
    sta $9, x ;JUMPING_COUNTER          ;
                                        ;     
    jmp endCheckDownButtonPressed2      ;
  @else:                                ;
                                        ; 
                                        ;
  endCheckDownButtonPressed2:           ;
    rts                                 ;
;---------------------------------------;

;-----------check select button---------;
  checkSelectButtonPressed2:            ;
    lda INPUT2                          ;
    and #%00100000                      ;
    beq @else                           ; 
                                        ;  
    jmp endCheckSelectButtonPressed2    ;
  @else:                                ;
                                        ; 
  endCheckSelectButtonPressed2:         ;
    rts                                 ;
;---------------------------------------;

;-----------check start button----------;
  checkStartButtonPressed2:             ;
    lda INPUT2                          ;
    and #%00010000                      ;
    beq @else                           ;
                                        ;  
    jmp endCheckStartButtonPressed2     ;
  @else:                                ;
                                        ; 
  endCheckStartButtonPressed2:          ;
    rts                                 ;
;---------------------------------------;

;-----------check A button--------------;
  checkAButtonPressed2:                 ;
    lda INPUT2                          ;
    and #%01000000                      ;
    beq @else                           ;
                                        ;
    lda #PLAYER2_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER2_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr initializeProjectile            ;
                                        ;
    jsr loadAtack                       ;
                                        ;   
    jmp endCheckAButtonPressed2         ;
  @else:                                ;
                                        ;
  endCheckAButtonPressed2:              ;
    rts                                 ;
;---------------------------------------;

;-----------check B button--------------;
  checkBButtonPressed2:                 ;
    lda INPUT2                          ;
    and #%10000000                      ;
    beq @else                           ;
                                        ;
    ldx #PLAYER2_INFORMATION            ;
    lda #$1                             ;
    sta $E, x ;TAKE_DAMEGE1             ;
                                        ;   
    jmp endCheckBButtonPressed2         ;
  @else:                                ;
                                        ;  
  endCheckBButtonPressed2:              ;
    rts                                 ;
;---------------------------------------;

;-----------check if not moving---------;
  checkNotMovingPressed2:               ;
    lda INPUT2                          ;
    and #%00000011                      ;
    bne @else                           ;
                                        ;
    lda #PLAYER2_INFORMATION            ;
    sta PARAMETRO1                      ;
    lda #PLAYER2_SPRITES                ;
    sta PARAMETRO2                      ;
    jsr loadStillFrame                  ;
                                        ;
    jmp endcheckNotMovingPressed2       ;
  @else:                                ;
                                        ; 
  endcheckNotMovingPressed2:            ;
    rts                                 ;
;---------------------------------------;