
.include "constants.inc"
.include "header.inc"

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

; Main code segment for the program
.segment "CODE"



reset:
  sei		          ; disable IRQs
  cld		          ; disable decimal mode
  ldx #$40
  stx $4017	      ; disable APU frame IRQ
  ldx #$ff       	; Set up stack
  txs		
  inx		          ; now X = 0
  stx PPUCTRL	    ; disable NMI
  stx PPUMASK 	  ; disable rendering
  stx $4010 	    ; disable DMC IRQs

;---------------------- first wait for vblank to make sure PPU is ready
vblankwait1:          ;
  bit PPUSTATUS       ;
  bpl vblankwait1     ;
                      ;
clear_memory:         ;
  lda #$00            ;
  sta $0000, x        ;
  sta $0100, x        ;
  sta $0200, x        ;
  sta $0300, x        ;
  sta $0400, x        ;
  sta $0500, x        ;
  sta $0600, x        ;
  sta $0700, x        ;
  inx                 ;
  bne clear_memory    ;
                      ;
                      ; second wait for vblank, PPU is ready after this
vblankwait2:          ;
  bit PPUSTATUS       ;
  bpl vblankwait2     ;
                      ;
;----------------------

main:

jsr initializePlayer1

jsr load_default_palettes

jsr LoadAttr

jsr LoadBackground1

;-----------;
  ldx #$0000;
  stx $2003 ;This is where the 64 sprites will be stored.
;-----------;


;PPUCTRL
; 7  bit  0
; ---- ----
; VPHB SINN
; |||| ||||
; |||| ||++- Base nametable address
; |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
; |||| |+--- VRAM address increment per CPU read/write of PPUDATA
; |||| |     (0: add 1, going across; 1: add 32, going down)
; |||| +---- Sprite pattern table address for 8x8 sprites
; ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
; |||+------ Background pattern table address (0: $0000; 1: $1000)
; ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
; |+-------- PPU master/slave select
; |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
; +--------- Generate an NMI at the start of the
;            vertical blanking interval (0: off; 1: on)


; PPUMASK
; 7  bit  0
; ---- ----
; BGRs bMmG
; |||| ||||
; |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
; |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
; |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
; |||| +---- 1: Show background
; |||+------ 1: Show sprites
; ||+------- Emphasize red (green on PAL/Dendy)
; |+-------- Emphasize green (red on PAL/Dendy)
; +--------- Emphasize blue


  ; lda #%10111000	      ; Enable NMI
  ; sta PPUCTRL           ; $2000
  ; lda #%00011110	      ; Enable Sprites
;-----------------------;
enable_rendering:       ;
  lda #%10010000	      ; Enable NMI
  sta PPUCTRL           ; $2000
  lda #%00011010	      ; Enable Sprites
  sta PPUMASK           ; $2001
;-----------------------;

;----------------
forever:       

  jmp forever   ;
;----------------


nmi:  
  ;*****************   Start of NMI   ***************************
  ;-----;
  PHA   ;                                
  TXA   ;                                                         
  PHA   ; Save the registers on the stack                                                    
  TYA   ;                                                         
  PHA   ;       
  ;-----;
  jsr calculatePlayerCoordinates
  
  jsr readController1

  jsr loadMiddleSprite
  jsr checkLeftButtonPressed
  jsr checkRightButtonPressed
  jsr checkUpButtonPressed 
  jsr checkDownButtonPressed
  jsr checkSelectButtonPressed 
  jsr checkStartButtonPressed
  
  jsr checkBButtonPressed 
  jsr checkNotMovingPressed


  jsr checkAndFixMirroring
  

  jsr checkGrounded1

  lda JUMPING
  cmp #1
  beq @continue
  jsr gravityEffect
  @continue:

  jsr jumpingManager

  jsr checkAButtonPressed 

  jsr projectileManager

  jsr blinkPlayer1

  ;-----------;
  ldx #$02    ;
  stx $4014   ;
  ;-----------;

  

  ;-----;
  PLA   ;                                    
  TAY   ;                                                        
  PLA   ;                                                        
  TAX   ; Restore values from stack                                                    
  PLA   ;                                                         
  ;-----;   

  RTI   ; NMI is done      
  ;*********************   End of NMI  ****************************




;--------The next pieces of code are subroutines!----------------


;-----read inputs of control 1-------;
  readController1:                   ;
    lda #1                           ;
    sta INPUT1                       ;
                                     ;
    sta $4016                        ;
    lda #0                           ;
    sta $4016                        ;
  readLoop:                          ;
    lda $4016                        ;
    lsr a                            ;
    rol INPUT1                       ;
    bcc readLoop                     ;
    rts                              ;
;------------------------------------;

;-----------check left button------------;
  checkLeftButtonPressed:                ;
    lda INPUT1                              ;
    and #%00000010                       ;
    beq @else                            ;
    jsr loadLeftAnimation
    jsr loadLeftPressedSprite            ;  <--- if pressed
    jsr moveToLeftPlayer1
    jmp endCheckLeftButtonPressed        ; 
  @else:                                 ;
    jsr loadLeftSprite                   ;  <--- if not
  endCheckLeftButtonPressed:             ;
    rts                                  ;
;----------------------------------------;

;-----------check right button-----------;
  checkRightButtonPressed:               ;
    lda INPUT1                              ;
    and #%00000001                       ;
    beq @else  
    jsr loadRightAnimation                          ;
    jsr loadRightPressedSprite           ;  <--- if pressed
    jsr moveToRightPlayer1
    jmp endCheckRightButtonPressed       ;
  @else:                                 ;
    jsr loadRightSprite                  ;  <--- if not
  endCheckRightButtonPressed:            ;
    rts                                  ;
;----------------------------------------;

;-----------check up button--------------;
  checkUpButtonPressed:                  ;
    lda INPUT1                              ;
    and #%00001000                       ;
    beq @else                            ;

    lda GROUNDED
    cmp #$1
    bne @continue
    sta JUMPING
    @continue:

    jsr loadUpPressedSprite              ;  <--- if pressed
    jmp endCheckUpButtonPressed          ;
  @else:                                 ;

    ; lda #$0
    ; sta JUMPING

    jsr loadUpSprite                     ;  <--- if not
  endCheckUpButtonPressed:               ;
    rts                                  ;
;----------------------------------------;

;-----------check down button------------;
  checkDownButtonPressed:                ;
    lda INPUT1                           ;
    and #%00000100                       ;
    beq @else                            ;

    lda #$0
    sta JUMPING
    sta JUMPING_COUNTER

    jsr moveDownPlayer1
    jsr moveDownPlayer1
    
    jsr loadDownPressedSprite            ;  <--- if pressed
    jmp endCheckDownButtonPressed        ;
  @else:                                 ;
    jsr loadDownSprite                   ;  <--- if not

  endCheckDownButtonPressed:             ;
    rts                                  ;
;----------------------------------------;

;-----------check select button----------;
  checkSelectButtonPressed:              ;
    lda INPUT1                           ;
    and #%00100000                       ;
    beq @else                            ; 
    jsr loadSelectPressedSprite          ;  <--- if pressed
    jmp endCheckSelectButtonPressed      ;
  @else:                                 ;
    jsr loadSelectSprite                 ;  <--- if not
  endCheckSelectButtonPressed:           ;
    rts                                  ;
;----------------------------------------;

;-----------check start button-----------;
  checkStartButtonPressed:               ;
    lda INPUT1                             ;
    and #%00010000                       ;
    beq @else                            ;
    jsr loadStartPressedSprite           ;  <--- if pressed
    jmp endCheckStartButtonPressed       ;
  @else:                                 ;
    jsr loadStartSprite                  ;  <--- if not
  endCheckStartButtonPressed:            ;
    rts                                  ;
;----------------------------------------;

;-----------check A button---------------;
  checkAButtonPressed:                   ;
    lda INPUT1                             ;
    and #%01000000                       ;
    beq @else                            ;
    jsr loadAtack
    jsr initializeProjectile
    jsr loadAPressedSprite               ;  <--- if pressed
    jmp endCheckAButtonPressed           ;
  @else:                                 ;
    jsr loadASprite                      ;  <--- if not
  endCheckAButtonPressed:                ;
    rts                                  ;
;----------------------------------------;

;-----------check B button---------------;
  checkBButtonPressed:                   ;
    lda INPUT1                             ;
    and #%10000000                       ;
    beq @else                            ;
    lda #$1
    sta TAKE_DAMEGE1
    jsr loadBPressedSprite               ;  <--- if pressed
    jmp endCheckBButtonPressed           ;
  @else:                                 ;
    jsr loadBSprite                      ;  <--- if not
  endCheckBButtonPressed:                ;
    rts                                  ;
;----------------------------------------;

;-----------check if not moving----------;
  checkNotMovingPressed:                 ;
    lda INPUT1                           ;
    and #%00000011                       ;
    bne @else                            ;

      lda GROUNDED
      cmp #$1
      bne @continue
      jsr loadStillFrame                 ;  <--- if not moving
      @continue:
    jmp endcheckNotMovingPressed         ;
  @else:                                 ;
                                         ;  <--- if moving
  endcheckNotMovingPressed:              ;
    rts                                  ;
;----------------------------------------;

  

;--Load attribute table---------;
LoadAttr:                       ;
    lda PPUSTATUS               ;
    LDA #$23                    ;
    STA $2006                   ;
    LDA #$c0                    ;
    STA $2006                   ;
    LDX #$00                    ;
LoadAttrLoop:                   ;
    LDA attributeTable0, x      ;
    STA $2007                   ;
    INX                         ;
    CPX #$40                    ;
    BNE LoadAttrLoop            ;
    rts                         ;
;-------------------------------;

;--Deafault Palettes----------------;
load_default_palettes:              ;
  lda PPUSTATUS                     ;
  lda #$3f                          ;
  sta PPUADDR                       ;
  lda #$00                          ;
  sta PPUADDR                       ;
  ldx #$00                          ;
                                    ;
@load_default_palettes_loop:        ;
  lda defaultPalettes, x            ;
  sta PPUDATA                       ;
  inx                               ;
  cpx #$20                          ;
  bne @load_default_palettes_loop   ;
  rts                               ;
;-----------------------------------;



;----------load the background----------;
LoadBackground1:                        ;
    ;reset scroll                       ;
    lda #$00                            ;
    sta $2005                           ;
    sta $2005                           ;
                                        ;
    lda PPUSTATUS                       ;
    LDA #$20                            ;
    STA $2006                           ;
    LDA #$00                            ;
    STA $2006                           ;
    LDX #$00                            ;
LoadBackgroundLoop1:                    ;
    LDA background, x                   ;
    STA $2007                           ;
    INX                                 ;
    CPX #$00                            ;
    BNE LoadBackgroundLoop1             ;
                                        ;
LoadBackgroundLoop2:                    ;
    LDA background+256, x               ;
    STA $2007                           ;
    INX                                 ;
    CPX #$00                            ;
    BNE LoadBackgroundLoop2             ;
                                        ;
LoadBackgroundLoop3:                    ;
    LDA background+512, x               ;
    STA $2007                           ;
    INX                                 ;
    CPX #$00                            ;
    BNE LoadBackgroundLoop3             ;
                                        ;
LoadBackgroundLoop4:                    ;
    LDA background+768, x               ;
    STA $2007                           ;
    INX                                 ;
    CPX #$c0                            ;
    BNE LoadBackgroundLoop4             ;
    rts                                 ;
;---------------------------------------;


;----Initialize values for player 1----;
initializePlayer1:                     ;
  ldx #$00                             ;
  stx INDEX_FRAME1                     ; INDEX_FRAME = 0
  stx COUNTER_FRAME1                   ; COUNTER_FRAME1 = 0
  stx FIX_MIRRORING                    ; FIX_MIRRORING = 0
                                       ;
  ldx #$0006                           ;
  stx NMI_FRECUENCY_FRAME1             ; NMI_FRECUENCY_FRAME1 = 6
                                       ;
  ldx #$01                             ;
  stx DIRECTION1                       ; DIRECTION1 = 1 = left    ***0 = right***
                                       ;
  jsr initializePlayer1Sprites         ; loads the sprites of the player looking still to the left
  rts                                  ;
                                       ;
initializePlayer1Sprites:              ;
  ldx #$00                             ;
  @loop:                               ;        
    lda initializationSprites, x       ;
    sta $0224, x                       ; 
    inx                                ;
    cpx #$10                           ;
    bne @loop                          ;
  rts                                  ;
;--------------------------------------;


; 0234 y
; 0235 tile indx
; 0236 attr
; 0237 x
initializeProjectile:

  lda PROJECTILE
  cmp #1
  beq endInitializeProjectile

    lda #$1
    sta PROJECTILE

    lda DIRECTION1
    sta PROJECTILE_DIRECTION

    lda PLAYER1_Y
    sta $0234 ; Y

    lda #$33
    sta $0235 ; tile

    lda #$04
    sta $0236 ; attr

    lda PROJECTILE_DIRECTION ; x
    cmp #$0
    beq @continue
      lda PLAYER1_X
      clc
      sbc #$10
      sta $0237
      jmp endInitializeProjectile
    @continue:
    lda PLAYER1_X
    clc
    adc #$8
    sta $0237

endInitializeProjectile:
rts

destroyProjectile:
  lda #$0
  sta PROJECTILE
  sta PROJECTILE_COUNTER

  lda #$0
  sta $0234

  lda #$0
  sta $0235

  lda #$04
  sta $0236

  lda #$0
  sta $0237


endDestroyProjectile:
rts

projectileManager:
  lda PROJECTILE
  cmp #0
  beq @continue1
    inc PROJECTILE_COUNTER
    lda PROJECTILE_COUNTER
    cmp #PROJECTILE_TIME
    bne @continue2
      jsr destroyProjectile
    @continue2:
    lda PROJECTILE_DIRECTION
    cmp #$0
    beq @continue3
      dec $0237
      dec $0237
      dec $0237
      jmp @continue1
    @continue3:
    inc $0237
    inc $0237
    inc $0237

  @continue1:

endProjectileManager:
rts

;-----------------------------------------------
;Antes de llamar la subrutina, cargen los valores en las direcciones: NUM1 = DIVIDENDO, NUM2 = DIVISOR
;Despues de llamar la rutina, el cociente estara en la direccion NUM1 y el residuo en el acumulador.
division:
  LDA #0
  LDX #8
  ASL NUM1
L1: 
  ROL
  CMP NUM2
  BCC L2
  SBC NUM2
L2: 
  ROL NUM1
  DEX
  BNE L1
rts
;-----------------------------------------------

;-----------------------------------------------
; Subrutina de Multiplicación en el 6502
; Multiplica dos números de 1 byte: NUM1 y NUM2
; Guarda el resultado en RESULT_HI y RESULT_LOW
MULTIPLY:
  ldx #0      ; Inicializa X a 0 (contador para las sumas)
  lda #0      ; Inicializa A a 0 (acumulador para el resultado bajo)
  sta RESULT_LOW
  lda #0      ; Inicializa A a 0 (acumulador para el resultado alto)
  sta RESULT_HI

  ldy NUM1    ; Carga Y con el primer número (NUM1)

LOOP:
  cpy #0      ; Comprueba si Y es 0
  beq DONE    ; Si es 0, termina

  lda RESULT_LOW   ; Suma NUM2 a RESULT_LOW
  clc
  adc NUM2
  sta RESULT_LOW

  lda RESULT_HI   ; Añade el carry a RESULT_HI
  adc #0
  sta RESULT_HI

  dey         ; Decrementa Y
  bne LOOP    ; Si Y no es 0, repite el bucle

DONE:
  rts         ; Retorna de la subrutina
;-----------------------------------------------






;-----------------------------------------------
;x/64 + (y/8*4) byte index in collisionMap
;x/8 and %0111
checkCollide:
  txa      ; x/64
  lsr
  lsr
  lsr
  lsr
  lsr
  lsr
  sta TEMP
  TYA       ;(y/8)
  lsr
  lsr
  lsr
  asl   
  asl        ;*4
  clc
  adc TEMP
  TAY       ;byte index

  txa
  lsr
  lsr
  lsr
  and #%0111
  tax

  lda collisionMap, Y
  and bitMaskTable, x
  rts
;-----------------------------------------------

calculatePlayerCoordinates:
  lda $0227 ;set x
  cmp $022B
  bpl @continue 
  lda $022B ;set x
  @continue:
  sta PLAYER1_X

  lda $0224 ;set y
  sta PLAYER1_Y
  rts

;----------------------l
setCoordinatesBottomLeft:
  lda $0224 ;set y
  clc
  adc #$8
  tay

  lda $0227 ;set x
  cmp $022B
  bpl @continue 
  lda $022B ;set x
@continue:
  tax
  rts
;----------------------

;----------------------l
setCoordinatesBottomRight:
  lda $0224 ;set y
  clc
  adc #$8
  tay

  lda $0227 ;set x
  cmp $022B
  bpl @continue 
  lda $022B ;set x
@continue:
  clc 
  adc #$7
  tax
  rts
;----------------------

;----------------------l
setCoordinatesTopLeft:
  lda $0224 ;set y
  clc
  adc #$1
  tay

  lda $0227 ;set x
  cmp $022B
  bpl @continue 
  lda $022B ;set x
@continue:
  tax
  rts
;----------------------

;----------------------l
setCoordinatesTopRight:
  lda $0224 ;set y
  clc
  adc #$1
  tay

  lda $0227 ;set x
  cmp $022B
  bpl @continue 
  lda $022B ;set x
@continue:
  clc 
  adc #$7
  tax
  rts
;----------------------

;Moves to the left the 4 tiles saved in RAM by 1 pixels.
;---------------------;
moveToLeftPlayer1:    ;
  dec $0227           ; 
  dec $022B           ;
  dec $022F           ;
  dec $0233           ;

  jsr setCoordinatesTopLeft
  jsr checkCollide
  beq @continue1
  inc $0227           ; 
  inc $022B           ;
  inc $022F           ;
  inc $0233           ;
  @continue1:

  jsr setCoordinatesBottomLeft
  jsr checkCollide
  beq @continue2
  inc $0227           ; 
  inc $022B           ;
  inc $022F           ;
  inc $0233           ;
  @continue2:
                      ;
endMoveToLeftPlayer1:
  rts
;---------------------;




;Moves to the right the 4 tiles saved in RAM by 2 pixels.
;---------------------;
moveToRightPlayer1:   ;
  inc $0227           ;
  inc $022B           ;
  inc $022F           ;
  inc $0233           ;

  jsr setCoordinatesBottomRight
  jsr checkCollide
  beq @continue1
  dec $0227           ;
  dec $022B           ;
  dec $022F           ;
  dec $0233           ;
  @continue1:

  jsr setCoordinatesBottomRight
  jsr checkCollide
  beq @continue2
  dec $0227           ;
  dec $022B           ;
  dec $022F           ;
  dec $0233           ;
  @continue2:
endMoveToRightPlayer1:
  rts                 ;
;---------------------;



;Moves up the 4 tiles saved in RAM by 2 pixels.
;---------------------;
moveUpPlayer1:        ;
  dec $0224           ;
  dec $0228           ;
  dec $022c           ;
  dec $0230           ;

  jsr setCoordinatesTopLeft
  jsr checkCollide
  beq @continue1
  inc $0224           ;
  inc $0228           ;
  inc $022c           ;
  inc $0230           ;
  @continue1:

  jsr setCoordinatesTopRight
  jsr checkCollide
  beq @continue2
  inc $0224           ;
  inc $0228           ;
  inc $022c           ;
  inc $0230           ;
  @continue2:
  

endMoveUpPlayer1:
  rts                 ;
;---------------------;

;Moves down the 4 tiles saved in RAM by 2 pixels.
;---------------------;
moveDownPlayer1:      ;
  inc $0224           ;
  inc $0228           ;
  inc $022C           ;
  inc $0230           ;

  ; ldx #$01
  ; stx JUMPING

  jsr setCoordinatesBottomLeft
  jsr checkCollide
  beq @continue1
  dec $0224           ;
  dec $0228           ;
  dec $022C           ;
  dec $0230           ;
  ; ldx #$00
  ; stx JUMPING
  @continue1:

  jsr setCoordinatesBottomRight
  jsr checkCollide
  beq @continue2
  dec $0224           ;
  dec $0228           ;
  dec $022C           ;
  dec $0230           ;
  ; ldx #$00
  ; stx JUMPING
  @continue2:

endMoveDownPlayer1:
  rts                 ;
;---------------------;


;Moves down the 4 tiles saved in RAM by 2 pixels.
;---------------------;
checkGrounded1:      ;
  inc $0224           ;
  inc $0228           ;
  inc $022C           ;
  inc $0230           ;

  ldx #$00
  stx GROUNDED

  jsr setCoordinatesBottomLeft
  jsr checkCollide
  beq @continue1
  ldx #$01
  stx GROUNDED
  @continue1:

  jsr setCoordinatesBottomRight
  jsr checkCollide
  beq @continue2
  ldx #$01
  stx GROUNDED
  @continue2:

  dec $0224           ;
  dec $0228           ;
  dec $022C           ;
  dec $0230           ;

  lda GROUNDED
  cmp #$0
  bne endcheckJunping1
  jsr loadinAirFrame

endcheckJunping1:
  rts                 ;
;---------------------;



;---------------------;
gravityEffect:
 jsr moveDownPlayer1
 jsr moveDownPlayer1
rts
;---------------------;

;---------------------;
jumpingManager:
  lda JUMPING
  cmp #$0
  beq @continue
    jsr moveUpPlayer1
    jsr moveUpPlayer1
    jsr moveUpPlayer1
    inc JUMPING_COUNTER
    lda JUMPING_COUNTER
    cmp #JUNPING_TIME
    bne @continue
    lda #$0
    sta JUMPING
    sta JUMPING_COUNTER
  @continue:


endJumpingManager:
rts
;---------------------;


blinkPlayer1:
  lda TAKE_DAMEGE1
  cmp #$0
  beq endBlinkPlayer1

  inc BLINK_PLAYER_COUNTER
  inc BLINK_PLAYER_TIMER
  lda BLINK_PLAYER_COUNTER
  cmp #$8
  bmi @invisible
    cmp #$10
    beq @reset 
      jmp @continue
    @reset: 
    lda #$0
    sta BLINK_PLAYER_COUNTER
    jmp @continue
  @invisible:
  lda #$0
  sta $0225
  sta $0229
  sta $022D
  sta $0231
  @continue:

  lda BLINK_PLAYER_TIMER
  cmp #$50
  bne endBlinkPlayer1
    lda #$0
    sta TAKE_DAMEGE1
    sta BLINK_PLAYER_COUNTER
    sta BLINK_PLAYER_TIMER

endBlinkPlayer1:
rts



;----------------------------------;
loadStillFrame:                    ;
  lda DIRECTION1                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadStillRight             ;If islooking to the left
    jmp endLoadStillFrame          ;
  @else:                           ;
    jsr loadStillLeft              ;If islooking to the right
endLoadStillFrame:                 ;
  rts                              ;
                                   ;
loadStillLeft:                     ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda stillLeft, x               ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda stillLeft, x               ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadStillRight:                    ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda stillRight, x              ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda stillRight, x              ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;



;----------------------------------;
loadAtack:                         ;
  lda DIRECTION1                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadRightAtack             ;If islooking to the left
    jmp endLoadinAirFrame          ;
  @else:                           ;
    jsr loadLeftAtack              ;If islooking to the right
endLoadAtack:                 ;
  rts                              ;
                                   ;
loadLeftAtack:                     ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda atackLeft, x               ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda atackLeft, x               ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$04                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadRightAtack:                    ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda atackRight, x              ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda atackRight, x              ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$04                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

;----------------------------------;
loadinAirFrame:                    ;
  lda DIRECTION1                   ;
  and #%00000001                   ; 
  bne @else                        ;
    jsr loadinAirRight             ;If islooking to the left
    jmp endLoadinAirFrame          ;
  @else:                           ;
    jsr loadinAirLeft              ;If islooking to the right
endLoadinAirFrame:                 ;
  rts                              ;
                                   ;
loadinAirLeft:                     ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda inAirLeft, x               ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda inAirLeft, x               ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
                                   ;
loadinAirRight:                    ;
  ldx #$00                         ;
  ldy #$00                         ;
  @loop:                           ;
    lda inAirRight, x              ;
    sta $0225, y                   ;
    iny                            ;
    inx                            ;
    lda inAirRight, x              ;
    sta $0225, y                   ;
    inx                            ;
    iny                            ;
    iny                            ;
    iny                            ;
    cpx #$08                       ;
    bne @loop                      ;
  rts                              ;
;----------------------------------;

;Load the animation of walking to the left
;-----------------------------------;
loadLeftAnimation:                  ;
  ; lda PLAYER_STATUS1              ;
  ; ora %00000001                   ;
  ; sta PLAYER_STATUS1              ;
                                    ;
  lda DIRECTION1                    ; 
  cmp #$00                          ;
  bne @continue                     ;
  lda #$01                          ;
  sta FIX_MIRRORING                 ;
  @continue:                        ;
                                    ;
  lda #$01                          ;
  sta DIRECTION1                    ;
                                    ;
  lda INDEX_FRAME1                  ;
  ldx $00                           ;
                                    ;
  cmp #$00                          ;
  beq loadLeftFrame1                ;
  cmp #$01                          ;
  beq loadLeftFrame2                ;
  cmp #$02                          ;
  beq loadLeftFrame3                ;
  cmp #$03                          ;
  beq loadLeftFrame4                ;
endLoadLeftAnimation:               ;
  clc                               ;
  lda COUNTER_FRAME1                ;
  adc #$01                          ;
  sta COUNTER_FRAME1                ;
  cmp NMI_FRECUENCY_FRAME1          ;
  bne @else1                        ;
    lda #$00                        ;
    sta COUNTER_FRAME1              ;
                                    ;
    lda INDEX_FRAME1                ;
    clc
    adc #$01                        ;
    cmp #$04                        ;
    bne @else2                      ;
      lda #$00                      ;
    @else2:                         ;
      sta INDEX_FRAME1              ;
  @else1:                           ;
    rts                             ;
                                    ;
                                    ;
loadLeftFrame1:                     ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda leftFrame1, x               ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda leftFrame1, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation          ;
                                    ;
loadLeftFrame2:                     ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda leftFrame2, x               ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda leftFrame2, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation          ;
                                    ;
loadLeftFrame3:                     ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda leftFrame3, x               ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda leftFrame3, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation          ;
                                    ;
loadLeftFrame4:                     ;
  ldx #$00                          ;
  ldy #$00                          ; 
  @loop:                            ;  
    lda leftFrame4, x               ;
    sta $0225, y                    ; 
    iny                             ; 
    inx                             ; 
    lda leftFrame4, x               ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ; 
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadLeftAnimation          ;
;-----------------------------------;

;Load the animation of walking to the right
;-----------------------------------;
loadRightAnimation:                 ;
  ; lda PLAYER_STATUS1              ;
  ; and %11111110                   ;
  ; sta PLAYER_STATUS1              ;
                                    ;
  lda DIRECTION1                    ; 
  cmp #$01                          ;
  bne @continue                     ;
  lda #$01                          ;
  sta FIX_MIRRORING                 ;
  @continue:                        ;
                                    ;
  lda #$00                          ; 
  sta DIRECTION1                    ;
                                    ;
  lda INDEX_FRAME1                  ;
  ldx $00                           ;
                                    ;
  cmp #$00                          ;
  beq loadRightFrame1               ;
  cmp #$01                          ;
  beq loadRightFrame2               ;
  cmp #$02                          ;
  beq loadRightFrame3               ;
  cmp #$03                          ;
  beq loadRightFrame4               ; 
endLoadRightAnimation:              ;
  clc                               ;
  lda COUNTER_FRAME1                ;
  adc #$01                          ;
  sta COUNTER_FRAME1                ;
  cmp NMI_FRECUENCY_FRAME1          ;
  bne @else1                        ;
    lda #$00                        ;
    sta COUNTER_FRAME1              ;
                                    ;
    lda INDEX_FRAME1                ;
    clc
    adc #$01                        ;
    cmp #$04                        ;
    bne @else2                      ;
      lda #$00                      ; 
    @else2:                         ;
      sta INDEX_FRAME1              ; 
  @else1:                           ;
    rts                             ;
                                    ;
                                    ;
loadRightFrame1:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda rightFrame1, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame1, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ; 
  jmp endLoadRightAnimation         ;
                                    ;
loadRightFrame2:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ; 
    lda rightFrame2, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame2, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ; 
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation         ;
                                    ;
loadRightFrame3:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda rightFrame3, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame3, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation         ;
                                    ;
loadRightFrame4:                    ;
  ldx #$00                          ;
  ldy #$00                          ;
  @loop:                            ;
    lda rightFrame4, x              ;
    sta $0225, y                    ;
    iny                             ;
    inx                             ;
    lda rightFrame4, x              ;
    sta $0225, y                    ;
    inx                             ;
    iny                             ;
    iny                             ;
    iny                             ;
    cpx #$08                        ;
    bne @loop                       ;
  jmp endLoadRightAnimation         ;
;-----------------------------------;


; Fixes a problem that occurs when reversing the tiles
; vertically when making the character change direction.
; This problem arises since we must remember that the
; character is made of 4 tiles and what is inverted
; are those 4 tiles, not the complete character.
; So you have to reflect the tiles but also exchange positions.
;-----------------------------------;
checkAndFixMirroring:               ;
  lda FIX_MIRRORING                 ;
  cmp #$01                          ;
  bne endCheckAndFixMirroring       ;
    ldx $0227                       ;
    ldy $022B                       ;
                                    ;
    sty $0227                       ;
    sty $022F                       ;
    stx $022B                       ;
    stx $0233                       ;
                                    ;
    lda #$00                        ;
    sta FIX_MIRRORING               ;
endCheckAndFixMirroring:            ;
  rts                               ;
;-----------------------------------;              

;-----load left button------;
loadLeftSprite:	            ;
  ldx #$00                  ;
  @loop:                    ;
    lda leftSprite, x       ;
    sta $0200, x            ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;--load left button pressed---;
loadLeftPressedSprite:	      ;
  ldx #$00                    ;
  @loop:                      ;
    lda leftPressedSprite, x  ; 
    sta $0200 , x             ;
    inx                       ;
    cpx #$04                  ;
    bne @loop                 ;
    rts                       ;
;-----------------------------;

;-load right button-----------;
loadRightSprite:	            ;
  ldx #$00                    ;
  @loop:                      ;
    lda rightSprite, x        ;
    sta $0204, x              ;
    inx                       ;
    cpx #$04                  ;
    bne @loop                 ;
    rts                       ;
;-----------------------------;

;--load right button pressed------;
loadRightPressedSprite:	          ;
  ldx #$00                        ;
  @loop:                          ;
    lda rightPressedSprite, x     ;
    sta $0204 , x                 ;
    inx                           ;
    cpx #$04                      ;
    bne @loop                     ;
    rts                           ;
;---------------------------------;

;---load middle button------;
loadMiddleSprite:	          ;
  ldx #$00                  ;
  @loop:                    ;
    lda middleSprite, x     ; this button is not pressable
    sta $0208, x            ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;--load up button-----------;
loadUpSprite:	              ;
  ldx #$00                  ;
  @loop:                    ;
    lda upSprite, x         ;
    sta $020C , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;-load up button pressed----;
loadUpPressedSprite:	      ;
  ldx #$00                  ;
  @loop:                    ;
    lda upPressedSprite, x  ;
    sta $020C , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;-load down button----------;
loadDownSprite:	            ;
  ldx #$00                  ;
  @loop:                    ;
    lda downSprite, x       ;
    sta $0210 , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;-load down button pressed--;
loadDownPressedSprite:	    ;
  ldx #$00                  ;
  @loop:                    ;
    lda downPressedSprite, x;
    sta $0210 , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;--load select button-------;
loadSelectSprite:	          ;
  ldx #$00                  ;
  @loop:                    ;
    lda selctSprite, x      ;
    sta $0214 , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;--load select button pressed--;
loadSelectPressedSprite:	     ;
  ldx #$00                     ;
  @loop:                       ;
    lda selctPressedSprite, x  ;
    sta $0214 , x              ;
    inx                        ;
    cpx #$04                   ;
    bne @loop                  ;
    rts                        ;
;------------------------------;

;-load start button---------;
loadStartSprite:	          ;
  ldx #$00                  ;
  @loop:                    ;
    lda startSprite, x      ;
    sta $0218 , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;-load start button pressed----;
loadStartPressedSprite:	       ;
  ldx #$00                     ;
  @loop:                       ;
    lda startPressedSprite, x  ;
    sta $0218 , x              ;
    inx                        ;
    cpx #$04                   ;
    bne @loop                  ;
    rts                        ;
;------------------------------;

;--load A button------------;
loadASprite:	              ;
  ldx #$00                  ;
  @loop:                    ;
    lda ASprite, x          ;
    sta $021C , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;-load A button pressed-----;
loadAPressedSprite:	        ;
  ldx #$00                  ;
  @loop:                    ;
    lda APressedSprite, x   ;
    sta $021C , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;--load B button------------;
loadBSprite:	              ;
  ldx #$00                  ;
  @loop:                    ;
    lda BSprite, x          ;
    sta $0220 , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;

;-load B button pressed-----;
loadBPressedSprite:         ;
  ldx #$00                  ;
  @loop:                    ;
    lda BPressedSprite, x   ;
    sta $0220 , x           ;
    inx                     ;
    cpx #$04                ;
    bne @loop               ;
    rts                     ;
;---------------------------;



;--------------Binary Data------------------;

;third byte
; 76543210
; ||||||||
; ||||||++- Palette (4 to 7) of sprite
; |||+++--- Unimplemented (read 0)
; ||+------ Priority (0: in front of background; 1: behind background)
; |+------- Flip sprite horizontally
; +-------- Flip sprite vertically

initializationSprites:
  .byte INITIAL1_Y, $01, $40, INITIAL1_X + $08
  .byte INITIAL1_Y, $02, $40, INITIAL1_X
  .byte INITIAL1_Y + $08, $11, $40, INITIAL1_X + $08
  .byte INITIAL1_Y + $08, $12, $40, INITIAL1_X 


leftFrame1:
  .byte $07, $40 
  .byte $08, $40
  .byte $17, $40
  .byte $18, $40

leftFrame2:
  .byte $04, $40
  .byte $05, $40 
  .byte $14, $40
  .byte $15, $40

leftFrame3:
  .byte $0A, $40
  .byte $0B, $40
  .byte $1A, $40
  .byte $1B, $40

leftFrame4:
  .byte $04, $40
  .byte $05, $40
  .byte $14, $40
  .byte $15, $40




stillLeft:
  .byte $01, $40 
  .byte $02, $40
  .byte $11, $40 
  .byte $12, $40 

stillRight:
  .byte $01, $04 
  .byte $02, $04  
  .byte $11, $04 
  .byte $12, $04



rightFrame1:
  .byte $07, $04 
  .byte $08, $04
  .byte $17, $04
  .byte $18, $04

rightFrame2:
  .byte $04, $04
  .byte $05, $04
  .byte $14, $04
  .byte $15, $04

rightFrame3:
  .byte $0A, $04 
  .byte $0B, $04
  .byte $1A, $04
  .byte $1B, $04

rightFrame4:
  .byte $04, $04 
  .byte $05, $04
  .byte $14, $04
  .byte $15, $04


inAirRight:
  .byte $0A, $04 
  .byte $0B, $04
  .byte $1A, $04
  .byte $1B, $04

inAirLeft:
  .byte $0A, $40
  .byte $0B, $40
  .byte $1A, $40
  .byte $1B, $40


atackLeft:
  .byte $31, $40
  .byte $32, $40

atackRight:
  .byte $31, $04
  .byte $32, $04 



controlX = $16
controlY = $D0


leftSprite:       
  .byte controlY, $25, $05, controlX
middleSprite:  
  .byte controlY, $27, $05, controlX+8
upSprite: 
  .byte controlY-8, $28, $05, controlX+8
rightSprite:  
  .byte controlY, $25, $41, controlX+8*2
downSprite:  
  .byte controlY+8, $28, $81, controlX+8

selctSprite:  
  .byte controlY, $23, $05, controlX+8*4
startSprite:  
  .byte controlY, $23, $05, controlX+8*6

ASprite:  
  .byte controlY, $22, $05, controlX+8*8
BSprite:  
  .byte controlY, $22, $05, controlX+8*10


contolPressedX = $16
controlPressedY = $D0

leftPressedSprite:  
  .byte controlPressedY, $26, $05, contolPressedX
upPressedSprite: 
  .byte controlPressedY-8, $29, $05, contolPressedX+8
rightPressedSprite:  
  .byte controlPressedY, $26, $41, contolPressedX+8*2
downPressedSprite:  
  .byte controlPressedY+8, $29, $81, contolPressedX+8

selctPressedSprite:  
  .byte controlPressedY, $24, $05, contolPressedX+8*4
startPressedSprite:  
  .byte controlPressedY, $24, $05, contolPressedX+8*6

APressedSprite:  
  .byte controlPressedY, $21, $05, contolPressedX+8*8
BPressedSprite:  
  .byte controlPressedY, $21, $05, contolPressedX+8*10


backgroundPalettes1:
          ;00  01   10   11                   
  .byte $0f, $06, $16, $26; 00              
  .byte $0f, $00, $00, $00; 01   background     
  .byte $0f, $00, $00, $00; 02              
  .byte $0f, $00, $00, $00; 03  
                                            
defaultPalettes:                            
        ;00  01   10   11                   
  .byte $0f, $38, $17, $27; 00              
  .byte $0f, $30, $17, $27; 01   background     
  .byte $0f, $3c, $37, $09; 02              
  .byte $0f, $00, $00, $00; 03              
                                            
  .byte $0f, $38, $17, $27; 04              
  .byte $0f, $30, $17, $27; 05   foreground         
  .byte $0f, $3c, $37, $09; 06              
  .byte $0f, $3c, $37, $09; 07              

attributeTable0:
  ; 7654 3210
  ; |||| ||++- Color bits 3-2 for top left quadrant of this byte
  ; |||| ++--- Color bits 3-2 for top right quadrant of this byte
  ; ||++------ Color bits 3-2 for bottom left quadrant of this byte
  ; ++-------- Color bits 3-2 for bottom right quadrant of this byte

  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa 
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa 
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa 
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa  
  .byte $aa, $aa, $a2, $a0, $a0, $a8, $aa, $aa 
  .byte $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa  
  .byte $00, $00, $00, $00, $00, $00, $40, $10 
  .byte $00, $00, $00, $00, $00, $00, $00, $00 

background:
  .byte $15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$15,$15,$15,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$15,$15,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$15,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02

	.byte $15,$15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $15,$15,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$08,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$09,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02

	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$06,$07,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$05,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$0f,$0e,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$05,$01,$0c,$0d,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$05,$01,$0a,$0b,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
	.byte $02,$02,$05,$01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02

	.byte $03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04,$03,$04
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$14,$11,$13,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
	.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

 
collisionMap:
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00111111, %11111110, %00000001
  .byte %10000000, %00111111, %11111110, %00000001
  .byte %10000000, %00111111, %11111110, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %10000000, %00000000, %00000000, %00000001
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %11111111, %11111111
  .byte %11111111, %11111111, %11111111, %11111111

  
bitMaskTable:
  .byte %10000000
  .byte %01000000
  .byte %00100000
  .byte %00010000
  .byte %00001000
  .byte %00000100
  .byte %00000010
  .byte %00000001
                                            
background_tiles:                           
.segment "CHARS"                            
.incbin "Project2_background_sprites.chr"                         
;-------------------------------------------;
