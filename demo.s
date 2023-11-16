
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

  ;-----------;
  ldx #$0000  ;
  stx $2003   ;This is where the 64 sprites will be stored.
  ;-----------;
  
  jsr readController1
  jsr loadMiddleSprite
  jsr checkLeftButtonPressed
  jsr checkRightButtonPressed
  jsr checkUpButtonPressed 
  jsr checkDownButtonPressed
  jsr checkSelectButtonPressed 
  jsr checkStartButtonPressed
  jsr checkAButtonPressed 
  jsr checkBButtonPressed 
  jsr checkNotMovingPressed
  jsr checkAndFixMirroring
  jsr gravityEffect
  
  ; jsr loadPlayerSprites

  jsr calculatePlayerIndexs
  jsr checkCollision

  jsr restoreFromDamage

  

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
    jsr loadUpPressedSprite              ;  <--- if pressed
    jmp endCheckUpButtonPressed          ;
  @else:                                 ;
    jsr loadUpSprite                     ;  <--- if not
  endCheckUpButtonPressed:               ;
    rts                                  ;
;----------------------------------------;

;-----------check down button------------;
  checkDownButtonPressed:                ;
    lda INPUT1                              ;
    and #%00000100                       ;
    beq @else                            ;
    jsr loadDownPressedSprite            ;  <--- if pressed
    jmp endCheckDownButtonPressed        ;
  @else:                                 ;
    jsr loadDownSprite                   ;  <--- if not
  endCheckDownButtonPressed:             ;
    rts                                  ;
;----------------------------------------;

;-----------check select button----------;
  checkSelectButtonPressed:              ;
    lda INPUT1                              ;
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

    jsr takeDamage
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
      jsr loadStillFrame                 ;  <--- if not moving
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



; PLAYER1_INDEX_X = $0008
; PLAYER1_INDEX_Y = $0009

; 24 y
; 25
; 26
; 27 x

; 28 y
; 29
; 2A
; 2B x

; 2C y
; 2D
; 2E
; 2F x

; 30 y
; 31
; 32
; 33 x
calculatePlayerIndexs:
  lda $0227
  cmp $022B
  bmi @continue1
    lda $022B
  @continue1:
  sta NUM1
  lda #$8
  sta NUM2
  jsr division
  lda NUM1
  sta PLAYER1_INDEX_X


  lda $0224
  cmp $0228
  bmi @continue2
    lda $0228
  @continue2:
  sta NUM1
  lda #$8
  sta NUM2
  jsr division
  lda NUM1
  sta PLAYER1_INDEX_Y
  rts

checkCollision:
  lda PLAYER1_INDEX_Y
  asl
  asl
  asl
  asl
  asl
  clc
  adc PLAYER1_INDEX_X
  tax
  sta $0016
  lda background, x
  cmp #$02
  beq collision
    lda #$0
    sta $0015
    jmp endCheckCollision
  collision:
    lda #01
    sta $0015
  endCheckCollision:
  rts



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

;-------------------;
gravityEffect:      ;
                    ;
  clc               ;
  lda $0224         ;
  adc #GRAVITY      ;
  sta $0224         ;
                    ;
  clc               ;
  lda $0228         ;
  adc #GRAVITY      ;
  sta $0228         ;
  clc               ;
  lda $022C         ;
  adc #GRAVITY      ;
  sta $022C         ;
  clc               ;
  lda $0230         ;
  adc #GRAVITY      ;
  sta $0230         ;
                    ;
  rts               ; 
;-------------------;




; 24 y
; 25
; 26
; 27 x

; 28 y
; 29
; 2A
; 2B x

; 2C y
; 2D
; 2E
; 2F x

; 30 y
; 31
; 32
; 33 x
takeDamage:
  
    lda #$00
    ; sta $0224
    sta $0225
    sta $0226
    ; sta $0227

    ; sta $0228
    sta $0229
    sta $022A
    ; sta $022B

    ; sta $022C
    sta $022D
    sta $022E
    ; sta $022F

    ; sta $0230
    sta $0231
    sta $0232
    ; sta $0233
endTakeDamage:
  lda #$01
  sta takeDamage
  rts




restoreFromDamage:
  lda takeDamage
  cmp #$01
  beq @SUM
  jmp @continue

  @SUM:

    lda TIMER
    clc
    adc #$01
    cmp #$030
    beq @restore
    JMP endRestoreFromDamage

  @continue:
    LDA #$02
    STA $4014
    jmp endRestoreFromDamage

    @restore:
      lda #$00
      sta TIMER
      sta takeDamage

endRestoreFromDamage:
rts

;Moves to the left the 4 tiles saved in RAM by 1 pixels.
;---------------------;
moveToLeftPlayer1:    ;
  clc                 ;
  lda $0227           ;
  sbc #$0001          ;
  sta $0227           ;
                      ;
  clc                 ; 
  lda $022B           ;
  sbc #$0001          ;
  sta $022B           ;
                      ;
  clc                 ;
  lda $022F           ;
  sbc #$0001          ;
  sta $022F           ;
                      ;
  clc                 ;
  lda $0233           ;
  sbc #$0001          ;
  sta $0233           ;
                      ;
  rts                 ;
;---------------------;


;Moves to the right the 4 tiles saved in RAM by 2 pixels.
;---------------------;
moveToRightPlayer1:   ;
  clc                 ;
  lda $0227           ;
  adc #$0002          ;
  sta $0227           ;
                      ;
  clc                 ;
  lda $022B           ;
  adc #$0002          ;
  sta $022B           ;
                      ;
  clc                 ;
  lda $022F           ;
  adc #$0002          ;
  sta $022F           ;
                      ;
  clc                 ;
  lda $0233           ;
  adc #$0002          ;
  sta $0233           ;
                      ;
  rts                 ;
;---------------------;


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
  
                                            
background_tiles:                           
.segment "CHARS"                            
.incbin "Project2_background_sprites.chr"                         
;-------------------------------------------;
