
.include "../inclusionFiles/constants.inc"
.include "../inclusionFiles/header.inc"

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

;-------------Setup, occurs only once at the beginning of the program-------------;
ldx #PLAYER1_INFORMATION                                                          ;
ldy #PLAYER1_SPRITES                                                              ;
lda #$00                                                                          ;
sta PARAMETRO1                                                                    ;
lda #$10                                                                          ;
sta PARAMETRO2                                                                    ;
lda #$10                                                                          ;
sta PARAMETRO3                                                                    ;
jsr initializePlayer                                                              ;
                                                                                  ;
ldx #PLAYER2_INFORMATION                                                          ;
ldy #PLAYER2_SPRITES                                                              ;
lda #$01                                                                          ;
sta PARAMETRO1                                                                    ;
lda #$20                                                                          ;
sta PARAMETRO2                                                                    ;
lda #$20                                                                          ;
sta PARAMETRO3                                                                    ;
jsr initializePlayer                                                              ;
                                                                                  ;
jsr load_default_palettes                                                         ;
                                                                                  ;
jsr LoadAttr                                                                      ;
                                                                                  ;
jsr LoadBackground1                                                               ;
                                                                                  ;
                                                                                  ;
;-----------;                                                                     ;
  ldx #$0000;                                                                     ;
  stx $2003 ;This is where the 64 sprites will be stored.                         ;
;-----------;                                                                     ;
                                                                                  ;
                                                                                  ;
;PPUCTRL                                                                          ;
; 7  bit  0                                                                       ;
; ---- ----                                                                       ;
; VPHB SINN                                                                       ;
; |||| ||||                                                                       ;
; |||| ||++- Base nametable address                                               ;
; |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)                         ;
; |||| |+--- VRAM address increment per CPU read/write of PPUDATA                 ;
; |||| |     (0: add 1, going across; 1: add 32, going down)                      ;
; |||| +---- Sprite pattern table address for 8x8 sprites                         ;
; ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)                           ;
; |||+------ Background pattern table address (0: $0000; 1: $1000)                ;
; ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)     ;
; |+-------- PPU master/slave select                                              ;
; |          (0: read backdrop from EXT pins; 1: output color on EXT pins)        ;
; +--------- Generate an NMI at the start of the                                  ;
;            vertical blanking interval (0: off; 1: on)                           ;
                                                                                  ;
                                                                                  ;
; PPUMASK                                                                         ;
; 7  bit  0                                                                       ;
; ---- ----                                                                       ;
; BGRs bMmG                                                                       ;
; |||| ||||                                                                       ;
; |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)          ;
; |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide           ;
; |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide              ;
; |||| +---- 1: Show background                                                   ;
; |||+------ 1: Show sprites                                                      ;
; ||+------- Emphasize red (green on PAL/Dendy)                                   ;
; |+-------- Emphasize green (red on PAL/Dendy)                                   ;
; +--------- Emphasize blue                                                       ;
                                                                                  ;
;-----------------------;                                                         ;
enable_rendering:       ;                                                         ;
  lda #%10010000        ; Enable NMI                                              ;
  sta PPUCTRL           ; $2000                                                   ;
  lda #%00011010        ; Enable Sprites                                          ;
  sta PPUMASK           ; $2001                                                   ;
;-----------------------;                                                         ;
                                                                                  ;
;---------------------------------------------------------------------------------;

;---------------;                                                                 
forever:        ;                                                                 
                ;                                                                 
  jmp forever   ;                                                                 
;---------------;  


nmi:  
  ;*****************   Start of NMI   ***************************
  ;-----;
  PHA   ;                                
  TXA   ;                                                         
  PHA   ; Save the registers on the stack                                                    
  TYA   ;                                                         
  PHA   ;       
  ;-----;

  lda #PLAYER1_INFORMATION
  sta PARAMETRO1
  lda #PLAYER1_SPRITES
  sta PARAMETRO2
  jsr calculatePlayerCoordinates

  lda #PLAYER2_INFORMATION
  sta PARAMETRO1
  lda #PLAYER2_SPRITES
  sta PARAMETRO2
  jsr calculatePlayerCoordinates
  
  
  jsr readController1
  jsr checkLeftButtonPressed1
  jsr checkRightButtonPressed1
  jsr checkUpButtonPressed1
  jsr checkDownButtonPressed1
  jsr checkSelectButtonPressed1
  jsr checkStartButtonPressed1
  jsr checkBButtonPressed1
  
  jsr checkNotMovingPressed1

  jsr readController2
  jsr checkLeftButtonPressed2
  jsr checkRightButtonPressed2
  jsr checkUpButtonPressed2
  jsr checkDownButtonPressed2
  jsr checkSelectButtonPressed2
  jsr checkStartButtonPressed2
  jsr checkBButtonPressed2

  jsr checkNotMovingPressed2


  lda #PLAYER1_INFORMATION
  sta PARAMETRO1
  lda #PLAYER1_SPRITES
  sta PARAMETRO2  
  jsr checkAndFixMirroring   

  ldx PARAMETRO1
  lda $8, x ;JUMPING
  cmp #1
  beq @continue1
  jsr gravityEffect
  @continue1:
   
  jsr checkGrounded
  jsr jumpingManager
  jsr checkAButtonPressed1
  jsr blinkPlayer
  jsr projectileManager



  lda #PLAYER2_INFORMATION
  sta PARAMETRO1
  lda #PLAYER2_SPRITES
  sta PARAMETRO2
  jsr checkAndFixMirroring

  ldx PARAMETRO1
  lda $8, x ;JUMPING
  cmp #1
  beq @continue2
  jsr gravityEffect
  @continue2:

  jsr checkGrounded
  jsr jumpingManager
  jsr checkAButtonPressed2
  jsr blinkPlayer
  jsr projectileManager


  ldx #PLAYER2_INFORMATION
  lda $B, x ;PROJECTILE
  cmp #$1
  bne @con1

  lda #PLAYER1_INFORMATION
  sta PARAMETRO1
  lda #PLAYER2_SPRITES
  sta PARAMETRO2
  jsr ProjectileCollide

  @con1:


  ldx #PLAYER1_INFORMATION
  lda $B, x ;PROJECTILE
  cmp #$1
  bne @con2

  lda #PLAYER2_INFORMATION
  sta PARAMETRO1
  lda #PLAYER1_SPRITES
  sta PARAMETRO2
  jsr ProjectileCollide

  @con2:
  
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


.include "Inputs.s"

.include "worldSetup.s"

.include "ArithmeticSubroutines.s"

.include "Player.s"

.include "binaryData.s"
                                                                      
.segment "CHARS"                            
.incbin "Project2_background_sprites.chr"                         

