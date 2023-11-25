


;Parametro1 en el registro X: Direccion en la que se va a inicializar el jugador
;Parametro2 en el registro Y: Direccion low byte donde se va a guardar la informacion del sprite conrespecto a la $02XX
;Parametro3 en el registro PARAMETRO1: La paleta del jugador
;Parametro4 en el registro PARAMETRO2: Coordenada X
;Parametro5 en el registro PARAMETRO3: Coordenada Y
;-----------------------------------------------------------------------------------------------------;
initializePlayer:                                                                                     ;
    lda PARAMETRO2                                                                                    ;
    sta $0, X ;PLAYERX                                                                                ;
                                                                                                      ;
    lda PARAMETRO3                                                                                    ;
    sta $1, X ;PLAYERY                                                                                ;
                                                                                                      ;
    lda #$00                                                                                          ;
    sta $2, X ;INDEX_FRAME1                                                                           ;
    sta $3, X ;COUNTER_FRAME1                                                                         ;
    sta $4, X ;FIX_MIRRORING                                                                          ;
                                                                                                      ;
    lda#$6                                                                                            ;
    sta $5, X ;NMI_FRECUENCY_FRAME                                                                    ;
                                                                                                      ;
    lda #$1                                                                                           ;
    sta $6, X ; DIRECTION                                                                             ;
                                                                                                      ;
    lda #$0                                                                                           ;
    sta $7, X ; PLAYER_COLLIDE                                                                        ;
    sta $8, X ; JUMPING                                                                               ;
    sta $9, X ; JUMPING_COUNTER                                                                       ;
    sta $A, X ; GOUNDED                                                                               ;
    sta $B, X ; PROJECTILE                                                                            ;
    sta $C, X ; PROJECTILE_COUNTER                                                                    ;
    sta $D, X ; PROJECTILE_DIRECTION                                                                  ;
    sta $E, X ; TAKE_DAMAGE                                                                           ;
    sta $F, X ; BLINK_PLAYER_COUNTER                                                                  ;
    sta $10, X ; BLINK_PLAYER_TIMER 
    
    lda #$3                                                                 
    sta $11, X ; Life points                                                                   
                                                                                                      ;
    ;#$40 para hacer el mirror del tile y el or para encender los bits de la paleta del parametro     ;
    lda #$40                                                                                          ;
    ora PARAMETRO1                                                                                    ;
    tax                                                                                               ;
                                                                                                      ;
    ;Sprite1                                                                                          ;
    lda PARAMETRO3        ;Y                                                                          ;
    sta $0200, Y                                                                                      ;
    lda #$01              ;Tile index                                                                 ;
    sta $0200+$1, Y                                                                                   ;
    txa                   ;Attribute                                                                  ;
    sta $0200+$2, Y                                                                                   ;
    lda PARAMETRO2        ;X                                                                          ;
    clc                                                                                               ;
    adc #$8                                                                                           ;
    sta $0200+$3, Y                                                                                   ;
                                                                                                      ;
    ;Sprite2                                                                                          ;
    lda PARAMETRO3        ;Y                                                                          ;
    sta $0200+$4, Y                                                                                   ;
    lda #$02              ;Tile index                                                                 ;
    sta $0200+$5, Y                                                                                   ;
    txa                   ;Attribute                                                                  ;
    sta $0200+$6, Y                                                                                   ;
    lda PARAMETRO2        ;X                                                                          ;
    sta $0200+$7, Y                                                                                   ;
                                                                                                      ;
    ;Sprite3                                                                                          ;
    lda PARAMETRO3        ;Y                                                                          ;
    clc                                                                                               ;
    adc #$8                                                                                           ;
    sta $0200+$8, Y                                                                                   ;
    lda #$11             ;Tile index                                                                  ;
    sta $0200+$9, Y                                                                                   ;
    txa                  ;Attribute                                                                   ;
    sta $0200+$A, Y                                                                                   ;
    lda PARAMETRO2       ;X                                                                           ;
    clc                                                                                               ;
    adc #$8                                                                                           ;
    sta $0200+$B, Y                                                                                   ;
                                                                                                      ;
    ;Sprite4                                                                                          ;
    lda PARAMETRO3      ;Y                                                                            ;
    clc                                                                                               ;
    adc #$8                                                                                           ;
    sta $0200+$C, Y                                                                                   ;
    lda #$12            ;Tile index                                                                   ;
    sta $0200+$D, Y                                                                                   ;
    txa                 ;Attribute                                                                    ;
    sta $0200+$E, Y                                                                                   ;
    lda PARAMETRO2      ;X                                                                            ;
    sta $0200+$F, Y                                                                                   ;
rts                                                                                                   ;
;-----------------------------------------------------------------------------------------------------;




; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;-----------------------------------------------;
initializeProjectile:                           ;
    ldx PARAMETRO1                              ;
    ldy PARAMETRO2                              ;
                                                ;
    lda $B, x ;PROJECTILE                       ;
    cmp #1                                      ;
    beq endInitializeProjectile                 ;
                                                ;
        lda #$1                                 ;
        sta $B, x ;PROJECTILE                   ;
                                                ;
        lda $6, x ;DIRECTION                    ;
        sta $D, x ;PROJECTILE_DIRECTION         ;
                                                ;
        lda $1, x ;PLAYER_Y                     ;
        sta $0210, y ; Y                        ;
                                                ;
        lda #$33                                ;
        sta $0211, y ; tile                     ;
                                                ;
        lda #$04                                ;
        sta $0212, y ; attr                     ;
                                                ;
        lda $D, x ;PROJECTILE_DIRECTION ; x     ;
        cmp #$0                                 ;
        beq @continue                           ;
        lda $0, x ;PLAYER1_X                    ;
        clc                                     ;
        sbc #$10                                ;
        sta $0213, y                            ;
        jmp endInitializeProjectile             ;
        @continue:                              ;
        lda $0, x ;PLAYER1_X                    ;
        clc                                     ;
        adc #$8                                 ;
        sta $0213, y                            ;
                                                ;
endInitializeProjectile:                        ;
rts                                             ;
;-----------------------------------------------;


; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;---------------------------------------;
destroyProjectile:                      ;
    ldx PARAMETRO1                      ;
    ldy PARAMETRO2                      ;
                                        ;
    lda #$0                             ;
    sta $B, x ;PROJECTILE               ;
    sta $C, x ;PROJECTILE_COUNTER       ;
    sta $D, x; PROJECTILE_DIRECTION     ;
                                        ;
    lda #$0                             ;
    sta $0210, y                        ;
                                        ;
    lda #$0                             ;
    sta $0211, y                        ;
                                        ;
    lda #$04                            ;
    sta $0212, y                        ;
                                        ;
    lda #$0                             ;
    sta $0213, y                        ;
                                        ;
endDestroyProjectile:                   ;
rts                                     ;
;---------------------------------------;





; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
PROJECTILE_TIME = $0020
;---------------------------------------------;
projectileManager:                            ;
    ldx PARAMETRO1                            ;
                                              ;
    lda $B, x ;PROJECTILE                     ;
    cmp #0                                    ;
    beq @continue1                            ;
        inc $C, x ;PROJECTILE_COUNTER         ;
        lda $C, x ;PROJECTILE_COUNTER         ;
        cmp #PROJECTILE_TIME                  ;
        bne @continue2                        ;
        jsr destroyProjectile                 ;
        ; jmp endProjectileManager ; TEMP     ;
        @continue2:                           ;
        lda $D, x ;PROJECTILE_DIRECTION       ;
        ldx PARAMETRO2                        ;
        cmp #$0                               ;
        beq @continue3                        ;
        dec $0213, x                          ;
        dec $0213, x                          ;
        dec $0213, x                          ;
        jmp @continue1                        ;
        @continue3:                           ;
        inc $0213, x                          ;
        inc $0213, x                          ;
        inc $0213, x                          ;
                                              ;
    @continue1:                               ;
                                              ;
endProjectileManager:                         ;
rts                                           ;
;---------------------------------------------;




; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;-------------------------------;
calculatePlayerCoordinates:     ;
    ldx PARAMETRO2              ;
    ldy PARAMETRO1              ;
                                ;
    lda $0200+$3, x             ;
    cmp $0200+$7, x             ;
    bpl @continue               ;
        lda $0200+$7, x         ;
    @continue:                  ;
    sta $0, y                   ;
                                ;
    lda $0200, x                ;
    sta $1, y                   ;
    rts                         ;
;-------------------------------;



; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;----------------------------;
gravityEffect:               ;
    jsr moveToDownPlayer     ;
    jsr moveToDownPlayer     ;
rts                          ;
;----------------------------;




; Moves to the left the 4 tiles saved in RAM by 1 pixels.
; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;------------------------------------------;
moveToLeftPlayer:                          ;
                                           ;
    ldx PARAMETRO2                         ;
    dec $0203, x                           ;
    dec $0207, x                           ;
    dec $020B, x                           ;
    dec $020F, x                           ;
                                           ;
    jsr calculatePlayerCoordinates         ;
                                           ;
    jsr setCoordinatesTopLeft              ;
    jsr checkCollide                       ;
    beq @continue1                         ;
        ldx PARAMETRO2                     ;
        inc $0203, x                       ;
        inc $0207, x                       ;
        inc $020B, x                       ;
        inc $020F, x                       ;
        jsr calculatePlayerCoordinates     ;
    @continue1:                            ;
                                           ;
    jsr setCoordinatesBottomLeft           ;
    jsr checkCollide                       ;
    beq @continue2                         ;
        ldx PARAMETRO2                     ;
        inc $0203, x                       ;
        inc $0207, x                       ;
        inc $020B, x                       ;
        inc $020F, x                       ;
        jsr calculatePlayerCoordinates     ;
    @continue2:                            ;
                                           ;
endMoveToLeftPlayer1:                      ;
rts                                        ;
;------------------------------------------;





; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;------------------------------------------;
moveToRightPlayer:                         ;
                                           ;
    ldx PARAMETRO2                         ;
    inc $0203, x                           ;
    inc $0207, x                           ;
    inc $020B, x                           ;
    inc $020F, x                           ;
    jsr calculatePlayerCoordinates         ;
                                           ;
    jsr setCoordinatesTopLeft              ;
    jsr checkCollide                       ;
    beq @continue1                         ;
        ldx PARAMETRO2                     ;
        dec $0203, x                       ;
        dec $0207, x                       ;
        dec $020B, x                       ;
        dec $020F, x                       ;
        jsr calculatePlayerCoordinates     ;
    @continue1:                            ;
                                           ;
    jsr setCoordinatesBottomLeft           ;
    jsr checkCollide                       ;
    beq @continue2                         ;
        ldx PARAMETRO2                     ;
        dec $0203, x                       ;
        dec $0207, x                       ;
        dec $020B, x                       ;
        dec $020F, x                       ;
        jsr calculatePlayerCoordinates     ;
    @continue2:                            ;
                                           ;
endMoveToRightPlayer:                      ;
rts                                        ;
;------------------------------------------;







; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;------------------------------------------;
moveToDownPlayer:                          ;
                                           ;
    ldx PARAMETRO2                         ;
    inc $0200, x                           ;
    inc $0204, x                           ;
    inc $0208, x                           ;
    inc $020C, x                           ;
    jsr calculatePlayerCoordinates         ;
                                           ;
    jsr setCoordinatesTopLeft              ;
    jsr checkCollide                       ;
    beq @continue1                         ;
        ldx PARAMETRO2                     ;
        dec $0200, x                       ;
        dec $0204, x                       ;
        dec $0208, x                       ;
        dec $020C, x                       ;
        jsr calculatePlayerCoordinates     ;
    @continue1:                            ;
                                           ;
    jsr setCoordinatesBottomLeft           ;
    jsr checkCollide                       ;
    beq @continue2                         ;
        ldx PARAMETRO2                     ;
        dec $0200, x                       ;
        dec $0204, x                       ;
        dec $0208, x                       ;
        dec $020C, x                       ;
        jsr calculatePlayerCoordinates     ;
    @continue2:                            ;
                                           ;
endMoveToDownPlayer:                       ;
rts                                        ;
;------------------------------------------;






; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;------------------------------------------;
moveToUpPlayer:                            ;
                                           ;
    ldx PARAMETRO2                         ;
    dec $0200, x                           ;
    dec $0204, x                           ;
    dec $0208, x                           ;
    dec $020C, x                           ;
    jsr calculatePlayerCoordinates         ;
                                           ;
    jsr setCoordinatesTopLeft              ;
    jsr checkCollide                       ;
    beq @continue1                         ;
        ldx PARAMETRO2                     ;
        inc $0200, x                       ;
        inc $0204, x                       ;
        inc $0208, x                       ;
        inc $020C, x                       ;
        jsr calculatePlayerCoordinates     ;
    @continue1:                            ;
                                           ;
    jsr setCoordinatesBottomLeft           ;
    jsr checkCollide                       ;
    beq @continue2                         ;
        ldx PARAMETRO2                     ;
        inc $0200, x                       ;
        inc $0204, x                       ;
        inc $0208, x                       ;
        inc $020C, x                       ;
        jsr calculatePlayerCoordinates     ;
    @continue2:                            ;
                                           ;
endMoveToUpPlayer:                         ;
rts                                        ;
;------------------------------------------;



; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
JUNPING_TIME = $0020
;--------------------------------------;
jumpingManager:                        ;
    ldx PARAMETRO1                     ;
    ldy PARAMETRO2                     ;
                                       ;
    lda $8, x ;JUMPING                 ;
    cmp #$0                            ;
    beq @continue                      ;
        jsr moveToUpPlayer             ;
        jsr moveToUpPlayer             ;
        jsr moveToUpPlayer             ;
        ldx PARAMETRO1                 ;
        inc $9, x ;JUMPING_COUNTER     ;
        lda $9, x ;JUMPING_COUNTER     ;
        cmp #JUNPING_TIME              ;
        bne @continue                  ;
        lda #$0                        ;
        sta $8, x ;JUMPING             ;
        sta $9, x ;JUMPING_COUNTER     ;
    @continue:                         ;
                                       ;
                                       ;
endJumpingManager:                     ;
rts                                    ;
;--------------------------------------;


; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.

;--------------------------------------;
checkGrounded:                         ;
    ldx PARAMETRO2                     ;
    ldy PARAMETRO1                     ;
                                       ;
    inc $0200, x                       ;
    inc $0204, X                       ;
    inc $0208, X                       ;
    inc $020C, X                       ;
    jsr calculatePlayerCoordinates     ;
                                       ;
                                       ;
    lda #$00                           ;
    sta $A, y ;GROUNDED                ;
                                       ;
    jsr setCoordinatesBottomLeft       ;
    jsr checkCollide                   ;
    beq @continue1                     ;
        ldy PARAMETRO1                 ;
        lda #$01                       ;
        sta $A, Y ;GROUNDED            ;
    @continue1:                        ;
                                       ;
    jsr setCoordinatesBottomRight      ;
    jsr checkCollide                   ;
    beq @continue2                     ;
        ldy PARAMETRO1                 ;
        lda #$01                       ;
        sta $A, Y ;GROUNDED            ;
    @continue2:                        ;
                                       ;
    ldx PARAMETRO2                     ;
    dec $0200, x                       ;
    dec $0204, X                       ;
    dec $0208, X                       ;
    dec $020C, X                       ;
    jsr calculatePlayerCoordinates     ;
                                       ;
                                       ;
    lda $A, y ;GROUNDED                ;
    cmp #$0                            ;
    bne endcheckGrounded               ;
    jsr loadinAirFrame                 ;
                                       ;
endcheckGrounded:                      ;
rts                                    ;
;--------------------------------------;
               


; Parametro1 en el registro x: coordenada en x del jugador.
; Parametro2 en el registro y: coordenada en y del jugador.
; return: modifica la vandera de estado Z. Si es 0 significa que no hubo colision. Si es igual a 1 si hubo colision.
;---------------------------;
checkCollide:               ;
  txa      ; x/64           ;
  lsr                       ;
  lsr                       ;
  lsr                       ;
  lsr                       ;
  lsr                       ; x/64 + (y/8*4) byte index in collisionMap
  lsr                       ; x/8 and %0111
  sta TEMP                  ;
  TYA       ;(y/8)          ;
  lsr                       ;
  lsr                       ;
  lsr                       ;
  asl                       ;
  asl        ;*4            ;
  clc                       ;
  adc TEMP                  ;
  TAY       ;byte index     ;
                            ;
  txa                       ;
  lsr                       ;
  lsr                       ;
  lsr                       ;
  and #%0111                ;
  tax                       ;
                            ;
  lda collisionMap, Y       ;
  and bitMaskTable, x       ;
  rts                       ;
;---------------------------;

;Parametro1 en registro x: coordenada x de una entidad
;Parametro2 en registro y: coordenada y de una entidad
tileIndex:
    txa
    lsr
    lsr
    lsr
    sta TEMP
    tya 
    lsr
    lsr
    lsr
    asl
    asl
    asl
    asl
    asl
    clc
    adc TEMP
rts

; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador al que atacaron.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador que disparo el proyectil.
ProjectileCollide:
    ; lda #$0
    ; sta $00c8

    ldx PARAMETRO1
    lda $0, x ;Player x
    sta TEMP
    lda $1, x ; Player y
    tay
    ldx TEMP
    jsr tileIndex
    sta RETURN_VALUE1

    ldx PARAMETRO2
    lda $0213, x ;Projectile x
    sta TEMP
    lda $0210, x ;Projectile y
    tay
    ldx TEMP
    jsr tileIndex
    sta RETURN_VALUE2

    cmp RETURN_VALUE1
    bne @continue1
        ldx PARAMETRO1
        lda #$1
        sta $E, x ;take damage

    @continue1:


endProjectileCollide:
    rts



; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
;-----------------------------;
setCoordinatesBottomLeft:     ;
    ldy PARAMETRO1            ;
    ldx $0, y                 ;
    stx TEMP                  ;
    ldx PARAMETRO1            ;
    ldy $1, x                 ;
    ldx TEMP                  ;
                              ;
    tya                       ;
    clc                       ;
    adc #$A                   ;
    tay                       ;
                              ;
  rts                         ;
;-----------------------------;




; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
;------------------------------;
setCoordinatesBottomRight:     ;
    ldy PARAMETRO1             ;
    ldx $0, Y                  ;
    stx TEMP                   ;
    ldx PARAMETRO1             ;
    ldy $1, X                  ;
    ldx TEMP                   ;
                               ;
    tya                        ;
    clc                        ;
    adc #$A                    ;
    tay                        ;
                               ;
    txa                        ;
    clc                        ;
    adc #$7                    ;
    tax                        ;
                               ;
rts                            ;
;------------------------------;



; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
;--------------------------;
setCoordinatesTopLeft:     ;
    ldy PARAMETRO1         ;
    ldx $0, Y              ;
    stx TEMP               ;
    ldx PARAMETRO1         ;
    ldy $1, X              ;
    ldx TEMP               ;
                           ;
    tya                    ;
    clc                    ;
    adc #$3                ;
    tay                    ;
                           ;
rts                        ;
;--------------------------;



; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
;---------------------------;
setCoordinatesTopRight:     ;
    ldy PARAMETRO1          ;
    ldx $0, Y               ;
    stx TEMP                ;
    ldx PARAMETRO1          ;
    ldy $1, X               ;
    ldx TEMP                ;
                            ;
    tya                     ;
    clc                     ;
    adc #$3                 ;
    tay                     ;
                            ;
    txa                     ;
    clc                     ;
    adc #$7                 ;
    tax                     ;
                            ;
rts                         ;
;---------------------------;





; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;-------------------------------------------------;
blinkPlayer:                                      ;
    ldx PARAMETRO1                                ;
    ldy PARAMETRO2                                ;
                                                  ;
    lda $E, x ;TAKE_DAMEGE                        ;
    cmp #$0                                       ;
    beq endBlinkPlayer                            ;
                                                  ;
    inc $F, x ;BLINK_PLAYER_COUNTER               ;
    inc $10, x ;BLINK_PLAYER_TIMER                ;
    lda $F, x ;BLINK_PLAYER_COUNTER               ;
    cmp #$8 ; tiempo estando invisible            ;
    bmi @invisible                                ;
        cmp #$10 ;tiempo no estando invisible     ;
        beq @reset                                ;
        jmp @continue                             ;
        @reset:                                   ;
        lda #$0                                   ;
        sta $F, x ;BLINK_PLAYER_COUNTER           ;
        jmp @continue                             ;
    @invisible:                                   ;
    lda #$0                                       ;
    sta $0201, y                                  ;
    sta $0205, y                                  ;
    sta $0209, y                                  ;
    sta $020D, y                                  ;
    @continue:                                    ;
                                                  ;
    lda $10, X ;BLINK_PLAYER_TIMER                ;
    cmp #$50  ;tiempo de la animacion             ;
    bne endBlinkPlayer                            ;
        lda #$0                                   ;
        sta $E, x ;TAKE_DAMEGE                    ;
        sta $F, x ;BLINK_PLAYER_COUNTER           ;
        sta $10, x ;BLINK_PLAYER_TIMER            ;
        dec $11, x ;life points
                                                  ;
endBlinkPlayer:                                   ;
rts                                               ;
;-------------------------------------------------;





; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;------------------------------------------------;
                                                 ;
loadAtack:                                       ;
    ldx PARAMETRO1                               ;
    ldy PARAMETRO2                               ;
                                                 ;
    lda $6, x ;DIRECTION1                        ;
    and #%00000001                               ;
    bne @else                                    ;
        jsr loadRightAtack                       ;
        jmp endLoadAtack                         ;
    @else:                                       ;
        jsr loadLeftAtack                        ;
endLoadAtack:                                    ;
  rts                                            ;
                                                 ;
loadLeftAtack:                                   ;
    lda #$31                                     ;
    sta $0201, y ; tile                          ;
    lda $0202, y                                 ;
    ora #%01000000                               ;
    sta $0202, x ; attr                          ;
                                                 ;
    lda #$32                                     ;
    sta $0205, y ; tile                          ;
    lda $0206, y                                 ;
    ora #%01000000                               ;
    sta $0206, y ; attr                          ;
    rts                                          ;
                                                 ;
loadRightAtack:                                  ;
    lda #$31                                     ;
    sta $0201, y ; tile                          ;
    lda $0202, y                                 ;
    and #%10111111                               ;
    sta $0202, y ; attr                          ;
                                                 ;
    lda #$32                                     ;
    sta $0205, y ; tile                          ;
    lda $0206, y                                 ;
    and #%10111111                               ;
    sta $0206, y ; attr                          ;
    rts                                          ;
;------------------------------------------------; 



; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;------------------------------------------------;
loadinAirFrame:                                  ;
    ldx PARAMETRO1                               ;
    ldy PARAMETRO2                               ;
                                                 ;
    lda $6, x ;DIRECTION1                        ;
    and #%00000001                               ;
    bne @else                                    ;
        jsr loadinAirRight                       ;
        jmp endLoadinAirFrame                    ;
    @else:                                       ;
        jsr loadinAirLeft                        ;
endLoadinAirFrame:                               ;
     rts                                         ;
                                                 ;
loadinAirLeft:                                   ;
    lda #$0A                                     ;
    sta $0201, y ; tile                          ;
    lda $0202, y                                 ;
    ora #%01000000                               ;
    sta $0202, x ; attr                          ;
                                                 ;
    lda #$0B                                     ;
    sta $0205, y ; tile                          ;
    lda $0206, y                                 ;
    ora #%01000000                               ;
    sta $0206, y ; attr                          ;
                                                 ;
    lda #$1A                                     ;
    sta $0209, y ; tile                          ;
    lda $020A, y                                 ;
    ora #%01000000                               ;
    sta $020A, y ; attr                          ;
                                                 ;
    lda #$1B                                     ;
    sta $020D, y ; tile                          ;
    lda $020E, y                                 ;
    ora #%01000000                               ;
    sta $020E, y ; attr                          ;
    rts                                          ;
                                                 ;
loadinAirRight:                                  ;
    lda #$0A                                     ;
    sta $0201, y ; tile                          ;
    lda $0202, y                                 ;
    and #%10111111                               ;
    sta $0202, y ; attr                          ;
                                                 ;
    lda #$0B                                     ;
    sta $0205, y ; tile                          ;
    lda $0206, y                                 ;
    and #%10111111                               ;
    sta $0206, y ; attr                          ;
                                                 ;
    lda #$1A                                     ;
    sta $0209, y ; tile                          ;
    lda $020A, y                                 ;
    and #%10111111                               ;
    sta $020A, y ; attr                          ;
                                                 ;
    lda #$1B                                     ;
    sta $020D, y ; tile                          ;
    lda $020E, y                                 ;
    and #%10111111                               ;
    sta $020E, y ; attr                          ;
rts                                              ;
;------------------------------------------------;             



; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
;------------------------------------------------;
loadStillFrame:                                  ;
    ldx PARAMETRO1                               ;
    ldy PARAMETRO2                               ;
                                                 ;
    lda $6, X ;DIRECTION1                        ;
    and #%00000001                               ;
    bne @else                                    ;
        jsr loadStillRight                       ;
        jmp endLoadStillFrame                    ;
    @else:                                       ;
        jsr loadStillLeft                        ;
endLoadStillFrame:                               ;
    rts                                          ;
                                                 ;
loadStillLeft:                                   ;
    lda #$01                                     ;
    sta $0201, y ; tile                          ;
    lda $0202, y                                 ;
    ora #%01000000                               ;
    sta $0202, x ; attr                          ;
                                                 ;
    lda #$02                                     ;
    sta $0205, y ; tile                          ;
    lda $0206, y                                 ;
    ora #%01000000                               ;
    sta $0206, y ; attr                          ;
                                                 ;
    lda #$11                                     ;
    sta $0209, y ; tile                          ;
    lda $020A, y                                 ;
    ora #%01000000                               ;
    sta $020A, y ; attr                          ;
                                                 ;
    lda #$12                                     ;
    sta $020D, y ; tile                          ;
    lda $020E, y                                 ;
    ora #%01000000                               ;
    sta $020E, y ; attr                          ;
rts                                              ;
                                                 ;
loadStillRight:                                  ;
    lda #$01                                     ;
    sta $0201, y ; tile                          ;
    lda $0202, y                                 ;
    and #%10111111                               ;
    sta $0202, y ; attr                          ;
                                                 ;
    lda #$02                                     ;
    sta $0205, y ; tile                          ;
    lda $0206, y                                 ;
    and #%10111111                               ;
    sta $0206, y ; attr                          ;
                                                 ;
    lda #$11                                     ;
    sta $0209, y ; tile                          ;
    lda $020A, y                                 ;
    and #%10111111                               ;
    sta $020A, y ; attr                          ;
                                                 ;
    lda #$12                                     ;
    sta $020D, y ; tile                          ;
    lda $020E, y                                 ;
    and #%10111111                               ;
    sta $020E, y ; attr                          ;
rts                                              ;
;------------------------------------------------;      



   
; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
; Load the animation of walking to the left
;---------------------------------------------------;
loadLeftAnimation:                                  ;
                                                    ;
    ldx PARAMETRO1                                  ;
    lda $6, x ; DIRECTION                           ;
    ; lda DIRECTION1                                ;
    cmp #$00                                        ;
    bne @continue                                   ;
    lda #$01                                        ;
    sta $4, x ;FIX_MIRRORING                        ;
    ; sta FIX_MIRRORING                             ;
    @continue:                                      ;
                                                    ;
    lda #$01                                        ;
    sta $6, x ;DIRECTION1                           ;
                                                    ;
    lda $2, x ;INDEX_FRAME1                         ;
                                                    ;
                                                    ;
    ldx PARAMETRO2                                  ;
    cmp #$00                                        ;
    beq loadLeftFrame1                              ;
    jmp LEFTnext1                                   ;
                                                    ;
endLoadLeftAnimation:                               ;
    ldx PARAMETRO1                                  ;
    clc                                             ;
    lda $3, x ;COUNTER_FRAME1                       ;
    adc #$01                                        ;
    sta $3, x ;COUNTER_FRAME1                       ;
    cmp $5, x ;NMI_FRECUENCY_FRAME1                 ;
    bne @else1                                      ;
        lda #$00                                    ;
        sta $3, x ;COUNTER_FRAME1                   ;
                                                    ;
        lda $2, x ;INDEX_FRAME1                     ;
        clc                                         ;
        adc #$01                                    ;
        cmp #$04                                    ;
        bne @else2                                  ;
        lda #$00                                    ;
        @else2:                                     ;
        sta $2, x ;INDEX_FRAME1                     ;
    @else1:                                         ;
        rts                                         ;
                                                    ;
                                                    ;
loadLeftFrame1:                                     ;
    lda #$07                                        ;
    sta $0201, X ; tile                             ;
    lda $0202, X                                    ;
    ora #%01000000                                  ;
    sta $0202, x ; attr                             ;
                                                    ;
    lda #$08                                        ;
    sta $0205, X ; tile                             ;
    lda $0206, X                                    ;
    ora #%01000000                                  ;
    sta $0206, x ; attr                             ;
                                                    ;
    lda #$17                                        ;
    sta $0209, X ; tile                             ;
    lda $020A, X                                    ;
    ora #%01000000                                  ;
    sta $020A, x ; attr                             ;
                                                    ;
    lda #$18                                        ;
    sta $020D, X ; tile                             ;
    lda $020E, X                                    ;
    ora #%01000000                                  ;
    sta $020E, x ; attr                             ;
                                                    ;
    jmp endLoadLeftAnimation                        ;
                                                    ;
LEFTnext1:                                          ;
    cmp #$01                                        ;
    beq loadLeftFrame2                              ;
    jmp LEFTnext2                                   ;
                                                    ;
                                                    ;
loadLeftFrame2:                                     ;
    lda #$04                                        ;
    sta $0201, X ; tile                             ;
    lda $0202, X                                    ;
    ora #%01000000                                  ;
    sta $0202, x ; attr                             ;
                                                    ;
    lda #$05                                        ;
    sta $0205, X ; tile                             ;
    lda $0206, X                                    ;
    ora #%01000000                                  ;
    sta $0206, x ; attr                             ;
                                                    ;
    lda #$14                                        ;
    sta $0209, X ; tile                             ;
    lda $020A, X                                    ;
    ora #%01000000                                  ;
    sta $020A, x ; attr                             ;
                                                    ;
    lda #$15                                        ;
    sta $020D, X ; tile                             ;
    lda $020E, X                                    ;
    ora #%01000000                                  ;
    sta $020E, x ; attr                             ;
    jmp endLoadLeftAnimation                        ;
                                                    ;
LEFTnext2:                                          ;
    cmp #$02                                        ;
    beq loadLeftFrame3                              ;
    jmp LEFTnext3                                   ;
                                                    ;
loadLeftFrame3:                                     ;
    lda #$0A                                        ;
    sta $0201, X ; tile                             ;
    lda $0202, X                                    ;
    ora #%01000000                                  ;
    sta $0202, x ; attr                             ;
                                                    ;
    lda #$0B                                        ;
    sta $0205, X ; tile                             ;
    lda $0206, X                                    ;
    ora #%01000000                                  ;
    sta $0206, x ; attr                             ;
                                                    ;
    lda #$1A                                        ;
    sta $0209, X ; tile                             ;
    lda $020A, X                                    ;
    ora #%01000000                                  ;
    sta $020A, x ; attr                             ;
                                                    ;
    lda #$1B                                        ;
    sta $020D, X ; tile                             ;
    lda $020E, X                                    ;
    ora #%01000000                                  ;
    sta $020E, x ; attr                             ;
    jmp endLoadLeftAnimation                        ;
                                                    ;
                                                    ;
LEFTnext3:                                          ;
    cmp #$03                                        ;
    beq loadLeftFrame4                              ;
    jmp endLoadLeftAnimation                        ;
                                                    ;
loadLeftFrame4:                                     ;
    lda #$04                                        ;
    sta $0201, X ; tile                             ;
    lda $0202, X                                    ;
    ora #%01000000                                  ;
    sta $0202, x ; attr                             ;
                                                    ;
    lda #$05                                        ;
    sta $0205, X ; tile                             ;
    lda $0206, X                                    ;
    ora #%01000000                                  ;
    sta $0206, x ; attr                             ;
                                                    ;
    lda #$14                                        ;
    sta $0209, X ; tile                             ;
    lda $020A, X                                    ;
    ora #%01000000                                  ;
    sta $020A, x ; attr                             ;
                                                    ;
    lda #$15                                        ;
    sta $020D, X ; tile                             ;
    lda $020E, X                                    ;
    ora #%01000000                                  ;
    sta $020E, x ; attr                             ;
    jmp endLoadLeftAnimation                        ;
;---------------------------------------------------;      






; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.
; Load the animation of walking to the left

;---------------------------------------------------;
                                                    ;
loadRightAnimation:                                 ;
  ; lda PLAYER_STATUS1                              ;
  ; ora %00000001                                   ;
  ; sta PLAYER_STATUS1                              ;
                                                    ;
    ldx PARAMETRO1                                  ;
    lda $6, x ; DIRECTION                           ;
    ; lda DIRECTION1                                ;
    cmp #$01                                        ;
    bne @continue                                   ;
    lda #$01                                        ;
    sta $4, x ;FIX_MIRRORING                        ;
    ; sta FIX_MIRRORING                             ;
    @continue:                                      ;
                                                    ;
    lda #$00                                        ;
    sta $6, x ;DIRECTION1                           ;
                                                    ;
    lda $2, x ;INDEX_FRAME1                         ;
                                                    ;
                                                    ;
    ldx PARAMETRO2                                  ;
    cmp #$00                                        ;
    beq loadRightFrame1                             ;
    jmp RIGHTnext1                                  ;
                                                    ;
endLoadRightAnimation:                              ;
    ldx PARAMETRO1                                  ;
    clc                                             ;
    lda $3, x ;COUNTER_FRAME1                       ;
    adc #$01                                        ;
    sta $3, x ;COUNTER_FRAME1                       ;
    cmp $5, x ;NMI_FRECUENCY_FRAME1                 ;
    bne @else1                                      ;
        lda #$00                                    ;
        sta $3, x ;COUNTER_FRAME1                   ;
                                                    ;
        lda $2, x ;INDEX_FRAME1                     ;
        clc                                         ;
        adc #$01                                    ;
        cmp #$04                                    ;
        bne @else2                                  ;
        lda #$00                                    ;
        @else2:                                     ;
        sta $2, x ;INDEX_FRAME1                     ;
    @else1:                                         ;
        rts                                         ;
                                                    ;
                                                    ;
loadRightFrame1:                                    ;
    lda #$07                                        ;
    sta $0201, X ; tile                             ;
    lda $0202, X                                    ;
    and #%10111111                                  ;
    sta $0202, x ; attr                             ;
                                                    ;
    lda #$08                                        ;
    sta $0205, X ; tile                             ;
    lda $0206, X                                    ;
    and #%10111111                                  ;
    sta $0206, x ; attr                             ;
                                                    ;
    lda #$17                                        ;
    sta $0209, X ; tile                             ;
    lda $020A, X                                    ;
    and #%10111111                                  ;
    sta $020A, x ; attr                             ;
                                                    ;
    lda #$18                                        ;
    sta $020D, X ; tile                             ;
    lda $020E, X                                    ;
    and #%10111111                                  ;
    sta $020E, x ; attr                             ;
                                                    ;
    jmp endLoadRightAnimation                       ;
                                                    ;
RIGHTnext1:                                         ;
    cmp #$01                                        ;
    beq loadRightFrame2                             ;
    jmp RIGHTnext2                                  ;
                                                    ;
                                                    ;
loadRightFrame2:                                    ;
    lda #$04                                        ;
    sta $0201, X ; tile                             ;
    lda $0202, X                                    ;
    and #%10111111                                  ;
    sta $0202, x ; attr                             ;
                                                    ;
    lda #$05                                        ;
    sta $0205, X ; tile                             ;
    lda $0206, X                                    ;
    and #%10111111                                  ;
    sta $0206, x ; attr                             ;
                                                    ;
    lda #$14                                        ;
    sta $0209, X ; tile                             ;
    lda $020A, X                                    ;
    and #%10111111                                  ;
    sta $020A, x ; attr                             ;
                                                    ;
    lda #$15                                        ;
    sta $020D, X ; tile                             ;
    lda $020E, X                                    ;
    and #%10111111                                  ;
    sta $020E, x ; attr                             ;
    jmp endLoadRightAnimation                       ;
                                                    ;
RIGHTnext2:                                         ;
    cmp #$02                                        ;
    beq loadRightFrame3                             ;
    jmp RIGHTnext3                                  ;
                                                    ;
loadRightFrame3:                                    ;
    lda #$0A                                        ;
    sta $0201, X ; tile                             ;
    lda $0202, X                                    ;
    and #%10111111                                  ;
    sta $0202, x ; attr                             ;
                                                    ;
    lda #$0B                                        ;
    sta $0205, X ; tile                             ;
    lda $0206, X                                    ;
    and #%10111111                                  ;
    sta $0206, x ; attr                             ;
                                                    ;
    lda #$1A                                        ;
    sta $0209, X ; tile                             ;
    lda $020A, X                                    ;
    and #%10111111                                  ;
    sta $020A, x ; attr                             ;
                                                    ;
    lda #$1B                                        ;
    sta $020D, X ; tile                             ;
    lda $020E, X                                    ;
    and #%10111111                                  ;
    sta $020E, x ; attr                             ;
    jmp endLoadRightAnimation                       ;
                                                    ;
                                                    ;
RIGHTnext3:                                         ;
    cmp #$03                                        ;
    beq loadRightFrame4                             ;
    jmp endLoadRightAnimation                       ;
                                                    ;
loadRightFrame4:                                    ;
    lda #$04                                        ;
    sta $0201, X ; tile                             ;
    lda $0202, X                                    ;
    and #%10111111                                  ;
    sta $0202, x ; attr                             ;
                                                    ;
    lda #$05                                        ;
    sta $0205, X ; tile                             ;
    lda $0206, X                                    ;
    and #%10111111                                  ;
    sta $0206, x ; attr                             ;
                                                    ;
    lda #$14                                        ;
    sta $0209, X ; tile                             ;
    lda $020A, X                                    ;
    and #%10111111                                  ;
    sta $020A, x ; attr                             ;
                                                    ;
    lda #$15                                        ;
    sta $020D, X ; tile                             ;
    lda $020E, X                                    ;
    and #%10111111                                  ;
    sta $020E, x ; attr                             ;
    jmp endLoadRightAnimation                       ;
                                                    ;
;---------------------------------------------------;




; Fixes a problem that occurs when reversing the tiles
; vertically when making the character change direction.
; This problem arises since we must remember that the
; character is made of 4 tiles and what is inverted
; are those 4 tiles, not the complete character.
; So you have to reflect the tiles but also exchange positions.

; Parametro1 en el registro PARAMETRO1: direccion de la ubicacion de la informacion del jugador.
; Parametro2 en el registro PARAMETRO2: direccion de la ubicacion de los sprites del jugador.

;----------------------------------------;
                                         ;
checkAndFixMirroring:                    ;
    ldx PARAMETRO1                       ;
    ldy PARAMETRO2                       ;
                                         ;
    lda $4, X                            ;
    cmp #$1                              ;
    bne endCheckAndFixMirroring          ;
                                         ;
        lda $0203, Y                     ;
        sta TEMP                         ;
        lda $0207, Y                     ;
                                         ;
        sta $0203, y                     ;
        sta $020B, y                     ;
        lda TEMP                         ;
        sta $0207, Y                     ;
        sta $020F, y                     ;
                                         ;
        lda #$0                          ;
        sta $4, X                        ;
endCheckAndFixMirroring:                 ;
  rts                                    ;
;----------------------------------------;                         

            










