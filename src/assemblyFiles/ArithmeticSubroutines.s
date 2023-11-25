
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