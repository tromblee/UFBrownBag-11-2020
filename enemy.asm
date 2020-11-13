    processor 6502

    include "vcs.h"
    include "macro.h"

    seg.u Variables
    org $80
    
JetXPos         byte    ; P0
JetYPos         byte
EnemyXPos       byte    ; P1
EnemyYPos       byte
MissileXPos     byte    ; M0
MissileYPos     byte
JetSpritePtr    word
JetColorPtr     word
EnemySpritePtr  word
EnemyColorPtr   word
JetAnimOffset   byte
Random          byte
Score           byte
Timer           byte    ; relative to Score
OnesDigitOffset word    ; 1's lookup
TensDigitOffset word    ; 10's lookup
Temp            byte
ScoreSprite     byte
TimerSprite     byte

BoundryColor    byte
PlayfieldColor  byte

; Constants
JET_HEIGHT = 9
ENEMY_HEIGHT = 9
DIGITS_HEIGHT = 5
PLAYFIELD_BOUNDS_LEFT = 75
PLAYFIELD_BOUNDS_RIGHT = 255
PLAYFIELD_BOUNDS_TOP =  50
PLAYFIELD_BOUNDS_BOTTOM = 5

    seg Code
    org $F000

Reset:
    CLEAN_START

    ; init
    lda #10
    sta JetYPos
    lda #68
    sta JetXPos

    lda #83
    sta EnemyYPos
    lda #30
    sta EnemyXPos

    lda #%11010100
    sta Random

    lda #$00
    sta Score
    sta Timer

    ; pointers
    lda #<JetSprite
    sta JetSpritePtr
    lda #>JetSprite
    sta JetSpritePtr+1

    lda #<JetColor
    sta JetColorPtr
    lda #>JetColor
    sta JetColorPtr+1

    lda #<EnemySprite
    sta EnemySpritePtr
    lda #>EnemySprite
    sta EnemySpritePtr+1

    lda #<EnemyColor
    sta EnemyColorPtr
    lda #>EnemyColor
    sta EnemyColorPtr+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #%00000000          ; clear (disable)
        cpx MissileYPos
        bne .SkipMissileDraw    ; !=
.DrawMissile:
        lda #%00000010          ; enable missile  display
        inc MissileYPos
.SkipMissileDraw:
        sta ENAM0               ; TIA missile register
    ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StartFrame:
    
    ; VSYNC
    lda #2
    sta VBLANK
    sta VSYNC

    REPEAT 3
        sta WSYNC
    REPEND
    lda #0
    sta VSYNC

    ; VBLANK
    REPEAT 31
        sta WSYNC
    REPEND

    ; Post VBlank - cycle count important
    ; P0
    lda JetXPos         
    ldy #0              
    jsr SetObjectXPos
    ; P1
    lda EnemyXPos     
    ldy #1
    jsr SetObjectXPos
    ; M0
    lda MissileXPos
    ldy #2
    jsr SetObjectXPos

    jsr CalculateDigitOffset

    jsr GenerateJetSound
    jsr GenerateMissileSound

    sta WSYNC
    sta HMOVE

    lda #0
    sta VBLANK

    ; Scoreboard -- 14 lines (10 + 4 spacing)
    lda #0              ; Clear TIA before each frame
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    lda #$1C
    sta COLUPF
    lda #%00000000
    sta CTRLPF          ; no Reflection

    ldx #DIGITS_HEIGHT
.ScoreDigitLoop:
    ; Ten's
    ldy TensDigitOffset ; offset for lookup table
    lda Digits,Y
    and #$F0            ; mask One's digit
    sta ScoreSprite

    ; One's
    ldy OnesDigitOffset ; offset for lookup table
    lda Digits,Y
    and #$0F            ; mask Ten's digit

    ; Merge One's with Ten's
    ora ScoreSprite
    sta ScoreSprite

    ; Display Score
    sta WSYNC
    sta PF1

    ; Timer
    ldy TensDigitOffset+1
    lda Digits,Y
    and #$F0
    sta TimerSprite

    ldy OnesDigitOffset+1
    lda Digits,Y
    and #$0F
    ora TimerSprite
    sta TimerSprite

    ; wait
    jsr Sleep12Cycles

    sta PF1

    ldy ScoreSprite
    sta WSYNC

    ; update Score display
    sty PF1
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles

    ; Display Timer
    dex
    sta PF1
    bne .ScoreDigitLoop ; !=0

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

    
GameVisibleLine:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Screen Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; colors
    lda PlayfieldColor
    sta COLUBK

    lda BoundryColor
    sta COLUPF
        
    ; playfield
    ldx #%00000001        ; Reflect
    stx CTRLPF
    ldx #%11111111        ; Full Wall
    stx PF0
    ldx #%11111000        ; Partial Wall, Partial playfield
    stx PF1
    ldx #%00000000        ; Empty playfield
    stx PF2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visible Game Screen - Draw Game Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #89             ; 192 normally but using a "2 line" kernel
.GameLineLoop:
    DRAW_MISSILE        ; macro

.InsideJetSprite:
    txa
    sec                 ; set carry flag (before any subtraction)
    sbc JetYPos
    cmp #JET_HEIGHT
    bcc .DrawSpriteP0   ; if <
    lda #0              
.DrawSpriteP0
    clc                 ; Clear and add offset to A
    adc JetAnimOffset
    tay                 ; Y only register that allows indirect accessing
    lda (JetSpritePtr),Y 
    sta WSYNC           ; wait for scanline
    sta GRP0
    lda (JetColorPtr),Y 
    sta COLUP0
    
.InsideEnemySprite:
    txa
    sec                 ; set carry flag (before any subtraction)
    sbc EnemyYPos
    cmp #ENEMY_HEIGHT
    bcc .DrawSpriteP1   ; if <
    lda #0              
.DrawSpriteP1
    tay                 ; Y only register that allows indirect accessing
    lda #%00000011
    sta NUSIZ1
    lda (EnemySpritePtr),Y 
    sta WSYNC           ; wait for scanline
    sta GRP1
    lda (EnemyColorPtr),Y 
    sta COLUP1
    
    dex
    bne .GameLineLoop

    lda #0              ; reset Jet to default offset
    sta JetAnimOffset
    

Overscan:
    lda #2
    sta VBLANK     ; turn VBLANK on again for overscan
    REPEAT 30
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Input - Joystick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000
    bit SWCHA
    bne CheckP0Down

    lda JetYPos             ; boundry check
    cmp #70
    bpl CheckP0Down         ; >=

    inc JetYPos
CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left

    lda JetYPos             ; boundry check
    cmp #5
    bmi CheckP0Left         ; <

    dec JetYPos             ;
CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right        

    lda JetXPos             ; boundry check
    cmp #35
    bmi CheckP0Right        ; <

    dec JetXPos
    lda #JET_HEIGHT          ; turning bitmap
    sta JetAnimOffset
CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne CheckButtonPressed

    lda JetXPos             ; boundry check
    cmp #100
    bpl CheckButtonPressed  ; >=

    inc JetXPos
    lda #JET_HEIGHT          ; turning bitmap
    sta JetAnimOffset
CheckButtonPressed:
    lda #%10000000
    bit INPT4
    bne EndInputCheck

    lda JetXPos             ; set missile position
    clc
    adc #4                  ; center missile on Jet
    sta MissileXPos
    lda JetYPos
    clc
    adc #8                  ; center missile on Jet
    sta MissileYPos

EndInputCheck:
;    jsr ValidatePlayerBounds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enemy Movement Calculations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateEnemyPosition:
    lda EnemyYPos
    clc
    cmp #0
    bmi .ResetEnemyPosition ; <
    dec EnemyYPos
    jmp EndPositionUpdate
.ResetEnemyPosition
    jsr GetRandomEnemyPos

    ; Increment Timer (in BCD)
    sed                     ; set decimal mode
    lda Timer
    clc
    adc #1
    sta Timer
    cld                     ; clear decimal mode

    ;inc Score
    ;inc Timer

EndPositionUpdate:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision Detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000          ; P0 & P1 collision?
    bit CXPPMM
    bne .CollisionP0P1

    jsr SetTerrainPlayfieldColor
   ; jmp CheckCollisionP0PF
    jmp CheckCollisionM0P1
.CollisionP0P1:
    jsr GameOver

;CheckCollisionP0PF:
;    lda #%10000000          ; P0 & P1 collision?
;    bit CXP0FB
;    bne .CollisionP0PF
;    jmp EndCollisionCheck;

;.CollisionP0PF:
  ;  jsr GameOver

CheckCollisionM0P1:
    lda #%10000000
    bit CXM0P
    bne .CollisionM0P1
    jmp EndCollisionCheck
.CollisionM0P1
    ; increment score
    sed
    lda Score
    clc
    adc #1
    sta Score
    cld
    ;  reset missile
    lda #0
    sta MissileYPos


EndCollisionCheck:
    sta CXCLR
    jmp StartFrame



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =========== Subroutines/Structs =========== 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GenerateJetSound subroutine
    ; volume
    lda #1;0 ;3      
    sta AUDV0   
    ; frequency
    lda JetYPos 
    lsr         ; change with Y Pos (divide by 8)
    lsr
    lsr
    sta Temp
    lda 31      ; max frequency
    sec
    sbc Temp
    sta AUDF0

    ; tone
    lda #8      
    sta AUDC0   

    rts

GenerateMissileSound subroutine

    lda #0      
    sta AUDV1   

    clc
    lda JetYPos
    adc #9                  ; center missile on Jet

    cmp MissileYPos          
    bcs .GenerateJetSoundEnd

    clc
    lda #89
    cmp MissileYPos          
    bcc .GenerateJetSoundEnd

    
    ; volume
    lda #3      
    sta AUDV1   
    ; frequency
    lda MissileYPos 
    lsr         ; change with Y Pos (divide by 8)
    lsr
    lsr
    sta Temp
    lda 31      ; max frequency
    sec
    sbc Temp
    sta AUDF1
    ; tone
    lda #15    
    sta AUDC1   
.GenerateJetSoundEnd
    rts

ValidatePlayerBounds subroutine
.BoundsXMin:    
    lda JetXPos
    cmp #PLAYFIELD_BOUNDS_LEFT
    bmi .BoundsXMax             ; <
    lda #PLAYFIELD_BOUNDS_LEFT
    sta JetXPos
    jmp .BoundsCheckComplete

.BoundsXMax:
    lda JetXPos
    cmp #PLAYFIELD_BOUNDS_RIGHT
    bpl .BoundsYMin             ; >
    lda #PLAYFIELD_BOUNDS_RIGHT
    sta JetXPos
    jmp .BoundsCheckComplete

.BoundsYMin:
    lda JetYPos
    cmp #PLAYFIELD_BOUNDS_BOTTOM
    bmi .BoundsYMax             ; <
    lda #PLAYFIELD_BOUNDS_BOTTOM
    sta JetYPos
    jmp .BoundsCheckComplete

.BoundsYMax:
    lda JetYPos
    cmp #PLAYFIELD_BOUNDS_TOP    ; >
    bpl .BoundsCheckComplete
    lda #PLAYFIELD_BOUNDS_TOP
    sta JetYPos

.BoundsCheckComplete

    rts

SetObjectXPos subroutine
    sta WSYNC       ; wait for next scanline
    sec             ; set carry flag before subtraction

.Div15Loop:
    sbc #15         ; subtract 15 from the accumulator
    bcs .Div15Loop  ; loop while carry flag is still set
    
    ; adjust the remainder in A between -8 and 7
    eor #7          
    ; shift left by 4, as HMP0 uses only 4 bits
    asl             
    asl
    asl
    asl

    sta HMP0,Y      ; set fine position
    sta RESP0,Y     ; reset 15-step brute position
    rts             ; return to subroutine

GameOver subroutine
    lda #$30
    ; sta COLUBK
    sta BoundryColor
    sta PlayfieldColor

    lda #0
    sta Score

    rts

CalculateDigitOffset subroutine
    ldx #1
.PrepareScoreLoop
    lda Score,X     ; load Timer value 
    and #%00001111  ; or $0F
    sta Temp
    
    ; temp *5
    asl             
    asl
    adc Temp
    sta OnesDigitOffset,X

    lda Score,X
    and #$F0
    sta Temp

    ; temp / 16 [high nibble] + temp * 5 [offset for score height]
    lsr
    lsr
    sta Temp
    lsr
    lsr
    adc Temp
    sta TensDigitOffset,X

    dex
    bpl .PrepareScoreLoop   ; >=0

    rts

Sleep12Cycles subroutine
    ;REPEAT 12
    ;    nop
    ;REND
    rts

SetTerrainPlayfieldColor subroutine
    lda #$A0
    sta BoundryColor
    
    lda #$00
    sta PlayfieldColor
    
    rts

Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

JetSprite:
    .byte #%00000000
    .byte #%10000010
    .byte #%11000110
    .byte #%11111110
    .byte #%01111100
    .byte #%00111000
    .byte #%00111000
    .byte #%00010000
    .byte #%00010000

JetSpriteTurn:
    .byte #%00000000
    .byte #%01000100
    .byte #%01000100
    .byte #%01111100
    .byte #%01111100
    .byte #%00111000
    .byte #%00111000
    .byte #%00010000
    .byte #%00010000

EnemySprite:
    .byte #%00000000
    .byte #%10000001
    .byte #%10000001
    .byte #%01000010
    .byte #%00100100
    .byte #%10011001
    .byte #%01100110
    .byte #%01011010
    .byte #%00111100

JetColor:
    .byte #$0E
    .byte #$40
    .byte #$04
    .byte #$04
    .byte #$06
    .byte #$06
    .byte #$08
    .byte #$0A
    .byte #$92
    
JetColorTurn:
    .byte #$0E
    .byte #$40
    .byte #$04
    .byte #$04
    .byte #$06
    .byte #$06
    .byte #$08
    .byte #$0A
    .byte #$92

EnemyColor:
    .byte #$00
    .byte #$6A
    .byte #$04
    .byte #$04
    .byte #$06
    .byte #$06
    .byte #$08
    .byte #$0A
    .byte #$04

GetRandomEnemyPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random

    lsr         ; Divide by 4 (by doing 2 Right shifts)
    lsr
    sta EnemyXPos
    lda #30
    adc EnemyXPos
    sta EnemyXPos

    lda #96
    sta EnemyYPos

    rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill ROM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    word Reset
    word Reset





    