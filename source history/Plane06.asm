;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Comenzado: 10/05/2001 - 18:00 Hs - Córdoba
;
;por Gonzalo Fernández
;

	processor 6502
	include VCS.H

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Variables

    	SEG.U vars
    	org $0080
    	
;background variables
moveBk	 		ds 1 		;controls the Background movement
colorBk1		ds 1		;Var for store in COLUBK (every scanline)
colorBk2		ds 1		;Var for store in colorBk1 calculable during Overscan
banda			ds 1		;height of the Background bars

;plane variables
planePtr		ds 2		;Pointer to aeroplane shape
planeColorPtr		ds 2		;plane color

;sprite 1 and 2 vaiables
sprite1Ptr		ds 2		;pointer to higher placed sprite on screen
spriteColor1Ptr		ds 2
sprite2Ptr		ds 2		;pointer to lower placed sprite on screen
spriteColor2Ptr		ds 2
sprite3Ptr		ds 2		;pointer to lower placed sprite on screen
spriteColor3Ptr		ds 2

;all vertical positions
VerPosPlane		= 14		;Vertical Position aeroplane
VerPosM0		ds 1		;Vertical Position of Misiles
VerPosM1		ds 1
VerPosPF		ds 1
VerPosSprite1		ds 1
VerPosSprite2		ds 1
VerPosSprite3		ds 1

;all horizontal positions
HorPosPlane		ds 1		
HorPosSprite1		ds 1
HorPosM0		ds 1
HorPosM1		ds 1
HorPosBl		ds 1
HorPosSprite2		ds 1		;only accept values between 27 & 160 (decimal)
HorPosSprite3		ds 1

;missile variables
M0Control		ds 1		;Boolean: 1=M0 shooted 0=M0 ready to be shooted
M1Control		ds 1

flyLow			ds 1		;if plane is flying low

changeK			ds 1		;a Kernel switcher

;misc variables
fCounter		ds 1
random			ds 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    	SEG  código
        org  $F000

Start   sei
	cld
	ldx #$FF
	txs
	lda #$00

clrstk  sta $00,X
	dex
	bne clrstk
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculos

;background setup
	lda #$AB			;Setting Background color (blue)
	sta colorBk2
	sta colorBk1
	lda #$0F   			;Initial height of the Background bars
	sta moveBk
	sta banda

	lda planeLPtr
	sta planePtr
	lda planeHPtr
	sta planePtr+1

	lda #74
	sta HorPosPlane
	
	lda #<planeColor-2		;Set pointers to aeroplane color
	sta planeColorPtr
	lda #>planeColor
	sta planeColorPtr+1
	
	lda #$01			;Reflct Playfield
	sta CTRLPF

	lda #$FF
	sta fCounter
	lda #158
	sta VerPosPF
	
	lda #$6D
	sta random
	sta random+1
	sta random+2
	sta random+3

	jsr RandomByte	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Start Vertical Blank	
	
vblank  lda #$02
	sta VBLANK
	sta VSYNC
	sta WSYNC
	sta WSYNC
	sta WSYNC
	lda #$00
	sta VSYNC
		
	lda #44
	sta TIM64T
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculos
				
 	ldy #4          	;2
PosHor	ldx HorPosPlane,Y	;4
        jsr PosPlayer		;6
        dey			;2
        bpl PosHor		;2
	sta WSYNC           	;3	Finish current line
        sta HMOVE		;3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Vertical Blank	

VblankLoop	
	lda INTIM
	bne VblankLoop
	sta VBLANK		;	Turn off Vertical Blank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Draw Screen
	
	ldy #16
top	sta WSYNC		;	We'll have Score or something here
	sty COLUBK
	dey
	bne top
	ldx #0
	
	lda colorBk1		;3  	TJ
	sta COLUBK		;3

	ldy #76   		;2	76 x 2 = 140 scanlines of main Kernel
	lda #$FA
	sta COLUPF
	
	sta HMCLR
	
	lda avionColor+14	;to avoid some cylce counting problem
	sta COLUP1		;this updates the COLUP1 in the middle of the scanline

	lda #0
	sta changeK
	
	lda #80
	sta VerPosSprite3
	
	lda #50
	sta VerPosSprite2

	lda #20
	sta VerPosSprite1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Dibujar pantalla

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	
k1	sta WSYNC			;k1/l1 Sprite 3
	clc				;2
	tya				;2
	sbc VerPosSprite3		;3	
	adc #13				;2	
	bpl noChangeK1
	inc changeK	
noChangeK1
	bcc noSprite1_A			;2
	lda sprite1Ptr),Y      	;5 
	sta GRP1			;3
	lda spriteColor1Ptr),Y       	;5 
	sta COLUP1			;3
sprite1Done_A

	sta WSYNC		;k1/l2	River - Island - misiles
	
	dec banda               ;5	Changing Background color
        beq f2                  ;2³	
        bpl f1                  ;2³	Thanks to T.J.
        lda #$0F                ;2
        sta banda               ;3
        jmp f3                  ;3
         
f1      lda colorBk1            ;3
        sta COLUBK              ;3
        jmp f3                  ;3

f2      lda #%00000010          ;2
        eor colorBk1            ;3
        sta colorBk1            ;3

f3
	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc noPF_A		;2	
	lsr			;2
	lsr			;2
	tax			;2
	lda island,x		;4
	sta PF2
pfDone_A

	ldx #$1E ;ENAM1		;2	Misiles Routine
	txs			;2
	cpy VerPosM0		;3
	php			;3
	cpy VerPosM0		;3
	php			;3

;end 2nd scanline
	lda changeK
	bne k2
	
	dey			;2
	bne k1			;2	Go back for a new scanline
	jmp exitKernel		;3

noPF_A	lda #$00		;2
	sta PF2			;3
	beq pfDone_A		;2

noSprite1_A
	lda #$00		;2
	sta GRP1		;3
	beq sprite1Done_A	;2

k2	dey			;k2/l1	re-position P1
	ldx HorPosSprite2	;3
	sta WSYNC         	;begin oscanline
        lda HorzTable+12,X   	;4
        sta HMP1          	;3
        and #$0F            	;2
        tax                 	;2
ch2     dex                 	
        bpl ch2              	
        sta RESP1         	;new position

	sta WSYNC		;k2/l2 River - Island - misiles
	sta HMOVE

	dec banda               ;5	Changing Background color
        beq f5                  ;2³	
        bpl f4                  ;2³	Thanks to T.J.
        lda #$0F                ;2
        sta banda               ;3
        jmp f6                  ;3
         
f4      lda colorBk1            ;3
        sta COLUBK              ;3
        jmp f6                  ;3

f5      lda #%00000010          ;2
        eor colorBk1            ;3
        sta colorBk1            ;3

f6
	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc noPF_B		;2	
	lsr			;2
	lsr			;2
	tax			;2
	lda island,x		;4
	sta PF2
pfDone_B

	ldx #$1E ;ENAM1		;2	Misiles Routine
	txs			;2
	cpy VerPosM0		;3
	php			;3
	cpy VerPosM0		;3
	php			;3

;end 2nd scanline
	dey			;2
	bne k3			;2	Go back for a new scanline
	jmp exitKernel		;3

noPF_B	lda #$00		;2
	sta PF2			;3
	beq pfDone_B		;2

noSprite1_B
	lda #$00		;2
	sta GRP1		;3
	beq sprite1Done_B	;2


k3	sta WSYNC		;k3/l1	Sprite 2
k3_	clc				;2
	tya				;2
	sbc VerPosSprite2		;3	
	adc #13				;2	
	bpl noChangeK3
	dec changeK	
noChangeK3
	bcc noSprite1_B			;2
	lda (sprite1Ptr),Y      	;5 
	sta GRP1			;3
	lda (spriteColor1Ptr),Y       	;5 
	sta COLUP1			;3
sprite1Done_B

	sta WSYNC			;k3/l2 River - Island - misiles
	
	dec banda               ;5	Changing Background color
        beq f8                  ;2³	
        bpl f7                  ;2³	Thanks to T.J.
        lda #$0F                ;2
        sta banda               ;3
        jmp f9                  ;3
         
f7      lda colorBk1            ;3
        sta COLUBK              ;3
        jmp f9                  ;3

f8      lda #%00000010          ;2
        eor colorBk1            ;3
        sta colorBk1            ;3

f9
	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc noPF_C		;2	
	lsr			;2
	lsr			;2
	tax			;2
	lda island,x		;4
	sta PF2
pfDone_C

	ldx #$1E ;ENAM1		;2	Misiles Routine
	txs			;2
	cpy VerPosM0		;3
	php			;3
	cpy VerPosM0		;3
	php			;3

;end 2nd scanline
	lda changeK
	beq k4
	
	dey			;2
	bne k3			;2	Go back for a new scanline
	jmp exitKernel		;3

noPF_C	lda #$00		;2
	sta PF2			;3
	beq pfDone_C		;2

noSprite1_C
	lda #$00		;2
	sta GRP1		;3
	beq sprite1Done_C	;2

k4	dey			;k4/l1 re-position P1
	ldx HorPosSprite3	;3
	sta WSYNC         	;begin oscanline
        lda HorzTable+12,X   	;4
        sta HMP1          	;3
        and #$0F            	;2
        tax                 	;2
ch3     dex                 	
        bpl ch3              	
        sta RESP1         	;new position
;I'll use some cycles to update de NUSIZ1 register, if it's needed


	sta WSYNC		;k4/l2 River - Island - misiles
	sta HMOVE

	dec banda               ;5	Changing Background color
        beq f11                  ;2³	
        bpl f10                  ;2³	Thanks to T.J.
        lda #$0F                ;2
        sta banda               ;3
        jmp f12                  ;3
         
f10      lda colorBk1            ;3
        sta COLUBK              ;3
        jmp f12                  ;3

f11      lda #%00000010          ;2
        eor colorBk1            ;3
        sta colorBk1            ;3

f12
	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc noPF_D		;2	
	lsr			;2
	lsr			;2
	tax			;2
	lda island,x		;4
	sta PF2
pfDone_D

	ldx #$1E ;ENAM1		;2	Misiles Routine
	txs			;2
	cpy VerPosM0		;3
	php			;3
	cpy VerPosM0		;3
	php			;3

;end 2nd scanline
	dey			;2
	bne k5			;2	Go back for a new scanline
	jmp exitKernel		;3

k5
;first line

	sta WSYNC		;k5/l1 Plane - Sprite 1
	tya			;2
	cmp VerPosPlane		;2
	bpl planeDone		;2
	lda (planePtr),Y        ;5 
	sta GRP0		;3
	lda (planeColorPtr),Y	;5
	sta COLUP0		;3

planeDone

	clc				;2
	tya				;2
	sbc VerPosSprite1		;3	
	adc #13				;2	
	bcc noSprite1_C			;2
	lda (sprite1Ptr),Y      	;5 
	sta GRP1			;3
	lda (spriteColor1Ptr),Y       	;5 
	sta COLUP1			;3
sprite1Done_C


;second line
	sta WSYNC			;k2/l2 River - Island - misiles
	
	dec banda               ;5	Changing Background color
        beq f14                  ;2³	
        bpl f13                  ;2³	Thanks to T.J.
        lda #$0F                ;2
        sta banda               ;3
        jmp f15	                  ;3
         
f13     lda colorBk1            ;3
        sta COLUBK              ;3
        jmp f15                  ;3

f14     lda #%00000010          ;2
        eor colorBk1            ;3
        sta colorBk1            ;3

f15
	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc noPF_E		;2	
	lsr			;2
	lsr			;2
	tax			;2
	lda island,x		;4
	sta PF2
pfDone_E

	ldx #$1E ;ENAM1		;2	Misiles Routine
	txs			;2
	cpy VerPosM0		;3
	php			;3
	cpy VerPosM0		;3
	php			;3

;end 2nd scanline
	
	dey			;2
	bne k5			;2	Go back for a new scanline
	jmp exitKernel		;3

noPF_D	lda #$00		;2
	sta PF2			;3
	beq pfDone_D		;2

noPF_E	lda #$00		;2
	sta PF2			;3
	beq pfDone_E		;2

exitKernel			;end main kernel

	sta WSYNC
	lda #$00
	sta PF2
	sta GRP1
	sta COLUBK
	ldx #$FF		;	Restore Stack Value
	txs		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	ldy #26			;2
bottom	sta WSYNC		;3	We'll have Score or something here
	stx COLUBK		;3
	dey			;2
	bne bottom		;2
	sty COLUBK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Start Overscan
		
	lda #$02		;2
	sta VBLANK		;3
	lda #35		     	;2	30 lines * 76 cycles per line / 64 timercycles
        sta TIM64T  		;4	=35,625 wich means ~2280 cycles to use

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculos
	
	lda colorBk2		;adjust the background variables
	sta colorBk1	
			
ReadJoy				;	Primitive reading routine, I'll change it later
	lda SWCHA		
	cmp #$BF		
	beq left		
	cmp #$7F		
	beq right		
	cmp #$EF		
	beq up			
	cmp #$DF		
	beq down		
	cmp #$6F
	beq riup
	cmp #$AF
	beq leup
	cmp #$5F
	beq rido
	cmp #$9F
	beq ledo
	
	jmp mov			
	
;the 8 possible movements
up      dec VerPosPF	
	dec VerPosSprite1
	dec VerPosSprite2
	dec fCounter
	lda moveBk		;3
	cmp #$0F		;2
	beq mov			;2
	inc moveBk		;5
	jmp mov			;3

down	inc flyLow
	jmp mov			;3
	
left    lda HorPosPlane		;3
	beq mov			;	Do not dec HorPos if the aeroplane
	dec HorPosPlane		;	is in the left limit (pixel 0)
	dec HorPosM0
	jmp mov
	
right   lda HorPosPlane
	cmp #152		;	Do not inc HorPos if the aeroplane
	beq mov			;	is in the right limit (pixel 152)
	inc HorPosPlane
	inc HorPosM0
	jmp mov

riup	dec VerPosPF	
	dec VerPosSprite1
	dec VerPosSprite2
	dec fCounter
	lda moveBk		;3
	cmp #$0F		;2
	beq mov			;2
	inc moveBk		;5
	lda HorPosPlane
	cmp #152		;	Do not inc HorPos if the aeroplane
	beq mov			;	is in the right limit (pixel 152)
	inc HorPosPlane
	inc HorPosM0
	jmp mov
	
leup	dec VerPosPF	
	dec VerPosSprite1
	dec VerPosSprite2
	dec fCounter
	lda moveBk		;3
	cmp #$0F		;2
	beq mov			;2
	inc moveBk		;5
	lda HorPosPlane		;3
	beq mov			;	Do not dec HorPos if the aeroplane
	dec HorPosPlane		;	is in the left limit (pixel 0)
	dec HorPosM0
	jmp mov
	
rido	inc flyLow
	lda HorPosPlane
	cmp #152		;	Do not inc HorPos if the aeroplane
	beq mov			;	is in the right limit (pixel 152)
	inc HorPosPlane
	inc HorPosM0
	jmp mov
	
ledo	inc flyLow
	lda HorPosPlane		;3
	beq mov			;	Do not dec HorPos if the aeroplane
	dec HorPosPlane		;	is in the left limit (pixel 0)
	dec HorPosM0
	jmp mov
	
;adjust all background stuffs
mov	inc moveBk	;5	;	This variable is for manage the 
				;	Background movement
	lda moveBk	;3
	sta banda	;3	;	This is the variable that makes the movement
	cmp #$10	;2
	bne movDone	;2
	lda #$00  	;2
	sta moveBk	;3
		
	lda #%00000010	;2
	eor colorBk2	;3	;	This changes Background color (LUM)
	sta colorBk2	;3
movDone

;M0 movement
M0Mov
	lda M0Control		;	Misile0 movement (only if M0Control is 1)
	beq joyButton
	inc VerPosM0
	inc VerPosM0
	lda VerPosM0
	cmp #80			;	Vertical limit M0
	bne buttonDone
	lda #0
	sta VerPosM0
	dec M0Control
	lda HorPosPlane
	adc #3			;	Place Misile in the middle of plane shape
	sta HorPosM0
	
joyButton		
	lda INPT4		;	Read joy 0 button
	bmi buttonDone
	lda #$01
	sta M0Control	
	lda #10
	sta VerPosM0
buttonDone
	
planeAnimation
	lda flyLow
	tax
	lda planeLPtr,X
	sta planePtr
	lda planeHPtr,X
	sta planePtr+1
	lda #0
	sta flyLow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	dec VerPosPF
	dec VerPosSprite1
	dec VerPosSprite2

	jsr RandomByte
	
	lda fCounter
	and #%11111110
	bne fCDone_A
	lda #$FF
	sta fCounter
	lda #158
	sta VerPosPF
	lda #128
	sta VerPosSprite1
	lda #98
	sta VerPosSprite2

fCDone_A

	lda fCounter
	and #%11111110
	cmp #$80
	bne fCDone_B
	lda #128
	sta VerPosSprite1
	lda #78
	sta VerPosSprite2
fCDone_B

;add some movement to sprite 1, just to see it
	inc HorPosSprite2
	lda HorPosSprite2
	cmp #160
	bne con
	lda #0
	sta HorPosSprite2
con	lda #75
	sta HorPosSprite1
	

;for the moment, sprite 1 is the purple plane (avion)
;and sprite 2 is the palm tree (palmera)
	clc
	lda #<avion       ; Init Pointers
	sbc VerPosSprite2
	adc #12
	sta sprite2Ptr
	lda #>avion       ; Init Pointers
	sta sprite2Ptr+1

	clc
	lda #<avionColor   ; Init Pointers
	sbc VerPosSprite2
	adc #12
	sta spriteColor2Ptr 
	lda #>avionColor        ; Init Pointers
	sta spriteColor2Ptr+1
	

	clc
	lda #<palmera        ; Init Pointers
	sbc VerPosSprite1
	adc #12
	sta sprite1Ptr
	lda #>palmera        ; Init Pointers
	sta sprite1Ptr+1

	clc
	lda #<palmeraColor   ; Init Pointers
	sbc VerPosSprite1
	adc #12
	sta spriteColor1Ptr 
	lda #>palmeraColor        ; Init Pointers
	sta spriteColor1Ptr+1

	dec fCounter

oscan   lda INTIM	;4	;	Finish the overscan
       	bne oscan	;2
       	jmp vblank	;3	;	Start the new screen

RandomByte
	ldx #8
RandomByte1
	jsr RandomBit
	dex
	bne RandomByte1
	rts

RandomBit		;Erik Mooney's random generator
	lda random+3
	asl
	asl
	asl
	eor random+3
	asl
	asl
	rol random
	rol random+1
	rol random+2
	rol random+3
	rts
	
PosPlayer
	sta WSYNC           	;begin oscanline
        lda HorzTable,X     	;+4  7  
        sta HMP0,Y          	;+3 10
        and #$0F            	;+2 12
        tax                 	;+2 14
P0      dex                 	;+2 16
        bpl P0              	;
        sta RESP0,Y         	;
        rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
spritesIslandH	.byte	#>palmera, #>islander1
spritesIslandL	.byte	#<palmera, #<islander1

colorsIslandH	.byte	#>palmeraColor, #>islanderColor
colorsIslandL	.byte	#<palmeraColor, #<islanderColor
		
planeColor	.byte	$00,$02,$02,$02,$90,$90,$90,$90,$90,$90,$90,$90

planeHPtr	.byte	#>plane, #>planeDown		;plane is always in the same Vert. Pos.
planeLPtr	.byte	#<plane-2, #<planeDown-2

palmeraColor    .byte   $00,$00,$2A,$2A,$2A,$20,$20,$20,$20,$D4,$D4,$D4,$D4

avionColor    	.byte  	$00,$04,$04,$04,$04,$00,$00,$00,$50,$50,$50,$50,$50

planeEnemyColor .byte  	$00,$04,$04,$04,$04,$00,$00,$A5,$A5,$A5,$A5,$A5,$A5

islanderColor   .byte  	$00,$00,$00,$00,$00,$29,$29,$29,$45,$45,$45,$29,$29

nullSprite	.byte	%00000000		;player1
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	
palmera		.byte	%00000000		;player1
		.byte	%00000000
		.byte	%01010000
		.byte	%00111000
		.byte	%01100000
		.byte	%01000000
		.byte	%00100000
		.byte	%00100000
		.byte	%00010000
		.byte	%10010100
		.byte	%11011100
		.byte	%00111001
		.byte	%01100110
	
avion		.byte	%00000000		;player1
		.byte	%00010000		
		.byte	%01111000
		.byte	%00010000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00111000
		.byte	%10011100
		.byte	%11110111
		.byte	%10011100
		.byte	%00111000

planeEnemy	.byte	%00000000		;player1
		.byte	%00011000		
		.byte	%00100100
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00011000
		.byte	%00111100
		.byte	%01100110
		.byte	%11111111
		.byte	%11011011
		.byte	%10000001

islander1	.byte	%00000000		;player1
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00110011
		.byte	%00010010
		.byte	%00001100
		.byte	%00001100
		.byte	%00001101
		.byte	%00011110
		.byte	%00101100
		.byte	%00101100

islander2	.byte	%00000000		;player1
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00110011
		.byte	%00010010
		.byte	%00001100
		.byte	%00001101
		.byte	%00011110
		.byte	%00101100
		.byte	%01001100

island2		.byte	%00000000		;PF1
		.byte	%00011000
		.byte	%00111100
		.byte	%00111110
		.byte	%01111110
		.byte	%01111111 
		.byte	%11111111
		.byte	%11111110
		.byte	%01111110
		.byte	%01111100
		.byte	%00111100
		.byte	%00011000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000 
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000

island		.byte	%10000000		;PF2
		.byte	%11100000
		.byte	%11110000
		.byte	%11111000
		.byte	%11111000
		.byte	%11111100 
		.byte	%11111100
		.byte	%11111100
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111100 
		.byte	%11111100
		.byte	%11111100
		.byte	%11111000
		.byte	%11111000
		.byte	%11110000
		.byte	%11100000
		.byte	%10000000


		.byte	%00000000		;player0
plane		.byte	%00000000
		.byte	%00100100
		.byte	%00011000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10000001
		.byte	%11011011 
		.byte	%11111111
		.byte	%01100110
		.byte	%00111100
		.byte	%00011000
		
		.byte	%00000000		;player0
planeDown	.byte	%00000000
		.byte	%01000010
		.byte	%00111100
		.byte	%00011000
		.byte	%10000001
		.byte	%11011011 
		.byte	%11111111
		.byte	%01100110
		.byte	%00111100
		.byte	%00011000	
		.byte	%00000000
		.byte	%00000000		

	ORG $FF4C	

HorzTable    ;this must not cross a page boundary
	    .byte $30,$20,$10,$00,$F0,$E0,$D0,$C0,$B0,$A0,$90
	    .byte $71,$61,$51,$41,$31,$21,$11,$01,$F1,$E1,$D1,$C1,$B1,$A1,$91
	    .byte $72,$62,$52,$42,$32,$22,$12,$02,$F2,$E2,$D2,$C2,$B2,$A2,$92
	    .byte $73,$63,$53,$43,$33,$23,$13,$03,$F3,$E3,$D3,$C3,$B3,$A3,$93
	    .byte $74,$64,$54,$44,$34,$24,$14,$04,$F4,$E4,$D4,$C4,$B4,$A4,$94
	    .byte $75,$65,$55,$45,$35,$25,$15,$05,$F5,$E5,$D5,$C5,$B5,$A5,$95
	    .byte $76,$66,$56,$46,$36,$26,$16,$06,$F6,$E6,$D6,$C6,$B6,$A6,$96
	    .byte $77,$67,$57,$47,$37,$27,$17,$07,$F7,$E7,$D7,$C7,$B7,$A7,$97
	    .byte $78,$68,$58,$48,$38,$28,$18,$08,$F8,$E8,$D8,$C8,$B8,$A8,$98
	    .byte $79,$69,$59,$49,$39,$29,$19,$09,$F9,$E9,$D9,$C9,$B9,$A9,$99
	    .byte $7A,$6A,$5A,$4A,$3A,$2A,$1A,$0A,$FA,$EA,$DA,$CA,$BA,$AA,$9A
	    .byte $7B,$6B,$5B,$4B,$3B,$2B,$1B,$0B,$FB,$EB,$DB,$CB,$BB,$AB,$9B

	org $FFFC
	.word Start
	.word Start     


