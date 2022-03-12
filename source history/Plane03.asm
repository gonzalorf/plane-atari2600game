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
    	
moveBk	 		ds 1 		;controls the Background movement
colorBk1		ds 1		;Var for store in COLUBK (every scanline)
colorBk2		ds 1		;Var for store in colorBk1 calculable during Overscan
banda			ds 1		;height of the Background bars
planePtr		ds 2		;Pointer to aeroplane shape
planeColorPtr		ds 2		;plane color
palmeraPtr		ds 2
palmeraColorPtr		ds 2
VerPosPlane		ds 1		;Vertical Position aeroplane
VerPosM0		ds 1		;Vertical Position of Misiles
VerPosM1		ds 1
VerPosPF		ds 1
VerPosP1		ds 1
VerPosP1T		ds 1
HorPosPlane		ds 1		;Horizontal Positions
HorPosP1		ds 1
HorPosM0		ds 1
HorPosM1		ds 1
M0Control		ds 1		;Boolean: 1=M0 shooted 0=M0 ready to be shooted
flyLow			ds 1		;if plane is flying low
islandTemp		ds 1

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

	lda #$AB			;Setting Background color (blue)
	sta colorBk2
	sta colorBk1
	lda #$0F   		;Initial height of the Background bars
	sta moveBk
	sta banda
	lda #76
	sta HorPosPlane			;Set Horizontal 
	lda #80
	sta HorPosM0
	lda #13				;and Vertical position of the aeroplane
	sta VerPosPlane
	lda #0
	sta VerPosM0
	lda #$90
	sta planeColor
	lda #0
	sta M0Control
	lda #76
	sta HorPosP1
				
	clc
	lda #<plane-2			;Set pointers to aeroplane shape
	sta planePtr
	lda #>plane
        sta planePtr+1
 
 	clc
	lda #<planeColor-2		;Set pointers to aeroplane color
	sta planeColorPtr
	lda #>planeColor
        sta planeColorPtr+1
        
        lda #50
	sta VerPosPF
	lda #60
	sta VerPosP1
	
	lda #$01			;Reflct Playfield
	sta CTRLPF
		
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
				
 	ldy #2          	;2
PosHor	ldx HorPosPlane,Y	;4
        jsr PosPlayer		;6
        dey			;2
        bpl PosHor		;2
	sta WSYNC           	;3	Finish current line
        sta HMOVE		;3
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Finish Vertical Blank	

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
	ldy #70   		;2	70 x 2 = 140 scanlines of Kernel
	lda #$FA
	sta COLUPF

	sta HMCLR	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Dibujar pantalla
	
river				;2LK

;first scanline

	sta WSYNC		;3

	clc
	tya			;2
	sbc VerPosP1		;3	
	adc #8			;2	
	bcc NoP1		;2
	;bne cont
	;sec
	;lda VerPosP1T
	;sbc #34
	;sta VerPosP1T
	
cont	lda (palmeraPtr),Y      	;5 
	sta GRP1			;3
	lda (palmeraColorPtr),Y       	;5 
	sta COLUP1			;3
	
Continue0

	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc NoPF
	; very simply, isn't it :) ..... yes, it is!
	lsr			;2
	lsr			
	tax			;2
	lda island,x		;4
	sta PF2
	
	
;2nd scanline

Continue1

	sta WSYNC		;3	
	
	clc			;2
	tya			;2
	sbc #13 		;3	Check if it's time to draw the aeroplane
	adc #11			;2
	bcc Noplane		;2
	lda (planePtr),Y        ;5 
	sta GRP0		;3
	lda (planeColorPtr),Y
	sta COLUP0
	
Continue2
	
	ldx #ENAM0		;	Misile0 Routine
	txs
	cpy VerPosM0
	php

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

;end 2nd scanline

f3	dey			;2
	bne river		;2	Go back for a new scanline
	jmp f4			;3

NoPF	lda #$00		;2
	sta PF2
	beq Continue1

NoP1	lda #$00		;2
	sta GRP1
	beq Continue0

Noplane	lda #$00		;2
	sta GRP0
	beq Continue2		;2

f4	sta WSYNC
	lda #$00
	sta PF2
	sta GRP1
	ldx #$FF		;	Restore Stack Value
	txs		
	
	ldy #16			;2
bottom	sta WSYNC		;3	We'll have Score or something here
	sty COLUBK		;3
	dey			;2
	bne bottom		;2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Start Overscan
		
	lda #$02		;2
	sta VBLANK		;3
	lda #35		     	;2	30 lines * 76 cycles per line / 64 timercycles
        sta TIM64T  		;4	=35,625 wich means ~2280 cycles to use

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculos
	
	lda colorBk2
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
	
up      dec VerPosPF	
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

M0Mov
	lda M0Control		;	Misile0 movement (only if M0Control is 1)
	beq joyButton
	inc VerPosM0
	inc VerPosM0
	lda VerPosM0
	cmp #80			;	Vertical limit M0
	bne joyButton
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
buttonDone
	
planeAnimation
	lda flyLow	;	Routine taken from Manuel (can I use it?)
	tax
	lda planeLPtr,X
	sta planePtr
	lda planeHPtr,X
	sta planePtr+1
	lda #0
	sta flyLow
	
	dec VerPosPF

	clc
	lda VerPosPF
	sbc #30
	sta VerPosP1
	
	;dec VerPosP1
	;lda VerPosP1
	;cmp #43
	;bne VPP1
	;lda #77
	;sta VerPosP1
;VPP1
	
	
	clc
	lda #<palmera        ; Init Pointers
	sbc VerPosP1
	adc #8
	sta palmeraPtr
	lda #>palmera        ; Init Pointers
	sta palmeraPtr+1

	clc
	lda #<palmeraColor   ; Init Pointers
	sbc VerPosP1
	adc #8
	sta palmeraColorPtr 
	lda #>palmeraColor        ; Init Pointers
	sta palmeraColorPtr+1
	
oscan   lda     INTIM	;4	;	Finish the overscan
       	bne     oscan	;2
       	jmp     vblank	;3	;	Start the new screen

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
	
planeColor	.byte	$00,$04,$04,$04,$90,$90,$90,$90,$90,$90,$90,$90

palmeraColor    .byte   $20,$20,$20,$20,$20,$D4,$D4,$D4,$D4,$D4,$D4,$D4

planeHPtr	.byte	#>plane, #>planeDown
planeLPtr	.byte	#<plane-2, #<planeDown-2
		
palmera		.byte	%00000000
		.byte	%01000000
		.byte	%00100000
		.byte	%00100000
		.byte	%00010000
		.byte	%10010100
		.byte	%11011100
		.byte	%00111001
		.byte	%01100110
		
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

	ORG $FF43

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
				
	ORG $FF5B	

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

	org $FFFC
	.word Start
	.word Start     


