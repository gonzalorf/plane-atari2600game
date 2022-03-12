;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Started: 10/05/2001 - 18:00 Hs
;
;      "Plane across the River Demo"
;          by Gonzalo Fernández
;
;I started this demo just for learning purposes.
;And I don't know if it will be a real game some day...
;
;The demo try to show a plane on a river, I'm making the movement
;effect only using the Background register, so I can use the playfied 
;for something else... 
;
;I think that the routine that makes the movement can be easier
;and it could be shorter, but I didn't find other way to do it.



	processor 6502
	include VCS.H
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Variables

    	SEG.U vars
    	org $80
    	
moveBk	 		ds 1 		;controls the Background movement
colorBk1		ds 1		;Var for store in COLUBK
colorBk2		ds 1		;Var for store in colorBk1 calculable during Overscan
banda			ds 1		;height of the Background bars
planePtr		ds 2		;Pointer to plane shape
VerPosPlane		ds 1		;Vertical Position plane
VerPosM0		ds 1		;Vertical Position of Misiles
VerPosM1		ds 1
HorPosPlane		ds 1		;Horizontal Positions
HorPosP1		ds 1
HorPosM0		ds 1
HorPosM1		ds 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    	SEG  código
        org  $F000


Start   SEI
	CLD
	LDX #$FF
	TXS
	LDA #$00

clrstk  STA $00,X
	DEX
	BNE clrstk
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculos

	lda #$AB			;Setting Background color (blue)
	sta colorBk2
	sta colorBk1
	lda #$10			;Initial height of the Background bars
	sta moveBk
	sta banda
	lda #76
	sta HorPosPlane			;Set Horizontal 
	lda #20				;and Vertical position of the plane
	sta VerPosPlane
	lda #10
	sta COLUP0
	lda #30
	sta VerPosM0
		
	clc
	lda #<plane			;Set pointers to plane shape
	sbc VerPosPlane
	adc #14
	sta planePtr
	lda #>plane
        sta planePtr+1
	lda #$01			;Reflct Playfield
	sta CTRLPF
	lda #$FF
	sta PF0
	
	lda #%00101100
	sta COLUPF
	
	
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
		
	lda #$44
	sta TIM64T
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculos

	
 	ldy #0          	;2
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
	sta VBLANK			;Turn off Vertical Blank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Draw Screen
	
	ldy #16
top	sta WSYNC
	sty COLUBK
	dey
	bne top
	
	
	ldy #159			;2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Dibujar pantalla


river	sta WSYNC		;3
	lda colorBk1		;3
	sta COLUBK		;3
	clc			;2
	tya			;2
	sbc VerPosPlane		;3	Check if is time to draw the plane
	adc #14			;2
	bcc Noplane		;2
	lda (planePtr),Y        ;5 
Continue
	sta GRP0		;3
		
	ldx #ENAM0		;	Misile Routine
	txs
	cpy VerPosM0
	php
	ldx #$FF
	txs
				;	  \	
	lda banda		;3	   |	
	cmp #$0F		;2	   |	This is for change the colors 
	bne f1			;2	   |	of the Background bars
	lda #%00000010		;2	   \
	eor colorBk1		;3	   /	Change the LUM part of the register
	sta colorBk1 		;3	   |
	lda #0			;2	   |
	sta banda		;3	   |
	jmp f2			;3	  /
f1	inc banda		;5
	
f2	dey			;2
	bne river		;2	Go back for a new oscanline
	jmp f3			;3

Noplane	lda #$00		;2
	beq Continue		;2
	
f3	ldy #16			;2
bottom	sta WSYNC		;3
	sty COLUBK		;3
	dey			;2
	bne bottom		;2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Start Overscan
	
	lda #$02		;2
	sta VBLANK		;3
	lda #34		     	;2	30 lines * 76 cycles per line / 64 timercycles
        sta TIM64T  		;4	=35,625 wich means ~2280 cycles to use

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculos

	
	
	
ReadJoy				;Primitive reading routine, I'll change it later
	lda SWCHA		;4
	cmp #$BF		;2
	beq left		;2
	cmp #$7F		;2
	beq right		;2
	cmp #$EF		;2
	beq up			;2
	cmp #$DF		;2
	beq down		;2
	jmp mov			;3
	
up      lda moveBk		;3
	cmp #$02		;2
	beq mov			;2
	dec moveBk		;5
	dec moveBk		;5
	jmp mov			;3

down	inc moveBk		;5
	jmp mov			;3
	
left    lda HorPosPlane		;3
	beq mov			;Do not dec HorPos if the plane 
	dec HorPosPlane		;is in the left limit (pixel 0)
	jmp mov

right   lda HorPosPlane
	cmp #152			;Do not inc HorPos if the plane 
	beq mov				;is in the right limit (pixel 152)
	inc HorPosPlane
	jmp mov


mov	dec moveBk	;5	;This variable is for manage the 
	dec moveBk	;5	;Background movement
	
	lda moveBk	;3
	cmp #$01	;2	
	bne mov1	;2	;A little correction, moveBk must not be 1, because
	dec moveBk	;5	;it will take values out of range in the next double dec moveBk.
mov1	lda moveBk	;3
	sta banda	;3	;This is the variable that makes the movement
	cmp #$00	;2
	bne oscan	;2
	lda #$10	;2
	sta moveBk	;3
	
	lda #%00000010	;2
	eor colorBk2	;3	;This changes Background color (LUM)
	sta colorBk1	;3
	sta colorBk2	;3
	
		
oscan   lda     INTIM	;4	;Finish the overscan
       	bne     oscan	;2
       	jmp     vblank	;3	;Start the new screen



PosPlayer
	sta WSYNC           ;begin oscanline
        lda HorzTable,X     ;+4  7  
        sta HMP0,Y          ;+3 10
        and #$0F            ;+2 12
        tax                 ;+2 14
P0      dex                 ;+2 16
        bpl P0              ;
        sta RESP0,Y         ;
        rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

plane		.byte	%00000000 
		.byte	%00001000
		.byte	%00011100 
		.byte	%00011100 
		.byte	%00001000 
		.byte	%00001000 
		.byte	%00001000 
		.byte	%00011100 
		.byte	%01111111
		.byte	%01111111
		.byte	%01111111
		.byte	%00011100
		.byte	%00001000
		.byte	%00001000 
		.byte	%00000000

	ORG $FF00
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
