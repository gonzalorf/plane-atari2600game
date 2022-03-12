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

;sprite 1 and 2 are made with player1 (GRP1), they can be shown in the same screen in different vert. pos.
sprite1Ptr		ds 2		;pointer to higher placed sprite on screen
spriteColor1Ptr		ds 2
sprite2Ptr		ds 2		;pointer to lower placed sprite on screen
spriteColor2Ptr		ds 2

;all vertical positions
VerPosPlane		ds 1		;Vertical Position aeroplane
VerPosM0		ds 1		;Vertical Position of Misiles
VerPosM1		ds 1
VerPosPF		ds 1
VerPosSprite1		ds 1
VerPosSprite2		ds 1

;all horizontal positions
HorPosPlane		ds 1		
HorPosSprite1		ds 1
HorPosM0		ds 1
HorPosM1		ds 1
HorPosBl		ds 1
HorPosSprite2		ds 1		;only accept values between 27 & 160 (decimal)

;missile variables
M0Control		ds 1		;Boolean: 1=M0 shooted 0=M0 ready to be shooted
flyLow			ds 1		;if plane is flying low

fcounter		ds 1
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

;	lda #13				;and Vertical position of the aeroplane
;	sta VerPosPlane

;	clc
;	lda #<plane-2			;Set pointers to aeroplane shape
;	sta planePtr
;	lda #>plane
;       sta planePtr+1

	lda #70
	sta HorPosPlane
	adc #4
	sta HorPosM0
	
 	clc
	lda #<planeColor-2		;Set pointers to aeroplane color
	sta planeColorPtr
	lda #>planeColor
	sta planeColorPtr+1
	
	lda #$01			;Reflct Playfield
	sta CTRLPF

	lda #$FF
	sta fcounter

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
	
	lda avionColor+14	;to avoid some cylce counting problem
	sta COLUP1		;that updates the COLUP1 in the middle of the scanline
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Dibujar pantalla

;There's 3 sections in the kernel: k1, k2 and k3.
;Each section have a first line and a second line.
;The second line is the same in the three sections
;and is the line where the plane, background and misiles are drawn.
;In the first line we draw the enemies obstacles and people to rescue
;using sprite 1 and sprite 2, and the PF too (islands). The firt line
;of k1 checks if it's time to draw sprite and draw it. Also, it's
;checking if (VerPosSprite1 > Y) and if it's true it means that it's time
;to jump to k2. In k2 player 1 is re-positioned with HorPosSprite2, then it jumps
;to k3 where the sprite 2 is drawn.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

k1
;first line of k1
	sta WSYNC			;3
	clc				;2
	tya				;2
	sbc VerPosSprite1		;3	
	adc #14				;2	
	bmi swap			;2
	bcc noSprite1_A			;2
	bne noJmp			;2
	jmp k2				;3
noJmp	
	lda (sprite1Ptr),Y      	;5 
	sta GRP1			;3
	lda (spriteColor1Ptr),Y       	;5 
	sta COLUP1			;3
sprite1Done_A

	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc noPF_A		;2	
	lsr			;2
	lsr			;2
	tax			;2
	lda island,x		;4
	nop			;free cycles
	nop
	nop
	nop
	nop			;free cycles
	nop
	nop
	nop			;free cycles
	sta PF2
pfDone_A

;second line of k1
	sta WSYNC		;3	
	clc			;2
	tya			;2
	sbc #13 		;3	Check if it's time to draw the aeroplane
	adc #11			;2
	bcc noPlane_A		;2
	lda (planePtr),Y        ;5 
	sta GRP0		;3
	lda (planeColorPtr),Y	;5
	sta COLUP0		;3
	
planeDone_A
	
	ldx #ENAM0		;2	Misile0 Routine
	txs			;2
	cpy VerPosM0		;3
	php			;3
	
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

;end 2nd scanline of k1
f3	nop			;free cycles
	nop
	nop
	nop
	nop			;free cycles
	nop
	dey			;2
	bne k1			;2	Go back for a new scanline
	jmp exitKernel		;3

noPlane_A
	lda #$00		;2
	sta GRP0		;3
	beq planeDone_A		;2

noPF_A	lda #$00		;2
	sta PF2			;3
	beq pfDone_A		;2

noSprite1_A
	lda #$00		;2
	sta GRP1		;3
	beq sprite1Done_A	;2

noPF_B	lda #$00		;2
	sta PF2			;3
	beq pfDone_B		;2

swap	;this section is part of k1, and it's used when there's no need to draw sprite 1
	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc noPF_B		;2
	lsr			;2
	lsr			;2	
	tax			;2
	lda island,x		;4
	nop
	nop			;free cycles
	nop
	nop
	nop			;free cycles
	nop
	nop
	nop
	nop			;free cycles
	nop
	nop
	nop
	nop			;free cycles
	nop
	nop
	nop
	nop			;free cycles
	nop
	sta PF2
pfDone_B


k2		
;first line of k2
	sta WSYNC		;3	
	clc			;2
	tya			;2
	sbc #13 		;3	Check if it's time to draw the aeroplane
	adc #11			;2
	bcc noPlane_B		;2
	lda (planePtr),Y        ;5 
	sta GRP0		;3
	lda (planeColorPtr),Y	;5
	sta COLUP0		;3
	
planeDone_B
	
	ldx #ENAM0		;2	Misile0 Routine
	txs			;2
	cpy VerPosM0		;3
	php			;3
	
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

;end 2nd scanline of k2
f6	nop			;free cycles
	nop
	nop
	nop			;free cycles	
	nop
	dey			;2
	bne sndLineK2		;2	Go back for a new scanline
	jmp exitKernel		;3

noPlane_B
	lda #$00		;2
	sta GRP0		;3
	beq planeDone_B		;2

sndLineK2	

;second line of k2 (re-position player 1)
	ldx HorPosSprite2	;3
	sta WSYNC         	;begin oscanline
        lda HorzTable+12,X   	;4
        sta HMP1          	;3
        and #$0F            	;2
        tax                 	;2
P02     dex                 	
        bpl P02              	
        sta RESP1         	;new position
;I'll use some cycles to update de NUSIZ1 register, if it's needed
	sta WSYNC
	sta HMOVE
	jmp f7		  	;jump to second line of k3

k3

;first line of k3
	sta WSYNC		;3
	clc			;2
	tya			;2
	sbc VerPosSprite2	;3		
	adc #14			;2	
	bcc noSprite1_C		;2
	lda (sprite2Ptr),Y      ;5 
	sta GRP1		;3
	lda (spriteColor2Ptr),Y ;5      	;5 
	sta COLUP1		;3
p1Done_C

	clc		    	;2	
	tya			;2
	sbc VerPosPF		;3	
	adc #88			;2
	bcc noPF_C		;2
	lsr			;2
	lsr			;2
	tax			;2
	lda island,x		;4
	nop
	nop			;free cycles
	nop
	nop
	nop
	nop			;free cycles
	nop
	nop
	nop
	nop			;free cycles
	nop
	sta PF2			;3
pfDone_C

;second line of k3
	sta WSYNC		;3	
f7	clc			;2
	tya			;2
	sbc #13 		;3	Check if it's time to draw the aeroplane
	adc #11			;2
	bcc noPlane_C		;2
	lda (planePtr),Y        ;5 
	sta GRP0		;3
	lda (planeColorPtr),Y	;5
	sta COLUP0		;3
	
planeDone_C
	
	ldx #ENAM0		;2	Misile0 Routine
	txs			;2
	cpy VerPosM0		;3
	php			;3
	
	dec banda               ;5	Changing Background color
        beq f9                  ;2³	
        bpl f8                  ;2³	Thanks to T.J.
        lda #$0F                ;2
        sta banda               ;3
        jmp f10                 ;3
         
f8      lda colorBk1            ;3
        sta COLUBK              ;3
        jmp f10                 ;3

f9      lda #%00000010          ;2
        eor colorBk1            ;3
        sta colorBk1            ;3

	
;end 2nd line of k3
f10	nop			;free cycles
	nop
	nop
	dey			;2
	bne k3			;2	Go back for a new scanline
	jmp exitKernel		;3

noSprite1_C
	lda #$00		;2
	sta GRP1		;3
	beq p1Done_C		;2

noPlane_C
	lda #$00		;2
	sta GRP0		;3
	beq planeDone_C		;2

noPF_C	lda #$00		;2
	sta PF2			;3
	beq pfDone_C		;2

exitKernel			;end main kernel

	sta WSYNC
	lda #$00
	sta PF2
	sta GRP1
	sta COLUBK
	ldx #$FF		;	Restore Stack Value
	txs		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	ldy #15			;2
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
	
;the 8 kinds of movement
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
	
;vert. pos. of the sprites are function of the PF vert. pos. 
	dec VerPosPF
	clc
	lda VerPosPF
	sbc #20
	sta VerPosSprite1
	sbc #30
	sta VerPosSprite2
	
;add some movement to sprite 1, just to see it
	inc HorPosSprite2
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
	lda #<avion        ; Init Pointers
	sbc VerPosSprite2
	adc #14
	sta sprite2Ptr
	lda #>avion        ; Init Pointers
	sta sprite2Ptr+1

	clc
	lda #<avionColor   ; Init Pointers
	sbc VerPosSprite2
	adc #14
	sta spriteColor2Ptr 
	lda #>avionColor        ; Init Pointers
	sta spriteColor2Ptr+1
	

	clc
	lda #<palmera        ; Init Pointers
	sbc VerPosSprite1
	adc #14
	sta sprite1Ptr
	lda #>palmera        ; Init Pointers
	sta sprite1Ptr+1

	clc
	lda #<palmeraColor   ; Init Pointers
	sbc VerPosSprite1
	adc #14
	sta spriteColor1Ptr 
	lda #>palmeraColor        ; Init Pointers
	sta spriteColor1Ptr+1

oscan   lda INTIM	;4	;	Finish the overscan
       	bne oscan	;2
       	jmp vblank	;3	;	Start the new screen

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
	
;		ORG $FD40
		
planeColor	.byte	$00,$04,$04,$04,$90,$90,$90,$90,$90,$90,$90,$90

planeHPtr	.byte	#>plane, #>planeDown		;plane is always in the same Vert. Pos.
planeLPtr	.byte	#<plane-2, #<planeDown-2
		

palmeraColor    .byte   $00,$00,$00,$00,$2A,$2A,$2A,$20,$20,$20,$20,$D4,$D4,$D4,$D4

avionColor    	.byte  	$00,$00,$00,$04,$04,$04,$04,$00,$00,$00,$50,$50,$50,$50,$50



palmera		.byte	%00000000		;player1
		.byte	%00000000
		.byte	%00000000
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
		.byte	%00000000
		.byte	%00000000
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

;	ORG $FF43

plane		.byte	%00000000		;player0
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
		
planeDown	.byte	%00000000		;player0
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


