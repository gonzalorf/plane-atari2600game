;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Comenzado-Started: 10/may/2001 - 18:00 Hs - Córdoba
;
;Gonzalo Fernández
;

	processor 6502
	include VCS.H

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Variables

    	SEG.U vars
    	org $0080
    	
;background variables
moveBkGround		ds 1		;height of the Background bars

;plane variables
planePtr		ds 2		;Pointer to aeroplane shape
planeColorPtr		ds 2		;Pointer to plane color
planeSpeed		ds 1
planeMovem		ds 1

;object 1, 2 and 3 variables
obj1Ptr			ds 2		
objColor1Ptr		ds 2
obj2Ptr			ds 2		
objColor2Ptr		ds 2
obj3Ptr			ds 2		
objColor3Ptr		ds 2

;objects info
object1			ds 1
object2			ds 1
object3			ds 1
;22
;all vertical positions
verPosPlane		= 14		
verPosM0		ds 1		
verPosM0a		ds 1		
verPosM0b		ds 1		
verPosM1		ds 1
verPosPF		ds 1
verPosObj1		ds 1
verPosObj2		ds 1
verPosObj3		ds 1
verPosCurrObj 		ds 1		

;all horizontal positions
horPosPlane		ds 1		
horPosObj1		ds 1
horPosObj2		ds 1		
horPosObj3		ds 1
horPosM0		ds 1
horPosM0a		ds 1
horPosM0b		ds 1
horPosM1		ds 1
horPosCurrObj 		ds 1
;40
;missile variables
blnM0a		ds 1		;Boolean: 1=M0 shooted 0=M0 ready to be shooted
blnM0b		ds 1		;Boolean: 1=M0 shooted 0=M0 ready to be shooted
blnM1		ds 1

;misc variables
rand			ds 2
pFShape			ds 1
fCounter		ds 1
currCTRLPF		ds 1

;score
score		ds 4
score1Ptr	ds 2
score2Ptr	ds 2
score3Ptr	ds 2
score4Ptr	ds 2
scoreTmp	ds 1

;vars used only during the kernel
graphP1			ds 1
colorP1			ds 1
branch			ds 1
currObj			ds 1
currObjPtr		ds 2
currObjColorPtr		ds 2

;vars used only during the overscan/vertblank
flyLow			ds 1
intSpeed		ds 1
fraSpeed		ds 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Program. Clear Memory
    	SEG  código
        org  $F000

Start   sei
	cld
	ldx #$FF
	txs
	lda #$00

Clrstk  sta $00,X
	dex
	bne Clrstk
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Initial Calculations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda PLANELPTR
	sta planePtr
	lda #>GRAPHICS
	sta planePtr+1	

	lda #74
	sta horPosPlane
	
	lda #<PLANECOLOR-2
	sta planeColorPtr
	lda #>PLANECOLOR
	sta planeColorPtr+1
	
	lda #$F0
	sta pFShape

	lda #76
	sta verPosPF

	lda #106
	sta verPosObj3
	sta horPosObj3
	
	lda #55
	sta horPosObj2

	lda #20
	sta horPosObj1
	sta horPosM1
	
	lda #$11
	sta rand+1
	lda #150
	sta rand

	jsr Random	
	
	lda #>GRAPHICS
	sta obj3Ptr+1
	sta obj2Ptr+1
	sta obj1Ptr+1	
	sta currObjPtr+1
	lda #>COLORS
	sta objColor3Ptr+1
	sta objColor2Ptr+1
	sta objColor1Ptr+1	
	sta currObjColorPtr+1
	lda #5	;SmallIsland
	sta object1
	sta object2
	sta object3

	lda #>NUMBER_GRAPHICS
	sta score1Ptr+1
	sta score2Ptr+1	
	sta score3Ptr+1
	sta score4Ptr+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Vertical Blank - 40*68=3040 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VerticalBlank
	lda #$02
	sta VBLANK
	sta VSYNC
	sta WSYNC
	sta WSYNC
	sta WSYNC
	lda #$00
	sta VSYNC
		
	lda #44
	sta TIM64T
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculations during Vertical Blank
	lda fCounter
	and #$01
	tay
	lda verPosM0a,Y
	sta verPosM0
	lda horPosM0a,Y
	sta horPosM0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Update pointers for plane and objects
;according to their vertical positions
	lda object3
	;and #%00001111
	lsr
	lsr
	lsr
	lsr
	tax

	clc
	lda SPRITESL,X 
	sbc verPosObj3
	adc #12
	sta obj3Ptr

	clc
	lda COLORSL,X  
	sbc verPosObj3
	adc #12
	sta objColor3Ptr 

	lda object2
	;and #%00001111
	lsr
	lsr
	lsr
	lsr
	tax

	clc
	lda SPRITESL,X    
	sbc verPosObj2
	adc #12
	sta obj2Ptr

	clc
	lda COLORSL,X
	sbc verPosObj2
	adc #12
	sta objColor2Ptr 
	
	lda object1
	;and #%00001111
	lsr
	lsr
	lsr
	lsr
	tax

	clc
	lda SPRITESL,X 
	sbc verPosObj1
	adc #12
	sta obj1Ptr

	clc
	lda COLORSL,X  
	sbc verPosObj1
	adc #12
	sta objColor1Ptr 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Main Kernel data

	lda #$0F
	sta branch

	lda #%10000001
	sta currObj	

	lda verPosObj3
	sta verPosCurrObj 
	
	lda horPosObj2
	sta horPosCurrObj 

	lda obj3Ptr
	sta currObjPtr
	lda objColor3Ptr
	sta currObjColorPtr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Generate a new Island

NewPlayfield
	lda verPosPF
	and #$FE
	cmp #$A4	;did the island leave the screen?
	bne ExitPF
	
	lda #76		;if yes, generate a new island
	sta verPosPF
	jsr Random
	lda rand
	and #%00000011
	beq PF00
	cmp #1
	beq PF01
	cmp #2
	beq PF02
PF03	lda #%00000000		;isla grande, reflejado, PF0 y PF1
	sta pFShape
	lda #1
	jmp PFShapeDone
PF02	lda #%00000001		;isla grande,reflejado, PF2
	sta pFShape
	lda #1
	jmp PFShapeDone
PF01	lda #%10000001		;isla chica, reflejado, PF1
	sta pFShape
	lda #1
	jmp PFShapeDone
PF00	lda #%10000000		;isla chica,no reflejado, PF1
	sta pFShape
	lda #0
PFShapeDone
	sta currCTRLPF
ExitPF
	lda currCTRLPF
	sta CTRLPF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set Score Pointers

	ldx score
	lda NUMBERSL,x
	sta score1Ptr
	ldx score+1
	lda NUMBERSL,x
	sta score2Ptr
	ldx score+2
	lda NUMBERSL,x
	sta score3Ptr
	ldx score+3
	lda NUMBERSL,x
	sta score4Ptr


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;End Vertical Blank	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VblankLoop	
	lda INTIM
	bne VblankLoop
	sta VBLANK		;	Turn off Vertical Blank

	jmp KERNEL	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;KERNEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ORG $F200
KERNEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TOP KERNEL

	lda #23		     	;2	
        sta TIM64T  		;4	

        lda #255
        sta COLUP0
        sta COLUP1

	and #0
	sta REFP1
	sta NUSIZ1
	sta WSYNC
	ldx #23
loop1   dex
        bne loop1
        sta RESP0

        sta WSYNC
        ldx #23
loop2   dex
        bne loop2
        nop
        sta RESP1
        sta HMCLR
        lda #%10100000
        sta HMP1
        lda #%11000000
        sta HMP0
        sta WSYNC
        sta HMOVE
	
	ldy #5
Score	lda (score1Ptr),y
	and #$0F
	sta scoreTmp
        lda (score2Ptr),y
	and #$F0
	ora scoreTmp
        sta WSYNC
        sta GRP1

        lda (score3Ptr),y
	and #$0F
	sta scoreTmp
        lda (score4Ptr),y
	and #$F0
	ora scoreTmp
        sta GRP0
	dey
	bne Score
	sta WSYNC
	lda #0
	sta GRP0
	sta GRP1

	jsr PosHorMain

TopLoop
	lda INTIM
       	bne TopLoop
;	sta WSYNC	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MAIN KERNEL

	ldy #76   		;2	76 x 2 = 140 scanlines of main Kernel
	lda #$FA
	sta COLUPF
	sta HMCLR

	lda #%10000000
	sta HMM1	;adjust horizontal positions
	sta HMM0

	lda object3
	and #%11001111
	sta REFP1
	sta NUSIZ1

	lda #0
	sta COLUP0
	lda #$10
	sta NUSIZ0

	jmp Line1

	ORG $F700
	
NoPF	nop	;don't draw PF, but keep processing anyway
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	jmp PFDone
	
Line1
	lda moveBkGround
	sta WSYNC
ContL1				;ContL1 used to return from Reposition
	lxa #%00010000

	lda RIVERCOLOR,X	;4
	sta COLUBK		;3

	tya
	sbc verPosPF
	bmi NoPF
	lsr
	lsr
	tax	;22

	lda pFShape
	beq PFSh2
	bmi PFSh1
	lda ISLAND,X
	sta PF2 
	jmp PFDone	;26

PFSh1	lda ISLAND2,X ;ISLAND2
	sta PF1
	jmp PFDone	;26
	
PFSh2	lda ISLAND,X ;ISLAND2
	sta PF1
	sta PF0		;24
	nop		;26

PFDone
	ldx ENAM1
	txs		
	cpy verPosM1
	php		
	cpy verPosM0
	php
	
	lda branch	;Riddles in the dark... what to do, what to do...
	bmi Reposition
	beq UpdateP1

	lda graphP1	;preload data
	ldx colorP1
Line2
	sta WSYNC

	sta GRP1	;store preloaded data
	stx COLUP1
	
	cpy #12
	bpl NoPlane
	lda (planePtr),Y
	sta GRP0
	lda (planeColorPtr),Y
	sta COLUP0
NoPlane
	clc
	tya
	sbc verPosCurrObj 
	adc #13
	bpl NoChObj
	lda currObj
	sta branch
NoChObj	bcc NoObj
	lda (currObjPtr),Y
	sta graphP1		;store data for late use. If used here, crapy image
	lda (currObjColorPtr),Y
	sta colorP1
ObjDone

	dec moveBkGround	;update river data
	dey
	bne Line1
	jmp ExitKernel

Reposition			;reposition P1 in the middle of screen, twice per frame.
	ldx horPosCurrObj 	;Calcs from the previous scanline
	dec moveBkGround
	dey

	sta WSYNC
        lda HorzTable+35,X   	;4
	
        sta HMP1          	;3
        and #$0F            	;2
        tax                 	;2
Waste5Cycles
	dex                 	
	bne Waste5Cycles
        sta RESP1

	stx branch	;X=0. Means that the next "riddle" will result in a branch to UpdateP1
	
	sta WSYNC
	;sta HMOVE not here
	lda moveBkGround
	jmp ContL1	;do not go to Line1. We did part of its job, let's skip some lines


NoObj			;bad luck, graphic registers. Zero to you!
	lda #$00	
	sta graphP1	
	sta GRP1
	beq ObjDone	



UpdateP1	;Update pointers and positions of P1
	;sta WSYNC ja! not anymore! precious cycles
		
	sta HMOVE
	dec moveBkGround
	
	;sta WSYNC	in your
	;sta HMOVE	dreams, expensive instructions
	
	lda #$0F
	sta branch	
	
	dec currObj
	bmi SndObj

FstObj
	lda obj1Ptr
	sta currObjPtr
	lda objColor1Ptr
	sta currObjColorPtr
	lda objColor1Ptr+1
	sta currObjColorPtr+1
	
	lda object1
	and #%11001111
	sta REFP1
	sta NUSIZ1
	
	lda verPosObj1
	sta verPosCurrObj 
	dey
	jmp Line1
	
SndObj
	lda horPosObj1
	sta horPosCurrObj 		

	lda obj2Ptr
	sta currObjPtr
	lda objColor2Ptr
	sta currObjColorPtr
	lda objColor2Ptr+1
	sta currObjColorPtr+1
	
	lda object2
	and #%11001111
	sta REFP1
	sta NUSIZ1

	lda verPosObj2
	sta verPosCurrObj 

	dey
	jmp Line1

ExitKernel		;end main kernel

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BOTTOM KERNEL

	sta WSYNC
	lda #$00
	sta PF2
	sta PF1
	sta PF0
	sta GRP1
	sta COLUBK
	sta graphP1
	ldx #$FF		;Restore Stack Value
	txs	
	sec
	lda moveBkGround
	sbc #20			;adjust the ending value
	sta moveBkGround
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	lda #38		     	;2	
        sta TIM64T  		;4	
	
	sta WSYNC
	sta WSYNC
	sta WSYNC	
	lda #$FF
	sta PF2
	sta COLUPF
	lda #1
	sta CTRLPF
	sta WSYNC
	sta WSYNC
	sta WSYNC	
	sta WSYNC
	sta WSYNC	
	lda #$00
	sta PF2
	sta WSYNC
	sta WSYNC
	sta WSYNC	
	lda #$FF
	sta PF2
	sta COLUPF
	sta WSYNC
	sta WSYNC	
	sta WSYNC
	sta WSYNC
	sta WSYNC	
	lda #$00
	sta PF2


BottLoop
	lda INTIM
       	bne BottLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	lda #$02		;2
	sta VBLANK		;3
	lda #35		     	;2	30 lines * 76 cycles per line / 64 timercycles
        sta TIM64T  		;4	=35,625 wich means ~2280 cycles to use

	inc fCounter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Joystick routine

ReadJoy
	lda SWCHA		
	sec
	rol
	bcs NoRight
	jsr Right
NoRight	rol
	bcs NoLeft
	jsr Left
NoLeft	rol
	bcs NoDown
	jsr Down
NoDown	rol
	bcs NoUp
	jmp Up
NoUp
	lda planeSpeed
	beq JoyDone
	dec planeSpeed
	
	jmp JoyDone
	
Right	tax
	lda horPosPlane
	and #$FE
	cmp #158
	beq RightDone		

	clc
	lda planeMovem
	adc #140
	sta planeMovem
	lda horPosPlane
	adc #1
	sta horPosPlane
RightDone
	txa
	sec
	rts
Left
	tax
	lda horPosPlane		
	and #$FE
	cmp #24			
	beq LeftDone
	
	sec
	lda planeMovem
	adc #140
	sta planeMovem
	lda horPosPlane
	sbc #1
	sta horPosPlane
LeftDone
	txa
	sec
	rts

Down	inc flyLow
	
DownDone
	sec
	rts

Up	lda planeSpeed
	cmp #40
	beq UpDone
	inc planeSpeed
UpDone

JoyDone

	lda #0		;Get the proper speed of plane
	sta intSpeed
	lda planeSpeed
	lsr
	lsr
	lsr
	tax		;speed changes every 8 frames

	clc
	lda FRASPEEDTABLE,X
	adc fraSpeed
	sta fraSpeed
	lda INTSPEEDTABLE,X
	adc #0
	tay
	
RepeatMov
	beq MovDone	
	dec verPosPF
	dec verPosObj3
	dec verPosObj2
	dec verPosObj1
	inc moveBkGround
	dey
	jmp RepeatMov
MovDone

;M0 movement
	lda fCounter
	and #$01
	tax
	eor #$01
	tay

JoyButton		
	lda INPT4		;	Read joy 0 button
	bmi ButtonDone
	lda blnM0a,X
	bne ButtonDone
	lda blnM0a,Y
	beq ShootM0
	lda verPosM0a,Y
	cmp #27
	bmi ButtonDone
ShootM0
	lda #1
	sta blnM0a,X		;@-> optimizar usando la misma variable para blnM0 y verPosM0
	lda #10
	sta verPosM0a,X
	lda horPosPlane
	sec
	sbc #12
	sta horPosM0a,X
ButtonDone

M0Mov
	lda blnM0a,X		;	Misile0 movement (only if blnM0 is 1)
	beq M0MovDone
	inc verPosM0a,X
	inc verPosM0a,X
;	inc verPosM0a,X	
	lda verPosM0a,X
	cmp #78			;	Vertical limit M0
	bmi M0MovDone
	lda #0
	sta verPosM0a,X
	sta blnM0a,X
	lda horPosPlane
	sbc #12			;	Place Misile in the middle of plane shape
	sta horPosM0a,X
M0MovDone	

	
;Plane animation
	ldx flyLow
	lda PLANELPTR,X
	sta planePtr
	lda #0
	sta flyLow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Check if it's time to change
;the objects graphics and positions
	clc	
	lda verPosObj1
	bmi Switch
	jmp NoSwitch
Switch
	adc #90
	sta verPosObj3
	sbc #30
	sta verPosObj2
	sbc #30
	sta verPosObj1
	
	lda horPosObj2
	sta horPosObj1
	lda horPosObj3
	sta horPosObj2

	jsr Random	;generate horPos for Obj3
	lda rand
	cmp #140
	bcc OkHorRnd
	and #%01111111
OkHorRnd
	sta horPosObj3
	
	lda object2
	sta object1
	lda object3
	sta object2

	clc	;comprobar si el nuevo objeto está sobre una isla
	lda verPosPF
	sbc #88
	adc #96
	bpl InPF
	
	;jmp InPF
;	
NotInPF	jsr Random
	lda rand
	and #%00001111
	ora #%00001000
	tax
	jmp conti
InPF	
	jsr Random
	lda rand
	and #%00000111
	tax
	cmp #3
	bpl conti
	
	lda pFShape
	beq ObjInPF03
	cmp #1
	beq ObjInPF02
	cmp #%10000001
	beq ObjInPF01

ObjInPF00	
	lda horPosObj3
	cmp #80
	bpl bigg00
	lda #30	
	sta horPosObj3
	jmp conti	
bigg00	lda #110
	sta horPosObj3	
	jmp conti	
	
ObjInPF02
	lda #76
	sta horPosObj3
	jmp conti		
	
ObjInPF01	
	lda horPosObj3
	cmp #80
	bpl bigg01
	lda #30	
	sta horPosObj3
	jmp conti	
bigg01	lda #124
	sta horPosObj3	
	jmp conti	
	

ObjInPF03
	lda horPosObj3
	cmp #80
	bpl bigg03
	lda #12	
	sta horPosObj3
	jmp conti	
bigg03	lda #140
	sta horPosObj3	
	jmp conti	


conti	
	lda OBJECTS,X
	sta object3

NoSwitch

	jsr Random
		
OverScanLoop
	lda INTIM
       	bne OverScanLoop
       	jmp VerticalBlank	;Start the new screen

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PosHorMain
	ldy #0	;RESP0		;Horizontal positions in the Main Kernel
	ldx horPosPlane
	jsr PosPlayer
	iny	;RESP1
	ldx horPosObj3
	jsr PosPlayer
	iny	;RESM0
	ldx horPosM0
	jsr PosPlayer
	iny 	;RESM1
	ldx horPosM1
	jsr PosPlayer
	sta WSYNC
	sta HMOVE
	rts
	
Random	lda	rand	;Random generator from Dragon Fire (a little tribute)
	ror
	ror
	ror
	eor	rand+1
	adc #%01010101	;oh, my own modification
	asl
	asl
	rol	rand
	rol	rand+1
	lda	rand
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FRASPEEDTABLE
	.byte #165, #205, #230, #50, #85, #110

INTSPEEDTABLE
	.byte #0, #0, #0, #1, #1, #1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ORG $FC00
NUMBERSL	.byte	#<ZERO,	#<ONE, #<TWO, #<THREE, #<FOUR, #<FIVE, #<SIX, #<SEVEN, #<EIGHT, #<NINE

NUMBER_GRAPHICS
ZERO	.byte 	#%00000000
	.byte	#%11101110
	.byte	#%10101010
	.byte	#%10101010
	.byte	#%10101010
	.byte 	#%11101110

ONE	.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%01000100
	.byte 	#%01000100
	.byte 	#%11001100
	.byte 	#%01000100

TWO	.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%10001000
	.byte 	#%11101110
	.byte 	#%00100010
	.byte 	#%11101110

THREE	.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%00100010
	.byte 	#%11101110
	.byte 	#%00100010
	.byte 	#%11101110

FOUR	.byte 	#%00000000
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%10101010

FIVE	.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%00100010
	.byte 	#%11101110
	.byte 	#%10001000
	.byte 	#%11101110

SIX	.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%11101110
	.byte 	#%10001000
	.byte 	#%10001000

SEVEN	.byte 	#%00000000
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%11101110

EIGHT	.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%11101110

NINE	.byte 	#%00000000
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%11101110

	ORG $FD00

OBJECTS		.byte	#%00000000, #%00010000, #%00100000, #%00110000, #%01000000, #%01010111, #%01100000, #%01111000
		.byte	#%10001101, #%10010101, #%10100001, #%10111000, #%11001000, #%11011101, #%11100000, #%11110101	
	
SPRITESL	.byte	#<ISLANDER, 		#<PALMERA, 		#<CHEST, 		#<JET, 			#<FLYINGBOMB, 		#<SMALLISLAND, 		#<DOUBLE, 		#<FLYINGBOMB
		.byte	#<SUBMARINE,		#<SUBMARINE, 		#<JET, 			#<JET, 			#<FLYINGBOMB,		#<SMALLISLAND,		#<BLINDAGE,		#<SMALLISLAND

COLORSL		.byte	#<ISLANDERCOLOR, 	#<PALMERACOLOR, 	#<CHESTCOLOR, 		#<JETCOLOR, 		#<FLYINGBOMBCOLOR, 	#<SMALLISLANDCOLOR, 	#<DOUBLECOLOR, 		#<FLYINGBOMBCOLOR
		.byte	#<SUBMARINECOLOR,	#<SUBMARINECOLOR,	#<JETCOLOR,		#<JETCOLOR,		#<FLYINGBOMBCOLOR,	#<SMALLISLANDCOLOR,	#<BLINDAGECOLOR,	#<SMALLISLANDCOLOR
		
PLANELPTR	.byte	#<PLANE-2, #<PLANEDOWN-2

RIVERCOLOR	.byte #%10101010
		
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		.byte #$00
		
		.byte #%10101000
	
	ORG $FD5E
COLORS
PLANECOLOR		.byte	$02,$02,$02,$90,$90,$90,$90,$90,$90,$90,$90
PALMERACOLOR    	.byte   $00,$00,$2A,$2A,$2A,$20,$20,$20,$20,$D4,$D4,$D4,$D4
JETCOLOR    		.byte  	$00,$04,$04,$04,$04,$00,$00,$00,$50,$50,$50,$50,$50
FLYINGBOMBCOLOR		.byte  	$00,$04,$04,$04,$04,$00,$00,$00,$00,$02,$04,$06,$0A
SMALLISLANDCOLOR	.byte  	$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA
SUBMARINECOLOR		.byte  	$04,$04,$04,$04,$04,$0F,$0F,$0F,$0F,$00,$02,$04,$04
CHESTCOLOR 		.byte  	$00,$04,$04,$04,$04,$00,$00,$20,$20,$22,$00,$26,$28
ISLANDERCOLOR   	.byte  	$00,$00,$00,$00,$29,$29,$29,$45,$45,$45,$29,$22,$29
DOUBLECOLOR		.byte  	$00,$04,$04,$04,$04,$00,$7A,$7A,$78,$74,$74,$72,$70
BLINDAGECOLOR		.byte  	$00,$04,$04,$04,$04,$00,$4A,$48,$46,$44,$42,$40,$40

	ORG $FE00
GRAPHICS
ISLAND		.byte	%00000000
		.byte	%10000000		
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
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000

ISLAND2		.byte	%00000000
		.byte	%00011000
		.byte	%00111100
		.byte	%01111100
		.byte	%01111110
		.byte	%01111110 
		.byte	%11111111
		.byte	%11111111
		.byte	%11111110
		.byte	%01111110
		.byte	%01111111
		.byte	%00111110
		.byte	%00011100
		.byte	%00111100
		.byte	%00111110
		.byte	%01111111
		.byte	%01111110
		.byte	%11111100
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
		.byte	%00000000

PLANEDOWN	.byte	%01000010
		.byte	%00111100
		.byte	%00011000
		.byte	%10000001
		.byte	%11011011 
		.byte	%11111111
		.byte	%01100110
		.byte	%00111100
		.byte	%00011000	
		.byte	%00000000

PLANE		.byte	%00100100
		.byte	%00011000
		.byte	%00000000
		.byte	%00000000
		.byte	%10000001
		.byte	%11011011 
		.byte	%11111111
		.byte	%01100110
		.byte	%00111100
		.byte	%00011000
		
DOUBLE		.byte	%00000000		
		.byte	%00000000
		.byte	%00001000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00101010
		.byte	%00011100
		.byte	%00001000
		.byte	%00000000

PALMERA		.byte	%00000000		
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
	
BLINDAGE	.byte	%00000000		
		.byte	%00000000
		.byte	%00111000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%01111100
		.byte	%11001110
		.byte	%11010110
		.byte	%11001110
		.byte	%11010110
		.byte	%11001110
		.byte	%01111100

JET		.byte	%00000000		
		.byte	%00000000
		.byte	%00010000		
		.byte	%01111000
		.byte	%00010000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00111000
		.byte	%10011100
		.byte	%11110111
		.byte	%10011100
		.byte	%00111000

	
FLYINGBOMB	.byte	%00000000		
		.byte	%00011000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%01111101
		.byte	%11111110
		.byte	%11111110
		.byte	%01111101
		.byte	%00000000

SUBMARINE	.byte	%00000000		
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%00110000
		.byte	%00010000
		.byte	%00000000


SMALLISLAND	.byte	%00000000		
		.byte	%00011100
		.byte	%01111111
		.byte	%11111111
		.byte	%01111110
		.byte	%00111100
		.byte	%00111000
		.byte	%11111110
		.byte	%11111111
		.byte	%01111111
		.byte	%01111100
		.byte	%00111100
		.byte	%00111000

CHEST		.byte	%00000000		
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%11111111
		.byte	%11100111
		.byte	%11111111
		.byte	%01111110
		
ISLANDER	.byte	%00000000		
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
		.byte	%00001100
		.byte	%00000000
		
	
	ORG $FF3C

HorzTable    ;this must not cross a page boundary
	    .byte $30,$20,$10,$00,$F0,$E0,$D0,$C0
	    .byte $B0,$A0,$90
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
	    .byte $7C,$6C,$5C,$4C,$3C,$2C,$1C,$0C,$FC,$EC,$DC,$CC,$BC,$AC,$9C

	org $FFFC
	.word Start
	.word Start     


