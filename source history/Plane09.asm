;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Comenzado-Started: 10/may/2001 - 18:00 Hs - Córdoba
;
;Gonzalo Fernández
;

	processor 6502
	include VCS.H
	include Macro.h

NO_ILLEGAL_OPCODES = 0
TOP_TIMER	= 24	;1x lines
SCANLINES_RIVER	= 76	;76 x 2 = 152 scanlines
BOTTOM_TIMER	= 24	;2x lines
VBLANK_TIMER	= 44
OSCAN_TIMER	= 36
OBJECTS_H	= 13
PLANE_VER_POS	= 12

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
horPosCurrObj 		ds 1
;38
;missile variables
;blnM0a		ds 1		;Boolean: 1=M0 shooted 0=M0 ready to be shooted
;blnM0b		ds 1		;Boolean: 1=M0 shooted 0=M0 ready to be shooted

;misc variables
rand			ds 2
pFShape			ds 1
fCounter		ds 1
currCTRLPF		ds 1
;43

score1		ds 1 ;holds the score values
score2		ds 1 ;holds the score values

level1Ptr	ds 2
level2Ptr	ds 2
livesPtr	ds 2

score1RAM	ds 5 ;score graphics in RAM
score2RAM	ds 5
score1Ptr	ds 2
score2Ptr	ds 2

;67
scoreTmp	ds 1
livesLevel	ds 1	;Bits 7,6,5=lives. Rest=level

time	ds 1
km	ds 1

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
;84

;pruebas
pFI	ds 1
pFD	ds 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin Program. Clear Memory
    	SEG  código
        org  $F000

Start   
	CLEAN_START

;level/score/lives H_Pointers
	lda #>NUMBER_GRAPHICS
	sta level1Ptr+1
	sta level2Ptr+1	
	sta livesPtr+1
	sta scoreTmp+1
	sta score1Ptr+1
	sta score2Ptr+1		;21
;plane Pointers
	lda #>GRAPHICS
	sta planePtr+1	
	lda #<PLANECOLOR-2
	sta planeColorPtr
	lda #>PLANECOLOR
	sta planeColorPtr+1	;18
;objects H_Pointers
	lda #>GRAPHICS
	sta obj3Ptr+1
	sta obj2Ptr+1
	sta obj1Ptr+1	
	sta currObjPtr+1
	lda #>COLORS
	sta objColor3Ptr+1
	sta objColor2Ptr+1
	sta objColor1Ptr+1	
	sta currObjColorPtr+1	;30
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Initial Calculations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #74
	sta horPosPlane
	
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
	
	lda #$11
	sta rand+1
	lda #150
	sta rand

	jsr Random	
	
	lda #5	;SmallIsland
	sta object1
	sta object2
	sta object3


;init para pruebas
	lda #$FF
	sta pFI
	sta pFD
	
	lda #%01011110
	sta livesLevel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Vertical Blank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
VerticalBlank
	VERTICAL_SYNC
		
	lda #VBLANK_TIMER
	sta TIM64T
	
;Update pointers for objects
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


;Generate a new Island
NewPlayfield SUBROUTINE
	lda verPosPF
	and #$FE
	cmp #$A4	;did the island leave the screen?
	bne ExitPF
	
	lda #76		;if yes, generate a new island
	sta verPosPF
	jsr Random
	lda rand
	and #%00000011
	beq .PFSh0
	cmp #1
	beq .PFSh1
	cmp #2
	beq .PFSh2
.PFSh3	lda #%00000000		;isla grande, reflejado, PF0 y PF1
	sta pFShape
	lda #1
	jmp .PFDone
.PFSh2	lda #%00000001		;isla grande,reflejado, PF2
	sta pFShape
	lda #1
	jmp .PFDone
.PFSh1	lda #%10000001		;isla chica, reflejado, PF1
	sta pFShape
	lda #1
	jmp .PFDone
.PFSh0	lda #%10000000		;isla chica,no reflejado, PF1
	sta pFShape
	lda #0
.PFDone
	sta currCTRLPF
ExitPF

;Set Score Pointers
Level/LivesPointers
	clc
	ldy #0
	lda livesLevel
	and #%00011111
	cmp #10
	bmi .cont
	sec
.loop	sbc #10
	iny
	cmp #10
	bcs .loop
.cont	
	tax
	lda NUMBERSL,y
	sta level1Ptr
	lda NUMBERSL,x
	sta level2Ptr

	lda livesLevel
	lsr
	lsr
	lsr
	lsr
	lsr
	tax
	sec
	lda NUMBERSL,x
	sbc #1 ;correct pointer
	sta livesPtr

	lda #13
	sta score1
Score1 SUBROUTINE	;score1=first 2 digits	
	clc
	ldy #0
	lda score1
	cmp #10
	bmi .cont
	sec
.loop	sbc #10
	iny
	cmp #10
	bcs .loop
.cont	tax
	lda NUMBERSL,x
	sta score2Ptr
	lda NUMBERSL,y
	sta score1Ptr
	ldy #4
.copy	lda (score2Ptr),y
	and #$0F
	sta scoreTmp
	lda (score1Ptr),y
	and #$F0
	ora scoreTmp
	sta score1RAM,y
	dey
	bpl .copy
	
Score2 SUBROUTINE	;score1=second 2 digits
	clc
	ldy #0
	lda score2
	cmp #10
	bmi .cont
	sec
.loop	sbc #10
	iny
	cmp #10
	bcs .loop
.cont	tax
	lda NUMBERSL,x
	sta score2Ptr
	lda NUMBERSL,y
	sta score1Ptr
	ldy #4
.copy	lda (score2Ptr),y
	and #$0F
	sta scoreTmp
	lda (score1Ptr),y
	and #$F0
	ora scoreTmp
	sta score2RAM,y
	dey
	bpl .copy

	inc livesLevel

;End Vertical Blank	
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

	lda #TOP_TIMER		     	;2	
        sta TIM64T  		;4	

        lda #255
        sta COLUP0
        sta COLUP1

	lda #0
	sta REFP1
	sta NUSIZ1

PlaceDigits SUBROUTINE
	sta WSYNC
	ldx #6
.loop   dex
        bne .loop
        sta RESP0
        sta RESP1
	sta HMCLR
        lda #$E0
        sta HMP0
	lda #$F0
	sta HMP1
	lda #$06
	sta NUSIZ0
	sta NUSIZ1	

DrawScore SUBROUTINE
	ldy #5
	lda (livesPtr),y	;preload lives data
	and #$F0
	tax         			
	dey
        sta WSYNC
        sta HMOVE

.loop 	sta WSYNC	;3
        lda LEVEL,y	;4
        sta GRP0	;3
	lda (level1Ptr),y	;5
	and #$F0		;2
	sta scoreTmp		;3
        lda (level2Ptr),y	;5
	and #$0F		;2
	ora scoreTmp		;3   
        sta GRP1     		;3
	nop
	lda score1RAM,y
	sta GRP0
        lda score2RAM,y
	sta GRP1
        lda LIVES,y	;4
        sta GRP0	;3
	stx GRP1
	lda (livesPtr),Y	;5
	and #$F0		;2
	tax         			
	dey
        bpl .loop ;3 =  8

	sta WSYNC
	ldx #$FF
	txs
	lda #0
	sta GRP0
	sta GRP1

	jsr PosHorMain

	lda currCTRLPF
	sta CTRLPF
TopLoop
	lda INTIM
       	bne TopLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MAIN KERNEL
	ldy #SCANLINES_RIVER
	lda #$FA
	sta COLUPF
	sta HMCLR

	lda #%10000000
;	sta HMM1	;adjust horizontal positions
	sta HMM0

	lda object3
	and #%11001111
	sta REFP1
	sta NUSIZ1

	lda #0
	sta COLUP0
	lda #$00
	sta NUSIZ0

	jmp Line1

	ORG $F700
	
NoPF	SLEEP 20
	jmp DrawMissile
	
Line1 SUBROUTINE
	lda moveBkGround
	sta WSYNC
ContL1				;ContL1 used to return from Reposition
	and #%00010000
	tax
	
	lda RIVERCOLOR,X	;4
	sta.w COLUBK		;3

DrawPlayfield SUBROUTINE
	tya
	sbc verPosPF
	bmi NoPF
	lsr
	lsr
	tax	;22
	lda pFShape
	beq .PFSh2
	bmi .PFSh1
	lda ISLAND,X
	sta PF2 
	jmp .PFDone	;26

.PFSh1	lda ISLAND2,X ;ISLAND2
	sta PF1
	jmp .PFDone	;26
	
.PFSh2	lda ISLAND,X ;ISLAND2
	sta PF1
	sta PF0		;24
	nop		;26
.PFDone

DrawMissile SUBROUTINE
	tya
	sbc verPosM0
  	adc #4
  	rol
  	rol
  	sta ENAM0
	
	lda branch	;Riddles in the dark... what to do, what to do...
	bmi Reposition
	beq UpdateP1

	lda graphP1	;preload data
	ldx colorP1
Line2 SUBROUTINE
	sta WSYNC

	sta GRP1	;store preloaded data
	stx COLUP1
	
	cpy #PLANE_VER_POS
	bpl NoPlane
	lda (planePtr),Y
	sta GRP0
	lda (planeColorPtr),Y
	sta COLUP0
NoPlane
	clc
	tya
	sbc verPosCurrObj 
	adc #OBJECTS_H
	bpl NoChObj
	lda currObj
	sta branch
NoChObj	bcc NoObj
	lda (currObjPtr),Y
	sta graphP1		;store data for late use. If used here, crappy image
	lda (currObjColorPtr),Y
	sta colorP1
ObjDone

	dec moveBkGround	;update river data
	dey
	bne Line1
	jmp ExitKernel

Reposition SUBROUTINE			;reposition P1 in the middle of screen, twice per frame.
	ldx horPosCurrObj
	dec moveBkGround
	dey

	sta WSYNC
        lda HorzTable+35,X   	;4
        sta HMP1          	;3
        and #$0F            	;2
        tax                 	;2
.wait
	dex                 	
	bne .wait
        sta RESP1
	stx branch	;X=0. Means that the next "riddle" will result in a branch to UpdateP1
	sta WSYNC

	lda moveBkGround
	jmp ContL1	;do not go to Line1. We did part of its job, let's skip some lines

NoObj			;bad luck, graphic registers. Zero to you!
	lda #$00	
	sta graphP1	
	sta GRP1
	beq ObjDone	

UpdateP1 SUBROUTINE	;Update pointers and positions of P1
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

TimeBar SUBROUTINE
	lda #BOTTOM_TIMER     	;2	
        sta TIM64T  		;4	
	inc time
	lda time
	cmp #61
	bmi .cont1
	lda #0
	sta time
.cont1
	sta WSYNC
	ldy #4
	clc
	lda time	;range 0-60
	adc #49
	tax
	jsr PosPlayer

	sta WSYNC
	sta HMOVE

	clc
	lda time
	cmp #32
	bpl .pfD
	lsr
	lsr
	tax
	lda TIMETABLE_I,x
	sta pFI
	lda #$00
	sta pFD
	jmp .cont
.pfD	sbc #32
	lsr
	lsr
	tax
	lda #$FF
	sta pFI
	lda TIMETABLE_D,x
	sta pFD
.cont
	sta WSYNC

	lda #$FF
	sta COLUPF
	ldx #4
.loop	sta WSYNC	
	lda #2
	sta ENABL
	lda pFI
	sta PF2
	lda #%00100001
	sta.w CTRLPF
	lda pFD
	SLEEP 25
	sta PF2
	dex
	bne .loop
	sta WSYNC
	lda #0
	sta ENABL
	
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
	lda #$00
	sta PF2

BottLoop
	lda INTIM
       	bne BottLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	sta WSYNC		
	lda #$02		;2
	sta VBLANK		;3
	lda #OSCAN_TIMER     	;2	30 lines * 76 cycles per line / 64 timercycles
        sta TIM64T  		;4	=35,625 wich means ~2280 cycles to use

	inc fCounter

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
	adc #96
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
	adc #255-96
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
;	lda blnM0a,x
	lda verPosM0a,x ;n
	and #%10000000 ;n
	bne ButtonDone
;	lda blnM0a,y
	lda verPosM0a,y ;n
	and #%10000000 ;n
	beq ShootM0
	lda verPosM0a,y
	and #%01111111 ;n
	cmp #38
	bmi ButtonDone
ShootM0
;	lda #1
	lda #%10001010 ;n
;	sta blnM0a,x		;@ optimizar usando la misma variable para blnM0 y verPosM0
;	lda #10
	sta verPosM0a,x
	lda horPosPlane
	sec
	sbc #12
	sta horPosM0a,X
ButtonDone

M0Mov
;	lda blnM0a,X		;	Misile0 movement (only if blnM0 is 1)
	lda verPosM0a,x ;n
	and #%10000000
	beq M0MovDone
	inc verPosM0a,X
	inc verPosM0a,X
;	inc verPosM0a,X	
	lda verPosM0a,X
	and #%01111111 ;n
	cmp #78			;	Vertical limit M0
	bmi M0MovDone
	lda #0
	sta verPosM0a,X
;	sta blnM0a,X
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

	lda object2
	sta object1
	lda object3
	sta object2

	jsr Random	;generate horPos for Obj3
	lda rand
	cmp #140
	bcc OkHorRnd
	and #%01111111
OkHorRnd
	sta horPosObj3
	
	clc	;comprobar si el nuevo objeto está sobre una isla
	lda verPosPF
	sbc #88
	adc #96
	bpl InPF
	
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
;	jmp conti	
conti	
	lda OBJECTS,X
	sta object3
NoSwitch

	jsr Random

;Missile0 flicker
	lda fCounter
	and #$01
	tay
	eor #$01
	tax
	lda verPosM0a,Y
	and #%01111111 ;n
	sta verPosM0
	lda horPosM0a,Y
	sta horPosM0

ReadCollision SUBROUTINE
	lda CXM0P
	and #%10000000
	beq .cont
	sec
	lda verPosM0a,x
	and #%01111111
	sbc #2
	cmp verPosObj1
	bcs obj2
	ldy #0
	beq .collision
obj2	cmp verPosObj2
	bcs obj3
	ldy #1
	bne .collision
obj3	ldy #2

.collision
;	lda verPosObj1,y
;	sbc verPosM0a,x
	lda verPosM0a,x ;
	and #%01111111 ;n
	sbc verPosObj1,y ;n
;	cmp #4
	cmp #%11111010 ;n
	bmi .cont
	lda object1,y
	lsr
	lsr
	lsr
	lsr
	and #%00000111
	cmp #3
	bmi notFlying


notFlying
	cmp #5
	bcs .cont ;notShootable
	lda #%10010101
	sta object1,y
	lda #0
	sta verPosM0a,x
;	sta blnM0a,x
	lda horPosPlane
	sbc #12			;	Place Misile in the middle of plane shape
	sta horPosM0a,x
.cont
	sta CXCLR
	
OverScanLoop
	lda INTIM
       	bne OverScanLoop
       	jmp VerticalBlank	;Start the new screen

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Random	SUBROUTINE
	lda	rand	;Random generator from Dragon Fire (a little tribute)
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
;	iny 	;RESM1
;	ldx horPosM1
;	jsr PosPlayer
	sta WSYNC
	sta HMOVE
	rts
	
PosPlayer SUBROUTINE
	sta WSYNC           	;begin oscanline
        lda HorzTable,X     	;+4  7  
        sta HMP0,Y          	;+3 10
        and #$0F            	;+2 12
        tax                 	;+2 14
.P0	dex                 	;+2 16
        bpl .P0              	;
        sta RESP0,Y         	;
        rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FRASPEEDTABLE
	.byte #165, #205, #230, #50, #85, #110

INTSPEEDTABLE
	.byte #0, #0, #0, #1, #1, #1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TIMETABLE_I	.byte #1,#3,#7,#15,#31,#63,#127,#255
TIMETABLE_D	.byte #128,#192,#224,#240,#248,#252,#254,#255
	ORG $FC00
NUMBERSL	.byte	#<ZERO,	#<ONE, #<TWO, #<THREE, #<FOUR, #<FIVE, #<SIX, #<SEVEN, #<EIGHT, #<NINE

NUMBER_GRAPHICS
ZERO	;.byte 	#%00000000
	.byte	#%11101110
	.byte	#%10101010
	.byte	#%10101010
	.byte	#%10101010
	.byte 	#%11101110

ONE	;.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%01000100
	.byte 	#%01000100
	.byte 	#%11001100
	.byte 	#%01000100

TWO	;.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%10001000
	.byte 	#%11101110
	.byte 	#%00100010
	.byte 	#%11101110

THREE	;.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%00100010
	.byte 	#%11101110
	.byte 	#%00100010
	.byte 	#%11101110

FOUR	;.byte 	#%00000000
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%10101010

FIVE	;.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%00100010
	.byte 	#%11101110
	.byte 	#%10001000
	.byte 	#%11101110

SIX	;.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%11101110
	.byte 	#%10001000
	.byte 	#%10001000

SEVEN	;.byte 	#%00000000
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%11101110

EIGHT	;.byte 	#%00000000
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%11101110

NINE	;.byte 	#%00000000
	.byte 	#%00100010
	.byte 	#%00100010
	.byte 	#%11101110
	.byte 	#%10101010
	.byte 	#%11101110

LEVEL	;.byte 	#%00000000
	.byte	#%00111000
	.byte	#%00100010
	.byte	#%00100000
	.byte	#%00100010
	.byte 	#%00100000

LIVES	;.byte 	#%00000000
	.byte	#%01100000
	.byte	#%00110010
	.byte	#%01101000
	.byte	#%00110010
	.byte 	#%01100000

	ORG $FD00

OBJECTS		.byte	#%00000000, #%00010000, #%00100000, #%00110000, #%01000000, #%01010111, #%01100000, #%01110100
		.byte	#%10001101, #%10010101, #%10100001, #%10111000, #%11001000, #%11011101, #%11100000, #%11111111	
	
SPRITESL	.byte	#<ISLANDER, 		#<PALMERA, 		#<CHEST, 		#<JET, 			#<FLYINGBOMB, 		#<SMALLISLAND, 		#<DOUBLE, 		#<SMALLISLAND
		.byte	#<SUBMARINE,		#<NULL, 		#<NULL, 			#<JET, 			#<FLYINGBOMB,		#<SMALLISLAND,		#<BLINDAGE,		#<SMALLISLAND

COLORSL		.byte	#<ISLANDERCOLOR, 	#<PALMERACOLOR, 	#<CHESTCOLOR, 		#<JETCOLOR, 		#<FLYINGBOMBCOLOR, 	#<SMALLISLANDCOLOR, 	#<DOUBLECOLOR, 		#<SMALLISLANDCOLOR
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
ISLANDERCOLOR   	.byte  	$00,$00,$00,$00,$00,$29,$29,$29,$45,$45,$45,$29,$22
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

;SMALLISLAND	.byte	%00000000		
;		.byte	%00011100
;		.byte	%01111111
;		.byte	%11111111
;		.byte	%01111110
;		.byte	%00111100
;		.byte	%00111000
;		.byte	%11111110
;		.byte	%11111111
;		.byte	%01111111
;		.byte	%01111100
;		.byte	%00111100
;		.byte	%00111000
SMALLISLAND	.byte	%00000000		
		.byte	%00011100
		.byte	%00111110
		.byte	%01111111
		.byte	%01111111
		.byte	%00111111
		.byte	%01111110
		.byte	%11111111
		.byte	%11111111
		.byte	%01111110
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
		.byte	%00000000
		.byte	%00110011
		.byte	%00010010
		.byte	%00001100
		.byte	%00001101
		.byte	%00011110
		.byte	%00101100
		.byte	%00001100
NULL		
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


