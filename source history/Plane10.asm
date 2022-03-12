            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;Comenzado/Started: 10/may/2001 - 18:00 Hs - Córdoba;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;Autor/Programmer: Gonzalo Fernández                ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;Code name: Plane                                   ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
	processor 6502
	include Vcs.h
	include Macro.h

;Constants
NO_ILLEGAL_OPCODES = 0

TOP_TIMER	= 24	;1x lines
SCANLINES_RIVER	= 76	;76 x 2 = 152 scanlines of river
BOTTOM_TIMER	= 24	;2x lines
VBLANK_TIMER	= 44
OSCAN_TIMER	= 36
OBJECTS_H	= 13
PLANE_VER_POS	= 13
MISSILE_SIZE	= 5
ISLAND_COLOR	= $FA
TIMEBAR_YELLOW	= $48
TIMEBAR_RED	= $1F
LIGHTBLUE	= %10101010
BLUE		= %10101000
P0 = 0
P1 = 1
MO = 2
M1 = 3
BL = 4


;RAM Variables

    	SEG.U vars
    	org $0080
    	
;background variables
moveBkGround		ds 1	;height of the Background river bars
bkMask			ds 1

;plane variables
planePtr		ds 2	;Pointer to aeroplane shape
planeColorPtr		ds 2	;Pointer to plane color
planeSpeed		ds 1
planeMovem		ds 1

;object 1, 2 and 3 pointers
obj1Ptr			ds 2		
obj2Ptr			ds 2		
obj3Ptr			ds 2		
objColor1Ptr		ds 2
objColor2Ptr		ds 2
objColor3Ptr		ds 2

;objects info
object1			ds 1	;don't alter this order
verPosObj1		ds 1
object2			ds 1
verPosObj2		ds 1
object3			ds 1
verPosObj3		ds 1
;22
;all vertical positions
verPosM0		ds 1		
verPosM0a		ds 1		
verPosM0b		ds 1		
verPosPF		ds 1
verPosCurrObj 		ds 1		

;all horizontal positions
horPosPlane		ds 1		
horPosObj1		ds 1	;range 0-156 max for objects
horPosObj2		ds 1		
horPosObj3		ds 1
horPosM0		ds 1
horPosM0a		ds 1
horPosM0b		ds 1
horPosCurrObj 		ds 1
;38

;misc variables
rand			ds 2
pFShape			ds 1
fCounter		ds 1
currCTRLPF		ds 1
;43

score1			ds 1	;holds the score values
score2			ds 1	;holds the score values

level1Ptr		ds 2
level2Ptr		ds 2
livesPtr		ds 2

score1RAM		ds 5	;score graphics in RAM
score2RAM		ds 5
score1Ptr		ds 2
score2Ptr		ds 2

;67
;score, level and lives variables
scoreTmp		ds 1
livesLevel		ds 1	;Bits 7,6,5=lives. Rest=level
time			ds 1
fraTime			ds 1
km			ds 1
fraKm			ds 1
intKm			ds 1
;asymetric Playfield variables
pFI			ds 1	;left
pFD			ds 1	;right

;vars used only in the kernel time
branch			ds 1
currObj			ds 1
currObjPtr		ds 2
currObjColorPtr		ds 2

;vars used only the overscan/vertblank time
flyLow			ds 1
intSpeed		ds 1
fraSpeed		ds 1
;86

gameState		ds 1
temp			ds 1

;;;;;;;;
;;Code;;
;;;;;;;;

    	SEG  código
        org  $F000

;;;;;;;;;;;;;;;
;;Subroutines;;
;;;;;;;;;;;;;;;

EndVblank
;;;;;;;;;
	lda INTIM
	bne EndVblank
	sta VBLANK		;	Turn off Vertical Blank
	rts

EndOverscan
;;;;;;;;;;;	
	lda INTIM
       	bne EndOverscan
	rts

InitNewGame
;;;;;;;;;;;
	lda #%01000001		;level=0. lives=2
	sta livesLevel

	lda #0
	sta score1
	sta score2
	sta bkMask

	sta km			;init distbar
	lda #60			;init timebar
	sta time

ResetObjects
	lda #74			;plane in the middle of screen
	sta horPosPlane
	lda PLANELPTR
	sta planePtr
	lda #0
	sta verPosM0
	sta verPosM0a
	sta verPosM0b
	sta fraSpeed
	sta planeSpeed
	lda #$F0		;first island...
	sta pFShape
	lda #76			;in the top of screen
	sta verPosPF
	lda #90
	sta verPosObj3
	sta horPosObj3
	sbc #30
	sta horPosObj2
	sbc #30
	sta horPosObj1
	lda #$90		;null obj
	sta object1
	sta object2
	sta object3

	rts

LevelScreen SUBROUTINE
;;;;;;;;;;;;;;;;;;;;;;
	VERTICAL_SYNC
	lda #VBLANK_TIMER
	sta TIM64T
	jsr EndVblank

	lda #0
	sta COLUP0
	
	ldy #87
.TopLoop
	sta WSYNC
	dey
	bpl .TopLoop

	lda #76
	ldy #P0
	jsr Bz
	sta WSYNC
	sta HMOVE

	clc
	ldy #0
	lda livesLevel
	and #%00011111
;hex to dec
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

	ldy #4
.Level	sta WSYNC
	lda #$0F
	sta COLUP0
	lda (level1Ptr),y	;5
	and #$F0		;2
	sta scoreTmp		;3
        lda (level2Ptr),y	;5
	and #$0F		;2
	ora scoreTmp		;3   
        sta GRP0     		;3
	dey
	bpl .Level

	ldy #94
.BottomLoop
	sta WSYNC
	lda #0
	sta COLUP0
	dey
	bpl .BottomLoop

	sta WSYNC		
	lda #$02	
	sta VBLANK		
	lda #OSCAN_TIMER   
        sta TIM64T  		
	jmp EndOverscan

Score
;;;;;
;Draw Level/Score/Lives digits
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
        bpl .loop

	sta WSYNC
	lda #0
	sta GRP0
	sta GRP1
	rts

;;;;;;;;;;;;;;;;;;;;;;;
;;Program begins here;;
;;;;;;;;;;;;;;;;;;;;;;;

Start
	CLEAN_START

;Permanent Settings
;;;;;;;;;;;;;;;;;;;
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
	lda #<PLANECOLOR-3
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
;seed random generator
	lda #$11
	sta rand+1
	lda #150
	sta rand
	jsr Random	

	jsr InitNewGame

;Main Loop
;;;;;;;;;;
MLOOP
	lda SWCHB
	and #1
	bne contMain
	lda #1
	sta gameState
	lda #120
	sta fCounter
	jsr InitNewGame
contMain

	lda gameState
	bne .f0
	jsr MainScreen	;ScreenSaver
	dec fCounter
	lda fCounter
	and #%11000000
	sta bkMask
	jmp MLOOP
.f0
	cmp #1		;show level screen?
	bne .f2
	dec fCounter
	lda fCounter
	beq .f1
	jsr LevelScreen	
	jmp MLOOP
.f1	lda #2
	sta gameState
	jmp MLOOP

.f2			;game in progress
	jsr MainScreen
;	lda gameState
;	beq MLOOP
	jsr MainOverscan
	jmp MLOOP


	ORG $F500
;;;;;;;;;;;;;;;;;;;;;;;
;;Main Vertical Blank;;
;;;;;;;;;;;;;;;;;;;;;;;

MainScreen
	VERTICAL_SYNC
		
	lda #VBLANK_TIMER
	sta TIM64T
	
;Update pointers for objects
;according to their vertical positions
ObjectPointers SUBROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;
	ldy #4
.loop
	lda object1,y
	;and #%00001111
	lsr
	lsr
	lsr
	lsr
	tax

	clc
	lda SPRITESL,x 
	sbc verPosObj1,y
	adc #12
	sta obj1Ptr,y
	
	clc
	lda COLORSL,x
	sbc verPosObj1,y
	adc #12
	sta objColor1Ptr,y
	
	dey
	dey
	bpl .loop

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
;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;
	clc
	ldy #0
	lda livesLevel
	and #%00011111
;hex to dec
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


;End Vertical Blank	
	jsr EndVblank
	
;;;;;;;
;;TOP;;
;;;;;;;

	lda #TOP_TIMER
        sta TIM64T

        lda #255
        sta COLUP0
        sta COLUP1

	lda #0
	sta REFP1
	sta NUSIZ1

	jsr Score
	jsr PosHorMain

	lda currCTRLPF
	sta CTRLPF
TopLoop
	lda INTIM
       	bne TopLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Main Kernel - River Screen;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldy #SCANLINES_RIVER	;reg Y is the line counter

	lda #ISLAND_COLOR
	sta COLUPF

	sta HMCLR
	lda #%10000000
	sta HMM0
	sta HMP0
	lda object3
	and #%11001111
	sta REFP1
	sta NUSIZ1

	lda #0
	sta COLUP0
	sta temp
	sta NUSIZ0
	sta COLUP1

	lda temp+1
	beq Line1
	sta VDELP1
	jmp Line1

NoPF	SLEEP 20
	jmp PlayfieldDone

Line1 SUBROUTINE
;;;;;;;;;;;;;;;;
	lda moveBkGround
	sta WSYNC
ContL1				;ContL1 used to return from Reposition
	and #%00010000
	tax
	lda RIVERCOLOR,X	;4
	eor bkMask
	sta COLUBK		;3
	dec moveBkGround	;update river data
	lda temp
	sta GRP0

DrawPlayfield SUBROUTINE
	sec
	tya
	sbc verPosPF
	bmi NoPF
	lsr
	lsr
	tax
	lda pFShape
	beq .PFSh2
	bmi .PFSh1
	lda ISLAND,X
	sta.w PF2 
	jmp PlayfieldDone	;26

.PFSh1	lda ISLAND2,X ;ISLAND2
	sta PF1
	jmp PlayfieldDone	;26
	
.PFSh2	lda ISLAND,X ;ISLAND2
	sta PF1
	sta PF0		;24
	nop		;26
PlayfieldDone

	lda branch	;Choose routine: Repostion, UpdateP1 or Line2 (P1,P0,M0)
	bmi Reposition
	beq UpdateP1
	
Line2 SUBROUTINE
;;;;;;;;;;;;;;;;
	clc
	tya
	sbc verPosCurrObj 
	adc #OBJECTS_H
	bpl NoChObj
	lda currObj
	sta branch
NoChObj	bcc NoObj
	lda (currObjPtr),Y
	sta GRP1
	lda (currObjColorPtr),Y
	sta COLUP1
ObjDone

;Plane
	cpy #PLANE_VER_POS
	bpl NoPlane
	lda (planePtr),Y
	sta temp		;preload and use it in the next line to alow VDELP1
	lda (planeColorPtr),Y
	sta COLUP0
NoPlane

;Draw Missile
	tya
	sbc verPosM0
  	adc #MISSILE_SIZE
  	rol
  	rol
  	sta ENAM0

	dey
	bne Line1
	jmp ExitKernel

Reposition SUBROUTINE	;reposition P1 in the middle of screen, twice per frame.
;;;;;;;;;;;;;;;;;;;;;
	lda horPosCurrObj
	dey
	sta WSYNC
	sec
.loop	sbc #$0F
	bcs .loop
	and #$0F
	tax
	lda BZHM,x
	ldx #0
	sta HMP1
	sta RESP1
	stx branch	;X=0. Means that the next "lda branch" will result in a branch to UpdateP1
	lda moveBkGround
	sta WSYNC
	jmp ContL1	;do not go to Line1. We did part of its job, let's skip some lines
;	jmp Line1

NoObj
	lda #$00	
	sta GRP1
	beq ObjDone	

UpdateP1 SUBROUTINE	;Update pointers and positions of P1. Also draw missile
;;;;;;;;;;;;;;;;;;;
	sta HMOVE
	
	lda #$0F
	sta branch	
	
	dec currObj
	bmi SndObj
	ldx #0
	jmp .cont
SndObj
	lda horPosObj1
	sta horPosCurrObj 		
	ldx #2
.cont
	lda obj1Ptr,x
	sta currObjPtr
	lda objColor1Ptr,x
	sta currObjColorPtr
	
	lda object1,x
	and #%11001111
	sta REFP1
	sta NUSIZ1
	
	lda verPosObj1,x
	sta verPosCurrObj 

;Draw Missile
	tya
	sbc verPosM0
  	adc #MISSILE_SIZE
  	rol
  	rol
  	sta ENAM0

	dey
	jmp Line1

ExitKernel
;;;;;;;;;;

;;;;;;;;;;
;;BOTTOM;;
;;;;;;;;;;

	sta WSYNC
	lda #BOTTOM_TIMER     	;2	
        sta TIM64T  		;4	
	lda #$00
	sta VDELP1
	sta COLUBK
	sta PF0
	sta PF1
	sta PF2
	sta GRP1
	sta GRP0
	sec
	lda moveBkGround
	sbc #20			;adjust the final value
	sta moveBkGround

TimeBar SUBROUTINE
;;;;;;;;;;;;;;;;;;
;select bar color
	sec
	lda #60
	sbc #2
	sbc km
	cmp time
	bmi red
	lda #TIMEBAR_YELLOW
	sta COLUPF
	jmp yellow
red	lda #TIMEBAR_RED
	sta COLUPF	
yellow
	sta WSYNC
	ldy #BL
	clc
	lda time	;range 0-60
	adc #47
	tax
	jsr Bz
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
	ldx #5
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
	SLEEP 3
	lda #0
	sta ENABL

DistBar SUBROUTINE	
;;;;;;;;;;;;;;;;;;
	lda #$00
	sta PF2
	lda RIVERCOLOR
	sta COLUPF
	clc
	lda km	;range 0-60
	adc #45
	tax
	ldy #P0
	jsr Bz
	sta WSYNC
	sta HMOVE
	lda #100 ;?
	sta COLUP0
	ldx #4
.loop	sta WSYNC
	lda #$FF
	sta PF2
	lda LIVES,x
	and #%11111100
	sta GRP0
	dex
	bpl .loop
	sta WSYNC
	lda #$00
	sta PF2
	sta GRP0

BottomLoop
	lda INTIM
       	bne BottomLoop

	rts

;;;;;;;;;;;;;;;;;
;;Main Overscan;;
;;;;;;;;;;;;;;;;;

MainOverscan
	sta WSYNC		
	lda #$02		;2
	sta VBLANK		;3
	lda #OSCAN_TIMER     	;2	30 lines * 76 cycles per line / 64 timercycles
        sta TIM64T  		;4	=35,625 wich means ~2280 cycles to use

	inc fCounter

UpdateTimeDist SUBROUTINE
	lda time
	cmp #0
	beq .cont1
	clc
	lda fraTime
	adc #2		;0.2
	sta fraTime
	bcc .cont1
	dec time
.cont1
	lda livesLevel
	and #%00011111
	lsr
	lsr
	sta temp
	lda SWCHA
	eor #$FF
	and #%00010000
	lsr
	adc temp
	tax
	clc
	lda fraKm
	adc FRA_DIST_SLOW,x
	sta fraKm
	lda intKm
	adc INT_DIST_SLOW,x
	sta intKm
	bcc .cont
	inc km
	lda km
	cmp #62
	bmi .cont
	lda #62
	sta km
.cont

;Joystick routine
ReadJoy	SUBROUTINE
	lda SWCHA		
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
	cmp #140
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
	cmp #8
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
	cmp #63
	beq UpDone
	inc planeSpeed
UpDone

JoyDone

;	lda #0		;Get the proper speed of plane
;	sta intSpeed
	lda planeSpeed
	lsr
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
;n<
	lda #0
	sta temp+1
	tya
	bne RepeatMov2
	lda #1
	sta temp+1
;n>	
RepeatMov2
	tya
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
	lda verPosM0a,x
	and #%10000000
	bne ButtonDone
	lda verPosM0a,y
	and #%10000000
	beq ShootM0
	lda verPosM0a,y
	and #%01111111
	cmp #38
	bmi ButtonDone
ShootM0
	lda #%10001010
	sta verPosM0a,x
	lda horPosPlane
	adc #3
	sta horPosM0a,X
ButtonDone

M0Mov
	lda verPosM0a,x
	and #%10000000
	beq M0MovDone
	inc verPosM0a,X
	inc verPosM0a,X
	inc verPosM0a,X	
	lda verPosM0a,X
	and #%01111111
	cmp #80			;	Vertical limit M0
	bmi M0MovDone
	lda #0
	sta verPosM0a,X
	lda horPosPlane
	adc #3
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
	cmp #135 ;@
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
bigg03	lda #135
	sta horPosObj3	
conti	
	lda OBJECTS,X
	sta object3
	clc
	lda horPosObj3
	adc #14

	sta horPosObj3
NoSwitch

	jsr Random

;objects movement
ObjMovement SUBROUTINE
	ldx #2
	ldy #4
.loop	lda object1,y
	and #%01110000
	cmp #%01110000
	beq .cont
	cmp #%00110000
	bmi .cont

	cmp #%00110000
	beq .chk1
	cmp #%01000000
	beq .chk1
.chk1	lda object1,y
	and #%00001000
	cmp #%00001000
	beq .left

.right	lda horPosObj1,x
	cmp #135+14
	bcs .rLimit
	inc horPosObj1,x
	jmp .cont
.rLimit	lda object1,y
	ora #%00001000	;reverse direction
	sta object1,y
	jmp .cont

.left	lda horPosObj1,x
	cmp #10+14
	bcc .lLimit
	dec horPosObj1,x
	jmp .cont
.lLimit	lda object1,y
	and #%11110111	;reverse direction
	sta object1,y
.cont	dey
	dey
	dex
	bpl .loop

;Missile0 flicker
	lda fCounter
	and #$01
	tay
	eor #$01
	tax
	lda verPosM0a,y
	and #%01111111
	sta verPosM0
	lda horPosM0a,y
	sta horPosM0

M0P0Collision SUBROUTINE
	lda SWCHA
	and #%00100000
	sta temp

	lda CXM0P
	and #%10000000
	beq .cont

	lda verPosM0a,x
	and #%01111111
	sbc #4
	cmp verPosObj1
	bpl obj2
	ldy #0
	beq .collision
obj2	cmp verPosObj2
	bpl obj3
	ldy #2
	bne .collision
obj3	ldy #4

.collision
	lda verPosM0a,x
	and #%01111111
	sbc verPosObj1,y
	cmp #%11111001
	bmi .cont
	lda object1,y
	lsr
	lsr
	lsr
	lsr
	and #%00000111
	cmp #3
	bpl .notFlying
	lda temp
	bne .cont
	beq eliminate
.notFlying
	cmp #6
	bpl .cont ;nonShootable
	lda temp
	beq .cont
eliminate
	lda #%10010101
	sta object1,y
	lda #0
	sta verPosM0a,x
	lda horPosPlane
	adc #3
	sta horPosM0a,x
.cont

P0P1Collision SUBROUTINE
	lda CXPPMM
	and #%10000000
	beq .cont
	lda object1
	lsr
	lsr
	lsr
	lsr
	tax
	and #%00000111
	cmp #3
	bcs .notFlying
	lda temp
	bne .cont
	cpx #2
	bcs .dead
	lda #%10010101
	sta object1
	bne .cont

.notFlying
	txa
	and #%00000111
	cmp #6
	bcs .cont ;nonShootable
	lda temp
	beq .cont

.dead	lda livesLevel
	and #%11100000
	bne decLive
	sta gameState	;0=game over
;	jsr ResetObjects
	jmp .cont
decLive	lda #1
	sta gameState
	lda livesLevel
	sec
	sbc #%00100000
	sta livesLevel
	lda #100
	sta fCounter
	jsr ResetObjects
.cont
	sta CXCLR
	
	jmp EndOverscan

;;;;;;;;;;;;;;;
;;Subroutines;;
;;;;;;;;;;;;;;;

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
	ldy #P0	;RESP0		;Horizontal positions in the Main Kernel
	lda horPosPlane
	jsr Bz
	iny	;RESP1
	lda horPosObj3
	sbc #13
	jsr Bz
	iny	;RESM0
	lda horPosM0
	jsr Bz
;	iny 	;RESM1
;	ldx horPosM1
;	jsr Bz
	sta WSYNC
	sta HMOVE
	rts

Bz SUBROUTINE
	sta WSYNC
	sec
.loop	sbc #$0F
	bcs .loop
	and #$0F
	tax
	lda BZHM,x
	sta HMP0,y
	sta RESP0,y
	rts

;;;;;;;;;;
;;Tables;;
;;;;;;;;;;

BZHM
	.byte $80,$70,$60,$50,$40,$30,$20,$10,$00
	.byte $F0,$E0,$D0,$C0,$B0,$A0,$90

FRA_DIST_SLOW
	.byte #120, #110, #100, #90, #85, #80, #75, #70
FRA_DIST_FAST
	.byte #150, #130, #120, #110, #100, #90, #85, #80
INT_DIST_SLOW
	.byte #1, #1, #1, #1, #1, #1, #1, #1
INT_DIST_FAST
	.byte #2, #2, #2, #2, #2, #2, #2, #2

FRASPEEDTABLE
	.byte #192, #205, #30, #110

INTSPEEDTABLE
	.byte #0, #0, #1, #1

TIMETABLE_I	.byte #1,#3,#7,#15,#31,#63,#127,#255
TIMETABLE_D	.byte #128,#192,#224,#240,#248,#252,#254,#255
;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
;;Graphics Data;;
;;;;;;;;;;;;;;;;;

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

OBJECTS		.byte	#%00000000, #%00010000, #%00100000, #%00110000, #%01000000, #%01011000, #%01100000, #%01110101
		.byte	#%10001101, #%10010101, #%10100101, #%10111000, #%11001000, #%11011000, #%11100000, #%11111111	
	
SPRITESL	.byte	#<ISLANDER, 		#<CHEST, 		#<PALMERA, 		#<JET, 			#<FLYINGBOMB, 		#<JET, 			#<DOUBLE, 		#<SMALLISLAND
		.byte	#<SUBMARINE,		#<NULL, 		#<SUBMARINE, 		#<JET, 			#<FLYINGBOMB,		#<FLYINGBOMB,		#<BLINDAGE,		#<SMALLISLAND

COLORSL		.byte	#<ISLANDERCOLOR, 	#<CHESTCOLOR,	 	#<PALMERACOLOR, 		#<JETCOLOR, 		#<FLYINGBOMBCOLOR, 	#<JETCOLOR, 		#<DOUBLECOLOR, 		#<SMALLISLANDCOLOR
		.byte	#<SUBMARINECOLOR,	#<SUBMARINECOLOR,	#<SUBMARINECOLOR,		#<JETCOLOR,		#<FLYINGBOMBCOLOR,	#<FLYINGBOMBCOLOR,	#<BLINDAGECOLOR,	#<SMALLISLANDCOLOR
		
PLANELPTR	.byte	#<PLANE-3, #<PLANEDOWN-3

	ORG $FD5E
			.byte	$02
COLORS
PLANECOLOR		.byte	$02,$02,$02,$90,$90,$90,$90,$90,$90,$90,$90
PALMERACOLOR    	.byte   $2A,$28,$2A,$2A,$2A,$20,$20,$20,$20,$D4,$D4,$D4,$D4
JETCOLOR    		.byte  	$00,$04,$04,$04,$04,$00,$00,$00,$50,$50,$50,$50,$50
FLYINGBOMBCOLOR		.byte  	$00,$04,$04,$04,$04,$00,$00,$00,$00,$02,$04,$06,$08
SMALLISLANDCOLOR	.byte  	$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA,$FA
SUBMARINECOLOR		.byte  	$04,$04,$04,$04,$04,$00,$00,$00,$00,$02,$04,$04,$04
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
		.byte	%10111110
		.byte	%01111111
		.byte	%01111111
		.byte	%10111110
		.byte	%00000000

SUBMARINE	.byte	%00000000		
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%01111110
		.byte	%11111111
		.byte	%00110000
		.byte	%00010000
		.byte	%00000000

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

RIVERCOLOR	.byte #LIGHTBLUE
NULL		.byte #$00
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
		.byte #BLUE	;end RIVERCOLOR

	org $FFFC
	.word Start
	.word Start     


