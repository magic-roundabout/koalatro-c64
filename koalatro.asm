;
; KOALATRO
;

; Code and graphics by T.M.R/Cosine
; Music by aNdy/Cosine


; A somewhat masochistic exercise in cramming a bitmap, music,
; scroller (with unrolled colour splitting code) and text into just
; 15K of bank 0 (using space between $0400 and $3FFF) after a
; discussion on Facebook. *Strictly* speaking it's not a Koala
; format picture because some of the colour has been packed, but
; all the data is still present!


; This source code is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with PuCrunch which can be downloaded at
; http://csdb.dk/release/?id=6089

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "koalatro.prg",cbm


; Yank in binary data
		* = $1400
music		!binary"data/lesewch_tap.prg",,$0c

; Partially packed colour data
		* = $0400
		!binary "data/hussein.kla",,$01f42

; Bitmap data
		* = $2000
		!binary "data/hussein.kla",$1f40,$02


; Constants
rstr1p		= $19
rstr2p		= $f2

; Static colours for the scroller
split1_col_00	= $06
split1_col_01	= $04
split1_col_02	= $0e
split1_col_03	= $0d
split1_col_04	= $03
split1_col_05	= $05
split1_col_06	= $08
split1_col_07	= $02
split1_col_08	= $09

split2_col_00	= $09
split2_col_01	= $08
split2_col_02	= $0a
split2_col_03	= $07
split2_col_04	= $0f
split2_col_05	= $0c
split2_col_06	= $04
split2_col_07	= $0b
split2_col_08	= $06

; Label assignments
rn		= $50
d021_colour	= $51

scroll_cnt	= $52

pulse_tmr_1	= $53
pulse_tmr_2	= $54
char_buffer	= $55		; $30 bytes

sprite_buffer	= $0800


; Entry point at $c000 (gets overwritten by picture data later)
		* = $0a00
entry		sei

; Turn off the ROMS and screen
		lda #$35
		sta $01

		lda #$0b
		sta $d011

		lda #$00
		sta $d020
		sta $d021
		sta $3fff

; Initialise $D800 colour for the picture (one byte contains two
; nybbles of colour)
		lda $09dc
		sta d021_colour

		ldx #$00
		ldy #$00
bitmap_colour	lda $07e8+$000,x
		sta $d800,y
		lsr
		lsr
		lsr
		lsr
		sta $d801,y

		lda $07e8+$080,x
		sta $d900,y
		lsr
		lsr
		lsr
		lsr
		sta $d901,y

		lda $07e8+$100,x
		sta $da00,y
		lsr
		lsr
		lsr
		lsr
		sta $da01,y

		cpy #$e8
		bcs bitmap_skip
		lda $07e8+$180,x
		sta $db00,y
		lsr
		lsr
		lsr
		lsr
		sta $db01,y

bitmap_skip	inx
		iny
		iny
		bne bitmap_colour

; Set up the NMI and IRQ interrupts
		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #rstr1p
		sta $d012

		lda #$0b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Zero the workspaces in ZP and initialise a few labels
		ldx #$53
		lda #$00
nuke_zp		sta $00,x
		inx
		bne nuke_zp

		lda #$01
		sta rn

		lda #$b0
		sta scroll_cnt

		lda #$20
		sta pulse_tmr_1
		lda #$2b
		sta pulse_tmr_2

; Since the hardware sprites don't move in X or change data pointer,
; turn them on and set both up
		lda #$ff
		sta $d015
		sta $d01d

		ldx #$00
		ldy #$00
set_sprx_1	lda sprite_x,x
		sta $d000,y
		lda sprite_dp,x
		sta $07f8,x
		iny
		iny
		inx
		cpx #$08
		bne set_sprx_1

		lda sprite_x+$08
		sta $d010

; Clear the sprite buffer ready for use!
		ldx #$00
		txa
sprite_clr	sta sprite_buffer+$000,x
		sta sprite_buffer+$100,x
		inx
		bne sprite_clr


; Initialise the music
		lda #$00
		jsr music+$00

		cli


; Runtime loop - check for space bar and quit
main_loop	lda $dc01
		cmp #$ef
		bne main_loop

; Switch everything to black, kill the volume and reset the C64
		sei
		lda #$0b
		sta $d011
		lda #$00
		sta $d020
		sta $d021
		sta $d418

		lda #$36
		sta $01
		jmp $fce2


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		bne *+$05
		jmp rout2


; Raster split 1
rout1		lda #$3b
		sta $d011
		lda #$18
		sta $d016
		sta $d018

; Set the sprite Y positions for the upper scroller
		lda #$20
		sta $d001
		sta $d003
		sta $d005
		sta $d007
		sta $d009
		sta $d00b
		sta $d00d
		sta $d00f

; Jitter removal for the upper scroller
		ldx #$04
		dex
		bne *-$01
		nop
		nop
		nop
		nop
		lda $d012
		cmp #rstr1p+$01
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		lda $d012
		cmp #rstr1p+$02
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		nop
		lda $d012
		cmp #rstr1p+$03
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		bit $ea
		lda $d012
		cmp #rstr1p+$04
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		bit $ea
		lda $d012
		cmp #rstr1p+$05
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		bit $ea
		lda $d012
		cmp #rstr1p+$06
		beq *+$02
;		sta $d020

		nop
		nop
		nop
		nop
		nop

; Call the splitter code
		jsr splitter

; Set the screen colour for the picture
		ldx #$04
		dex
		bne *-$01
		nop
		nop
		nop
		lda d021_colour
		sta $d021

; Play the music (first pass)
		jsr music+$03

; Update the scroller
		ldx #$00
mover		asl char_buffer,x
		rol sprite_buffer+$1c5,x
		rol sprite_buffer+$1c4,x
		rol sprite_buffer+$1c3,x

		rol sprite_buffer+$185,x
		rol sprite_buffer+$184,x
		rol sprite_buffer+$183,x

		rol sprite_buffer+$145,x
		rol sprite_buffer+$144,x
		rol sprite_buffer+$143,x

		rol sprite_buffer+$105,x
		rol sprite_buffer+$104,x
		rol sprite_buffer+$103,x

		rol sprite_buffer+$0c5,x
		rol sprite_buffer+$0c4,x
		rol sprite_buffer+$0c3,x

		rol sprite_buffer+$085,x
		rol sprite_buffer+$084,x
		rol sprite_buffer+$083,x

		rol sprite_buffer+$045,x
		rol sprite_buffer+$044,x
		rol sprite_buffer+$043,x

		rol sprite_buffer+$005,x
		rol sprite_buffer+$004,x
		rol sprite_buffer+$003,x

		txa
		clc
		adc #$06
		tax
		cpx #$30
		beq *+$05
		jmp mover

; Fetch a new character if needed
		ldx scroll_cnt
		dex
		cpx #$ff
		bne scroll_xb

mread		lda scroll_text
		bne okay
		jsr reset
		jmp mread

okay		sta def_copy+$01
		lda #$00
		asl def_copy+$01
		rol
		asl def_copy+$01
		rol
		asl def_copy+$01
		rol
		clc
		adc #$d8
		sta def_copy+$02

		lda #$33
		sta $01

		ldx #$00
		ldy #$00
def_copy	lda $6464,x
		sta char_buffer,y
		tya
		clc
		adc #$06
		tay
		inx
		cpx #$08
		bne def_copy

		lda #$35
		sta $01

		inc mread+$01
		bne *+$05
		inc mread+$02

		ldx #$07
scroll_xb	stx scroll_cnt

; Update the reversed scroller
		ldx #$00
mover_2		lda sprite_buffer+$1c5,x
		lsr

		ror sprite_buffer+$006,x
		ror sprite_buffer+$007,x
		ror sprite_buffer+$008,x

		ror sprite_buffer+$046,x
		ror sprite_buffer+$047,x
		ror sprite_buffer+$048,x

		ror sprite_buffer+$086,x
		ror sprite_buffer+$087,x
		ror sprite_buffer+$088,x

		ror sprite_buffer+$0c6,x
		ror sprite_buffer+$0c7,x
		ror sprite_buffer+$0c8,x

		ror sprite_buffer+$106,x
		ror sprite_buffer+$107,x
		ror sprite_buffer+$108,x

		ror sprite_buffer+$146,x
		ror sprite_buffer+$147,x
		ror sprite_buffer+$148,x

		ror sprite_buffer+$186,x
		ror sprite_buffer+$187,x
		ror sprite_buffer+$188,x

		ror sprite_buffer+$1c6,x
		ror sprite_buffer+$1c7,x
		ror sprite_buffer+$1c8,x

		txa
		clc
		adc #$06
		tax
		cpx #$30
		beq *+$05
		jmp mover_2

; Update the left hand half of the lower scroller's colour effect
		lda pulse_tmr_1
		clc
		adc #$01
		cmp #$5f
		bcc *+$04
		lda #$00
		sta pulse_tmr_1
		lsr
		cmp #$10
		bcc *+$04
		lda #$00
		tax

		lda colour_pulse_1+$00,x
		sta split_1_11+$01
		lda colour_pulse_1+$01,x
		sta split_1_0f+$01
		lda colour_pulse_1+$02,x
		sta split_1_0d+$01
		lda colour_pulse_1+$03,x
		sta split_1_0b+$01
		lda colour_pulse_1+$04,x
		sta split_1_09+$01
		lda colour_pulse_1+$05,x
		sta split_1_07+$01
		lda colour_pulse_1+$06,x
		sta split_1_05+$01
		lda colour_pulse_1+$07,x
		sta split_1_03+$01
		lda colour_pulse_1+$08,x
		sta split_1_01+$01

; Update the right hand half of the lower scroller's colour effect
		lda pulse_tmr_2
		clc
		adc #$01
		cmp #$5f
		bcc *+$04
		lda #$00
		sta pulse_tmr_2
		lsr
		cmp #$10
		bcc *+$04
		lda #$00
		tax

		lda colour_pulse_2+$08,x
		sta split_2_11+$01
		lda colour_pulse_2+$07,x
		sta split_2_0f+$01
		lda colour_pulse_2+$06,x
		sta split_2_0d+$01
		lda colour_pulse_2+$05,x
		sta split_2_0b+$01
		lda colour_pulse_2+$04,x
		sta split_2_09+$01
		lda colour_pulse_2+$03,x
		sta split_2_07+$01
		lda colour_pulse_2+$02,x
		sta split_2_05+$01
		lda colour_pulse_2+$01,x
		sta split_2_03+$01
		lda colour_pulse_2+$00,x
		sta split_2_01+$01

; Wait for rasterline $cb before calling the music again
		lda #$cb
		cmp $d012
		bne *-$03

		jsr music+$03

; Set up for the second raster split
		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

		jmp ea31

; Raster split 2
rout2		ldx #$06
		dex
		bne *-$01
		nop

; Jitter removal for the lower scroller
		lda $d012
		cmp #rstr2p+$01
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		lda $d012
		cmp #rstr2p+$02
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		nop
		lda $d012
		cmp #rstr2p+$03
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		nop
		lda $d012
		cmp #rstr2p+$04
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		bit $ea
		nop
		lda $d012
		cmp #rstr2p+$05
		beq *+$02
;		sta $d020

		ldx #$09
		dex
		bne *-$01
		nop
		nop
		nop
		nop
		lda $d012
		cmp #rstr2p+$06
		beq *+$02
;		sta $d020

; Lining things up for before first scanline of the scroller
		ldx #$0a
		dex
		bne *-$01

		lda #$34
		sta $d011

		ldx #$03
		dex
		bne *-$01

; Set the sprite Y positions for the lower scroller
		lda #$fa
		sta $d001
		sta $d003
		sta $d005
		sta $d007
		sta $d009
		sta $d00b
		sta $d00d
		sta $d00f
		nop
		nop

		lda #$00
		sta $d021

		jsr splitter

; Update the left hand half of the upper scroller's colour effect
		lda pulse_tmr_1
		lsr
		cmp #$10
		bcc *+$04
		lda #$00
		tax

		lda colour_pulse_2+$08,x
		sta split_1_11+$01
		lda colour_pulse_2+$07,x
		sta split_1_0f+$01
		lda colour_pulse_2+$06,x
		sta split_1_0d+$01
		lda colour_pulse_2+$05,x
		sta split_1_0b+$01
		lda colour_pulse_2+$04,x
		sta split_1_09+$01
		lda colour_pulse_2+$03,x
		sta split_1_07+$01
		lda colour_pulse_2+$02,x
		sta split_1_05+$01
		lda colour_pulse_2+$01,x
		sta split_1_03+$01
		lda colour_pulse_2+$00,x
		sta split_1_01+$01

; Update the right hand half of the upper scroller's colour effect
		lda pulse_tmr_2
		lsr
		cmp #$10
		bcc *+$04
		lda #$00
		tax

		lda colour_pulse_1+$00,x
		sta split_2_11+$01
		lda colour_pulse_1+$01,x
		sta split_2_0f+$01
		lda colour_pulse_1+$02,x
		sta split_2_0d+$01
		lda colour_pulse_1+$03,x
		sta split_2_0b+$01
		lda colour_pulse_1+$04,x
		sta split_2_09+$01
		lda colour_pulse_1+$05,x
		sta split_2_07+$01
		lda colour_pulse_1+$06,x
		sta split_2_05+$01
		lda colour_pulse_1+$07,x
		sta split_2_03+$01
		lda colour_pulse_1+$08,x
		sta split_2_01+$01

; Set up for the first raster split
		lda #$01
		sta rn
		lda #rstr1p
		sta $d012

; Exit IRQ interrupt
ea31		pla
		tay
		pla
		tax
		pla
nmi		rti

; Reset the scrolling message
reset		lda #<scroll_text
		sta mread+$01
		lda #>scroll_text
		sta mread+$02
		rts


; Splitter code (called twice per frame)
splitter	lda #$18
		bit $ea

; Scanline $00
		ldx #split1_col_00
		ldy #split2_col_00
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_01	ldx #$01
		sta $d016

; Scanline $01
		sty $d02c
		sty $d02d
		sty $d02e
split_2_01	ldy #$01
		stx $d027
		stx $d028
		stx $d029
		stx $d02a
		sty $d02b
		dec $d016
		ldx #split1_col_01
		sta $d016

; Scanline $02
		sty $d02c
		sty $d02d
		sty $d02e
		ldy #split2_col_01
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_03	ldx #$01
		sta $d016

; Scanline $03
		sty $d02c
		sty $d02d
		sty $d02e
split_2_03	ldy #$01
		stx $d027
		stx $d028
		stx $d029
		stx $d02a
		sty $d02b
		dec $d016
		ldx #split1_col_02
		sta $d016

; Scanline $04
		sty $d02c
		sty $d02d
		sty $d02e
		ldy #split2_col_02
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_05	ldx #$01
		sta $d016

; Scanline $05
		sty $d02c
		sty $d02d
		sty $d02e
split_2_05	ldy #$01
		stx $d027
		stx $d028
		stx $d029
		stx $d02a
		sty $d02b
		dec $d016
		ldx #split1_col_03
		sta $d016

; Scanline $06
		sty $d02c
		sty $d02d
		sty $d02e
		ldy #split2_col_03
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_07	ldx #$01
		sta $d016

; Scanline $07
		sty $d02c
		sty $d02d
		sty $d02e
split_2_07	ldy #$01
		stx $d027
		stx $d028
		stx $d029
		stx $d02a
		sty $d02b
		dec $d016
		ldx #split1_col_04
		sta $d016

; Scanline $08
		sty $d02c
		sty $d02d
		sty $d02e
		ldy #split2_col_04
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_09	ldx #$01
		sta $d016

; Scanline $09
		sty $d02c
		sty $d02d
		sty $d02e
split_2_09	ldy #$01
		stx $d027
		stx $d028
		stx $d029
		stx $d02a
		sty $d02b
		dec $d016
		ldx #split1_col_05
		sta $d016

; Scanline $0a
		sty $d02c
		sty $d02d
		sty $d02e
		ldy #split2_col_05
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_0b	ldx #$01
		sta $d016

; Scanline $0b
		sty $d02c
		sty $d02d
		sty $d02e
split_2_0b	ldy #$01
		stx $d027
		stx $d028
		stx $d029
		stx $d02a
		sty $d02b
		dec $d016
		ldx #split1_col_06
		sta $d016

; Scanline $0c
		sty $d02c
		sty $d02d
		sty $d02e
		ldy #split2_col_06
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_0d	ldx #$01
		sta $d016

; Scanline $0d
		sty $d02c
		sty $d02d
		sty $d02e
split_2_0d	ldy #$01
		stx $d027
		stx $d028
		stx $d029
		stx $d02a
		sty $d02b
		dec $d016
		ldx #split1_col_07
		sta $d016

; Scanline $0e
		sty $d02c
		sty $d02d
		sty $d02e
		ldy #split2_col_07
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_0f	ldx #$01
		sta $d016

; Scanline $0f
		sty $d02c
		sty $d02d
		sty $d02e
split_2_0f	ldy #$01
		stx $d027
		stx $d028
		stx $d029
		stx $d02a
		sty $d02b
		dec $d016
		ldx #split1_col_08
		sta $d016

; Scanline $10
		sty $d02c
		sty $d02d
		sty $d02e
		ldy #split2_col_08
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
split_1_11	ldx #$01
		sta $d016

; Scanline $10
		sty $d02c
		sty $d02d
		sty $d02e
split_2_11	ldy #$09
		stx $d027
		stx $d028
		stx $d029
		sty $d02a
		stx $d02b
		dec $d016
		nop
		sta $d016

		rts

; Guess what kids...
scroll_text	!scr "Welcome to   -=> Koalatro <=-"
		!scr "     "

		!scr "A masochistic exercise in cramming a bitmap (with some "
		!scr "of the colour data packed), music and a scroller "
		!scr "into bank 0 whilst leaving free space for some text!"
		!scr "      "

		!scr "Code, wiring and graphics by T.M.R "
		!scr "with double speed music from aNdy."
		!scr "      "

		!scr "The tune is quite short because it was originally "
		!scr "created for the CSDb loading music compo a few years "
		!scr "back but remained unfinished until I needed something "
		!scr "compact for this release."
		!scr "      "

		!scr "The code was built around the scroller which is based "
		!scr "on one from Super Swap Sweden's ",$22,"Contribution",$22," "
		!scr "- I wanted to do my own version and then got just a "
		!scr "teensy bit [ahem] carried away...!"
		!scr "      "

		!scr "There's almost no memory left now, but greetings to all "
		!scr "of Cosine's friends and this has been T.M.R, powering "
		!scr "down on 2016/12/21... .. .  ."
		!scr "         "
		!byte $00


		* = $3f40

; Colour tables for the moving effect in the scroller
colour_pulse_1	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$06,$0b,$04,$0e,$04,$0b,$06

colour_pulse_2	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$09,$02,$08,$0a,$08,$02,$09
		!byte $00,$00,$00,$00,$00,$00,$00,$00

; Sprite X co-ordinates and data pointers for the scroller
sprite_x	!byte $f0,$28,$58,$88,$b8,$e8,$18,$48
		!byte $c1
sprite_dp	!byte $20,$21,$22,$23,$24,$25,$26,$27