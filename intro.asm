			device zxspectrum128
Sprite_page		equ #18
Tile_page		equ #3f
Vid_page		equ #40
Tile0_spr_page		equ #30

Main_emu_page		equ #10
muz_page		equ #11
screen_page		equ #13

		org #c000

start:		di
		nop
		ld sp,#ffff
		call sprite_off
		call font_conversion
		ld bc,PAGE2
		ld a,Tile0_spr_page
		out (c),a
		ld hl,#dddd
		ld (#8000),hl
		ld hl,clr_tiles
		call set_ports
		call dma_stats
		ld hl,#8000
		ld b,360/2
		ld a,$92
1		ld (hl),a
		inc l
		djnz 1b
		ld hl,#8e00
		ld b,360/2
		ld a,$29
1		ld (hl),a
		inc l
		djnz 1b

		ld hl,#9000
		ld b,8
		xor a
1		ld (hl),a
		inc l
		ld (hl),a
		inc l
		ld (hl),a
		inc l
		ld (hl),a
		ld l,0
		inc h
		djnz 1b

		ld bc,PAGE2
		ld a,Vid_page
		out (c),a
		ld hl,0
		ld (#8000),hl
		ld hl,init_ts
		call set_ports
		call dma_stats
		ld b,high DMALEN
		ld a,40
		out (c),a
		ld b,high DMASTATUS
		ld a,DMA_FILL
		out (c),a

		xor a
		ld bc,GXOFFSL
		ld e,4
		call fill_ports
		ld b,high T0XOFFSL
		ld e,8
		call fill_ports

		ld hl,#0080
		call clear_tileset0
		ld hl,scroll_buffer
		ld de,scroll_buffer+1
		ld (hl),#db
		ld bc,chunk_x*char_height-1
		ldir
		ld hl,pal_dma
		call set_ports

		call rnd_screen
		ld hl,screen
		call set_ports
		ld hl,#0000
		ld de,#0000
		ld bc,#2d02;(chunk_x/4)*256+2
		ld a,Tile_page
		call tile_filler_page0

		ld a,#20
		ld bc,T0YOFFSL
		out (c),a
		inc b
		ld a,1
		out (c),a


		ld bc,PAGE2
		ld a,muz_page
		out (c),a
		call #8000

		ld a,#f0
		ld i,a
		ld hl,int
		ld (#f0ff),hl
		im 2
		ei
		halt
		ld bc,VCONFIG
		ld a,VID_360X288+VID_256C
		out (c),a

intro_		halt
		call scroll_pal
rol		ld a,#ff
		inc a
		and 7
		ld (rol+1),a
		call z,scroll_txt
		call roll_scroll
		call scroll_view
sin1		ld a,0
		inc a
		ld (sin1+1),a
		ld h,high sin
		ld l,a
		inc (hl)
		ld l,(hl)
		ld a,(hl)
		ld bc,T0YOFFSL
		out (c),a

		call #8005
		LD BC,#F7FE	;1
		IN A,(C)
		BIT 0,A
		jr nz,1f
		ld a,3	; bridge
		jr exit

1		BIT 1,A
		jr nz,1f
		ld a,6
		jr exit

1		BIT 2,A
		jr nz,1f
		ld a,9
		jr exit

1		BIT 3,A
		jr nz,1f
		ld a,#0c
		jr exit

1		BIT 4,A
		jr nz,1f
		ld a,#10	; sky base
		jr exit

1		LD BC,#EFFE	;0
		IN A,(C)
		BIT 0,A
		jr nz,1f
		ld a,#ff	; immortal
		jr exit

1		LD BC,#7FFE	;space
		IN A,(C)
		BIT 0,A
		jr nz,intro_
		xor a
exit		ld (#e000),a
		call #8000
		ld bc,PAGE2
		ld a,Vid_page
		out (c),a
		ld hl,0
		ld (#8000),hl
		ld hl,init_ts
		call set_ports
		ld b,high TSCONFIG
		xor a
		out (c),a
		ld b,24
1		halt
		DJNZ 1B
		jp #e400

int 		ei
		ret


scroll_view	ld hl,scroll_blt
		call set_ports
		ld de,#200+DMA_RAM+DMA_DALGN
		ld l,6
svn1		ld b,#1e
		out (c),d
		ld b,#27
		out (c),e
		call dma_stats
		inc d
		inc d
		dec l
		jr nz,svn1
		ret

roll_scroll	

		ld hl,scroll_buffer+1
		ld de,scroll_buffer
		ld bc,chunk_x*char_height-1
		ldir

scroll_color	ld a,0
		inc a
		and #1
		ld (scroll_color+1),a
		add 4
		rrca
		rrca
		rrca
		rrca
;		ld (rsc2+1),a
		add #10
;		ld (rsc3+1),a
roll_scroll_char
		ld hl,0
		ld a,(hl)
		inc hl
		ld (roll_scroll_char+1),hl
		ld hl,scroll_buffer+chunk_x-1
		ld de,chunk_x
	
		ld bc,#0379
rsc2		ld (hl),#db	; empty
		rla
		jr nc,1f
		ld (hl),c
1		add hl,de
rsc3		ld (hl),#bd	
		rla
		jr nc,2f
		ld (hl),c
2		add hl,de
		djnz rsc2
		ret

scroll_txt	ld hl,text
		ld a,(hl)
		inc hl
		cp #ff
		jr nz,st1
		ld hl,text
		ld a,(hl)
st1		ld (scroll_txt+1),hl
		ld l,a
		ld h,0
		add hl,hl
		add hl,hl
		add hl,hl
		ld de,fnt-#100
		add hl,de
		; ------------
;		inc hl
;		inc hl
; ------------
		ld (roll_scroll_char+1),hl
		ret


font_conversion
		ld hl,conv_fnt
;		ld de,#4000
		ld de,fnt
		ld c,#40	;#20 " " - #5F "_"
fc1		ld b,8
fc2		xor a
		push hl
		or a
		dup 8
		rl (hl)
		rla
		inc hl
		edup
		ld (de),a
		inc de
;		call incd
		pop hl
		djnz fc2
		ld a,l
		add 8
		ld l,a
		ld a,0
		adc h
		ld h,a
		dec c
		jr nz,fc1
		ret


scroll_pal	
		ld hl,pal_db+#c0*2
		ld e,(hl)
		inc hl
		ld d,(hl)
		ld hl,pal_roll
		call set_ports
		call dma_stats
		ld hl,pal_db+#ff*2
		ld (hl),e
		inc hl
		ld (hl),d
		ld hl,pal_dma
		jp set_ports



rnd_screen	
		ld hl,#8000
1		exx
		call rnd
		exx
		or #c0
		ld b,3
		call rfill
		inc hl
		bit 1,h
		jr z,1b
		ld a,h
		add 6
		ld h,a
		cp #c0
		jr nz,1b
		ld bc,PAGE2
rs1		ld a,Vid_page
		inc a
		ld (rs1+1),a
		cp Vid_page+9
		ret z
		out (c),a
		ld h,#80
		jr 1b

rfill		ld d,h
2		ld (hl),a
		inc h
		inc h
		ld (hl),a
		inc h
		inc h
		ld (hl),a
		ld h,d
		inc l
		djnz 2b
		ret

sprite_off	ld bc, FMADDR
		ld a, FM_EN+#8
		out (c), a      ; open FPGA arrays at #0000
		; clean SFILE

FM_SFILE        equ #0200+#8000
		ld hl,FM_SFILE
		xor a
spr_off_l1	ld (hl), a
		inc l
		jr nz,spr_off_l1
		inc h
spr_off_l2	ld (hl), a
		inc l
		jr nz,spr_off_l2
		out (c), a      ; close FPGA arrays at #0000
		ret


;chunk_y		equ char_height
;chunk_x		equ 160	;64
scroll_blt	db #1a,low scroll_buffer
		db #1b,high scroll_buffer
		db #1c,Main_emu_page
	        db #1d,#0
	        db #1e,#10
	        db #1f,Tile0_spr_page
	        db #26,chunk_x/2-1
	        db #28,0
;		db #27,DMA_RAM + DMA_DALGN
		db #ff

screen
		db #1a,0
	        db #1b,0
		db #1c,screen_page
	        db #1d,76+4
	        db #1e,60
	        db #1f,Vid_page
	        db #26,208/2-1
	        db #28,208-1
		db #27,DMA_DALGN + DMA_ASZ + DMA_BLT ;DMA_RAM
		db #ff


init_ts			db high VCONFIG,VID_360X288+VID_256C+VID_NOGFX
			db high MEMCONFIG,%00001110
			db high SYSCONFIG,6	; 
			db high VPAGE,Vid_page
			db high BORDER,0	; border
			db high TSCONFIG,TSU_T0EN+TSU_T0ZEN;+TSU_T1EN +TSU_T1ZEN		; TSConfig
			db high PALSEL,0;+#10
			db high TMPAGE, Tile_page
			db high T0GPAGE,Tile0_spr_page

clr_screen
		defb #1a,0	;
		defb #1b,0	;
		defb #1c,Vid_page	;
		defb #1d,0	;
		defb #1e,0	;
		defb #1f,Vid_page	;
		defb #28,#ff	;
		defb #26,#ff	;
		defb #27,DMA_FILL
		db #ff

clr_tiles
		defb #1a,0	;
		defb #1b,0	;
		defb #1c,Tile0_spr_page
		defb #1d,0	;
		defb #1e,0	;
		defb #1f,Tile0_spr_page
		defb #28,#ff	;
		defb #26,#2f	;
		defb #27,DMA_FILL
		db #ff

pal_dma			db #1a,low pal_db
		        db #1b,high pal_db
			db #1c,Main_emu_page
		        db #1d,0
		        db #1e,0
		        db #1f,0
		        db #26,#ff
		        db #28,0
			db #27,#84
			db #ff

pal_roll
			db #1a,low (pal_db+#c0*2+2)
		        db #1b,high (pal_db+2+#c0*2)
			db #1c,Main_emu_page
		        db #1d,low (pal_db+#c0*2)
		        db #1e,high (pal_db+#c0*2)
		        db #1f,Main_emu_page
		        db #26,#40-1
		        db #28,0
			db #27,DMA_RAM
			db #ff

	include "includes.asm"
	include "tsconfig.asm"

	align 2
pal_db	incbin "_spg/logo.tga.pal"


char_height	equ 6	;5
char_pos_y	equ 5

chunk_y		equ char_height
chunk_x		equ 360/2	;64

conv_fnt	incbin "_spg/fon.$C.bin"
fnt 		ds 768

	align 256
sin 	incbin "_spg/tab1.bin"
scroll_buffer	equ #f800
;		equ chunk_buffer + chunk_y*char_pos_y
	align 256
chunk_buffer	ds chunk_y*chunk_x

text	db " HI!   ENHANCERS PROUD TO PRESENT FOR YOU OUR NEW GEM - SONIC THE HEDGEHOG (C) SEGA MASTER SYSTEM.   "
	db " < PRESS SPACE TO START >   HAPPY BIRTHDAY TO TSL! THIS IS GIFT FOR YOU! "
	db " FOR ZONE CHANGE, PRESS: 1 - BRIGE, 2 - JUNGLE, 3 - LABYRINTH, 4 - SCRAP BRAIN, 5 - SKY BASE."
	db "     HI DEMOSCENE GUYS!    YOU KNOWN AS CONSCIOUSNESS GROUP - AND YES, WE ARE! "
	DB " AND OUR NEW NAME IS - ENHANCERS!   BUT, WE STILL ENLARGE YOUR CONSCIOUSNESS! ;)           "
	DB " CREDITS: GAME PROGRAM - SHINOBU HAYASHI, SEGA. REALLY COOL ENGINE!   "
	DB " PORTED TO ZX ENHANCED; INTRO CODE, MUSIC: HACKER VBI, 12/05 - 14/06/2018!  "
	DB " BABY, YEA! I DO IT ONLY ONE!)   THANKS TO FLEXX FOR MORAL SUPPORT!  AND NOW - HELLO TO ALL MY FRIEND: "
	db " ROOK, ROB F., FLEXX, TSL,  KALANTAY, WBC, SLIDER, PANDA, KENOTRON, DENIS GRACHEV, INTROSPEC, MR287CC, MOROZ1999, QUIET, "
	DB " NIK-O, KOSHI, KOWALSKI, KOTSOFT, BLADE, TREFI, NYUK, R0BAT, PSNDCJ, DIVER, NODEUS, TMK, C-DJEFF, SQ, BUYAN, BFOX, BREEZE, "
	db " PSB, DDP, MMCM, SAND, ATURBIDFLOW... AND - YOU!!!                             "
	DB #FF


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end

		SAVEBIN "_spg/intro.bin",start, end-start