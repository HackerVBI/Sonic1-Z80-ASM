			device zxspectrum128
Sprite_page		equ #18
Tile_page		equ #3f
Vid_page		equ #40
Tile0_spr_page		equ #20

Main_emu_page		equ #10
muz_page		equ #11
screen_page		equ #13

		org #c000

start:		di

		ld sp,#ffff
		call sprite_off
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

		ld b,high GXOFFSL
		ld e,4
		call fill_ports
		ld b,high T0XOFFSL
		ld e,8
		call fill_ports
;		ld hl,pal_db+#76*2
;		ld de,pal_db+512-(#40*2)
;		ld bc,#80
;		ldir

		ld hl,pal_dma
		call set_ports
		call rnd_screen
		ld hl,screen
		call set_ports
		ld b,high VCONFIG
		ld a,VID_360X288+VID_256C
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
1		halt
		call scroll_pal
/*
ss1		ld a,0
		inc a
		and 1
		ld (ss1+1),a
		ld bc,GXOFFSL
		out (c),a
		ld b,GYOFFSL
		out (c),a
*/
		call #8005
		LD BC,#7FFE	;space
		IN A,(C)
		BIT 0,A
		jr nz,1b
		call #8000
		ld bc,PAGE2
		ld a,Vid_page
		out (c),a
		ld hl,0
		ld (#8000),hl
		ld hl,init_ts
		call set_ports

		ld b,#40
1		halt
		DJNZ 1B
		jp #e400                ; start the Pac-Man ROM!

int 		ei
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

screen
		db #1a,0
	        db #1b,0
		db #1c,screen_page
	        db #1d,76+8
	        db #1e,18
	        db #1f,Vid_page
	        db #26,208/2-1
	        db #28,216-1
		db #27,DMA_DALGN + DMA_ASZ + DMA_BLT ;DMA_RAM
		db #ff


init_ts			db high VCONFIG,VID_360X288+VID_256C+VID_NOGFX
			db high MEMCONFIG,%00001110
			db high SYSCONFIG,6	; 
			db high CacheConfig,7
			db high VPAGE,Vid_page
			db high BORDER,0	; border
			db high TSCONFIG,0;TSU_T0EN+TSU_T0ZEN;+TSU_T1EN +TSU_T1ZEN		; TSConfig
			db high PALSEL,0
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

text	db " HELLO! GUYS FROM ZX ENHANCED TEAM PROUD TO PRESENT FOR YOU OUR NEW GEM - SONIC THE HEDGEHOG, FROM SEGA MASTER SYSTEM,"
	db " PORTED TO ZX ENHANCED BY HACKER VBI, 19/05 - 09/06/2018!     HI DEMOSCENE GYUS!    YOU KNOWN AS CONSCIOUSNESS GROUP"
	db " - AND YES, WE ARE! "
	DB " AND OUR NEW NAME IS - ZX ENHANCED TEAM!   BUT, WE STILL ENLARGE YOUR CONSCIOUSNESS! ;)           "
	DB " CREDITS: GAME BY SEGA. UNBELIEVABLE COOL ENGINE!   "
	DB " HARDWARE ENGINE: TS LABS.    GAME PORT TO ZX ENHANCED BY HACKER VBI.     INTRO CODING AND MUSIC: HACKER VBI.       "
	DB " BABY, YEA! I DO IT ONLY ONE!)      HELLO TO ALL MY FRIEND: ROB F., FLEXX, KALANTAY, SLIDER, WBC, PANDA, KENOTRON, INTROSPEC, "
	DB " NIK-O, TREFI, NU	K, PSNDCJ, DIVER,NODEUS, TMK, C-DJEFF, BUYAN, BREEZE, CHKALOV, AND - YOU!!!                               "
	DB #FF

	align 2
pal_db	incbin "_spg/logo.tga.pal"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	include "includes.asm"
	include "tsconfig.asm"

end

		SAVEBIN "_spg/intro.bin",start, end-start