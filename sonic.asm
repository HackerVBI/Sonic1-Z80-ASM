			device zxspectrum128
Sprite_page		equ #18
Tile_page		equ #3f
Vid_page		equ #40
Tile0_spr_page		equ #20

Main_emu_page		equ #10
RAM_PAGE_1	equ $D235
RAM_TEMP1	equ $D20E
RAM_SPRITETABLE equ $D000	;X/Y/I data for the 64 sprites


			org #e400
main
			jp start

			org #e404	
			jp decode_pix
			org #e408	;tile ram adress
tileram_adr		ds 2
			org #e40a	;tilemap ram adress
tilemap_adr		ds 2
			org #e40c	; set_tile_proc
			jr set_tile

			org #e40e	
			jr send_palette

			org #e410
			jp key_scan
			org #e413
			jp send_sprites
			org #e416
			jp set_simple_tile
			org #e419
			jp cls_tileset
			
start:			di
			ld sp,#feff
			call setup
			jp  0                ; start the Pac-Man ROM!

/*
Data format

The data takes up a total of 13 bits, stored in two bytes:
Bit  15 14 13	12	11	  10		9    8 7 6 5 4 3 2 1 0
Data Unused   Priority Palette Vertical  Horizontal Tile number
				 flip	    flip
*/
set_tile		push af
			push hl
			push de
			push bc
			push af
			ld bc,PAGE0
			ld a,Tile_page
			out (c),a
			ld e,0
			ld hl,(tilemap_adr)
			res 7,l
			ld a,(RAM_TEMP1)
			bit 4,a
			jr nz,1f
			set 7,l

1			bit 1,a
			jr z,1f
			set 6,e	;TL_XF

1			bit 2,a
			jr z,1f
			set 6,e	;TL_XF

1			bit 2,a
			jr z,1f
			set 7,e	;TL_YF

1			bit 3,a
			jr z,1f
			set 4,e	;pal

1			pop af
			ld (hl),a
			inc l
			ld a,(RAM_TEMP1)
			and 6		; V / H flips
			rrca
			rrca
			rrca
			or e
			ld (hl),a
			inc l
			ld a,l
			and #3f
			jr nz,1f
			ld l,a
			inc h
1			ld (tilemap_adr),hl
;			ld bc,PAGE0
			xor a
			out (c),a
			pop bc
			pop de
			pop hl
			pop af
			ret


send_palette
; hl - pal, c-pal index
; 	    MSB  LSB
; 	    --BBGGRR
; ts pal: 0RRrrrG gggBBbbb 
			push de
			ex de,hl
			ld hl,pal_db
			or a
			rl c
			ld l,c

1			push bc
			push hl
			ld a,(de)
			inc de
			add a
			ld c,a
			ld b,0
			ld hl,sms_pal
			add hl,bc
			ld c,(hl)
			inc hl
			ld b,(hl)
			pop hl
			ld (hl),c
			inc hl
			ld (hl),b
			inc hl
			pop bc
			djnz 1b
/*
;			ld a,c
;			add a
;			ld (pal_dma_num+1),a
;			ld b,#10
pal_decode		push bc
			ld a,(de)
			and 3	; RR
			or a
			rrca
			rrca
			rrca
			ld b,a	; high pal byte
			ld c,0
			ld a,(de)
			and 8+4	; GG
			or a
			rra
			rra
			rra
			rr c
			or b
			ld b,a
			ld a,(de)
			and #30	; BB
			or a
			rra
			or c
			ld c,a
			inc de
			ld (hl),c
			inc hl
			ld (hl),b
			inc hl
			pop bc
			djnz pal_decode
*/
			ld hl,pal_dma
			call set_ports
			ex de,hl
			pop de
			ret


set_simple_tile
			push hl
			push bc
			push af
			ld bc,PAGE0
			ld a,Tile_page
			out (c),a
			pop af
			ld hl,(tilemap_adr)
			ld (hl),a
			inc l
;			ld a,l
;			and #3f
;			jr nz,1f
;			ld l,a
;			inc h
1			ld (tilemap_adr),hl
			xor a
			out (c),a
			pop bc
			pop hl
			ret



/*
RAM_SPRITETABLE		$D000	;X/Y/I data for the 64 sprites
*/

send_sprites		;ld a,b
			;or a
			;ret z
			push ix
			ld hl,RAM_SPRITETABLE
			ld de,6*2
			ld ix,spr
			ld b, 64/2
2
			ld a,(hl)
			ld (ix+2),a
			ld (ix+2+6),a
			inc l
			set 5,(ix+1) ; SP_ACT
			set 5,(ix+1+6) ; SP_ACT
			ld a,(hl)
			cp 224
			jr c,1f
			res 5,(ix+1)
			res 5,(ix+1+6)
1			ld (ix+0),a
			add 8
			ld (ix+6),a
			inc l
			ld a,(hl)
			ld (ix+4),a
			inc a
			ld (ix+4+6),a
			inc l
			add ix,de
			djnz 2b
			pop ix
			ld hl,sprites
			jp set_ports

/*
The tile data is in a planar format, split by tile row. That means that the first byte contains the least significant bit, 
bit 0, of each pixel in the top row of the tile. The second byte contains bit 1 of each pixel, the third bit 2, and the fourth bit 3. 
Thus the top eight pixels are represented by the first four bytes of data, split by “bitplane”. The process is repeated for consecutive rows of the tile,
 producing 32 bytes total. 
*/
decode_pix		di
			push de
			push bc
			ld bc,PAGE1
			ld a,Tile0_spr_page
			out (c),a
			ld bc,(tileram_adr)
			call rocknroll
			ld (bc),a
			inc c
			call rocknroll
			ld (bc),a
			inc c
			call rocknroll
			ld (bc),a
			inc c
			call rocknroll
			ld (bc),a
			ld hl,(tileram_adr)
			inc h
			ld a,h
			and 7
			or a
			jr nz,1f
			ld a,h
			sub 8
			ld h,a
			inc l
			inc l
			inc l
			inc l
			jr nz,1f
			ld a,h
			add 8
			ld h,a
1			ld (tileram_adr),hl
			ld bc,PAGE1
			ld a,(RAM_PAGE_1)
			out (c),a
			pop bc
			pop de
			ei
			ret


rocknroll		xor a
			ld e,l
			sll (hl)
			rla
			dec l
			sll (hl)
			rla
			dec l
			sll (hl)
			rla
			dec l
			sll (hl)
			rla
			ld l,e
			sll (hl)
			rla
			dec l
			sll (hl)
			rla
			dec l
			sll (hl)
			rla
			dec l
			sll (hl)
			rla
			ld l,e
			ret



setup			ld bc,PAGE2
			ld a,Vid_page
			out (c),a
			ld hl,0		;1111 - white
			ld (#8000),hl
			ld hl,init_ts
			call set_ports
			call spr_off
			ld a,Tile_page
			ld bc,PAGE0
			out (c),a
			ld hl,0
			ld de,1
			ld (hl),l
			ld bc,#3fff
			ldir
/*
			ld hl,0
			ld c,32
			xor a
2			ld b,32
1			ld (hl),a
			inc a
			inc l
			inc l
			djnz 1b
			ld l,0
			inc h
			dec c
			jr nz,2b
*/
			ld bc, PAGE0
			ld a,0
			out (c),a
			inc b
			inc a
			out (c),a
			inc b
			inc a
			out (c),a
			ret

cls_tileset
		ld bc,PAGE1
		ld a,Tile_page
		out (c),a
		ld hl,0
		ld (#4000),hl
		ld hl,tileset_clr
		call set_ports
		ld bc,PAGE1
		ld a,(RAM_PAGE_1)
		out (c),a
		ret

tileset_clr
		defb #1a,0	;
		defb #1b,0	;
		defb #1c,Tile_page	;
		defb #1d,0	;
		defb #1e,0	;
		defb #1f,Tile_page	;
		defb #28,#ff	;
		defb #26,#1f	;
		defb #27,%00000100
		db #ff

/*button: 0 for pressed, 1 for released
  bit 7: Joypad 2 Down
      6: Joypad 2 Up
      5: Joypad 1 Fire B
      4: Joypad 1 Fire A
      3: Joypad 1 Right
      2: Joypad 1 Left
      1: Joypad 1 Down
      0: Joypad 1 Up
*/

fireB 	equ 5
fireA 	equ 4
right	equ 3
left 	equ 2
down 	equ 1
up	equ 0
stop 	equ #ff


key_scan	
		ld l,stop
		LD BC,#effe
		; cs 8
		IN A,(C)
		BIT 2,A
		jr nz,1f
		res right,l

1		IN A,(C)
		BIT 3,A
		jr nz,1f
		res up,l

1		;cs 6
		IN A,(C)
		BIT 4,A
		jr nz,1f
		res down,l

1		LD BC,#f7fe
		IN A,(C)
		;cs 5
		BIT 4,A
		jr nz,1f
		res left,l

1		LD BC,#7FFE	;space
		IN A,(C)
		BIT 0,A
		jr nz,1f
		res fireB,l
1		ld a,l
		ret

init_ts			db high VCONFIG,VID_320X240+VID_NOGFX
			db high MEMCONFIG,%00001110
			db high VPAGE,Vid_page
			db high SYSCONFIG,6	; 
			db high BORDER,0	; border
			db high TSCONFIG,TSU_SEN+TSU_T0EN+TSU_T0ZEN+TSU_T1EN +TSU_T1ZEN		; TSConfig
			db high PALSEL,0
			db high SGPAGE,Tile0_spr_page
			db high TMPAGE, Tile_page
			db high T0GPAGE,Tile0_spr_page
			db high T1GPAGE,Tile0_spr_page


pal_dma			db #1a,low pal_db
		        db #1b,high pal_db
			db #1c,Main_emu_page
		        db #1d,0
		        db #1e,0
		        db #1f,0
		        db #26,#20
		        db #28,0
			db #27,#84
			db #ff

sprites		db #1a,low spr_db
		db #1b,high spr_db
		db #1c,Main_emu_page
		db #1d,0
		db #1e,2
		db #1f,0
		db #26,(spr_db_end-spr_db)/2
		db #28,0
		db #27,DMA_RAM_SFILE
		db #ff

	align 2
spr_db		
		DB 0
		DB %01000000	; leap
		DB 0
		DB %00010000
		DB 0
		DB %11100000

		
		DB 0
		DB %01000000	; leap
		DB 0
		DB %00010000
		DB 0
		DB %11100000


spr
		dup 64
		db 0		;y
		db SP_SIZE8+SP_ACT
		db #0		;x
		db SP_SIZE8
		db #0
		db #10+1
		edup
spr_db_end

	align 256
pal_db	ds 32*2

/*			
clr_screen
			defb #1a,0	;
			defb #1b,0	;
			defb #1c,Vid_page	;
			defb #1d,0	;
			defb #1e,0	;
			defb #1f,Vid_page	;

			defb #28,200	;
			defb #26,#ff	;
			defb #27,%00000100
*/
			db #ff

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	include "includes.asm"
	include "../tsconfig.asm"
sms_pal		incbin "_spg/sms.pal"


end


		SAVEBIN "_spg/emu.bin",main, end-main