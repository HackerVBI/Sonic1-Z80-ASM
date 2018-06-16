			device zxspectrum128
Sprite_page		equ #18
Tile_page		equ #3f
Vid_page		equ #40
Tile0_spr_page		equ #30
rom_page		equ #20
Main_emu_page		equ #10
tilemem			equ #11

RAM_PAGE_1	equ $D235
RAM_SPRITETABLE equ $D000	;X/Y/I data for the 64 sprites

viewer=1


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
;			jr set_tile
			org #e40e	
			jr send_palette

			org #e410
			jp key_scan
			org #e413
			jp send_sprites
			org #e416
			jp update_tilemem
			org #e419
			jp cls_tileset
			org #e41c
			jp view_tilemem
			org #e41f
			jp update_2byte_tilemem
			org #e422
			jp ay_send
			org #e425
			jp ay_send_byte

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
			push hl
			ld hl,pal_db
			ld c,(hl)
			inc l
			ld b,(hl)
			ld hl,pal_db+32*2+2
			ld (hl),c
			inc l
			ld (hl),b
			pop hl
			ld hl,pal_dma
			call set_ports
			ex de,hl
			pop de
			ret

ay_control	equ $FFFD
ay_data		equ $BFFD

ay_send_byte	
			push bc
			push de
			push hl
			ld d,a
			and #f0
			cp #f0
			jr z,ay_noise
			ld a,d
			call ay_sel
			ld a,d		; 4 bits of volume
			cpl 
			and #0f
			jr ay_send_ex

ay_noise		ld a,(ay_num+1)
			cp 4
			jr z,ay_send_ex2
			call _SC2
			ld a,9
			ld b,high ay_control
			out (c),a
			ld a,d		; most 6 bits
			cpl
			and #0f
			or a
			jr z,1f		
			add 3
;			and #0f
1			ld b,high ay_data
			out (c),a
			call _SC1
			jr ay_send_ex2


ay_send			push af
ay_send1		ld a,0
			cpl
			ld (ay_send1+1),a
			or a
			jr z,1f
			pop af
			ld (two_sn_bytes+1),a
			ret

1			pop af
			ld (two_sn_bytes),a
			push bc
			push de
			push hl
			ld de,(two_sn_bytes)
			ld a,d
			call ay_sel
			ld (ay_reg+1),a
ay_tone			ld a,e		; most 6 bits
			and #3f
			push af
			rra		
			or a
			rra		; 4 bits course pitch ay
			ld b,high ay_data
			out (c),a
ay_reg			ld a,0
			dec a		; fine pitch
			ld b,high ay_control
			out (c),a
			pop af
			ld e,0
			or a
			rra
			rr e
			rra
			rr e
			ld a,d
			and #0f
			rla
			rla
			or e
ay_send_ex		ld b,high ay_data
			out (c),a
ay_send_ex2		pop hl
			pop de
			pop bc
			ret

ay_sel			and #f0		; command + least low 4 bits
			rlca
			rlca
			rlca
			rlca
			ld l,a
			ld h,0
			ld bc,ay_channels
			add hl,bc
			ld a,(hl)
			ld bc,ay_control
			out (c),a
			ret

two_sn_bytes		dw 0
			;  0,1,2,3,4,5,6,7,8,9,a,b, c,d,e, f
ay_channels		db 0,0,0,0,0,0,0,0,1,8,5,10,3,9,6,16



view_tilemem
			di
;			ld a,2
;			out (#fe),a

			push bc
			ld a,Tile_page
			ld bc,PAGE1
			out (c),a
			pop bc	
	IF viewer=2
; simple screen viewer
			ld hl,0
			ld de,#4000
			ld bc,32*256+#1d
1			push bc
2			ld a,(hl)
			ld (de),a
			inc hl
			inc de
			xor a
			ld (de),a
			inc hl
			inc de
			djnz 2b
			pop bc
			ld e,0
			inc d
			dec c
			jr nz,1b
	ENDIF

	IF viewer=0

; simple tile viewer
			ld de,#1000
			ld hl,#4a00
			ld bc,32*256+#20
1			push bc
2			ld (hl),e
			inc hl
			ld (hl),d
			inc hl
			inc de
			djnz 2b
			pop bc
			ld l,0
			inc h
			dec c
			jr nz,1b
	ENDIF


	IF viewer=1

			ld a,b
			and %11111000		;round the scroll to the nearest 8 pixels
			;multiply the vertical scroll offset by 8. since the scroll offset is already
			 ;a multiple of 8, this will give you 64 bytes per screen row (32 16-bit tiles)
			ld h,0
			add a			;x2
			rl h
			add a			;x4
			rl h
			add a			;x8
			rl h
			ld ixl,a
;			ld (start_tile_x+1),a
			ld a,h
			add #f8
			ld h,a
			ld a,c
			and %11111000		;and then round to the nearest 8 pixels
			srl a			;divide by 2 ...
			srl a			;divide by 4
			and #3f
			ld l,a

			exa 
			ld a,l
			exa
			ld de,#4000		; TileMap adress
			ld c,25			; screen height
vt1			ld b,32/8			; screen width

vt2
	dup 8
			exa
			add 2
			and #3f
			ld l,a
			exa
			ld a,l
;start_tile_x
			add ixl
			ld l,a
			ld a,(hl)
			ld (de),a
			inc e
			inc e
	edup
			djnz vt2
			ld e,b
			inc d
;			exa
;			add 2
;			exa
;			ld a,(start_tile_x+1)
			ld a,ixl
			add #40
			ld ixl,a
;			ld (start_tile_x+1),a
			ld a,0
			adc h
			cp #f8+7
			jr c,1f
			ld a,#f8
1			ld h,a
			dec c
			jp nz,vt1

	ENDIF		
			ld a,(RAM_PAGE_1)
			ld bc,PAGE1
			out (c),a
;			out (#fe),a
			ei
			ret

/*
Data format

The data takes up a total of 13 bits, stored in two bytes:
Bit  15 14 13	12	11	  10		9    8 7 6 5 4 3 2 1 0
Data Unused   Priority Palette Vertical  Horizontal Tile number
				 flip	    flip
*/
/*
set_tile		
			push af
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
*/




update_tilemem		push hl
			ld hl,(tilemap_adr)
			ld (hl),a
			inc hl
			ld (tilemap_adr),hl
			pop hl
			ret

update_2byte_tilemem	push hl
			ld hl,(tilemap_adr)
			inc l
			inc hl		
			ld (tilemap_adr),hl
			pop hl
			ret

/*
RAM_SPRITETABLE		$D000	;X/Y/I data for the 64 sprites
*/

send_sprites		
;		ld a,1
;		out (#fe),a	
			push ix
			ld hl,RAM_SPRITETABLE
			ld ix,spr_db_end-6*2
			ld e,0
sc1			ld a,l
			cp #c0
			jr z,sp_ex
			ld b,(hl)	; I
			inc l
			ld a,(hl)	; Y
			inc l
			ld c,(hl)	; X
			inc l
			cp #e0
			jr z,sc1

			ld (ix+6),a	; Y
			add 8
			ld (ix+0),a

			ld a,b		; X
			sub 8
			cp 256-7
			jr nc,sc1

			ld (ix+2),a
			ld (ix+2+6),a

			ld a,c		; I
			ld (ix+4+6),a
			inc a
			ld (ix+4),a

			ld a,SP_SIZE8+SP_ACT
			ld (ix+1),a
			ld (ix+1+6),a
			xor a
			ld (ix+3),a
			ld (ix+3+6),a
			ld a,#10+1
			ld (ix+5),a
			ld (ix+5+6),a

			or a
			ld a,ixl
			sub 12
			ld ixl,a
			jr nc,1f
			jr z,1f
			dec ixh
1
			inc e
			ld a,e
			cp 82/2
			jr nz,sc1
			jr sp_ex1

sp_ex			ld a,82/2
			sub e
			ld b,a
			push ix
			pop hl
			inc hl
			ld de,6
			add hl,de
2			xor a
			ld (hl),a
			sbc hl,de
			ld (hl),a
			sbc hl,de
1			djnz 2b

sp_ex1			pop ix
			ld hl,sprites
			jp set_ports

/*
2			ld a,(hl)
			inc l
			sub 7
			cp 256-7
			jr nc,1f

3			ld (ix+2),a
			ld (ix+2+6),a

			ld a,(hl)

			set 5,(ix+1) ; SP_ACT
			set 5,(ix+1+6) ; SP_ACT
			ld (ix+6),a
			add 8
			ld (ix+0),a
			jr 3f
1			res 5,(ix+1)
			res 5,(ix+1+6)
			inc l
			jr 5f

3			inc l
			ld a,(hl)
			ld (ix+4+6),a
			inc a
			ld (ix+4),a
5			inc l
			or a
			ld a,ixl
			sub 12
			ld ixl,a
			jr nc,1f
			jr z,1f
			dec ixh
1
;			sbc ix,de
6			djnz 2b
			pop ix
			ld hl,sprites
			jp set_ports
;		xor a
;		out (#fe),a
;		ret
*/

/*
The tile data is in a planar format, split by tile row. That means that the first byte contains the least significant bit, 
bit 0, of each pixel in the top row of the tile. The second byte contains bit 1 of each pixel, the third bit 2, and the fourth bit 3. 
Thus the top eight pixels are represented by the first four bytes of data, split by “bitplane”. The process is repeated for consecutive rows of the tile,
 producing 32 bytes total. 
*/
decode_pix		
			di
			push de
			push bc
			ld bc,PAGE1
			push bc
			ld a,Tile0_spr_page
			out (c),a
			ld bc,(tileram_adr)
			push bc
			ld e,(hl)
			dec l
			ld d,(hl)
			dec l
			ld a,(hl)
			dec l
			ld h,(hl)
			ld l,a
			xor a
			rl e
			rla
			rl d
			rla
			rl l
			rla
			rl h
			rla
			rl e
			rla
			rl d
			rla
			rl l
			rla
			rl h
			rla
			ld (bc),a
			inc c
			xor a
			rl e
			rla
			rl d
			rla
			rl l
			rla
			rl h
			rla
			rl e
			rla
			rl d
			rla
			rl l
			rla
			rl h
			rla
			ld (bc),a
			inc c
			xor a
			rl e
			rla
			rl d
			rla
			rl l
			rla
			rl h
			rla
			rl e
			rla
			rl d
			rla
			rl l
			rla
			rl h
			rla
			ld (bc),a
			inc c
			xor a
			rl e
			rla
			rl d
			rla
			rl l
			rla
			rl h
			rla
			rl e
			rla
			rl d
			rla
			rl l
			rla
			rl h
			rla
			ld (bc),a
			pop bc
			inc b
			ld a,b
			and 7
			or a
			jr nz,1f
			ld a,b
			sub 8
			ld b,a
			ld a,c
			add 4
			ld c,a
			jr nz,1f
			ld a,b
			add 8
			ld b,a
1			ld (tileram_adr),bc
			pop bc
			ld a,(RAM_PAGE_1)
			out (c),a
			pop bc
			pop de
			ei
			ret

start:			di
			call check_ts
			ld (ay_num+1),a
; 4 — single AY (FE FE)
; 3 — double AY (FD FE)
			ld sp,stack
			call spr_off
			ld hl,1
			call cls_tileset
			ld hl,copy_rom
			call set_ports
			call dma_stats
			ld b,#27
			ld a,DMA_RAM
			out (c),a
			call dma_stats

			ld bc,PAGE2
			push bc
			ld a,Tile0_spr_page+1
			out (c),a
			ld a,#11
			ld b,4
			ld hl,#8000
1			ld (hl),a
			inc h
			bit 3,h
			jr z,1b
			ld h,#80
			inc l
			djnz 1b
			out (c),a
			pop bc
			ld a,Vid_page
			out (c),a
			ld hl,$0		;1111 - white
			ld (#8000),hl
			ld hl,init_ts
			call set_ports
			ld bc, PAGE0
			xor a
			out (c),a
			inc b
			inc a
			out (c),a
			inc b
			inc a
			out (c),a
;			ld b,high MEMCONFIG
;			ld a,MEM_W0RAM+MEM_W0MAP_N
;			out (c),a
			ld b,high VSINTL
			ld a,#ff;#10
			out (c),a
;			inc b
;			ld a,1
;			out (c),a
			call _SC1
			ld hl,snd_init		
			call init_volume
ay_num			ld a,0
			cp 4
			jr z,1f
			call _SC2
			call init_volume
			ld d,2
			call init_volume1
1			ld a,#3f
			ld i,a
			rst 0                ; start the Pac-Man ROM!

init_volume		ld d,8
init_volume1		ld bc,ay_control
			ld a,(hl)
			out (c),a
			inc hl
			ld a,(hl)
			ld b,high ay_data
			out (c),a
			inc hl
			dec d
			jr nz,init_volume1
			ret	

snd_init	db 7,#e0,8,0,9,0,10,0
		db 7,#f8,8,0,9,0,10,0
		db #6,#1f

_SC1		LD BC, #FFFD
		LD A, #FE
		OUT (C), A
		RET
 
; Set Chip 2: 

_SC2		LD BC, #FFFD
		LD A, #FF
		OUT (C), A
		RET

cls_tileset	
		ld bc,PAGE1
		ld a,Tile_page
		out (c),a
		ld (#4000),hl
		push hl
		ld hl,tileset_clr
		call set_ports
		call dma_stats
		pop hl
		ld a,l
		or a
		jr nz,1f
		ld hl,#4000+32*2+#80-2
		ld b,24
		xor a
2		ld (hl),a
		inc l
		ld (hl),2+#20
		dec l
		inc h
		djnz 2b
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

	include "includes.asm"
	include "tsconfig.asm"

init_ts			db high VCONFIG,VID_320X240+VID_NOGFX
			db high MEMCONFIG,%00001110
			db high VPAGE,Vid_page
			db high SYSCONFIG,6
			db high BORDER,0	; border
			db high TSCONFIG,TSU_SEN+TSU_T0EN+TSU_T0ZEN+TSU_T1EN +TSU_T1ZEN		; TSConfig
			db high PALSEL,0
			db high T0YOFFSH,0
			db high SGPAGE,Tile0_spr_page
			db high TMPAGE, Tile_page
			db high T0GPAGE,Tile0_spr_page
			db high T1GPAGE,Tile0_spr_page
			db #ff

copy_rom		db #1a,0
		        db #1b,0
			db #1c,rom_page
		        db #1d,0
		        db #1e,0
		        db #1f,0
		        db #26,#ff
		        db #28,#ff
			db #27,DMA_RAM
			db #ff

pal_dma			db #1a,low pal_db
		        db #1b,high pal_db
			db #1c,Main_emu_page
		        db #1d,0
		        db #1e,0
		        db #1f,0
		        db #26,#21
		        db #28,0
			db #27,#84
			db #ff

sprites		db #1a,low spr_db
		db #1b,high spr_db
		db #1c,Main_emu_page
		db #1d,0
		db #1e,2
		db #1f,0
		db #26,#ff
		db #28,0
		db #27,DMA_RAM_SFILE
		db #ff

	align 2
spr_db		

		DB 0
		DB %01000000	; leap
		DB 0
		DB 0
		DB 0
		DB 0

/*		
		DB 0
		DB %01000000	; leap
		DB 0
		DB %00010000
		DB 0
		DB %11100000
*/

spr
	ds 82*6
spr_db_end
	
		DB 0
		DB %01000000	; leap
		DB 0
		DB 0
		DB 0
		DB 0
		
		DB 0
		DB %01000000	; leap
		DB 0
		DB 0
		DB 0
		DB 0

	align 256
pal_db	ds 32*2+4


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

sms_pal		incbin "_spg/sms.pal"


stack	equ #fff0

check_ts	
	ld de,#ffbf
	ld bc,#fffd
	ld hl,#fe00
	out (c),h ;chip1
	out (c),L ;reg 0
	ld b,e
	out (c),c ;val #FD
	ld b,d
	out (c),b ;chip2
	out (c),l ;reg 0
	ld b,e
	out (c),h ;val #FE
	ld b,d
	out (c),h ;chip1
	out (c),L ;reg 0
	in h,(c)
	out (c),b ;chip2
	out (c),L ;reg 0
	in a,(c)
	xor h
	ret nz
	ld a,4
	inc h
	and h
	ret

; 0 — no chip (FF FF)
; 4 — single AY (FE FE)
; 3 — double AY (FD FE)
; 1 — TS, no 1st (FF FE)
; 2 — TS, no 2nd (FD FF)
end

		DISPLAY "len: ",end-main
		SAVEBIN "_spg/emu.bin",main, end-main