INCLUDE "hardware.inc"

SECTION "MainHRAM", HRAM
    hFoo:: db

SECTION "rst", ROM0
; ds $40

SECTION "vblank", ROM0
    jp VBlank

SECTION "lcd", ROM0[$0048]
    reti

SECTION "timer", ROM0[$0050]
    reti

SECTION "serial", ROM0[$0058]
    reti

SECTION "joypad", ROM0[$0060]
    reti

SECTION "Header", ROM0[$0100]
EntryPoint:
    jp Main

; Fill this area with zeros to make sure nothing else allocates to the header
ds $50 - 3


SECTION "DMA", WRAM0
OUT:
    ; Support up to 16 tiles (16 bytes each)
    ds 16*16
OUT_BOTTOM:


SECTION "MainMem", WRAM0

VBlankRoutine:
    dw

NumTileChunks:
    db
TileChunkCounter:
    db
TileChunkPtr:
    dw
CurTileChunkPtr:
    dw
Render_TileStart:
    db
Render_NumTiles:
    db

SECTION "Main",ROM0

MACRO SET_VBLANK
    ld a, LOW(\1)
    ld [VBlankRoutine], a
    ld a, HIGH(\1)
    ld [VBlankRoutine+1], a
ENDM

Main:
    ld sp, $ffff
    call ClearWRam
    ld sp, $dfff
    call ClearHRam

    SET_VBLANK VBlank_Init

    call DoubleSpeed
    call CopyShaderCode


    ; Shut down audio circuitry
    ; ld a, 0
    ; ld [rNR52], a
    ld a, $8f
    ld [rNR52], a
    ld a, $ff
    ld [rNR51], a
    ld a, $ff
    ld [rNR50], a

    ; Do not turn the LCD off outside of VBlank
.waitvbank:
    ld a, [rLY]
    cp 144
    jr c, .waitvbank

    ; We're in vblank now

    ; Turn the LCD off
    ld a, 0
    ld [rLCDC], a

    call SetPalette
    ; ...

    ; Turn the LCD on
    ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BLK21
    ld [rLCDC], a

    ; During the first (blank) frame, initialize display registers
    ld a, %11100100
    ld [rBGP], a

    ld a, IEF_VBLANK
    ld [rIE], a
    ei

.loop

    jr .loop

MACRO HEXCOLOR
    dw ((\1) >> 3) | (((\2) >> 3) << 5) | (((\3) >> 3) << 10)
ENDM

Palette:
    ; #221920
    ; #594156
    ; #77557C
    ; #FFFFFF

    HEXCOLOR $22, $19, $20
    HEXCOLOR $59, $41, $56
    HEXCOLOR $77, $55, $7C
    HEXCOLOR $FF, $FF, $FF

SetPalette:
    ; Write to the BG palette RAM at address 0. Bit 7 = auto-increment.
    ld a, $80
    ldh [rBCPS], a

    ld hl, Palette
    ld b, 8
.copy_loop:
    ld a, [hl+]
    ldh [rBCPD], a
    dec b
    jr nz, .copy_loop

    ret


; Writes the tilemap to VRAM, and initializes frame data
;
; Input:
;   HL = Pointer to frame data
SetupFrame:
    ; Read layout data
.read_layout_command
    ld a, [hl+]
    cp a, $80
    jr nz, .cont1

    ; T_POS
    ; The VRAM address to write to
    ld a, [hl+]
    ld e, a
    ld a, [hl+]
    ld d, a

    jr .read_layout_command

.cont1
    cp a, $81
    jr nz, .cont2

    ; T_END

    jr .done_layout

.cont2

    ; T_TILE
    ; A = tile number

    ld [de], a
    inc de

    jr .read_layout_command

.done_layout

    ld a, [hl+]
    ; A = number of tile chunks

    ld [NumTileChunks], a
    ld [TileChunkCounter], a

    ld a, l
    ld [TileChunkPtr], a
    ld [CurTileChunkPtr], a
    ld a, h
    ld [TileChunkPtr+1], a
    ld [CurTileChunkPtr+1], a

    ret

VBlank:
    ; Load the VBlankRoutine function pointer and jump to it
    ld a, [VBlankRoutine]
    ld l, a
    ld a, [VBlankRoutine+1]
    ld h, a
    jp hl

VBlank_Init:
    SET_VBLANK VBlank_ClearScreen
    reti

VBlank_ClearScreen:
    ; Set to tile 127
    ld a, 127

    ld hl, $9800
    ld de, 12

    ; 18 rows
    ld b, 18
.loop1

    ; 20 columns
REPT 20
    ld [hl+], a
ENDR

    ; Add 12 to get to the next row
    add hl, de

    dec b
    jr nz, .loop1

    SET_VBLANK VBlank_SetupFrame
    reti

VBlank_SetupFrame:
    ld hl, FRAME1
    call SetupFrame
    SET_VBLANK VBlank_Shader
    reti

; Returns BC = A << 4
Shl_A_4_To_BC:
    swap a
    ld b, a
    and a, $F0
    ld c, a
    ; C = a << 4
    ld a, b
    and a, $0F
    ld b, a
    ; B = a >> 4
    ; BC = a << 4
    ret

VBlank_Shader:
    ; DMA whatever we computed last frame
    ; Write to $9000 + (16*tile_start)
    ; tile_start is between 0 and 127 inclusive.
    ld a, [Render_TileStart]
    call Shl_A_4_To_BC
    ld hl, $9000
    add hl, bc
    ; HL = $9000 + (16*tile_start)

    ld a, [Render_NumTiles]
    ld b, a

    ld de, OUT
    ; HL = 9000 + (16*tile_start)
    ; DE = OUT
    ; B = num_tiles
    call VRamDma

    ; Run the shader


    ld a, [Shader_Lt]
    inc a
    ld [Shader_Lt], a
    call SetShaderState

    ; Load 

    ld a, [CurTileChunkPtr]
    ld l, a
    ld a, [CurTileChunkPtr+1]
    ld h, a

    ld a, [hl+]
    ; A = ROM bank
    ; Switch bank! (MBC3)
    ld [$2000], a

    ld a, [hl+]
    ld e, a
    ld a, [hl+]
    ld d, a
    ; DE = Source data address

    ld a, [hl+]
    ; A = number of tiles
    ld [Render_NumTiles], a

    ld a, [hl+]
    ; A = first tile number
    ld [Render_TileStart], a

    ld a, [Render_NumTiles]
    ; A = number of tiles (note: always between 0 and 31)

    call Shl_A_4_To_BC
    ld hl, OUT
    add hl, bc
    ; HL = OUT + number_of_tiles*16

    ld a, [Render_NumTiles]
    swap a
    rrca
    ; A = number_of_tiles*8

    ; DE = Source data address
    ; HL = OUT + number_of_tiles*16
    ; A = number_of_tiles*8
    call RunShader

    ; Update the pointers

    ld a, [TileChunkCounter]
    dec a
    jr nz, .nonzero

    ; Counter is zero.
    ; Reset the variables.

    ; TileChunkCounter = NumTileChunks
    ld a, [NumTileChunks]
    ld [TileChunkCounter], a

    ; CurTileChunkPtr = TileChunkPtr
    ld a, [TileChunkPtr]
    ld [CurTileChunkPtr], a
    ld a, [TileChunkPtr+1]
    ld [CurTileChunkPtr+1], a

    jr .done

.nonzero:
    ; TileChunkCounter -= 1
    ld [TileChunkCounter], a

    ; CurTileChunkPtr += 5
    ld a, [CurTileChunkPtr]
    ld l, a
    ld a, [CurTileChunkPtr+1]
    ld h, a
    ld bc, 5
    add hl, bc
    ld a, l
    ld [CurTileChunkPtr], a
    ld a, h
    ld [CurTileChunkPtr+1], a

.done:

    reti


; Enable double speed mode (CGB only)
DoubleSpeed:
    ; First, check if we're on CGB
    ldh a, [rKEY1]
    bit 7, a
    ; Skip if we're already in double-speed
    ret nz

    ; Request speed switch
    ld a, $01
    ldh [rKEY1], a

    stop
    nop

.already_double:
    ret


; Inputs:
;   DE = Source address. The lower 4 bytes are ignored.
;   HL = Destination address. The lower 4 bytes are ignored.
;   B  = number of bytes divided by 16
; Leaves DE and HL alone.
VRamDma:
    dec b

    ; Source address
    ld a, d
    ldh [rHDMA1], a
    ld a, e
    ldh [rHDMA2], a

    ; Destination address
    ld a, h
    ldh [rHDMA3], a
    ld a, l
    ldh [rHDMA4], a

    ; Set up length and start transfer
    ; Bit 7 = 0 for general purpose DMA (immediate transfer)
    ld a, b
    and a, $7F
    ldh [rHDMA5], a

    ; Wait for transfer to complete
.wait_dma:
    ldh a, [rHDMA5]
    bit 7, a
    jr z, .wait_dma

    ret


ClearWRam:
    ld bc, $200
    ld hl, $C000

.loop:
    xor a
    REPT 16
    ld [hl+], a
    ENDR

    dec bc
    ld a, b
    or a, c
    jr nz, .loop
    ret

ClearHRam:
    ld bc, $80
    ld hl, $FF80

.loop:
    xor a
    REPT 16
    ld [hl+], a
    ENDR

    dec bc
    ld a, b
    or a, c
    jr nz, .loop
    ret