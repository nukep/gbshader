INCLUDE "hardware.inc"

SECTION "rst", ROM0
; No RST routines here!

SECTION "vblank", ROM0
    jp VBlank

SECTION "lcd", ROM0
    reti

SECTION "timer", ROM0
    reti

SECTION "serial", ROM0
    reti

SECTION "joypad", ROM0
    reti

SECTION "Header", ROM0
EntryPoint:
    jp Main

; Fill this area with zeros to make sure nothing else allocates to the header
ds $50 - 3


SECTION "DMA", WRAM0
; A buffer that gets populated before copying to VRAM
; Support up to 16 tiles (16 bytes per tile)
wDMA_OUT:           ds 16*16
wDMA_OUT_BOTTOM:


SECTION "MainMem", WRAM0

wVBlankRoutine:     dw

wNumTileChunks:     db
wTileChunkCounter:  db
wTileChunkPtr:      dw
wCurTileChunkPtr:   dw
wRender_TileStart:  db
wRender_NumTiles:   db

wInputDpad:         db
wInputButtons:      db
wLeftRightBalance:  db
wUpDownBalance:     db
wVelocityCounter:   db
wVelocity1:         db
wVelocity2:         db

wCurFrameTick:      dw


DEF INPUT_DPAD_UP    EQU %00000100
DEF INPUT_DPAD_DOWN  EQU %00001000
DEF INPUT_DPAD_LEFT  EQU %00000010
DEF INPUT_DPAD_RIGHT EQU %00000001

DEF INPUT_DPAD_UP_BIT EQU 2
DEF INPUT_DPAD_DOWN_BIT EQU 3
DEF INPUT_DPAD_LEFT_BIT EQU 1
DEF INPUT_DPAD_RIGHT_BIT EQU 0

SECTION "Main",ROM0

MACRO SET_VBLANK
    ld a, LOW(\1)
    ld [wVBlankRoutine], a
    ld a, HIGH(\1)
    ld [wVBlankRoutine+1], a
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


; Writes the tilemap to VRAM,
;
; Input:
;   HL = Pointer to tilemap layout data
SetupTilemapLayout:
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

    ret

.cont2

    ; T_TILE
    ; A = tile number

    ld [de], a
    inc de

    jr .read_layout_command


; Sets wTileChunkPtr and wCurTileChunkPtr.
; Accounts for the counter when setting wCurTileChunkPtr.
;
; Input:
;   HL = Tile data
SetupTileChunks::
    ; Set wTileChunkPtr = HL

    ld a, l
    ld [wTileChunkPtr], a
    ld a, h
    ld [wTileChunkPtr+1], a

    ; Set wCurTileChunkPtr to whatever offset it should be based on the current counter
    ; wCurTileChunkPtr = wTileChunkPtr + (wNumTileChunks - wTileChunkCounter)*TILE_CHUNK_SIZE

    ld a, [wTileChunkCounter]
    ld c, a
    ld a, [wNumTileChunks]
    sub a, c
    ; A = wNumTileChunks - wTileChunkCounter

    cp a, 0
    jr z, .done

    ld bc, TILE_CHUNK_SIZE
.loop:
    add hl, bc
    dec a
    jr nz, .loop
.done:

    ld a, l
    ld [wCurTileChunkPtr], a
    ld a, h
    ld [wCurTileChunkPtr+1], a

    ret


ReadInput::
    ; Read D-Pad
    ld a, %00100000
    ldh [rP1], a
    ldh a, [rP1]
    ld [wInputDpad], a

    ; Read buttons
    ld a, %00010000
    ldh [rP1], a
    ldh a, [rP1]
    ld [wInputButtons], a

    ; Set balance variables
    ; Left/Right balance

    ld a, [wInputDpad]
    bit INPUT_DPAD_LEFT_BIT, a
    jr nz, .lr_notleft

    ld a, -1
    jr .lr_done

.lr_notleft:
    bit INPUT_DPAD_RIGHT_BIT, a
    jr nz, .lr_notright

    ld a, 1
    jr .lr_done

.lr_notright:
    xor a
.lr_done:
    ld [wLeftRightBalance], a

    ; Up/Down balance

    ld a, [wInputDpad]
    bit INPUT_DPAD_DOWN_BIT, a
    jr nz, .du_notdown

    ld a, -1
    jr .du_done

.du_notdown:
    bit INPUT_DPAD_UP_BIT, a
    jr nz, .du_notup

    ld a, 1
    jr .du_done

.du_notup:
    xor a
.du_done:
    ld [wUpDownBalance], a

    ret


UpdateVelocity::
    ; Only update the velocity once every certain number of frames
    ld a, [wVelocityCounter]
    inc a
    ld [wVelocityCounter], a
    cp a, 6
    ; Return early if it's not our turn
    ret nz

    xor a
    ld [wVelocityCounter], a

_UpdateVelocity_1:


    ld a, [wLeftRightBalance]
    ld b, a

    ld a, [wVelocity1]
    add a, b
    add a, b

    jr z, .atzero

    bit 7, a
    jr z, .positive
.negative:

    cp a, 256-8
    ; flagc = a > 256-8
    jr nc, .negative_under
    inc a
.negative_under

    inc a
    jr .done

.positive:

    cp a, 8
    ; flagc = a < 8
    jr c, .positive_under
    dec a
.positive_under

    dec a

.atzero:
.done:
    ld [wVelocity1], a


_UpdateVelocity_2:
    ld a, [wUpDownBalance]
    ld b, a

    ld a, [wVelocity2]
    add a, b
    add a, b

    jr z, .atzero

    bit 7, a
    jr z, .positive
.negative:

    cp a, 256-8
    ; flagc = a > 256-8
    jr nc, .negative_under
    inc a
.negative_under

    inc a
    jr .done

.positive:

    cp a, 8
    ; flagc = a < 8
    jr c, .positive_under
    dec a
.positive_under

    dec a

.atzero:
.done:
    ld [wVelocity2], a

    ret

UpdateCurrentFrame::
    ld a, [wCurFrameTick+1]
    ld b, a
    ld a, [wCurFrameTick+0]
    ld c, a

    ld a, [wVelocity1]
    ld h, 0
    ld l, a
    bit 7, a
    jr z, .vel_positive
    ld h, $FF
.vel_positive


    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl

    add hl, bc
    ; HL = wCurFrameTick + wVelocity1*16
    ; H is the frame number that should be rendered.

    ; if HL < 0, keep it within bounds (0 <= H < NUM_FRAMES)
    bit 7, h
    jr z, .justfine1
    ;Underflow: h < 0
    ld a, h
    add a, NUM_FRAMES
    ld h, a

    jr .justfine2

.justfine1:
    ; if H >= NUM_FRAMES, keep it within bounds (0 <= H < NUM_FRAMES)
    ld a, h
    cp a, NUM_FRAMES
    ; flagC = a < NUM_FRAMES
    jr c, .justfine2

    sub a, NUM_FRAMES
    ld h, a

.justfine2:

    ; HL = (wCurFrameTick + wVelocity1*4) % (NUM_FRAMES*256)
    ; H is the frame number that should be rendered.

    ; If this number is different than the previous one, trigger a change
    ld a, [wCurFrameTick+1]
    ld d, a

    ; Save HL to memory
    ld a, h
    ld [wCurFrameTick+1], a
    ld a, l
    ld [wCurFrameTick+0], a

    ld a, d
    ; A = old frame number
    ; H = new frame number

    cp a, h
    jr z, .same

    ld a, h
    ; A = new frame number
    ld h, 0
    ld l, a
    ; HL = new frame number
    add hl, hl
    ; HL = frame*2

    ld bc, FRAMES
    add hl, bc
    ; HL = FRAMES + frame*2

    ld a, [hl+]
    ld b, a
    ; B = lo
    ld a, [hl+]
    ld h, a
    ; H = hi
    ld a, b
    ld l, a
    ; L = lo
    ; HL = 

    ; Change frame!
    call SetupTileChunks

.same:

    ret

UpdateLightAngle::
    ld a, [wVelocity2]
    ld b, a

    ldh a, [hShader_Lt]
    add a, b
    ldh [hShader_Lt], a

    ret

VBlank:
    ; Load the wVBlankRoutine function pointer and jump to it
    ld a, [wVBlankRoutine]
    ld l, a
    ld a, [wVBlankRoutine+1]
    ld h, a
    jp hl

VBlank_Init:
    SET_VBLANK VBlank_ClearScreen
    reti

VBlank_ClearScreen:
    ; Set to tile 0
    xor a

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

    SET_VBLANK VBlank_SetupHud1
    reti

VBlank_SetupHud1:
    ; Copy tiles to VRAM

    ld de, TILES_HUD
    ld hl, $9000
    ld b, HUD_NUM_TILES
    
    call VRamDma

    SET_VBLANK VBlank_SetupHud2
    reti

VBlank_SetupHud2:
    ; Write tilemap to VRAM

    ld hl, TILEMAP_HUD_LAYOUT
    call SetupTilemapLayout

    SET_VBLANK VBlank_SetupFrame
    reti

VBlank_SetupFrame:
    ld a, NUM_TILE_CHUNKS
    ld [wNumTileChunks], a
    ld [wTileChunkCounter], a

    ld hl, TILEMAP_LAYOUT
    call SetupTilemapLayout
    ld hl, FRAME00
    call SetupTileChunks
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
    ld a, [wRender_TileStart]
    call Shl_A_4_To_BC
    ld hl, $9000
    add hl, bc
    ; HL = $9000 + (16*tile_start)

    ld a, [wRender_NumTiles]
    ld b, a

    ld de, wDMA_OUT
    ; HL = 9000 + (16*tile_start)
    ; DE = wDMA_OUT
    ; B = num_tiles
    call VRamDma

    call ReadInput

    call UpdateVelocity

    call UpdateCurrentFrame

    call UpdateLightAngle


    call SetShaderState


    ld a, [wCurTileChunkPtr]
    ld l, a
    ld a, [wCurTileChunkPtr+1]
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
    ld [wRender_NumTiles], a

    ld a, [hl+]
    ; A = first tile number
    add a, TILE_FIRST_NUM
    ld [wRender_TileStart], a

    ld a, [wRender_NumTiles]
    ; A = number of tiles (note: always between 0 and 31)

    call Shl_A_4_To_BC
    ld hl, wDMA_OUT
    add hl, bc
    ; HL = wDMA_OUT + number_of_tiles*16

    ld a, [wRender_NumTiles]
    swap a
    rrca
    ; A = number_of_tiles*8

    ; DE = Source data address
    ; HL = wDMA_OUT + number_of_tiles*16
    ; A = number_of_tiles*8
    call RunShader

    ; Update the pointers

    ld a, [wTileChunkCounter]
    dec a
    jr nz, .nonzero

    ; Counter is zero.
    ; Reset the variables.

    ; wTileChunkCounter = wNumTileChunks
    ld a, [wNumTileChunks]
    ld [wTileChunkCounter], a

    ; wCurTileChunkPtr = wTileChunkPtr
    ld a, [wTileChunkPtr]
    ld [wCurTileChunkPtr], a
    ld a, [wTileChunkPtr+1]
    ld [wCurTileChunkPtr+1], a

    jr .done

.nonzero:
    ; wTileChunkCounter -= 1
    ld [wTileChunkCounter], a

    ; wCurTileChunkPtr += 5
    ld a, [wCurTileChunkPtr]
    ld l, a
    ld a, [wCurTileChunkPtr+1]
    ld h, a
    ld bc, 5
    add hl, bc
    ld a, l
    ld [wCurTileChunkPtr], a
    ld a, h
    ld [wCurTileChunkPtr+1], a

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
VRamDma::
    ; Return early if B = 0
    xor a
    cp a, b
    ret z

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