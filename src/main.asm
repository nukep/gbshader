INCLUDE "hardware.inc"

SECTION "MainHRAM", HRAM
    hFoo:: db

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


SECTION "MainMem", WRAM0
OUT:
    ds 256
OUT_BOTTOM:


SECTION "Main",ROM0

Main:
    call DoubleSpeed

    ld sp, $ffff
    call ClearWRam
    ld sp, $dfff
    call ClearHRam

    ld a, -8
    ld [$ff80], a

    ld de, IN
    ld hl, OUT_BOTTOM
    ld a, 7

    call RunShader


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


    ld de, Main
    ld hl, $8000
    ld b, 14
    ; Takes 1060 cycles for 14 tiles
    call VRamDma

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


IN:
    ; L_theta = 8

    REPT 8
    db 11, 10, 4
    ; 1 (44)
    db 230, 7, 19
    ; 1 (59)
    db 83, 9, 8
    ; 0 (251)
    db 255, 7, 21
    ; 2 (78)
    db 18, 9, 9
    ; 1 (54)
    db 225, 7, 19
    ; 1 (51)
    db 184, 21, 250
    ; 0 (246)
    db 129, 10, 3
    ; 0 (219)
    ENDR

    ; Should yield:
    ; Low:  11001100
    ; High: 00010000


VBlank:
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