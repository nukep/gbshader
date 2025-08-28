section "ShaderHRAM", HRAM
; Lt: L-theta
Shader_Lt:: db

hNumRows: db
hOldStackPtr: dw


section "ShaderROM", ROM0

; Implements the simplified dot product:
;   v = m * cos(n_theta - l_theta) + b
;
; where v is the dot-product in linear-space.

; The source data buffer encodes an array of 3 bytes per pixel:
; - 1 byte: n_theta  (64 = 90 degrees, 128 = 180 degrees...)
; - 1 byte: m_log    (log-space value of the above "m" coefficient)
; - 1 byte: b        (linear-space value)

; Global variables are:
; - l_theta  (similar to n_theta, but is the angle of the light source)


; Pseudocode:
;     For each row of pixels:
;         Low = 0
;         High = 0

;         firstbyte = *In
;         if firstbyte == 0:
;             # Run-length-encoded zeros
;             # Write four 0's the specified number of times
;             In++
;             rows_div_2 = *In++
;             loop rows_div_2:
;                 *--Out = 0
;                 *--Out = 0
;                 *--Out = 0
;                 *--Out = 0
;             continue

;         loop 8:
;             Nt = *In++
;             m_log = *In++
;             b = *In++

;             i = Nt - Lt
;             j = cos_log_lookup[i]
;             k = m_log + j
;             l = pow_lookup[k]
;             m = l + b
;             A = m

;             # rol means "shift to the left, set carry to the most significant bit that was pushed off"
;             rol A
;             if carry is set:
;                 # m is negative, so assign to 0
;                 Low <<= 1
;                 High <<= 1
;                 continue

;             rol A
;             shift carry into LSB of Low
;             rol A
;             shift carry into LSB of High

;         *--Out = High
;         *--Out = Low

RleDisableInjectPayload:
    ld d, h
    ld e, l

    cpl
    inc a
    ; A = -A
    ld l, a
    ld h, $FF

    add hl, hl
    add hl, hl
    ; HL = offset + 4
    add hl, sp
    ld sp, hl

    ld h, d
    ld l, e

    ; hNumRows -= number of rows
    ldh a, [hNumRows]
    sub a, c
    sub a, c
    ldh [hNumRows], a
    jp nz, RunShader.row_loop
    jp RunShader.end

; Input:
;   DE = In pointer
;   HL = Out pointer
;   A = number of rows (Note: 0 = 256)
RunShaderROM:

LOAD "ShaderExecutable", WRAM0
RunShader::
    ; Set stack to the output pointer
    ld [hOldStackPtr], sp
    ld sp, hl

    ; Save the number of rows
    ldh [hNumRows], a

    ; Set HL to the input pointer
    ld h, d
    ld l, e

    ; Setting this for crazy performance reasons explained later
    ld b, high(POW_LOOKUP)

.row_loop:

    ; Set LowAcc and HighAcc to 0
    ld de, 0

.row_loop_skipde:

    ; Check for a special value to do a run-length encoding of "zero" rows
    ld a, [hl+]
    cp a, d
    jr nz, .nonzero
    ; 8+4+8 = 20 cycles

    ; Do run-length encoding of zeros
    ; 8+4
    ld a, [hl+]
    ld c, a
    ; A = C = number of rows divided by 2. Exception: 0 = 256.

    ; If we want to disable run-length encoding of zeros,
    ; we replace the following with a custom payload
    ;; push de; push de; dec a
    ;;   D5 D5 3D
    ;; jp RleDisableInjectPayload
    ;;   C3 <LO> <HI>
.inject_rlezero:
    ; jp RleDisableInjectPayload

    ; Push a "0" row "A*2" number of times
    ; (16+16+4)*A + 12*(A-1) + 8
    ; = 48*A - 4
    ; = 44 cycles for 16 pixels = 2.75/pixel (A=1, 2 rows)
    ; = 92 cycles for 32 pixels = 2.875/pixel (A=2, 4 rows)
    ; = 1532 cycles for 512 pixels = 2.992/pixel (A=32, 64 rows, 8 tiles)
.zero_loop:
    push de
    push de
    dec a
    jr nz, .zero_loop

.rle_done:

    ; hNumRows -= number of rows
    ldh a, [hNumRows]
    sub a, c
    sub a, c
    ldh [hNumRows], a
    jr nz, .row_loop_skipde
    jp .end

.nonzero
    ; 8+4+12 = 24 cycles if branch wasn't taken
    
    ; Unrolled loop. Doing this instead of using a counter saves precious cycles!
    ; (ld b, 8; ...; dec b; jr nz .loop) - is about 8 + 4 + 12 (8 on final branch) = 8 + (4+12)*7 + (4+8) = 132 cycles of overhead

    ; Each iteration of the REPT loop below is 136 cycles
FOR N, 8
.loop\@:
    ; Exclude this for the first iteration
    IF N > 0
    ; Nt = *In++
    ld a, [hl+]
    ENDC
    
    ; i = Nt - Lt
.inject_lt\@:
    sub a, 0
    ; A = i
    
    ; j = cos_log[i]
    ld c, a
    dec b   ; Instead of ld b, high(COS_LOG_LOOKUP). (4 cycles instead of 8.)
    ld a, [bc]
    ld c, a
    ; C = j
    
    ; m_log = *In++
    ld a, [hl+]
    
    ; k = m_log + j
    add c
    ; A = k
    
    ; l = pow[k]
    ld c, a
    inc b   ; Instead of ld b, high(POW_LOOKUP). (4 cycles instead of 8. Hey, it adds up!)
    ld a, [bc]
    ld c, a
    ; C = l
    
    ; b = *In++
    ld a, [hl+]
    
    ; m = l + b
    add c
    ; A = m
    
    ; Check if negative
    rla
    jr nc, .positive\@
    ; 8 cycles to fallthru
    
    ; m is negative, so shift LowAcc and HighAcc left and continue

    ; 4 cycles
    xor a

.positive\@:
    ; 12 cycles to branch

    ; E = LowAcc
    ; D = HighAcc

    ; HighAcc = (HighAcc << 1) | bit 5
    ; LowAcc = (LowAcc << 1) | bit 6
    ; 24 cycles
    rla
    rl d
    rla
    rl e

.continue\@:
ENDR

    ; Push the LowAcc and HighAcc values to the stack (i.e. our output buffer)
    ; 16 cycles
    push de

    ; 12 + 4 + 12 + 16/12 = 44 cycles on branch, 40 on fallthru
    ldh a, [hNumRows]
    dec a
    ldh [hNumRows], a
    jp nz, .row_loop

.end:

    ; Restore the old stack pointer
    ; 40 cycles
    ldh a, [hOldStackPtr]
    ld l, a
    ldh a, [hOldStackPtr+1]
    ld h, a
    ld sp, hl
    
    ret
RunShader_End:

DEF ShaderROMLength EQU (RunShader_End - RunShader)


section "Shader", ROM0


; Copy bytes from one area to another.
; de: Source
; hl: Destination
; bc: Length
; Source: https://gbdev.io/gb-asm-tutorial/part2/functions.html
Memcpy:
    ld a, [de]
    ld [hl+], a
    inc de
    dec bc
    ld a, b
    or a, c
    jp nz, Memcpy
    ret

; Copies the shader ROM to RAM
CopyShaderCode::
    ld de, RunShaderROM
    ld hl, RunShader
    ld bc, ShaderROMLength
    jr Memcpy

MACRO LD_INJECT
    ld [(\1)], a
ENDM

; Modify the shader executable using the state of the program, such as the light position.
SetShaderState::
    ld a, [Shader_Lt]
    LD_INJECT RunShader.inject_lt_u1+1
    LD_INJECT RunShader.inject_lt_u2+1
    LD_INJECT RunShader.inject_lt_u3+1
    LD_INJECT RunShader.inject_lt_u4+1
    LD_INJECT RunShader.inject_lt_u5+1
    LD_INJECT RunShader.inject_lt_u6+1
    LD_INJECT RunShader.inject_lt_u7+1
    LD_INJECT RunShader.inject_lt_u8+1
    ret
