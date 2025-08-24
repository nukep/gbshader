section "ShaderHRAM", HRAM
; Lt: L-theta
Shader_Lt:: db

num_rows: db
old_stack_ptr: dw


section "ShaderROM", ROM0

; Pseudocode:
;     For each row of pixels:
;         Low = 0
;         High = 0
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

; Input:
;   DE = In pointer
;   HL = Out pointer
;   A = number of rows (Note: 0 = 256)
RunShaderROM:
LOAD "ShaderExecutable", WRAM0
RunShader::
    ; Set stack to the output pointer
    ld [old_stack_ptr], sp
    ld sp, hl

    ; Save the number of rows
    ld [num_rows], a

    ; Set HL to the input pointer
    ld h, d
    ld l, e

    ; Setting this for crazy performance reasons explained later
    ld b, high(POW_LOOKUP)

.row_loop:

    ; Set LowAcc and HighAcc to 0
    ld de, 0
    
    ; Unrolled loop. Doing this instead of using a counter saves precious cycles!
    ; (ld b, 8; ...; dec b; jr nz .loop) - is about 8 + 4 + 12 (8 on final branch) = 8 + (4+12)*7 + (4+8) = 132 cycles of overhead

    ; Each iteration of the REPT loop below is 136 cycles
    REPT 8
.loop\@:
    ; Nt = *In++
    ld a, [hl+]
    
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

    ; 16 + 4 + 16 + 16/12 = 52 cycles on branch, 48 on fallthru
    ld a, [num_rows]
    dec a
    ld [num_rows], a
    jp nz, .row_loop

    ; Restore the old stack pointer
    ; 12 + 8 + 4 + 8 + 4+4+4 + 8 = 52 cycles
    ld hl, old_stack_ptr
    ld a, [hl+]
    ld e, a
    ld a, [hl+]
    ld d, a
    ld h, d
    ld l, e
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
