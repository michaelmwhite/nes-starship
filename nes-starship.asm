; NES Starship
;
; Author: Michael White


; HEADER
    .inesprg 1      ; 16kb PRG-ROM banks
    .ineschr 1      ; 8kb CHR-ROM banks
    .inesmap 0      ; NES mapper
    .inesmir 1      ; VRAM mirroring banks


; PROGRAM
    .bank 0         ; 8kb bank for PRG-ROM
    .org $C000

RESET:
InfiniteLoop:
    JMP InfiniteLoop

NMI:
    RTI


; INTERUPTS
    .bank 1
    .org $FFFA
    .dw NMI
    .dw RESET
    .dw 0