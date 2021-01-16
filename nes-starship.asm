; NES Starship
;
; Author: Michael White


; HEADER
    .inesprg 1      ; 16kb PRG-ROM banks
    .ineschr 1      ; 8kb CHR-ROM banks
    .inesmap 0      ; NES mapper
    .inesmir 1      ; VRAM mirroring banks

    .rsset $0000        ; Allocate variables starting at address $0000 
pointerBackgroundLowByte .rs 1
pointerBackgroundHighByte .rs 1

; PROGRAM
    .bank 0         ; 8kb bank for PRG-ROM
    .org $C000

RESET:
    JSR LoadBackground
    LDA #%10000000      ; enable NMI, sprites and background on table 0
    STA $2000       ; load into ppu control register 1
    LDA #%00011110      ; enable sprites and background
    STA $2001       ; load into ppu control register 2
    LDA #$00        ; clear vram address registers, this means no background scrolling
    STA $2006       
    STA $2006
    STA $2005
    STA $2005

LoadBackground:
    LDA $2002       ; $2002 is a ppu status register, reading it resets bit 7 which means vblank is not occuring; $2006 and $2007 are also reset
    LDA #$20        ; $2000 is name table 0 for backgrounds
    STA $2006       ; vram address register 2
    LDA #$00
    STA $2006       ; ppu addresses are 16 bits but registers are 8 bits, so two writes are necessary

    LDA #LOW(background)        ; take the low byte of our background and load it into our variable (again, ppu addresses are 16 bits)
    STA pointerBackgroundLowByte
    LDA #HIGH(background)       
    STA pointerBackgroundHighByte       ; load high byte into our variable

    LDX #$00
    LDY #$00
.Loop
    LDA [pointerBackgroundLowByte], y       ; load from the address of our low byte of background data using indirect indexed addressing
    STA $2007       ; writes to vram i/o register $2007 will offset automatically based on bit 2 in $2000 
    INY             ; increment y by 1, effectively being used to load the next byte in our loop
    CPY #$00        ; repeat this loop until it overflows after 256 times
    BNE .Loop

    INC pointerBackgroundHighByte       ; incrementing the high byte, essentially shifting us over 256 bytes
    INX
    CPX #$04        ; repeat this loop until we've done everythin 4 times, or 1024 total bytes written (just larger than our background size)
    BNE .Loop
    RTS


NMI:
    RTI


; INTERUPTS
    .bank 1     ; allocate another 8kb bank of memory for PRG-ROM for 16 kb total
    .org $E000
background:
    .include "graphics/background.asm"

    .org $FFFA
    .dw NMI
    .dw RESET
    .dw 0

; GRAPHICS 
    .bank 2     ; allocate 8kb of memory for CHR-ROM for 8kb total
    .org $0000
    .incbin "graphics/tiles.chr"     ; include binary file, generate using program YY-CHR