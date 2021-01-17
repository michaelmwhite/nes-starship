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
    JSR LoadPalettes
    JSR LoadAttributes
    JSR LoadSprites

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

LoadPalettes:
    LDA $2002       ; read from $2002 to reset vram address registers $2005 and $2006
    LDA #$3F        ; $3F00 is where color palettes are stored in ppu memory (see ppu memory map in pdf)
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00
.Loop               ; labels that start with periods are local and not available outside of the parent label
    LDA palettes, x
    STA $2007       ; write to vram i/o register
    INX
    CPX #$20        ; only 32 total bytes of palette data
    BNE .Loop
    RTS

LoadAttributes:
    LDA $2002
    LDA #$23        ; $23C0 is attribute table 0 in the ppu memory map
    STA $2006
    LDA #$C0
    STA $2006

    LDX #$00
.Loop
    LDA attributes, x
    STA $2007
    INX
    CPX #$40        ; load 64 bytes of attribute data for our background
    BNE .Loop
    RTS

LoadSprites:
    LDA #$00
.Loop
    LDA sprites, x
    STA $0300, x        ; load our sprites into ram at address $0300 - we will be accessing these sprites via DMA (direct memory access)
    INX                 ; so no need to draw them to the ppu here
    CPX #$04            ; loop 4 times as we only have 4 bytes of sprite data
    BNE .Loop
    RTS

NMI:            ; Non Maskable Interrupt - this gets called once per frame - we constantly modify sprite data, so to update it we must update the data before every frame
    LDA #$00
    STA $2003       ; SPR-RAM address register - storing address here to initiate DMA as it's more efficient than manually writing lots of sprite data to ppu
    LDA #$03
    STA $4014       ; writing to $4014, the sprite DMA register, initiates DMA for 256 bytes starting from the given address 
    RTI             ; during DMA, the memory bus is in use and the cpu must wait until it finishes, and takes the equivalent of 512 cycles
                    ; since each sprite takes 4 bytes of data, we can only load 64 sprites total here

; PROGRAM PART 2 + INTERUPTS
    .bank 1     ; allocate another 8kb bank of memory for PRG-ROM for 16 kb total
    .org $E000
background:
    .include "graphics/background.asm"
palettes:
    .include "graphics/palettes.asm"
attributes:
    .include "graphics/attributes.asm"
sprites:
    .include "graphics/sprites.asm"

    .org $FFFA
    .dw NMI
    .dw RESET
    .dw 0

; GRAPHICS 
    .bank 2     ; allocate 8kb of memory for CHR-ROM for 8kb total
    .org $0000
    .incbin "graphics/tiles.chr"     ; include binary file, generate using program YY-CHR