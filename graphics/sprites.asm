    .db $78, $04, $00, $80      ; each sprite is represented by 4 bytes - 1 byte each for their y position in pixels, the tile id in the 
                                ; sprite sheet, sprite attributes such as color palette, and x position in pixels
                                ; making a separate file for this as many main character sprites contain multiple tiles and it's an 
                                ; easy way to consolidate that info, although here I use a single tile for the mc