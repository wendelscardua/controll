.include "constants.inc"
.include "header.inc"

.feature force_range
.linecont +

; famitone2 config
FT_PAL_SUPPORT=0
FT_NTSC_SUPPORT=1
FT_SFX_ENABLE=1
FT_THREAD=1
FT_DPCM_ENABLE=0
FT_SFX_STREAMS=4
FT_DPCM_OFF=$c000

; music/sfx constants
.enum music_track
  ; TODO - list tracks
.endenum

.enum sfx
  ; TODO - list effects
.endenum

; game config

; debug - macros for NintendulatorDX interaction
.ifdef DEBUG
.macro debugOut str
  sta $4040
  jmp :+
      .byte str, 0
:
.endmacro

.macro debugRegs
  STA debug_a
  STX debug_x
  STY debug_y
.endmacro

.define fHex8( addr ) 1, 0, <(addr), >(addr)
.define fDec8( addr ) 1, 1, <(addr), >(addr)
.define fHex16( addr ) 1, 2, <(addr), >(addr)
.define fDec16( addr ) 1, 3, <(addr), >(addr)
.else
.macro debugOut str
.endmacro
.macro debugRegs
.endmacro
.endif

.segment "ZEROPAGE"
FT_TEMP: .res 3
.segment "FAMITONE"
FT_BASE_ADR: .res 186
.segment "CODE"
.include "famitone2.s"

.segment "OAM"
.struct Sprite
  ycoord .byte
  tile .byte
  flag .byte
  xcoord .byte
.endstruct

oam_sprites:
  .repeat 64
    .tag Sprite
  .endrepeat

.zeropage

.enum game_states
  waiting_to_start
  playing
.endenum

.enum directions
  up
  down
  left
  right
.endenum

.enum collidable_type
  nothing
  wall
  ; TODO - maybe add more
.endenum

.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rle_ptr

; zp vars
addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2 ; temporary address for PPU_ADDR

temp_x: .res 1
temp_y: .res 1

nmis: .res 1
old_nmis: .res 1

game_state: .res 1

sprite_counter: .res 1

debug_x: .res 1
debug_y: .res 1
debug_a: .res 1

SNEK_QUEUE_SIZE = 32
snek_ppu_l: .res SNEK_QUEUE_SIZE
snek_ppu_h: .res SNEK_QUEUE_SIZE
snek_head: .res 1
snek_tail: .res 1

snek_delay: .res 1
snek_frame_counter: .res 1
snek_direction: .res 1
snek_growth: .res 1

; precomputed collidable objects, indexed by snek head directions
collidable_per_direction: .res 4
target_ppu_h_per_direction: .res 4
target_ppu_l_per_direction: .res 4

; flag telling if we should recompute stuff
precomputed_are_dirty: .res 1

.segment "BSS"
; non-zp RAM goes here

.segment "CODE"

.import reset_handler
.import readjoy
.import unrle

.import music_data
.import sfx_data

.macro KIL ; pseudo instruction to kill the program
  .byte $12
.endmacro

.macro VBLANK
  .local vblankwait
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait
.endmacro

.macro save_regs
  PHA
  TXA
  PHA
  TYA
  PHA
.endmacro

.macro restore_regs
  PLA
  TAY
  PLA
  TAX
  PLA
.endmacro

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  INC nmis
  RTI
.endproc

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  ; load palettes
  JSR load_palettes

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  LDX #<music_data
  LDY #>music_data
  LDA #1
  JSR FamiToneInit

  ; init FamiTone SFX
  LDX #<sfx_data
  LDY #>sfx_data
  LDA #1
  JSR FamiToneSfxInit

  ; JSR go_to_title ; TODO - restore when ready
  JSR go_to_playing

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  ; new frame code
  JSR game_state_handler
  JSR screen_stuff
  JSR FamiToneUpdate

etc:
  JSR off_frame_processing
  JMP forever
.endproc

.proc screen_stuff
  ; Fix Scroll
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDA #$00 ; horizontal scroll
  STA PPUSCROLL
  STA PPUSCROLL

  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  RTS
.endproc

.proc load_palettes
  ; cobbles Y
  LDY PPUSTATUS
  LDY #$3f
  STY PPUADDR
  LDY #$00
  STY PPUADDR
:
  LDA palettes,Y
  STA PPUDATA
  INY
  CPY #$20
  BNE :-
  RTS
.endproc

.proc game_state_handler
  LDX game_state
  LDA game_state_handlers_h, X
  PHA
  LDA game_state_handlers_l, X
  PHA
  RTS
.endproc

.proc go_to_title
  LDA #game_states::waiting_to_start
  STA game_state

  LDA #$00
  STA PPUCTRL ; disable NMI
  STA PPUMASK ; disable rendering

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_title
  STA rle_ptr
  LDA #>nametable_title
  STA rle_ptr+1
  JSR unrle

  VBLANK

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc go_to_playing
  LDA #game_states::playing
  STA game_state

  LDA #$00
  STA PPUCTRL ; disable NMI
  STA PPUMASK ; disable rendering

  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

  LDA #<nametable_main
  STA rle_ptr
  LDA #>nametable_main
  STA rle_ptr+1
  JSR unrle

  ; init snek
  LDA #$00
  STA snek_tail
  LDA #$03
  STA snek_head

  ; snek ppu coordinates
  LDA #$21
  STA snek_ppu_h
  STA snek_ppu_h+1
  STA snek_ppu_h+2
  STA snek_ppu_h+3

  LDA #$2e
  STA snek_ppu_l
  LDA #$2f
  STA snek_ppu_l+1
  LDA #$30
  STA snek_ppu_l+2
  LDA #$31
  STA snek_ppu_l+3

  ; TODO variable speed
  LDA #24
  STA snek_delay
  STA snek_frame_counter

  LDA #directions::right
  STA snek_direction
  
  LDA #$00
  STA snek_growth

  LDA #$01
  STA precomputed_are_dirty

  VBLANK

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc waiting_to_start
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+
  JSR go_to_playing
:
  RTS
.endproc

.proc playing
  JSR update_snek

  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+
  LDA snek_direction
  CMP #directions::down
  BEQ :+
  LDA #directions::up
  STA snek_direction
:
  LDA pressed_buttons
  AND #BUTTON_DOWN
  BEQ :+
  LDA snek_direction
  CMP #directions::up
  BEQ :+
  LDA #directions::down
  STA snek_direction
:
  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ :+
  LDA snek_direction
  CMP #directions::right
  BEQ :+
  LDA #directions::left
  STA snek_direction
:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ :+
  LDA snek_direction
  CMP #directions::left
  BEQ :+
  LDA #directions::right
  STA snek_direction
:
  RTS
.endproc

.proc update_snek
  DEC snek_frame_counter
  BEQ :+
  RTS
:

  ; while we know snek old tail, erase tail (unless growing)
  LDA snek_growth
  BEQ delete_old_tail
  DEC snek_growth
  JMP skip_delete_old_tail
delete_old_tail:
  LDX snek_tail
  BIT PPUSTATUS
  LDA snek_ppu_h, X
  STA PPUADDR
  LDA snek_ppu_l, X
  STA PPUADDR
  LDA #$60 ; empty arena tile
  STA PPUDATA

  ; dequeue tail
  ; TODO - if optimization is necessary, count head/tail backwards
  INC snek_tail
  LDA snek_tail
  CMP #SNEK_QUEUE_SIZE
  BNE :+
  LDA #$00
  STA snek_tail
:

skip_delete_old_tail:

  ; while we know snek old head, replace head w/ body
  LDX snek_head
  LDA snek_ppu_h, X
  STA PPUADDR
  STA ppu_addr_ptr+1
  LDA snek_ppu_l, X
  STA PPUADDR
  STA ppu_addr_ptr
  LDA #$81 ; body tile
  STA PPUDATA
  
  ; get new head x,y
  LDX snek_direction
  LDA target_ppu_l_per_direction, X
  STA ppu_addr_ptr
  LDA target_ppu_h_per_direction, X
  STA ppu_addr_ptr+1

  ; draw new head
  ; (implicit LDA ppu_addr_ptr+1)
  STA PPUADDR
  LDA ppu_addr_ptr
  STA PPUADDR
  LDA #$80 ; head tile
  STA PPUDATA

  ; enqueue new head
  INC snek_head
  LDX snek_head
  CPX #SNEK_QUEUE_SIZE
  BNE :+
  LDX #$00
  STX snek_head
:
  LDA ppu_addr_ptr
  STA snek_ppu_l, X
  LDA ppu_addr_ptr+1
  STA snek_ppu_h, X

  ; mark precomputed data as dirty
  ; XXX - surely we'll have precomputed before the next frame
  ;       so this shouldn't overflow
  INC precomputed_are_dirty

  ; refresh frame counter
  LDA snek_delay
  STA snek_frame_counter
  RTS
.endproc

.proc off_frame_processing
  LDA game_state
  CMP #game_states::playing
  BEQ :+
  RTS
  :

  LDA precomputed_are_dirty
  BEQ skip_precomputing

  precompute_target_ppu_per_direction:
  ; X = directions, decreasing
  ; Y = snek head index
  LDY snek_head  
  LDX #$03
@loop:
  CLC
  LDA snek_ppu_l, Y
  ADC delta_ppu_l, X
  STA target_ppu_l_per_direction, X
  LDA snek_ppu_h, Y
  ADC delta_ppu_h, X
  STA target_ppu_h_per_direction, X

  DEX
  BPL @loop

  precompute_collidables_per_direction:
  ; X = directions, decreasing
  LDX #$03
@loop:
  LDA target_ppu_h_per_direction, X
  ; ppu_h goes from 20 to 22

  ; $22 is the first row below arena
  CMP #$22
  BNE @ppu_h_20_or_21
@ppu_h_22:
  LDA #collidable_type::wall
  JMP @next

@ppu_h_20_or_21:
  CMP #$20
  BNE @ppu_h_21
@ppu_h_20:
  ; x >= $24 && x <= $3B = top wall
  LDA target_ppu_l_per_direction, X
  CMP #$24
  BCC @no_top_wall
  CMP #$3C
  BCS @no_top_wall
@top_wall:
  LDA #collidable_type::wall
  JMP @next
@no_top_wall:
@ppu_h_21:
  ; for ppu_h 20 and 21, ppu_l values for left/right walls are the same

  ; left wall ppu l = $03, $23, $43, $63, $83, $a3, $c3, $e3
  ; aka AND #%00011111 == %00000011
  ; right wall ppu l = $1c, $3c, $5c, $7c, $9c, $bc, $dc, $fc
  ; aka AND #%00011111 == %00011100

  LDA target_ppu_l_per_direction, X
  AND #%00011111
  CMP #%00000011
  BNE @no_left
  LDA #collidable_type::wall
  JMP @next
@no_left:
  CMP #%00011100
  BNE @no_wall
  LDA #collidable_type::wall
  JMP @next
@no_wall:

  ; snek collision
  LDY snek_tail
@snek_loop:
  LDA snek_ppu_h, Y
  CMP target_ppu_h_per_direction, X
  BNE @snext
  LDA snek_ppu_l, Y
  CMP target_ppu_l_per_direction, X
  BNE @snext
  LDA #collidable_type::wall
  JMP @next
@snext:
  CPY snek_head
  BEQ @end_snek_loop

  INY
  CPY #SNEK_QUEUE_SIZE
  BNE @snek_loop
  LDY #$00
  JMP @snek_loop
@end_snek_loop:

  LDA #collidable_type::nothing
  JMP @next
@next:
  STA collidable_per_direction, X
  DEX
  BPL @loop

  LDA #$00
  STA precomputed_are_dirty
  
skip_precomputing:
  RTS
.endproc

.proc display_metasprite
  ; input: (addr_ptr) = metasprite pointer
  ;        temp_x and temp_y = screen position for metasprite origin
  ; cobbles X, Y
  LDY #0
  LDX sprite_counter
loop:
  LDA (addr_ptr),Y ; delta x
  CMP #128
  BEQ return
  INY
  CLC
  ADC temp_x
  STA oam_sprites+Sprite::xcoord,X
  LDA (addr_ptr),Y ; delta y
  INY
  SEC
  SBC #$01
  CLC
  ADC temp_y
  STA oam_sprites+Sprite::ycoord,X
  LDA (addr_ptr),Y ; tile
  INY
  STA oam_sprites+Sprite::tile,X
  LDA (addr_ptr),Y ; flags
  INY
  STA oam_sprites+Sprite::flag,X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  JMP loop
return:
  STX sprite_counter
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

game_state_handlers_l:
  .byte <(waiting_to_start-1)
  .byte <(playing-1)

game_state_handlers_h:
  .byte >(waiting_to_start-1)
  .byte >(playing-1)

; what to add to ppu address by direction
; up:    -$0020 = $FFE0
; down:   $0020
; left:  -$0001 = $FFFF
; right:  $0001

delta_ppu_h:
  .byte $ff, $00, $ff, $00
delta_ppu_l:
  .byte $e0, $20, $ff, $01

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

sprites:
.include "../assets/metasprites.s"

strings:
  ; TODO put strings here if needed

nametable_main: .incbin "../assets/nametables/main.rle"
nametable_title: .incbin "../assets/nametables/title.rle"

.segment "CHR"
.incbin "../assets/graphics.chr"
