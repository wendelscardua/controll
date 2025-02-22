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
  NibblesTitleRetromix
  NibblesLevelRetromix
.endenum

.enum sfx
  Confirm
  Toggle
  CoinGet
  BigCoinGet
  Bonk
  Swap
.endenum

.macro SFX effect, channel
  save_regs
  LDA #sfx::effect
  LDX #.ident ( .concat( "FT_SFX_", .string(channel) ) )
  JSR FamiToneSfxPlay
  restore_regs
.endmacro

.macro PLAY track
  save_regs
  LDA #music_track::track
  JSR FamiToneMusicPlay
  restore_regs
.endmacro

; game config
FIRST_SPAWN_DELAY = 10
SPAWN_DELAY = 20

SWITCH_INITIAL_TIMER = 3
SWITCH_TIMER = 5

SNEK_QUEUE_SIZE = 32
THINGS_ARRAY_SIZE = 32

SECONDS_TO_LEVEL_UP = 30

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
  game_over
.endenum

.enum directions
  up
  down
  left
  right
.endenum

.enum button_type
  up
  down
  left
  right
  a_action
  b_action
.endenum

.enum collidable_type
  nothing
  wall
  small_coin
  big_coin
.endenum

.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rng_seed
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

; on screen buttons data
command_per_button: .res 6
sprite_x_per_command: .res 6
sprite_y_per_command: .res 6
target_sprite_x_per_command: .res 6
target_sprite_y_per_command: .res 6

dirty_sprite_data: .res 1

switcheroo: .res 1
switch_timer: .res 1

snek_ppu_l: .res SNEK_QUEUE_SIZE
snek_ppu_h: .res SNEK_QUEUE_SIZE
snek_head: .res 1
snek_tail: .res 1

snek_frame_counter: .res 1
snek_direction: .res 1
snek_length: .res 1
snek_growth: .res 1
snek_previous_direction: .res 1
snek_segment_tile: .res 1

; clock (level up happens every X seconds)
clock: .res 2

; score
score_digits: .res 5
score_buffer: .res 1

high_score_digits: .res 5
alt_high_score_digits: .res 5

; level
selected_level_digits: .res 2
selected_level_hex: .res 1
level_digits: .res 2
level_hex: .res 1 ; binary level (-1, so first level is $00)

; fun
fun_enabled: .res 1

; every X frames, spawn a thing
thing_spawn_counter: .res 1
next_thing_to_spawn: .res 1

; precomputed collidable objects, indexed by snek head directions
collidable_per_direction: .res 4
target_ppu_h_per_direction: .res 4
target_ppu_l_per_direction: .res 4

; flag telling if we should recompute stuff
precomputed_are_dirty: .res 1

; flag for coin processing
coin_buffer: .res 1

; ppu coordinates to erase (deprecated walls)
erase_ppu_h: .res 1
erase_ppu_l: .res 1

.segment "BSS"
; non-zp RAM goes here

; coins / walls /enemies
things_ppu_l: .res THINGS_ARRAY_SIZE
things_ppu_h: .res THINGS_ARRAY_SIZE
things_type: .res THINGS_ARRAY_SIZE
things_count: .res 1
thing_index_per_direction: .res 4

.segment "CODE"

.import reset_handler
.import readjoy
.import rand
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

  ; init rng
  LDA #$a9
  STA rng_seed
  LDA #$73
  STA rng_seed+1

  LDA #$00
  STA selected_level_hex
  STA selected_level_digits
  LDA #$01
  STA selected_level_digits+1

  LDA #$01
  STA fun_enabled

  JSR go_to_title

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  ; new frame code
  JSR animate_tiles
  JSR game_state_handler
  JSR screen_stuff
  JSR FamiToneUpdate

etc:
  JSR off_frame_processing
  JSR rand ; shuffle rng around
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

  PLAY NibblesTitleRetromix

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

  ; init buttons
  LDX #$05
:
  TXA
  STA command_per_button, X
  LDA command_positions_x, X
  STA sprite_x_per_command, X
  STA target_sprite_x_per_command, X
  LDA command_positions_y, X
  STA sprite_y_per_command, X
  STA target_sprite_y_per_command, X

  DEX
  BPL :-

  LDA #$01
  STA dirty_sprite_data

  LDA #$00
  STA switcheroo

  ; set level
  LDA selected_level_hex
  STA level_hex
  .repeat 2, index
  LDA selected_level_digits+index
  STA level_digits+index
  .endrepeat


  ; init things
  LDA #FIRST_SPAWN_DELAY
  STA thing_spawn_counter
  LDA #collidable_type::small_coin
  STA next_thing_to_spawn

  LDA #$00
  STA things_count
  STA erase_ppu_h
  STA erase_ppu_l

  LDA #$FF
  STA coin_buffer

  ; init snek
  LDA #$00
  STA snek_tail
  LDA #$03
  STA snek_head
  LDA #$04
  STA snek_length

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

  LDA snek_delay_per_level
  STA snek_frame_counter

  LDA #directions::right
  STA snek_direction
  STA snek_previous_direction

  LDA #$00
  STA snek_growth

  ; init etc
  LDA #SWITCH_INITIAL_TIMER
  STA switch_timer

  LDA #$00
  .repeat 5, index
  STA score_digits+index
  .endrepeat
  STA score_buffer
  STA clock
  STA clock+1

  LDA #$01
  STA precomputed_are_dirty

  VBLANK

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  PLAY NibblesLevelRetromix

  RTS
.endproc

.proc go_to_game_over
  LDA #game_states::game_over
  STA game_state

  ; erase sprites
  LDX #$00
  LDA #$F0
:
  STA oam_sprites+Sprite::ycoord, X
  .repeat .sizeof(Sprite)
  INX
  .endrepeat
  BNE :-

  LDA #$00
  STA PPUCTRL ; disable NMI
  STA PPUMASK ; disable rendering

  LDA PPUSTATUS
  LDY snek_head
  LDA snek_ppu_h, Y
  STA PPUADDR
  LDA snek_ppu_l, Y
  STA PPUADDR
  LDA #$83
  STA PPUDATA

  LDA #$22
  STA PPUADDR
  LDA #$40
  STA PPUADDR

  LDA #<nametable_game_over
  STA rle_ptr
  LDA #>nametable_game_over
  STA rle_ptr+1
  JSR unrle

  VBLANK

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc waiting_to_start
  BIT PPUSTATUS
  LDA #$22
  STA PPUADDR
  LDA #$ec
  STA PPUADDR
  LDA selected_level_digits
  STA PPUDATA
  LDA selected_level_digits+1
  STA PPUDATA

  LDA #$22
  STA PPUADDR
  LDA #$f2
  STA PPUADDR
  LDX #$0
  LDA fun_enabled
  BEQ @loop_boring
@loop_fun:
  LDA string_fun, X
  STA PPUDATA
  INX
  CPX #6
  BNE @loop_fun
  JMP @readjoy
@loop_boring:
  LDA string_boring, X
  STA PPUDATA
  INX
  CPX #6
  BNE @loop_boring

@readjoy:

  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+
  SFX Confirm, CH0
  JSR go_to_playing
:
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+
  LDA selected_level_hex
  CMP #29
  BEQ :+
  SFX Toggle, CH0
  INC selected_level_hex
  INC selected_level_digits+1
  LDA selected_level_digits+1
  CMP #10
  BNE :+
  LDA #0
  STA selected_level_digits+1
  INC selected_level_digits
:
  LDA pressed_buttons
  AND #BUTTON_DOWN
  BEQ :+
  LDA selected_level_hex
  BEQ :+
  SFX Toggle, CH0
  DEC selected_level_hex
  DEC selected_level_digits+1
  BPL :+
  LDA #9
  STA selected_level_digits+1
  DEC selected_level_digits
:
  LDA pressed_buttons
  AND #(BUTTON_LEFT|BUTTON_RIGHT)
  BEQ :+
  SFX Toggle, CH0
  LDA fun_enabled
  EOR #%1
  STA fun_enabled
  LDX #$04
@loop:
  LDA high_score_digits, X
  STA temp_x
  LDA alt_high_score_digits, X
  STA high_score_digits, X
  LDA temp_x
  STA alt_high_score_digits, X
  DEX
  BPL @loop
:

  RTS
.endproc

.proc game_over
  JSR readjoy
  LDA pressed_buttons
  AND #BUTTON_START
  BEQ :+
  JSR go_to_title
:
  RTS
.endproc

.proc decode_command
  ; converts button (what was pressed) into command (what to do)
  ; input: A = pressed button (from enum button_type)
  TAX
  LDA command_per_button, X
  TAX
  LDA command_handlers_h, X
  PHA
  LDA command_handlers_l, X
  PHA
  RTS
.endproc

.proc command_up
  LDA snek_previous_direction
  CMP #directions::down
  BEQ :+
  LDA #directions::up
  STA snek_direction
:
  RTS
.endproc

.proc command_down
  LDA snek_previous_direction
  CMP #directions::up
  BEQ :+
  LDA #directions::down
  STA snek_direction
:
  RTS
.endproc

.proc command_left
  LDA snek_previous_direction
  CMP #directions::right
  BEQ :+
  LDA #directions::left
  STA snek_direction
:
  RTS
.endproc

.proc command_right
  LDA snek_previous_direction
  CMP #directions::left
  BEQ :+
  LDA #directions::right
  STA snek_direction
:
  RTS
.endproc

.proc command_noop
  RTS
.endproc

.proc update_command_sprites
  LDA #$00
  STA sprite_counter

  ; X = commands index
  LDX #$05
loop:
  LDA sprite_x_per_command, X
  STA temp_x
  LDA sprite_y_per_command, X
  STA temp_y
  LDA metasprite_l_per_command, X
  STA addr_ptr
  LDA metasprite_h_per_command, X
  STA addr_ptr+1
  save_regs
  JSR display_metasprite
  restore_regs
  DEX
  BPL loop
  RTS
.endproc

.proc switch_random_buttons
  LDA fun_enabled
  BNE :+
  RTS
:

  SFX Swap, CH2

  LDA switcheroo
  BEQ :+
  RTS
:
  INC switcheroo

  LDA #120
  STA snek_frame_counter ; give some time to think

first_loop:
  LDA rng_seed
  AND #%111
  CMP #$06
  BCC :+
  JSR rand
  JMP first_loop
:
  STA temp_x

second_loop:
  JSR rand
  LDA rng_seed
  AND #%111
  CMP #$06
  BCS second_loop
  CMP temp_x
  BEQ second_loop
  STA temp_y

  LDX temp_x
  LDY temp_y

  LDA command_per_button, X
  STA temp_x
  LDA command_per_button, Y
  STA temp_y

  LDA command_per_button, X
  PHA
  LDA command_per_button, Y
  STA command_per_button, X
  PLA
  STA command_per_button, Y

  LDX temp_x
  LDY temp_y

  LDA sprite_x_per_command, X
  STA target_sprite_x_per_command, Y
  LDA sprite_y_per_command, X
  STA target_sprite_y_per_command, Y

  LDA sprite_x_per_command, Y
  STA target_sprite_x_per_command, X
  LDA sprite_y_per_command, Y
  STA target_sprite_y_per_command, X

  INC dirty_sprite_data

  RTS
.endproc

.proc update_command_positions
  LDA switcheroo
  BNE :+
  RTS
:
  LDA #$01
  STA dirty_sprite_data

  LDA #$06
  STA switcheroo

  LDX #$05
@loop:

  LDA sprite_x_per_command, X
  CMP target_sprite_x_per_command, X

  BEQ @check_y

  BCC @move_right
@move_left:
  .repeat 2
  DEC sprite_x_per_command, X
  .endrepeat
  JMP @next
@move_right:
  .repeat 2
  INC sprite_x_per_command, X
  .endrepeat
  JMP @next

@check_y:
  LDA sprite_y_per_command, X
  CMP target_sprite_y_per_command, X
  BNE @not_finished
@finished:
  DEC switcheroo
  JMP @next
@not_finished:

  BCC @move_down
@move_up:
  .repeat 2
  DEC sprite_y_per_command, X
  .endrepeat
  JMP @next
@move_down:
  .repeat 2
  INC sprite_y_per_command, X
  .endrepeat
  JMP @next

@next:
  DEX
  BPL @loop

  RTS
.endproc

.proc update_clock
  INC clock
  LDA clock
  CMP #60
  BNE :+
  LDA #0
  STA clock
  INC clock+1
  LDA clock+1
  CMP #SECONDS_TO_LEVEL_UP
  BNE :+
  LDA #0
  STA clock+1
  INC level_hex
  INC level_digits+1
  LDA level_digits+1
  CMP #10
  BNE :+
  LDA #0
  STA level_digits+1
  INC level_digits
:
  RTS
.endproc

.proc animate_tiles
  LDA nmis
  AND #%11000
  LSR
  LSR
  LSR
  TAX
  STA chrrom_bankswitch, X
  RTS
.endproc

.proc playing
  BIT PPUSTATUS
  LDA erase_ppu_h
  BEQ :+
  STA PPUADDR
  LDA erase_ppu_l
  STA PPUADDR
  LDA #$9c ; empty tile
  STA PPUDATA
  LDA #$00
  STA erase_ppu_h
:

  JSR update_clock
  JSR update_snek
  JSR update_command_positions

  JSR readjoy
  .ifdef DEBUG
  LDA pressed_buttons
  AND #BUTTON_SELECT
  BEQ :+
  JSR switch_random_buttons
:
  .endif
  LDA pressed_buttons
  AND #BUTTON_UP
  BEQ :+
  LDA #button_type::up
  JSR decode_command
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_DOWN
  BEQ :+
  LDA #button_type::down
  JSR decode_command
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_LEFT
  BEQ :+
  LDA #button_type::left
  JSR decode_command
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ :+
  LDA #button_type::right
  JSR decode_command
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_RIGHT
  BEQ :+
  LDA #button_type::right
  JSR decode_command
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_A
  BEQ :+
  LDA #button_type::a_action
  JSR decode_command
  RTS
:
  LDA pressed_buttons
  AND #BUTTON_B
  BEQ :+
  LDA #button_type::b_action
  JSR decode_command
  ; RTS
:
  RTS
.endproc

.proc update_score
  LDA score_buffer
  BEQ skip_score_buffer

  DEC score_buffer

  LDX #$04
@loop:
  INC score_digits, X
  LDA score_digits, X
  CMP #10
  BNE @endloop
  LDA #0
  STA score_digits, X
  DEX
  JMP @loop
@endloop:

  LDX #$00
@comploop:
  LDA score_digits, X
  CMP high_score_digits, X
  BNE :+
  INX
  CPX #$05
  BNE @comploop
  JMP skip_score_buffer
:
  BCC skip_score_buffer

@copyloop:
  LDA score_digits, X
  STA high_score_digits, X
  INX
  CPX #$05
  BNE @copyloop

skip_score_buffer:

  ; non snek frame, we have time to refresh score (?)
  BIT PPUSTATUS
  LDA #$22
  STA PPUADDR
  LDA #$0b
  STA PPUADDR
  .repeat 5, index
  LDA score_digits+index
  STA PPUDATA
  .endrepeat

  LDA #$22
  STA PPUADDR
  LDA #$19
  STA PPUADDR
  LDA level_digits+0
  STA PPUDATA
  LDA level_digits+1
  STA PPUDATA

  BIT PPUSTATUS
  LDA #$22
  STA PPUADDR
  LDA #$2b
  STA PPUADDR
  .repeat 5, index
  LDA high_score_digits+index
  STA PPUDATA
  .endrepeat

  RTS
.endproc

.proc update_snek
  DEC snek_frame_counter
  BEQ :+
  JSR update_score
  RTS
:

  ; compute new segment tile
  LDA snek_previous_direction
  ASL
  ASL
  ORA snek_direction
  TAY
  LDA tile_per_directions, Y
  STA snek_segment_tile

  LDY snek_direction
  STY snek_previous_direction

  ; if we are going to collide, game over
  LDA collidable_per_direction, Y
  CMP #collidable_type::wall
  BNE :+
  SFX Bonk, CH1
  JSR go_to_game_over
  RTS
:
  CMP #collidable_type::nothing
  BEQ :+
  ; set reminder for off-frame score processing
  LDA thing_index_per_direction, Y
  STA coin_buffer
:

  ; while we know snek old tail, erase tail (unless growing)
  LDA snek_length
  CMP #SNEK_QUEUE_SIZE
  BEQ delete_old_tail
  LDA snek_growth
  BEQ delete_old_tail
  DEC snek_growth
  INC snek_length
  JMP skip_delete_old_tail
delete_old_tail:
  LDX snek_tail
  BIT PPUSTATUS
  LDA snek_ppu_h, X
  STA PPUADDR
  LDA snek_ppu_l, X
  STA PPUADDR

  LDA things_count
  CMP #THINGS_ARRAY_SIZE
  BEQ no_spawn

  DEC thing_spawn_counter
  BNE no_spawn
  ; spawn thing where tail was (so we know (?) the player won't collide with it)
  LDY next_thing_to_spawn
  LDA tile_per_thing, Y
  LDY things_count
  STA PPUDATA

  LDA next_thing_to_spawn
  STA things_type, Y

  LDA snek_ppu_h, X
  STA things_ppu_h, Y
  LDA snek_ppu_l, X
  STA things_ppu_l, Y
  INC things_count

  LDA #collidable_type::nothing
  STA next_thing_to_spawn

  LDA #SPAWN_DELAY
  STA thing_spawn_counter

  LDY snek_direction ; restore cobbled Y
  JMP dequeue_tail

no_spawn:
  LDA #$9c ; empty arena tile
  STA PPUDATA
dequeue_tail:

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
  LDA snek_segment_tile ; body tile
  STA PPUDATA

  ; get new head x,y
  LDA target_ppu_l_per_direction, Y
  STA ppu_addr_ptr
  LDA target_ppu_h_per_direction, Y
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
  LDX level_hex
  LDA snek_delay_per_level, X
  STA snek_frame_counter
  RTS
.endproc

.proc off_frame_processing
  LDA game_state
  CMP #game_states::playing
  BEQ :+
  RTS
:

  LDX coin_buffer
  BMI skip_coin_buffer

  ; before using coin buffer, since it's a pickup we check switch timer as well
  DEC switch_timer
  BNE :+
  JSR switch_random_buttons
  JSR rand
  AND #%11
  ADC #SWITCH_TIMER
  STA switch_timer
:

  LDA things_type, X

  CMP #collidable_type::small_coin
  BNE :+
  SFX CoinGet, CH1
  INC snek_growth
  CLC
  LDA #10
  ADC score_buffer
  STA score_buffer
  JMP delete_thing
:
  CMP #collidable_type::big_coin
  BNE skip_coin_buffer

  SFX BigCoinGet, CH1
  CLC
  LDA #50
  ADC score_buffer
  STA score_buffer

  INC snek_growth
  INC snek_growth
delete_thing:
  DEC things_count
  BEQ :+
  CPX things_count
  BEQ :+
  LDY things_count
  LDA things_type, Y
  STA things_type, X
  LDA things_ppu_l, Y
  STA things_ppu_l, X
  LDA things_ppu_h, Y
  STA things_ppu_h, X
:
  LDA #$FF
  STA coin_buffer

skip_coin_buffer:

  LDA things_count
  CMP #THINGS_ARRAY_SIZE
  BNE skip_full_array

  ; find any wall to delete
  LDX #$00
@loop:
  LDA things_type, X
  CMP #collidable_type::wall
  BNE @next

  DEC things_count
  BEQ skip_full_array
  CPX things_count
  BEQ skip_full_array

  LDA things_ppu_l, X
  STA erase_ppu_l
  LDA things_ppu_h, X
  STA erase_ppu_h

  LDY things_count

  LDA things_type, Y
  STA things_type, X
  LDA things_ppu_l, Y
  STA things_ppu_l, X
  LDA things_ppu_h, Y
  STA things_ppu_h, X
  JMP skip_full_array

@next:
  INX
  CPX things_count
  BNE @loop


skip_full_array:

  LDA next_thing_to_spawn
  BNE skip_thing_randomization

  JSR rand
  LDA rng_seed
  AND #%11
  BNE :+
  LDA #collidable_type::small_coin
:
  STA next_thing_to_spawn

skip_thing_randomization:

  LDA precomputed_are_dirty
  BEQ skip_precomputing

  JSR compute_collisions

skip_precomputing:

  LDA dirty_sprite_data
  BEQ :+
  JSR update_command_sprites
  LDA #$00
  STA dirty_sprite_data
:
  RTS
.endproc

.proc compute_collisions
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
  ; wall "collision" (wraparound)

  LDA target_ppu_h_per_direction, X
  ; ppu_h goes from 20 to 22

  ; $22 is the first row below arena
  CMP #$22
  BNE @ppu_h_20_or_21
@ppu_h_22:
  LDA #$20
  STA target_ppu_h_per_direction, X
  LDA target_ppu_l_per_direction, X
  CLC
  ADC #$40
  STA target_ppu_l_per_direction, X
  JMP @no_wall

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
  CLC
  ADC #$C0
  STA target_ppu_l_per_direction, X
  LDA #$21
  ADC #$00
  STA target_ppu_h_per_direction, X
  JMP @no_wall
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
  LDA target_ppu_l_per_direction, X
  CLC
  ADC #$18
  STA target_ppu_l_per_direction, X
  LDA target_ppu_h_per_direction, X
  ADC #$00
  STA target_ppu_h_per_direction, X
  JMP @no_wall
@no_left:
  CMP #%00011100
  BNE @no_wall

  LDA target_ppu_l_per_direction, X
  CLC
  ADC #$e8
  STA target_ppu_l_per_direction, X
  LDA target_ppu_h_per_direction, X
  ADC #$ff
  STA target_ppu_h_per_direction, X

  ; JMP @no_wall
@no_wall:

  JSR compute_snek_collision
  BNE @next

  JSR compute_thing_collision
  JMP @next

@next:
  STA collidable_per_direction, X
  DEX
  BPL @loop

  LDA #$00
  STA precomputed_are_dirty

  RTS
.endproc

.proc compute_snek_collision
  ; input X = direction index
  ; cobbles Y
  ; returns object type in A

  LDY snek_tail
@snek_loop:
  LDA snek_ppu_h, Y
  CMP target_ppu_h_per_direction, X
  BNE @snext
  LDA snek_ppu_l, Y
  CMP target_ppu_l_per_direction, X
  BNE @snext
  LDA #collidable_type::wall
  RTS
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
  RTS
.endproc

.proc compute_thing_collision
  ; input X = direction index
  ; cobbles Y
  ; returns object type in A

  LDY things_count
  DEY
  BMI @no_collision

@things_loop:
  LDA target_ppu_h_per_direction, X
  CMP things_ppu_h, Y
  BNE @next_thing
  LDA target_ppu_l_per_direction, X
  CMP things_ppu_l, Y
  BNE @next_thing
  TYA
  STA thing_index_per_direction, X
  LDA things_type, Y
  RTS
@next_thing:
  DEY
  BPL @things_loop

@no_collision:
  LDA #collidable_type::nothing
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

chrrom_bankswitch:
  .byte $00, $01, $02, $03

game_state_handlers_l:
  .byte <(waiting_to_start-1)
  .byte <(playing-1)
  .byte <(game_over-1)

game_state_handlers_h:
  .byte >(waiting_to_start-1)
  .byte >(playing-1)
  .byte >(game_over-1)

command_handlers_l:
  .byte <(command_up-1)
  .byte <(command_down-1)
  .byte <(command_left-1)
  .byte <(command_right-1)
  .byte <(command_noop-1)
  .byte <(command_noop-1)
command_handlers_h:
  .byte >(command_up-1)
  .byte >(command_down-1)
  .byte >(command_left-1)
  .byte >(command_right-1)
  .byte >(command_noop-1)
  .byte >(command_noop-1)

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
string_fun: .byte $82, "FUN", $82, $82
string_boring: .byte "BORING"

nametable_main: .incbin "../assets/nametables/main.rle"
nametable_title: .incbin "../assets/nametables/title.rle"
nametable_game_over: .incbin "../assets/nametables/game_over.rle"

command_positions_x:
  .byte $40, $40, $20, $60, $b0, $90

command_positions_y:
  .byte $90, $d0, $b0, $b0, $a0, $a0

snek_delay_per_level:
   ; lv  01  02  03  04  05  06  07  08  09  10  11  12  13  14  15
   .byte 24, 22, 20, 18, 16, 14, 12, 11, 10, 10,  9,  9,  8,  8,  8
   ; lv  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30
   .byte  7,  7,  7,  6,  6,  6,  5,  5,  5,  4,  4,  4,  3,  2,  1

tile_per_directions:
   ; index = old-direction bits, new-direction bits (some are invalid)
   .byte $90 ; 0000 up up
   .byte $58 ; 0001 X up down
   .byte $94 ; 0010 up left
   .byte $95 ; 0011 up right
   .byte $58 ; 0100 X down up
   .byte $91 ; 0101 down down
   .byte $96 ; 0110 down left
   .byte $97 ; 0111 down right
   .byte $98 ; 1000 left up
   .byte $99 ; 1001 left down
   .byte $93 ; 1010 left left
   .byte $58 ; 1011 X left right
   .byte $9a ; 1100 right up
   .byte $9b ; 1101 right down
   .byte $58 ; 1110 X right left
   .byte $92 ; 1111 right right

tile_per_thing:
  .byte $9c ; nothing
  .byte $9d ; wall / enemy
  .byte $8C ; small coin
  .byte $8D ; big coin

metasprite_l_per_command:
  .byte <metasprite_0_data
  .byte <metasprite_1_data
  .byte <metasprite_2_data
  .byte <metasprite_3_data
  .byte <metasprite_4_data
  .byte <metasprite_4_data

metasprite_h_per_command:
  .byte >metasprite_0_data
  .byte >metasprite_1_data
  .byte >metasprite_2_data
  .byte >metasprite_3_data
  .byte >metasprite_4_data
  .byte >metasprite_4_data


.segment "CHR"
.incbin "../assets/chr/sprites.chr"
.incbin "../assets/chr/bg1.chr"
.incbin "../assets/chr/sprites.chr"
.incbin "../assets/chr/bg2.chr"
.incbin "../assets/chr/sprites.chr"
.incbin "../assets/chr/bg3.chr"
.incbin "../assets/chr/sprites.chr"
.incbin "../assets/chr/bg4.chr"

