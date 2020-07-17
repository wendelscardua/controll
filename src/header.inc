.segment "HEADER"
.byte "NES", $1a ; Magic string that always begins an iNES header
.byte $02        ; Number of 16KB PRG-ROM banks
.byte $04        ; Number of 8KB CHR-ROM banks
.byte %00110001  ; Vertical mirroring, no save RAM, mapper 03 (CNROM)
.byte %00000000  ; No special-case flags set, no mapper
.byte $00        ; No PRG-RAM present
.byte $00        ; NTSC format
