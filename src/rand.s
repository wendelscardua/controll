.zeropage
rng_seed: .res 2
.exportzp rng_seed

.segment "CODE"
.export rand
.proc rand
  ; generate random number (0-255) in A
  LDA rng_seed + 1
  LSR
  LDA rng_seed + 0
  ROR
  EOR rng_seed + 1
  STA rng_seed + 1 ; high part of x ^= x << 7 done
  ROR              ; A has now x >> 9 and high bit comes from low byte
  EOR rng_seed + 0
  STA rng_seed + 0 ; x ^= x >> 9 and the low part of x ^= x << 7 done
  EOR rng_seed + 1
  STA rng_seed + 1 ; x ^= x << 8 done
  RTS
.endproc
