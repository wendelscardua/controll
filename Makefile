PROJECT=controll
LD65_FLAGS=
CA65_FLAGS=
NSF2DATA=/mnt/c/NESDev/famitone2d/NSF/nsf2data.exe
TEXT2DATA=/mnt/c/NESDev/famitone2d/text2data.exe
FAMITRACKER=/mnt/c/NESDev/famitracker/FamiTracker.exe
EMULATOR=/mnt/c/Games/fceux/fceux.exe

TARGET=${PROJECT}.nes

.PHONY : debug run

default: ${TARGET}

${TARGET}: src/${PROJECT}.o src/reset.o src/readjoy.o src/unrle.o src/rand.o src/audio-data.o
	ld65 $^ -t nes -o ${TARGET} ${LD65_FLAGS}

debug: LD65_FLAGS += -Ln labels.txt --dbgfile ${PROJECT}.nes.dbg
debug: CA65_FLAGS += -g -DDEBUG=1
debug: ${TARGET}

src/${PROJECT}.o: src/${PROJECT}.s src/constants.inc src/header.inc src/famitone2.s \
	assets/metasprites.s assets/bg-palettes.pal assets/sprite-palettes.pal \
	assets/nametables/main.rle \
	assets/chr/bg1.chr \
	assets/chr/bg2.chr \
	assets/chr/bg3.chr \
	assets/chr/bg4.chr \
	assets/chr/sprites.chr
	ca65 src/${PROJECT}.s ${CA65_FLAGS}

src/audio-data.o: src/audio-data.s assets/audio/${PROJECT}-sfx.s assets/audio/${PROJECT}-soundtrack.s
	ca65 src/audio-data.s ${CA65_FLAGS}

assets/audio/${PROJECT}-soundtrack.s: assets/audio/${PROJECT}-soundtrack.txt
	${TEXT2DATA} $^ -ca65 -allin

assets/audio/${PROJECT}-soundtrack.txt: assets/audio/${PROJECT}-soundtrack.ftm
	${FAMITRACKER} $^ -export $@

assets/audio/${PROJECT}-sfx.nsf: assets/audio/${PROJECT}-sfx.ftm
	${FAMITRACKER} assets/audio/${PROJECT}-sfx.ftm -export assets/audio/${PROJECT}-sfx.nsf

assets/audio/${PROJECT}-sfx.s: assets/audio/${PROJECT}-sfx.nsf
	${NSF2DATA} assets/audio/${PROJECT}-sfx.nsf -ca65 -ntsc

%.o: %.s
	ca65 $< ${CA65_FLAGS}

clean:
	rm src/*.o *.nes labels.txt *.dbg

run: debug
	${EMULATOR} ${PROJECT}.nes
