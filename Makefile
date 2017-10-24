.PHONY: default all clean res

TARGET_NAME = tandy64.exe
TARGET = bin/${TARGET_NAME}
ASM = nasm
ASMFLAGS = -fobj -Wall
LD = alink
LDFLAGS = -oEXE -m

default: $(TARGET)
all: res default

OBJECTS = main.o intro.o pre_intro.o post_intro.o pztimer.o

%.o: src/%.asm
	$(ASM) $(ASMFLAGS) $< -o $@

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	echo "Linking..."
	$(LD) $(OBJECTS) $(LDFLAGS) -o $@

clean:
	echo "Cleaning..."
	-rm -f *.o
	-rm -f bin/*.map

res:
	echo "Generating resources..."
	python3 tools/convert_gfx_to_bios_format.py res/PVM\ Logo\ PC.data -g 9 -o src/logo.raw
	python3 tools/convert_vgm_to_pvm.py res/uc-adogi.vgm
	cp res/uc-adogi.pvm src/uctumi-song.pvm
	cp res/font_unknown_2x2-charset.bin src/
	cp res/c64_charset-charset.bin src/

run: $(TARGET)
	echo "Running game..."
	dosbox-x -conf conf/dosbox-x.conf -c "mount c bin/ && dir" -c "c:" -c ${TARGET_NAME}

x: $(TARGET)
	echo "Compressing game..."
	-upx -9 --8086 $(TARGET)

runx: x
	echo "Running game..."
	dosbox-x -conf conf/dosbox-x.conf -c "mount c bin/ && dir" -c "c:" -c ${TARGET_NAME}
