.PHONY: default all clean res

TARGET_NAME = tandy64.exe
TARGET = bin/${TARGET_NAME}
ASM = nasm
ASMFLAGS = -fobj -Wall
LD = alink
LDFLAGS = -oEXE -m

default: $(TARGET)
all: res default

OBJECTS = main.o intro.o pztimer.o border.o

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
	python3 tools/convert_vgm_to_pvm.py res/uc-adogi.vgm -o src/uctumi-song.pvm

run: $(TARGET)
	echo "Running game..."
	dosbox-x -conf conf/dosbox-x.conf -c "mount c bin/ && dir" -c "c:" -c ${TARGET_NAME}

