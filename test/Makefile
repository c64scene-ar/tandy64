.PHONY: default all clean res

TARGET_NAME = test.exe
TARGET = ${TARGET_NAME}
ASM = nasm
ASMFLAGS = -fobj -Wall
LD = alink
LDFLAGS = -oEXE -m

default: $(TARGET)
all: default

OBJECTS = test.o pztimer.o

%.o: %.asm
	$(ASM) $(ASMFLAGS) $< -o $@

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	echo "Linking..."
	$(LD) $(OBJECTS) $(LDFLAGS) -o $@

clean:
	echo "Cleaning..."
	-rm -f *.o *.map *.exe

test:
	r2 -A -a x86 -b 16 $(TARGET)

run: $(TARGET)
	echo "Running game..."
	dosbox-x -conf ../conf/dosbox-x.conf -c "mount c ./ && dir" -c "c:" -c ${TARGET_NAME}

scrollbar:
	$(ASM) $(ASMFLAGS) $@.asm -o $@.o
	$(LD) $@.o $(LDFLAGS) -o ../bin/$@.exe
	dosbox-x -conf ../conf/dosbox-x.conf -c "mount c ../bin/ && dir" -c "c:" -c "scroll~1.exe"
