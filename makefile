DEBUG   = -DDEBUG -O0 -ggdb
RELEASE = -O3

OS_NAME := $(shell uname)

IS_MACOS := 0
IS_LINUX := 0
OS_FILE :=

ifeq ($(OS_NAME), Darwin)
	IS_MACOS=1
	OS_FILE=macos
endif

ifeq ($(OS_NAME), Linux)
	IS_LINUX=1
	OS_FILE=linux
	RELEASE+=-march=x86-64-v3
endif

FLAGS := $(DEBUG)
FLAGS += -std=c++23
FLAGS += -mcmodel=medium -nostdinc++ -fno-rtti -fno-exceptions -Wno-vla-cxx-extension
FLAGS += -Wno-c99-designator -Wno-reorder-init-list -Wshift-op-parentheses
# FLAGS += -MJ compile_commands.json

all: qualia

qualia: *.cc *.h $(OS_FILE).o
	clang -lm -no-pie $(FLAGS) qualia.cc $(OS_FILE).o -o qualia

$(OS_FILE).o: $(OS_FILE).cc
	clang $(FLAGS) -c $^ -o $@

general.o: general.asm
	nasm -felf64 general.asm -o general.o

run: qualia
	./qualia

time: qualia
	fish -c "time ./qualia"

tests/ir: tests/ir.cc *.h $(OS_FILE).o
	clang -lm -no-pie $(FLAGS) tests/ir.cc $(OS_FILE).o -o tests/ir

test: qualia tests/ir
	./run_tests.sh

.PHONY: all run time test clean

clean:
	rm -f qualia
	rm -f tests/ir tests/ir_*
	rm -f *.o
