bin/qualia: *.cpp *.h
	mkdir -p bin
	nasm -felf64 util.asm -o bin/util.o
	clang -o bin/qualia -march=znver3 -lm -std=c++20 -nostdinc++ -fno-rtti -fno-exceptions qualia.cpp bin/util.o \
		-Og -ggdb -g3 -fno-omit-frame-pointer -DDEBUG
		# -O3 -DDEBUG

run: bin/qualia
	./bin/qualia

time: bin/qualia
	fish -c "time ./bin/qualia"

clean:
	rm bin/qualia

tags: *.cpp *.h
	ctags -R --language-force=c++ --fields=+S *.h

