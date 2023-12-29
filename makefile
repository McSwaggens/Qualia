bin/qualia: *.cpp *.h *.asm
	mkdir -p bin

	# nasm -felf64 linux_bootstrap.asm -o bin/linux_bootstrap.o
	nasm -felf64 general.asm -o bin/general.o

	clang -o bin/qualia\
		-march=znver3\
		-std=c++20 -Wno-c99-designator -Wno-reorder-init-list\
		-nostdinc -nostdinc++\
		-fno-rtti -fno-exceptions\
		qualia.cpp bin/general.o -lm\
		-Og -ggdb -DDEBUG
		# -O3
		# -ggdb -g3 -fno-omit-frame-pointer -DDEBUG
		# -fno-stack-protector\
		# -nostdlib\
		# -O3 -DDEBUG -Og 

run: bin/qualia
	./bin/qualia

time: bin/qualia
	fish -c "time ./bin/qualia"

clean:
	rm bin/qualia
	rm bin/*.o

tags: *.cpp *.h
	ctags -R --language-force=c++ --fields=+S qualia.cpp

