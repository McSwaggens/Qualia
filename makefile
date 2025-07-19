qualia_xxx: *.cpp *.h *.asm
	mkdir -p bin

	# nasm -felf64 linux_bootstrap.asm -o linux_bootstrap.o
	nasm -felf64 general.asm -o general.o

	clang -o qualia \
		-march=znver3 \
		-std=c++20 -Wno-c99-designator -Wno-reorder-init-list \
		-nostdinc -nostdinc++ \
		-fno-rtti -fno-exceptions -Wno-vla-cxx-extension \
		-Wno-all \
		qualia.cpp general.o -lm \
		-O0 -ggdb -DDEBUG -MJ compile_commands.json
		# -O3
		# -ggdb -g3 -fno-omit-frame-pointer -DDEBUG
		# -fno-stack-protector\
		# -nostdlib\
		# -O3 -DDEBUG -Og 

run: qualia
	./qualia

time: qualia
	fish -c "time ./qualia"

clean:
	rm -f qualia
	rm -f *.o

tags: *.cpp *.h
	ctags -R --language-force=c++ --fields=+S *.cpp *.h

