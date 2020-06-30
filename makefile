bin/qualia: *.cpp *.h
	mkdir -p bin
	clang -o bin/qualia -g3 -std=c++20 -nostdinc++ -fno-rtti -fno-exceptions *.cpp

run: bin/qualia
	./bin/qualia

clean:
	rm bin/qualia

tags: *.cpp *.h
	ctags -R --language-force=c++ --fields=+S *.h
	# ctags *.h -R --language-force=c++ --c++-kinds=+p --fields=+S --extras=+q

