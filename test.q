
enum Unix:
	Read   = 0
	Write  = 1
	Open   = 2
	Close  = 3
	Mmap   = 9
	Munmap = 11

Test():
	str := "Testing testing, one... two... three!\nHello World!\n"
	SystemCall(Unix.Write, 1, &str[0], str.length, 0, 0, 0)

