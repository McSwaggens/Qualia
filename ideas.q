// ---------------- Disable Zero Initialization ---------------- //
// The programmer should have the control to turn off zero initialization.
// Consider the following:
//   There exists a load, L, to the address, P.
//   The value at address P is not zero initialized.
//   No store to P exists prior to L.
// Should the compiler be allowed to assume the value obtained by L is zero?
buffer : [4096]byte = { ? }
buffer : [4096]byte = ?
buffer : [4096]byte = ()
buffer : [4096]byte = %
buffer[0] = 0 // Should b = true?

// ---------------- Memory Semantics ---------------- //
// 
// The compiler is tasked with representing the logic of the program in the most optimal way possible.
// Only memory that the programmer has 'accessed' may be 'touched' by the compiler.
// What memory is 'accessed' must follow branching rules.
// Memory is 'touched' when it has been loaded, stored to, or is executed.
// 
// 
// A pointer only allows the programmer to describe unary/singular access to memory.
// This logic is very useful, but is often not the best tool for the job,
//      especially for modern programs running on modern hardware.
// 
// The problem with *ONLY* allowing the programmer to describe singular access to memory is that it;
//     1) Removes control from the programmer to make informed decissions.
//     2) Cripples the compilers ability to provide good optimizations.
// 
// Take this function for example:
// 
Contains(begin: *int, end: *int, value: int) -> bool:
	for p := begin, p < end:
		if *p = value:
			return true
	return false
// 
// The problem with the above function, which uses a pointer to search through the array,
//     is that the compiler is being prohibited from touching `p+K`,
//     even if `p+K >= begin` and `p+K < end`.
//     This is an issue because the programmer may actually want to allow the touching of `p+K`,
//       .. so long as it's inside the array, of course.
// 
// The language should allow *MULTIPLE* ways to access memory, not just one.


// ---------------- Array Memory Access ---------------- //
// Use of an array should express array memory access,
//     where, all elements of the array are accessed.
// This allows the programmer to easily provoke the compiler
//     into providing a vectorization of a loop, for example.
// 
// Consider the change to the previous function:
Contains(nums: []int, value: int) -> bool = false:
	for n in nums:
		if n = value:
			return true
	return false

// ---------------- Buffered Pointers ---------------- //
// A buffered pointer is used the same way as a regular pointer,
//     however, where a regular pointer only accesses a single element,
//     a buffered pointer accesses N elements.
// The compiler can optimize the function below
//     by testing 8 consecutive elements with a single QWORD test:
SkipZeroes(p : {8}int8) -> {8}int8:
	while *p = 0:
		inc p
	return p


*p+n

p+1

p[n].foo
*p + n

SkipZeroes(p : *int8) -> *int8:
	access [p .. 8]
	while *p = 0:
		inc p
		access [p .. 8]
	return p

// Atomics
atom : @int
atom += 1

// Weak Volatile
volly : *!int = 80000h
*volly = 1
*volly = 2

// Strong Volatile
volly : *!!int = p
*volly = 1
*volly = 1
*volly = *volly+2 if *volly = 1 else *volly+2

// Should the compiler emit a load and store for this?
*volly = *volly

// Get *Foo from *Bar, knowing Foo has a Bar field.
struct Foo:
	a: Bar
	b: Bar
	c: Bar

struct Bar:
	// ..

foo: Foo
bar: *Bar = &foo.b

p := bar as *Foo.b
p := bar as *Foo\.bar
// p = &foo

// Conditional polymorphism
struct Jester:
	kind : bool

struct Moon:
	inline Cow -> kind
	x : int

struct C:
	inline A -> not kind
	y : int

a : *A = GetA()
if a is *B as b:
	b.x = 123

// Targeted Identifiers
Formal  :: { FooBar, Foo_Bar } -> [Functions, Structs, Enums, Enum Members] TOKEN_IDENTIFIER_FORMAL
Casual  :: { fooBar, foo_bar } -> [Variables, Constants, Struct Members]    TOKEN_IDENTIFIER_CASUAL
Bold    :: { FOOBAR, FOO_BAR } -> [Constants, Enum Members]                 TOKEN_IDENTIFIER_BOLD
Generic :: { A, B, C .. Y, Z } -> [Generic Types, Generic Constants]        TOKEN_IDENTIFIER_GENERIC

Runtime Variable: foo_bar, fooBar ->
Weak    Constant: Foo_Bar, FooBar ->
Strong  Constant: FOO_BAR, FOOBAR ->
Generic Constant:

Value:    Binary state
PosValue: A Value who's possibility at runtime has not been disproven
DisValue: A Value that is proven not to be possible at runtime
ProValue: A Value that is proven to be possible at runtime
Variable: Set of PosValues
Constant: Set of ProValues
Strong:   A Constant C that may only be used if |C| = 1
Weak:     A Constant C that is implemented as a Variable if |C| > 1
Generic:  A Strong Constant

Variable Name: First letter lowercase
Generic  Name: Single uppercase letter
Strong   Name: All letters uppercase
Weak     Name: First letter uppercase

a ROTL b
a ROTR b

a >>> b
a <<< b

a <~ b
a ~> b

i < j and= {a,b} and< {x,y}
i < j and  {a,b} and< {x,y}

a<b and= c and< d

if "ABCD" = "ABCD":
if { "A", "B", "C", "D" } = { "A", "B", "C", "D" }:
if { true, true, true, true }:
if true:

if i:
if i != 0:

({a, b} as bool) = (a and b)

a := [N]T

false or a[0] .. and a[N-1]

{a,b}={x,y}

{0,1} as bool
{0,1} != 0
{0,1} != {0, 0}
{false, true} != {false, false}:
(false != false) and (true != false):

// Operator overloading with claim keyword
struct Vector2:
	x : float32
	y : float32

claim Vector2 as a + Vector2 as b = Vector2(a.x + b.x, a.y + b.y)

// inter-procedural nature of claim
Foo(n: int):
	if n != 42: Print("% != 42\n", n)

Bar(n: int, b: bool):
	if b: claim n != 42

Foo(42)
Bar(42, true)

// Output:
//   42 != 42

// Conditional claim
claim if x then y

if x:
	// Compiler can assume 'y' will always = true in this context.

if y:
	// Compiler CANNOT assume 'y' = true.
	// Maybe allow the compiler to test 'x' instead of 'y' if 'x' is local and has no logical effects?

// Generic claim
a : []int
claim if N < 100 then a[N] = N*2

n := a[10]  // Compiler is allowed to assume 'a[10]' will always = '20'. Load will be removed.
j := a[200] // Compiler is forced to load 'a[200]' to get it's value.

i : int
claim if i < 10 then a[i] = 42 // Compiler should give us an illogical claim error. (Or maybe just lose the claim?)

enum Fruit:
	Apple
	Banana
	Cherry
	Cranberry
	Date
	Lime
	Orange
	Pair

// Contextual hints
a : Fruit
a = Date:
	// ...

b : [3]Fruit = { Apple, Lime, Orange }
if b = { Cherry, Date, Pair }:

// Array subscript with multiple indices
nums := { 3, 7, 99, 2, 3, 8, 5, 11, 10, 42 }

even : [4]int = nums[9, 5, 8, 3] // { 42, 8, 10, 2 }

// Variable sized fixed arrays
size := GetBufferSize()
buffer : [size]int

// The type of 'buffer' is '[]int'
// 'buffer.length' = 'size'
// 'buffer.data' will be an address on the stack.

// Even though the value of 'wings' can be known at compile time,
//    the type of 'buffalo' should be '[]int', NOT '[42]int'.
wings := 42
buffalo : [wings]int

// User constants should always be treated as if they were inlined.
BUFFER_SIZE = 4096
buffer : [BUFFER_SIZE]byte // buffer : [4096]byte

struct Output_Buffer:
	head : uint
	buffer : [BUFFER_SIZE]int8

// Match Block
GetColor(fruit : Fruit) -> Color:
	match fruit:
		Lime, Pair, Apple: return Green
		Lemon, Banana:     return Yellow
		Cherry, Cranberry: return Red
		Orange:            return Orange

// Match Expression
GetColor(fruit : Fruit) => match fruit:
		Lime, Pair, Apple => Green,
		Lemon, Banana     => Yellow, Cherry, Cranberry => Red,
		Orange            => Orange

GetColor(fruit : Fruit) => match fruit:
		Lime, Pair, Apple => Green, Lemon, Banana => Yellow, Cherry, Cranberry => Red, Orange => Orange

Foo(match x: = 0 => 1, = 1 => 10, = 2 => 100, = 3 => 1000, 123)
Foo(match x: 0 => 1, 1 => 10, 2 => 100, 3 => 1000; 123)

x => match x { 0 => 1, 1 => 10, 2 => 100, 3 => 1000 }
	* 3

Foo(match x as (0 => 1, 1 => 10, 2 => 100, 3 => 1000), 123)
Foo(1 if x=0 else 10 if x=1 else 100 if x=2 else 1000 if x=3, 123)

// Match with comparator
//  default would be =
match x:
	< y: return -1
	= y: return  0
	> y: return  1

z := match x
	< y => -1,
	= y =>  0,
	> y =>  1

// if a newline is encountered, 
Foo(
	)

+a.Foo()

// Match branch with default:
match x:
	< min: y = min
	> max: y = max
else:
	return x
then:
	return y

// Match expression error:
z = match x:
		<= 10  => 4
		<= 100 => 7
		// Error: All possible values of x not matched.

// Match expression with default:
z = match x:
		<= 10  => 4
		<= 100 => 7
		x => x

// Match expression without reevaluation:
z = match BigComputation():
		<= 10  => 4,
		<= 100 => 7,
		// or:
		() => ()

a := { 1, 3, 3, 7 }
b := { 1, 3, 3, 7 }

x := and ({ a, b, c, d } = { 1, 2, 3, 4 })

struct Foo:
	enabled : bool
	volume  : int

phones : [4]*Phone = { &telephone, &microphone, &xylophone, &megaphone }

if or(phones[..].enabled and phones[..].volume > 50):

p := a.begin
k := b.begin

f : (bool, bool, bool, bool)
(x, y, z, w) = (a, b, c, d) // (bool, bool, bool, bool)

GetSequenceCode(a: int, b: int, c: int) => match (a, b, c):
	(1, 2, 3) => 123
	(4, 5, 6) => 456
	(7, 8, 9) => 789

[Qualia.Operator(^)]
Power(a: int, b: int) -> int:
	c := 1
	for b: c *= a
	return c

(x:[N]T) op (y:T) = { x[0] op y, x[1] op y, ..  x[N-1] op y }

{1,2}+1 // { 2, 3 }
nums[0..3]

SumBackwards(nums: []int, from: int, to: int) -> int:
	n := 0
	(from, to) = (from, to) if from <= to else (to, from)

	for j := from, j < to, inc j:
		n += nums[j]

	return n

// Could do a special check to allow this?
Foo(a: int, b: int = a match: 0, 1 => 42, 2, 3 => 69, c: int):
Foo(a: int, b: int = (a match 0, 1 => 42, 2, 3 => 69), c: int):
//                                      ^ Still non-ambiguous? Pretty sure.


// Struct member visability based on scope
// difficulty: no idea
struct Number:
	signed: bool

	if signed:
		value: int
	else:
		value: uint

PrintNumber(number: Number):
	// Print(number.value) // Error!

	if number.signed:
		Print(number.value) // Prints value: int
	else:
		Print(number.value) // Prints value: uint

PrintSignedNumber(number: Number):
	claim number.signed
	Print(number.value) // Prints value: int

// if specialization:
GetNextPowerOf2(n: uint64) -> uint:
	if$ BitCount(n) = 1:
		return n
	else if$ n > 1:
		return 1 << 64 - CountLeadingZeroes(n)
	else if$ n > 0:
		return 1 << 64 - CountLeadingZeroes(n-1)
	else:
		dec n
		n = n OR n >> 1
		n = n OR n >> 2
		n = n OR n >> 4
		n = n OR n >> 8
		n = n OR n >> 16
		n = n OR n >> 32
		inc n
		return n

GetNextPowerOf2(n: uint64) -> uint where BitCount(n) = 1 => n
GetNextPowerOf2(n: uint64) -> uint where n > 1 => 1 << 64 - CountLeadingZeroes(n)
GetNextPowerOf2(n: uint64) -> uint where n > 0 => 1 << 64 - CountLeadingZeroes(n-1)
GetNextPowerOf2(n: uint64) -> uint:
	dec n
	n = n OR n >> 1
	n = n OR n >> 2
	n = n OR n >> 4
	n = n OR n >> 8
	n = n OR n >> 16
	n = n OR n >> 32
	inc n
	return n


// Short-String implementation:
struct String:
	size: uint32

	if size > 8:
		chars_ptr: *int8
	else:
		chars: [8]int8

IsAlphaNumeric(c: uint8) -> bool:
	return c >= "A" and c <= "Z"
		or c >= "a" and c <= "z"
		or c >= "0" and c <= "9"

Print(string: String):
	if string.size > 8:
		for c in string.chars_ptr where c.IsAlphaNumeric():
			Print(c)
	else for c in string.chars where c.IsAlphaNumeric():
		Print(c)

// once keyword:
// difficulty: trivial
PrintNumbers(numbers : []int):
	for n in numbers:
		once: Print("{ ", n);
		else: Print(", ", n)
	then: Print(" }");

// Range:
Fib(n : int) -> int:
	a := 0
	b := 1
	for i in [0..n]:
		c := a * b
		a = b
		b = c
	return b

// Tuple assignment:
Fib(n : int) -> int:
	(a, b) := (0, 1)
	for i in [0..n]:
		(a, b) = (b, a * b)
	return b

// 'where' keyword:
PrintOdds(nums : []int):
	for n in nums where n & 1:
		Print(n)

struct Vector:
	((x?r), (y?g), (z?b), (w?a)) : (float32, float32, float32, float32)
	alias (x?y?z?w)..(x?y?z?w)..(x?y?z?w)..(x?y?z?w) as (float32, float32, float32, float32)

	((x or r) .. (y or g) .. (z or b) .. (w or a)) : (float32, float32, float32, float32)

	alias x as r
	alias y as g
	alias z as b
	alias w as a

	alias ((x or y or z or w) .. (x or y or z or w)) as (float32, float32)
	alias ((x or y or z or w) .. (x or y or z or w) .. (x or y or z or w)) as (float32, float32, float32)
	alias ((x or y or z or w) .. (x or y or z or w) .. (x or y or z or w) .. (x or y or z or w)) as (float32, float32, float32, float32)

v : Vector = ..
v.x = v.g
v.xyzw = v.wzyx
v.ryza = v.xxxx

Count(a: []T, v: T) -> total: uint:
	for x in a where x = v: inc total

// Constant TypeID:
Print(format: []int8, arguments: [..](Type: Qualia.TypeID, value: Type)):
	n := 0
	for c in format:
		if c = "%" and n < arguments.count:
			match arguments[n].Type:
				int:  buffer.Print(arguments[n].value)
				bool: buffer.Print(arguments[n].value)
				T:    buffer.Print(arguments[n].value)
			inc n
		else:
			buffer.Write(c)

output_buffer.Print("foo = %, bar = %\n", foo, bar)

// Generics:
Add(array: *[]T, value: T):
	if BitCount(array.count+1) = 1:
		array.data = ReAllocate(array.data, SizeOf(T) * array.count)
	array[array.count] = value
	inc array.count

// Expression functions:
IsNegative(n : int) -> bool => n < 0

is Negative(n : int) -> bool => n < 0

for n in nums where n is not Negative:

is(p : *Thing, kind : Kind) -> bool => p >> 12 AND 0b11 = kind

if p is ThingKind.Foo:
	p.foo_things

// Implied return type:
IsPositive(n : int) => !n.IsNegative()

Sort(a : float32, b : float32) => (a, b) if a <= b else (b, a)

// Aliasing:
alias uint8  as HWORD
alias uint16 as WORD
alias uint32 as DWORD
alias uint32 as QWORD

alias HWORD = uint8
alias WORD  = uint16
alias DWORD = uint32
alias QWORD = uint64

// Operator overloading
// Forced to be inline
// Must be pure
// Must return a value
// Comparison operators must return bool

[Qualia.Operator(=)]
IsEqual(a : float32, b : float32) => a - b < 0.01

(a: [4]float32 + b : [4]float32) -> [4]float32 => { a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3] }
+(a: [4]float32, b : [4]float32) => { a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3] }

// Iterator functions:
[Qualia.DefaultIterator]
IterateString(string : String) -> [..]uint8:
	if string.count <= 8:
		for c in string.chars:
			yield c
	else:
		for c in string.chars_ptr:
			yield c

Print(string : String):
	for c in string:
		Print(c)

// struct inlining:
struct Alpha:
	is_beta : bool
	a : int
	b : int

struct Beta:
	inline Alpha
	c : int
	d : int

DoAlphaStuff(alpha : *Alpha):
	alpha.a = 1
	alpha.b = 2

DoBetaStuff(beta : *Beta):
	beta.c = beta.a + 2
	beta.d = beta.b + 3

FooBar():
	beta : Beta
	DoAlphaStuff(&beta)
	DoBetaStuff(&beta)

// Optionals:
struct Foo:
	bar : int
	fiz : int

OptionalAccess(a : *Foo, b : *Foo) -> int:
	return a?.bar ? b?.bar ? 0

OptionalAccess(a : *int, b : *int) -> int:
	a = a ? b
	a ?= b

	k : ?int = a ? b
	j : int = k
	n : int = a ? b ? 0
		
// Enums as catagories
//   - Compiler figures out the best layout for bits
//   - Minimum amount 
enum Alpha:
	// 1 bit
	Bravo:
		// 1 bit
		Delta
		Echo:
			// 2 bit
			India
			Juliet
			Kilo
			Lima
	Charlie:
		// 2 bit
		Foxtrot
		Golf
		Hotel

enum X:
	Y:
		I
		J
		K
		L
	Z:
		N
		W

X.Z.W as X   as int = 5
X.Z.W as X.Z as int = 1

a : Alpha
a = Alpha.Bravo.Delta
a = Alpha.Charlie.Foxtrot

b : Alpha.Bravo
b = Alpha.Bravo.Delta
b = Alpha.Charlie.Foxtrot // Error: Cannot cast Alpha.Charlie to Alpha.Bravo

enum Omega:
	Epsilon:
		Theta
		Delta
	Sigma:
		Psi
		Chi

Xi(o: Omega.Epsilon)   => 1
Xi(o: Omega.Sigma)     => 2
Xi(o: Omega.Sigma.Chi) => 3

Yo(a: Omega, b: Omega) -> int:
	return Xi(a) + Xi(b)

c : Omega
c is Omega.Sigma
// What is the value of c?
c is Omega.Sigma
c is Omega.Sigma.Psi

MallNinja(a) // MallNinja(Alpha)
MallNinja(b) // MallNinja(Alpha.Bravo)
MallNinja(b) // MallNinja(Alpha.Bravo)

MallNinja(alpha:   Alpha)
MallNinja(beta:    Alpha.Bravo)
MallNinja(charlie: Alpha.Charlie)

SystemCall(
	arg0: !int64 asm rax,
	arg1:  int64 asm rdi = *,
	arg2:  int64 asm rsi = *,
	arg3:  int64 asm rdx = *,
	arg4:  int64 asm rcx = *,
	arg5:  int64 asm r10 = *,
	arg6:  int64 asm r8  = *,
	arg7:  int64 asm r9  = *
) -> asm rax inline asm:
	syscall
	return

Foo(asm[rsp+8] a: *int) -> int32:
SystemCall(asm(rax) id: int64, asm(rdi) arg0: int64, asm(rsi) arg1: int64, asm(rdx) arg2: int64, asm(rcx) arg3: int64, asm(r10) arg4: int64, asm(r8) arg5: int64, asm(r9) arg6: int64):

import "StackAllocator"

alias Stack  = StackAllocator.Stack
alias Handle = int32

Sum(nums: ..[]int) -> sum: int = 0:
	for n in nums:
		sum += n
	return sum

Print(format: []int8, arguments: [](Type : Qualia.TypeID, value: )):

Array(T: Qualia.TypeID) -> struct:
	data  : *T
	count : int

Array(T: Qualia.TypeID) -> Qualia.TypeID:
	struct Array:
		data  : *T
		count : int
	return Array

SizeOf(Qualia.TypeID type) => Qualia.GetTypeDescriptor().size
int.SizeOf()

Qualia.Mode = .Debug
Qualia.Mode = .Release

Add(a: int64 : asm(rax), b: int64) -> int64:
	asm (rax, rbx) = (a, b):
		add rax, rbx
		ret

BOOT_SECTOR := &asm:
	

asm:
	test rsi, rsi
	jz exit

asm loop:
	mov rax, [rsi]
	test rax, rax
	jz exit
	inc rsi
	jmp &loop


