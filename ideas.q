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

MallNinja(a) // MallNinja(Alpha)
MallNinja(b) // MallNinja(Alpha.Bravo)
MallNinja(b) // MallNinja(Alpha.Bravo)

MallNinja(alpha   : Alpha)
MallNinja(beta    : Alpha.Bravo)
MallNinja(charlie : Alpha.Charlie)


// Conditional polymorphism
struct A:
	kind : bool

struct B:
	inline A -> kind
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
Generic :: { A, B, C, D, ... } -> [Generic Types, Generic Constants]        TOKEN_IDENTIFIER_GENERIC

// Operator overloading with claim keyword
struct Vector2:
	x : float32
	y : float32

claim Vector2 as a + Vector2 as b = Vector2(a.x + b.x, a.y + b.y)

// Conditional claim
a, b, c : bool

claim a if c else b

// Equivilant to:
claim c = a or !c = b // Compiler will know at least one of these expressions is always true

a c b
1 1 0
1 1 1
0 0 1
1 0 1

// @Note: Knowing a and/or b does not tell you c

// Implies:


// Impossible to be true, will constantly evaluate to false:
c and b

// Both sides will always evaluate to the same value,
// compiler can choose just to evaluate one side and not the other:
c or a

if c:
	if a: // Optimized out, can never be false in this context (supposedly).


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

// Match Block
GetColor(fruit : Fruit) -> Color:
	match fruit:
		Lime, Pair, Apple:   return Green
		Lemon, Banana:       return Yellow
		Cherry, Cranberry:   return Red
		Orange:              return Orange

// Match Expression
GetColor(fruit : Fruit) => match fruit:
	Lime, Pair, Apple => Green
	Lemon, Banana     => Yellow
	Cherry, Cranberry => Red
	Orange            => Orange

// Match with array of lambdas?
ReversePhonetic(l : Phonetic):
	return match l -> {
			Alpha   => Foxtrot,
			Bravo   => Echo,
			Charlie => Delta,
			Delta   => Charlie,
			Echo    => Bravo,
			Foxtrot => Alpha
		}

// Match with comparator
//  default would be =
match x:
	< y: return -1
	= y: return  0
	> y: return  1

z = match x:
		< y => -1
		= y =>  0
		> y =>  1

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
		<= 10  => 4
		<= 100 => 7
		// or:
		() => ()

GetSequenceCode(a : int, b : int, c : int) => match:
	(1, 2, 3) => 123
	(4, 5, 6) => 456
	(7, 8, 9) => 789

[Qualia.UseForExponentiation]
Power(a: int, b: int) -> int:
	if b = 0: return 1

	x, y, z, w := 1

	for b AND 3:
		x *= a

	for b >> 2:
		x, y, z, w *= a

	return x * y * z * w

(x:[N]T) op (y:T) = { x[0] op y, x[1] op y, ..  x[N-1] op y }

SumBackwards(nums : []int, from : int, to : int) -> int:
	n := 0
	(from, to) = (from, to) if from <= to else (to, from)

	for j := from, j < to, inc j:
		n += j

	return n

Foo(a : int, b : int = a match: 0, 1 => 42; 2, 3 => 69, c : int):

Foo(a : int, b : int, c : int):
		

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


// Short-String implementation:
struct String:
	size: uint32

	if size > 8:
		chars_ptr: *[size]uint8
	else:
		chars: [size]uint8

IsAlphaNumeric(c: uint8) -> bool:
	return c >= "A" and c <= "Z"
		or c >= "a" and c <= "z"
		or c >= "0" and c <= "9"

Print(string: String):
	if string.size > 8:
		for c in string.chars_ptr where c.IsAlphaNumeric():
			Print(c)
	else:
		for c in string.chars where c.IsAlphaNumeric():
			Print(c)

// once keyword:
// difficulty: trivial
PrintNumbers(numbers : []int):
	for n in numbers:
		once: Print("{ ", n);
		else: Print(", ", n)
	Print(" }");



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
	a, b := (0, 1)
	for i in [0..n]:
		(a, b) = (b, a * b)
	return b

// 'where' keyword:
PrintOdds(nums : []int):
	for n in nums where n & 1:
		Print(n)

// Any type:
Print(format : []uint8, arguments : []Qualia.Any):
	n := 0
	for c in format:
		if c = "%" and n < arguments.count:
			Write(arguments[n])
			inc n
		else:
			Write(c)

// Generics:
Add(array : *[]T, value : T):
	if BitCount(array.count+1) = 1:
		array.data = ReAllocate(array.data, SizeOf(T) * array.count)
	array[array.count] = value
	inc array.count

// Expression functions:
IsNegative(n : int) -> bool => n < 0

// Implied return type:
IsPositive(n : int) => !n.IsNegative()

Sort(a : float32, b : float32) => (a, b) if a <= b else (b, a)

// Aliasing:
alias uint32 as DWORD

// Operator overloading
// Forced to be inline
// Must be pure
// Must return a value
// Comparison operators must return bool

[Qualia.Operator(=)]
IsEqual(a : float32, b : float32) => a - b < 0.01

(a : [4]float32 + b : [4]float32) -> [4]float32 => { a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3] }


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
	DoAlphaStuff(&alpha)
	DoBetaStuff(&beta)

// Relation Table:
A < C
B > C

         A   B   C
      A  =       L 
                   
      B      =   G 
                   
      C  G   L   = 

         A   B   C
      A  =   L   L 
                   
      B  G   =   G 
                   
      C  G   L   = 

	>  : 0 0 1    >
     = : 0 1 0   =
	>= : 0 1 1   =>
	<  : 1 0 0  <
    != : 1 0 1  < >
	<= : 1 1 0  <=
	?? : 1 1 1  <=>






A < C
B > C


  A B C
A = < < 
B > = >
C > < =



B = A + 2
C = B - 1
D = C - 1


   A  B  C  D
A  0 +2 +1  0
B -2  0 -1 -2
C -1 +1  0 -1
D  0 +2 +1  0





B = A + X
C = B - Y
D = A + B


   A  B  C  D  X  Y  Z
A  0 +X               
B -X  0 -Y +B -A -C   
C    +Y  0            
D -B        0         
X    +A        0      
Y    +C           0   
Z                    0

