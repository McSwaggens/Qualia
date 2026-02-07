// Enum syntax tests

// Simple enum with explicit values
enum Color:
	Red = 0
	Green = 1
	Blue = 2

// Enum with expression values
enum Flags:
	None = 0
	Read = 1
	Write = 2
	Execute = 4

// Enum with larger values
enum Status:
	Pending = 0
	Active = 1
	Paused = 2
	Complete = 3
	Error = 4

// Single member enum
enum Singleton:
	Only = 0

// Enum with binary literal values
enum Bits:
	Bit0 = 1b
	Bit1 = 10b
	Bit2 = 100b
	Bit3 = 1000b

// Enum with hex literal values
enum HexValues:
	Low = 0Fh
	High = F0h
	Full = FFh

// Enum member access
TestEnumAccess():
	// Access enum members
	c := Color.Red
	g := Color.Green
	b := Color.Blue

	// Access members from different enums
	s := Status.Active
	p := Status.Pending

	// Enum member in comparison
	x := Color.Red
	eq := x = Color.Red
	ne := x != Color.Blue

	// Enum member in assignment
	flag := Flags.None
	flag = Flags.Read

	// Enum member in expression
	combined := Flags.Read | Flags.Write

	// Hex enum access
	lo := HexValues.Low
	hi := HexValues.High
