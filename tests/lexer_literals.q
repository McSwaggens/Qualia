// Comprehensive literal lexing tests

TestDecimalIntegers():
	a := 0
	b := 1
	c := 42
	d := 123
	e := 999999
	f := 1234567890

TestHexLiterals():
	a := Ah
	b := Fh
	c := 10h
	d := FFh
	e := ABCh
	f := DEFh
	g := 123h
	h := ABCDEFh
	i := DEADBEEFh
	j := abch
	k := defh
	l := deadbeefh

TestBinaryLiterals():
	a := 0b
	b := 1b
	c := 10b
	d := 11b
	e := 1010b
	f := 11110000b
	g := 10101010b
	h := 11111111b

TestLiteralsWithUnderscores():
	a := 1_000
	b := 1_000_000
	c := 999_999_999
	d := FF_FFh
	e := DEAD_BEEFh
	f := 1111_0000b
	g := 1010_1010_1010_1010b

TestSizeQualifiers8Bit():
	a := 0s8
	b := 127s8
	c := 0u8
	d := 255u8

TestSizeQualifiers16Bit():
	a := 0s16
	b := 32767s16
	c := 0u16
	d := 65535u16

TestSizeQualifiers32Bit():
	a := 0s32
	b := 2147483647s32
	c := 0u32
	d := 4294967295u32

TestSizeQualifiers64Bit():
	a := 0s64
	b := 9223372036854775807s64
	c := 0u64
	d := 18446744073709551615u64

TestSignQualifiers():
	a := 42s
	b := 42u
	c := 0s
	d := 0u
	e := 999s
	f := 999u

TestCombinedSizeAndSign():
	a := 42u8
	b := 42s8
	c := 42u16
	d := 42s16
	e := 42u32
	f := 42s32
	g := 42u64
	h := 42s64

TestScaleQualifiersKilo():
	a := 1k
	b := 2k
	c := 10k
	d := 100k
	e := 1ku64
	f := 1ks64

TestScaleQualifiersMega():
	a := 1m
	b := 2m
	c := 10m
	d := 100m
	e := 1mu64
	f := 1ms64

TestScaleQualifiersGiga():
	a := 1g
	b := 2g
	c := 10g
	d := 100g
	e := 1gu64
	f := 1gs64

TestScaleQualifiersTera():
	a := 1t
	b := 2t
	c := 10t
	d := 1tu64
	e := 1ts64

TestScaleQualifiersPeta():
	a := 1p
	b := 2p
	c := 1pu64
	d := 1ps64

TestScaleQualifiersExa():
	a := 1e
	b := 2e
	c := 1eu64
	d := 1es64

TestMixedScaleAndSize():
	a := 1ku16
	b := 1ku32
	c := 1ku64
	d := 1mu32
	e := 1mu64
	f := 1gu64

TestHexWithQualifiers():
	a := Ahs8
	b := FFhs16
	c := ABCDhs32
	d := DEADBEEFhs64
	e := Ahu8
	f := FFhu16
	g := ABCDhu32
	h := DEADBEEFhu64

TestBinaryWithQualifiers():
	a := 11111111bs8
	b := 1111111111111111bs16
	c := 11111111111111111111111111111111bs32
	d := 10101010b
	e := 10101010bu8
	f := 1010101010101010bu16

TestFloatLiterals():
	a := 0.0
	b := 1.0
	c := 3.14
	d := 2.718
	e := 0.5
	f := 123.456
	g := 999.999

TestFloatWithQualifier():
	a := 3.14f
	b := 2.718f
	c := 0.5f
	d := 1.0f

TestFloatWithSize():
	a := 3.14f32
	b := 2.718f32
	c := 0.5f64
	d := 1.0f64
	e := 123.456f32
	f := 999.999f64

TestFloatEdgeCases():
	a := 0.0f32
	b := 0.0f64
	c := 1.f32
	d := 1.f64
	e := .5f32
	f := .5f64

TestZeroVariants():
	a := 0
	b := 00
	c := 000
	d := 0h
	e := 0b
	f := 0u8
	g := 0s64
	h := 0.0
	i := 0.0f32

TestLeadingZeros():
	a := 01
	b := 001
	c := 0042
	d := 00000123

TestUnderscoreVariations():
	a := 1_0
	b := 10_0
	c := 1_0_0
	d := 1_2_3_4_5
	e := A_Bh
	f := 1_0_1_0b

TestComplexCombinations():
	a := 1ku64
	b := 2ms32
	c := 10gu64
	d := 100tu64
	e := FFhu16
	f := DEADBEEFhu64
	g := 11111111bu8
	h := 1_000ku64

TestMaxValues():
	max_u8  := 255u8
	max_u16 := 65535u16
	max_u32 := 4294967295u32
	max_s8  := 127s8
	max_s16 := 32767s16
	max_s32 := 2147483647s32

TestScaleArithmetic():
	kilobyte := 1k
	megabyte := 1m
	gigabyte := 1g
	terabyte := 1t
	many := 128m
	lots := 512g

TestMixedBases():
	decimal := 255
	hexadecimal := FFh
	binary := 11111111b
	all_same := decimal

TestFloatPrecision():
	single1 := 3.14159f32
	single2 := 2.71828f32
	double1 := 3.14159265358979f64
	double2 := 2.71828182845905f64

// Edge case: identifiers that look like they could be literals
TestIdentifiersThatAreNotLiterals():
	// These are identifiers, not literals
	abc := 1
	def := 2
	check1 := 3
	value2 := 4
	test3x := 5
	hexadecimal := 6
	binary := 7
	float := 8

// Mixed literal and identifier usage
TestMixedUsage():
	x := 42
	y := x
	z := 42k
	w := FFh
	result := x
	scaled := 1m
	precise := 3.14f64
