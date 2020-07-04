#pragma once

enum Platform
{
	PLATFORM_LINUX,
	PLATFORM_MACOS,
	PLATFORM_WINDOWS,
};

enum Arch
{
	ARCH_X86,
	ARCH_ARM,
	ARCH_RISC_V,   // Never support these on purpose?
	ARCH_POWER_PC  // why the fuck would anyone use these?
};

enum Arch_Address_Size
{
	ARCH_ADDRESS_SIZE_8   = 8,     // I think only RISC-V will use this?
	ARCH_ADDRESS_SIZE_16  = 16,
	ARCH_ADDRESS_SIZE_32  = 32,
	ARCH_ADDRESS_SIZE_64  = 64,
	ARCH_ADDRESS_SIZE_128 = 128,
};

struct Target
{
	Arch arch;
	Arch_Address_Size address_size;
	Platform platform;
};
