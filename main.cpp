#include <iostream>
#include <cstring>
#include <vector>
#include <fstream>

#include "cpu_6502.h"

/*
 * see https://wiki.nesdev.com for details on nes hardware.
 */

/*
 * HARDWARE
 */

/*
 * THE NES has 0xFFFF (64 KiB) of memory.
 *
 * $0000-$07FF - 2KiB of internal RAM
 * $0800-$0FFF -
 * $1000-$17FF -
 * $1800-$1FFF - Mirrors of $0000-$07FF
 *
 * $2000-$2007 - Picture Processing registers.
 * $2000 - PPUCTRL - BITS(VPHB SINN)
 *		     NMI enable (V)
 *		     PPU master/slave (P)
 *		     Sprite height (H)
 *		     Background tile select (B)
 *		     Sprite tile select (S)
 *		     Increment mode (I)
 *		     Nametable select (NN)
 * $2001 - PPUMASK - BITS(BGRs bMmG)
 *		     Color emphasis (BGR)
 *		     Sprite enable (s)
 *		     Background enable (b)
 *		     Sprite left column enable (M)
 *		     Background left column enable (m)
 *		     Greyscale (G)
 * $2002 - PPUSTATUS - BITS(VSO- ----)
 *		     vblank (V)
 *		     Sprite 0 hit (S)
 *		     Sprite overflow (O)
 *		     Read resets write pair for $2005/2006
 * $2003 - OAMADDR - BITS(aaaa aaaa) - OAM read/write address
 * $2004 - OAMDATA - BITS(dddd dddd) - OAM data read/write
 * $2005 - PPUSCROLL - BITS(xxxx xxxx) - Fine ctroll position (two writes: X, Y)
 * $2006 - PPUADDR - BITS(aaaa aaaa) - PPU read/write address (two writes:MSB, LSB)
 * $2007 - PPUDATA - BITS(dddd dddd) - PPU data read/write
 *
 * $2008-$3FFF - Mirrors of $2000-$2007 (repeats every 8 bytes)
 *
 * $4000-$4017 - NES APU and I/O registers
 * $4000 - SQ1_VOL - Duty and volume for square wave 1
 * $4001 - SQ1_SWEEP - Sweep control register for square wave 1
 * $4002 - SQ1_LO - Low byte of period for square wave 1
 * $4003 - SQ1_HI - High byte of period and length counter value for square wave 1
 * $4004 - SQ2_VOL - Duty and volume for square wave 2
 * $4005 - SQ2_SWEEP - Sweep control register for square wave 2
 * $4006 - SQ2_LO - Low byte of period for square wave 2
 * $4007 - SQ2_HI - High byte of period and length counter value for square wave 2
 * $4008 - TRI_LINEAR - Triangle wave linear counter
 * $4009 - Unused, but is eventually accessed in memory clearing loops
 * $400A - TRI_LO - Low byte of period for triangle wave
 * $400B - TRI_HI - High byte period of period and length counter value for triangle wave
 * $400C - NOISE_VOL - Volume for noise generator
 * $400D - Unused, but is eventually accessed in memory clearing loops
 * $400E - NOISE_LO - Period and waveform shape for noise generator
 * $400F - NOISE_HI - Length counter value for noise generator
 * $4010 - DMC_FREQ - Play mode and frequency for DMC samples
 * $4011 - DMC_RAW - 7-bit DAC
 * $4012 - DMC_START - Start of DMC waveform is at address $C000 + $40*$xx
 * $4013 - DMC_LEN - Length of DMC waveform is $10*$xx + 1 bytes (128*$xx + 8 samples)
 * $4014 - OAMDMA - BITS(aaaa aaaa) - Writing $xx copies 256 bytes by reading from $xx00-$xxFF and writing to OAMDATA ($2004)
 * $4015 - SND_CHN - Sound channels enable and status
 * $4016 - JOY1 - Joystick 1 data (R) and joystick strobe (W)
 * $4017 - JOY2 - Joystick 2 data (R) and frame counter control (W)
 *
 * $4018-$401F - APU and I/O functionality that is normally disabled.
 * $4020-$FFFF - Cartridge space. PRG ROM, PRG RAM, and mapper registers.
 * $6000-$7FFF - Battry Backed Save or Work RAM
 * $8000-$FFFF - Usual ROM, commonly with Mapper Registers.
 * $FFFA-$FFFB - NMI vector
 * $FFFC-$FFFD - Reset vector
 * $FFFE-$FFFF - IRG/BRK vector
 */

unsigned char internal_memory[0x10000];

enum memory_registers {
	/*
	 * PPU registers
	 */
	PPUCTRL = 0x2000,
	PPUMASK = 0x2001,
	PPUSTATUS = 0x2002,
	OAMADDR = 0x2003,
	OAMDATA = 0x2004,
	PPUSCROLL = 0x2005,
	PPUADDR = 0x2006,
	PPUDATA = 0x2007 ,
	/*
	 * SOUND registers and I/O registers
	 */
	SQ1_VOL = 0x4000,
	SQ1_SWEEP = 0x4001,
	SQ1_LO = 0x4002,
	SQ1_HI = 0x4003,
	SQ2_VOL = 0x4004,
	SQ2_SWEEP = 0x4005,
	SQ2_LO = 0x4006,
	SQ2_HI = 0x4007,
	TRI_LINEAR = 0x4008,
	TRI_LO = 0x400A,
	TRI_HI = 0x400B,
	NOISE_VOL = 0x400C,
	NOISE_LO = 0x400E,
	NOISE_HI = 0x400F,
	DMC_FREQ = 0x4010,
	DMC_RAW = 0x4011,
	DMC_START = 0x4012,
	DMC_LEN = 0x4013,
	OAMDMA = 0x4014,
	SND_CHN = 0x4015,
	JOY1 = 0x4016,
	JOY = 0x4017
};

/*
 * The NES also has 0x1000 for dedicated memory for the Picture Processing Unit.
 *
 * Bytes $0000 - 0x1FFF are used for the ppu patern table.
 * The patern table is an area of memory connected to the PPU that defines the shapes of tiles that make up backgrounds and sprites.
 * Each tile in the pattern table is 16 bytes, made of two planes.
 * The first plane controls bit 0 of the color, the second plane controls bit 1.
 * Any pixel whose color is 0 is background/transparent.
 * The pattern table is divided into two 256-tile sections:
 * $0000-$0FFF, nicknamed "left"
 * $1000-$1FFF, nicknamed "right"
 * Traditionally they are displayed as two side-by-side 128x128 pixel sections, each representing 16x16 tiles from the pattern table.
 *
 * Addressing:
 * PPU addresses within the pattern tables can be decoded as follows:
 * DCBA98 76543210
 * ---------------
 * 0HRRRR CCCCPTTT
 * |||||| |||||+++- T: Fine Y offset, the row number within a tile
 * |||||| ||||+---- P: Bit plane (0: "lower"; 1: "upper")
 * |||||| ++++----- C: Tile column
 * ||++++---------- R: Tile row
 * |+-------------- H: Half of sprite table (0: "left"; 1: "right")
 * +--------------- 0: Pattern table is at $0000-$1FFF
 *
 * The value written to PPUCTRL($2000) controls whether the background and sprites use the left half or the right half of the pattern table.
 * PPUCTRL bit 4 applies to backgrounds, bit 3 applies to 8x8 sprites, and bit 0 of each OAM entry's tile number applies to 8x16 sprites.
 *
 * NAMETABLES
 *
 * The NES has four nametables, arranged in a 2x2 pattern. Each occupies a 1 KiB chunk of PPU address space. Starting at:
 * $2000 at the top left
 * $2400 at the top right
 * $2800 at the bottom left
 * $2C00 at the bottom right
 * But the  NES system board itself has only 2 KiB of VRAM (called CIRAM, stored in a seperate SRAM chip), enough for two nametables.
 * Hardware on the cartridge controls address bit 10 of CIRAM to map one nametable on top of another.
 * Vertical mirroring - $2000 equals $2800 and $2400 equals $2C00
 * Horizontal mirroring - $2000 equals $2400 and $2800 equals $2C00
 * One screen mirroring - All nametables refer to the same memory at a given time and the mapper directly manipulates CIRAM address bit 10.
 * Four-screen mirroring - CIRAM is disabled and the cardridge contains additional VRAM used for all nametables.
 * Other: Some advanced mappers can present arbitrary combinations of CIRAM, VRAM or even CHR ROM in the nametable area.
 *
 * ATTRIBUTE TABLEs
 *
 * The attribute table is a 64 byte array at the end of each nametable that controls which palette is assigned to each part of the background.
 * Each attribute table is arranged as an 8x8 byte array. Startin at:
 * $23C0 for top left
 * $27C0 for top right
 * $2BC0 for bottom left
 * $2C00 for bottom right
 *
 * Each byte controls the palette of a 32x32 pixel or 4x4 tile part of the nametable and is divided into four 2-bit areas.
 * Each area covers 16x16 pixels or 2x2 tiles.
 * Given pallete numbers topleft, topright, bottomleft, bottomright each in the range 0 to 3, the value of the byte is:
 * Int32 value = (topleft << 0) | (topright << 2) | (bottomleft << 4) | (bottomright << 6)
 *
 * PPU PALETTES
 *
 * The palette for the background runs from VRAM $3F00-$3F0F
 * The palette for the sprites runs from $3F10-$3F1F
 * $3F00 - Universal background color
 * $3F01-$3F03 - Background palette 0
 * $3F05-$3F07 - Background palette 1
 * $3F09-$3F0B - Background palette 2
 * $3F0D-$3F0F - Background palette 3
 * $3F11-$3F13 - Sprite palette 1
 * $3F15-$3F17 - Sprite palette 2
 * $3F18-$3F1B - Sprite palette 3
 * $3F1D-$3F1F - Sprite palette 4
 *
 * Each palette contains three colors.
 * Each 16x16 pixel area of the background can use the backdrop color and the three colors from one of the four background palettes.
 * The choice of palette for each 16x16 pixel area is controlled by bits in the attribute table at the end of each nametable.
 * Each sprite can use the three colors from one of the sprite palettes. The choice of palette is in attribute 2 of each sprite.
 * Addresses $3F04/$3F08/$3F0C can contain unique data, though these values are not used by the PPU when normally rendering.
 * Addresses $3F10/$3F14/$3F18/$3F1C are mirros of $3F00/$3F04/$3F08/$3F0C. Note that this goes for writing as well as reading.
 *
 * Incicies into the palette are formed as follows:
 * 43210
 * |||||
 * |||++- Pixel value from tile data.
 * |++--- Palette number from attribute table or OAM.
 * +----- Background/Sprite select.
 *
 * Values in the NES palette are based on hue and brightness
 * 76543210
 * ||||||||
 * ||||++++- Hue (phase, determines NTSC/PAL chroma)
 * ||++----- Value (voltage, determines NTSC/PAL luma)
 * ++------- Unimplemented, reads back as 0
 */
unsigned char ppu_memory[0x4000];

cpu_6502 cpu(0x4020, internal_memory);

struct ines {
	std::string filename;

	struct ines_header {
		char header_data[16];

		/*
		 * check for signature.
		 * NES followed by MS-DOS eol in the first 4 bytes of the header.
		 */
		bool signature_valid() const {
			return header_data[0] == 0x4E &&
			       header_data[1] == 0x45 &&
			       header_data[2] == 0x53 &&
			       header_data[3] == 0x1A;
		}

		/*
		 * size of PRG ROM in 16KB units.
		 */
		unsigned char prg_rom_size() const {
			return header_data[4];
		}

		/*
		 *  size of CHR ROM in 8KB units (0 means the board uses CHR RAM)
		 */
		unsigned char chr_rom_size() const {
			return header_data[5];
		}

		/*
		 * flags on the 6th byte of the header
		 */
		unsigned char flags6() const {
			return header_data[6];
		}

		/*
		 * 0: horizontal
		 * 1: vertical
		 */
		unsigned char mirroring() const {
			return flags6() & 0x01;
		}

		/*
		 * true: cartride contains battry-backed PRG RAM
		 */
		bool prg_ram() const {
			return (flags6() >> 1) & 0x01;
		}

		/*
		 * true: 512B trainer at $7000-$71FF
		 */
		bool trainer() const {
			return (flags6() >> 2) & 0x01;
		}

		/*
		 * true: ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
		 */
		bool ignore_mirroring() const {
			return (flags6() >> 3) & 0x01;
		}

		/*
		 * lower nybble of the mapper number.
		 */
		unsigned char lower_nybble_mapper() const {
			return (flags6() >> 4) & 0x0F;
		}

		/*
		 * flags on the 7th byte of the header
		 */
		unsigned char flags7() const {
			return header_data[7];
		}

		/*
		 * VS Unisystem
		 */
		bool vs_unisystem() const {
			return flags7() & 0x01;
		}

		/*
		 * Playchoice-10 (8KB of Hint Screen data stored after CHR data)
		 */
		bool play_choice() const {
			return (flags7() >> 1) & 0x01;
		}

		/*
		 * if equal to 2 flags 8-15 are in NES 2.0 format
		 */
		unsigned char version() const {
			return (flags7() >> 2) & 0x03;
		}

		/*
		 * upper nybble of the mapper number.
		 */
		unsigned char upper_nybble_mapper() const {
			return (flags7() >> 4) & 0x0F;
		}

		/*
		 * full mapper number
		 */
		unsigned char mapper_number() const {
			return ((flags6() >> 4) & 0x0F) | (flags7() & 0xF0);
		}

		/*
		 * size of PRG RAM in 8KB units (0 infers 8KB for compatibility)
		 */
		unsigned char prg_ram_size() const {
			return header_data[8];
		}

		/*
		 * flags present on the 9th byte of the header
		 */
		unsigned char flags9() const {
			return header_data[9];
		}

		/*
		 * 0: NTSC
		 * 1: PAL
		 */
		unsigned char tv_system() const {
			return flags9() & 0x01;
		}

		/*
		 * flags present on the 10th byte of the header.
		 * flags10 byte is not part of the official specification and very few emulators honor it.
		 */
		unsigned char flags10() const {
			return header_data[10];
		}

		/*
		 * tv_system flag on flag 10
		 * 0: NTSC
		 * 2: PAL
		 * 1/3: duel compatible
		 */
		unsigned char tv_system10() const {
			return flags10() & 0x03;
		}

		/*
		 * is there prg ram present
		 * 0: present
		 * 1: not present
		 */
		unsigned char prg_ram_present() const {
			return (flags10() >> 4) & 0x01;
		}

		/*
		 * does the board have bus conflicts
		 */
		bool has_bus_conflicts() const {
			return (flags10() >> 5) & 0x01;
		}

		/*
		 * bytes 1-15 are 0 filled.
		 */

		/*
		 * print values in the header to cout
		 */
		void print() {
			printf("Signature: %c%c%c\n", header_data[0], header_data[1], header_data[2]);
			printf("PRG ROM data size: 0x4000 * %d Bytes, (%d)\n", prg_rom_size(), prg_rom_size() * 0x4000);
			printf("CHR ROM data size: 0x2000 * %d Bytes, (%d)\n", chr_rom_size(), chr_rom_size() * 0x2000);
			if(mirroring() == 0)
				printf("Mirroring: Vertical\n");
			else if(mirroring() == 1)
				printf("Mirroring: Horizontal\n");
			if(prg_ram())
				printf("Contains battery-backed PRG RAM\n");
			if(trainer())
				printf("Contains 512B Trainer\n");
			if(ignore_mirroring())
				printf("Ignore mirroring bit. Provide four-screen VRAM\n");
			printf("Mapper Number: %d\n", mapper_number());
		}
	};

	ines_header header;

	/*
	 * 0 or 512 bytes.
	 */
	std::vector<char> trainer;

	/*
	 * 16384 * x bytes
	 */
	std::vector<char> prg_rom_data;

	/*
	 * 8192 * y bytes
	 */
	std::vector<char> chr_rom_data;

	/*
	 * playchoice inst-rom if present
	 * 0 or 8192 bytes
	 */
	std::vector<char> playchoice_instrom;

	/*
	 * playchoice prom
	 * 16 bytes data
	 * 16 bytes counterout
	 * often missing
	 */
	std::vector<char> playchoice_prom;

	ines(const char* file) {
		filename = file;
		std::ifstream f(file, std::ios_base::binary);

		f.read((char*)&header, sizeof(ines::ines_header));

		/*
		 * check that the file is a iNES file
		 */
		if(!header.signature_valid()) {
			/* throw std::exception(); */
		}

		/*
		 * read trainer if present
		 */
		if(header.trainer()) {
			trainer.resize(0x200);
			f.read(&trainer[0], sizeof(char) * 0x200);
		}

		size_t prg_rom_data_size = header.prg_rom_size() * 0x4000;
		prg_rom_data.resize(prg_rom_data_size);
		f.read(&prg_rom_data[0], sizeof(char) * prg_rom_data_size);

		size_t chr_rom_data_size = header.chr_rom_size() * 0x1000;
		chr_rom_data.resize(chr_rom_data_size);
		f.read(&chr_rom_data[0], sizeof(char) * chr_rom_data_size);

		if(header.play_choice()) {
			playchoice_instrom.resize(0x1000);
			f.read(&playchoice_instrom[0], sizeof(char) * 0x1000);

			playchoice_prom.resize(0x20);
			f.read(&playchoice_prom[0], sizeof(char) * 0x20);
		}

		header.print();

		f.close();
	}

	~ines() { }
};

void ines_mapper_003(ines& data) {
	for(unsigned short x = 0; x < 0xFFFF; ++x) {
		internal_memory[x] = 0x00;
	}
	memcpy(internal_memory + 0x4020, &data.prg_rom_data[0], data.prg_rom_data.size());

	for(unsigned short x = 0; x < 0x3FFF; ++x) {
		ppu_memory[x] = 0x00;
	}
	memcpy(ppu_memory, &data.chr_rom_data[0], data.chr_rom_data.size());
	printf("%x\n", (unsigned int)data.chr_rom_data.size());
}

int main(int argc, const char *argv[]) {
	if(argc == 2) {
		auto data = ines(argv[1]);
		ines_mapper_003(data);
		cpu.next();
	} else {
		auto data = ines("./donkey_kong_classics.nes");
		ines_mapper_003(data);
		cpu.next();
	}


	return 0;
}
