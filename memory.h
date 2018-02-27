#ifndef _NESSIE_MEMORY_H_
#define _NESSIE_MEMORY_H_

class memory {
private:
	/*
	 * standard memory
	 */
	unsigned char ram[0x10000];

	/*
	 * video memory used by the ppu
	 */
	unsigned char vram[0x4000];

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


	unsigned short ppuscroll_latch = 0x0000;

	void increment_ppuaddr_latch();
	unsigned char ppuaddr_write_count = 0;
	unsigned short ppuaddr_latch = 0x0000;

public:
	memory();

	unsigned char read(unsigned short addr);
	void write(unsigned char val, unsigned short addr);

	void map();
};

#endif /* end of include guard: _NESSIE_MEMORY_H_ */
