#include "memory.h"

void memory::increment_ppuaddr_latch() {
	ppuaddr_latch += ((ram[PPUCTRL] >> 2) & 0x01);
}

memory::memory() { }

unsigned char memory::read(unsigned short addr) {
	if(addr == PPUSTATUS) {
		ppuscroll_latch = 0x0000;
		ppuaddr_write_count = 0;
		ppuaddr_latch = 0x0000;
		return ram[PPUSTATUS];
	} else if(addr == PPUDATA) {
		unsigned char val = ram[ppuaddr_latch];
		increment_ppuaddr_latch();
		return val;
	} else {
		return ram[addr];
	}
}

void memory::write(unsigned char val, unsigned short addr) {
	if(addr >= 0x0000 && addr < 0x2000) {
		/*
		 * for 0x0800 bytes is for internal ram.
		 * the next 3 chunks of 0x0800 ram mirror the internal ram
		 */
		unsigned short a = addr % 0x0800;
		ram[a] = val;
		ram[a + 0x0800] = val;
		ram[a + 0x1000] = val;
		ram[a + 0x1800] = val;
	} else if(addr >= 0x2000 && addr < 0x4000) {
		/*
		 * 0x2000-0x2007 contain PPU registers
		 * these registers are mirrored every 8 bytes from 0x2008-0x3FFF
		 */
		if(addr == PPUSCROLL) {
			ram[ppuscroll_latch++] = val;
		} else if(addr == PPUADDR) {
			ram[PPUADDR] = val;
			auto sval = static_cast<unsigned short>(val);
			if(ppuaddr_write_count == 0) {
				ppuaddr_latch = ppuaddr_latch | (sval << 8);
			} else if(ppuaddr_write_count == 1) {
				ppuaddr_latch = ppuaddr_latch | sval;
				ppuaddr_latch = ppuaddr_latch % 0x4000;
			}
		} else if(addr == PPUDATA) {
			vram[ppuaddr_latch] = val;
			increment_ppuaddr_latch();
		}

		for(unsigned short a = (addr - 0x2000) % 0x08; a < 0x4000; a += 0x08) {
			ram[addr + a] = val;
		}
	} else {
		ram[addr] = val;
	}
}
