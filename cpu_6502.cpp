#include "cpu_6502.h"

#include <iostream>

cpu_6502::cpu_6502() { }

cpu_6502::cpu_6502(unsigned short pc, unsigned char* mem_ptr) {
	PC = pc;
	memory = mem_ptr;
}

unsigned char cpu_6502::get_carry() { return P & 0x01; }
unsigned char cpu_6502::get_zero()  { return (P & 0x02) >> 1; }
unsigned char cpu_6502::get_interrupt() { return (P & 0x04) >> 2; }
unsigned char cpu_6502::get_decimal() { return (P & 0x08) >> 3; }
unsigned char cpu_6502::get_break() { return (P & 0x10) >> 4; }
unsigned char cpu_6502::get_overflow() { return (P & 0x40) >> 6; }
unsigned char cpu_6502::get_negative() { return (P & 0x80) >> 7; }

void cpu_6502::set_carry(unsigned char val, unsigned char bit) { P = P | (val << -bit); }
void cpu_6502::set_zero(unsigned char val, unsigned char bit) { P = P | (val << (1 - bit)); }
void cpu_6502::set_interrupt(unsigned char val, unsigned char bit) { P = P | (val << (2 - bit)); }
void cpu_6502::set_decimal(unsigned char val, unsigned char bit) { P = P | (val << (3 - bit)); }
void cpu_6502::set_break(unsigned char val, unsigned char bit) { P = P | (val << (4 - bit)); }
void cpu_6502::set_overflow(unsigned char val, unsigned char bit) { P = P | (val << (6 - bit)); }
void cpu_6502::set_negative(unsigned char val, unsigned char bit) { P = P | (val << (7 - bit)); }

void cpu_6502::set_carry() { set_carry(0x01); }
void cpu_6502::set_zero() { set_zero(0x01); }
void cpu_6502::set_interrupt() { set_interrupt(0x01); }
void cpu_6502::set_decimal() { set_decimal(0x01); }
void cpu_6502::set_break() { set_break(0x01); }
void cpu_6502::set_overflow() { set_overflow(0x01); }
void cpu_6502::set_negative() { set_negative(0x01); }

void cpu_6502::set_zero_if(unsigned char val) {
	if(val == 0)
		set_zero();
	else
		clear_zero();
}

void cpu_6502::clear_carry() { set_carry(0x00); }
void cpu_6502::clear_zero() { set_zero(0x00); }
void cpu_6502::clear_interrupt() { set_interrupt(0x00); }
void cpu_6502::clear_decimal() { set_decimal(0x00); }
void cpu_6502::clear_break() { set_break(0x00); }
void cpu_6502::clear_overflow() { set_overflow(0x00); }
void cpu_6502::clear_negative() { set_negative(0x00); }

void cpu_6502::push_stack(unsigned char val) {
	--S;
	write(val, 0x0100 + S);
}

unsigned char cpu_6502::pull_stack() {
	return memory[0x0100 + S++];
}

/*
 * read absolute value from memory
 */
unsigned char cpu_6502::read(unsigned short addr) {
	return memory[addr];
}

void cpu_6502::write(unsigned char val, unsigned short addr) {
	if(addr >= 0x0000 && addr < 0x2000) {
		/*
		 * for 0x0800 bytes is for internal ram.
		 * the next 3 chunks of 0x0800 ram mirror the internal ram
		 */
		unsigned short a = addr % 0x0800;
		memory[a] = val;
		memory[a + 0x0800] = val;
		memory[a + 0x1000] = val;
		memory[a + 0x1800] = val;
	} else if(addr >= 0x2000 && addr < 0x4000) {
		/*
		 * 0x2000-0x2007 contain PPU registers
		 * these registers are mirrored every 8 bytes from 0x2008-0x3FFF
		 */
		for(unsigned short a = (addr - 0x2000) % 0x08; a < 0x4000; a += 0x08) {
			memory[a] = val;
		}
	} else {
		memory[addr] = val;
	}
}

/*
 * read address from memory based on current PC and addressing mode.
 */
unsigned short cpu_6502::read_address(address_mode amode) {
	#define ABSOLUTE(LOC) ((((unsigned short)memory[LOC]) << 8) | (unsigned short)memory[LOC + 1])
	#define INDIRECT(LOC) ((memory[LOC] & 0x00FF) | ((memory[LOC + 1] & 0x00FF) << 8))
	#define PAGE_BOUNDARY_CYCLES(LOC) if(LOC == 0xFF) cycles += 1;

	/*
	 * we can add cycles here
	 */

	switch(amode) {
	case IMP: { //this function should never be called with implied or IMMEDIATE anyway.
		return 0;
	} case IMM: {
		return 0;
	} case ZP: { //zero page Y
		cycles += 3;
		return memory[PC + 1];
	} case ZPX: { //zero page X
		cycles += 4;
		return memory[PC + 1] + X;
	} case ZPY: { //zero page Y
		cycles += 4;
		return memory[PC + 1] + Y;
	} case ABS: { //absolute
		cycles += 4;
		return ABSOLUTE(PC + 1);
	} case ABX: { //absolute X
		cycles += 4;
		PAGE_BOUNDARY_CYCLES(PC + 1);
		return ABSOLUTE(PC + 1) + X;
	} case ABY: { //absolute Y
		cycles += 4;
		PAGE_BOUNDARY_CYCLES(PC + 1);
		return ABSOLUTE(PC + 1) + Y;
	} case IND: { //indirect
		cycles += 5;
		return INDIRECT(PC + 1);
	} case IZX: {
		cycles += 6;
		auto zpx = memory[PC + 1] + X; //get zero page + X address.
		PAGE_BOUNDARY_CYCLES(zpx);
		return INDIRECT(zpx);
	} case IZY: {
		cycles += 5;
		PAGE_BOUNDARY_CYCLES(PC + 1);
		return INDIRECT(PC + 1) + Y;
	} case REL: {
		return PC + (char)memory[PC + 1];
	}
	}
	#undef ABSOLUTE
	#undef INDIRECT
	#undef PAGE_BOUNDARY_CYCLES
	return 0;
}

/*
 * read value from memory based on current address mode.
 */
unsigned char cpu_6502::read_value(address_mode amode) {
	switch(amode) {
	case IMM:
		return memory[PC + 1];
	default:
		return memory[read_address(amode)];
	}
}

/*
 * get the value of the most significant bits at the address
 */
unsigned char cpu_6502::read_hi_value(address_mode amode) {
	return (read_value(amode) >> 4) & 0x0F;
}

/*
 * get the value of the least significant bits at the address
 */
unsigned char cpu_6502::read_lo_value(address_mode amode) {
	return read_value(amode) & 0x0F;
}

/*
 * ARITHMETIC OPERATIONS
 */
void cpu_6502::ora_op(address_mode addr_mode) {
	unsigned char val = read_value(addr_mode);
	A = A | val;
	set_negative(A, 7);
	set_zero_if(A);
}

void cpu_6502::and_op_ex(unsigned char val) {
	A = A & val;
	set_negative(A, 7);
	set_zero_if(A);
}

void cpu_6502::and_op(address_mode addr_mode) {
	and_op_ex(read_value(addr_mode));
}

void cpu_6502::eor_op(address_mode addr_mode) {
	A = A ^ read_value(addr_mode);
	set_negative(A, 7);
	set_zero_if(A);
}

void cpu_6502::adc_op(address_mode addr_mode) {
	unsigned char val = read_value(addr_mode);
	short res = A + val + get_carry();
	if(get_decimal()) {
		if(res > 100) {
			A = res % 100;
			set_carry();
		} else {
			A = res;
			clear_carry();
		}
	} else {
		if(res > 255) {
			A = res % 255;
			set_carry();
			set_overflow();
		} else {
			A = res;
			clear_carry();
			clear_overflow();
		}
	}
	set_negative(A, 7);
	set_zero_if(A);
}

void cpu_6502::sbc_op(address_mode addr_mode) {
	unsigned char mem = read_value(addr_mode);
	short res = A - mem - get_carry();
	if(get_decimal()) {
		if(res < 0) {
			A = (100 + res) % 100;
			clear_carry();
		} else {
			A = res % 100;
			set_carry();
		}
	} else {
		if(res < 0) {
			A = 255 + res;
			clear_carry();
			set_overflow();
		} else {
			A = res;
			set_carry();
			clear_overflow();
		}
	}
	set_negative(A, 7);
	set_zero_if(A);
}

void cpu_6502::cmp_op_ex(unsigned char mem_val, unsigned char cmp_val) {
	if(mem_val < cmp_val) {
		set_negative(cmp_val, 7);
		clear_zero();
		clear_carry();
	} else if(mem_val == cmp_val) {
		clear_negative();
		set_zero();
		set_carry();
	} else {
		set_negative(cmp_val, 7);
		clear_zero();
		set_carry();
	}
}

void cpu_6502::cmp_op(address_mode addr_mode) {
	cmp_op_ex(read_value(addr_mode), A);
}

void cpu_6502::cpx_op(address_mode addr_mode) {
	cmp_op_ex(read_value(addr_mode), X);
}

void cpu_6502::cpy_op(address_mode addr_mode) {
	cmp_op_ex(read_value(addr_mode), Y);
}

void cpu_6502::dec_op(address_mode addr_mode) {
	auto addr = read_address(addr_mode);
	auto val = memory[addr] - 1;
	set_negative(val, 7);
	set_zero_if(val);
	write(val, addr);
}

void cpu_6502::dex_op(address_mode) {
	//implied mode only
	--X;
	set_negative(X, 7);
	set_zero_if(X);
}

void cpu_6502::dey_op(address_mode) {
	--Y;
	set_negative(Y, 7);
	set_zero_if(Y);
}

void cpu_6502::inc_op(address_mode addr_mode) {
	auto addr = read_address(addr_mode);
	auto val = memory[addr] + 1;
	set_negative(val, 7);
	set_zero_if(val);
	write(val, addr);
}

void cpu_6502::inx_op(address_mode) {
	++X;
	set_negative(X, 7);
	set_zero_if(X);
}

void cpu_6502::iny_op(address_mode) {
	++Y;
	set_negative(Y, 7);
	set_zero_if(Y);
}

#define FUNC(OP) \
auto addr = read_address(addr_mode); \
short res = memory[addr] OP; \
unsigned char val = res % 255; \
set_negative(val, 7); \
set_zero_if(val); \
if(res > 255) set_overflow(); \
write(val, addr); \

void cpu_6502::asl_op(address_mode addr_mode) {
	FUNC(* 2);
}

void cpu_6502::rol_op(address_mode addr_mode) {
	FUNC(* 2 + get_carry());
}

void cpu_6502::lsr_op(address_mode addr_mode) {
	FUNC(/ 2);
}

void cpu_6502::ror_op(address_mode addr_mode) {
	FUNC(/ 2 + get_carry() * 0x80);
}

#undef FUNC

/*
 * MOVE OPERATIONS
 */
void cpu_6502::lda_op(address_mode addr_mode) {
	A = read_value(addr_mode);
	set_negative(A, 7);
	set_zero_if(A);
}

void cpu_6502::sta_op(address_mode addr_mode) {
	auto addr = read_address(addr_mode);
	write(A, addr);
}

void cpu_6502::ldx_op(address_mode addr_mode) {
	X = read_value(addr_mode);
	set_negative(X, 7);
	set_zero_if(X);
}

void cpu_6502::stx_op(address_mode addr_mode) {
	auto addr = read_address(addr_mode);
	write(X, addr);
}

void cpu_6502::ldy_op(address_mode addr_mode) {
	Y = read_value(addr_mode);
	set_negative(Y, 7);
	set_zero_if(Y);
}

void cpu_6502::sty_op(address_mode addr_mode) {
	auto addr = read_address(addr_mode);
	write(Y, addr);
}

void cpu_6502::tax_op(address_mode) {
	X = A;
	set_negative(X, 7);
	set_zero_if(X);
}

void cpu_6502::txa_op(address_mode) {
	A = X;
	set_negative(A, 7);
	set_zero_if(A);
}

void cpu_6502::tay_op(address_mode) {
	Y = A;
	set_negative(Y, 7);
	set_zero_if(Y);
}

void cpu_6502::tya_op(address_mode) {
	A = Y;
	set_negative(A, 7);
	set_zero_if(A);
}

void cpu_6502::tsx_op(address_mode) {
	X = S;
	set_negative(X, 7);
	set_zero_if(X);
}

void cpu_6502::txs_op(address_mode) {
	S = X;
}

void cpu_6502::pla_op(address_mode) {
	A = pull_stack();
}

void cpu_6502::pha_op(address_mode) {
	push_stack(A);
}

void cpu_6502::plp_op(address_mode) {
	P = pull_stack();
}

void cpu_6502::php_op(address_mode) {
	push_stack(P);
}

/*
 * JUMP/FLAGS OPERATIONS
 */

#define BRANCH(CONDITION) if(CONDITION) PC = read_address(addr_mode); else PC += 2;

void cpu_6502::bpl_op(address_mode addr_mode) {
	BRANCH(!get_negative());
}

void cpu_6502::bmi_op(address_mode addr_mode) {
	BRANCH(get_negative());
}

void cpu_6502::bvc_op(address_mode addr_mode) {
	BRANCH(!get_overflow());
}

void cpu_6502::bvs_op(address_mode addr_mode) {
	BRANCH(get_overflow());
}

void cpu_6502::bcc_op(address_mode addr_mode) {
	BRANCH(!get_carry());
}

void cpu_6502::bcs_op(address_mode addr_mode) {
	BRANCH(get_carry());
}

void cpu_6502::bne_op(address_mode addr_mode) {
	BRANCH(!get_zero());
}

void cpu_6502::beq_op(address_mode addr_mode) {
	BRANCH(get_zero());
}

#undef BRANCH

void cpu_6502::brk_op(address_mode) {
	push_stack(PC);
	push_stack(P);
	PC = 0xFFFE;
	set_break();
	set_interrupt();
	throw std::exception();
}

void cpu_6502::rti_op(address_mode) {
	P = pull_stack();
	PC = pull_stack();
}

void cpu_6502::jsr_op(address_mode addr_mode) {
	push_stack(PC & 0xFF);
	push_stack((PC >> 8) & 0xFF);
	PC = read_address(addr_mode);
}

void cpu_6502::rts_op(address_mode) {
	PC = ((pull_stack() << 8) & 0xFF00) | (pull_stack() & 0x00FF);
}

void cpu_6502::jmp_op(address_mode addr_mode) {
	PC = read_address(addr_mode);
}

void cpu_6502::bit_op(address_mode addr_mode) {
	unsigned char val = read_value(addr_mode);
	set_negative(val, 7);
	set_overflow(val, 6);
	set_zero_if(A & val);
}

void cpu_6502::clc_op(address_mode) {
	clear_carry();
}

void cpu_6502::sec_op(address_mode) {
	set_carry();
}

void cpu_6502::cld_op(address_mode) {
	clear_decimal();
}

void cpu_6502::sed_op(address_mode) {
	set_decimal();
}

void cpu_6502::cli_op(address_mode) {
	clear_interrupt();
}

void cpu_6502::sei_op(address_mode) {
	set_interrupt();
}

void cpu_6502::clv_op(address_mode) {
	clear_overflow();
}

/*
 * No Operation
 */
void cpu_6502::nop_op(address_mode) { } //do nothing

/*
 * KIL operation
 */
void cpu_6502::kil_op(address_mode) {
	throw std::exception();
}

/*
 * ILLEGAL OPCODES
 */
void cpu_6502::slo_op(address_mode addr_mode) {
	asl_op(addr_mode);
	ora_op(addr_mode);
}

void cpu_6502::rla_op(address_mode addr_mode) {
	rol_op(addr_mode);
	and_op(addr_mode);
}

void cpu_6502::sre_op(address_mode addr_mode) {
	lsr_op(addr_mode);
	eor_op(addr_mode);
}

void cpu_6502::rra_op(address_mode addr_mode) {
	ror_op(addr_mode);
	adc_op(addr_mode);
}

void cpu_6502::sax_op(address_mode addr_mode) {
	write(A & X, read_address(addr_mode));
}

void cpu_6502::lax_op(address_mode addr_mode) {
	lda_op(addr_mode);
	ldx_op(addr_mode);
}

void cpu_6502::dcp_op(address_mode addr_mode) {
	dec_op(addr_mode);
	cmp_op(addr_mode);
}

void cpu_6502::isc_op(address_mode addr_mode) {
	inc_op(addr_mode);
	sbc_op(addr_mode);
}

void cpu_6502::anc_op(address_mode) {
	and_op(IMM);
	asl_op(IMP);
}

void cpu_6502::anc2_op(address_mode) {
	and_op(IMM);
	rol_op(IMP);
}

void cpu_6502::alr_op(address_mode) {
	and_op(IMM);
	lsr_op(IMP);
}

void cpu_6502::arr_op(address_mode) {
	and_op(IMM);
	ror_op(IMP);
}

void cpu_6502::xaa_op(address_mode) {
	txa_op(IMP);
	and_op(IMM);
}

void cpu_6502::lax2_op(address_mode) {
	lda_op(IMM);
	tax_op(IMP);
}

void cpu_6502::axs_op(address_mode addr_mode) {
	unsigned short res = (A & X) - read_value(addr_mode);
	unsigned char val = res % 255;
	set_negative(val, 7);
	set_zero_if(val);
	if(res > 255) set_carry();
}

void cpu_6502::ahx_op(address_mode addr_mode) {
	write(A & X & read_hi_value(addr_mode), read_address(addr_mode));
}

void cpu_6502::shy_op(address_mode addr_mode) {
	write(Y & read_hi_value(addr_mode), read_address(addr_mode));
}

void cpu_6502::shx_op(address_mode addr_mode) {
	write(X & read_hi_value(addr_mode), read_address(addr_mode));
}

void cpu_6502::tas_op(address_mode addr_mode) {
	push_stack(A & X);
	write(S & read_hi_value(addr_mode), read_address(addr_mode));
}

void cpu_6502::las_op(address_mode addr_mode) {
	auto val = read_value(addr_mode) & S;
	A = val;
	X = val;
	Y = val;
}



#define OPCODES(OP) \
/* arithmetic operations */ \
OP(ORA, 0x09, ora_op, IMM, 2) OP(ORA, 0x05, ora_op, ZP,  2) OP(ORA, 0x15, ora_op, ZPX, 2) OP(ORA, 0x01, ora_op, IZX, 2) OP(ORA, 0x11, ora_op, IZY, 2) OP(ORA, 0x0D, ora_op, ABS, 3) OP(ORA, 0x1D, ora_op, ABX, 3) OP(ORA, 0x19, ora_op, ABY, 3) \
OP(AND, 0x29, and_op, IMM, 2) OP(AND, 0x25, and_op, ZP,  2) OP(AND, 0x35, and_op, ZPX, 2) OP(AND, 0x21, and_op, IZX, 2) OP(AND, 0x31, and_op, IZY, 2) OP(AND, 0x2D, and_op, ABS, 3) OP(AND, 0x3D, and_op, ABX, 3) OP(AND, 0x39, and_op, ABY, 3) \
OP(EOR, 0x49, eor_op, IMM, 2) OP(EOR, 0x45, eor_op, ZP,  2) OP(EOR, 0x55, eor_op, ZPX, 2) OP(EOR, 0x41, eor_op, IZX, 2) OP(EOR, 0x51, eor_op, IZY, 2) OP(EOR, 0x4D, eor_op, ABS, 3) OP(EOR, 0x5D, eor_op, ABX, 3) OP(EOR, 0x59, eor_op, ABY, 3) \
OP(ADC, 0x69, adc_op, IMM, 2) OP(ADC, 0x65, adc_op, ZP,  2) OP(ADC, 0x75, adc_op, ZPX, 2) OP(ADC, 0x61, adc_op, IZX, 2) OP(ADC, 0x71, adc_op, IZY, 2) OP(ADC, 0x6D, adc_op, ABS, 3) OP(ADC, 0x7D, adc_op, ABX, 3) OP(ADC, 0x79, adc_op, ABY, 3) \
OP(SBC, 0xE9, sbc_op, IMM, 2) OP(SBC, 0xE5, sbc_op, ZP,  2) OP(SBC, 0xF5, sbc_op, ZPX, 2) OP(SBC, 0xE1, sbc_op, IZX, 2) OP(SBC, 0xF1, sbc_op, IZY, 2) OP(SBC, 0xED, sbc_op, ABS, 3) OP(SBC, 0xFD, sbc_op, ABX, 3) OP(SBC, 0xF9, sbc_op, ABY, 3) \
OP(CMP, 0xC9, cmp_op, IMM, 2) OP(CMP, 0xC5, cmp_op, ZP,  2) OP(CMP, 0xD5, cmp_op, ZPX, 2) OP(CMP, 0xC1, cmp_op, IZX, 2) OP(CMP, 0xD1, cmp_op, IZY, 2) OP(CMP, 0xCD, cmp_op, ABS, 3) OP(CMP, 0xDD, cmp_op, ABX, 3) OP(CMP, 0xD9, cmp_op, ABY, 3) \
OP(CPX, 0xE0, cpx_op, IMM, 2) OP(CPX, 0xE4, cpx_op, ZP,  2) OP(CPX, 0xEC, cpx_op, ABS, 3) \
OP(CPY, 0xC0, cpy_op, IMM, 2) OP(CPY, 0xC4, cpy_op, ZP,  2) OP(CPY, 0xCC, cpy_op, ABS, 3) \
OP(DEC, 0xC6, dec_op, ZP,  2) OP(DEC, 0xD6, dec_op, ZPX, 2) OP(DEC, 0xCE, dec_op, ABS, 3) OP(DEC, 0xDE, dec_op, ABX, 3) \
OP(DEX, 0xCA, dex_op, IMP, 1) \
OP(DEY, 0x88, dey_op, IMP, 1) \
OP(INC, 0xE6, inc_op, ZP,  2) OP(INC, 0xF6, inc_op, ZPX, 2) OP(INC, 0xEE, inc_op, ABS, 3) OP(INC, 0xFE, inc_op, ABX, 3) \
OP(INX, 0xE8, inx_op, IMP, 1) \
OP(INY, 0xC8, iny_op, IMP, 1) \
OP(ASL, 0x0A, asl_op, IMP, 1) OP(ASL, 0x06, asl_op, ZP,  2) OP(ASL, 0x16, asl_op, ZPX, 2) OP(ASL, 0x0E, asl_op, ABS, 3) OP(ASL, 0x1E, asl_op, ABX, 3) \
OP(ROL, 0x2A, rol_op, IMP, 1) OP(ROL, 0x26, rol_op, ZP,  2) OP(ROL, 0x36, rol_op, ZPX, 2) OP(ROL, 0x2E, rol_op, ABS, 3) OP(ROL, 0x3E, rol_op, ABX, 3) \
OP(LSR, 0x4A, lsr_op, IMP, 1) OP(LSR, 0x46, lsr_op, ZP,  2) OP(LSR, 0x56, lsr_op, ZPX, 2) OP(LSR, 0x4E, lsr_op, ABS, 3) OP(LSR, 0x5E, lsr_op, ABX, 3) \
OP(ROR, 0x6A, ror_op, IMP, 1) OP(ROR, 0x66, ror_op, ZP,  2) OP(ROR, 0x76, ror_op, ZPX, 2) OP(ROR, 0x6E, ror_op, ABS, 3) OP(ROR, 0x7E, ror_op, ABX, 3) \
/* move operations */ \
OP(LDA, 0xA9, lda_op, IMM, 2) OP(LDA, 0xA5, lda_op, ZP,  2) OP(LDA, 0xB5, lda_op, ZPX, 2) OP(LDA, 0xA1, lda_op, IZX, 2) OP(LDA, 0xB1, lda_op, IZY, 2) OP(LDA, 0xAD, lda_op, ABS, 3) OP(LDA, 0xBD, lda_op, ABX, 3) OP(LDA, 0xB9, lda_op, ABY, 3) \
OP(STA, 0x85, sta_op, ZP,  2) OP(STA, 0x95, sta_op, ZPX, 2) OP(STA, 0x81, sta_op, IZX, 2) OP(STA, 0x91, sta_op, IZY, 2) OP(STA, 0x8D, sta_op, ABS, 3) OP(STA, 0x9D, sta_op, ABX, 3) OP(STA, 0x99, sta_op, ABY, 3) \
OP(LDX, 0xA2, ldx_op, IMM, 2) OP(LDX, 0xA6, ldx_op, ZP,  2) OP(LDX, 0xB6, ldx_op, ZPY, 2) OP(LDX, 0xAE, ldx_op, ABS, 3) OP(LDX, 0xBE, ldx_op, ABY, 3) \
OP(STX, 0x86, stx_op, ZP,  2) OP(STX, 0x96, stx_op, ZPY, 2) OP(STX, 0x8E, stx_op, ABS, 3) \
OP(LDY, 0xA0, ldy_op, IMM, 2) OP(LDY, 0xA4, ldy_op, ZP,  2) OP(LDY, 0xB4, ldy_op, ZPX, 2) OP(LDY, 0xAC, ldy_op, ABS, 3) OP(LDY, 0xBC, ldy_op, ABX, 3) \
OP(STY, 0x84, sty_op, ZP,  2) OP(STY, 0x94, sty_op, ZPX, 2) OP(STY, 0x8C, sty_op, ABS, 3) \
OP(TAX, 0xAA, tax_op, IMP, 1) \
OP(TXA, 0x8A, txa_op, IMP, 1) \
OP(TAY, 0xA8, tay_op, IMP, 1) \
OP(TYA, 0x98, tya_op, IMP, 1) \
OP(TSX, 0xBA, tsx_op, IMP, 1) \
OP(TXS, 0x9A, txs_op, IMP, 1) \
OP(PLA, 0x68, pla_op, IMP, 1) \
OP(PHA, 0x48, pha_op, IMP, 1) \
OP(PLP, 0x28, plp_op, IMP, 1) \
OP(PHP, 0x08, php_op, IMP, 1) \
/* jump/flags commands */ \
OP(BPL, 0x10, bpl_op, REL, 0) \
OP(BMI, 0x30, bmi_op, REL, 0) \
OP(BVC, 0x50, bvc_op, REL, 0) \
OP(BVS, 0x70, bvs_op, REL, 0) \
OP(BCC, 0x90, bcc_op, REL, 0) \
OP(BCS, 0xB0, bcs_op, REL, 0) \
OP(BNE, 0xD0, bne_op, REL, 0) \
OP(BEQ, 0xF0, beq_op, REL, 0) \
OP(BRK, 0x00, brk_op, IMP, 0) \
OP(RTI, 0x40, rti_op, IMP, 1) \
OP(JSR, 0x20, jsr_op, ABS, 0) \
OP(RTS, 0x60, rts_op, IMP, 0) \
OP(JMP, 0x4C, jmp_op, ABS, 3) OP(JMP, 0x6C, jmp_op, IND, 3) \
OP(BIT, 0x24, bit_op, ZP,  2) OP(BIT, 0x2C, bit_op, ABS, 3) \
OP(CLC, 0x18, clc_op, IMP, 1) \
OP(SEC, 0x38, sec_op, IMP, 1) \
OP(CLD, 0xD8, cld_op, IMP, 1) \
OP(SED, 0xF8, sed_op, IMP, 1) \
OP(CLI, 0x58, cli_op, IMP, 1) \
OP(SEI, 0x78, sei_op, IMP, 1) \
OP(CLV, 0xB8, clv_op, IMP, 1) \
/* No op opcodes */ \
OP(NOP, 0x80, nop_op, IMP, 1) OP(NOP, 0x82, nop_op, IMP, 1) OP(NOP, 0xC2, nop_op, IMP, 1) OP(NOP, 0xE2, nop_op, IMP, 1) OP(NOP, 0x04, nop_op, IMP, 1) OP(NOP, 0x14, nop_op, IMP, 1) OP(NOP, 0x34, nop_op, IMP, 1) \
OP(NOP, 0x44, nop_op, IMP, 1) OP(NOP, 0x54, nop_op, IMP, 1) OP(NOP, 0x64, nop_op, IMP, 1) OP(NOP, 0xD4, nop_op, IMP, 1) OP(NOP, 0xF4, nop_op, IMP, 1) OP(NOP, 0x89, nop_op, IMP, 1) OP(NOP, 0x1A, nop_op, IMP, 1) \
OP(NOP, 0x3A, nop_op, IMP, 1) OP(NOP, 0x5A, nop_op, IMP, 1) OP(NOP, 0x7A, nop_op, IMP, 1) OP(NOP, 0xDA, nop_op, IMP, 1) OP(NOP, 0xEA, nop_op, IMP, 1) OP(NOP, 0xFA, nop_op, IMP, 1) OP(NOP, 0x0C, nop_op, IMP, 1) \
OP(NOP, 0x1C, nop_op, IMP, 1) OP(NOP, 0x3C, nop_op, IMP, 1) OP(NOP, 0x5C, nop_op, IMP, 1) OP(NOP, 0x7C, nop_op, IMP, 1) OP(NOP, 0xDC, nop_op, IMP, 1) OP(NOP, 0xFC, nop_op, IMP, 1) \
/* Kill opcodes */ \
OP(KIL, 0x02, kil_op, IMP, 0) OP(KIL, 0x12, kil_op, IMP, 0) OP(KIL, 0x22, kil_op, IMP, 0) OP(KIL, 0x32, kil_op, IMP, 0) OP(KIL, 0x42, kil_op, IMP, 0) OP(KIL, 0x52, kil_op, IMP, 0) \
OP(KIL, 0x62, kil_op, IMP, 0) OP(KIL, 0x72, kil_op, IMP, 0) OP(KIL, 0x92, kil_op, IMP, 0) OP(KIL, 0xB2, kil_op, IMP, 0) OP(KIL, 0xD2, kil_op, IMP, 0) OP(KIL, 0xF2, kil_op, IMP, 0) \
/* Illegal opcodes */ \
OP(SLO, 0x07, slo_op, ZP,  2) OP(SLO, 0x17, slo_op, ZPX, 2) OP(SLO, 0x03, slo_op, IZX, 2) OP(SLO, 0x13, slo_op, IZY, 2) OP(SLO, 0x0F, slo_op, ABS, 3) OP(SLO, 0x1F, slo_op, ABX, 3) OP(SLO, 0x1B, slo_op, ABY, 3) \
OP(RLA, 0x27, rla_op, ZP,  2) OP(RLA, 0x37, rla_op, ZPX, 2) OP(RLA, 0x23, rla_op, IZX, 2) OP(RLA, 0x33, rla_op, IZY, 2) OP(RLA, 0x2F, rla_op, ABS, 3) OP(RLA, 0x3F, rla_op, ABX, 3) OP(RLA, 0x3B, rla_op, ABY, 3) \
OP(SRE, 0x47, sre_op, ZP,  2) OP(SRE, 0x57, sre_op, ZPX, 2) OP(SRE, 0x43, sre_op, IZX, 2) OP(SRE, 0x53, sre_op, IZY, 2) OP(SRE, 0x4F, sre_op, ABS, 3) OP(SRE, 0x5F, sre_op, ABX, 3) OP(SRE, 0x5B, sre_op, ABY, 3) \
OP(RRA, 0x67, rra_op, ZP,  2) OP(RRA, 0x77, rra_op, ZPX, 2) OP(RRA, 0x63, rra_op, IZX, 2) OP(RRA, 0x73, rra_op, IZY, 2) OP(RRA, 0x6F, rra_op, ABS, 3) OP(RRA, 0x7F, rra_op, ABX, 3) OP(RRA, 0x7B, rra_op, ABY, 3) \
OP(SAX, 0x87, sax_op, ZP,  2) OP(SAX, 0x97, sax_op, ZPY, 2) OP(SAX, 0x83, sax_op, IZX, 2) OP(SAX, 0x8F, sax_op, ABS, 3) \
OP(LAX, 0xA7, lax_op, ZP,  2) OP(LAX, 0xB7, lax_op, ZPY, 2) OP(LAX, 0xA3, lax_op, IZX, 2) OP(LAX, 0xB3, lax_op, IZY, 2) OP(LAX, 0xAF, lax_op, ABS, 3) OP(LAX, 0xBF, lax_op, ABY, 3) \
OP(DCP, 0xC7, dcp_op, ZP,  2) OP(DCP, 0xD7, dcp_op, ZPX, 2) OP(DCP, 0xC3, dcp_op, IZX, 2) OP(DCP, 0xD3, dcp_op, IZY, 2) OP(DCP, 0xCF, dcp_op, ABS, 3) OP(DCP, 0xDF, dcp_op, ABX, 3) OP(DCP, 0xDB, dcp_op, ABY, 3) \
OP(ISC, 0xE7, isc_op, ZP,  2) OP(ISC, 0xF7, isc_op, ZPX, 2) OP(ISC, 0xE3, isc_op, IZX, 2) OP(ISC, 0xF3, isc_op, IZY, 2) OP(ISC, 0xEF, isc_op, ABS, 3) OP(ISC, 0xFF, isc_op, ABX, 3) OP(ISC, 0xFB, isc_op, ABY, 3) \
OP(ANC, 0x0B, anc_op, IMM, 2) \
OP(ANC, 0x2B, anc2_op,IMM, 2) \
OP(ALR, 0x4B, alr_op, IMM, 2) \
OP(ARR, 0x6B, arr_op, IMM, 2) \
OP(XAA, 0x8B, xaa_op, IMM, 2) \
OP(LAX, 0xAB, lax2_op,IMM, 2) \
OP(AXS, 0xCB, axs_op, IMM, 2) \
OP(SBC, 0xEB, sbc_op, IMM, 3) /* this is an SBC op but with a no op on the end */ \
OP(AHX, 0x93, ahx_op, IZY, 2) OP(AHX, 0x9F, ahx_op, ABY, 3) \
OP(SHY, 0x9C, shy_op, ABX, 3) \
OP(SHX, 0x9E, shx_op, ABY, 3) \
OP(TAS, 0x9B, tas_op, ABY, 3) \
OP(LAS, 0xBB, las_op, ABY, 3)

void cpu_6502::next() {
#define OP(MNEUMONIC, OPCODE, OP, ADDRESS_MODE, OPLEN) \
case OPCODE: printf(#MNEUMONIC " %20x\t%s\t%20x\n", OPCODE & 0xFF, #ADDRESS_MODE, PC & 0xFFFF); OP(ADDRESS_MODE); PC += OPLEN; break;

	bool running = true;
	while(running) {
		unsigned char opcode = memory[PC];
		switch(opcode) {
		OPCODES(OP)
		default:
			printf("    %20x\n", opcode & 0xFF);
			++PC;
			break;
		}
		#undef OPCODES
		#undef OPCODE
	}
}
