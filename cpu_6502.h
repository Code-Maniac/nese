#ifndef _NESSIE_CPU_6502_H_
#define _NESSIE_CPU_6502_H_

class cpu_6502 {
private:
	/*
	 * CPU REGISTERS
	 */
	/*
	 * accumulator. A
	 */
	unsigned char A = 0x00;

	/*
	 * indexes. X and Y
	 */
	unsigned char X = 0x00;
	unsigned char Y = 0x00;
	/*
	 * program counter. PC
	 */
	unsigned short PC = 0x0000;
	unsigned char* memory = nullptr;
	/*
	 * stack pointer. S
	 */
	unsigned char S = 0xFF;
	/*
	 * status register. P - uses only 6 bits.
	 * ---- ----
	 * NVsB DIZC
	 * |||| ||||
	 * |||| |||+- Carry: 1 if last addition or shift resulted in a carry, or if last subtraction resulted in no borrow.
	 * |||| ||+-- Zero: 1 if last operation resulted in 0 zero value.
	 * |||| |+--- Interrupt: INterrupt inhibit (0: /IRG and /NMI get rhough; 1: only /NMI gets through)
	 * |||| +---- Decimal: 1 to make ADC and DBC use binary-coded decimal arithmetic (ignored on second-source 6502 like that on the NES)
	 * |||+ ----- Break: break flag - this is actually unused in NES.
	 * ||+- ----- s: No effect, used by the stack copy.
	 * |+-- ----- Overflow: 1 if last ADC or SBC resuled in signed overflow, or D6 from last BIT
	 * +--- ----- Negative: Set to bit 7 of the last operation
	 *
	 * Instruction	Bits 5 and 4	Side effects after pushing
	 * PHP		11		None
	 * BRK		11		I is set to 1
	 * /IRQ		10		I is set to 1
	 * /NMI		10		I is set to 1
	 */
	unsigned char P = 0x00;

	/*
	 * MASTER CLOCK
	 * the frequency of the master clock is 21.477272 MHz
	 */
	double MCF = 21477272;

	/*
	 * clock divisor
	 */
	unsigned char d = 12;

	/*
	 * CPU CLOCK
	 * the frequency of the CPU clock 1.789773 MHz
	 * this is calculated by dividing the master clock freqency by the clock divisor
	 */
	double CPUCF = MCF / d;

	/*
	 * the length of each cycle is approximately 559ns
	 * for each operation that is performed a number of cycles are added.
	 * the next operation should then not be carried out until the time for this number of cycles has elapsed
	 */
	unsigned char cycles = 0;

public:
	cpu_6502();
	cpu_6502(unsigned short pc, unsigned char* mem_ptr);

private:

	unsigned char get_carry();
	unsigned char get_zero();
	unsigned char get_interrupt();
	unsigned char get_decimal();
	unsigned char get_break();
	unsigned char get_overflow();
	unsigned char get_negative();

	void set_carry(unsigned char val, unsigned char bit = 0);
	void set_zero(unsigned char val, unsigned char bit = 0);
	void set_interrupt(unsigned char val, unsigned char bit = 0);
	void set_decimal(unsigned char val, unsigned char bit = 0);
	void set_break(unsigned char val, unsigned char bit = 0);
	void set_overflow(unsigned char val, unsigned char bit = 0);
	void set_negative(unsigned char val, unsigned char bit = 0);

	void set_carry();
	void set_zero();
	void set_interrupt();
	void set_decimal();
	void set_break();
	void set_overflow();
	void set_negative();

	void set_zero_if(unsigned char val);

	void clear_carry();
	void clear_zero();
	void clear_interrupt();
	void clear_decimal();
	void clear_break();
	void clear_overflow();
	void clear_negative();

	void push_stack(unsigned char val);

	unsigned char pull_stack();

	enum address_mode {
		IMP,
		IMM,
		ZP,
		ZPX,
		ZPY,
		ABS,
		ABX,
		ABY,
		IND,
		IZX,
		IZY,
		REL
	};

	/*
	 * read absolute value from memory
	 */
	unsigned char read(unsigned short addr);

	void write(unsigned char val, unsigned short addr);

	/*
	 * read address from memory based on current PC and addressing mode.
	 */
	unsigned short read_address(address_mode addr_mode);

	/*
	 * read value from memory based on current address mode.
	 */
	unsigned char read_value(address_mode addr_mode);

	/*
	 * get the value of the most significant bits at the address
	 */
	unsigned char read_hi_value(address_mode addr_mode);

	/*
	 * get the value of the least significant bits at the address
	 */
	unsigned char read_lo_value(address_mode addr_mode);

	/*
	 * ARITHMETIC OPERATIONS
	 */
	void ora_op(address_mode addr_mode);
	void and_op_ex(unsigned char val);
	void and_op(address_mode addr_mode);
	void eor_op(address_mode addr_mode);
	void adc_op(address_mode addr_mode);
	void sbc_op(address_mode addr_mode);
	void cmp_op_ex(unsigned char mem_val, unsigned char cmp_val);
	void cmp_op(address_mode addr_mode);
	void cpx_op(address_mode addr_mode);
	void cpy_op(address_mode addr_mode);
	void dec_op(address_mode addr_mode);
	void dex_op(address_mode addr_mode);
	void dey_op(address_mode addr_mode);
	void inc_op(address_mode addr_mode);
	void inx_op(address_mode addr_mode);
	void iny_op(address_mode addr_mode);
	void asl_op(address_mode addr_mode);
	void rol_op(address_mode addr_mode);
	void lsr_op(address_mode addr_mode);
	void ror_op(address_mode addr_mode);

	/*
	 * MOVE OPERATIONS
	 */
	void lda_op(address_mode addr_mode);
	void sta_op(address_mode addr_mode);
	void ldx_op(address_mode addr_mode);
	void stx_op(address_mode addr_mode);
	void ldy_op(address_mode addr_mode);
	void sty_op(address_mode addr_mode);
	void tax_op(address_mode addr_mode);
	void txa_op(address_mode addr_mode);
	void tay_op(address_mode addr_mode);
	void tya_op(address_mode addr_mode);
	void tsx_op(address_mode addr_mode);
	void txs_op(address_mode addr_mode);
	void pla_op(address_mode addr_mode);
	void pha_op(address_mode addr_mode);
	void plp_op(address_mode addr_mode);
	void php_op(address_mode addr_mode);

	/*
	 * JUMP/FLAGS OPERATIONS
	 */
	void bpl_op(address_mode addr_mode);
	void bmi_op(address_mode addr_mode);
	void bvc_op(address_mode addr_mode);
	void bvs_op(address_mode addr_mode);
	void bcc_op(address_mode addr_mode);
	void bcs_op(address_mode addr_mode);
	void bne_op(address_mode addr_mode);
	void beq_op(address_mode addr_mode);
	void brk_op(address_mode addr_mode);
	void rti_op(address_mode addr_mode);
	void jsr_op(address_mode addr_mode);
	void rts_op(address_mode addr_mode);
	void jmp_op(address_mode addr_mode);
	void bit_op(address_mode addr_mode);
	void clc_op(address_mode addr_mode);
	void sec_op(address_mode addr_mode);
	void cld_op(address_mode addr_mode);
	void sed_op(address_mode addr_mode);
	void cli_op(address_mode addr_mode);
	void sei_op(address_mode addr_mode);
	void clv_op(address_mode addr_mode);

	/*
	 * NO OP
	 */
	void nop_op(address_mode addr_mode);

	/*
	 * KIL operation
	 */
	void kil_op(address_mode addr_mode);

	/*
	 * ILLEGAL OPCODES
	 */
	void slo_op(address_mode addr_mode);
	void rla_op(address_mode addr_mode);
	void sre_op(address_mode addr_mode);
	void rra_op(address_mode addr_mode);
	void sax_op(address_mode addr_mode);
	void lax_op(address_mode addr_mode);
	void dcp_op(address_mode addr_mode);
	void isc_op(address_mode addr_mode);
	void anc_op(address_mode addr_mode);
	void anc2_op(address_mode addr_mode);
	void alr_op(address_mode addr_mode);
	void arr_op(address_mode addr_mode);
	void xaa_op(address_mode addr_mode);
	void lax2_op(address_mode addr_mode);
	void axs_op(address_mode addr_mode);
	void ahx_op(address_mode addr_mode);
	void shy_op(address_mode addr_mode);
	void shx_op(address_mode addr_mode);
	void tas_op(address_mode addr_mode);
	void las_op(address_mode addr_mode);

public:
	void next();

};

#endif /* end of include guard: _NESSIE_CPU_6502_H_ */

