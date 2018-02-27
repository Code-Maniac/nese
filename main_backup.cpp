
	void ora_op() {
		A = A | memory[PC];
		set_negative(A, 7);
		set_zero_if(A);
		PC += 2;
	}

	void and_op_ex(unsigned char val) {
		A = A & val;
		set_negative(A, 7);
		set_zero_if(A);
	}

	void and_op() {
		and_op_ex(memory[PC]);
		PC += 2;
	}

	void eor_op() {
		A = A ^ memory[PC];
		set_negative(A, 7);
		set_zero_if(A);
		PC += 2;
	}

	void adc_op() {
		unsigned char val = memory[PC];
		short res = A + val + get_carry();
		/* if(get_decimal()) { */
		/* 	if(res > 100) { */
		/* 		A = res % 100; */
		/* 		set_carry(); */
		/* 	} else { */
		/* 		A = res; */
		/* 		clear_carry(); */
		/* 	} */
		/* } else { */
			if(res > 255) {
				A = res % 255;
				set_carry();
				set_overflow();
			} else {
				A = res;
				clear_carry();
				clear_overflow();
			}
		/* } */
		set_negative(A, 7);
		set_zero_if(A);
		PC += 2;
	}

	void sbc_op_ex(unsigned char val) {
		//unsigned char mem = memory[PC];
		short res = A - val - get_carry();
		/* if(get_decimal()) { */
		/* 	if(res < 0) { */
		/* 		A = (100 + res) % 100; */
		/* 		clear_carry(); */
		/* 	} else { */
		/* 		A = res % 100; */
		/* 		set_carry(); */
		/* 	} */
		/* } else { */
			if(res < 0) {
				A = 255 + res;
				clear_carry();
				set_overflow();
			} else {
				A = res;
				set_carry();
				clear_overflow();
			}
		/* } */
		set_negative(A, 7);
		set_zero_if(A);
	}

	void sbc_op() { //revisist this because i'm not 100% sure that the decimal part is right.
		sbc_op_ex(memory[PC]);
		PC += 2;
	}

	void cmp_op_ex(unsigned char mem_val, unsigned char cmp_val) {
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

	void cmp_op() {
		cmp_op_ex(memory[PC], A);
		PC += 2;
	}

	void cpx_op() {
		cmp_op_ex(memory[PC], X);
		PC += 2;
	}

	void cpy_op() {
		cmp_op_ex(memory[PC], Y);
		PC += 2;
	}

	void dec_op_ex() {
		auto loc = PC + 1;
		--memory[loc];
		set_negative(memory[loc], 7);
		set_zero_if(memory[loc]);
	}

	void dec_op() {
		dec_op_ex();
		PC += 1;
	}

	void dex_op() {
		--X;
		set_negative(X, 7);
		set_zero_if(X);
		++PC;
	}

	void dey_op() {
		--Y;
		set_negative(Y, 7);
		set_zero_if(Y);
		++PC;
	}

	void inc_op_ex() {
		auto loc = PC + 1;
		++memory[loc];
		set_negative(memory[loc], 7);
		set_zero_if(memory[loc]);
	}

	void inc_op() {
		inc_op_ex();
		++PC;
	}

	void inx_op() {
		++X;
		set_negative(X, 7);
		set_zero_if(X);
		++PC;
	}

	void iny_op() {
		++Y;
		set_negative(Y, 7);
		set_zero_if(Y);
		++PC;
	}

	void asl_op_ex() {
		short res = memory[++PC] * 2;
		memory[PC] = res % 255;
		set_negative(memory[PC], 7);
		set_zero_if(memory[PC]);
		if(res > 255) set_overflow();
	}

	void asl_op() {
		asl_op_ex();
		++PC;
	}

	void rol_op_ex() {
		short res = memory[++PC] * 2 + get_carry();
		memory[PC] = res % 255;
		set_negative(memory[PC], 7);
		set_zero_if(memory[PC]);
		if(res > 255) set_overflow();
	}

	void rol_op() {
		rol_op_ex();
		++PC;
	}

	void lsr_op_ex() {
		short res = memory[++PC] / 2;
		memory[PC] = res % 255;
		set_negative(memory[PC], 7);
		set_zero_if(memory[PC]);
		if(res > 255) set_overflow();
	}

	void lsr_op() {
		lsr_op_ex();
		++PC;
	}

	void ror_op_ex() {
		short res = memory[++PC] / 2 + get_carry() * 0x80;
		memory[PC] = res % 255;
		set_negative(memory[PC], 7);
		set_zero_if(memory[PC]);
		if(res > 255) set_overflow();
	}

	void ror_op() {
		ror_op_ex();
		++PC;
	}

	void lda_op_ex(unsigned char val) {
		A = val;
		set_negative(A, 7);
		set_zero_if(A);
	}

	void lda_op() {
		lda_op_ex(memory[++PC]);
		++PC;
	}

	void sta_op() {
		memory[++PC] = A;
		++PC;
	}

	void ldx_op() {
		X = memory[++PC];
		set_negative(X, 7);
		set_zero_if(X);
		++PC;
	}

	void stx_op() {
		memory[++PC] = X;
		++PC;
	}

	void ldy_op() {
		Y = memory[++PC];
		set_negative(Y, 7);
		set_zero_if(Y);
		++PC;
	}

	void sty_op() {
		memory[++PC] = Y;
		++PC;
	}

	void tax_op() {
		X = A;
		set_negative(X, 7);
		set_zero_if(X);
		++PC;
	}

	void txa_op_ex() {
		A = X;
		set_negative(A, 7);
		set_zero_if(A);
	}

	void txa_op() {
		txa_op_ex();
		++PC;
	}

	void tay_op() {
		Y = A;
		set_negative(Y, 7);
		set_zero_if(Y);
		++PC;
	}

	void tya_op() {
		A = Y;
		set_negative(A, 7);
		set_zero_if(A);
		++PC;
	}

	void tsx_op() {
		X = S;
		set_negative(X, 7);
		set_zero_if(X);
		++PC;
	}

	void txs_op() {
		S = X;
		++PC;
	}

	void pla_op() {
		A = pull_stack();
		++PC;
	}

	void pha_op() {
		push_stack(A);
		++PC;
	}

	void plp_op() {
		P = pull_stack();
		++PC;
	}

	void php_op() {
		push_stack(P);
		++PC;
	}

	void bpl_op() {
		if(!get_negative())
			PC = memory[PC + 1];
		else
			PC += 2;
	}

	void bmi_op() {
		if(get_negative())
			PC = memory[PC + 1];
		else
			PC += 2;
	}

	void bvc_op() {
		if(!get_overflow())
			PC = memory[PC + 1];
		else
			PC += 2;
	}

	void bvs_op() {
		if(get_overflow())
			PC = memory[PC + 1];
		else
			PC += 2;
	}

	void bcc_op() {
		if(!get_carry())
			PC = memory[PC + 1];
		else
			PC += 2;
	}

	void bcs_op() {
		if(get_carry())
			PC = memory[PC + 1];
		else
			PC += 2;
	}

	void bne_op() {
		if(!get_zero())
			PC = memory[PC + 1];
		else
			PC += 2;
	}

	void beq_op() {
		if(get_zero())
			PC = memory[PC + 1];
		else
			PC += 2;
	}

	void brk_op() {
		push_stack(PC);
		push_stack(P);
		PC = 0xFFFE;
		set_break();
		set_interrupt();
	}

	void rti_op() {
		P = pull_stack();
		PC = pull_stack();
	}

	void jsr_op() {
		push_stack(PC);
		PC = memory[PC + 1];
	}

	void rts_op() {
		PC = pull_stack() + 2;
	}

	void jmp_op() {
		PC = memory[PC + 1];
	}

	void bit_op() {
		unsigned char mem = memory[++PC];
		set_negative(mem, 7);
		set_overflow(mem, 6);
		set_zero_if(A & mem);
		++PC;
	}

	void clc_op() {
		clear_carry();
		++PC;
	}

	void sec_op() {
		set_carry();
		++PC;
	}

	void cld_op() {
		clear_decimal();
		++PC;
	}

	void sed_op() {
		set_decimal();
		++PC;
	}

	void cli_op() {
		clear_interrupt();
		++PC;
	}

	void sei_op() {
		set_interrupt();
		++PC;
	}

	void clv_op() {
		clear_overflow();
		++PC;
	}

	void nop_op() {
		++PC;
	}

	void slo_op() {
		asl_op_ex();
		ora_op();
	}

	void rla_op() {
		rol_op_ex();
		and_op();
	}

	void sre_op() {
		lsr_op_ex();
		eor_op();
	}

	void rra_op() {
		ror_op_ex();
		adc_op();
	}

	void sax_op() {
		memory[++PC] = A & X;
	}

	void lax_op() {
		lda_op_ex(memory[PC + 1]);
		ldx_op();
	}

	void dcp_op() {
		dec_op_ex();
		cmp_op();
	}

	void isc_op() {
		inc_op_ex();
		sbc_op();
	}

	void anc_op() {
		and_op_ex(memory[0x00]);
		asl_op();
	}

	void anc2_op() {
		and_op_ex(memory[0x00]);
		rol_op();
	}

	void alr_op() {
		and_op_ex(memory[0x00]);
		lsr_op();
	}

	void arr_op() {
		and_op_ex(memory[0x00]);
		ror_op();
	}

	void xaa_op() {
		txa_op_ex();
		and_op_ex(memory[0x00]);
		++PC;
	}

	void lax2_op() {
		lda_op_ex(memory[0x00]);
		tax_op();
	}

	void axs_op() {
		X = (A & X) - memory[0x00];
		++PC;
	}

	void sbc2_op() {
		sbc_op_ex(memory[0x00]);
		nop_op();
	}

	void ahx_op() {
		memory[++PC] = A & X;
	}

	void shy_op() {
		memory[++PC] = Y;
	}

	void shx_op() {
		memory[++PC] = X;
	}

	void tas_op() {
		S = A & X;
		ahx_op();
	}

	void las_op() {
		unsigned char val = memory[++PC] & S;
		A = val;
		X = val;
		Y = val;
	}

#define OPCODES(OP) \
/* Logical and Arithmetic Commands*/ \
OP(ORA, 0x09, ora_op) OP(ORA, 0x05, ora_op) OP(ORA, 0x15, ora_op) OP(ORA, 0x01, ora_op) OP(ORA, 0x11, ora_op) OP(ORA, 0x0D, ora_op) OP(ORA, 0x1D, ora_op) OP(ORA, 0x19, ora_op) \
OP(AND, 0x29, and_op) OP(AND, 0x25, and_op) OP(AND, 0x35, and_op) OP(AND, 0x21, and_op) OP(AND, 0x31, and_op) OP(AND, 0x2D, and_op) OP(AND, 0x3D, and_op) OP(AND, 0x39, and_op) \
OP(EOR, 0x49, eor_op) OP(EOR, 0x45, eor_op) OP(EOR, 0x55, eor_op) OP(EOR, 0x41, eor_op) OP(EOR, 0x51, eor_op) OP(EOR, 0x4D, eor_op) OP(EOR, 0x5D, eor_op) OP(EOR, 0x59, eor_op) \
OP(ADC, 0x69, adc_op) OP(ADC, 0x65, adc_op) OP(ADC, 0x75, adc_op) OP(ADC, 0x61, adc_op) OP(ADC, 0x71, adc_op) OP(ADC, 0x6D, adc_op) OP(ADC, 0x7D, adc_op) OP(ADC, 0x79, adc_op) \
OP(SBC, 0xE9, sbc_op) OP(SBC, 0xE5, sbc_op) OP(SBC, 0xF5, sbc_op) OP(SBC, 0xE1, sbc_op) OP(SBC, 0xF1, sbc_op) OP(SBC, 0xED, sbc_op) OP(SBC, 0xFD, sbc_op) OP(SBC, 0xF9, sbc_op) \
OP(CMP, 0xC9, cmp_op) OP(CMP, 0xC5, cmp_op) OP(CMP, 0xD5, cmp_op) OP(CMP, 0xC1, cmp_op) OP(CMP, 0xD1, cmp_op) OP(CMP, 0xCD, cmp_op) OP(CMP, 0xDD, cmp_op) OP(CMP, 0xD9, cmp_op) \
OP(CPX, 0xE0, cpx_op) OP(CPX, 0xE4, cpx_op) OP(CPX, 0xEC, cpx_op) \
OP(CPY, 0xC0, cpy_op) OP(CPY, 0xC4, cpy_op) OP(CPY, 0xCC, cpy_op) \
OP(DEC, 0xC6, dec_op) OP(DEC, 0xD6, dec_op) OP(DEC, 0xCE, dec_op) OP(DEC, 0xDE, dec_op) \
OP(DEX, 0xCA, dex_op) \
OP(DEY, 0x88, dey_op) \
OP(INC, 0xE6, inc_op) OP(INC, 0xF6, inc_op) OP(INC, 0xEE, inc_op) OP(INC, 0xFE, inc_op) \
OP(INX, 0xE8, inx_op) \
OP(INY, 0xC8, iny_op) \
OP(ASL, 0x0A, asl_op) OP(ASL, 0x06, asl_op) OP(ASL, 0x16, asl_op) OP(ASL, 0x0E, asl_op) OP(ASL, 0x1E, asl_op) \
OP(ROL, 0x2A, rol_op) OP(ROL, 0x26, rol_op) OP(ROL, 0x36, rol_op) OP(ROL, 0x2E, rol_op) OP(ROL, 0x3E, rol_op) \
OP(LSR, 0x4A, lsr_op) OP(LSR, 0x46, lsr_op) OP(LSR, 0x56, lsr_op) OP(LSR, 0x4E, lsr_op) OP(LSR, 0x5E, lsr_op) \
OP(ROR, 0x6A, ror_op) OP(ROR, 0x66, ror_op) OP(ROR, 0x76, ror_op) OP(ROR, 0x6E, ror_op) OP(ROR, 0x7E, ror_op) \
/* Move Commands */ \
OP(LDA, 0xA9, lda_op) OP(LDA, 0xA5, lda_op) OP(LDA, 0xB5, lda_op) OP(LDA, 0xA1, lda_op) OP(LDA, 0xB1, lda_op) OP(LDA, 0xAD, lda_op) OP(LDA, 0xBD, lda_op) OP(LDA, 0xB9, lda_op) \
OP(STA, 0x85, sta_op) OP(STA, 0x95, sta_op) OP(STA, 0x81, sta_op) OP(STA, 0x91, sta_op) OP(STA, 0x8D, sta_op) OP(STA, 0x9D, sta_op) OP(STA, 0x99, sta_op) \
OP(LDX, 0xA2, ldx_op) OP(LDX, 0xA6, ldx_op) OP(LDX, 0xB6, ldx_op) OP(LDX, 0xAE, ldx_op) OP(LDX, 0xBE, ldx_op) \
OP(STX, 0x86, stx_op) OP(STX, 0x96, stx_op) OP(STX, 0x8E, stx_op) \
OP(LDY, 0xA0, ldy_op) OP(LDY, 0xA4, ldy_op) OP(LDY, 0xB4, ldy_op) OP(LDY, 0xAC, ldy_op) OP(LDY, 0xBC, ldy_op) \
OP(STY, 0x84, sty_op) OP(STY, 0x94, sty_op) OP(STY, 0x8C, sty_op) \
OP(TAX, 0xAA, tax_op) \
OP(TXA, 0x8A, txa_op) \
OP(TAY, 0xA8, tay_op) \
OP(TYA, 0x98, tya_op) \
OP(TSX, 0xBA, tsx_op) \
OP(TXS, 0x9A, txs_op) \
OP(PLA, 0x68, pla_op) \
OP(PHA, 0x48, pha_op) \
OP(PLP, 0x28, plp_op) \
OP(PHP, 0x08, php_op) \
/* Branch Commands*/ \
OP(BPL, 0x10, bpl_op) \
OP(BMI, 0x30, bmi_op) \
OP(BVC, 0x50, bvc_op) \
OP(BVS, 0x70, bvs_op) \
OP(BCC, 0x90, bcc_op) \
OP(BCS, 0xB0, bcs_op) \
OP(BNE, 0xD0, bne_op) \
OP(BEQ, 0xF0, beq_op) \
OP(BRK, 0x00, brk_op) \
OP(RTI, 0x40, rti_op) \
OP(JSR, 0x20, jsr_op) \
OP(RTS, 0x60, rts_op) \
OP(JMP, 0x4C, jmp_op) OP(JMP, 0x6C, jmp_op) \
OP(BIT, 0x24, bit_op) OP(BIT, 0x2C, bit_op) \
OP(CLC, 0x18, clc_op) \
OP(SEC, 0x38, sec_op) \
OP(CLD, 0xD8, cld_op) \
OP(SED, 0xF8, sed_op) \
OP(CLI, 0x58, cli_op) \
OP(SEI, 0x78, sei_op) \
OP(CLV, 0xB8, clv_op) \
/* No operation */ \
OP(NOP, 0xEA, nop_op) \
/* Illegal opcodes */ \
OP(SLO, 0x07, slo_op) OP(SLO, 0x17, slo_op) OP(SLO, 0x03, slo_op) OP(SLO, 0x13, slo_op) OP(SLO, 0x0F, slo_op) OP(SLO, 0x1F, slo_op) OP(SLO, 0x1B, slo_op) \
OP(RLA, 0x27, rla_op) OP(RLA, 0x37, rla_op) OP(RLA, 0x23, rla_op) OP(RLA, 0x33, rla_op) OP(RLA, 0x2F, rla_op) OP(RLA, 0x3F, rla_op) OP(RLA, 0x3B, rla_op) \
OP(SRE, 0x47, sre_op) OP(SRE, 0x57, sre_op) OP(SRE, 0x43, sre_op) OP(SRE, 0x53, sre_op) OP(SRE, 0x4F, sre_op) OP(SRE, 0x5F, sre_op) OP(SRE, 0x5B, sre_op) \
OP(RRA, 0x67, rra_op) OP(RRA, 0x77, rra_op) OP(RRA, 0x63, rra_op) OP(RRA, 0x73, rra_op) OP(RRA, 0x6F, rra_op) OP(RRA, 0x7F, rra_op) OP(RRA, 0x7B, rra_op) \
OP(SAX, 0x87, sax_op) OP(SAX, 0x97, sax_op) OP(SAX, 0x83, sax_op) OP(SAX, 0x8F, sax_op) \
OP(LAX, 0xA7, lax_op) OP(LAX, 0xB7, lax_op) OP(LAX, 0xA3, lax_op) OP(LAX, 0xB3, lax_op) OP(LAX, 0xAF, lax_op) OP(LAX, 0xBF, lax_op) \
OP(DCP, 0xC7, dcp_op) OP(DCP, 0xD7, dcp_op) OP(DCP, 0xC3, dcp_op) OP(DCP, 0xD3, dcp_op) OP(DCP, 0xCF, dcp_op) OP(DCP, 0xDF, dcp_op) OP(DCP, 0xDB, dcp_op) \
OP(ISC, 0xE7, isc_op) OP(ISC, 0xF7, isc_op) OP(ISC, 0xE3, isc_op) OP(ISC, 0xF3, isc_op) OP(ISC, 0xEF, isc_op) OP(ISC, 0xFF, isc_op) OP(ISC, 0xFB, isc_op) \
OP(ANC, 0x0B, anc_op) \
OP(ANC, 0x2B, anc2_op) \
OP(ALR, 0x4B, alr_op) \
OP(ARR, 0x6B, arr_op) \
OP(XAA, 0x8B, xaa_op) \
OP(LAX, 0xAB, lax2_op) \
OP(AXS, 0xCB, axs_op) \
OP(SBC, 0xEB, sbc2_op) \
OP(AHX, 0x93, ahx_op) OP(AHX, 0x9F, ahx_op) \
OP(SHY, 0x9C, shy_op) \
OP(SHX, 0x9E, shx_op) \
OP(TAS, 0x9B, tas_op) \
OP(LAS, 0xBB, las_op)

