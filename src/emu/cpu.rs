//
// cpu.rs --- 6809 CPU core.
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

//! A simple 6809 CPU core.
//!
//! ## Implementation Notes
//!
//! The 6809's many addressing modes are handled by methods
//! such as `CPU::indexed` that fetch additional bytes and
//! calculate the effective address of the memory operand.

#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(non_snake_case)]

use emu::mem::Mem;
use std::fmt;

bitflags!{
    /// The 6809's 8-bit flags register.
    pub flags CCFlags: u8 {
        const CC_E = 0b10000000,
        const CC_F = 0b01000000,
        const CC_H = 0b00100000,
        const CC_I = 0b00010000,
        const CC_N = 0b00001000,
        const CC_Z = 0b00000100,
        const CC_V = 0b00000010,
        const CC_C = 0b00000001,
    }
}

impl CCFlags {
    /// Set one or more flags if a boolean condition is true.
    fn set_if(&mut self, val: Self, cond: bool) {
        if cond {
            self.insert(val);
        } else {
            self.remove(val);
        }
    }
}

impl fmt::Display for CCFlags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}{}{}{}{}{}",
               if self.contains(CC_E) { "E" } else { "" },
               if self.contains(CC_F) { "F" } else { "" },
               if self.contains(CC_H) { "H" } else { "" },
               if self.contains(CC_I) { "I" } else { "" },
               if self.contains(CC_N) { "N" } else { "" },
               if self.contains(CC_Z) { "Z" } else { "" },
               if self.contains(CC_V) { "V" } else { "" },
               if self.contains(CC_C) { "C" } else { "" })
    }
}

/// The set of 6809 CPU registers.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Regs {
    pub a: u8,
    pub b: u8,
    pub x: u16,
    pub y: u16,
    pub u: u16,
    pub s: u16,
    pub pc: u16,
    pub dp: u8,
    pub cc: CCFlags,
}

impl Regs {
    /// Create registers in their power-on state.
    pub fn new() -> Regs {
        Regs {
            a: 0,
            b: 0,
            x: 0,
            y: 0,
            u: 0,
            s: 0,
            pc: 0,
            dp: 0,
            cc: CCFlags::empty(),
        }
    }

    /// Return the value of `A` and `B` combined as the `D` register.
    pub fn d(&self) -> u16 {
        let hi = self.a as u16;
        let lo = self.b as u16;
        (hi << 8) | lo
    }

    /// Set `A` and `B` as the `D` register to a 16-bit value.
    pub fn set_d(&mut self, val: u16) {
        self.a = (val >> 8) as u8;
        self.b = (val >> 0) as u8;
    }

    /// Dump all registers to the standard output.
    pub fn dump(&self) {
        print!("A={:02X} B={:02X} ", self.a, self.b);
        print!("X={:04X} Y={:04X} U={:04X} S={:04X} ",
               self.x, self.y, self.u, self.s);
        println!("PC={:04X} DP={:02X} CC={:02X} ({})",
                 self.pc, self.dp, self.cc.bits, self.cc);
    }
}

/////////////////////////////////////////////////////////////////////
// CPU Emulation

/// The 6809 CPU core.
pub struct CPU<M: Mem> {
    pub regs: Regs,
    pub mem: M,
    fetched: Vec<u8>,           // bytes fetched during op cycle
}

impl <M: Mem> CPU<M> {
    /// Create a new CPU given a memory interface.
    ///
    /// The memory interface will often be a custom board-specific
    /// object that implements address decoding on top of RAM, devices,
    /// etc.
    ///
    /// # Examples
    ///
    /// ```
    /// use jb8::emu::{RAM, CPU};
    ///
    /// let mut cpu = CPU::new(RAM::new(0x10000));
    /// ```
    pub fn new(mem: M) -> CPU<M> {
        CPU {
            regs: Regs::new(),
            mem: mem,
            fetched: Vec::new(),
        }
    }

    /// Dump registers to the standard output.
    pub fn dump_regs(&self) {
        print!("*** {:04X} REGS  ", self.regs.pc);
        self.regs.dump();
    }

    fn execute(&mut self, opcode: u8) {
        cpu_decode!(opcode, self);
    }

    /// Execute the next instruction at the program counter.
    pub fn step(&mut self) {
        let pc = self.regs.pc;

        self.fetched.clear();
        let opcode = self.fetchb();
        self.execute(opcode);

        print!("*** {:04X} FETCH ", pc);
        for b in self.fetched.iter() {
            print!("{:02X} ", *b);
        }

        println!("");
    }

    /// Execute the next `n` instructions starting at the
    /// current value of the program counter.
    pub fn step_n(&mut self, n: usize) {
        for _ in 0..n {
            self.step();
        }
    }

    /// Run the CPU until the PC equals `addr`.
    pub fn run_until(&mut self, pc_stop: u16) {
        while self.regs.pc != pc_stop {
            self.step();
        }
    }

    /// Read an 8-bit value from memory at `pc` then increment `pc` by 1.
    fn fetchb(&mut self) -> u8 {
        let val = self.mem.loadb(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        self.fetched.push(val);
        val
    }

    /// Read a 16-bit value from memory at `pc` then increment `pc` by 2.
    fn fetchw(&mut self) -> u16 {
        let hi = self.fetchb() as u16;
        let lo = self.fetchb() as u16;

        (hi << 8) | lo
    }

    /// Push a byte onto the SP stack.
    fn pushb_s(&mut self, val: u8) {
        self.regs.s = self.regs.s.wrapping_sub(1);
        self.mem.storeb(self.regs.s, val);
    }

    /// Pop a byte from the SP stack.
    fn popb_s(&mut self) -> u8 {
        let val = self.mem.loadb(self.regs.s);
        self.regs.s = self.regs.s.wrapping_add(1);
        val
    }

    /// Push a word onto the SP stack.
    fn pushw_s(&mut self, val: u16) {
        self.pushb_s(((val >> 0) & 0xff) as u8);
        self.pushb_s(((val >> 8) & 0xff) as u8);
    }

    /// Pop a word from the SP stack.
    fn popw_s(&mut self) -> u16 {
        let hi = self.popb_s() as u16;
        let lo = self.popb_s() as u16;
        (hi << 8) | lo
    }

    /// Push a byte onto the user stack.
    fn pushb_u(&mut self, val: u8) {
        self.regs.u = self.regs.u.wrapping_sub(1);
        self.mem.storeb(self.regs.u, val);
    }

    /// Pop a byte from the user stack.
    fn popb_u(&mut self) -> u8 {
        let val = self.mem.loadb(self.regs.u);
        self.regs.u = self.regs.u.wrapping_add(1);
        val
    }

    /// Push a word onto the user stack.
    fn pushw_u(&mut self, val: u16) {
        self.pushb_u(((val >> 0) & 0xff) as u8);
        self.pushb_u(((val >> 8) & 0xff) as u8);
    }

    /// Pop a word from the user stack.
    fn popw_u(&mut self) -> u16 {
        let hi = self.popb_u() as u16;
        let lo = self.popb_u() as u16;
        (hi << 8) | lo
    }
}

/////////////////////////////////////////////////////////////////////
// Addressing Modes

impl <M: Mem> CPU<M> {
    /// 8-bit immediate addressing.
    fn imm8(&mut self) -> u16 {
        let addr = self.regs.pc;
        self.regs.pc = self.regs.pc.wrapping_add(1);

        let val = self.mem.loadb(addr);
        self.fetched.push(val);

        addr
    }

    /// 16-bit immediate addressing.
    fn imm16(&mut self) -> u16 {
        let addr = self.regs.pc;
        self.regs.pc = self.regs.pc.wrapping_add(2);

        let hi = self.mem.loadb(addr);
        let lo = self.mem.loadb(addr.wrapping_add(1));
        self.fetched.push(hi);
        self.fetched.push(lo);

        addr
    }

    /// Extended addressing.
    fn extended(&mut self) -> u16 {
        self.fetchw()
    }

    /// Direct addressing.
    fn direct(&mut self) -> u16 {
        let hi = self.regs.dp as u16;
        let lo = self.fetchb() as u16;

        (hi << 8) | lo
    }

    /// Return a reference to an indexed register from a postbyte.
    fn indexed_reg(&mut self, postbyte: u8) -> &mut u16 {
        match (postbyte >> 5) & 0x03 {
            0b00 => &mut self.regs.x,
            0b01 => &mut self.regs.y,
            0b10 => &mut self.regs.u,
            0b11 => &mut self.regs.s,
            _    => panic!("Unexpected indexed register value"),
        }
    }

    /// Return the register value for an indexed mode postbyte.
    fn indexed_reg_val(&mut self, postbyte: u8) -> u16 {
        *self.indexed_reg(postbyte)
    }

    /// Add a signed offset to an indexed mode register.
    fn indexed_reg_inc(&mut self, postbyte: u8, offset: i16) {
        let p = self.indexed_reg(postbyte);
        *p = (*p as i16).wrapping_add(offset) as u16;
    }

    /// Indexed addressing. By far the most complex addressing mode.
    fn indexed(&mut self) -> u16 {
        let postbyte = self.fetchb();

        // If the top bit is clear, it's `EA = ,R + 5-bit offset`.
        if (postbyte & 0x80) == 0 {
            // Mask out 5-bit signed offset and sign extend.
            let offset = (((postbyte & 0x1f) ^ 0x10).wrapping_sub(0x10)) as i16;
            let val = self.indexed_reg_val(postbyte) as i16;
            return val.wrapping_add(offset) as u16;
        }

        // Otherwise, look up the mode based on the lower 4 bits:
        let ixmode = postbyte & 0x0f;

        match ixmode {
            0b0000 => {     // ,R+
                let addr = self.indexed_reg_val(postbyte);
                self.indexed_reg_inc(postbyte, 1);
                addr
            },
            0b0001 => {     // ,R++
                let addr = self.indexed_reg_val(postbyte);
                self.indexed_reg_inc(postbyte, 2);
                addr
            },
            0b0010 => {     // ,-R
                self.indexed_reg_inc(postbyte, -1);
                self.indexed_reg_val(postbyte)
            },
            0b0011 => {     // ,--R
                self.indexed_reg_inc(postbyte, -2);
                self.indexed_reg_val(postbyte)
            },
            0b0100 => {     // EA = ,R + 0 offset
                self.indexed_reg_val(postbyte)
            },
            0b0101 => {     // EA = ,R + B
                let base = self.indexed_reg_val(postbyte) as i16;
                let offset = (self.regs.b as i8) as i16;
                base.wrapping_add(offset) as u16
            },
            0b0110 => {     // EA = ,R + A
                let base = self.indexed_reg_val(postbyte) as i16;
                let offset = (self.regs.a as i8) as i16;
                base.wrapping_add(offset) as u16
            },
            0b1000 => {     // EA = ,R + 8-bit offset
                let base = self.indexed_reg_val(postbyte) as i16;
                let offset = self.fetchb() as i16;
                base.wrapping_add(offset) as u16
            }
            0b1001 => {     // EA = ,R + 16-bit offset
                let base = self.indexed_reg_val(postbyte) as i16;
                let offset = self.fetchw() as i16;
                base.wrapping_add(offset) as u16
            },
            0b1011 => {     // EA = ,R + D offset
                let base = self.indexed_reg_val(postbyte) as i16;
                let offset = self.regs.d() as i16;
                base.wrapping_add(offset) as u16
            },
            0b1100 => {     // EA = ,PC + 8-bit offset
                self.pcrel8()
            },
            0b1101 => {     // EA = ,PC + 16-bit offset
                self.pcrel16()
            }
            0b1111 => {     // EA = (, Address)
                let x = self.fetchw();
                self.mem.loadw(x)
            },
            _ => panic!("Unexpected indexed addressing mode"),
        }
    }

    /// 8-bit PC-relative addressing.
    fn pcrel8(&mut self) -> u16 {
        let offset = self.fetchb() as i8 as i16;
        let pc = self.regs.pc as i16;
        pc.wrapping_add(offset) as u16
    }

    /// 16-bit PC-relative addressing.
    fn pcrel16(&mut self) -> u16 {
        let offset = self.fetchw() as i16;
        let pc = self.regs.pc as i16;
        pc.wrapping_add(offset) as u16
    }
}

/////////////////////////////////////////////////////////////////////
// Instruction Set

impl <M: Mem> CPU<M> {
    /// Clear the N, Z, V, and C flags.
    fn clear_nzvc8(&mut self) {
        self.regs.cc.remove(CC_N | CC_Z | CC_V | CC_C);
    }

    /// Set the negative and zero flags given an ALU result.
    fn set_nz(&mut self, c: u8) {
        self.regs.cc.set_if(CC_N, (c & 0x80) != 0);
        self.regs.cc.set_if(CC_Z, c == 0);
    }

    /// Set the N, Z, V, and C flags from operands and result.
    fn set_nzvc8(&mut self, a: u16, b: u16, c: u16) {
        self.regs.cc.set_if(CC_C, (c & 0x100) != 0);
        self.regs.cc.set_if(CC_V, ((a ^ b ^ c ^ (c >> 1)) & 0x80) != 0);
        self.set_nz(c as u8);
    }

    /// Replaces the operand with its twos complement. The C (carry)
    /// bit represents a borrow and is set to the inverse of the
    /// resulting binary carry. Note that 0x80 is replaced by itself
    /// and only in this case is the V (overflow) bit set. The value
    /// 0x00 is also replaced by itself, and only in this case is the
    /// C (carry) bit cleared.
    ///
    /// Condition Codes:
    ///
    ///   H - U ndefined.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Set if the original operand was 1 0000000.
    ///   C - Set if a borrow is generated; cleared otherwise.
    fn neg(&mut self, val: u8) -> u8 {
        let a = 0u16;
        let b = val as u16;
        let c = a.wrapping_sub(b);

        self.set_nzvc8(a, b, c);
        c as u8
    }

    fn op_NEG(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.neg(val);
        self.mem.storeb(ea, res);
    }

    fn op_NEGA(&mut self) {
        let val = self.regs.a;
        let res = self.neg(val);
        self.regs.a = res;
    }

    fn op_NEGB(&mut self) {
        let val = self.regs.b;
        let res = self.neg(val);
        self.regs.b = res;
    }

    /// Replaces the contents of memory location M or accumulator A or B
    /// with its logical complement. When operating on unsigned values,
    /// only BEQ and BNE branches can be expected to behave properly
    /// following a COM instruction. When operating on twos complement
    /// values, all signed branches are available.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Always cleared.
    ///   C - Always set.
    fn com(&mut self, val: u8) -> u8 {
        let res = !val;
        self.set_nz(res);
        self.regs.cc.remove(CC_V);
        self.regs.cc.insert(CC_C);
        res
    }

    fn op_COM(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.com(val);
        self.mem.storeb(ea, res);
    }

    fn op_COMA(&mut self) {
        let val = self.regs.a;
        let res = self.com(val);
        self.regs.a = res;
    }

    fn op_COMB(&mut self) {
        let val = self.regs.b;
        let res = self.com(val);
        self.regs.b = res;
    }

    /// Shifts all bits of accumulator A or B or memory
    /// location M one place to the left. Bit zero is
    /// loaded with a zero. Bit seven of accumulator A or B or
    /// memory location M is shifted into the C (carry) bit.
    ///
    /// Condition Codes:
    ///
    ///   H - Undefined.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Loaded with the result of the exclusive OR of
    ///       bits six and seven of the original operand.
    ///   C - Loaded with bit seven of the original operand.
    fn lsl(&mut self, val: u8) -> u8 {
        let res = val << 1;
        self.set_nz(res);
        self.regs.cc.set_if(CC_C, (val & 0x80) != 0);
        self.regs.cc.set_if(CC_V, (val ^ (val << 1)) & 0x80 != 0);
        res
    }

    fn op_LSL(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.lsl(val);
        self.mem.storeb(ea, res);
    }

    fn op_LSLA(&mut self) {
        let val = self.regs.a;
        let res = self.lsl(val);
        self.regs.a = res;
    }

    fn op_LSLB(&mut self) {
        let val = self.regs.b;
        let res = self.lsl(val);
        self.regs.b = res;
    }

    /// Performs a logical shift right on the operand. Shifts a
    /// zero into bit seven and bit zero into the C (carry) bit.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Always cleared.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Not affected.
    ///   C - Loaded with bit zero of the original operand.
    fn lsr(&mut self, val: u8) -> u8 {
        let res = val >> 1;
        self.set_nz(res);
        self.regs.cc.set_if(CC_C, (val & 0x01) != 0);
        res
    }

    fn op_LSR(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.lsr(val);
        self.mem.storeb(ea, res);
    }

    fn op_LSRA(&mut self) {
        let val = self.regs.a;
        let res = self.lsr(val);
        self.regs.a = res;
    }

    fn op_LSRB(&mut self) {
        let val = self.regs.b;
        let res = self.lsr(val);
        self.regs.b = res;
    }

    /// Shifts all bits of the operand one place to the
    /// right. Bit seven is held constant. Bit zero is
    /// shifted i nto the C (carry) bit.
    ///
    /// Condition Codes:
    ///
    ///   H - Undefined.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Not affected.
    ///   C - Loaded with bit zero of the original operand.
    fn asr(&mut self, val: u8) -> u8 {
        let res = ((val as i8) >> 1) as u8;
        self.set_nz(res);
        self.regs.cc.set_if(CC_C, (val & 0x01) != 0);
        res
    }

    fn op_ASR(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.asr(val);
        self.mem.storeb(ea, res);
    }

    fn op_ASRA(&mut self) {
        let val = self.regs.a;
        let res = self.asr(val);
        self.regs.a = res;
    }

    fn op_ASRB(&mut self) {
        let val = self.regs.b;
        let res = self.asr(val);
        self.regs.b = res;
    }

    /// Rotates all bits of the operand one place right
    /// through the C (carry) bit. This is a 9·bit rotation.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Not affected.
    ///   C - Loaded with bit zero of the previous operand.
    fn ror(&mut self, val: u8) -> u8 {
        let hi = if self.regs.cc.contains(CC_C) { 0x80 } else { 0x00 };
        let res = (val >> 1) | hi;

        self.set_nz(res);
        self.regs.cc.set_if(CC_C, (val & 0x01) != 0);
        res
    }

    fn op_ROR(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.ror(val);
        self.mem.storeb(ea, res);
    }

    fn op_RORA(&mut self) {
        let val = self.regs.a;
        let res = self.ror(val);
        self.regs.a = res;
    }

    fn op_RORB(&mut self) {
        let val = self.regs.b;
        let res = self.ror(val);
        self.regs.b = res;
    }

    /// Rotates all bits of the operand one place left through the
    /// C (carry) bit. This is a 9-bit rotation.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Loaded with the result of the exclusive OR of bits six
    ///       and seven of the original operand.
    ///   C - Loaded with bit seven of the original operand.
    fn rol(&mut self, val: u8) -> u8 {
        let lo = if self.regs.cc.contains(CC_C) { 0x01 } else { 0x00 };
        let res = (val << 1) | lo;

        self.set_nz(res);
        self.regs.cc.set_if(CC_C, (val & 0x80) != 0);
        self.regs.cc.set_if(CC_V, (val ^ (val << 1)) & 0x80 != 0);
        res
    }

    fn op_ROL(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.rol(val);
        self.mem.storeb(ea, res);
    }

    fn op_ROLA(&mut self) {
        let val = self.regs.a;
        let res = self.rol(val);
        self.regs.a = res;
    }

    fn op_ROLB(&mut self) {
        let val = self.regs.b;
        let res = self.rol(val);
        self.regs.b = res;
    }

    /// Subtract one from the operand. The carry bit is not affected, thus
    /// allowing this instruction to be used as a loop counter in multiple
    /// precision computations. When operating on unsigned values, only
    /// BEQ and BNE branches can be expected to behave consistently.
    /// When operating on twos complement values, all signed branches
    /// are available.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Set if the original operand was 0b10000000; cleared otherwise.
    ///   C - Not affected.
    fn dec(&mut self, val: u8) -> u8 {
        let res = val.wrapping_sub(1);

        self.set_nz(res);
        self.regs.cc.set_if(CC_V, val == 0b10000000);
        res
    }

    fn op_DEC(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.dec(val);
        self.mem.storeb(ea, res);
    }

    fn op_DECA(&mut self) {
        let val = self.regs.a;
        let res = self.dec(val);
        self.regs.a = res;
    }

    fn op_DECB(&mut self) {
        let val = self.regs.b;
        let res = self.dec(val);
        self.regs.b = res;
    }

    /// Adds to the operand. The carry bit is not affected, thus allowing this
    /// instruction to be used as a loop counter in multiple-precision computations.
    /// When operating on unsigned values, only the BEQ and
    /// BNE branches can be expected to behave consistently. When
    /// operating on twos complement values, all signed branches are correctly
    /// available.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Set if the original operand was 0b01111111; cleared otherwise.
    ///   C - Not affected.
    fn inc(&mut self, val: u8) -> u8 {
        let res = val.wrapping_add(1);

        self.set_nz(res);
        self.regs.cc.set_if(CC_V, val == 0b01111111);
        res
    }

    fn op_INC(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        let res = self.inc(val);
        self.mem.storeb(ea, res);
    }

    fn op_INCA(&mut self) {
        let val = self.regs.a;
        let res = self.inc(val);
        self.regs.a = res;
    }

    fn op_INCB(&mut self) {
        let val = self.regs.b;
        let res = self.inc(val);
        self.regs.b = res;
    }

    /// Set the N (negative) and Z (zero) bits according to the contents of
    /// memory location M, and clear the V (overflow) bit. The TST instruction
    /// provides only minimum information when testing unsigned
    /// values; since no unsigned value is less than zero, BLO and BLS have
    /// no utility. While BHI could be used after TST, it provides exactly the
    /// same control as BNE, which is preferred. The signed branches are
    /// available.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Always cleared.
    ///   C - Not affected.
    fn tst(&mut self, val: u8) {
        self.set_nz(val);
        self.regs.cc.remove(CC_V);
    }

    fn op_TST(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        self.tst(val);
    }

    fn op_TSTA(&mut self) {
        let val = self.regs.a;
        self.tst(val);
    }

    fn op_TSTB(&mut self) {
        let val = self.regs.b;
        self.tst(val);
    }

    /// Program control is transferred to the effective address.
    ///
    /// Condition Codes: Not affected.
    fn op_JMP(&mut self, ea: u16) {
        self.regs.pc = ea;
    }

    /// Accumulator A or B or memory location M is loaded with 00000000.
    /// Note that the EA is read during this operation.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Always cleared.
    ///   Z - Always set.
    ///   V - Always cleared.
    ///   C - Always cleared.
    fn op_CLR(&mut self, ea: u16) {
        let _ = self.mem.loadb(ea);
        self.mem.storeb(ea, 0x00);
        self.regs.cc.remove(CC_N | CC_V | CC_C);
        self.regs.cc.insert(CC_Z);
    }

    fn op_CLRA(&mut self) {
        self.regs.a = 0x00;
        self.regs.cc.remove(CC_N | CC_V | CC_C);
        self.regs.cc.insert(CC_Z);
    }

    fn op_CLRB(&mut self) {
        self.regs.b = 0x00;
        self.regs.cc.remove(CC_N | CC_V | CC_C);
        self.regs.cc.insert(CC_Z);
    }

    /// This instruction causes the program counter to be incremented.
    /// No other registers or memory locations are affected.
    fn op_NOP(&mut self) {}

    /// Synchronize to external event. Not implemented yet.
    fn op_SYNC(&mut self) { self.dump_regs(); }

    /// Causes an unconditional branch given a 16-bit offset.
    fn op_LBRA(&mut self, ea: u16) {
        self.regs.pc = ea;
    }

    /// Causes an unconditional branch given an 8-bit offset.
    fn op_BRA(&mut self, ea: u16) {
        self.regs.pc = ea;
    }

    /// Branch never, equivalent to NOP.
    fn op_BRN(&mut self, ea: u16) {}

    /// Branch to `ea` if `flags` are clear.
    fn branch_if_clear(&mut self, flags: CCFlags, ea: u16) {
        let cc = self.regs.cc;
        let val = cc & flags;
        if val.is_empty() { self.regs.pc = ea; }
    }

    /// Branch to `ea` if `flags` are set.
    fn branch_if_set(&mut self, flags: CCFlags, ea: u16) {
        let cc = self.regs.cc;
        let val = cc & flags;
        if val == flags { self.regs.pc = ea; }
    }

    /// Causes a branch if the previous operation caused neither a carry nor
    /// a zero result. When used after a subtract or compare operation on
    /// unsigned binary val ues, this instruction will branch if the register
    /// was higher than the memory operand.
    fn op_BHI(&mut self, ea: u16) {
        self.branch_if_clear(CC_C | CC_Z, ea);
    }

    /// Causes a branch if the previous operation caused either a carry or a
    /// zero result. When used after a subtract or compare operation on unsigned
    /// binary values, this instruction will branch if the register was
    /// lower than or the same as the memory operand.
    fn op_BLS(&mut self, ea: u16) {
        self.branch_if_set(CC_C | CC_Z, ea);
    }

    /// Tests the state of the C (carry) bit and causes a branch if it is clear.
    /// When used after a subtract or compare on unsigned binary values,
    /// this instruction will branch if the register was higher than or the
    /// same as the memory operand.
    fn op_BHS(&mut self, ea: u16) {
        self.branch_if_clear(CC_C, ea);
    }

    /// Tests the state of the C (carry) bit and causes a branch if it is set.
    /// When used after a subtract or compare on unsigned binary values,
    /// this instruction will branch if the register was lower than the
    /// memory operand.
    fn op_BLO(&mut self, ea: u16) {
        self.branch_if_set(CC_C, ea);
    }

    /// Tests the state of the Z (zero) bit and causes a branch if it is clear.
    /// When used after a subtract or compare operation on any binary
    /// values, this instruction will branch if the register is, or would be, not
    /// equal to the memory operand.
    fn op_BNE(&mut self, ea: u16) {
        self.branch_if_clear(CC_Z, ea);
    }

    /// Tests the state of the Z (zero) bit and causes a branch if it is set.
    /// When used after a subtract or compare operation, this instruction
    /// will branch if the compared values, signed or unsigned, were exactly
    /// the same.
    fn op_BEQ(&mut self, ea: u16) {
        self.branch_if_set(CC_Z, ea);
    }

    /// Tests the state of the V (overflow) bit and causes a branch if it is
    /// clear. That is, branch if the twos complement result was valid. When
    /// used after an operation on twos complement binary values, this instruction
    /// will branch if there was no overflow.
    fn op_BVC(&mut self, ea: u16) {
        self.branch_if_clear(CC_V, ea);
    }

    /// Tests the state of the V (overflow) bit and causes a branch if it is set.
    /// That is, branch if the twos complement result was inval id. When used
    /// after an operation on twos complement binary values, this instruction
    /// will branch if there was an overflow.
    fn op_BVS(&mut self, ea: u16) {
        self.branch_if_set(CC_V, ea);
    }

    /// Tests the state of the N (negative) bit and causes a branch if it is
    /// clear. That is, branch if the sign of the twos complement result is
    /// positive.
    fn op_BPL(&mut self, ea: u16) {
        self.branch_if_clear(CC_N, ea);
    }

    /// Tests the state of the N (negative) bit and causes a branch if set.
    /// That is, branch if the sign of the twos complement result is negative.
    fn op_BMI(&mut self, ea: u16) {
        self.branch_if_set(CC_N, ea);
    }

    /// Causes a branch if the N (negative) bit and the V (overflow) bit are
    /// either both set or both clear. That is, branch if the sign of a valid
    /// twos complement result is, or would be, positive. When used after a
    /// subtract or compare operation on twos complement values, this instruction
    /// will branch if the register was greater than or equal to the
    /// memory operand.
    fn op_BGE(&mut self, ea: u16) {
        let cc = self.regs.cc;
        let cc_n = (cc & CC_N) == CC_N;
        let cc_v = (cc & CC_V) == CC_V;

        if !(cc_n ^ cc_v) {
            self.regs.pc = ea;
        }
    }

    /// Causes a branch if either, but not both, of the N (negative) or V
    /// (overflow) bits is set. That is, branch if the sign of a valid twos complement
    /// result is, or would be, negative. When used after a subtract
    /// or compare operation on twos complement binary values, this instruction
    /// will branch if the register was less than the memory
    /// operand.
    fn op_BLT(&mut self, ea: u16) {
        let cc = self.regs.cc;
        let cc_n = (cc & CC_N) == CC_N;
        let cc_v = (cc & CC_V) == CC_V;

        if cc_n ^ cc_v {
            self.regs.pc = ea;
        }
    }

    /// Causes a branch if the N (negative) bit and V (overflow) bit are either
    /// both set or both clear and the Z (zero) bit is clear. In other words,
    /// branch if the sign of a valid twos complement result is, or would be,
    /// positive and not zero. When used after a subtract or compare operation
    /// on twos complement values, this instruction will branch if the
    /// register was greater than the memory operand.
    fn op_BGT(&mut self, ea: u16) {
        let cc = self.regs.cc;
        let cc_n = (cc & CC_N) == CC_N;
        let cc_v = (cc & CC_V) == CC_V;
        let cc_z = (cc & CC_Z) == CC_Z;

        if !(cc_z & (cc_n ^ cc_v)) {
            self.regs.pc = ea;
        }
    }

    /// Causes a branch if the exclusive OR of the N (negative) and V
    /// (overflow) bits is 1 or if the Z (zero) bit is set. That is, branch if the
    /// sign of a valid twos complement result is, or would be, negative.
    /// When used after a subtract or compare operation on twos complement
    /// values, this instruction will branch if the register was less than
    /// or equal to the memory operand.
    fn op_BLE(&mut self, ea: u16) {
        let cc = self.regs.cc;
        let cc_n = (cc & CC_N) == CC_N;
        let cc_v = (cc & CC_V) == CC_V;
        let cc_z = (cc & CC_Z) == CC_Z;

        if cc_z | (cc_n ^ cc_v) {
            self.regs.pc = ea;
        }
    }

    /// Performs an inclusive OR operation between the contents of the
    /// condition code registers and the immediate value, and the result is
    /// placed in the condition code register. This instruction may be used
    /// to set interrupt masks (disable interrupts) or any other bit(s).
    fn op_ORCC(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        self.regs.cc |= CCFlags::from_bits_truncate(val);
    }

    /// Performs a logical AND between the condition code register and the
    /// immediate byte specified in the instruction and places the result in
    /// the condition code register.
    fn op_ANDCC(&mut self, ea: u16) {
        let val = self.mem.loadb(ea);
        self.regs.cc &= CCFlags::from_bits_truncate(val);
    }

    // The program counter is pushed onto the stack. The program counter
    // is then loaded with the sum of the program counter and the offset.
    fn op_BSR(&mut self, ea: u16) {
        let pc = self.regs.pc;
        self.pushw_s(pc);
        self.regs.pc = ea;
    }

    // The program counter is pushed onto the stack. The program counter
    // is then loaded with the sum of the program counter and the offset.
    //
    // TODO: Write a test for me!
    fn op_LBSR(&mut self, ea: u16) {
        let pc = self.regs.pc;
        self.pushw_s(pc);
        self.regs.pc = ea;
    }

    /// Program control is returned from the subroutine to the calling program.
    /// The return address is pulled from the stack.
    fn op_RTS(&mut self) {
        let new_pc = self.popw_s();
        self.regs.pc = new_pc;
    }

    /// The sequence of a single-byte add instruction on accumulator A
    /// (either ADDA or ADCA) and a following decimal addition adjust
    /// instruction results in a BCD addition with an appropriate carry bit.
    ///
    /// Both values to be added must be in proper BCD form (each nibble
    /// such that: O <= nibble <= 9). Multiple-precision addition must add the
    /// carry generated by this decimal addition adjust into the next higher
    /// digit during the add operation (ADCA) immediately prior to the next
    /// decimal addition adjust.
    ///
    /// Operation:
    ///
    ///   ACCA' <- ACCA + CF (MSN):CF(LSN)
    ///
    /// where CF is a Correction Factor, as follows:
    ///
    /// The CF for each nibble (BCD) digit is determined separately,
    /// and is either 6 or 0
    ///.
    /// Least Significant Nibble
    ///
    /// CF(LSN) = 6 IFF 1) C = 1
    ///              or 2) LSN >9
    ///
    /// Most Significant Nibble
    ///
    /// CF(MSN) = 6 IFF 1) C = 1
    ///              or 2) MSN>9
    ///              or 3) MSN>8 and LSN >9
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Set if the result is negative; cleared otherwise.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Undefined.
    ///   C - Set if a carry is generated or if the carry bit was set before the
    ///       operation; cleared otherwise.
    fn op_DAA(&mut self) {
        let lsn = (self.regs.a >> 0) & 0x0f;
        let msn = (self.regs.a >> 4) & 0x0f;
        let cc_c = self.regs.cc.contains(CC_C);
        let cc_h = self.regs.cc.contains(CC_H);
        let cf_lsn = if cc_h || lsn > 9 { 0x06 } else { 0x00 };
        let cf_msn = if cc_c || msn > 9 || (msn > 8 && lsn > 9) { 0x60 } else { 0x00 };
        let cf = cf_lsn | cf_msn;
        self.regs.a = self.regs.a.wrapping_add(cf);
    }

    /// This instruction transforms a twos complement
    /// 8-bit value in accumulator B into a twos complement
    /// 16-bit value in the D accumulator.
    fn op_SEX(&mut self) {
        let res = if self.regs.b & 0x80 != 0 { 0xff } else { 0x00 };
        self.set_nz(res);
        self.regs.a = res;
    }

    fn reg8_load(&mut self, postnibble: u8) -> u8 {
        match postnibble {
            0b1000 => self.regs.a,
            0b1001 => self.regs.b,
            0b1010 => self.regs.cc.bits,
            0b1011 => self.regs.dp,
            _      => panic!("Invalid register: {:X}", postnibble)
        }
    }

    fn reg8_store(&mut self, postnibble: u8, val: u8) {
        match postnibble {
            0b1000 => { self.regs.a = val; },
            0b1001 => { self.regs.b = val; },
            0b1010 => { self.regs.cc = CCFlags::from_bits_truncate(val); },
            0b1011 => { self.regs.dp = val; },
            _      => panic!("Invalid register: {:X}", postnibble)
        }
    }

    fn reg16_load(&mut self, postnibble: u8) -> u16 {
        match postnibble {
            0b0000 => self.regs.d(),
            0b0001 => self.regs.x,
            0b0010 => self.regs.y,
            0b0011 => self.regs.u,
            0b0100 => self.regs.s,
            0b0101 => self.regs.pc,
            _      => panic!("Invalid register: {:X}", postnibble)
        }
    }

    fn reg16_store(&mut self, postnibble: u8, val: u16) {
        match postnibble {
            0b0000 => { self.regs.set_d(val); },
            0b0001 => { self.regs.x = val; },
            0b0010 => { self.regs.y = val; },
            0b0011 => { self.regs.u = val; },
            0b0100 => { self.regs.s = val; },
            0b0101 => { self.regs.pc = val; },
            _      => panic!("Invalid register: {:X}", postnibble)
        }
    }

    fn exg8(&mut self, postbyte: u8) {
        let reg1 = (postbyte >> 0) & 0x0f;
        let reg2 = (postbyte >> 4) & 0x0f;
        let val1 = self.reg8_load(reg1);
        let val2 = self.reg8_load(reg2);
        self.reg8_store(reg1, val2);
        self.reg8_store(reg2, val1);
    }

    fn exg16(&mut self, postbyte: u8) {
        let reg1 = (postbyte >> 0) & 0x0f;
        let reg2 = (postbyte >> 4) & 0x0f;
        let val1 = self.reg16_load(reg1);
        let val2 = self.reg16_load(reg2);
        self.reg16_store(reg1, val2);
        self.reg16_store(reg2, val1);
    }

    /// Exchanges data between two designated registers. Bits 3-0 of the
    /// postbyte define one register, while bits 7-4 define the other, as
    /// follows:
    ///
    /// OOOO = A:B            1000 = A
    /// 0001 = X              1001 = B
    /// 0010 = Y              1010 = CCR
    /// 0011 = US             1011 = DPR
    /// 0100 = SP             1100 = Undefined
    /// 0101 = PC             1101 = Undefined
    /// 0110 = Undefined      1110 = Undefined
    /// 0111 = Undefined      1111 = Undefined
    ///
    /// Only like size registers may be exchanged. (8-bit with 8-bit or 16-bit
    /// with 16-bit.)
    fn op_EXG(&mut self) {
        let postbyte = self.fetchb();

        if postbyte & 0x88 == 0x88 {
            self.exg8(postbyte);
        } else if postbyte & 0x88 == 0x00 {
            self.exg16(postbyte);
        } else {
            // It looks like many other 6809 emulators allow mixed
            // size exchanges. Let's see if this causes problems.
            panic!("Illegal EXG postbyte: {:X}", postbyte);
        }
    }

    fn tfr8(&mut self, postbyte: u8) {
        let dst_reg = (postbyte >> 0) & 0x0f;
        let src_reg = (postbyte >> 4) & 0x0f;
        let src_val = self.reg8_load(src_reg);
        self.reg8_store(dst_reg, src_val);
    }

    fn tfr16(&mut self, postbyte: u8) {
        let dst_reg = (postbyte >> 0) & 0x0f;
        let src_reg = (postbyte >> 4) & 0x0f;
        let src_val = self.reg16_load(src_reg);
        self.reg16_store(dst_reg, src_val);
    }

    /// Transfers data between two designated registers. Bits 7-4 of the
    /// postbyte define the source register, while bits 3-0 define
    /// the destination register.
    fn op_TFR(&mut self) {
        let postbyte = self.fetchb();

        if postbyte & 0x88 == 0x88 {
            self.tfr8(postbyte);
        } else if postbyte & 0x88 == 0x00 {
            self.tfr16(postbyte);
        } else {
            // It looks like many other 6809 emulators allow mixed
            // size transfers. Let's see if this causes problems.
            panic!("Illegal TFR postbyte: {:X}", postbyte);
        }
    }

    fn op_LEAX(&mut self, ea: u16) {
        self.regs.x = ea;
        self.regs.cc.set_if(CC_Z, ea == 0);
    }

    fn op_LEAY(&mut self, ea: u16) {
        self.regs.y = ea;
        self.regs.cc.set_if(CC_Z, ea == 0);
    }

    fn op_LEAS(&mut self, ea: u16) {
        self.regs.s = ea;
    }

    fn op_LEAU(&mut self, ea: u16) {
        self.regs.u = ea;
    }

    fn ld8(&mut self, ea: u16) -> u8 {
        let res = self.mem.loadb(ea);
        self.set_nz(res);
        self.regs.cc.remove(CC_V);
        res
    }

    fn ld16(&mut self, ea: u16) -> u16 {
        let res = self.mem.loadw(ea);
        self.regs.cc.set_if(CC_Z, res == 0);
        self.regs.cc.set_if(CC_N, res & 0x8000 != 0);
        self.regs.cc.remove(CC_V);
        res
    }

    fn op_LDA(&mut self, ea: u16) {
        let res = self.ld8(ea);
        self.regs.a = res;
    }

    fn op_LDB(&mut self, ea: u16) {
        let res = self.ld8(ea);
        self.regs.b = res;
    }

    fn op_LDX(&mut self, ea: u16) {
        let res = self.ld16(ea);
        self.regs.x = res;
    }

    fn op_LDY(&mut self, ea: u16) {
        let res = self.ld16(ea);
        self.regs.y = res;
    }

    fn op_LDD(&mut self, ea: u16) {
        let res = self.ld16(ea);
        self.regs.set_d(res);
    }

    fn op_LDU(&mut self, ea: u16) {
        let res = self.ld16(ea);
        self.regs.u = res;
    }

    fn op_LDS(&mut self, ea: u16) {
        let res = self.ld16(ea);
        self.regs.s = res;
    }

    fn st8(&mut self, ea: u16, val: u8) {
        self.mem.storeb(ea, val);
        self.set_nz(val);
        self.regs.cc.remove(CC_V);
    }

    fn st16(&mut self, ea: u16, val: u16) {
        self.mem.storew(ea, val);
        self.regs.cc.set_if(CC_Z, val == 0);
        self.regs.cc.set_if(CC_N, val & 0x8000 != 0);
        self.regs.cc.remove(CC_V);
    }

    fn op_STA(&mut self, ea: u16) {
        let val = self.regs.a;
        self.st8(ea, val);
    }

    fn op_STB(&mut self, ea: u16) {
        let val = self.regs.b;
        self.st8(ea, val);
    }

    fn op_STX(&mut self, ea: u16) {
        let val = self.regs.x;
        self.st16(ea, val);
    }

    fn op_STY(&mut self, ea: u16) {
        let val = self.regs.y;
        self.st16(ea, val);
    }

    fn op_STD(&mut self, ea: u16) {
        let val = self.regs.d();
        self.st16(ea, val);
    }

    fn op_STU(&mut self, ea: u16) {
        let val = self.regs.u;
        self.st16(ea, val);
    }

    fn op_STS(&mut self, ea: u16) {
        let val = self.regs.s;
        self.st16(ea, val);
    }

    /// All, some, or none of the processor registers are pushed onto the
    /// hardware stack (with the exception of the hardware stack pointer
    /// itself).
    fn op_PSHS(&mut self) {
        let postbyte = self.fetchb();
        let regs = self.regs;

        if postbyte & (1 << 7) != 0 { self.pushw_s(regs.pc);      }
        if postbyte & (1 << 6) != 0 { self.pushw_s(regs.u);       }
        if postbyte & (1 << 5) != 0 { self.pushw_s(regs.y);       }
        if postbyte & (1 << 4) != 0 { self.pushw_s(regs.x);       }
        if postbyte & (1 << 3) != 0 { self.pushb_s(regs.dp);      }
        if postbyte & (1 << 2) != 0 { self.pushb_s(regs.b);       }
        if postbyte & (1 << 1) != 0 { self.pushb_s(regs.a);       }
        if postbyte & (1 << 0) != 0 { self.pushb_s(regs.cc.bits); }
    }

    /// All, some, or none of the processor registers are pulled from the
    /// hardware stack (with the exception of the hardware stack pointer
    /// itself).
    fn op_PULS(&mut self) {
        let postbyte = self.fetchb();

        if postbyte & (1 << 0) != 0 { self.regs.cc = CCFlags::from_bits_truncate(self.popb_s()); }
        if postbyte & (1 << 1) != 0 { self.regs.a  = self.popb_s(); }
        if postbyte & (1 << 2) != 0 { self.regs.b  = self.popb_s(); }
        if postbyte & (1 << 3) != 0 { self.regs.dp = self.popb_s(); }
        if postbyte & (1 << 4) != 0 { self.regs.x  = self.popw_s(); }
        if postbyte & (1 << 5) != 0 { self.regs.y  = self.popw_s(); }
        if postbyte & (1 << 6) != 0 { self.regs.u  = self.popw_s(); }
        if postbyte & (1 << 7) != 0 { self.regs.pc = self.popw_s(); }
    }

    /// All, some, or none of the processor registers are pushed onto the
    /// user stack (with the exception of the hardware stack pointer
    /// itself).
    fn op_PSHU(&mut self) {
        let postbyte = self.fetchb();
        let regs = self.regs;

        if postbyte & (1 << 7) != 0 { self.pushw_u(regs.pc);      }
        if postbyte & (1 << 6) != 0 { self.pushw_u(regs.s);       }
        if postbyte & (1 << 5) != 0 { self.pushw_u(regs.y);       }
        if postbyte & (1 << 4) != 0 { self.pushw_u(regs.x);       }
        if postbyte & (1 << 3) != 0 { self.pushb_u(regs.dp);      }
        if postbyte & (1 << 2) != 0 { self.pushb_u(regs.b);       }
        if postbyte & (1 << 1) != 0 { self.pushb_u(regs.a);       }
        if postbyte & (1 << 0) != 0 { self.pushb_u(regs.cc.bits); }
    }

    /// All, some, or none of the processor registers are pulled from the
    /// user stack (with the exception of the user stack pointer
    /// itself).
    fn op_PULU(&mut self) {
        let postbyte = self.fetchb();

        if postbyte & (1 << 0) != 0 { self.regs.cc = CCFlags::from_bits_truncate(self.popb_u()); }
        if postbyte & (1 << 1) != 0 { self.regs.a  = self.popb_u(); }
        if postbyte & (1 << 2) != 0 { self.regs.b  = self.popb_u(); }
        if postbyte & (1 << 3) != 0 { self.regs.dp = self.popb_u(); }
        if postbyte & (1 << 4) != 0 { self.regs.x  = self.popw_u(); }
        if postbyte & (1 << 5) != 0 { self.regs.y  = self.popw_u(); }
        if postbyte & (1 << 6) != 0 { self.regs.s  = self.popw_u(); }
        if postbyte & (1 << 7) != 0 { self.regs.pc = self.popw_u(); }
    }

    /// Add the 8-bit unsigned value in accumulator B
    /// into index register X.
    ///
    /// Condition Codes: Not affected.
    fn op_ABX(&mut self) {
        self.regs.x = self.regs.x.wrapping_add(self.regs.b as u16);
    }

    /// The saved machine state is recovered from the hardware stack and
    /// control is returned to the interrupted program. If the recovered E
    /// (entire) bit is clear, it indicates that only a subset of the machine
    /// state was saved (return address and condition codes) and only that
    /// subset is recovered.
    fn op_RTI(&mut self) {
        self.regs.cc = CCFlags::from_bits_truncate(self.popb_s());

        if self.regs.cc.contains(CC_E) {
            self.regs.a  = self.popb_s();
            self.regs.b  = self.popb_s();
            self.regs.dp = self.popb_s();
            self.regs.x  = self.popw_s();
            self.regs.y  = self.popw_s();
            self.regs.u  = self.popw_s();
        }

        self.regs.pc = self.popw_s();
    }

    /// For now, use this instruction to stop the emulator.
    fn op_CWAI(&mut self) {
        self.running = false;
    }

    /// Multiply the unsigned binary numbers in the accumulators and
    /// place the result in both accumulators (ACCA contains the most
    /// significant byte of the result). Unsigned multiply allows
    /// multiple precision operations.
    ///
    /// Condition Codes:
    ///
    ///   H - Not affected.
    ///   N - Not affected.
    ///   Z - Set if the result is zero; cleared otherwise.
    ///   V - Not affected.
    ///   C - Set if ACCB bit 7 of result is set; cleared otherwise.
    fn op_MUL(&mut self) {
        let a = self.regs.a as u16;
        let b = self.regs.b as u16;
        let r = a.wrapping_mul(b);
        self.regs.set_d(r);
        self.regs.cc.set_if(CC_Z, r == 0);
        self.regs.cc.set_if(CC_C, r & 0x80 == 0x80);
    }

    /// Undocumented opcode, not yet implemented.
    fn op_RESET(&mut self) {
        unimplemented!();
    }

    /// Initiate a software interrupt with handler at `vec_addr`. If
    /// `mask_int` is true, the `I` and `F` flags will be set when
    /// calling the interrupt handler. All registers are stacked and
    /// the `E` flag will be set.
    fn swi(&mut self, vec_addr: u16, mask_int: bool) {
        self.regs.cc |= CC_E;               // entire state saved
        let regs = self.regs;

        self.pushw_s(regs.pc);
        self.pushw_s(regs.u);
        self.pushw_s(regs.y);
        self.pushw_s(regs.x);
        self.pushb_s(regs.dp);
        self.pushb_s(regs.b);
        self.pushb_s(regs.a);
        self.pushb_s(regs.cc.bits);

        let new_pc = self.mem.loadw(vec_addr);
        self.regs.pc = new_pc;

        if mask_int {
            self.regs.cc |= CC_I | CC_F;    // mask interrupts
        }
    }

    /// All of the processor registers are pushed onto the hardware stack
    /// (with the exception of the hardware stack pointer itself), and control
    /// is transferred through the software interrupt vector. Both the normal
    /// and fast interrupts are masked (disabled).
    fn op_SWI(&mut self) {
        self.swi(0xFFFA, true);
    }

    fn op_SWI2(&mut self) {
        self.swi(0xFFF4, false);
    }

    fn op_SWI3(&mut self) {
        self.swi(0xFFF2, false);
    }

    fn op_SUBA(&mut self, ea: u16) {}
    fn op_CMPA(&mut self, ea: u16) {}
    fn op_SBCA(&mut self, ea: u16) {}
    fn op_SUBD(&mut self, ea: u16) {}
    fn op_ANDA(&mut self, ea: u16) {}
    fn op_BITA(&mut self, ea: u16) {}
    fn op_EORA(&mut self, ea: u16) {}
    fn op_ADCA(&mut self, ea: u16) {}
    fn op_ORA(&mut self, ea: u16) {}
    fn op_ADDA(&mut self, ea: u16) {}
    fn op_CMPX(&mut self, ea: u16) {}
    fn op_JSR(&mut self, ea: u16) {}
    fn op_SUBB(&mut self, ea: u16) {}
    fn op_CMPB(&mut self, ea: u16) {}
    fn op_SBCB(&mut self, ea: u16) {}
    fn op_ADDD(&mut self, ea: u16) {}
    fn op_ANDB(&mut self, ea: u16) {}
    fn op_BITB(&mut self, ea: u16) {}
    fn op_EORB(&mut self, ea: u16) {}
    fn op_ADCB(&mut self, ea: u16) {}
    fn op_ORB(&mut self, ea: u16) {}
    fn op_ADDB(&mut self, ea: u16) {}

    fn op_LBRN(&mut self, ea: u16) {}
    fn op_LBHI(&mut self, ea: u16) {}
    fn op_LBLS(&mut self, ea: u16) {}
    fn op_LBHS(&mut self, ea: u16) {}
    fn op_LBLO(&mut self, ea: u16) {}
    fn op_LBNE(&mut self, ea: u16) {}
    fn op_LBEQ(&mut self, ea: u16) {}
    fn op_LBVC(&mut self, ea: u16) {}
    fn op_LBVS(&mut self, ea: u16) {}
    fn op_LBPL(&mut self, ea: u16) {}
    fn op_LBMI(&mut self, ea: u16) {}
    fn op_LBGE(&mut self, ea: u16) {}
    fn op_LBLT(&mut self, ea: u16) {}
    fn op_LBGT(&mut self, ea: u16) {}
    fn op_LBLE(&mut self, ea: u16) {}
    fn op_CMPD(&mut self, ea: u16) {}
    fn op_CMPY(&mut self, ea: u16) {}

    fn op_CMPU(&mut self, ea: u16) {}
    fn op_CMPS(&mut self, ea: u16) {}

    /// Handle an illegal CPU instruction.
    fn illegal_instruction(&self, opcode: u8) {
        panic!("Illegal instruction {:02X}", opcode);
    }
}

/// Macro used to check flags after executing an instruction.
macro_rules! assert_flags {
    ($cpu:expr => $($flag:ident: $val:expr),*) => (
        $(assert!($cpu.regs.cc.contains($flag) == $val);)*);
}

#[cfg(test)]
#[path = "cpu_test.rs"]
mod tests;
