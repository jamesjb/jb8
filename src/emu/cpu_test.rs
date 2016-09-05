//
// cpu_test.rs --- CPU unit tests.
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

use super::*;
use emu::mem::{Mem,RAM};

// Fixture to create a test CPU.
fn test_cpu() -> CPU<RAM> {
    CPU::new(RAM::new(0x10000))
}

// Test that negating 0x80 (the most negative byte) sets CC_V.
#[test]
fn nega_0x80() {
    let mut cpu = test_cpu();

    cpu.regs.a = 0x80;
    cpu.op_NEGA();

    assert_eq!(cpu.regs.a, 0x80);
    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_V: true,
        CC_C: true
    }
}

// Test that negating 0x00 clears CC_C.
#[test]
fn negb_0x00() {
    let mut cpu = test_cpu();

    cpu.regs.b = 0x00;
    cpu.op_NEGB();

    assert_eq!(cpu.regs.b, 0x00);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: true,
        CC_V: false,
        CC_C: false
    }
}

// Test `NEGA` on `0x01` for value and flag setting.
#[test]
fn nega_0x01() {
    let mut cpu = test_cpu();

    cpu.regs.a = 0x01;
    cpu.op_NEGA();

    assert_eq!(cpu.regs.a, 0xFF);
    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_V: false,
        CC_C: true
    }

    cpu.op_NEGA();

    assert_eq!(cpu.regs.a, 0x01);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: false,
        CC_V: false,
        CC_C: true
    }
}

// Test LSR and the indexed autoincrement addressing mode.
#[test]
fn lsr_indexed() {
    let mut cpu = test_cpu();
    const CODE_ADDR: u16 = 0x100;
    const DATA_ADDR: u16 = 0x400;

    cpu.mem.store(DATA_ADDR, &[1, 2, 3, 4]);
    cpu.mem.store(CODE_ADDR, &[
        0x64, 0x80,                 // lsr ,x+
        0x64, 0x80,                 // lsr ,x+
        0x64, 0x80,                 // lsr ,x+
        0x64, 0x80,                 // lsr ,x+
    ]);

    cpu.regs.pc = CODE_ADDR;
    cpu.regs.x  = DATA_ADDR;
    cpu.step_n(4);

    let mut res = [0; 4];
    cpu.mem.load(DATA_ADDR, &mut res);

    assert_eq!(res, [0, 1, 1, 2]);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: false,
        CC_V: false,
        CC_C: false
    }
}

#[test]
fn asra() {
    let mut cpu = test_cpu();

    cpu.regs.a = -4i8 as u8;
    cpu.op_ASRA();

    assert_eq!(cpu.regs.a as i8, -2);
    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_C: false
    }

    cpu.op_ASRA();
    cpu.op_ASRA();

    assert_eq!(cpu.regs.a, 0xff);
    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_C: true
    }
}

#[test]
fn lsla() {
    let mut cpu = test_cpu();

    cpu.regs.a = 0b11000000;
    cpu.op_LSLA();

    assert_eq!(cpu.regs.a, 0b10000000);
    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_V: false,
        CC_C: true
    }

    cpu.op_LSLA();

    assert_eq!(cpu.regs.a, 0x00);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: true,
        CC_V: true,
        CC_C: true
    }

    cpu.op_LSLA();

    assert_eq!(cpu.regs.a, 0x00);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: true,
        CC_V: false,
        CC_C: false
    }
}

// Test wrap-around and overflow behavior of `DEC`.
#[test]
fn dec_wrap() {
    let mut cpu = test_cpu();

    cpu.regs.a = 2;
    cpu.op_DECA();

    assert_eq!(cpu.regs.a, 1);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: false,
        CC_V: false
    }

    cpu.op_DECA();

    assert_eq!(cpu.regs.a, 0);
    assert_flags! { cpu =>
        CC_Z: true,
        CC_N: false,
        CC_V: false
    }
}

// Test wrap-around and overflow behavior of `INC`.
#[test]
fn inc_wrap() {
    let mut cpu = test_cpu();

    cpu.regs.a = 0x7e;
    cpu.op_INCA();

    assert_eq!(cpu.regs.a, 0x7f);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: false,
        CC_V: false
    }

    cpu.op_INCA();

    assert_eq!(cpu.regs.a, 0x80);
    assert_flags! { cpu =>
        CC_Z: false,
        CC_N: true,
        CC_V: true
    }
}

#[test]
fn tsta() {
    let mut cpu = test_cpu();

    cpu.regs.a = 0x00;
    cpu.op_TSTA();

    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: true,
        CC_V: false
    }

    cpu.regs.a = 0xFF;
    cpu.op_TSTA();

    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_V: false
    }
}

#[test]
fn clra() {
    let mut cpu = test_cpu();

    cpu.regs.a = 0x80;
    cpu.op_CLRA();

    assert_eq!(cpu.regs.a, 0x00);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: true,
        CC_V: false,
        CC_C: false
    }
}

/////////////////////////////////////////////////////////////////
// Control Flow Instructions

#[test]
fn jmp() {
    let mut cpu = test_cpu();

    cpu.regs.pc = 0;
    cpu.op_JMP(0x100);
    assert_eq!(cpu.regs.pc, 0x100);
}

#[test]
fn bra() {
    let mut cpu = test_cpu();
    const CODE_ADDR: u16 = 0x100;

    cpu.mem.store(CODE_ADDR, &[
        0x12,               // top     nop
        0x12,               //         nop
        0x20, 0xfc          //         bra top
    ]);

    cpu.regs.pc = CODE_ADDR;
    cpu.step_n(3);

    assert_eq!(cpu.regs.pc, CODE_ADDR);
}

#[test]
fn lbra() {
    let mut cpu = test_cpu();
    const CODE_ADDR: u16 = 0x100;

    cpu.mem.store(CODE_ADDR, &[
        0x12,               // top     nop
        0x12,               //         nop
        0x16, 0xff, 0xfb    //         lbra top
    ]);

    cpu.regs.pc = CODE_ADDR;
    cpu.step_n(3);

    assert_eq!(cpu.regs.pc, CODE_ADDR);
}

#[test]
fn bhi() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x100, &[         //              org $100
        0x1c, 0xf0,                 // 0100         andcc #$f0
        0x1a, 0x05,                 // 0102         orcc #$05
        0x22, 0x1a,                 // 0104         bhi go
        0x1c, 0xfb,                 // 0106         andcc #$fb      ; clear Z
        0x22, 0x16,                 // 0108         bhi go
        0x1c, 0xfe,                 // 010A         andcc #$fe      ; clear C
        0x22, 0x12,                 // 010C         bhi go
    ]);                             //              org $120
                                    // 0120 go      ...

    cpu.regs.pc = 0x100;

    cpu.step_n(3);
    cpu.dump_regs();
    assert_eq!(cpu.regs.pc, 0x106);

    cpu.step_n(2);
    cpu.dump_regs();
    assert_eq!(cpu.regs.pc, 0x10a);

    cpu.step_n(2);
    cpu.dump_regs();
    assert_eq!(cpu.regs.pc, 0x120);
}

// Test calling and returning from (near) subroutine calls.
#[test]
fn bsr_rts() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x100, &[         //              org $100
        0x8d, 0x1E,                 // 0100         bsr go
        0x12,                       // 0102         nop
    ]);

    cpu.mem.store(0x120, &[         //              org $120
        0x39,                       // 0120 go      rts
    ]);

    cpu.regs.s  = 0x400;
    cpu.regs.pc = 0x100;

    cpu.step();
    cpu.dump_regs();

    assert_eq!(cpu.regs.s, 0x3fe);
    assert_eq!(cpu.regs.pc, 0x120);
    assert_eq!(cpu.mem.loadw(cpu.regs.s), 0x102);

    cpu.step();
    cpu.dump_regs();

    assert_eq!(cpu.regs.s, 0x400);
    assert_eq!(cpu.regs.pc, 0x102);
}

#[test]
fn sex() {
    let mut cpu = test_cpu();

    cpu.regs.b = 0x8E;
    cpu.op_SEX();
    assert_eq!(cpu.regs.a, 0xFF);

    cpu.regs.b = 0x10;
    cpu.op_SEX();
    assert_eq!(cpu.regs.a, 0x00);
}

#[test]
fn ld8() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x100, &[         // org $100
        0x86, 0x12,                 // lda #$12
        0xC6, 0x34,                 // ldb #$34
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);
    cpu.dump_regs();

    assert_eq!(cpu.regs.a, 0x12);
    assert_eq!(cpu.regs.b, 0x34);
}

#[test]
fn leax() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x100, &[         // org $100
        0x8E, 0x12, 0x34,           // ldx #$1234
        0x10, 0x8E, 0x43, 0x21,     // ldy #$4321
        0x30, 0x80,                 // leax ,x+
        0x30, 0x01,                 // leax 1,x
        0x30, 0x89, 0x01, 0x00,     // leax 256,x
        0x30, 0xA0,                 // leax ,y+
        0x30, 0xA3,                 // leax ,--y
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);          // load registers
    cpu.dump_regs();

    cpu.step();
    assert_eq!(cpu.regs.x, 0x1234);

    cpu.step();
    assert_eq!(cpu.regs.x, 0x1235);

    cpu.step();
    assert_eq!(cpu.regs.x, 0x1335);

    cpu.step();
    assert_eq!(cpu.regs.x, 0x4321);
    assert_eq!(cpu.regs.y, 0x4322);

    cpu.step();
    assert_eq!(cpu.regs.x, 0x4320);
    assert_eq!(cpu.regs.y, 0x4320);
}
