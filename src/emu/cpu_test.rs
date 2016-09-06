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

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0x80,             //      lda #$80
        0x40,                   //      nega
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

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

    cpu.mem.store(0x0100, &[    //      org $100
        0xC6, 0x00,             //      ldb #$00
        0x50,                   //      negb
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

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

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0x01,             //      lda #$01
        0x40,                   //      nega
        0x40,                   //      nega
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

    assert_eq!(cpu.regs.a, 0xFF);
    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_V: false,
        CC_C: true
    }

    cpu.step();

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

    cpu.mem.store(0x0400, &[1, 2, 3, 4]);
    cpu.mem.store(0x0100, &[    //      org $100
        0x64, 0x80,             //      lsr ,x+
        0x64, 0x80,             //      lsr ,x+
        0x64, 0x80,             //      lsr ,x+
        0x64, 0x80,             //      lsr ,x+
    ]);

    cpu.regs.pc = 0x100;
    cpu.regs.x  = 0x400;
    cpu.step_n(4);

    let mut res = [0; 4];
    cpu.mem.load(0x400, &mut res);

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

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0xFC,             //      lda #$FC (-4)
        0x47,                   //      asra
        0x47,                   //      asra
        0x47,                   //      asra
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

    assert_eq!(cpu.regs.a as i8, -2);
    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_C: false
    }

    cpu.step_n(2);

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

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0xC0,             //      lda #$C0
        0x48,                   //      lsla
        0x48,                   //      lsla
        0x48,                   //      lsla
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

    assert_eq!(cpu.regs.a, 0b10000000);
    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_V: false,
        CC_C: true
    }

    cpu.step();

    assert_eq!(cpu.regs.a, 0x00);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: true,
        CC_V: true,
        CC_C: true
    }

    cpu.step();

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

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0x02,             //      lda #$02
        0x4A,                   //      deca
        0x4A,                   //      deca
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

    assert_eq!(cpu.regs.a, 1);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: false,
        CC_V: false
    }

    cpu.step();

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

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0x7E,             //      lda #$7E
        0x4C,                   //      inca
        0x4C,                   //      inca
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

    assert_eq!(cpu.regs.a, 0x7f);
    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: false,
        CC_V: false
    }

    cpu.step();

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

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0x00,             //      lda #$00
        0x4D,                   //      tsta
        0x86, 0xFF,             //      lda #$FF
        0x4D,                   //      tsta
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

    assert_flags! { cpu =>
        CC_N: false,
        CC_Z: true,
        CC_V: false
    }

    cpu.step_n(2);

    assert_flags! { cpu =>
        CC_N: true,
        CC_Z: false,
        CC_V: false
    }
}

#[test]
fn clra() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0x80,             //      lda #$80
        0x4F,                   //      clra
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);

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

    cpu.mem.store(0x0100, &[    //      org $100
        0x7E, 0x01, 0x00,       //      jmp $100
    ]);

    cpu.regs.pc = 0x100;
    cpu.step();

    assert_eq!(cpu.regs.pc, 0x100);
}

#[test]
fn bra() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x0100, &[    //      org $100
        0x12,                   // top  nop
        0x12,                   //      nop
        0x20, 0xfc              //      bra top
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(3);

    assert_eq!(cpu.regs.pc, 0x100);
}

#[test]
fn lbra() {
    let mut cpu = test_cpu();
    const CODE_ADDR: u16 = 0x100;

    cpu.mem.store(0x0100, &[    //      org $100
        0x12,                   // top  nop
        0x12,                   //      nop
        0x16, 0xff, 0xfb        //      lbra top
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(3);

    assert_eq!(cpu.regs.pc, 0x100);
}

#[test]
fn bhi() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x0100, &[    //      org $100
        0x1c, 0xf0,             //      andcc #$f0
        0x1a, 0x05,             //      orcc #$05
        0x22, 0x1a,             //      bhi go
        0x1c, 0xfb,             //      andcc #$fb      ; clear Z
        0x22, 0x16,             //      bhi go
        0x1c, 0xfe,             //      andcc #$fe      ; clear C
        0x22, 0x12,             //      bhi go
    ]);                         //      org $120
                                // go   ...

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

    cpu.mem.store(0x0100, &[    //      org $100
        0x8d, 0x1E,             //      bsr go
        0x12,                   //      nop
    ]);

    cpu.mem.store(0x0120, &[    //      org $120
        0x39,                   // go   rts
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

    cpu.mem.store(0x0100, &[    //      org $100
        0xC6, 0x8E,             //      ldb #$8E
        0x1D,                   //      sex
        0xC6, 0x10,             //      ldb #$10
        0x1D,                   //      sex
    ]);

    cpu.regs.pc = 0x100;
    cpu.step_n(2);
    assert_eq!(cpu.regs.a, 0xFF);

    cpu.step_n(2);
    assert_eq!(cpu.regs.a, 0x00);
}

#[test]
fn ld8() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0x12,             //      lda #$12
        0xC6, 0x34,             //      ldb #$34
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

    cpu.mem.store(0x0100, &[    //      org $100
        0x8E, 0x12, 0x34,       //      ldx #$1234
        0x10, 0x8E, 0x43, 0x21, //      ldy #$4321
        0x30, 0x80,             //      leax ,x+
        0x30, 0x01,             //      leax 1,x
        0x30, 0x89, 0x01, 0x00, //      leax 256,x
        0x30, 0xA0,             //      leax ,y+
        0x30, 0xA3,             //      leax ,--y
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

#[test]
fn push_pull_s() {
    let mut cpu = test_cpu();

    cpu.regs.a  = 0x12;
    cpu.regs.b  = 0x34;
    cpu.regs.x  = 0xCAFE;
    cpu.regs.y  = 0xBABE;
    cpu.regs.s  = 0x0400;
    cpu.regs.u  = 0x0800;
    cpu.regs.pc = 0x0100;
    let orig_regs = cpu.regs;

    cpu.mem.store(0x0100, &[    //      org $100
        0x34, 0xFF,             //      pshs pc, u, y, x, dp, b, a, cc
        0x35, 0xFF,             //      puls pc, u, y, x, dp, b, a, cc
    ]);

    cpu.step();
    cpu.dump_regs();

    cpu.step();
    cpu.regs.pc -= 2;               // account for PC fetch
    cpu.dump_regs();
    assert_eq!(orig_regs, cpu.regs);
}

#[test]
fn push_pull_u() {
    let mut cpu = test_cpu();

    cpu.regs.a  = 0x12;
    cpu.regs.b  = 0x34;
    cpu.regs.x  = 0xCAFE;
    cpu.regs.y  = 0xBABE;
    cpu.regs.s  = 0x0400;
    cpu.regs.u  = 0x0800;
    cpu.regs.pc = 0x0100;
    let orig_regs = cpu.regs;

    cpu.mem.store(0x0100, &[    //      org $100
        0x36, 0xFF,             //      pshu pc, s, y, x, dp, b, a, cc
        0x37, 0xFF,             //      pulu pc, s, y, x, dp, b, a, cc
    ]);

    cpu.step();
    cpu.dump_regs();

    cpu.step();
    cpu.regs.pc -= 2;               // account for PC fetch
    cpu.dump_regs();
    assert_eq!(orig_regs, cpu.regs);
}

#[test]
fn abx() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x0100, &[    //      org $100
        0x8E, 0xFF, 0x80,       //      ldx #$FF80
        0xC6, 0xC0,             //      ldb #$C0
        0x3A,                   //      abx
    ]);

    cpu.regs.pc = 0x0100;

    cpu.step_n(3);
    cpu.dump_regs();

    assert_eq!(cpu.regs.x, 0x0040);
}

#[test]
fn rti_e_set() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x0100, &[    //      org $100
        0x1A, 0x80,             //      orcc #$80   ; set E
        0x34, 0xFF,             //      pshs pc, u, y, x, dp, b, a, cc
        0x3B,                   //      rti
    ]);

    cpu.regs.s  = 0x0400;
    cpu.regs.pc = 0x0100;

    cpu.step_n(3);
    cpu.dump_regs();

    assert_eq!(cpu.regs.s, 0x0400);
    assert_eq!(cpu.regs.pc, 0x0104);
    assert_flags! { cpu =>
        CC_E: true
    }
}

#[test]
fn rti_e_clear() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x0100, &[    //      org $100
        0x1C, 0x7F,             //      andcc #$7F   ; clear E
        0x34, 0x81,             //      pshs pc, cc
        0x3B,                   //      rti
    ]);

    cpu.regs.s  = 0x0400;
    cpu.regs.pc = 0x0100;

    cpu.step_n(3);
    cpu.dump_regs();

    assert_eq!(cpu.regs.s, 0x0400);
    assert_eq!(cpu.regs.pc, 0x0104);
    assert_flags! { cpu =>
        CC_E: false
    }
}

#[test]
fn mul() {
    let mut cpu = test_cpu();

    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0xFF,             //      lda #$FF
        0xC6, 0xFF,             //      ldb #$FF
        0x3D,                   //      mul
        0x4F,                   //      clra
        0x3D,                   //      mul
    ]);

    cpu.regs.pc = 0x0100;

    cpu.step_n(3);
    cpu.dump_regs();
    assert_eq!(cpu.regs.d(), 0xFE01);
    assert_flags! { cpu =>
        CC_Z: false,
        CC_C: false
    }

    cpu.step_n(2);
    assert_eq!(cpu.regs.d(), 0x0000);
    assert_flags! { cpu =>
        CC_Z: true,
        CC_C: false
    }
}

#[test]
fn swi() {
    let mut cpu = test_cpu();

    // Our OS lives at 0xE000 and handles the SWI vector.
    // When a software interrupt occurs, it writes:
    //
    //   $CAFE to 0x0000
    //   $BABE to 0x0002
    cpu.mem.store(0xE000, &[    //      org $E000
        0x8E, 0xCA, 0xFE,       //      ldx #$CAFE
        0x9F, 0x00,             //      stx <$00
        0x10, 0x8E, 0xBA, 0xBE, //      ldy #$BABE
        0x10, 0x9F, 0x02,       //      sty <$02
        0x3B,                   //      rti
    ]);

    // Test code lives at 0x0100.
    cpu.mem.store(0x0100, &[    //      org $100
        0x86, 0x00,             //      lda #$00
        0x3F,                   //      swi
        0x9E, 0x00,             //      ldx <$00
        0x10, 0x9E, 0x02,       //      ldy <$02
        0x86, 0xAA,             //      lda #$AA
    ]);

    // Store the address of the SWI handler at the vector.
    cpu.mem.storew(0xFFFA, 0xE000);

    cpu.regs.s = 0x0400;
    cpu.regs.pc = 0x0100;
    cpu.step_n(10);
    cpu.dump_regs();

    assert_eq!(cpu.regs.a, 0xAA);
    assert_eq!(cpu.regs.x, 0xCAFE);
    assert_eq!(cpu.regs.y, 0xBABE);
}
