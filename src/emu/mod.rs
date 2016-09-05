//
// mod.rs --- JB-8 emulator module.
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

//! The JB-8 emulator.
//!
//! This is an emulator for a 6809-based system. Currently
//! there is a CPU core in the `cpu` module and a memory
//! interface in the `mem` module. There are no memory-mapped
//! devices yet.
//!
//! # Examples
//!
//! Create a CPU with 64k of RAM, load an Intel Hex image
//! from the file `rom.hex`, and execute the first 100
//! instructions starting at 0x100.
//!
//! ```rust,no_run
//! use jb8::emu::*;
//!
//! let mut cpu = CPU::new(RAM::new(0x10000));
//! cpu.mem.load_ihex_file("rom.hex").unwrap();
//! cpu.regs.pc = 0x100;
//! cpu.step_n(100);
//! ```

pub mod error;
pub mod mem;
pub mod cpu;

pub use self::mem::{Mem, RAM};
pub use self::cpu::CPU;
pub use self::error::Result;
