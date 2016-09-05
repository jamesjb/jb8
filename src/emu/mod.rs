//
// mod.rs --- JB-8 emulator module.
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

pub mod error;
pub mod mem;
pub mod cpu;

pub use self::mem::{Mem, RAM};
pub use self::cpu::CPU;
pub use self::error::Result;
