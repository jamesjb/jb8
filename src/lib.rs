//
// lib.rs --- JB-8 core library.
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

#[macro_use]
extern crate bitflags;

#[macro_use]
pub mod cpu_decode {
    include!(concat!(env!("OUT_DIR"), "/cpu_decode.rs"));
}

pub mod mem;
pub mod cpu;
