//
// mem.rs
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

#![allow(dead_code)]

/// Operations on memory.
pub trait Mem {
    /// Load a byte from `addr` and return it.
    fn loadb(&mut self, addr: u16) -> u8;

    /// Store a byte at `addr`.
    fn storeb(&mut self, addr: u16, val: u8);

    /// Load a 16-bit value from `addr` and return it.
    fn loadw(&mut self, addr: u16) -> u16 {
        let hi = self.loadb(addr) as u16;
        let lo = self.loadb(addr.wrapping_add(1)) as u16;

        (hi << 8) | lo
    }

    /// Store a 16-bit value at `addr`.
    fn storew(&mut self, addr: u16, val: u16) {
        self.storeb(addr, ((val >> 8) & 0xff) as u8);
        self.storeb(addr.wrapping_add(1), (val & 0xff) as u8);
    }

    /// Store an array of bytes starting at `addr`. Useful for tests.
    fn store(&mut self, mut addr: u16, bytes: &[u8]) {
        for x in bytes.iter() {
            self.storeb(addr, *x);
            addr += 1;
        }
    }

    /// Read an array of bytes starting at `addr`. Useful for tests.
    fn load(&mut self, mut addr: u16, bytes: &mut [u8]) {
        for x in bytes.iter_mut() {
            *x = self.loadb(addr);
            addr += 1;
        }
    }
}

/// A random access memory device.
pub struct RAM {
    pub contents: Vec<u8>
}

impl RAM {
    /// Create a new `RAM` containing `size` bytes of memory.
    pub fn new(size: usize) -> RAM {
        assert!(size <= 0x10000);
        RAM {
            contents: vec![0u8; size]
        }
    }
}

// TODO: What to do if address is out of bounds?
impl Mem for RAM {
    fn loadb(&mut self, addr: u16) -> u8 {
        self.contents[addr as usize]
    }

    fn storeb(&mut self, addr: u16, val: u8) {
        self.contents[addr as usize] = val;
    }
}
