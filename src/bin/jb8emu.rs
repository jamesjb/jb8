//
// jb8emu.rs --- Emulator tool entry point.
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

extern crate getopts;
extern crate jb8;

use std::env;
use std::process::exit;

use getopts::Options;

use jb8::emu::{Mem,RAM};
use jb8::emu::CPU;
use jb8::emu::Result;

fn print_usage(opts: Options) {
    let brief = format!("Usage: jb8emu [OPTIONS...] FILE...");
    print!("{}", opts.usage(&brief));
}

/// Parse and validate command line options, returning the `Matches`
/// object containing option information.
fn parse_options() -> getopts::Matches {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();

    opts.optopt("e", "entry", "entry point address (default: reset vector at FFFE)", "ADDR");
    opts.optopt("u", "until", "run until this PC", "ADDR");
    opts.optflag("", "help", "display this help and exit");
    opts.optflag("", "version", "output version information and exit");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m },
        Err(f) => {
            println!("jb8emu: {}\n", f.to_string());
            print_usage(opts);
            exit(1);
        },
    };

    if matches.opt_present("help") {
        print_usage(opts);
        exit(0);
    }

    if matches.opt_present("version") {
        println!("0.0.1");
        exit(0);
    }

    if matches.free.is_empty() {
        println!("jb8emu: no input files\n");
        print_usage(opts);
        exit(1);
    }

    matches
}

fn run() -> Result<()> {
    let matches = parse_options();

    let mut ram = RAM::new(0x10000);
    for infile in matches.free.iter() {
        try!(ram.load_ihex_file(infile));
    }

    let entry =
        if matches.opt_present("e") {
            let arg = matches.opt_str("e").unwrap();
            try!(u16::from_str_radix(&arg, 16))
        } else {
            ram.loadw(0xfffe)
        };

    let run_until: Option<u16> =
        if matches.opt_present("u") {
            let arg = matches.opt_str("u").unwrap();
            Some(try!(u16::from_str_radix(&arg, 16)))
        } else {
            None
        };

    let mut cpu = CPU::new(ram);
    cpu.regs.pc = entry;

    match run_until {
        Some(addr) => cpu.run_until(addr),
        None       => cpu.step_n(20),
    }

    Ok(())
}

fn main() {
    match run() {
        Ok(_) => (),
        Err(err) => {
            println!("jb8emu: {}", err);
            exit(1);
        }
    }
}
