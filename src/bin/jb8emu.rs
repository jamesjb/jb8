
extern crate jb8;

use jb8::emulator::mem::{Mem,RAM};
use jb8::emulator::cpu::CPU;

fn main() {
    println!("JB-8 Emulator");

    let mut ram = RAM::new(0x10000);

    // Poke some interesting code in to test...
    ram.storeb(0x0000, 0xCC);
    ram.storeb(0x0100, 0x73);        // COM $0000
    ram.storeb(0x0101, 0x00);
    ram.storeb(0x0102, 0x00);

    let mut cpu = CPU::new(ram);
    cpu.regs.pc = 0x100;
    cpu.dump_regs();
    cpu.step();
    cpu.dump_regs();
    println!("{:02X}", cpu.mem.loadb(0));
}
