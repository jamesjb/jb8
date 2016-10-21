//
// decoder.v --- JB-8 address decoder.
//
// Copyright (C) 2016, James Bielman <jamesjb@gmail.com>
// All Rights Reserved.
//
// Released under the "BSD3" license. See the file "LICENSE"
// for details.
//

// System Memory Map
// =================
//
// 0000 - DFFF		RAM
// E000 - E00F		I/O device #1
// E010 - E01F		I/O device #2
// E020 - E02F		I/O device #2
// E030 - E03F		I/O device #2
// E040 - E0FF		reserved
// E100 - FFFF		ROM

module decoder (
	input  wire [15:0] addr,
	input  wire clk_e,
	input  wire bus_rw,
	output wire ram_sel_N,
	output wire rom_sel_N,
	output wire [3:0] io_sel_N,
	output wire rd_N,
	output wire wr_N
);
	// Active low read and write enable inputs, valid when E is high.
	assign rd_N = ~((clk_e == 1'b1) & (bus_rw == 1'b1));
	assign wr_N = ~((clk_e == 1'b1) & (bus_rw == 1'b0));

	// Active low chip selects from address decoding.
	assign ram_sel_N   = ~(addr  <  16'hE000);
	assign rom_sel_N   = ~(addr  >= 16'hE100);
	assign io_sel_N[0] = ~((addr >= 16'hE000) & (addr <= 16'hE00F));
	assign io_sel_N[1] = ~((addr >= 16'hE010) & (addr <= 16'hE01F));
	assign io_sel_N[2] = ~((addr >= 16'hE020) & (addr <= 16'hE02F));
	assign io_sel_N[3] = ~((addr >= 16'hE030) & (addr <= 16'hE03F));
endmodule
