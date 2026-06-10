// Multi-instance picorv32 harness (the paper scaled this benchmark the same
// way — see the commented-out generate loop in loop.v). K picorv32 units each
// run the counting loop and fold their checked-store values into a signature;
// a single reporter (the only privileged process) periodically $displays all
// signatures and $finishes — keeping every unit live (observed) while
// spreading ~10 processes per unit across the grid, which on a multi-chip
// torus forces NoC traffic across the chip-to-chip seams.

`timescale 1 ns / 1 ps

module picorv_unit (
	input  wire        clock,
	output reg  [31:0] signature
);

	localparam RESET_CYCLES = 10;
	wire resetn;
	reg [31:0] cycle_counter = 0;

	assign resetn = !(cycle_counter < RESET_CYCLES);

	wire trap;
	wire mem_valid;
	wire mem_instr;
	reg mem_ready;
	wire [31:0] mem_addr;
	wire [31:0] mem_wdata;
	wire [3:0] mem_wstrb;
	reg  [31:0] mem_rdata;

	picorv32 #(
		.TWO_CYCLE_ALU(1),
		.TWO_CYCLE_COMPARE(1)
	) dut (
		.clk         (clock      ),
		.resetn      (resetn     ),
		.trap        (trap       ),
		.mem_valid   (mem_valid  ),
		.mem_instr   (mem_instr  ),
		.mem_ready   (mem_ready  ),
		.mem_addr    (mem_addr   ),
		.mem_wdata   (mem_wdata  ),
		.mem_wstrb   (mem_wstrb  ),
		.mem_rdata   (mem_rdata  )
	);

	reg [31:0] memory [0:255];

	initial begin
		signature = 0;
		memory[0] = 32'h 3fc00093; //       li      x1,1020
		memory[1] = 32'h 0000a023; //       sw      x0,0(x1)
		memory[2] = 32'h 0000a103; // loop: lw      x2,0(x1)
		memory[3] = 32'h 00110113; //       addi    x2,x2,1
		memory[4] = 32'h 0020a023; //       sw      x2,0(x1)
		memory[5] = 32'h ff5ff06f; //       j       <loop>
	end

	always @(posedge clock) begin
		cycle_counter <= cycle_counter + 1;
	end

	reg [31 : 0] wword;
	wire [31 : 0] mem_word_addr;
	assign mem_word_addr = mem_addr >> 2;

	always @(posedge clock) begin
		mem_ready <= 0;
		if (mem_valid && !mem_ready) begin
			if (mem_addr < 1024) begin
				if (mem_addr == 1020 && mem_wstrb == 8'hf) begin
					// fold the stored counter value into the unit's signature
					signature <= (signature ^ mem_wdata) + 1;
				end
				mem_ready <= 1;
				mem_rdata <= memory[mem_word_addr];

				wword = memory[mem_word_addr];
				// NOTE: no partial bit-enable writes (Yosys memwr restriction)
				if (mem_wstrb[0]) wword[ 7: 0] = mem_wdata[ 7: 0];
				if (mem_wstrb[1]) wword[15: 8] = mem_wdata[15: 8];
				if (mem_wstrb[2]) wword[23:16] = mem_wdata[23:16];
				if (mem_wstrb[3]) wword[31:24] = mem_wdata[31:24];
				memory[mem_word_addr] <= wword;
			end
		end
	end
endmodule

module Main(input wire clock);

	localparam NUM_CYCLES = 1024;
	localparam K = 3;

	wire [31:0] sig0, sig1, sig2;

	picorv_unit u0(.clock(clock), .signature(sig0));
	picorv_unit u1(.clock(clock), .signature(sig1));
	picorv_unit u2(.clock(clock), .signature(sig2));

	// single reporter == the only privileged process
	reg [31:0] cyc = 0;
	always @(posedge clock) begin
		cyc <= cyc + 1;
		if (cyc[7:0] == 8'hFF) begin
			$display("SIG %d %d %d", sig0, sig1, sig2);
		end
		if (cyc == NUM_CYCLES) begin
			$finish;
		end
	end
endmodule
