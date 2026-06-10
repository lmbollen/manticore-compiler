// Variant of loop.v whose self-reporting uses $display/$finish (the MIPS32-
// harness style, lowered to privileged FLUSH/FINISH interrupts) instead of
// $masm_expect/$masm_stop, which the masm frontend currently rejects on the
// top module ("Top module can not have output"). The picorv32 core itself is
// unchanged; only the testbench reporting differs, and NUM_CYCLES is reduced
// so interpretation/co-simulation completes quickly.

`timescale 1 ns / 1 ps

module Main(input wire clock);

	localparam NUM_CYCLES = 1024;
	localparam RESET_CYCLES = 10;
	wire resetn;
	reg [31:0] cycle_counter = 0;
	reg [31:0] expected = 0;
	wire trap;

	assign resetn = !(cycle_counter < RESET_CYCLES);

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
		memory[0] = 32'h 3fc00093; //       li      x1,1020
		memory[1] = 32'h 0000a023; //       sw      x0,0(x1)
		memory[2] = 32'h 0000a103; // loop: lw      x2,0(x1)
		memory[3] = 32'h 00110113; //       addi    x2,x2,1
		memory[4] = 32'h 0020a023; //       sw      x2,0(x1)
		memory[5] = 32'h ff5ff06f; //       j       <loop>
	end

	always @(posedge clock) begin
		cycle_counter <= cycle_counter + 1;
		if (cycle_counter == NUM_CYCLES) begin
			$finish;
		end
	end
	reg [31 : 0] wword;
	wire [31 : 0] mem_word_addr;
	assign mem_word_addr = mem_addr >> 2;

	always @(posedge clock) begin
		mem_ready <= 0;
		if (mem_valid && !mem_ready) begin
			if (mem_addr < 1024) begin
				if (mem_addr == 1020 && mem_wstrb == 8'hf) begin
					// self-report: host/golden checks wdata == expected
					$display("STORE %d expected %d", mem_wdata, expected);
					expected <= expected + 1;
				end
				mem_ready <= 1;
				mem_rdata <= memory[mem_word_addr];

				wword = memory[mem_word_addr];

				// NOTE: we can not directly assign to subwords in memory
				// because then Yosys will make partial bit enables for
				// memwr cells which will make us generate invalid code!
				if (mem_wstrb[0]) wword[ 7: 0] = mem_wdata[ 7: 0];
				if (mem_wstrb[1]) wword[15: 8] = mem_wdata[15: 8];
				if (mem_wstrb[2]) wword[23:16] = mem_wdata[23:16];
				if (mem_wstrb[3]) wword[31:24] = mem_wdata[31:24];
				memory[mem_word_addr] <= wword;

			end
			/* add memory-mapped IO here */
		end
	end
endmodule
