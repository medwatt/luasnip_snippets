-- imports <<<
local ls = require("luasnip")
local extras = require("luasnip.extras")
local d = ls.dynamic_node
local f = ls.function_node
local i = ls.insert_node
local isn = ls.indent_snippet_node
local rep = extras.rep
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local k = require("luasnip.nodes.key_indexer").new_key
local line_begin = require("luasnip.extras.conditions").line_begin
-- >>>

return {

    -- priority encoder <<<
    s(
        { trig = "encoder", dscr = "priority encoder" },
        fmt([[
            module {} #(
                parameter integer NUM_INPUTS = {}
            )(
                input logic [NUM_INPUTS-1:0] enc_in,
                output logic [$clog2(NUM_INPUTS)-1:0] enc_out,
                output logic enc_valid
            );

            always_comb begin
                enc_out = '0;
                enc_valid = 1'b0;
                for (int i = 0; i < NUM_INPUTS; i++) begin // MSB has the highest priority
                    if (enc_in[i] == 1'b1) begin
                        enc_out = $clog2(NUM_INPUTS)'(i);
                        enc_valid = 1'b1;
                    end
                end
            end

            endmodule: {}
        ]], {
            i(1, "priority_encoder"),
            i(2, "16"),
            rep(1),
        }),
        { condition = line_begin }
    ),

    -- >>>

    -- decoder <<<
    s(
        { trig = "decoder", dscr = "decoder" },
        fmt([[
            module {} #(
                parameter integer NUM_OUTPUTS = {}
            )(
                input logic [$clog2(NUM_OUTPUTS)-1:0] dec_in,
                output logic [NUM_OUTPUTS-1:0] dec_out
            );

                always_comb begin
                    dec_out = 2**dec_in;
                end

            endmodule: {}
        ]], {
            i(1, "decoder"),
            i(2, "16"),
            rep(1),
        }),
        { condition = line_begin }
    ),
    -- >>>

    -- adder <<<
    s(
        { trig = "adder", dscr = "adder" },
        fmt([[
            module {} #(
                parameter WIDTH = {}
            )(
                input logic [WIDTH-1:0] a, b,
                input logic carryin,
                output logic [WIDTH-1:0] sum,
                output logic carryout
            );

                assign {{carryout, sum}} = a + b + carryin;

            endmodule: {}
        ]], {
            i(1, "adder"),
            i(2, "8"),
            rep(1),
        }),
        { condition = line_begin }
    ),
    -- >>>

    -- binary counter <<<
    s(
        { trig = "counter", dscr = "binary counter" },
        fmt([[
            module {} #(
                parameter integer WIDTH = {}
            )(
                input logic clk,
                input logic rst_n,
                input logic enable,
                output logic [WIDTH-1:0] count
            );

            always_ff @(posedge clk) begin
                if (!rst_n) begin
                    count <= {{WIDTH{{1'b0}}}};
                end else if (enable) begin
                    count <= count + 1'b1;
                end
            end

            endmodule: {}
        ]], {
            i(1, "counter"),
            i(2, "8"),
            rep(1),
        }),
        { condition = line_begin }
    ),
    -- >>>

    -- register file <<<
    s(
        {trig = "regfile", dscr = "register file" },
        fmt([[
            module {} #(
                parameter int NUM_REGS = {},
                parameter int REG_WIDTH = {}
            )(
                input logic clk,
                input logic write_enable,
                input logic [$clog2(NUM_REGS)-1:0] read_addr1, read_addr2, write_addr,
                input logic [REG_WIDTH-1:0] write_data,
                output logic [REG_WIDTH-1:0] read_data1, read_data2
            );

                // Register array
                logic [REG_WIDTH-1:0] regs [NUM_REGS-1:0];

                // Write operation
                always_ff @(posedge clk) begin
                    if (write_enable)
                        regs[write_addr] <= write_data;
                end

                // Read operation
                assign read_data1 = regs[read_addr1];
                assign read_data2 = regs[read_addr2];

            endmodule: {}
        ]], {
            i(1, "regfile"),
            i(2, "32"),
            i(3, "32"),
            rep(1),
        }),
        { condition = line_begin }
    ),
    -- >>>

    -- rom <<<
    s(
        {trig = "rom", dscr = "read-only memory" },
        fmt([[
            module {} #(
                parameter DATA_WIDTH = {},
                parameter ADDR_WIDTH = {},
                parameter INIT_FILE = {}
            )(
                input wire [ADDR_WIDTH-1:0] addr,
                output reg [DATA_WIDTH-1:0] data_out
            );

                // ROM memory array
                reg [DATA_WIDTH-1:0] memory_array [(1<<ADDR_WIDTH)-1:0];

                // Initialize ROM contents from a file
                initial begin
                    // Use $readmemh for hex, $readmemb for binary
                    $readmemh(INIT_FILE, memory_array);
                end

                // Read data from memory array
                data_out <= memory_array[addr];

            endmodule: {}
        ]], {
            i(1, "rom"),
            i(2, "32"),
            i(3, "6"),
            i(4, '"data.txt"'),
            rep(1),
        }),
        { condition = line_begin }
    ),
    -- >>>

}
