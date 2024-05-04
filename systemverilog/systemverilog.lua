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

-- parse module for parameter and port names <<<
local function parse_module(module_text)

    local insert_matches = function(matches, result_table)
        for _, match in ipairs(matches) do
            if match then
                for param in match do
                    table.insert(result_table, param)
                end
            end
        end
    end

    local module_name = module_text:match("module%s+([%w_]+)%s*") or "ModuleName"
    local module_parameters = {}
    local module_signals = {}

    -- Define block patterns
    local parameter_pattern = "#%(([^%)]+)%)"
    local port_pattern = "[^#]%(([^;]+)%)"

    -- Extract blocks
    local parameter_block = module_text:match(parameter_pattern) or ""
    local port_block = module_text:match(port_pattern) or ""

    -- Find parameters
    local parameter_with_type_pattern = "parameter%s+%w+%s([%w_]+)%s*="
    local parameter_without_type_pattern = "parameter%s([%w_]+)%s*="
    local parameter_match_table = {
        parameter_block:gmatch(parameter_with_type_pattern),
        parameter_block:gmatch(parameter_without_type_pattern)
    }
    insert_matches(parameter_match_table, module_parameters)

    -- Find ports
    local port_name_pattern1 = "%s([%w_]+)%s*,"
    local port_name_pattern2 = "%s([%w_]+)%s*$"
    local port_match_table = {
        port_block:gmatch(port_name_pattern1),
        port_block:gmatch(port_name_pattern2)
    }
    insert_matches(port_match_table, module_signals)

    return module_name, module_parameters, module_signals
end

local function create_instance_from_module_definition(_, parent)
    selected_text = table.concat(parent.snippet.env.LS_SELECT_RAW)

    if (#parent.snippet.env.LS_SELECT_RAW > 0) then

        local function make_sub_snippet(index, tbl, begin_paren, end_paren)
            local nodes = {}
            if #tbl > 0 then
                table.insert(nodes, t(begin_paren))
                for idx, v in ipairs(tbl) do
                    table.insert(nodes, t({"", "\t." .. v .. "(" .. v .. ")"}))
                    if idx ~= #tbl then
                        table.insert(nodes, t(","))
                    end
                end
                table.insert(nodes, t({"", end_paren}))
            end
            return sn(index, nodes)
        end

        local module_name, parameters, ports = parse_module(selected_text)

        return sn(index,
            {
                t(module_name .. " "),
                i(1, "instance_name"),
                t(" "),
                make_sub_snippet(2, parameters, "#(", ")"),
                make_sub_snippet(3, ports, "(", ")")
            }
        )
    else
        return sn(nil, i(1, ""))
    end
end
-- >>>

-- generate else / else if block <<<
local function generate_else_if ()
    -- The function to add else if and else blocks dynamically
    return sn(nil, c(1, {
        t(""),  -- No else or else if, just end the if structure
        sn(nil, fmt([[
            else if ({}) begin
                {}
            end
            {}
        ]],
        {
            i(1, "else_if_condition"),
            i(2, "action_else_if_true"),
            d(3, generate_else_if, {})  -- Recursive call to add more else if or else blocks
        })),
        sn(nil, fmt([[
            else begin
                {}
            end
        ]], {
            i(1, "action_else"),
        }))
    }))
end
-- >>>

-- generate case <<<
local function generate_case(_, _, _, indent)
    local indent_str = indent and "$PARENT_INDENT\t" or "$PARENT_INDENT"
    return isn(nil, fmt([[
        case({})
            {}: // Case item
                {}

            default:
                {}
        endcase
    ]],
    {
        i(1, "expression"),
        i(2, "value"),
        i(3, "// Implement actions for this case"),
        i(4, "// Implement default actions")
    }), indent_str)
end
-- >>>

-- generate ternary <<<
local function generate_ternary(node_index, current_depth)
    if current_depth <= 1 then
        node_index = node_index or nil
        return sn(node_index, fmt("{} ? {} : {}", {
                i(1, "condition" ),
                i(2, "value_if_true"),
                i(3, "value_if_false")
            })
        )
    else
        return isn(node_index, fmt([[
            {} ? ({})
            : ({})
            ]], {
                i(1, "condition_" .. tostring(current_depth)),
                generate_ternary(2, current_depth - 1),  -- Recursive call for true case
                generate_ternary(3, current_depth - 1),  -- Recursive call for false case
            }), "$PARENT_INDENT" .. string.rep("\t", 3))
    end
end
-- >>>

return {

    -- module <<<
    s(
        { trig = "module", dscr = "systemverilog module" },
        {
            t("module "),
            i(1, "ModuleName"),
            t(" "),
            c(2, {
                t(""),
                sn(nil,
                    {
                        t({"#(", ""}),
                        t({"\tparameter integer WIDTH = 8,", ""}),
                        t({"\tparameter integer DEPTH = 256", ""}),
                        t(")")
                    }
                )
            }),
            c(3, {
                sn(1,
                    {
                        t("("),
                        i(1),
                        t(")"),
                    }
                ),
                sn(nil, fmt([[
                    (
                        input logic clk,
                        input logic rst_n,
                        input logic [WIDTH-1:0] data_in,
                        output logic [WIDTH-1:0] data_out
                    )
                ]], {}))
            }),
            t({";", "\t// Module Body", "\t"}),
            i(4, "// Implementation details"),
            t({"", "endmodule: "}),
            rep(1)
        }
    ),
    -- >>>

    -- instantiating a module <<<
    s(
        { trig = "inst", dscr = "instantiating a systemverilog module" },
            {
                i(1, "ModuleName"),
                t(" "),
                c(2, {
                    t(""),  -- No parameters
                    sn(nil, fmt([[
                         #(
                            .{}({}), // Parameter 1
                            .{}({}), // Parameter 2
                            {}
                        ){}
                    ]], {
                        i(1, "PARAM1"),
                        i(2, "value1"),
                        i(3, "PARAM2"),
                        i(4, "value2"),
                        i(5, "// Additional parameters"),
                        t(" "),
                    }))
                }),
                i(3, "instance_name"),
                c(4, {
                    sn(1,
                        {
                            t(" ("),
                            i(1, "out1, in1, in2"),
                            t(")"),
                        }
                    ),
                    sn(2, fmt([[
                         (
                            .{}({}), // Port 1
                            .{}({}), // Port 2
                            {}
                        )
                    ]], {
                        i(1, "signal1"),
                        i(2, "net1"),
                        i(3, "signal2"),
                        i(4, "net2"),
                        i(5, "// Additional connections"),
                    }))
                }),
            }
    ),
    -- >>>

    -- instantiating from a module definition <<<
    s({ trig = "modinst", dscr = "instantiate a systemverilog module from module definition" }, {
        d(1, create_instance_from_module_definition),
    }),
    -- >>>

    -- if statement <<<
    s(
        { trig = "if", dscr = "if, else if, and else statements" },
        sn(1, fmt([[
            if ({}) begin
                {};
            end
            {}
        ]],
        {
            i(1, "condition"),
            i(2, "action_if_true"),
            d(3, generate_else_if, nil),
        })),
        { condition = line_begin }
    ),
    -- >>>

     -- case statement <<<
    s(
        { trig = "case", dscr = "case statement" },
        c(1, {
            -- First choice: Only the case statement
            sn(1, {
                d(1, generate_case, nil, {user_args={false}})
            }),
            -- Second choice: always_comb block wrapping the case statement
            sn(2, {
                t({"always_comb", "\t"}),
                d(1, generate_case, nil, {user_args={true}}),
                t({"", "end"}),
            })
        }),
        { condition = line_begin }
    ),

 -- >>>

    -- always block <<<
    s(
        { trig = "always", dscr = "always block" },
        c(1, {
            -- always_ff block for synchronous logic
            fmt([[
                always_ff @(posedge {} or negedge {}) begin
                    if ({} == 1'b0) begin
                        {}
                    end else begin
                        {}
                    end
                end
            ]], {
                i(1, "clk"),
                i(2, "rst_n"),
                rep(2),
                i(3, "// Reset logic here"),
                i(4, "// Non-reset logic here")
            }),
            -- always_comb block for combinational logic
            fmt([[
                always_comb begin
                    {}
                end
            ]], {
                i(1, "// Combinational logic here")
            }),
            -- always_latch block for latching logic
            fmt([[
                always_latch begin
                    if ({}) begin
                        {}
                    end
                end
            ]], {
                i(1, "condition"),
                i(2, "// Latching logic here")
            })
        }),
        { condition = line_begin }
    ),
    -- >>>

    -- ternary operator <<<
    s(
        { trig = "ternary(%d*)", regTrig = true, dscr = "ternary operator"},
        d(1, function(_, snip)
            local depth = tonumber(snip.captures[1]) or 1
            return generate_ternary(nil, depth)
        end, {}),
        { condition = line_begin }
    ),
    -- >>>

    -- fsm <<<
    s(
        { trig = "fsm", dscr = "FSM template with three states" },
        fmt([[
            // Define state encoding
            typedef enum logic [1:0] {{
                S1, S2, S3
            }} state_t;

            // State and Next State Variables
            state_t state_reg, state_next;

            // State Register
            always_ff @(posedge clk or negedge rst_n) begin
                if (!rst_n) begin
                    state_reg <= S1; // Reset state
                end else begin
                    state_reg <= state_next; // Next state logic
                end
            end

            // Next State Logic
            always_comb begin
                case(state_reg)
                    S1: begin
                        // Conditions to transition from S1
                        if (condition1) begin
                            state_next = S2;
                        end else begin
                            state_next = S1; // Stay in S1
                        end
                    end
                    S2: begin
                        // Conditions to transition from S2
                        if (condition2) begin
                            state_next = S3;
                        end else begin
                            state_next = S2; // Stay in S2
                        end
                    end
                    S3: begin
                        // Conditions to transition from S3
                        if (condition3) begin
                            state_next = S1; // Transition to S1 or some other state
                        end else begin
                            state_next = S3; // Stay in S3
                        end
                    end
                    default: state_next = S1; // Default state
                endcase
            end

            // Output Logic
            always_comb begin
                case(state_reg)
                    S1: output_signal = value1; // Output value for S1
                    S2: output_signal = value2; // Output value for S2
                    S3: output_signal = value3; // Output value for S3
                    default: output_signal = default_value; // Default output value
                endcase
            end
        ]], {}),
        { condition = line_begin }
    ),
    -- >>>

    -- for generate <<<
    s(
        { trig = "forgenerate", dscr = "for generate loop" },
        fmt([[
            genvar {};
            for ({} = {}; {} <= {}; {} = {} + 1) begin : {}
                {}
            end
        ]], {
            i(1, "i"),
            rep(1),
            i(2, "0"),
            rep(1),
            i(3, "N"),
            rep(1),
            rep(1),
            i(4, "genblk"),
            i(5, "// Contents of the generate loop")
        }),
        { condition = line_begin }
    ),
    -- >>>

}
