// SystemVerilog example
`timescale 1ns / 1ps

package hello_pkg;
    typedef enum logic [1:0] {
        IDLE  = 2'b00,
        BUSY  = 2'b01,
        DONE  = 2'b10
    } state_t;
endpackage

interface bus_if #(parameter WIDTH = 32) (input logic clk);
    logic [WIDTH-1:0] data;
    logic             valid;
    logic             ready;

    modport master (output data, valid, input  ready);
    modport slave  (input  data, valid, output ready);
endinterface

module hello_world
    import hello_pkg::*;
#(
    parameter int WIDTH = 8
) (
    input  logic             clk,
    input  logic             rst_n,
    bus_if.slave             bus
);

    state_t state, next_state;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) state <= IDLE;
        else        state <= next_state;
    end

    always_comb begin
        unique case (state)
            IDLE: next_state = bus.valid ? BUSY : IDLE;
            BUSY: next_state = DONE;
            DONE: next_state = IDLE;
            default: next_state = IDLE;
        endcase
    end

    initial begin
        $display("Hello, SystemVerilog!");
        assert (WIDTH > 0) else $error("WIDTH must be positive");
    end

endmodule
