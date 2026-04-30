// Verilog header file
`ifndef HELLO_VH
`define HELLO_VH

`timescale 1ns / 1ps

module hello_world #(
    parameter WIDTH = 8
) (
    input  wire             clk,
    input  wire             rst_n,
    input  wire [WIDTH-1:0] data_in,
    output reg  [WIDTH-1:0] data_out
);

    reg [WIDTH-1:0] counter;

    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            counter  <= 8'h00;
            data_out <= 8'h00;
        end else begin
            counter  <= counter + 1'b1;
            data_out <= data_in ^ counter;
        end
    end

    initial begin
        $display("Hello, Verilog!");
    end

endmodule

`endif
