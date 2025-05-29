`timescale 1ns/1ps
module tb_RISC_V_top #(
    parameter half_cycle_100MHz=5
);
reg clk;
reg res;
RISC_V_top RISC_V_top(
    .clk(clk),
    .res(res)
);
initial begin
    clk<='b0;
    res<='b0;
    #13
    res<='b1;
    #150
    $stop;
end
always #half_cycle_100MHz clk<=~clk;
endmodule