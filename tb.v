`timescale 1ns/1ns

module tb();

    reg clk = 0;
    reg reset = 0;
    wire [31:0] writedata, adr;
    wire memwrite;

    top dut(clk,reset,writedata,adr,memwrite);

    initial
        begin
            reset <= 1; # 5; reset <= 0;
        end

    initial 
	begin
	   forever begin
		clk<=~clk;
		#5;
	end
    end

    initial
    begin 
    #630;
    $stop;
    end



endmodule