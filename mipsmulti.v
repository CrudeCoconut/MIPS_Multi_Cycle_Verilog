//-------------------------------------------------------
// Multicycle MIPS processor
//------------------------------------------------

module mips(input        clk, reset,
            output [31:0] adr, writedata,
            output        memwrite,
            input [31:0] readdata);

  wire        zero, pcen, irwrite, regwrite,
               alusrca, iord, memtoreg, regdst;
  wire [1:0]  alusrcb, pcsrc;
  wire [2:0]  alucontrol;
  wire [5:0]  op, funct;

  controller c(clk, reset, op, funct, zero,
               pcen, memwrite, irwrite, regwrite,
               alusrca, iord, memtoreg, regdst, 
               alusrcb, pcsrc, alucontrol);
  datapath dp(clk, reset, 
              pcen, irwrite, regwrite,
              alusrca, iord, memtoreg, regdst,
              alusrcb, pcsrc, alucontrol,
              op, funct, zero,
              adr, writedata, readdata);
endmodule

// Todo: Implement controller module
module controller(input       clk, reset,
                  input [5:0] op, funct,
                  input       zero,
                  output       pcen, memwrite, irwrite, regwrite,
                  output       alusrca, iord, memtoreg, regdst,
                  output [1:0] alusrcb, pcsrc,
                  output [2:0] alucontrol);

// **PUT YOUR CODE HERE**
    wire [1:0] ALUOp;
    wire PCWrite, Branch;

    maindec mainDecModule(clk,reset,op,
                        PCWrite, memwrite,irwrite,
                        regwrite,alusrca,Branch,iord,
                        memtoreg,regdst,
                        alusrcb,pcsrc,ALUOp);
    aludec aluDecModule(funct, ALUOp, alucontrol);

    assign pcen = (zero & Branch) | PCWrite;//pc enable logic 


endmodule

module maindec(input clk, reset,
               input [5:0] op,
               output PCWrite, memwrite, irwrite,
               output regwrite, alusrca, Branch, iord,
               output memtoreg, regdst,
               output [1:0] alusrcb, pcsrc, ALUOp);

    
    reg [14:0] fsmControl;
    assign {PCWrite,memwrite,irwrite,regwrite,
            alusrca,Branch,iord,memtoreg,regdst,
            alusrcb,pcsrc,ALUOp} = fsmControl;
    reg [3:0] state;

    always @ (posedge clk, posedge reset)
    begin
        if(reset)//s0 when reset signal is 1
        begin
            fsmControl <= 15'b101000000010000;//fetch
            state<=4'b0000;//s0
        end
            
        else if(state==4'b0000)
        begin
            fsmControl <= 15'b000000000110000;//decode
            state<=4'b0001;//s1
        end
            
        else if(state==4'b0001)//s1
        begin 
            case(op)//transition from s1 based on op
                6'b100011, 6'b101011: //lw/sw
                    begin
                    fsmControl<=15'b000010000100000;//memadr
                    state<=4'b0010;//s2
                    end
                6'b000000: //rtype
                    begin 
                    fsmControl<=15'b000010000000010;//rtypeex
                    state<=4'b0110;//s6
                    end
                6'b000100://beq
                    begin
                    fsmControl<=15'b000011000000101;//beqex
                    state<=4'b1000;//s8
                    end 
                6'b001000: //addi
                    begin
                    fsmControl<=15'b000010000100000;//addiex
                    state<=4'b1001;//s9
                    end
                6'b000010: //j
                    begin
                    fsmControl<=15'b100000000001000;//jumpex
                    state<=4'b1011;//s11
                    end 
                default: fsmControl <= 15'bxxxxxxxxxxxxxxx;//illegal
            endcase
        end

        else if(state==4'b0010)//s2 
        begin
            if(op==6'b100011)//lw
            begin
                fsmControl<=15'b000000100000000;//memrd
                state<=4'b0011;//s3
            end   
            else //sw
            begin
                fsmControl<=15'b010000100000000;//memwrt
                state<=4'b0101;//s5
            end                 
        end

        else if(state==4'b0011)//s3
        begin
            fsmControl<=15'b000100010000000;//memwb
            state<=4'b0100;//s4
        end

        else if(state==4'b0110)//s6
        begin
            fsmControl<=15'b000100001000000;//rtypewb
            state<=4'b0111;//s7
        end
        
        else if(state==4'b1001)//s9
        begin
            fsmControl<=15'b000100000000000;//addiwb
            state<=4'b1010;//s10
        end

        else //all other case transition to state 0
        begin
            fsmControl<=15'b101000000010000;//fetch 
            state<=4'b0000;//s0
        end
    end

endmodule


module aludec(input [5:0] funct,
              input [1:0] aluop,
              output reg [2:0] alucontrol);
    always@(*)
    begin
        case(aluop)//same decoder as single cycle
            2'b00: alucontrol <= 3'b010;
            2'b01: alucontrol <= 3'b110;
            default:
                case(funct)
                    6'b100000: alucontrol<=3'b010;
                    6'b100010: alucontrol<=3'b110;
                    6'b100100: alucontrol<=3'b000;
                    6'b100101: alucontrol<=3'b001;
                    6'b101010: alucontrol<=3'b111;
                    default:   alucontrol<=3'bxxx;
                endcase
        endcase
    end
endmodule

// Todo: Implement datapath
module datapath(input        clk, reset,
                input        pcen, irwrite, regwrite,
                input        alusrca, iord, memtoreg, regdst,
                input [1:0]  alusrcb, pcsrc, 
                input [2:0]  alucontrol,
                output [5:0]  op, funct,
                output        zero,
                output [31:0] adr, writedata, 
                input [31:0] readdata);

// **PUT YOUR CODE HERE** 
    wire [31:0] PC, PCNEXT;
    wire [31:0] Instr, Data, WD3;
    wire [4:0] A3;
    wire [31:0] RD1,RD2;
    wire [31:0] A,B;
    wire [31:0] SrcA,SrcB;
    wire [31:0] immExt,shiftImmExt;
    wire [31:0] ALUResult, ALUOut;
    wire [31:0] PCJump;

    dflipflop pcNextReg(clk,reset,pcen,PCNEXT,PC);//PC Reg with PCEn enable
    mux2 pcMux(PC,ALUOut,iord,adr);//mux with IorD select

    dflipflop instrReg(clk,reset,irwrite, readdata,Instr);//Instr Reg with IRWrite enable
    dflipflop dataReg( clk, reset, 1'b1, readdata, Data);//Data Reg

    //assigns op and funct with respective Instruction bits
    assign op = Instr[31:26];
    assign funct = Instr[5:0];

    mux2 #(5) regDstMux(Instr[20:16],Instr[15:11],regdst, A3);//mux with RegDst select
    mux2 memtoRegMux(ALUOut,Data,memtoreg,WD3);//mux with memtoreg select
    regfile rf(clk,regwrite,Instr[25:21],Instr[20:16],A3,WD3,RD1,RD2);//register file 
    dflipflop aReg(clk,reset,1'b1,RD1,A);//reg A, stores RD1 of register file
    dflipflop bReg(clk,reset,1'b1,RD2,B);//reg B, store RD2 of register file
    assign writedata = B;//assigns writedata to wire B

    mux2 srcAMux(PC,A,alusrca,SrcA);//mux to choose btw PC and A
    assign immExt = {{16{Instr[15]}},Instr[15:0]};//signext immediate
    assign shiftImmExt = {immExt[29:0],2'b00};//shifts signextimm
    mux4 srcBMux(B,32'b100,immExt,shiftImmExt,alusrcb,SrcB);//4 input mux to select btw B,4,signext,shiftedsignext

    ALU aluModule(SrcA,SrcB,alucontrol,ALUResult,zero);//alu module 

    dflipflop aluoutReg(clk,reset,1'b1,ALUResult,ALUOut);//ALUout register
    assign PCJump = {PC[31:28],Instr[25:0],2'b00};//creates PCJump adress
    mux4 pcNextMux(ALUResult,ALUOut,PCJump,32'bxxxx,pcsrc,PCNEXT);//4 input mux, with one illegal input
                                                                  //choose btw next values to store in PCNext

endmodule


//same regfile used in textbook
module regfile(input clk, we3,
                input [4:0] a1,a2,
                input [4:0] a3,
                input [31:0] wd3,
                output [31:0] rd1,rd2);
    reg [31:0] rf[31:0];//32 32 bit registers
    assign rd1 = (a1!=0)?rf[a1]:0;//if a1 is not register 0, returns word at a1 reg
    assign rd2 = (a2!=0)?rf[a2]:0;//if a2 is not register 0, returns word at a2 reg

    always @ (posedge clk)
        begin
            if(we3)//if enable is hi, writes data at wd3 to register a3
                rf[a3] <= wd3;
        end

endmodule

//dflipflop with and enable and reset
module dflipflop(input clk, rst,
                input en, 
                input [31:0] d,
                output reg [31:0] q);
    always@(posedge clk, posedge rst)
        begin
            if(rst)
                q<=0;
            else if(en) 
                q<=d;
        end
endmodule

//2 select mux
module mux2 #(parameter WIDTH = 32)//default 32 bit mux
            (input [WIDTH-1:0] d0, d1,
            input s,
            output [WIDTH-1 :0] y);
    assign y = s ? d1 : d0;
endmodule

//4 select mux
module mux4 #(parameter WIDTH = 32)//default 32 bit mux
            (input [WIDTH-1:0] d0, d1,
            input [WIDTH-1:0] d2, d3,
            input [1:0] s,
            output [WIDTH-1 :0] y);
    assign y = s[1] ? (s[0] ? d3 : d2) : (s[0] ? d1 : d0);
endmodule