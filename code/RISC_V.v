module RISC_V_top #(
    parameter RISC_word_size=32,
              RISC_addr_size=8
)(
    input clk,
    input res
);
wire [RISC_addr_size-1:0] CPU_pc;
wire [RISC_word_size-1:0] CPU_inst;
wire [RISC_word_size-1:0] CPU_rdata;
wire [RISC_word_size-1:0] CPU_wdata;
wire [RISC_word_size-1:0] CPU_addr;
wire                      CPU_wen;
wire                      CPU_ren;
Inst_Mem Inst_Mem(
    .IM_addr(CPU_pc),
    .IM_inst(CPU_inst)
);
CPU CPU(
    .CPU_clk(clk),
    .CPU_res(res),
    .CPU_inst(CPU_inst),
    .CPU_rdata(CPU_rdata),
    .CPU_pc(CPU_pc),
    .CPU_wdata(CPU_wdata),
    .CPU_addr(CPU_addr),
    .CPU_wen(CPU_wen),
    .CPU_ren(CPU_ren)
);
Data_Mem Data_Mem(
    .DM_clk(clk),
    .DM_res(res),
    .DM_wdata(CPU_wdata),
    .DM_addr(CPU_addr),
    .DM_wen(CPU_wen),
    .DM_ren(CPU_ren),
    .DM_rdata(CPU_rdata)
);
endmodule
module CPU #(
    parameter CPU_word_size=32,
              CPU_addr_size=8,
              CPU_opcode_size=7,
              CPU_fun3_size=3
)(
    input                      CPU_clk,
    input                      CPU_res,
    input  [CPU_word_size-1:0] CPU_inst,
    input  [CPU_word_size-1:0] CPU_rdata,
    output [CPU_addr_size-1:0] CPU_pc,
    output [CPU_word_size-1:0] CPU_wdata,
    output [CPU_word_size-1:0] CPU_addr,
    output                     CPU_wen,
    output                     CPU_ren
);
wire                       PRO_branch;
wire                       PRO_memread;
wire                       PRO_memtoreg;
wire                       PRO_regwrite;
wire                       PRO_aluctr;
wire                       PRO_memwrite;
wire [CPU_opcode_size-1:0] PRO_opcode;
wire [CPU_fun3_size-1:0]   PRO_func3;
wire                       PRO_func7;
PRO PRO(
    .PRO_clk(CPU_clk),
    .PRO_res(CPU_res),
    .PRO_inst(CPU_inst),
    .PRO_rdata(CPU_rdata),
    .PRO_branch(PRO_branch),
    .PRO_memread(PRO_memread),
    .PRO_memtoreg(PRO_memtoreg),
    .PRO_alusrc(PRO_alusrc),
    .PRO_regwrite(PRO_regwrite),
    .PRO_aluctr(PRO_aluctr),
    .PRO_memwrite(PRO_memwrite),
    .PRO_pc(CPU_pc),
    .PRO_wdata(CPU_wdata),
    .PRO_addr(CPU_addr),
    .PRO_opcode(PRO_opcode),
    .PRO_func3(PRO_func3),
    .PRO_func7(PRO_func7),
    .PRO_wen(CPU_wen),
    .PRO_ren(CPU_ren)
);
CTR CTR(
    .CTR_opcode(PRO_opcode),
    .CTR_fun7(PRO_func7),
    .CTR_fun3(PRO_func3),
    .CTR_branch(PRO_branch),
    .CTR_ren(PRO_memread),
    .CTR_memtoreg(PRO_memtoreg),
    .CTR_wen(PRO_memwrite),
    .CTR_alusrc(PRO_alusrc),
    .CTR_regwrite(PRO_regwrite),
    .CTR_aluctr(PRO_aluctr)
);
endmodule
module PRO #(
    parameter PRO_word_size=32,
              PRO_addr_size=8,
              PRO_opcode_size=7,
              PRO_r_size=5,
              PRO_fun3_size=3
)(
    input                        PRO_clk,
    input                        PRO_res,
    input  [PRO_word_size-1:0]   PRO_inst,
    input  [PRO_word_size-1:0]   PRO_rdata,
    input                        PRO_branch,
    input                        PRO_memread,
    input                        PRO_memtoreg,
    input                        PRO_alusrc,
    input                        PRO_regwrite,
    input                        PRO_aluctr,
    input                        PRO_memwrite,
    output [PRO_addr_size-1:0]   PRO_pc,
    output [PRO_word_size-1:0]   PRO_wdata,
    output [PRO_word_size-1:0]   PRO_addr,
    output [PRO_opcode_size-1:0] PRO_opcode,
    output [PRO_fun3_size-1:0]   PRO_func3,
    output                       PRO_func7,
    output                       PRO_wen,
    output                       PRO_ren
);
wire [PRO_word_size-1:0] if_id_pc_in;
wire [PRO_word_size-1:0] ex_pc_mux_din0;
wire                     ex_jump;
wire                     ex_load_use;
wire [PRO_word_size-1:0] id_ex_pc_in;
wire [PRO_word_size-1:0] id_inst_in;
wire [PRO_word_size-1:0] id_ex_imm_in;
wire [PRO_word_size-1:0] id_ex_rs1_data_in;
wire [PRO_word_size-1:0] id_ex_rs2_data_in;
wire [PRO_r_size-1:0]    id_ex_rs1_in;
wire [PRO_r_size-1:0]    id_ex_rs2_in;
wire [PRO_r_size-1:0]    id_ex_rd_in;
wire [PRO_word_size-1:0] ex_pc_in;
wire [PRO_word_size-1:0] ex_imm_in;
wire [PRO_r_size-1:0]    id_ex_rs1_out;
wire [PRO_r_size-1:0]    id_ex_rs2_out;
wire [PRO_r_size-1:0]    id_ex_rd_out;
wire [PRO_word_size-1:0] ex_rs1_data_in;
wire [PRO_word_size-1:0] ex_rs2_data_in;
wire                     ex_branch_in;
wire                     id_ex_memread_out;
wire                     ex_mem_memtoreg_in;
wire                     ex_aluctr_in;
wire                     id_ex_memwrite_out;
wire                     ex_alusrc_in;
wire                     id_ex_regwrite_out;
wire [PRO_r_size-1:0]    ex_mem_rd_out;
wire                     ex_mem_memread_out;
wire [PRO_r_size-1:0]    mem_wb_rd_out;
wire                     mem_wb_regwrite_out;
wire [PRO_word_size-1:0] ex_mem_alu_data_out;
wire [PRO_word_size-1:0] mem_wb_alu_data_out;
wire                     ex_mem_forwardc_in;
wire [PRO_word_size-1:0] ex_alu_data_out;
wire                     mem_wb_memtoreg_in;
wire                     mem_forward_c;
wire [PRO_word_size-1:0] mem_wb_rdata_out;
wire [PRO_word_size-1:0] ex_mem_rs2_data_out;
wire [PRO_word_size-1:0] wb_wdata_out;
wire                     wb_memtoreg_in;
wire [PRO_word_size-1:0] ex_pc_out;
assign PRO_addr=ex_mem_alu_data_out;
assign PRO_ren=ex_mem_memread_out;
if_stage if_stage(
    .if_clk(PRO_clk),
    .if_res(PRO_res),
    .if_load_use(ex_load_use),
    .if_pc_in(ex_pc_out),
    .if_pc_out(if_id_pc_in),
    .if_pc_adder1_out(ex_pc_mux_din0),
    .if_pc_addr(PRO_pc)
);
if_id_regs if_id_regs(
    .if_id_clk(PRO_clk),
    .if_id_res(PRO_res),
    .if_id_jump(ex_jump),
    .if_id_load_use(ex_load_use),
    .if_id_pc_in(if_id_pc_in),
    .if_id_inst_in(PRO_inst),
    .if_id_pc_out(id_ex_pc_in),
    .if_id_inst_out(id_inst_in)
);
id_stage id_stage(
    .id_clk(PRO_clk),
    .id_res(PRO_res),
    .id_inst_in(id_inst_in),
    .id_regwrite_in(mem_wb_regwrite_out),
    .id_wdata_in(wb_wdata_out),
    .id_rd_in(mem_wb_rd_out),
    .id_opcode_out(PRO_opcode),
    .id_imm_out(id_ex_imm_in),
    .id_func3_out(PRO_func3),
    .id_func7_out(PRO_func7),
    .id_rs1_data_out(id_ex_rs1_data_in),
    .id_rs2_data_out(id_ex_rs2_data_in),
    .id_rs1_out(id_ex_rs1_in),
    .id_rs2_out(id_ex_rs2_in),
    .id_rd_out(id_ex_rd_in)
);
id_ex_regs id_ex_regs(
    .id_ex_clk(PRO_clk),
    .id_ex_res(PRO_res),
    .id_ex_jump(ex_jump),
    .id_ex_load_use(ex_load_use),
    .id_ex_pc_in(id_ex_pc_in),
    .id_ex_imm_in(id_ex_imm_in),
    .id_ex_rs1_in(id_ex_rs1_in),
    .id_ex_rs2_in(id_ex_rs2_in),
    .id_ex_rd_in(id_ex_rd_in),
    .id_ex_rs1_data_in(id_ex_rs1_data_in),
    .id_ex_rs2_data_in(id_ex_rs2_data_in),
    .id_ex_branch_in(PRO_branch),
    .id_ex_memread_in(PRO_memread),
    .id_ex_memtoreg_in(PRO_memtoreg),
    .id_ex_aluctr_in(PRO_aluctr),
    .id_ex_memwrite_in(PRO_memwrite),
    .id_ex_alusrc_in(PRO_alusrc),
    .id_ex_regwrite_in(PRO_regwrite),
    .id_ex_pc_out(ex_pc_in),
    .id_ex_imm_out(ex_imm_in),
    .id_ex_rs1_out(id_ex_rs1_out),
    .id_ex_rs2_out(id_ex_rs2_out),
    .id_ex_rd_out(id_ex_rd_out),
    .id_ex_rs1_data_out(ex_rs1_data_in),
    .id_ex_rs2_data_out(ex_rs2_data_in),
    .id_ex_branch_out(ex_branch_in),
    .id_ex_memread_out(id_ex_memread_out),
    .id_ex_memtoreg_out(ex_mem_memtoreg_in),
    .id_ex_aluctr_out(ex_aluctr_in),
    .id_ex_memwrite_out(id_ex_memwrite_out),
    .id_ex_alusrc_out(ex_alusrc_in),
    .id_ex_regwrite_out(id_ex_regwrite_out)
);
ex_stage ex_stage(
    .ex_pc_in(ex_pc_in),
    .ex_pc_mux_din0(ex_pc_mux_din0),
    .ex_imm_in(ex_imm_in),
    .ex_branch_in(ex_branch_in),
    .ex_rs1_data_in(ex_rs1_data_in),
    .ex_rs2_data_in(ex_rs2_data_in),
    .ex_alusrc_in(ex_alusrc_in),
    .ex_aluctr_in(ex_aluctr_in),
    .id_ex_memwrite_out(id_ex_memwrite_out),
    .id_ex_rs1_out(id_ex_rs1_out),
    .id_ex_rs2_out(id_ex_rs2_out),
    .ex_mem_rd_out(ex_mem_rd_out),
    .ex_mem_memread_out(ex_mem_memread_out),
    .ex_mem_regwrite_out(ex_mem_regwrite_out),
    .mem_wb_rd_out(mem_wb_rd_out),
    .mem_wb_regwrite_out(mem_wb_regwrite_out),
    .id_ex_rs1_in(id_ex_rs1_in),
    .id_ex_rs2_in(id_ex_rs2_in),
    .id_ex_rd_out(id_ex_rd_out),
    .id_ex_memread_out(id_ex_memread_out),
    .id_ex_memwrite_in(PRO_memwrite),
    .id_ex_regwrite_out(id_ex_regwrite_out),
    .ex_mem_alu_data_out(ex_mem_alu_data_out),
    .mem_wb_alu_data_out(mem_wb_alu_data_out),
    .ex_pc_out(ex_pc_out),
    .ex_alu_data_out(ex_alu_data_out),
    .ex_forward_c(ex_mem_forwardc_in),
    .ex_load_use(ex_load_use),
    .ex_jump(ex_jump)
);
ex_mem_regs ex_mem_regs(
    .ex_mem_clk(PRO_clk),
    .ex_mem_res(PRO_res),
    .ex_mem_rs2_data_in(ex_rs2_data_in),
    .ex_mem_alu_data_in(ex_alu_data_out),
    .ex_mem_rd_in(id_ex_rd_out),
    .ex_mem_memread_in(id_ex_memread_out),
    .ex_mem_memtoreg_in(ex_mem_memtoreg_in),
    .ex_mem_memwrite_in(id_ex_memwrite_out),
    .ex_mem_regwrite_in(id_ex_regwrite_out),
    .ex_mem_forwardc_in(ex_mem_forwardc_in),
    .ex_mem_rs2_data_out(ex_mem_rs2_data_out),
    .ex_mem_alu_data_out(ex_mem_alu_data_out),
    .ex_mem_rd_out(ex_mem_rd_out),
    .ex_mem_memread_out(ex_mem_memread_out),
    .ex_mem_memtoreg_out(mem_wb_memtoreg_in),
    .ex_mem_memwrite_out(PRO_wen),
    .ex_mem_regwrite_out(ex_mem_regwrite_out),
    .ex_mem_forwardc_out(mem_forward_c)
);
mem_stage mem_stage(
    .mem_rdata_in(mem_wb_rdata_out),
    .mem_rs2_data_in(ex_mem_rs2_data_out),
    .mem_sel(mem_forward_c),
    .mem_wdata_out(PRO_wdata)
);
mem_wb_regs mem_wb_regs(
    .mem_wb_clk(PRO_clk),
    .mem_wb_res(PRO_res),
    .mem_wb_rdata_in(PRO_rdata),
    .mem_wb_alu_data_in(ex_mem_alu_data_out),
    .mem_wb_rd_in(ex_mem_rd_out),
    .mem_wb_memtoreg_in(mem_wb_memtoreg_in),
    .mem_wb_regwrite_in(ex_mem_regwrite_out),
    .mem_wb_rdata_out(mem_wb_rdata_out),
    .mem_wb_alu_data_out(mem_wb_alu_data_out),
    .mem_wb_rd_out(mem_wb_rd_out),
    .mem_wb_memtoreg_out(wb_memtoreg_in),
    .mem_wb_regwrite_out(mem_wb_regwrite_out)
);
wb_stage wb_stage(
    .wb_rdata_in(mem_wb_rdata_out),
    .wb_alu_data_in(mem_wb_alu_data_out),
    .wb_memtoreg_in(wb_memtoreg_in),
    .wb_wdata_out(wb_wdata_out)
);
endmodule
module CTR #(
    parameter CTR_opcode_size=7,
              CTR_fun3_size=3
)(
    input  [CTR_opcode_size-1:0] CTR_opcode,
    input                        CTR_fun7,
    input  [CTR_fun3_size-1:0]   CTR_fun3,
    output                       CTR_branch,
    output                       CTR_ren,
    output                       CTR_memtoreg,
    output                       CTR_wen,
    output                       CTR_alusrc,
    output                       CTR_regwrite,
    output                       CTR_aluctr
);
wire ac_aluop;
main_ctr main_ctr(
    .mc_opcode(CTR_opcode),
    .mc_branch(CTR_branch),
    .mc_memread(CTR_ren),
    .mc_memtoreg(CTR_memtoreg),
    .mc_aluop(ac_aluop),
    .mc_memwrite(CTR_wen),
    .mc_alusrc(CTR_alusrc),
    .mc_regwrite(CTR_regwrite)
);
alu_ctr alu_ctr(
    .ac_fun7(CTR_fun7),
    .ac_fun3(CTR_fun3),
    .ac_aluop(ac_aluop),
    .ac_out(CTR_aluctr)
);
endmodule
module if_stage #(
    parameter if_word_size=32,
              if_addr_size=8
)(
    input                     if_clk,
    input                     if_res,
    input                     if_load_use,
    input  [if_word_size-1:0] if_pc_in,
    output [if_word_size-1:0] if_pc_out,
    output [if_word_size-1:0] if_pc_adder1_out,
    output [if_addr_size-1:0] if_pc_addr
);
wire [if_word_size-1:0] pc_dout;
pc pc(
    .pc_clk(if_clk),
    .pc_res(if_res), 
    .pc_in(if_pc_in),
    .pc_load_use(if_load_use),
    .pc_out(pc_dout)
);
pc_adder1 pc_adder1(
    .pc_adder1_din(pc_dout),
    .pc_adder1_dout(if_pc_adder1_out)
);
assign if_pc_out=pc_dout;
assign if_pc_addr=if_pc_out[9:2];
endmodule
module pc #(
    parameter pc_size=32
)(
    input                    pc_clk,
    input                    pc_res, 
    input      [pc_size-1:0] pc_in,
    input                    pc_load_use,
    output reg [pc_size-1:0] pc_out
);
always@(posedge pc_clk or negedge pc_res) begin
    if(~pc_res)
        pc_out<='b0;
        else if(pc_load_use)
            pc_out<=pc_out;
            else
                pc_out<=pc_in;
end
endmodule
module pc_adder1 #(
    parameter pc_adder1_size=32
)(
    input  [pc_adder1_size-1:0] pc_adder1_din,
    output [pc_adder1_size-1:0] pc_adder1_dout
);
assign pc_adder1_dout=pc_adder1_din+32'd4;
endmodule
module Inst_Mem #(
    parameter IM_addr_size=8,
              IM_inst_size=32,
              IM_rom_size=256
)(
    input  [IM_addr_size-1:0] IM_addr,
    output [IM_inst_size-1:0] IM_inst
);
reg [IM_inst_size-1:0] IM_rom [IM_rom_size-1:0];
initial begin
    $readmemb("RISC_V_IM.txt",IM_rom);
end
assign IM_inst=IM_rom[IM_addr];
endmodule
module if_id_regs #(
    parameter if_id_size=32
)(
    input                       if_id_clk,
    input                       if_id_res,
    input                       if_id_jump,
    input                       if_id_load_use,
    input      [if_id_size-1:0] if_id_pc_in,
    input      [if_id_size-1:0] if_id_inst_in,
    output reg [if_id_size-1:0] if_id_pc_out,
    output reg [if_id_size-1:0] if_id_inst_out
);
always@(posedge if_id_clk or negedge if_id_res) begin
    if(~if_id_res)
        if_id_pc_out<='b0;
        else
            if_id_pc_out<=if_id_pc_in;
end
always@(posedge if_id_clk or negedge if_id_res) begin
    if((~if_id_res)|if_id_jump)
        if_id_inst_out<='b0;
        else if(if_id_load_use)
            if_id_inst_out<=if_id_inst_out;
            else
                if_id_inst_out<=if_id_inst_in;
end
endmodule
module id_stage #(
    parameter id_inst_size=32,
              id_opcode_size=7,
              id_rs_size=5,
              id_rd_size=5,
              id_imm_size=32,
              id_fun3_size=3
)(
    input                       id_clk,
    input                       id_res,
    input  [id_inst_size-1:0]   id_inst_in,
    input                       id_regwrite_in,
    input  [id_inst_size-1:0]   id_wdata_in,
    input  [id_rd_size-1:0]     id_rd_in,
    output [id_opcode_size-1:0] id_opcode_out,
    output [id_imm_size-1:0]    id_imm_out,
    output [id_fun3_size-1:0]   id_func3_out,
    output                      id_func7_out,
    output [id_inst_size-1:0]   id_rs1_data_out,
    output [id_inst_size-1:0]   id_rs2_data_out,
    output [id_rs_size-1:0]     id_rs1_out,
    output [id_rs_size-1:0]     id_rs2_out,
    output [id_rd_size-1:0]     id_rd_out
);
wire [id_rs_size-1:0] regs_rs1;
wire [id_rs_size-1:0] regs_rs2;
inst_dec inst_dec(
    .dec_inst(id_inst_in),
    .dec_opcode(id_opcode_out),
    .dec_rs1(regs_rs1),
    .dec_rs2(regs_rs2),
    .dec_rd(id_rd_out),
    .dec_func3(id_func3_out),
    .dec_func7(id_func7_out),
    .dec_imm(id_imm_out)
);
regs regs(
    .regs_clk(id_clk),
    .regs_res(id_res),
    .regs_wen(id_regwrite_in),
    .regs_rs1(regs_rs1),
    .regs_rs2(regs_rs2), 
    .regs_rd(id_rd_in),
    .regs_wdata(id_wdata_in),
    .regs_rs1_data(id_rs1_data_out),
    .regs_rs2_data(id_rs2_data_out)
);
assign id_rs1_out=regs_rs1;
assign id_rs2_out=regs_rs2;
endmodule
module inst_dec #(
    parameter dec_inst_size=32,
              dec_opcode_size=7,
              dec_rs_size=5,
              dec_rd_size=5,
              dec_func3_size=3,
              dec_imm_size=32,
              dec_I_size=7'b0000011,
              dec_S_size=7'b0100011,
              dec_B_size=7'b1100011
)(
    input  [dec_inst_size-1:0]   dec_inst,
    output [dec_opcode_size-1:0] dec_opcode,
    output [dec_rs_size-1:0]     dec_rs1,
    output [dec_rs_size-1:0]     dec_rs2,
    output [dec_rd_size-1:0]     dec_rd,
    output [dec_func3_size-1:0]  dec_func3,
    output                       dec_func7,
    output [dec_imm_size-1:0]    dec_imm
);
assign dec_opcode=dec_inst[6:0];
assign dec_rs1=dec_inst[19:15];
assign dec_rs2=dec_inst[24:20];
assign dec_rd=dec_inst[11:7];
assign dec_func3=dec_inst[14:12];
assign dec_func7=dec_inst[30];
wire dec_I_type;
wire dec_S_type;
wire dec_B_type;
assign dec_I_type=(dec_inst[6:0]==dec_I_size);
assign dec_S_type=(dec_inst[6:0]==dec_S_size);
assign dec_B_type=(dec_inst[6:0]==dec_B_size);
wire [dec_imm_size-1:0] dec_I_imm;
wire [dec_imm_size-1:0] dec_S_imm;
wire [dec_imm_size-1:0] dec_B_imm;
assign dec_I_imm={{20{dec_inst[31]}},dec_inst[31:20]};
assign dec_S_imm={{20{dec_inst[31]}},dec_inst[31:25],dec_inst[11:7]};
assign dec_B_imm={{20{dec_inst[31]}},dec_inst[7],dec_inst[30:25],dec_inst[11:8],1'b0};
assign dec_imm=dec_I_type?dec_I_imm:
               dec_S_type?dec_S_imm:
               dec_B_type?dec_B_imm:32'b0;
endmodule
module regs #(
    parameter regs_rs_size=5,
              regs_rd_size=5,
              regs_data_size=32
)(
    input                       regs_clk,
    input                       regs_res,
    input                       regs_wen,
    input  [regs_rs_size-1:0]   regs_rs1,
    input  [regs_rs_size-1:0]   regs_rs2, 
    input  [regs_rd_size-1:0]   regs_rd,
    input  [regs_data_size-1:0] regs_wdata,
    output [regs_data_size-1:0] regs_rs1_data,
    output [regs_data_size-1:0] regs_rs2_data
);
reg [regs_data_size-1:0] regs [regs_data_size-1:0];
initial begin
    $readmemb("RISC_V_regs.txt",regs);
end
always@(negedge regs_clk) begin
    if(regs_wen&(regs_rd!=0))
        regs[regs_rd]<=regs_wdata;
end
assign regs_rs1_data=(regs_rs1==5'b0)?32'b0:regs[regs_rs1];
assign regs_rs2_data=(regs_rs2==5'b0)?32'b0:regs[regs_rs2];
endmodule
module main_ctr #(
    parameter mc_opcode_size=7,
              mc_r_size=7'b0110011,
              mc_lw_size=7'b0000011,
              mc_sw_size=7'b0100011,
              mc_beq_size=7'b1100011
)(
    input  [mc_opcode_size-1:0] mc_opcode,
    output                      mc_branch,
    output                      mc_memread,
    output                      mc_memtoreg,
    output                      mc_aluop,
    output                      mc_memwrite,
    output                      mc_alusrc,
    output                      mc_regwrite
);
assign mc_branch=(mc_opcode==mc_beq_size);
assign mc_memread=(mc_opcode==mc_lw_size);
assign mc_memtoreg=(mc_opcode==mc_lw_size);
assign mc_aluop=(mc_opcode==mc_r_size);
assign mc_memwrite=(mc_opcode==mc_sw_size);
assign mc_alusrc=(mc_opcode==mc_lw_size)|(mc_opcode==mc_sw_size);
assign mc_regwrite=(mc_opcode==mc_r_size)|(mc_opcode==mc_lw_size);
endmodule
module alu_ctr #(
    parameter ac_fun3_size=3
)(
    input                     ac_fun7,
    input  [ac_fun3_size-1:0] ac_fun3,
    input                     ac_aluop,
    output                    ac_out
);
assign ac_out=ac_aluop&ac_fun7;
endmodule
module id_ex_regs #(
    parameter id_ex_word_size=32,
              id_ex_r_size=5
)(
    input                            id_ex_clk,
    input                            id_ex_res,
    input                            id_ex_jump,
    input                            id_ex_load_use,
    input      [id_ex_word_size-1:0] id_ex_pc_in,
    input      [id_ex_word_size-1:0] id_ex_imm_in,
    input      [id_ex_r_size-1:0]    id_ex_rs1_in,
    input      [id_ex_r_size-1:0]    id_ex_rs2_in,
    input      [id_ex_r_size-1:0]    id_ex_rd_in,
    input      [id_ex_word_size-1:0] id_ex_rs1_data_in,
    input      [id_ex_word_size-1:0] id_ex_rs2_data_in,
    input                            id_ex_branch_in,
    input                            id_ex_memread_in,
    input                            id_ex_memtoreg_in,
    input                            id_ex_aluctr_in,
    input                            id_ex_memwrite_in,
    input                            id_ex_alusrc_in,
    input                            id_ex_regwrite_in,
    output reg [id_ex_word_size-1:0] id_ex_pc_out,
    output reg [id_ex_word_size-1:0] id_ex_imm_out,
    output reg [id_ex_r_size-1:0]    id_ex_rs1_out,
    output reg [id_ex_r_size-1:0]    id_ex_rs2_out,
    output reg [id_ex_r_size-1:0]    id_ex_rd_out,
    output reg [id_ex_word_size-1:0] id_ex_rs1_data_out,
    output reg [id_ex_word_size-1:0] id_ex_rs2_data_out,
    output reg                       id_ex_branch_out,
    output reg                       id_ex_memread_out,
    output reg                       id_ex_memtoreg_out,
    output reg                       id_ex_aluctr_out,
    output reg                       id_ex_memwrite_out,
    output reg                       id_ex_alusrc_out,
    output reg                       id_ex_regwrite_out
);
always@(posedge id_ex_clk or negedge id_ex_res) begin
    if(~id_ex_res) begin
        id_ex_pc_out<='b0;
        id_ex_imm_out<='b0;
        id_ex_rs1_out<='b0;
        id_ex_rs2_out<='b0;
        id_ex_rd_out<='b0;
        id_ex_rs1_data_out<='b0;
        id_ex_rs2_data_out<='b0;
    end
        else begin
            id_ex_pc_out<=id_ex_pc_in;
            id_ex_imm_out<=id_ex_imm_in;
            id_ex_rs1_out<=id_ex_rs1_in;
            id_ex_rs2_out<=id_ex_rs2_in;
            id_ex_rd_out<=id_ex_rd_in;
            id_ex_rs1_data_out<=id_ex_rs1_data_in;
            id_ex_rs2_data_out<=id_ex_rs2_data_in;
        end
end
always@(posedge id_ex_clk or negedge id_ex_res) begin
    if((~id_ex_res)|id_ex_jump|id_ex_load_use) begin
        id_ex_branch_out<='b0;
        id_ex_memread_out<='b0;
        id_ex_memtoreg_out<='b0;
        id_ex_aluctr_out<='b0;
        id_ex_memwrite_out<='b0;
        id_ex_alusrc_out<='b0;
        id_ex_regwrite_out<='b0;
    end
        else begin
            id_ex_branch_out<=id_ex_branch_in;
            id_ex_memread_out<=id_ex_memread_in;
            id_ex_memtoreg_out<=id_ex_memtoreg_in;
            id_ex_aluctr_out<=id_ex_aluctr_in;
            id_ex_memwrite_out<=id_ex_memwrite_in;
            id_ex_alusrc_out<=id_ex_alusrc_in;
            id_ex_regwrite_out<=id_ex_regwrite_in;
        end
end
endmodule
module ex_stage #(
    parameter ex_word_size=32,
              ex_r_size=5,
              ex_forward_size=2
)(
    input  [ex_word_size-1:0] ex_pc_in,
    input  [ex_word_size-1:0] ex_pc_mux_din0,
    input  [ex_word_size-1:0] ex_imm_in,
    input                     ex_branch_in,
    input  [ex_word_size-1:0] ex_rs1_data_in,
    input  [ex_word_size-1:0] ex_rs2_data_in,
    input                     ex_alusrc_in,
    input                     ex_aluctr_in,
    input                     id_ex_memwrite_out,
    input  [ex_r_size-1:0]    id_ex_rs1_out,
    input  [ex_r_size-1:0]    id_ex_rs2_out,
    input  [ex_r_size-1:0]    ex_mem_rd_out,
    input                     ex_mem_memread_out,
    input                     ex_mem_regwrite_out,
    input  [ex_r_size-1:0]    mem_wb_rd_out,
    input                     mem_wb_regwrite_out,
    input  [ex_r_size-1:0]    id_ex_rs1_in,
    input  [ex_r_size-1:0]    id_ex_rs2_in,
    input  [ex_r_size-1:0]    id_ex_rd_out,
    input                     id_ex_memread_out,
    input                     id_ex_memwrite_in,
    input                     id_ex_regwrite_out,
    input  [ex_word_size-1:0] ex_mem_alu_data_out,
    input  [ex_word_size-1:0] mem_wb_alu_data_out,
    output [ex_word_size-1:0] ex_pc_out,
    output [ex_word_size-1:0] ex_alu_data_out,
    output                    ex_forward_c,
    output                    ex_load_use,
    output                    ex_jump
);
wire [ex_word_size-1:0]    pc_mux_din1;
wire                       alu_zero;
wire [ex_word_size-1:0]    alu_din1;
wire [ex_word_size-1:0]    alu_din2;
wire [ex_word_size-1:0]    regs_mux_din0;
wire [ex_forward_size-1:0] ex_forward_a;
wire [ex_forward_size-1:0] ex_forward_b;
forward forward(
    .id_ex_memwrite_out(id_ex_memwrite_out),
    .id_ex_rs1_out(id_ex_rs1_out),
    .id_ex_rs2_out(id_ex_rs2_out),
    .ex_mem_rd_out(ex_mem_rd_out),
    .ex_mem_memread_out(ex_mem_memread_out),
    .ex_mem_regwrite_out(ex_mem_regwrite_out),
    .mem_wb_rd_out(mem_wb_rd_out),
    .mem_wb_regwrite_out(mem_wb_regwrite_out),
    .id_ex_rs1_in(id_ex_rs1_in),
    .id_ex_rs2_in(id_ex_rs2_in),
    .id_ex_rd_out(id_ex_rd_out),
    .id_ex_memread_out(id_ex_memread_out),
    .id_ex_memwrite_in(id_ex_memwrite_in),
    .id_ex_regwrite_out(id_ex_regwrite_out),
    .forward_a(ex_forward_a),
    .forward_b(ex_forward_b),
    .forward_c(ex_forward_c),
    .load_use(ex_load_use)
);
ex_mux ex_mux_forwarda(
    .ex_mux_din1(ex_mem_alu_data_out),
    .ex_mux_din2(mem_wb_alu_data_out),
    .ex_mux_din3(ex_rs1_data_in),
    .ex_mux_sel(ex_forward_a),
    .ex_mux_dout(alu_din1)
);
ex_mux ex_mux_forwardb(
    .ex_mux_din1(ex_mem_alu_data_out),
    .ex_mux_din2(mem_wb_alu_data_out),
    .ex_mux_din3(ex_rs2_data_in),
    .ex_mux_sel(ex_forward_b),
    .ex_mux_dout(regs_mux_din0)
);
pc_adder2 pc_adder2(
    .pc_adder2_din1(ex_pc_in),
    .pc_adder2_din2(ex_imm_in),
    .pc_adder2_dout(pc_mux_din1)
);
pc_mux pc_mux(
    .pc_mux_din0(ex_pc_mux_din0),
    .pc_mux_din1(pc_mux_din1),
    .pc_mux_sel0(alu_zero),
    .pc_mux_sel1(ex_branch_in),
    .pc_mux_dout(ex_pc_out),
    .pc_jump(ex_jump)
);
regs_mux regs_mux(
    .regs_mux_din0(regs_mux_din0),
    .regs_mux_din1(ex_imm_in),
    .regs_mux_sel(ex_alusrc_in),
    .regs_mux_dout(alu_din2)
);
alu alu(
    .alu_din1(alu_din1),
    .alu_din2(alu_din2),
    .alu_ctr(ex_aluctr_in),
    .alu_zero(alu_zero),
    .alu_dout(ex_alu_data_out)
);
endmodule
module forward #(
    parameter forward_r_size=5,
              forward_size=2
)(
    input                       id_ex_memwrite_out,
    input  [forward_r_size-1:0] id_ex_rs1_out,
    input  [forward_r_size-1:0] id_ex_rs2_out,
    input  [forward_r_size-1:0] ex_mem_rd_out,
    input                       ex_mem_memread_out,
    input                       ex_mem_regwrite_out,
    input  [forward_r_size-1:0] mem_wb_rd_out,
    input                       mem_wb_regwrite_out,
    input  [forward_r_size-1:0] id_ex_rs1_in,
    input  [forward_r_size-1:0] id_ex_rs2_in,
    input  [forward_r_size-1:0] id_ex_rd_out,
    input                       id_ex_memread_out,
    input                       id_ex_memwrite_in,
    input                       id_ex_regwrite_out,
    output [forward_size-1:0]   forward_a,
    output [forward_size-1:0]   forward_b,
    output                      forward_c,
    output                      load_use
);
assign forward_a[1]=(ex_mem_regwrite_out&(ex_mem_rd_out!=5'b0)&(ex_mem_rd_out==id_ex_rs1_out));
assign forward_a[0]=(mem_wb_regwrite_out&(mem_wb_rd_out!=5'b0)&(mem_wb_rd_out==id_ex_rs1_out));
assign forward_b[1]=(ex_mem_regwrite_out&(ex_mem_rd_out!=5'b0)&(ex_mem_rd_out==id_ex_rs2_out));
assign forward_b[0]=(mem_wb_regwrite_out&(mem_wb_rd_out!=5'b0)&(mem_wb_rd_out==id_ex_rs2_out));
assign forward_c=ex_mem_regwrite_out&(ex_mem_rd_out!=5'b0)&(ex_mem_rd_out!=id_ex_rs1_out)&(ex_mem_rd_out==id_ex_rs2_out)
                 &id_ex_memwrite_out&ex_mem_memread_out;
assign load_use=id_ex_memread_out&id_ex_regwrite_out&(id_ex_rd_out!=5'b0)&(!id_ex_memwrite_in)&((id_ex_rd_out==id_ex_rs1_in)
                |(id_ex_rd_out==id_ex_rs2_in))|id_ex_memread_out&id_ex_regwrite_out&(id_ex_rd_out!=5'b0)&(id_ex_memwrite_in)
                &(id_ex_rd_out==id_ex_rs1_in);
endmodule
module ex_mux #(
    parameter ex_mux_word_size=32,
              ex_mux_sel_size=2
)(
    input  [ex_mux_word_size-1:0] ex_mux_din1,
    input  [ex_mux_word_size-1:0] ex_mux_din2,
    input  [ex_mux_word_size-1:0] ex_mux_din3,
    input  [ex_mux_sel_size-1:0]  ex_mux_sel,
    output [ex_mux_word_size-1:0] ex_mux_dout
);
assign ex_mux_dout=ex_mux_sel[1]?ex_mux_din1:ex_mux_sel[0]?ex_mux_din2:ex_mux_din3;
endmodule
module pc_adder2 #(
    parameter pc_adder2_size=32
)(
    input  [pc_adder2_size-1:0] pc_adder2_din1,
    input  [pc_adder2_size-1:0] pc_adder2_din2,
    output [pc_adder2_size-1:0] pc_adder2_dout
);
assign pc_adder2_dout=pc_adder2_din1+pc_adder2_din2;
endmodule
module pc_mux #(
    parameter pc_mux_size=32
)(
    input  [pc_mux_size-1:0] pc_mux_din0,
    input  [pc_mux_size-1:0] pc_mux_din1,
    input                    pc_mux_sel0,
    input                    pc_mux_sel1,
    output [pc_mux_size-1:0] pc_mux_dout,
    output                   pc_jump
);
assign pc_jump=pc_mux_sel0&pc_mux_sel1;
assign pc_mux_dout=(pc_mux_sel0&pc_mux_sel1)?pc_mux_din1:pc_mux_din0;
endmodule
module regs_mux #(
    parameter regs_mux_size=32
)(
    input  [regs_mux_size-1:0] regs_mux_din0,
    input  [regs_mux_size-1:0] regs_mux_din1,
    input                      regs_mux_sel,
    output [regs_mux_size-1:0] regs_mux_dout
);
assign regs_mux_dout=regs_mux_sel?regs_mux_din1:regs_mux_din0;
endmodule
module alu #(
    parameter alu_data_size=32,
              alu_add=0,
              alu_sub=1
)(
    input      [alu_data_size-1:0] alu_din1,
    input      [alu_data_size-1:0] alu_din2,
    input                          alu_ctr,
    output                         alu_zero,
    output reg [alu_data_size-1:0] alu_dout
);
assign alu_zero=(alu_din1==alu_din2);
always@(*) begin
    case(alu_ctr)
        alu_add:alu_dout<=alu_din1+alu_din2;
        alu_sub:alu_dout<=alu_din1-alu_din2;
    endcase
end
endmodule
module ex_mem_regs #(
    parameter ex_mem_word_size=32,
              ex_mem_rd_size=5
)(
    input                             ex_mem_clk,
    input                             ex_mem_res,
    input      [ex_mem_word_size-1:0] ex_mem_rs2_data_in,
    input      [ex_mem_word_size-1:0] ex_mem_alu_data_in,
    input      [ex_mem_rd_size-1:0]   ex_mem_rd_in,
    input                             ex_mem_memread_in,
    input                             ex_mem_memtoreg_in,
    input                             ex_mem_memwrite_in,
    input                             ex_mem_regwrite_in,
    input                             ex_mem_forwardc_in,
    output reg [ex_mem_word_size-1:0] ex_mem_rs2_data_out,
    output reg [ex_mem_word_size-1:0] ex_mem_alu_data_out,
    output reg [ex_mem_rd_size-1:0]   ex_mem_rd_out,
    output reg                        ex_mem_memread_out,
    output reg                        ex_mem_memtoreg_out,
    output reg                        ex_mem_memwrite_out,
    output reg                        ex_mem_regwrite_out,
    output reg                        ex_mem_forwardc_out
);
always@(posedge ex_mem_clk or negedge ex_mem_res) begin
    if(~ex_mem_res) begin
        ex_mem_rs2_data_out<='b0;
        ex_mem_alu_data_out<='b0;
        ex_mem_rd_out<='b0;
        ex_mem_memread_out<='b0;
        ex_mem_memtoreg_out<='b0;
        ex_mem_memwrite_out<='b0;
        ex_mem_regwrite_out<='b0;
        ex_mem_forwardc_out<='b0;
    end
        else begin
            ex_mem_rs2_data_out<=ex_mem_rs2_data_in;
            ex_mem_alu_data_out<=ex_mem_alu_data_in;
            ex_mem_rd_out<=ex_mem_rd_in;
            ex_mem_memread_out<=ex_mem_memread_in;
            ex_mem_memtoreg_out<=ex_mem_memtoreg_in;
            ex_mem_memwrite_out<=ex_mem_memwrite_in;
            ex_mem_regwrite_out<=ex_mem_regwrite_in;
            ex_mem_forwardc_out<=ex_mem_forwardc_in;
        end
end
endmodule
module mem_stage #(
    parameter mem_word_size=32
)(
    input  [mem_word_size-1:0] mem_rdata_in,
    input  [mem_word_size-1:0] mem_rs2_data_in,
    input                      mem_sel,
    output [mem_word_size-1:0] mem_wdata_out
);
mem_mux mem_mux_forwardc(
    .mem_mux_din1(mem_rdata_in),
    .mem_mux_din2(mem_rs2_data_in),
    .mem_mux_sel(mem_sel),
    .mem_mux_dout(mem_wdata_out)
);
endmodule
module mem_mux #(
    parameter mem_mux_word_size=32
)(
    input  [mem_mux_word_size-1:0] mem_mux_din1,
    input  [mem_mux_word_size-1:0] mem_mux_din2,
    input                          mem_mux_sel,
    output [mem_mux_word_size-1:0] mem_mux_dout
);
assign mem_mux_dout=mem_mux_sel?mem_mux_din1:mem_mux_din2;
endmodule
module Data_Mem #(
    parameter DM_data_size=32,
              DM_addr_size=32,
              DM_ram_size=256
)(
    input                      DM_clk,
    input                      DM_res,
    input  [DM_data_size-1:0]  DM_wdata,
    input  [DM_addr_size-1:0]  DM_addr,
    input                      DM_wen,
    input                      DM_ren,
    output [DM_data_size-1:0]  DM_rdata
);
reg [DM_data_size-1:0] DM_ram [DM_ram_size-1:0];
initial begin
    $readmemb("RISC_V_DM.txt",DM_ram);
end 
always@(negedge DM_clk) begin
    if(DM_wen)
        DM_ram[DM_addr[9:2]]<=DM_wdata;
end
assign DM_rdata=DM_ram[DM_addr[31:2]];
endmodule
module mem_wb_regs #(
    parameter mem_wb_word_size=32,
              mem_wb_rd_size=5
)(
    input                             mem_wb_clk,
    input                             mem_wb_res,
    input      [mem_wb_word_size-1:0] mem_wb_rdata_in,
    input      [mem_wb_word_size-1:0] mem_wb_alu_data_in,
    input      [mem_wb_rd_size-1:0]   mem_wb_rd_in,
    input                             mem_wb_memtoreg_in,
    input                             mem_wb_regwrite_in,
    output reg [mem_wb_word_size-1:0] mem_wb_rdata_out,
    output reg [mem_wb_word_size-1:0] mem_wb_alu_data_out,
    output reg [mem_wb_rd_size-1:0]   mem_wb_rd_out,
    output reg                        mem_wb_memtoreg_out,
    output reg                        mem_wb_regwrite_out
);
always@(posedge mem_wb_clk or negedge mem_wb_res) begin
    if(~mem_wb_res) begin
        mem_wb_rdata_out<='b0;
        mem_wb_alu_data_out<='b0;
        mem_wb_rd_out<='b0;
        mem_wb_memtoreg_out<='b0;
        mem_wb_regwrite_out<='b0;
    end
        else begin
            mem_wb_rdata_out<=mem_wb_rdata_in;
            mem_wb_alu_data_out<=mem_wb_alu_data_in;
            mem_wb_rd_out<=mem_wb_rd_in;
            mem_wb_memtoreg_out<=mem_wb_memtoreg_in;
            mem_wb_regwrite_out<=mem_wb_regwrite_in;
        end
end
endmodule
module wb_stage #(
    parameter wb_size=32
)(
    input  [wb_size-1:0] wb_rdata_in,
    input  [wb_size-1:0] wb_alu_data_in,
    input                wb_memtoreg_in,
    output [wb_size-1:0] wb_wdata_out
);
dm_mux dm_mux(
    .dm_mux_din0(wb_alu_data_in),
    .dm_mux_din1(wb_rdata_in),
    .dm_mux_sel(wb_memtoreg_in),
    .dm_mux_dout(wb_wdata_out)
);
endmodule
module dm_mux #(
    parameter dm_mux_size=32
)(
    input  [dm_mux_size-1:0] dm_mux_din0,
    input  [dm_mux_size-1:0] dm_mux_din1,
    input                    dm_mux_sel,
    output [dm_mux_size-1:0] dm_mux_dout
);
assign dm_mux_dout=dm_mux_sel?dm_mux_din1:dm_mux_din0;
endmodule