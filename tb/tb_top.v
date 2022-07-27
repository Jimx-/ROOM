`timescale 1ns / 1ps

module tb_top
  (
   input wire     sys_clk,
   input wire     sys_rst,

   output         instr_axi_arvalid,
   output [31:0]  instr_axi_araddr,
   output [7:0]   instr_axi_arlen,
   output [2:0]   instr_axi_arsize,
   output [1:0]   instr_axi_arburst,
   output [0:0]   instr_axi_arlock,
   output [3:0]   instr_axi_arcache,
   output [2:0]   instr_axi_arprot,
   output [3:0]   instr_axi_arqos,
   output [0:0]   instr_axi_arid,
   input          instr_axi_arready,
   input          instr_axi_rid,
   input          instr_axi_rvalid,
   input [127:0]  instr_axi_rdata,
   input [1:0]    instr_axi_rresp,
   input          instr_axi_rlast,
   output         instr_axi_rready,
   output         instr_axi_awvalid,
   output [31:0]  instr_axi_awaddr,
   output [7:0]   instr_axi_awlen,
   output [2:0]   instr_axi_awsize,
   output [1:0]   instr_axi_awburst,
   output [0:0]   instr_axi_awlock,
   output [3:0]   instr_axi_awcache,
   output [2:0]   instr_axi_awprot,
   output [3:0]   instr_axi_awqos,
   output [0:0]   instr_axi_awid,
   input          instr_axi_awready,
   output         instr_axi_wvalid,
   output [127:0] instr_axi_wdata,
   output [15:0]  instr_axi_wstrb,
   output         instr_axi_wlast,
   input          instr_axi_wready,
   input          instr_axi_bid,
   input          instr_axi_bvalid,
   input [1:0]    instr_axi_bresp,
   output         instr_axi_bready
   );

   assign instr_axi_arprot = 3'b010;

   wrapper wrapper_inst (.clk(sys_clk),
                         .rst(sys_rst),

                         .ar__valid(instr_axi_arvalid),
                         .ar__addr(instr_axi_araddr),
                         .ar__len(instr_axi_arlen),
                         .ar__size(instr_axi_arsize),
                         .ar__burst(instr_axi_arburst),
                         .ar__lock(instr_axi_arlock),
                         .ar__cache(instr_axi_arcache),
                         .ar__qos(instr_axi_arqos),
                         .ar__id(instr_axi_arid),
                         .ar__ready(instr_axi_arready),
                         .r__valid(instr_axi_rvalid),
                         .r__data(instr_axi_rdata),
                         .r__resp(instr_axi_rresp),
                         .r__last(instr_axi_rlast),
                         .r__ready(instr_axi_rready),
                         .aw__valid(instr_axi_awvalid),
                         .aw__addr(instr_axi_awaddr),
                         .aw__len(instr_axi_awlen),
                         .aw__size(instr_axi_awsize),
                         .aw__burst(instr_axi_awburst),
                         .aw__lock(instr_axi_awlock),
                         .aw__cache(instr_axi_awcache),
                         .aw__prot(instr_axi_awprot),
                         .aw__qos(instr_axi_awqos),
                         .aw__id(instr_axi_awid),
                         .aw__ready(instr_axi_awready),
                         .w__valid(instr_axi_wvalid),
                         .w__data(instr_axi_wdata),
                         .w__strb(instr_axi_wstrb),
                         .w__last(instr_axi_wlast),
                         .w__ready(instr_axi_wready),
                         .b__valid(instr_axi_bvalid),
                         .b__resp(instr_axi_bresp),
                         .b__ready(instr_axi_bready)
                         );


`ifdef COCOTB_SIM
   initial begin
      $dumpfile ("tb_top.vcd");
      $dumpvars (0, tb_top);
      #1;
   end
`endif

endmodule
