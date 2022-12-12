import "DPI-C" function void dpi_memory_read(input longint addr, output longint data, input int log_size);
import "DPI-C" function void dpi_memory_write(longint addr, int byte_mask, longint data, int log_size);

module dromajo_ram #
  (
   parameter ADDR_WIDTH = 25,
   parameter DATA_WIDTH = 64,
   parameter RAM_BASE = 'h80000000
   )
   (
    input wire                    clk_i,
    input wire                    rst_i,

    input wire [ADDR_WIDTH-1:0]   adr_i,
    input wire [DATA_WIDTH-1:0]   dat_i,
    input wire [DATA_WIDTH/8-1:0] sel_i,
    input wire                    we_i,
    input wire                    cyc_i,
    input wire                    stb_i,

    output wire                   ack_o,
    output wire [DATA_WIDTH-1:0]  dat_o
    );

   localparam                     ADDR_LSB_WIDTH = $clog2(DATA_WIDTH / 8);
   localparam                     LOG_SIZE = $clog2(DATA_WIDTH/8);

   reg [63:0]                     dat_r_reg;
   reg                            ack_reg;

   always @(*) begin
      dat_r_reg = 0;

      if (~rst_i & cyc_i & stb_i) begin
         dpi_memory_read(RAM_BASE + (adr_i << ADDR_LSB_WIDTH), dat_r_reg, LOG_SIZE);
      end
   end

   assign dat_o = dat_r_reg[DATA_WIDTH-1:0];

   always @(posedge clk_i) begin
      if (~rst_i) begin
         if (cyc_i & stb_i & we_i) begin
            dpi_memory_write(RAM_BASE + (adr_i << ADDR_LSB_WIDTH), sel_i, dat_i, LOG_SIZE);
         end
      end
   end

   always @(posedge clk_i) begin
      if (rst_i)
        ack_reg <= 0;
      else
        ack_reg <= cyc_i & stb_i & ~ack_o;
   end

   assign ack_o = ack_reg;

endmodule // dromajo_ram
