import "DPI-C" function void dpi_putchar(input int ch);

module debug_console #
  (
    parameter DATA_WIDTH = 32
   )
   (
     input wire                  clk_i,
     input wire                  rst_i,

     input wire [DATA_WIDTH-1:0] dat_i,
     input wire we_i
    );

   always @(posedge clk_i or posedge rst_i) begin
      if (~rst_i) begin
         if (we_i) begin
            dpi_putchar(dat_i);
         end
      end
   end

endmodule // debug_console
